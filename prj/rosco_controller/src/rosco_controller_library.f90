!  rosco_controller_library.f90 defines procedures for calling NREL's ROSCO controller as a component in the dispatch framework.
!
!  FUNCTIONS/SUBROUTINES exported from rosco_controller_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  this library implements an arbitrary number of cubic nonlinearity soft clippers. A sample (2-clipper) input file is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [soft_clipper]
!  input_signal_name = "my_input_signal"
!  output_signal_name = "my_output_signal"
!  center_value = 1000 # the center value for the clipping (zero distortion at this value)
!  saturation_limits = [0, 1010] # lower and upper signal limits (need not be symmetric about center_value; must not = center_value)
!  
!  [soft_clipper]
!  input_signal_name = "another_input_signal"
!  output_signal_name = "another_output_signal"
!  center_value = 0
!  saturation_limits = [-1, 1]
!
!  The cubic nonlinearity soft clipper has the slowest aliasing rate obtainable for an odd nonlinearity. It does, however,
!  introduce a gain of 1.5 near the center value.
!  See https://ccrma.stanford.edu/~jos/pasp/Soft_Clipping.html for further information on the cubic nonlinearity soft clipper
!
module rosco_controller_library

  use extf, only: itoa, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: string_container_t, wp
  USE, INTRINSIC  :: ISO_C_Binding
  USE             :: ROSCO_Types
  USE             :: ReadSetParameters
  USE             :: ControllerBlocks
  USE             :: Controllers
  USE             :: Constants
  USE             :: Filters
  USE             :: Functions
  USE             :: ExtControl
  USE             :: ROSCO_IO
  USE             :: ZeroMQInterface

  implicit none

  ! library variables
  type :: config_t
    integer(c_int) :: context
    real(wp) :: sample_period = 0.0_wp
  end type
  type(config_t), allocatable :: config(:)
  type :: library_data_t
    integer :: n_offers, n_requests
    type(string_container_t), allocatable :: offers(:)
    type(string_container_t), allocatable :: requests(:)
    real(wp), allocatable :: input_buffer(:)
    real(wp), allocatable :: output_buffer(:)
    integer, allocatable  :: input_to_swap_map(:)
    integer, allocatable  :: swap_to_output_map(:)
  end type
  type(library_data_t) :: library_data
  type :: rosco_t
    character(len=:), allocatable :: parameter_file
    character(len=:), allocatable :: output_file
  end type
  type(rosco_t) :: rosco

  contains
  
  subroutine initialize(parameters, parameters_length, parameters_as_toml_string, context, sample_period, n_requests, n_offers, error_string) bind(C, name="initialize")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::initialize
  #endif
  use tomlf
  use tomlf_error, only: toml_error, toml_stat
  use tomlf_error_handler, only: toml_error_message

  implicit none
  
  ! interface variables
  character(kind=c_char, len=*),              intent(in   ) :: parameters
  integer(kind=c_int),                        intent(in   ) :: parameters_length
  integer(kind=c_int),                        intent(in   ) :: parameters_as_toml_string
  integer(kind=c_int),                        intent(  out) :: context
  real(kind=c_double),                        intent(  out) :: sample_period
  integer(kind=c_int),                        intent(  out) :: n_requests
  integer(kind=c_int),                        intent(  out) :: n_offers
  character(kind=c_char, len=:), allocatable, intent(  out) :: error_string
  ! local variables related to toml reading
  type(toml_table), allocatable :: table
  type(toml_error), allocatable :: error_toml
  type(toml_context)            :: context_toml
  class(error_t), allocatable   :: error
  integer :: stat, origin
  
  try: block
    ! create a new config object for this instance of the library
    if (allocated(config)) then
      error = fail('Only one instance of the ROSCO controller is allowed: procedure calls cannot be guaranteed re-entrant.')
      exit try
    else
      context = 1
      allocate(config(1), source = config_t(context))
    end if
    ! read parameters
    if (parameters_as_toml_string .ne. 0) then
      ! implement a toml reader
      call toml_loads(table, parameters, context=context_toml, error=error_toml)
      if (allocated(error_toml)) then
        error = fail(error_toml%message)
        exit try
      end if
      ! get parameter_file
      call get_value(table, 'parameter_file', rosco%parameter_file, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read parameter_file", origin, "key is required."))
        call wrap_error(error, '"parameter_file" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! get output_file
      call get_value(table, 'output_file', rosco%output_file, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read output_file", origin, "key is required."))
        call wrap_error(error, '"output_file" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
    else
      error = fail("ROSCO parameters must be specified as toml via library->parameters->parameter_file and output_file in the dispatch configuration file.")
      exit try
    end if
    
    ! initialize library variables
    library_data%n_offers = 27
    library_data%n_requests = 78 + 18 ! there are 18 additional platform-related signals for floating turbines
    allocate(library_data%offers(library_data%n_offers))
    allocate(library_data%output_buffer(library_data%n_offers), source=0.0_wp)
    allocate(library_data%swap_to_output_map(library_data%n_offers), source=0)
    allocate(library_data%requests(library_data%n_requests))
    allocate(library_data%input_buffer(library_data%n_requests), source=0.0_wp)
    allocate(library_data%input_to_swap_map(library_data%n_requests), source=0)
    library_data%offers( 1)%s = "status_flag"
    library_data%offers( 2)%s = "generator_contactor"
    library_data%offers( 3)%s = "shaft_brake_status"
    library_data%offers( 4)%s = "demanded_blade_1_pitch_angle_or_rate"
    library_data%offers( 5)%s = "demanded_blade_2_pitch_angle_or_rate"
    library_data%offers( 6)%s = "demanded_blade_3_pitch_angle_or_rate"
    library_data%offers( 7)%s = "demanded_collective_pitch_angle"
    library_data%offers( 8)%s = "demanded_collective_pitch_rate"
    library_data%offers( 9)%s = "demanded_generator_torque"
    library_data%offers(10)%s = "demanded_yaw_rate"
    library_data%offers(11)%s = "message_length"
    library_data%offers(12)%s = "pitch_override"
    library_data%offers(13)%s = "torque_override"
    library_data%offers(14)%s = "n_variables_returned_for_logging"
    library_data%offers(15)%s = "generator_start_up_resistance"
    library_data%offers(16)%s = "request_for_loads"
    library_data%offers(17)%s = "variable_slip_current_demand_on_off"
    library_data%offers(18)%s = "variable_slip_current_demand"
    library_data%offers(19)%s = "mean_wind_speed_increment"
    library_data%offers(20)%s = "turbulence_intensity_increment"
    library_data%offers(21)%s = "wind_direction_increment"
    library_data%offers(22)%s = "safety_system_number_to_activate"
    library_data%offers(23)%s = "yaw_control_flag"
    library_data%offers(24)%s = "yaw_stiffness"
    library_data%offers(25)%s = "yaw_damping"
    library_data%offers(26)%s = "brake_torque_demand"
    library_data%offers(27)%s = "yaw_brake_torque_demand"
    library_data%swap_to_output_map( 1) =   1
    library_data%swap_to_output_map( 2) =  35
    library_data%swap_to_output_map( 3) =  36
    library_data%swap_to_output_map( 4) =  42
    library_data%swap_to_output_map( 5) =  43
    library_data%swap_to_output_map( 6) =  44
    library_data%swap_to_output_map( 7) =  45
    library_data%swap_to_output_map( 8) =  46
    library_data%swap_to_output_map( 9) =  47
    library_data%swap_to_output_map(10) =  48
    library_data%swap_to_output_map(11) =  49
    library_data%swap_to_output_map(12) =  55
    library_data%swap_to_output_map(13) =  56
    library_data%swap_to_output_map(14) =  65
    library_data%swap_to_output_map(15) =  72
    library_data%swap_to_output_map(16) =  79
    library_data%swap_to_output_map(17) =  80
    library_data%swap_to_output_map(18) =  81
    library_data%swap_to_output_map(19) =  92
    library_data%swap_to_output_map(20) =  93
    library_data%swap_to_output_map(21) =  94
    library_data%swap_to_output_map(22) =  98
    library_data%swap_to_output_map(23) = 102
    library_data%swap_to_output_map(24) = 103
    library_data%swap_to_output_map(25) = 104
    library_data%swap_to_output_map(26) = 107
    library_data%swap_to_output_map(27) = 108

    library_data%requests( 1)%s = "status_flag"
    library_data%requests( 2)%s = "current_time"
    library_data%requests( 3)%s = "communication_interval"
    library_data%requests( 4)%s = "blade_1_pitch_angle"
    library_data%requests( 5)%s = "below_rated_pitch_angle_setpoint"
    library_data%requests( 6)%s = "minimum_pitch_angle"
    library_data%requests( 7)%s = "maximum_pitch_angle"
    library_data%requests( 8)%s = "minimum_pitch_rate"
    library_data%requests( 9)%s = "maximum_pitch_rate"
    library_data%requests(10)%s = "pitch_actuator_type"
    library_data%requests(11)%s = "current_demanded_pitch_angle"
    library_data%requests(12)%s = "current_demanded_pitch_rate"
    library_data%requests(13)%s = "demanded_power"
    library_data%requests(14)%s = "measured_shaft_power"
    library_data%requests(15)%s = "measured_electrical_power_output"
    library_data%requests(16)%s = "optimal_mode_gain"
    library_data%requests(17)%s = "minimum_generator_speed"
    library_data%requests(18)%s = "optimal_mode_maximum_speed"
    library_data%requests(19)%s = "demanded_generator_speed_above_rated"
    library_data%requests(20)%s = "measured_generator_speed"
    library_data%requests(21)%s = "measured_rotor_speed"
    library_data%requests(22)%s = "demanded_generator_torque_above_rated"
    library_data%requests(23)%s = "measured_generator_torque"
    library_data%requests(24)%s = "measured_yaw_error"
    library_data%requests(25)%s = "start_record_for_torque_speed_lut"
    library_data%requests(26)%s = "n_points_in_torque_speed_lookup_table"
    library_data%requests(27)%s = "hub_wind_speed"
    library_data%requests(28)%s = "individual_pitch_control_mode"
    library_data%requests(29)%s = "yaw_control_mode"
    library_data%requests(30)%s = "blade_1_root_oop_bending_moment"
    library_data%requests(31)%s = "blade_2_root_oop_bending_moment"
    library_data%requests(32)%s = "blade_3_root_oop_bending_moment"
    library_data%requests(33)%s = "blade_2_pitch_angle"
    library_data%requests(34)%s = "blade_3_pitch_angle"
    library_data%requests(35)%s = "generator_contactor"
    library_data%requests(36)%s = "shaft_brake_status"
    library_data%requests(37)%s = "nacelle_yaw_angle_from_north"
    library_data%requests(38)%s = "demanded_yaw_actuator_torque"
    library_data%requests(39)%s = "message_length"
    library_data%requests(40)%s = "n_chars_in_infile"
    library_data%requests(41)%s = "n_chars_in_outname"
    library_data%requests(42)%s = "discon_interface_version_number"
    library_data%requests(43)%s = "tower_top_fore_aft_acceleration"
    library_data%requests(44)%s = "tower_top_side_side_acceleration"
    library_data%requests(45)%s = "rotor_azimuth"
    library_data%requests(46)%s = "n_blade"
    library_data%requests(47)%s = "max_num_values_for_logging"
    library_data%requests(48)%s = "start_record_for_logging"
    library_data%requests(49)%s = "max_chars_in_outname"
    library_data%requests(50)%s = "blade_1_root_in_plane_bending_moment"
    library_data%requests(51)%s = "blade_2_root_in_plane_bending_moment"
    library_data%requests(52)%s = "blade_3_root_in_plane_bending_moment"
    library_data%requests(53)%s = "rotating_hub_my"
    library_data%requests(54)%s = "rotating_hub_mz"
    library_data%requests(55)%s = "fixed_hub_my"
    library_data%requests(56)%s = "fixed_hub_mz"
    library_data%requests(57)%s = "yaw_bearing_my"
    library_data%requests(58)%s = "yaw_bearing_mz"
    library_data%requests(59)%s = "nacelle_roll_acceleration"
    library_data%requests(60)%s = "nacelle_nodding_acceleration"
    library_data%requests(61)%s = "nacelle_yaw_acceleration"
    library_data%requests(62)%s = "realtime_sim_time_step"
    library_data%requests(63)%s = "realtime_sim_time_step_multiplier"
    library_data%requests(64)%s = "safety_system_number_activated"
    library_data%requests(65)%s = "shaft_torque"
    library_data%requests(66)%s = "hub_fixed_fx"
    library_data%requests(67)%s = "hub_fixed_fy"
    library_data%requests(68)%s = "hub_fixed_fz"
    library_data%requests(69)%s = "network_voltage_disturbance_factor"
    library_data%requests(70)%s = "network_frequency_disturbance_factor"
    library_data%requests(71)%s = "controller_state"
    library_data%requests(72)%s = "settling_time"
    library_data%requests(73)%s = "teeter_angle"
    library_data%requests(74)%s = "teeter_velocity"
    library_data%requests(75)%s = "controller_failure_flag"
    library_data%requests(76)%s = "yaw_bearing_angular_position"
    library_data%requests(77)%s = "yaw_bearing_angular_velocity"
    library_data%requests(78)%s = "yaw_bearing_angular_acceleration"
    library_data%requests(79)%s = "platform_position_x"
    library_data%requests(80)%s = "platform_position_y"
    library_data%requests(81)%s = "platform_position_z"
    library_data%requests(82)%s = "platform_rotation_x"
    library_data%requests(83)%s = "platform_rotation_y"
    library_data%requests(84)%s = "platform_rotation_z"
    library_data%requests(85)%s = "platform_velocity_x"
    library_data%requests(86)%s = "platform_velocity_y"
    library_data%requests(87)%s = "platform_velocity_z"
    library_data%requests(88)%s = "platform_rotational_velocity_x"
    library_data%requests(89)%s = "platform_rotational_velocity_y"
    library_data%requests(90)%s = "platform_rotational_velocity_z"
    library_data%requests(91)%s = "platform_acceleration_x"
    library_data%requests(92)%s = "platform_acceleration_y"
    library_data%requests(93)%s = "platform_acceleration_z"
    library_data%requests(94)%s = "platform_rotational_acceleration_x"
    library_data%requests(95)%s = "platform_rotational_acceleration_y"
    library_data%requests(96)%s = "platform_rotational_acceleration_z"
    library_data%input_to_swap_map( 1) =    1
    library_data%input_to_swap_map( 2) =    2
    library_data%input_to_swap_map( 3) =    3
    library_data%input_to_swap_map( 4) =    4
    library_data%input_to_swap_map( 5) =    5
    library_data%input_to_swap_map( 6) =    6
    library_data%input_to_swap_map( 7) =    7
    library_data%input_to_swap_map( 8) =    8
    library_data%input_to_swap_map( 9) =    9
    library_data%input_to_swap_map(10) =   10
    library_data%input_to_swap_map(11) =   11
    library_data%input_to_swap_map(12) =   12
    library_data%input_to_swap_map(13) =   13
    library_data%input_to_swap_map(14) =   14
    library_data%input_to_swap_map(15) =   15
    library_data%input_to_swap_map(16) =   16
    library_data%input_to_swap_map(17) =   17
    library_data%input_to_swap_map(18) =   18
    library_data%input_to_swap_map(19) =   19
    library_data%input_to_swap_map(20) =   20
    library_data%input_to_swap_map(21) =   21
    library_data%input_to_swap_map(22) =   22
    library_data%input_to_swap_map(23) =   23
    library_data%input_to_swap_map(24) =   24
    library_data%input_to_swap_map(25) =   25
    library_data%input_to_swap_map(26) =   26
    library_data%input_to_swap_map(27) =   27
    library_data%input_to_swap_map(28) =   28
    library_data%input_to_swap_map(29) =   29
    library_data%input_to_swap_map(30) =   30
    library_data%input_to_swap_map(31) =   31
    library_data%input_to_swap_map(32) =   32
    library_data%input_to_swap_map(33) =   33
    library_data%input_to_swap_map(34) =   34
    library_data%input_to_swap_map(35) =   35
    library_data%input_to_swap_map(36) =   36
    library_data%input_to_swap_map(37) =   37
    library_data%input_to_swap_map(38) =   41
    library_data%input_to_swap_map(39) =   49
    library_data%input_to_swap_map(40) =   50
    library_data%input_to_swap_map(41) =   51
    library_data%input_to_swap_map(42) =   52
    library_data%input_to_swap_map(43) =   53
    library_data%input_to_swap_map(44) =   54
    library_data%input_to_swap_map(45) =   60
    library_data%input_to_swap_map(46) =   61
    library_data%input_to_swap_map(47) =   62
    library_data%input_to_swap_map(48) =   63
    library_data%input_to_swap_map(49) =   64
    library_data%input_to_swap_map(50) =   69
    library_data%input_to_swap_map(51) =   70
    library_data%input_to_swap_map(52) =   71
    library_data%input_to_swap_map(53) =   73
    library_data%input_to_swap_map(54) =   74
    library_data%input_to_swap_map(55) =   75
    library_data%input_to_swap_map(56) =   76
    library_data%input_to_swap_map(57) =   78
    library_data%input_to_swap_map(58) =   79
    library_data%input_to_swap_map(59) =   82
    library_data%input_to_swap_map(60) =   83
    library_data%input_to_swap_map(61) =   84
    library_data%input_to_swap_map(62) =   90
    library_data%input_to_swap_map(63) =   91
    library_data%input_to_swap_map(64) =   97
    library_data%input_to_swap_map(65) =  109
    library_data%input_to_swap_map(66) =  110
    library_data%input_to_swap_map(67) =  111
    library_data%input_to_swap_map(68) =  112
    library_data%input_to_swap_map(69) =  113
    library_data%input_to_swap_map(70) =  114
    library_data%input_to_swap_map(71) =  117
    library_data%input_to_swap_map(72) =  118
    library_data%input_to_swap_map(73) =  143
    library_data%input_to_swap_map(74) =  144
    library_data%input_to_swap_map(75) =  161
    library_data%input_to_swap_map(76) =  162
    library_data%input_to_swap_map(77) =  163
    library_data%input_to_swap_map(78) =  164
    library_data%input_to_swap_map(79) = 1001
    library_data%input_to_swap_map(80) = 1002
    library_data%input_to_swap_map(81) = 1003
    library_data%input_to_swap_map(82) = 1004
    library_data%input_to_swap_map(83) = 1005
    library_data%input_to_swap_map(84) = 1006
    library_data%input_to_swap_map(85) = 1007
    library_data%input_to_swap_map(86) = 1008
    library_data%input_to_swap_map(87) = 1009
    library_data%input_to_swap_map(88) = 1010
    library_data%input_to_swap_map(89) = 1011
    library_data%input_to_swap_map(90) = 1012
    library_data%input_to_swap_map(91) = 1013
    library_data%input_to_swap_map(92) = 1014
    library_data%input_to_swap_map(93) = 1015
    library_data%input_to_swap_map(94) = 1016
    library_data%input_to_swap_map(95) = 1017
    library_data%input_to_swap_map(96) = 1018
    
    ! set values returned to caller
    ! context was assigned a value above
    sample_period = config(context)%sample_period
    n_offers = library_data%n_offers
    n_requests = library_data%n_requests
    return
  end block try
  ! if we are here, then there has been an error: report it to the caller
  error_string = error%to_chars()
  
  end subroutine initialize
  
  subroutine get_signal_requests(context, requests, error_string) bind(C, name="get_signal_requests")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::get_signal_requests
  #endif
  implicit none
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(inout) :: requests
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  ! local variables
  integer :: i
  
  call string_container_array_to_delimited_string(library_data%requests, ";", requests)
  
  end subroutine
  
  subroutine get_signal_offers(context, offers, error_string) bind(C, name="get_signal_offers")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::get_signal_offers
  #endif
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(inout) :: offers
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  ! local variables
  integer :: i
  
  call string_container_array_to_delimited_string(library_data%offers, ";", offers)
  
  end subroutine
  
  subroutine update(context, input_array, output_array, error_string) bind(C, name="update")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::update
  #endif
  integer(kind=c_int), intent(in   ) :: context
  real(kind=c_double), intent(in   ) :: input_array(*)
  real(kind=c_double), intent(  out) :: output_array(*)
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  ! local variables
  class(error_t), allocatable   :: error
  integer :: i
  ! variables for managing the swap array used by the ROSCO controller
  real(4) :: avrSWAP(3500)                                ! https://openfast.readthedocs.io/en/main/source/user/servodyn/ExtendedBladedInterface.html
  integer(c_int) :: aviFAIL = 0
  CHARACTER(KIND=C_CHAR), allocatable :: accINFILE(:)     ! The name of the parameter input file
  CHARACTER(KIND=C_CHAR), allocatable :: avcOUTNAME(:)    ! OUTNAME (Simulation RootName)
  ! variables from ROSCO's DISCON procedure
  CHARACTER(len=:), allocatable       :: RootName         ! a Fortran version of the input C string (not considered an array here)    [subtract 1 for the C null-character]
  TYPE(ControlParameters), SAVE :: CntrPar
  TYPE(LocalVariables),    SAVE :: LocalVar
  TYPE(ObjectInstances),   SAVE :: objInst
  TYPE(PerformanceData),   SAVE :: PerfData
  TYPE(DebugVariables),    SAVE :: DebugVar
  TYPE(ErrorVariables),    SAVE :: ErrVar
  TYPE(ExtControlType),    SAVE :: ExtDLL
  CHARACTER(*),       PARAMETER :: RoutineName = 'ROSCO'

  try: block
    ! copy input array to avrSWAP
    do i = 1, library_data%n_requests
      avrSWAP(library_data%input_to_swap_map(i)) = real(input_array(i), kind=4)
    end do

    ! ignore the variable-size portion of the swap array (torque-speed look-up table, message-passing-as-real-vector, variable-logging)
    ! these features must be handled elsewhere in the code (input file, error handler, controller-managed log file, respectively)

    ! allocate strings based on sizes specified in the swap array
    allocate(accINFILE(len(rosco%parameter_file)))
    avrSWAP(50) = real(len(rosco%parameter_file), kind=4)
    do i = 1,len(rosco%parameter_file)
      accINFILE(i) = rosco%parameter_file(i:i)
    end do
    allocate(avcOUTNAME(len(rosco%output_file)))
    avrSWAP(51) = real(len(rosco%output_file), kind=4)
    RootName = repeat(c_null_char, len(rosco%output_file))
    do i = 1,len(rosco%output_file)
      avcOUTNAME(i) = rosco%output_file(i:i)
    end do

    ! the remainder of this code is copied directly from ROSCO's DISCON() procedure
    RootName = TRANSFER(avcOUTNAME, RootName)
    CALL GetRoot(RootName,RootName)

    ! Check for restart
    IF ( (NINT(avrSWAP(1)) == -9) .AND. (aviFAIL >= 0))  THEN ! Read restart files
        CALL ReadRestartFile(avrSWAP, LocalVar, CntrPar, objInst, PerfData, RootName, SIZE(avcOUTNAME), ErrVar)
        IF ( CntrPar%LoggingLevel > 0 ) THEN
            CALL Debug(LocalVar, CntrPar, DebugVar, ErrVar, avrSWAP, RootName, SIZE(avcOUTNAME))
        END IF 
    END IF

    ! Read avrSWAP array into derived types/variables
    CALL ReadAvrSWAP(avrSWAP, LocalVar, CntrPar, ErrVar)

    ! Set Control Parameters
    CALL SetParameters(avrSWAP, accINFILE, nint(avrSWAP(49)), CntrPar, LocalVar, objInst, PerfData, RootName, ErrVar)

    ! Call external controller, if desired
    IF (CntrPar%Ext_Mode > 0 .AND. ErrVar%aviFAIL >= 0) THEN
        CALL ExtController(avrSWAP, CntrPar, LocalVar, ExtDLL, ErrVar)
        ! Data from external dll is in ExtDLL%avrSWAP, it's unused in the following code
    END IF

    ! Filter signals
    CALL PreFilterMeasuredSignals(CntrPar, LocalVar, DebugVar, objInst, ErrVar)

    IF (((LocalVar%iStatus >= 0) .OR. (LocalVar%iStatus <= -8)) .AND. (ErrVar%aviFAIL >= 0))  THEN  ! Only compute control calculations if no error has occurred and we are not on the last time step
      IF ((LocalVar%iStatus == -8) .AND. (ErrVar%aviFAIL >= 0))  THEN ! Write restart files
          CALL WriteRestartFile(LocalVar, CntrPar, ErrVar, objInst, RootName, SIZE(avcOUTNAME))    
      ENDIF
      IF (CntrPar%ZMQ_Mode > 0) THEN
          CALL UpdateZeroMQ(LocalVar, CntrPar, ErrVar)
      ENDIF
      
      CALL WindSpeedEstimator(LocalVar, CntrPar, objInst, PerfData, DebugVar, ErrVar)
      CALL ComputeVariablesSetpoints(CntrPar, LocalVar, objInst, DebugVar, ErrVar)
      CALL StateMachine(CntrPar, LocalVar)
      CALL SetpointSmoother(LocalVar, CntrPar, objInst)
      CALL VariableSpeedControl(avrSWAP, CntrPar, LocalVar, objInst, ErrVar)
      CALL PitchControl(avrSWAP, CntrPar, LocalVar, objInst, DebugVar, ErrVar)
      
      IF (CntrPar%Y_ControlMode > 0) THEN
          CALL YawRateControl(avrSWAP, CntrPar, LocalVar, objInst, DebugVar, ErrVar)
      END IF
      
      IF (CntrPar%Flp_Mode > 0) THEN
          CALL FlapControl(avrSWAP, CntrPar, LocalVar, objInst)
      END IF

      ! Cable control
      IF (CntrPar%CC_Mode > 0) THEN
          CALL CableControl(avrSWAP,CntrPar,LocalVar, objInst, ErrVar)
      END IF

      ! Structural control
      IF (CntrPar%StC_Mode > 0) THEN
          CALL StructuralControl(avrSWAP,CntrPar,LocalVar, objInst, ErrVar)
      END IF
      
      IF ( CntrPar%LoggingLevel > 0 ) THEN
          CALL Debug(LocalVar, CntrPar, DebugVar, ErrVar, avrSWAP, RootName, SIZE(avcOUTNAME))
      END IF 
    ELSEIF ((LocalVar%iStatus == -1) .AND. (CntrPar%ZMQ_Mode > 0)) THEN
            CALL UpdateZeroMQ(LocalVar, CntrPar, ErrVar)
    END IF
    do i = 1, library_data%n_offers
      output_array(i) = real(avrSWAP(library_data%swap_to_output_map(i)), kind=wp)
    end do

    ! Add RoutineName to error message
    IF (ErrVar%aviFAIL < 0) THEN
      ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
      error = fail(ErrVar%ErrMsg)
      exit try
    ENDIF
    return
  end block try
  ! if we are here, then there has been an error: report it to the caller
  error_string = error%to_chars()

  end subroutine
  
  subroutine terminate(context, error_string) bind(C, name="terminate")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::terminate
  #endif
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  end subroutine
end module
!!  test_dll.f90 
!!
!!  FUNCTIONS/SUBROUTINES exported from test_dll.dll:
!!  test_dll_init - subroutine
!!  test_dll_update - subroutine
!!  test_dll_terminate - subroutine
!
module dispatch_interface

  use extf, only: itoa
  use dispatch_types, only: wp
  use dispatch_manager
  use error_handling, only: error_t, fail, wrap_error

  implicit none
  
  ! converted-real-kind copies of interface arrays for compatibility with dispatch
  integer :: n_input, n_output
  real(wp), allocatable :: input_buffer(:)
  real(wp), allocatable :: output_buffer(:)
  ! interface-level error handling
  class(error_t), allocatable :: error

  contains
  
!----------------------------------------------------------------------
! hawc_dll interface for the dispatch library
! https://hawc2.pages.windenergy.dtu.dk/documentation/hawc2-manual/09-dll_control.htm
!----------------------------------------------------------------------
  subroutine sample_dispatch_hawc_dll(n1,array1,n2,array2)
  !! This subroutine is to be sampled at each step (and possibly iteration) of the calling simulation software
  !! If the calling simulation software calls sample_dispatch_hawc_dll at every solver iteration, then 
  !! this code needs to be modified so that dispatch_sample() is called only on the first iteration of 
  !! each simulation time step (simulation time needs to be passed from the caller somewhere in array1,
  !! and dispatch_sample() called only when simulation time has changed)
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT, ALIAS:'sample_dispatch_hawc_dll'::sample_dispatch_hawc_dll
  !gcc$ attributes DLLEXPORT :: sample_dispatch_hawc_dll
  !gcc$ attributes cdecl :: sample_dispatch_hawc_dll
  #endif
  integer*4      :: n1, &  ! Dummy integer value containing the array size of array1
                    n2     ! Dummy integer value containing the array size of array2
  real*4, intent(in   ) :: array1(*)  ! fixed-length array, data from HAWC2 to DLL 
  real*4, intent(  out) :: array2(*)  ! fixed-length array, data from DLL to HAWC2
  integer :: i
  
  try: block
    ! error if input/output size mismatch
    if (n1 .ne. n_input) then
      error = fail("Input array size mismatch: " // itoa(n1) // " elements specified in caller; " // itoa(n_input) // " elements specified in dispatch.")
      exit try
    end if
    if (n2 .ne. n_output) then
      error = fail("Output array size mismatch: " // itoa(n2) // " elements specified in caller; " // itoa(n_output) // " elements specified in dispatch.")
      exit try
    end if
  
    ! copy input buffer
    do i = 1,n1
      input_buffer(i) = real(array1(i), kind=wp)
    end do
  
    ! sample signal processing libraries
    call dispatch_sample(input_buffer, output_buffer, error)
    if (allocated(error)) then
      call wrap_error(error, "Failure in sample_dispatch_hawc_dll()'s call to dispatch_sample().")
      exit try
    end if
  
    ! copy output buffer
    do i = 1,n2
      array2(i) = real(output_buffer(i), kind=4)
    end do
    
    return
  end block try
  ! for the hawc_dll interface there is no error mechanism, so just pass the error via message
  ! print it here too, in case the message gets truncated
  print *, error%to_chars()
  end subroutine sample_dispatch_hawc_dll

  subroutine initialize_dispatch_hawc_dll(string256)
  !! This subroutine performs initialization of dispatch and all invoked signal processing libraries  
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT, ALIAS:'initialize_dispatch_hawc_dll'::initialize_dispatch_hawc_dll
  !gcc$ attributes DLLEXPORT :: initialize_dispatch_hawc_dll
  !gcc$ attributes cdecl :: initialize_dispatch_hawc_dll
  #endif
  character*256 :: string256

  try: block
    ! initialize the dispatch framework according to the specifications in the input file whose path
    ! is contained in string256
    call dispatch_initialize(string256, n_input, n_output, error)
    if (allocated(error)) then
      call wrap_error(error, "Failure in initialize_dispatch_hawc_dll()'s call to dispatch_initialize().")
      exit try
    end if
    allocate(input_buffer(n_input), source=0.0_wp)
    allocate(output_buffer(n_output), source=0.0_wp)
    return
  end block try
  ! for the hawc_dll interface there is no error mechanism, so just pass the error via message
  ! print it here too, in case the message gets truncated
  print *, error%to_chars()
  end subroutine initialize_dispatch_hawc_dll

  subroutine dispatch_message_hawc_dll(string256)
  !! This subroutine sends a 256 character message from dispatch to the calling simulation program (not implemented)
  use iso_c_binding, only: C_NULL_CHAR
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT, ALIAS:'dispatch_message_hawc_dll'::dispatch_message_hawc_dll
  !gcc$ attributes DLLEXPORT :: dispatch_message_hawc_dll
  !gcc$ attributes cdecl :: dispatch_message_hawc_dll
  #endif
  character*256 :: string256
  character(len=:), allocatable :: error_message
  integer :: i, error_message_len

  if (allocated(error)) then
    error_message = error%to_chars()
    error_message_len = len_trim(error_message)
    ! truncate the message and allow for null-character string termination
    if (error_message_len .ge. 256) error_message_len = 255
    ! copy as much of the error message as possible
    do i = 1,min(256, error_message_len)
      string256(i:i) = error_message(i:i)
    end do
    ! pad the end of the string with null characters in case it contained other information
    if (error_message_len < 256) then
      do i = error_message_len+1, 256
        string256(i:i) = C_NULL_CHAR
      end do
    end if
  end if

  end subroutine dispatch_message_hawc_dll
!----------------------------------------------------------------------
! end hawc_dll interface
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! hawc type2 dll interface for the dispatch library
! https://hawc2.pages.windenergy.dtu.dk/documentation/hawc2-manual/09-dll_control.htm
!----------------------------------------------------------------------
  subroutine sample_dispatch_type2_dll(array1,array2) bind(C, name="sample_dispatch_type2_dll")
  !! This subroutine is to be sampled at each time step of the calling simulation software.
  !! Input and output array sizes are fixed at compile-time; if larger arrays are needed,
  !! dispatch will need to be recompiled.
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: sample_dispatch_type2_dll
  !gcc$ attributes DLLEXPORT :: sample_dispatch_type2_dll
  !gcc$ attributes cdecl :: sample_dispatch_type2_dll
  #endif
  
  real*8 :: array1(256) ! fixed-length array, data from HAWC2 to DLL
  real*8 :: array2(256) ! fixed-length array, data from DLL to HAWC2
  
  call dispatch_sample(array1, array2, error)
  if (allocated(error)) then
    call wrap_error(error, "Failure in sample_dispatch_type2_dll()'s call to dispatch_sample().")
    ! print the error message in case the message-passing mechanism doesn't work
    print *, error%to_chars()
  end if
  
  end subroutine sample_dispatch_type2_dll
  
  subroutine initialize_dispatch_type2_dll(array1,array2) bind(C, name="initialize_dispatch_type2_dll")
  !! This subroutine does nothing  
  !! Input and output array sizes are fixed at compile-time; if larger arrays are needed,
  !! dispatch will need to be recompiled.
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: initialize_dispatch_type2_dll
  !gcc$ attributes DLLEXPORT :: initialize_dispatch_type2_dll
  !gcc$ attributes cdecl :: initialize_dispatch_type2_dll
  #endif
  real*8 :: array1(1) ! fixed-length array, data from HAWC2 to DLL
  real*8 :: array2(1) ! fixed-length array, data from DLL to HAWC2

  end subroutine initialize_dispatch_type2_dll
  
  subroutine dispatch_message_type2_dll(string256) bind(C, name="dispatch_message_type2_dll")
  !! This subroutine sends a 256 character message from dispatch to the calling simulation program (not implemented)
  use iso_c_binding, only: C_CHAR, C_NULL_CHAR
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: dispatch_message_type2_dll
  !gcc$ attributes DLLEXPORT :: dispatch_message_type2_dll
  !gcc$ attributes cdecl :: dispatch_message_type2_dll
  #endif
  character(len=:), allocatable :: error_message
  character(kind=C_CHAR) :: string256(256)
  integer :: i, error_message_len
  
  if (allocated(error)) then
    error_message = error%to_chars()
    error_message_len = len_trim(error_message)
    ! truncate the message and allow for null-character string termination
    if (error_message_len .ge. 256) error_message_len = 255
    ! copy as much of the error message as possible
    do i = 1,min(256, error_message_len)
      string256(i) = error_message(i:i)
    end do
    ! pad the end of the string with null characters in case it contained other information
    if (error_message_len < 256) then
      do i = error_message_len+1, 256
        string256(i) = C_NULL_CHAR
      end do
    end if
  end if
  end subroutine dispatch_message_type2_dll

  subroutine get_version(string256) bind(C, name="get_version")
  !! This subroutine sends a null-terminated version string from dispatch to the calling program in
  !! a 256-character container
  use iso_c_binding, only: C_CHAR, C_NULL_CHAR
  use dispatch_manager, only: dispatch_version
  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: get_version
  !gcc$ attributes DLLEXPORT :: get_version
  !gcc$ attributes CDECL :: get_version
  #endif
  character(kind=C_CHAR), intent(inout) :: string256(256)
  character(len=:), allocatable :: version_string
  integer :: i, version_string_len
  
  ! get the dispatch version and transfer it to the supplied container, terminated with a null character
  version_string = dispatch_version()
  version_string_len = len_trim(version_string)
  if (version_string_len >= 256) then
    error = fail("Version string exceeds allocated container length (256); truncating to fit container.")
    version_string_len = 255
  end if
  do i = 1, version_string_len
    string256(i) = version_string(i:i)
  end do
  string256(version_string_len + 1) = C_NULL_CHAR
  end subroutine get_version
 
  subroutine initstring(istring) bind(C, name='initstring')
  !! This subroutine performs initialization of dispatch and all invoked signal processing libraries
  !! The HawC2 manual does not specify the size of the istring container, but alludes that it holds
  !! at least 256 integers.
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: initstring
  !gcc$ attributes DLLEXPORT :: initstring
  !gcc$ attributes CDECL :: initstring
  #endif
  use iso_c_binding, only: C_CHAR, C_NULL_CHAR  
  implicit none
  !! Input from HAWC2.
  !! The string is passed in decimal format.
  !! This is how the interface is defined in HAWC2 and must not be changed.
  integer(kind=1), dimension(*), intent(in) :: istring
  
  ! local variables
  integer :: string_size
  character(kind=C_CHAR, len=1), dimension(:), allocatable :: char_array
  character(kind=C_CHAR, len=:), allocatable :: string
  
  ! transfer the integer array to character string
  string_size = 0
  do while (string_size < 256) ! istring might not be null-terminated; hawc2 manual suggests its container has >= 256 elements
    if (istring(string_size + 1) >= 32) then ! ascii codes <32 are non-printing; if istring(i) is <32, then the string is over
      string_size = string_size + 1
    else
      exit
    end if
  end do
  allocate(char_array(string_size), mold=C_NULL_CHAR)
  char_array = char(istring(1:string_size))
  string = repeat(C_NULL_CHAR, string_size)
  string = transfer(char_array(1:string_size), string)
  
  try: block
    ! initialize dispatch
    call dispatch_initialize(string, n_input, n_output, error)
    if (allocated(error)) then
      call wrap_error(error, "Failure in initstring()'s call to dispatch_initialize().")
      exit try
    end if
  
    ! there is no need to allocate input and output buffers because the real(8) inputs to
    ! sample_dispatch_type2_dll() are the same kind as dispatch_signal_kind. Signal an error if
    ! the fixed-size input and output from sample_dispatch_type_2_dll() are not big enough.
    if (n_input > 256) then
      error = fail("Insufficient input array size: sample_dispatch_type_2_dll() was compiled with 256-element arrays; dispatch requires " // itoa(n_input) // " elements.")
      exit try
    end if
    if (n_output > 256) then
      error = fail("Insufficient output array size: sample_dispatch_type_2_dll() was compiled with 256-element arrays; dispatch requires " // itoa(n_output) // " elements.")
      exit try
    end if
    return
  end block try
  ! for the hawc type2 dll interface there is no error mechanism, so just pass the error via message
  ! print it here too, in case the message gets truncated
  print *, error%to_chars()
  end subroutine initstring
!----------------------------------------------------------------------
! end hawc type2 dll interface
!----------------------------------------------------------------------
  
!----------------------------------------------------------------------
! bladed dll interface for the dispatch library
!----------------------------------------------------------------------
  subroutine DISCON(avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG) bind (C, NAME='DISCON')
  use, intrinsic  :: iso_c_binding
  use dispatch_types, only: signal_descriptor_t

  implicit none
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT :: DISCON
  !GCC$ ATTRIBUTES DLLEXPORT :: DISCON
  #endif

  !------------------------------------------------------------------------------------------------------------------------------
  ! Variable declaration and initialization
  !------------------------------------------------------------------------------------------------------------------------------

  ! Passed Variables:
  real(4),                intent(inout)   :: avrSWAP(*)                       ! The swap array, used to pass data to, and receive data from, the DLL controller.
  integer(C_INT),         intent(inout)   :: aviFAIL                          ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
  character(kind=C_CHAR), intent(in   )   :: accINFILE(nint(avrSWAP(50)))     ! The name of the parameter input file
  character(kind=C_CHAR), intent(in   )   :: avcOUTNAME(nint(avrSWAP(51)))    ! OUTNAME (Simulation RootName)
  character(kind=C_CHAR), intent(inout)   :: avcMSG(nint(avrSWAP(49)))        ! MESSAGE (Message from DLL to simulation code [ErrMsg])  The message which will be displayed by the calling program if aviFAIL <> 0.
  ! local variables
  character(kind=C_CHAR, len=:), allocatable :: input_file, error_message
  integer :: i, string_size, swap_size, lut_record
  integer :: start_record_for_torque_speed_lut, n_points_in_torque_speed_lookup_table
  integer :: max_message_len_char, max_message_len_real, error_message_len
  integer :: min_logging_start_record, start_record_for_logging, max_logged_variables
  type(signal_descriptor_t) :: swap_signals(3500)
  integer, allocatable, save :: input_indices(:), output_indices(:)

  try: block
    ! set call status as successful until an error is generated
    aviFAIL = 0
    ! initialize if avrSWAP(1) == 1
    if (nint(avrSWAP(1)) .eq. 0) then
      ! initialize descriptions of the fixed elements of the swap array
      swap_signals(1:164) = (/ &
        signal_descriptor_t("status_flag",                           "i", "rw", "-",            "Status flag set as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation"), &
        signal_descriptor_t("current_time",                          "r", "r-", "s",            "Current time"), &
        signal_descriptor_t("communication_interval",                "r", "r-", "s",            "Communication interval"), &
        signal_descriptor_t("blade_1_pitch_angle",                   "r", "r-", "rad",          "Blade 1 pitch angle"), &
        signal_descriptor_t("below_rated_pitch_angle_setpoint",      "r", "r-", "rad",          "Below-rated pitch angle set-point"), &
        signal_descriptor_t("minimum_pitch_angle",                   "r", "r-", "rad",          "Minimum pitch angle"), &
        signal_descriptor_t("maximum_pitch_angle",                   "r", "r-", "rad",          "Maximum pitch angle"), &
        signal_descriptor_t("minimum_pitch_rate",                    "r", "r-", "rad/s",        "Minimum pitch rate (most negative value allowed)"), &
        signal_descriptor_t("maximum_pitch_rate",                    "r", "r-", "rad/s",        "Maximum pitch rate"), &
        signal_descriptor_t("pitch_actuator_type",                   "i", "r-", "-",            "0 = pitch position actuator, 1 = pitch rate actuator"), &
        signal_descriptor_t("current_demanded_pitch_angle",          "r", "r-", "rad",          "Current demanded pitch angle"), &
        signal_descriptor_t("current_demanded_pitch_rate",           "r", "r-", "rad/s",        "Current demanded pitch rate"), &
        signal_descriptor_t("demanded_power",                        "r", "r-", "W",            "Demanded power"), &
        signal_descriptor_t("measured_shaft_power",                  "r", "r-", "W",            "Measured shaft power"), &
        signal_descriptor_t("measured_electrical_power_output",      "r", "r-", "W",            "Measured electrical power output"), &
        signal_descriptor_t("optimal_mode_gain",                     "r", "r-", "Nm/(rad/s)^2", "Optimal mode gain"), &
        signal_descriptor_t("minimum_generator_speed",               "r", "r-", "rad/s",        "Minimum generator speed"), &
        signal_descriptor_t("optimal_mode_maximum_speed",            "r", "r-", "rad/s",        "Optimal mode maximum speed"), &
        signal_descriptor_t("demanded_generator_speed_above_rated",  "r", "r-", "rad/s",        "Demanded generator speed above rated"), &
        signal_descriptor_t("measured_generator_speed",              "r", "r-", "rad/s",        "Measured generator speed"), &
        signal_descriptor_t("measured_rotor_speed",                  "r", "r-", "rad/s",        "Measured rotor speed"), &
        signal_descriptor_t("demanded_generator_torque_above_rated", "r", "r-", "Nm",           "Demanded generator torque above rated"), &
        signal_descriptor_t("measured_generator_torque",             "r", "r-", "Nm",           "Measured generator torque"), &
        signal_descriptor_t("measured_yaw_error",                    "r", "r-", "rad",          "Measured yaw error"), &
        signal_descriptor_t("start_record_for_torque_speed_lut",     "r", "r-", "-",            "Start of below-rated torque-speed look-up table=R"), &
        signal_descriptor_t("n_points_in_torque_speed_lookup_table", "r", "r-", "-",            "No. of points in torque-speed look-up table"), &
        signal_descriptor_t("hub_wind_speed",                        "r", "r-", "m/s",          "Hub wind speed"), &
        signal_descriptor_t("individual_pitch_control_mode",         "i", "r-", "-",            "Pitch control: 0 = collective, 1 = individual"), &
        signal_descriptor_t("yaw_control_mode",                      "i", "r-", "-",            "Yaw control: 0 = yaw rate control, 1 = yaw torque control"), &
        signal_descriptor_t("blade_1_root_oop_bending_moment",       "r", "r-", "Nm",           "Blade 1 root out-of-plane bending moment"), &
        signal_descriptor_t("blade_2_root_oop_bending_moment",       "r", "r-", "Nm",           "Blade 2 root out-of-plane bending moment"), &
        signal_descriptor_t("blade_3_root_oop_bending_moment",       "r", "r-", "Nm",           "Blade 3 root out-of-plane bending moment"), &
        signal_descriptor_t("blade_2_pitch_angle",                   "r", "r-", "rad",          "Blade 2 pitch angle"), &
        signal_descriptor_t("blade_3_pitch_angle",                   "r", "r-", "rad",          "Blade 3 pitch angle"), &
        signal_descriptor_t("generator_contactor",                   "i", "rw", "-",            "Generator contactor"), &
        signal_descriptor_t("shaft_brake_status",                    "i", "rw", "-",            "Shaft brake status: 0=off, 1=Brake 1 on"), &
        signal_descriptor_t("nacelle_yaw_angle_from_north",          "r", "r-", "rad",          "Nacelle yaw angle from North"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("demanded_yaw_actuator_torque",          "r", "r-", "Nm",           "Demanded yaw actuator torque"), &
        signal_descriptor_t("demanded_blade_1_pitch_angle_or_rate",  "r", "-w", "rad or rad/s", "Demanded blade 1 individual pitch position or rate"), &
        signal_descriptor_t("demanded_blade_2_pitch_angle_or_rate",  "r", "-w", "rad or rad/s", "Demanded blade 2 individual pitch position or rate"), &
        signal_descriptor_t("demanded_blade_3_pitch_angle_or_rate",  "r", "-w", "rad or rad/s", "Demanded blade 3 individual pitch position or rate"), &
        signal_descriptor_t("demanded_collective_pitch_angle",       "r", "-w", "rad",          "Demanded pitch angle (Collective pitch)"), &
        signal_descriptor_t("demanded_collective_pitch_rate",        "r", "-w", "rad/s",        "Demanded pitch rate (Collective pitch)"), &
        signal_descriptor_t("demanded_generator_torque",             "r", "-w", "Nm",           "Demanded generator torque"), &
        signal_descriptor_t("demanded_yaw_rate",                     "r", "-w", "rad/s",        "Demanded nacelle yaw rate"), &
        signal_descriptor_t("message_length",                        "i", "rw", "-",            "Message length (when written to the caller); max chars allowed in message when read from the caller"), &
        signal_descriptor_t("n_chars_in_infile",                     "i", "r-", "-",            "Number of characters in the 'INFILE'  argument"), &
        signal_descriptor_t("n_chars_in_outname",                    "i", "r-", "-",            "Number of characters in the 'OUTNAME' argument"), &
        signal_descriptor_t("discon_interface_version_number",       "i", "r-", "-",            "DLL interface version number (reserved for future use)"), &
        signal_descriptor_t("tower_top_fore_aft_acceleration",       "r", "r-", "m/s^2",        "Tower top fore-aft     acceleration"), &
        signal_descriptor_t("tower_top_side_side_acceleration",      "r", "r-", "m/s^2",        "Tower top side-to-side acceleration"), &
        signal_descriptor_t("pitch_override",                        "i", "-w", "-",            "Pitch override (it is unclear what this means)"), &
        signal_descriptor_t("torque_override",                       "i", "-w", "-",            "Torque override (it is unclear what this means)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("rotor_azimuth",                         "r", "r-", "rad",          "Rotor azimuth angle"), &
        signal_descriptor_t("n_blade",                               "i", "r-", "-",            "Number of blades"), &
        signal_descriptor_t("max_num_values_for_logging",            "i", "r-", "-",            "Maximum number of values which can be returned for logging"), &
        signal_descriptor_t("start_record_for_logging",              "i", "r-", "-",            "Record number for start of logging output"), &
        signal_descriptor_t("max_chars_in_outname",                  "i", "r-", "-",            "Maximum number of characters which can be returned in 'OUTNAME'"), &
        signal_descriptor_t("n_variables_returned_for_logging",      "i", "-w", "-",            "Number of variables returned for logging"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-" ,           "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-" ,           "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-" ,           "Reserved (not used)"), &
        signal_descriptor_t("blade_1_root_in_plane_bending_moment",  "r", "r-", "Nm",           "Blade 1 root in-plane bending moment"), &
        signal_descriptor_t("blade_2_root_in_plane_bending_moment",  "r", "r-", "Nm",           "Blade 2 root in-plane bending moment"), &
        signal_descriptor_t("blade_3_root_in_plane_bending_moment",  "r", "r-", "Nm",           "Blade 3 root in-plane bending moment"), &
        signal_descriptor_t("generator_start_up_resistance",         "r", "-w", "ohm/phase",    "Generator start-up resistance"), &
        signal_descriptor_t("rotating_hub_my",                       "r", "r-", "Nm",           "Rotating hub My (GL co-ords)"), &
        signal_descriptor_t("rotating_hub_mz",                       "r", "r-", "Nm",           "Rotating hub Mz (GL co-ords)"), &
        signal_descriptor_t("fixed_hub_my",                          "r", "r-", "Nm",           "Fixed hub    My (GL co-ords)"), &
        signal_descriptor_t("fixed_hub_mz",                          "r", "r-", "Nm",           "Fixed hub    Mz (GL co-ords)"), &
        signal_descriptor_t("yaw_bearing_my",                        "r", "r-", "Nm",           "Yaw bearing  My (GL co-ords)"), &
        signal_descriptor_t("yaw_bearing_mz",                        "r", "r-", "Nm",           "Yaw bearing  Mz (GL co-ords)"), &
        signal_descriptor_t("request_for_loads",                     "i", "-w", "-",            "Request for loads (unclear what this means)"), &
        signal_descriptor_t("variable_slip_current_demand_on_off",   "i", "-w", "-",            "1 = Activate variable slip current demand in the caller"), &
        signal_descriptor_t("variable_slip_current_demand",          "r", "-w", "A",            "Variable slip current demand"), &
        signal_descriptor_t("nacelle_roll_acceleration",             "r", "r-", "rad/s^2",      "Nacelle roll    acceleration"), &
        signal_descriptor_t("nacelle_nodding_acceleration",          "r", "r-", "rad/s^2",      "Nacelle nodding acceleration"), &
        signal_descriptor_t("nacelle_yaw_acceleration",              "r", "r-", "rad/s^2",      "Nacelle yaw     acceleration"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("realtime_sim_time_step",                "r", "r-", "s",            "Real time simulation time step"), &
        signal_descriptor_t("realtime_sim_time_step_multiplier",     "r", "r-", "-",            "Real rime simulation time step multiplier"), &
        signal_descriptor_t("mean_wind_speed_increment",             "r", "-w", "m/s",          "Mean wind speed increment"), &
        signal_descriptor_t("turbulence_intensity_increment",        "r", "-w", "%",            "Turbulence intensity increment"), &
        signal_descriptor_t("wind_direction_increment",              "r", "-w", "rad",          "Wind direction increment"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("safety_system_number_activated",        "i", "r-", "-",            "Safety system number that has been activated"), &
        signal_descriptor_t("safety_system_number_to_activate",      "i", "-w", "-",            "Safety system number to activate"), &
        signal_descriptor_t("reserved",                              "i", "r-", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "i", "r-", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-",            "Reserved (not used)"), &
        signal_descriptor_t("yaw_control_flag",                      "i", "-w", "-",            "Yaw control flag (meaning: see next 2 signals)"), &
        signal_descriptor_t("yaw_stiffness",                         "r", "-w", "-",            "Yaw stiffness if yaw_control_flag = 1 or 3 (units unclear)"), &
        signal_descriptor_t("yaw_damping",                           "r", "-w", "-",            "Yaw damping if yaw_control_flag = 2 or 3 (units unclear)"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "r", "r-", "-",            "Reserved (not used)"), &
        signal_descriptor_t("brake_torque_demand",                   "r", "-w", "Nm",           "Brake torque demand"), &
        signal_descriptor_t("yaw_brake_torque_demand",               "r", "-w", "Nm",           "Yaw brake torque demand"), &
        signal_descriptor_t("shaft_torque",                          "r", "r-", "Nm",           "Shaft torque (= hub Mx for clockwise rotor)"), &
        signal_descriptor_t("hub_fixed_fx",                          "r", "r-", "N",            "Hub Fixed Fx"), &
        signal_descriptor_t("hub_fixed_fy",                          "r", "r-", "N",            "Hub Fixed Fy"), &
        signal_descriptor_t("hub_fixed_fz",                          "r", "r-", "N",            "Hub Fixed Fz"), &
        signal_descriptor_t("network_voltage_disturbance_factor",    "r", "r-", "-",            "Network voltage disturbance factor"), &
        signal_descriptor_t("network_frequency_disturbance_factor",  "r", "r-", "-",            "Network frequency disturbance factor"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("controller_state",                      "i", "r-", "-",            "Controller state"), &
        signal_descriptor_t("settling_time",                         "r", "r-", "s",            "Settling time (time to start writing output)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("user_defined_signal_01",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_02",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_03",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_04",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_05",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_06",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_07",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_08",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_09",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("user_defined_signal_10",                "r", "rw", "-",            "User-defined variables 1 to 10"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("teeter_angle",                          "r", "r-", "rad",          "Teeter angle"), &
        signal_descriptor_t("teeter_velocity",                       "r", "r-", "rad/s",        "Teeter velocity"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("reserved",                              "-", "--", "-",            "Reserved (not used)"), &
        signal_descriptor_t("controller_failure_flag",               "i", "r-", "-",            "Controller failure flag"), &
        signal_descriptor_t("yaw_bearing_angular_position",          "i", "r-", "rad",          "Yaw bearing angular position"), &
        signal_descriptor_t("yaw_bearing_angular_velocity",          "i", "r-", "rad/s",        "Yaw bearing angular velocity"), &
        signal_descriptor_t("yaw_bearing_angular_acceleration",      "i", "r-", "rad/s^2",      "Yaw bearing angular acceleration") &
        /)
      swap_signals(1001:1066) = (/ &
        signal_descriptor_t("platform_position_x",                   "r", "r-", "m",            "Platform position x)"), &
        signal_descriptor_t("platform_position_y",                   "r", "r-", "m",            "Platform position y)"), &
        signal_descriptor_t("platform_position_z",                   "r", "r-", "m",            "Platform position z)"), &
        signal_descriptor_t("platform_rotation_x",                   "r", "r-", "rad",          "Platform rotation x)"), &
        signal_descriptor_t("platform_rotation_y",                   "r", "r-", "rad",          "Platform rotation y)"), &
        signal_descriptor_t("platform_rotation_z",                   "r", "r-", "rad",          "Platform rotation z)"), &
        signal_descriptor_t("platform_velocity_x",                   "r", "r-", "m/s",          "Platform velocity x)"), &
        signal_descriptor_t("platform_velocity_y",                   "r", "r-", "m/s",          "Platform velocity y)"), &
        signal_descriptor_t("platform_velocity_z",                   "r", "r-", "m/s",          "Platform velocity z)"), &
        signal_descriptor_t("platform_rotational_velocity_x",        "r", "r-", "rad/s",        "Platform rotational velocity x)"), &
        signal_descriptor_t("platform_rotational_velocity_y",        "r", "r-", "rad/s",        "Platform rotational velocity y)"), &
        signal_descriptor_t("platform_rotational_velocity_z",        "r", "r-", "rad/s",        "Platform rotational velocity z)"), &
        signal_descriptor_t("platform_acceleration_x",               "r", "r-", "m/s^2",        "Platform acceleration x)"), &
        signal_descriptor_t("platform_acceleration_y",               "r", "r-", "m/s^2",        "Platform acceleration y)"), &
        signal_descriptor_t("platform_acceleration_z",               "r", "r-", "m/s^2",        "Platform acceleration z)"), &
        signal_descriptor_t("platform_rotational_acceleration_x",    "r", "r-", "rad/s^2",      "Platform rotational acceleration x)"), &
        signal_descriptor_t("platform_rotational_acceleration_y",    "r", "r-", "rad/s^2",      "Platform rotational acceleration y)"), &
        signal_descriptor_t("platform_rotational_acceleration_z",    "r", "r-", "rad/s^2",      "Platform rotational acceleration z)"), &
        signal_descriptor_t("ed_q_platform_surge",                   "r", "r-", "m",            "ElastoDyn platform surge state"), &
        signal_descriptor_t("ed_q_platform_sway",                    "r", "r-", "m",            "ElastoDyn platform sway state"),  &
        signal_descriptor_t("ed_q_platform_heave",                   "r", "r-", "m",            "ElastoDyn platform heave state"), &
        signal_descriptor_t("ed_q_platform_roll",                    "r", "r-", "rad",          "ElastoDyn platform roll state"),  &
        signal_descriptor_t("ed_q_platform_pitch",                   "r", "r-", "rad",          "ElastoDyn platform pitch state"), &
        signal_descriptor_t("ed_q_platform_yaw",                     "r", "r-", "rad",          "ElastoDyn platform yaw state"),   &
        signal_descriptor_t("ed_q_tower_1_fore_aft",                 "r", "r-", "m",            "ElastoDyn 1st tower fore-aft mode state"),     &
        signal_descriptor_t("ed_q_tower_1_side_side",                "r", "r-", "m",            "ElastoDyn 1st tower side-to-side mode state"), &
        signal_descriptor_t("ed_q_tower_2_fore_aft",                 "r", "r-", "m",            "ElastoDyn 2nd tower fore-aft mode state"),     &
        signal_descriptor_t("ed_q_tower_2_side-side",                "r", "r-", "m",            "ElastoDyn 2nd tower side-to-side mode state"), &
        signal_descriptor_t("ed_q_nacelle_yaw",                      "r", "r-", "rad",          "ElastoDyn nacelle-yaw state"), &
        signal_descriptor_t("ed_q_rotor_furl",                       "r", "r-", "rad",          "ElastoDyn rotor-furl state"), &
        signal_descriptor_t("ed_q_generator_azimuth",                "r", "r-", "rad",          "ElastoDyn the generator azimuth state"), &
        signal_descriptor_t("ed_q_drive_train_torsion",              "r", "r-", "rad",          "ElastoDyn drivetrain rotational-flexibility state"), &
        signal_descriptor_t("ed_q_tail_furl",                        "r", "r-", "rad",          "ElastoDyn tail-furl state"), &
        signal_descriptor_t("ed_q_blade_1_flap_1",                   "r", "r-", "m",            "ElastoDyn 1st blade flap mode for blade 1 state"), &
        signal_descriptor_t("ed_q_blade_1_edge_1",                   "r", "r-", "m",            "ElastoDyn 1st blade edge mode for blade 1 state"), &
        signal_descriptor_t("ed_q_blade_1_flap_2",                   "r", "r-", "m",            "ElastoDyn 2nd blade flap mode for blade 1 state"), &
        signal_descriptor_t("ed_q_blade_2_flap_1",                   "r", "r-", "m",            "ElastoDyn 1st blade flap mode for blade 2 state"), &
        signal_descriptor_t("ed_q_blade_2_edge_1",                   "r", "r-", "m",            "ElastoDyn 1st blade edge mode for blade 2 state"), &
        signal_descriptor_t("ed_q_blade_2_flap_2",                   "r", "r-", "m",            "ElastoDyn 2nd blade flap mode for blade 2 state"), &
        signal_descriptor_t("ed_q_blade_3_flap_1",                   "r", "r-", "m",            "ElastoDyn 1st blade flap mode for blade 3 state"), &
        signal_descriptor_t("ed_q_blade_3_edge_1",                   "r", "r-", "m",            "ElastoDyn 1st blade edge mode for blade 3 state"), &
        signal_descriptor_t("ed_q_blade_3_flap_2",                   "r", "r-", "m",            "ElastoDyn 2nd blade flap mode for blade 3 state"), &
        signal_descriptor_t("ed_qd_platform_surge",                  "r", "r-", "m/s",          "ElastoDyn platform surge state time derivative"),  &
        signal_descriptor_t("ed_qd_platform_sway",                   "r", "r-", "m/s",          "ElastoDyn platform sway state time derivative"),   &
        signal_descriptor_t("ed_qd_platform_heave",                  "r", "r-", "m/s",          "ElastoDyn platform heave state time derivative"),  &
        signal_descriptor_t("ed_qd_platform_roll",                   "r", "r-", "rad/s",        "ElastoDyn platform roll state time derivative"),   &
        signal_descriptor_t("ed_qd_platform_pitch",                  "r", "r-", "rad/s",        "ElastoDyn platform pitch state time derivative"),  &
        signal_descriptor_t("ed_qd_platform_yaw",                    "r", "r-", "rad/s",        "ElastoDyn platform yaw state time derivative"),    &
        signal_descriptor_t("ed_qd_tower_1_fore_aft",                "r", "r-", "m/s",          "ElastoDyn 1st tower fore-aft mode state time derivative"),     &
        signal_descriptor_t("ed_qd_tower_1_side_side",               "r", "r-", "m/s",          "ElastoDyn 1st tower side-to-side mode state time derivative"), &
        signal_descriptor_t("ed_qd_tower_2_fore_aft",                "r", "r-", "m/s",          "ElastoDyn 2nd tower fore-aft mode state time derivative"),     &
        signal_descriptor_t("ed_qd_tower_2_side_side",               "r", "r-", "m/s",          "ElastoDyn 2nd tower side-to-side mode state time derivative"), &
        signal_descriptor_t("ed_qd_nacelle_yaw",                     "r", "r-", "rad/s",        "ElastoDyn nacelle-yaw state time derivative"), &
        signal_descriptor_t("ed_qd_rotor_furl",                      "r", "r-", "rad/s",        "ElastoDyn rotor-furl state time derivative"),  &
        signal_descriptor_t("ed_qd_generator_azimuth",               "r", "r-", "rad/s",        "ElastoDyn the generator azimuth state time derivative"), &
        signal_descriptor_t("ed_qd_drive_train_torsion",             "r", "r-", "rad/s",        "ElastoDyn drivetrain rotational-flexibility state time derivative"), &
        signal_descriptor_t("ed_qd_tail_furl",                       "r", "r-", "rad/s",        "ElastoDyn tail-furl state time derivative"), &
        signal_descriptor_t("ed_qd_blade_1_flap_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade flap mode for blade 1 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_1_edge_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade edge mode for blade 1 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_1_flap_2",                  "r", "r-", "m/s",          "ElastoDyn 2nd blade flap mode for blade 1 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_2_flap_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade flap mode for blade 2 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_2_edge_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade edge mode for blade 2 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_2_flap_2",                  "r", "r-", "m/s",          "ElastoDyn 2nd blade flap mode for blade 2 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_3_flap_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade flap mode for blade 3 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_3_edge_1",                  "r", "r-", "m/s",          "ElastoDyn 1st blade edge mode for blade 3 state time derivative"), &
        signal_descriptor_t("ed_qd_blade_3_flap_2",                  "r", "r-", "m/s",          "ElastoDyn 2nd blade flap mode for blade 3 state time derivative")  &
        /)
      ! read the special swap array elements to figure out the total size of the swap array
      start_record_for_torque_speed_lut = nint(avrSWAP(25))
      n_points_in_torque_speed_lookup_table = nint(avrSWAP(26))
      ! check that start_record_for_torque_speed_lut is valid
      if (n_points_in_torque_speed_lookup_table .gt. 0) then
        if (start_record_for_torque_speed_lut .le. 164) then
          error = fail("Erroneous value for 'Start of below-rated torque-speed look-up table': " // &
                       itoa(start_record_for_torque_speed_lut) // " specified, but value must be > 164.")
          exit try
        end if
      end if
      ! get message length constraints
      max_message_len_char = nint(avrSWAP(49))
      max_message_len_real = ceiling(avrSWAP(49) / 4) ! a message can be passed via the swap array, 4 characters per swap element
      ! get signal logging parameters
      start_record_for_logging = nint(avrSWAP(63))
      max_logged_variables = nint(avrSWAP(62))
      if ((start_record_for_logging + max_logged_variables - 1) .gt. 1000) then
        error = fail("Too many swap array elements requested between end of fixed signals and start of platform-related signals")
        exit try
      end if
      ! check that start_record_for_logging is valid
      min_logging_start_record = 1 + start_record_for_torque_speed_lut + 2 * n_points_in_torque_speed_lookup_table + max_message_len_real
      if ((start_record_for_logging .gt. 0) .and. (start_record_for_logging .lt. min_logging_start_record)) then
        error = fail("Erroneous value for 'Record number for start of logging output': " // &
                     itoa(start_record_for_logging) // " specified, but value must be >=" // &
                     itoa(min_logging_start_record) // ".")
        exit try
      end if
      ! torque speed lookup table
      lut_record = 0
      do i = 1, (2 * n_points_in_torque_speed_lookup_table)
        if (mod(i, 2) .eq. 0) then
          swap_signals(164+i) = signal_descriptor_t("torque_speed_lut_torque_" // itoa(lut_record), "r", "r-", "Nm", "Torque-speed lookup table, generator torque " // itoa(lut_record))
        else
          lut_record = lut_record + 1
          swap_signals(164+i) = signal_descriptor_t("torque_speed_lut_generator_speed_" // itoa(lut_record), "r", "r-", "rad/s", "Torque-speed lookup table, generator speed " // itoa(lut_record))
        end if
      end do
      ! message
      swap_signals(164 + 2 * n_points_in_torque_speed_lookup_table + 1) = signal_descriptor_t("message_length", "i", "rw", "-", "Message length (only if record 49 < 0)")
      do i = 1, max_message_len_real
        swap_signals(164 + 2 * n_points_in_torque_speed_lookup_table + 1 + i) = signal_descriptor_t("message_text", "c", "rw", "-", "Message text, 4 characters per record")
      end do
      ! logging variables (set with rw permission so that it is optional for the dispatch libraries to populate these signals)
      do i = 1, max_logged_variables
        swap_signals(start_record_for_logging + i - 1) = signal_descriptor_t("logged_variable_" // itoa(i), "r", "rw", "-", "Logged variable " // itoa(i))
      end do
      ! get the input file name
      string_size = 0
      do while (string_size .lt. nint(avrSWAP(50)))
        if (iachar(accINFILE(string_size + 1)) >= 32) then ! ascii codes < 32 are non-printing, and would signal the end of meaningful input
          string_size = string_size + 1
        else
          exit
        end if
      end do
      input_file = repeat(C_NULL_CHAR, string_size)
      input_file = transfer(accINFILE(1:string_size), input_file)
      ! initialize dispatch
      call dispatch_initialize(input_file, n_input, n_output, error, swap_signals, swap_signals, input_indices, output_indices)
      if (allocated(error)) then
        call wrap_error(error, "Failure in DISCON()'s call to dispatch_initialize().")
        exit try
      end if
      allocate(input_buffer(n_input), source=0.0_wp)
      allocate(output_buffer(n_output), source=0.0_wp)
    end if
  
    ! fill input buffer from swap array
    do i = 1,n_input
      input_buffer(i) = real(avrSWAP(input_indices(i)), kind=wp)
    end do
  
    ! sample signal processing libraries
    call dispatch_sample(input_buffer, output_buffer, error)
    if (allocated(error)) then
      call wrap_error(error, "Failure in DISCON()'s call to dispatch_sample().")
      exit try
    end if
  
    ! copy output buffer elements to swap array
    do i = 1,n_output
      ! ignore signals whose output index is < 1 - they are unmatched
      if (output_indices(i) .gt. 0) avrSWAP(output_indices(i)) = real(output_buffer(i), kind=4)
    end do
    
    return
  end block try
  
  ! if we are here, then there has been an error. report it
  aviFAIL = -1
  error_message = error%to_chars()
  error_message_len = len_trim(error_message)
  if (error_message_len .ge. max_message_len_char) error_message_len = max_message_len_char - 1
  avcMSG = transfer(error_message(1:error_message_len) // C_NULL_CHAR, avcMSG, error_message_len + 1)

  return
  end subroutine DISCON
!----------------------------------------------------------------------
! end bladed dll interface
!----------------------------------------------------------------------
  
end module dispatch_interface
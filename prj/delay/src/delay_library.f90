!  delay_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from delay_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  This library implements delayed copies of input simulation signals. Signals can only be
!  delayed by an integer multiple of the library's sample period.
!  Typical toml specification for the library is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [signal]
!  input_signal_name = "current_time"
!  output_signal_name = "current_time_delayed"
!  delay_steps = 7
!  [signal]
!  input_signal_name = "measured_generator_speed"
!  output_signal_name = "measured_generator_speed_delayed"
!  delay_steps = 2
!
module delay_library

  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp
  use ifport
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: delay_signal_t
    integer :: delay_steps = 0
    real(wp), allocatable :: delay_buffer(:)
    logical :: first_call = .true.
  end type
  type :: config_t
    integer(kind=c_int) :: context
    real(wp)            :: sample_period = 0.0_wp
    integer             :: n_signal = 0
    integer(kind=c_int) :: n_offers = 0
    integer(kind=c_int) :: n_requests = 0
    type(string_container_t), allocatable :: offers(:)
    type(string_container_t), allocatable :: requests(:)
    type(delay_signal_t),     allocatable :: delay_signals(:)
  end type
  type(config_t), allocatable :: config(:)

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
  type(toml_table), pointer     :: child
  type(toml_array), pointer     :: array
  type(toml_error), allocatable :: error_toml
  type(toml_context)            :: context_toml
  class(error_t), allocatable   :: error
  type(toml_table), pointer            :: tmp_table
  type(toml_array), pointer            :: tmp_arr
  character(len=:), allocatable        :: tmp_str
  integer :: stat, origin, i, j, n
  type(config_t), allocatable   :: tmp_config(:)
  
  try: block
    ! create a new config object for this instance of the library
    if (allocated(config)) then
      context = config(size(config))%context + 1
      call move_alloc(config, tmp_config)
      allocate(config(context))
      config(1:context-1) = tmp_config
      config(context) = config_t(context)
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
      ! get values from [interface] section
      call get_value(table, 'interface', child, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read [interface]", origin, "section is required."))
        call wrap_error(error, '"interface" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      if (associated(child)) then
        ! sample period
        call get_value(child, 'sample_period', config(context)%sample_period, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read sample_period", origin, "expected real value."))
          call wrap_error(error, '"interface.sample_period" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
      end if
      ! get signal table
      call get_value(table, 'signal', array, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read [signal]", origin, "at least one signal is required."))
        call wrap_error(error, '"signal" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config(context)%n_signal = len(array)
      config(context)%n_offers = len(array)
      config(context)%n_requests = len(array)
      allocate(config(context)%offers(config(context)%n_signal))
      allocate(config(context)%requests(config(context)%n_signal))
      allocate(config(context)%delay_signals(config(context)%n_signal))
      ! read input_signal_name, output_signal_name, delay_steps
      do i = 1,config(context)%n_signal
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "input_signal_name", config(context)%requests(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value("input_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "output_signal_name", config(context)%offers(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value("output_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "delay_steps", config(context)%delay_signals(i)%delay_steps, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value("delay_steps") returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! the delay buffer also stores the current signal, so it needs to have length delay_steps + 1
        allocate(config(context)%delay_signals(i)%delay_buffer(config(context)%delay_signals(i)%delay_steps + 1), source=0.0_wp)
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library (nothing to do)
    
    ! set values returned to caller
    sample_period = config(context)%sample_period
    n_offers = config(context)%n_offers
    n_requests = config(context)%n_requests
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

  call string_container_array_to_delimited_string(config(context)%requests, ";", requests)

  end subroutine
  
  subroutine get_signal_offers(context, offers, error_string) bind(C, name="get_signal_offers")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::get_signal_offers
  #endif
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(inout) :: offers
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  
  call string_container_array_to_delimited_string(config(context)%offers, ";", offers)
  
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
  integer :: i, j
  
  try: block
    do i = 1, config(context)%n_signal
      if (config(context)%delay_signals(i)%first_call) then
        do j = 1,config(context)%delay_signals(i)%delay_steps + 1
          config(context)%delay_signals(i)%delay_buffer(j) = input_array(i)
        end do
      else
        config(context)%delay_signals(i)%delay_buffer = cshift(config(context)%delay_signals(i)%delay_buffer, -1)
        config(context)%delay_signals(i)%delay_buffer(1) = input_array(i)
      end if
      ! sample each filter
      output_array(i) = config(context)%delay_signals(i)%delay_buffer(config(context)%delay_signals(i)%delay_steps + 1)
    end do
    
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
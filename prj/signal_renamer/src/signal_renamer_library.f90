!  signal_renamer_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from signal_renamer_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  This library provides renamed copies of signals from within a dispatch environment. Typical toml specification
!  for the library is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [[signal]]
!  input_name = "my_signal"
!  output_name = "my_renamed_signal"
!  [[signal]]
!  input_name = "my_other_signal"
!  output_name = "my_signal"
!
!  In this example, libraries after this instance of the renamer would receive the value of "my_other_signal" when they request "my_signal".
!  There is no limit to the number of signal instances. 
!
module signal_renamer_library

  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp
  use ifport
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: config_t
    integer(c_int) :: context
    real(wp)       :: sample_period = 0.0_wp
    integer        :: n_signal = 0
    type(string_container_t), allocatable :: output_signals(:)
    type(string_container_t), allocatable :: input_signals(:)
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
  type(config_t), allocatable :: tmp_config(:)
  
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
      allocate(config(context)%input_signals(config(context)%n_signal))
      allocate(config(context)%output_signals(config(context)%n_signal))
      ! read input_name, output_name
      do i = 1,config(context)%n_signal
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "input_name", config(context)%input_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read input_name", origin, "expected character value."))
          call wrap_error(error, '"signal(' // itoa(i) // ').input_name" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "output_name", config(context)%output_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read output_name", origin, "expected character value."))
          call wrap_error(error, '"signal(' // itoa(i) // ').output_name" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library (nothing to do)
    
    ! set values returned to caller
    sample_period = config(context)%sample_period
    n_offers = config(context)%n_signal
    n_requests = config(context)%n_signal
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
  
  call string_container_array_to_delimited_string(config(context)%input_signals, ";", requests)
  
  end subroutine
  
  subroutine get_signal_offers(context, offers, error_string) bind(C, name="get_signal_offers")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::get_signal_offers
  #endif
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(inout) :: offers
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  
  call string_container_array_to_delimited_string(config(context)%output_signals, ";", offers)
  
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
  
  try: block
    do i = 1,config(context)%n_signal
      output_array(i) = input_array(i)
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
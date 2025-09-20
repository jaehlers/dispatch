!  monitor_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from monitor_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  This library implements a monitor function that emits a 0 signal value when a condition on 
!  a simulation signal is not met, and 1 once the condition is met (does not switch back to
!  zero if the condition no longer holds). Note that monitors can be cascaded by using output
!  of one monitor as input to another. Typical toml specification for the library is as follows:
!
!  # a monitor that starts monitoring current_time at current_time = 0, and begins emitting
!  # a signal value of 1 when current_time >= 500
!  [interface]
!  sample_period = 0.02
!  start_signal_name = "current_time"
!  start_signal_threshold = 0
!  start_signal_comparison = "ge"
!  monitor_signal_name = "current_time"
!  monitor_signal_threshold = 500
!  monitor_signal_comparison = "ge"
!  output_signal_name = "my_monitor_signal"
!
!  # a monitor that starts monitoring once another monitor has been triggered, and begins
!  # emitting a signal value of 1 as soon as hub_wind_speed <= 7
!  [interface]
!  sample_period = 0.02
!  start_signal_name = "my_monitor_signal"
!  start_signal_threshold = 1
!  start_signal_comparison = "ge"
!  monitor_signal_name = "hub_wind_speed"
!  monitor_signal_threshold = 7
!  monitor_signal_comparison = "le"
!  output_signal_name = "my_monitor_signal"
!
module monitor_library

  use extf, only: string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp
  use ifport
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char

  implicit none

  ! library variables
  enum, bind(c)
    enumerator :: comparisons = 0
    enumerator :: le = 447484
    enumerator :: eq = 909111
    enumerator :: ge = 193755
  end enum
  type :: config_t
    integer(kind=c_int) :: context
    real(wp)            :: sample_period = 0.0_wp
    integer(kind=c_int) :: n_requests = 0
    logical :: has_start_signal = .false.
    logical :: is_active = .false.
    logical :: is_triggered = .false.
    type(string_container_t) :: start_signal(1)
    type(string_container_t) :: monitor_signal(1)
    type(string_container_t) :: output_signal(1)
    real(wp) :: start_signal_threshold = 0.0_wp
    real(wp) :: monitor_signal_threshold = 0.0_wp
    integer(kind(comparisons)) :: start_signal_comparison = 0
    integer(kind(comparisons)) :: monitor_signal_comparison = 0
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
  type(toml_error), allocatable :: error_toml
  type(toml_context)            :: context_toml
  class(error_t), allocatable   :: error
  integer :: stat, origin, i, j, n
  character(len=:), allocatable :: comparison
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
    config(context)%n_requests = 0
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
        ! start_signal_name
        call get_value(child, 'start_signal_name', config(context)%start_signal(1)%s, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          if (stat .ne. toml_stat%missing_key) then
            error = fail(context_toml%report("Cannot read start_signal_name", origin, "expected character value."))
            call wrap_error(error, '"interface.start_signal_name" get_value returned status: ' // toml_error_message(stat))
            exit try
          else
            config(context)%is_active = .true. ! if the monitor has no start signal, it is active from the first call
          end if
        else
          config(context)%n_requests = config(context)%n_requests + 1
          config(context)%has_start_signal = .true.
        end if
        ! start_signal_threshold
        if (config(context)%has_start_signal) then
          call get_value(child, 'start_signal_threshold', config(context)%start_signal_threshold, stat=stat, origin=origin)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read start_signal_threshold", origin, "expected real value."))
            call wrap_error(error, '"interface.start_signal_threshold" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          ! start_signal_comparison
          call get_value(child, 'start_signal_comparison', comparison, stat=stat, origin=origin)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read start_signal_comparison", origin, "expected 2-character value [le, eq, ge]."))
            call wrap_error(error, '"interface.start_signal_comparison" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          select case(comparison)
          case('le')
            config(context)%start_signal_comparison = le
          case('eq')
            config(context)%start_signal_comparison = eq
          case('ge')
            config(context)%start_signal_comparison = ge
          case default
            error = fail("Unsupported start_signal_comparison: " // comparison)
            exit try
          end select
        end if
        ! monitor_signal_name
        call get_value(child, 'monitor_signal_name', config(context)%monitor_signal(1)%s, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read monitor_signal_name", origin, "expected character value."))
          call wrap_error(error, '"interface.monitor_signal_name" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        config(context)%n_requests = config(context)%n_requests + 1
        ! monitor_signal_threshold
        call get_value(child, 'monitor_signal_threshold', config(context)%monitor_signal_threshold, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read monitor_signal_threshold", origin, "expected real value."))
          call wrap_error(error, '"interface.monitor_signal_threshold" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! monitor_signal_comparison
        call get_value(child, 'monitor_signal_comparison', comparison, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read monitor_signal_comparison", origin, "expected 2-character value [le, eq, ge]."))
          call wrap_error(error, '"interface.monitor_signal_comparison" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        select case(comparison)
        case('le')
          config(context)%monitor_signal_comparison = le
        case('eq')
          config(context)%monitor_signal_comparison = eq
        case('ge')
          config(context)%monitor_signal_comparison = ge
        case default
          error = fail("Unsupported monitor_signal_comparison: " // comparison)
          exit try
        end select
        ! output_signal_name
        call get_value(child, 'output_signal_name', config(context)%output_signal(1)%s, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read output_signal_name", origin, "expected character value."))
          call wrap_error(error, '"interface.output_signal_name" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
      end if
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library (nothing to do)
    
    ! set values returned to caller
    sample_period = config(context)%sample_period
    n_offers = 1
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
  ! local variables
  type(string_container_t) :: input_signals(config(context)%n_requests)
  
  if (config(context)%has_start_signal) then
    input_signals(1)%s = config(context)%start_signal(1)%s
    input_signals(2)%s = config(context)%monitor_signal(1)%s
  else
    input_signals(1)%s = config(context)%monitor_signal(1)%s
  end if
  call string_container_array_to_delimited_string(input_signals, ";", requests)

  end subroutine
  
  subroutine get_signal_offers(context, offers, error_string) bind(C, name="get_signal_offers")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::get_signal_offers
  #endif
  integer(kind=c_int),                       intent(in   ) :: context
  character(kind=c_char,len=:), allocatable, intent(inout) :: offers
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  
  call string_container_array_to_delimited_string(config(context)%output_signal, ";", offers)
  
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
  integer :: j
  
  try: block
    output_array(1) = 0.0_wp
    j = 0
    ! activate the monitor when requested
    if (config(context)%has_start_signal) then
      j = j + 1
    end if
    if (.not. config(context)%is_active) then
      select case(config(context)%start_signal_comparison)
      case(le)
        if (input_array(j) .le. config(context)%start_signal_threshold) then
          config(context)%is_active = .true.
        end if
      case(eq)
        if (input_array(j) .eq. config(context)%start_signal_threshold) then
          config(context)%is_active = .true.
        end if
      case(ge)
        if (input_array(j) .ge. config(context)%start_signal_threshold) then
          config(context)%is_active = .true.
        end if
      end select
    end if
    ! check the monitor
    j = j + 1
    if (config(context)%is_active) then
      select case(config(context)%monitor_signal_comparison)
      case(le)
        if (input_array(j) .le. config(context)%monitor_signal_threshold) then
          config(context)%is_triggered = .true.
        end if
      case(eq)
        if (input_array(j) .eq. config(context)%monitor_signal_threshold) then
          config(context)%is_triggered = .true.
        end if
      case(ge)
        if (input_array(j) .ge. config(context)%monitor_signal_threshold) then
          config(context)%is_triggered = .true.
        end if
      end select
      ! generate output
      if (config(context)%is_triggered) then
        output_array(1) = 1.0_wp
      end if
    end if
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
!  n_ary_function_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from n_ary_function_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  This library applies functions to signals from within a dispatch environment (or within this
!  library itself). Typical toml specification for the library is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [function]
!  input_signal_names = ['signal_0', 'signal_1', 'signal_2']
!  function = '+' # other available functions: -,*,/
!  output_signal_name = 'my_output_signal'
!  [function]
!  input_signal_names = ['my_output_signal', 'my_output_signal']
!  function = '*'
!  output_signal_name = 'my_output_signal_squared'
!
!  In this example, the first function generates an output, which is used by the second function. 
!
module n_ary_function_library

  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: signal_pointer_t, wp
  use ifport
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: function_spec_t
    integer                               :: n_input = 0
    type(string_container_t), allocatable :: input_signals(:)
    logical,                  allocatable :: input_is_internal(:)
    type(signal_pointer_t),   allocatable :: input_pointers(:)
    integer,                  allocatable :: i_internal_input(:)
    character(len=:),         allocatable :: f
    character(len=:),         allocatable :: output_signal
    real(wp)                              :: output = 0.0_wp
  end type
  type :: config_t
    integer(c_int) :: context
    real(wp)       :: sample_period = 0.0_wp
    integer        :: n_function = 0
    integer        :: n_offers = 0
    integer        :: n_requests = 0
    real(wp),                 allocatable :: input_buffer(:)
    type(string_container_t), allocatable :: offers(:)
    type(string_container_t), allocatable :: requests(:)
    type(function_spec_t),    allocatable :: functions(:)
  end type
  type(config_t), allocatable, target :: config(:)

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
  integer :: stat, origin, i, j, k, request_index, n
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
      ! get function table
      call get_value(table, 'function', array, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read [function]", origin, "at least one function is required."))
        call wrap_error(error, '"function" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config(context)%n_function = len(array)
      config(context)%n_offers = len(array)
      allocate(config(context)%offers(config(context)%n_function))
      allocate(config(context)%functions(config(context)%n_function))
      ! read input_signal_names, function, output_signal_name
      do i = 1,config(context)%n_function
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"function"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! input_signal_names
        call get_value(child, 'input_signal_names', tmp_arr, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read input_signal_names", origin, "expected array of strings."))
          call wrap_error(error, '"function"(' // itoa(i) // ') get_value("input_signal_names") get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! copy the input signal names from toml_array to string_container_t
        if (associated(tmp_arr)) then
          config(context)%functions(i)%n_input = len(tmp_arr)
          allocate(config(context)%functions(i)%input_signals(config(context)%functions(i)%n_input))
          allocate(config(context)%functions(i)%input_is_internal(config(context)%functions(i)%n_input))
          allocate(config(context)%functions(i)%input_pointers(config(context)%functions(i)%n_input))
          allocate(config(context)%functions(i)%i_internal_input(config(context)%functions(i)%n_input), source=0)
          do j = 1,config(context)%functions(i)%n_input
            call get_value(tmp_arr, j, config(context)%functions(i)%input_signals(j)%s)
          end do
        else
          error = fail(context_toml%report("input_signal_names is invalid", origin, "expected array of strings."))
          call wrap_error(error, '"function"(' // itoa(i) // ')')
          exit try
        end if
        ! function
        call get_value(child, 'function', config(context)%functions(i)%f, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read function", origin, "expected string value."))
          call wrap_error(error, '"function"(' // itoa(i) // ') get_value("function") returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! output_signal_name
        call get_value(child, 'output_signal_name', config(context)%functions(i)%output_signal, stat=stat, origin=origin)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read output_signal_name", origin, "expected string value."))
          call wrap_error(error, '"function"(' // itoa(i) // ') get_value("output_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        config(context)%offers(i)%s = config(context)%functions(i)%output_signal
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library
    config(context)%n_requests = 0
    do i = 1,config(context)%n_function
      ! resolve signal connections between function inputs and the outputs of other functions
      do j = 1,config(context)%functions(i)%n_input
        config(context)%functions(i)%input_is_internal(j) = .false.
        function_outputs: do k = i, 1, -1
          if (config(context)%functions(i)%input_signals(j)%s .eq. config(context)%offers(k)%s) then
            config(context)%functions(i)%input_is_internal(j) = .true.
            config(context)%functions(i)%input_pointers(j)%ptr => config(context)%functions(k)%output
            config(context)%functions(i)%i_internal_input(j) = k
            exit function_outputs
          end if
        end do function_outputs
        ! request an input signal from the interface if none is provided by other functions
        if (.not. config(context)%functions(i)%input_is_internal(j)) then
          config(context)%n_requests = config(context)%n_requests + 1
        end if
      end do
    end do
    allocate(config(context)%requests(config(context)%n_requests))
    allocate(config(context)%input_buffer(config(context)%n_requests))
    request_index = 0
    do i = 1, config(context)%n_function
      do j = 1,config(context)%functions(i)%n_input
        if (.not. config(context)%functions(i)%input_is_internal(j)) then
          request_index = request_index + 1
          config(context)%requests(request_index)%s = config(context)%functions(i)%input_signals(j)%s
          config(context)%functions(i)%input_pointers(j)%ptr => config(context)%input_buffer(request_index)
        end if
      end do
    end do
    
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
  integer :: i, j, input_index
  real(wp) :: signal
  
  try: block
    do i = 1,config(context)%n_requests
      config(context)%input_buffer(i) = input_array(i)
    end do
    input_index = 1
    do i = 1,config(context)%n_function
      if (config(context)%functions(i)%input_is_internal(1)) then
        signal = config(context)%functions(config(context)%functions(i)%i_internal_input(1))%output
      else
        signal = config(context)%input_buffer(input_index)
        input_index = input_index + 1
      end if
      config(context)%functions(i)%output = signal
      select case(config(context)%functions(i)%f)
        case("+")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            config(context)%functions(i)%output = config(context)%functions(i)%output + signal
          end do
        case("-")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            config(context)%functions(i)%output = config(context)%functions(i)%output - signal
          end do
        case("*")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            config(context)%functions(i)%output = config(context)%functions(i)%output * signal
          end do
        case("/")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            config(context)%functions(i)%output = config(context)%functions(i)%output / signal
          end do
        case("min")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            if (config(context)%functions(i)%output .gt. signal) then
              config(context)%functions(i)%output = signal
            end if
          end do
        case("max")
          do j = 2,config(context)%functions(i)%n_input
            if (config(context)%functions(i)%input_is_internal(j)) then
              signal = config(context)%functions(config(context)%functions(i)%i_internal_input(j))%output
            else
              signal = config(context)%input_buffer(input_index)
              input_index = input_index + 1
            end if
            if (config(context)%functions(i)%output .lt. signal) then
              config(context)%functions(i)%output = signal
            end if
          end do
      end select
      output_array(i) = config(context)%functions(i)%output
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
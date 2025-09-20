!  iir_filter_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from iir_filter_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  this library implements an arbitrary number of SISO IIR filters, based on continuous-time transfer
!  function coefficients provided as parameters. Filters may be cascaded, as shown in the example below:
!  A sample (3-filter) input file is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [transfer_function]
!  input_signal_name = "my_input_signal"
!  output_signal_name = "my_first_filter_output_signal"
!  numerator = [1, 2, 3, 4] # ordered in decreasing orders of s, ie. this line means s^3 + 2s^2 + 3s + 4
!  denominator = [1, 2, 3, 4, 5] # decreasing orders of s, order must be >= numerator
!  prewarp_frequency = 2.3 # Hz (optional)
!  [transfer_function]
!  input_signal_name = "my_first_filter_output_signal" # this filter takes as input, the output of a preceding filter
!  output_signal_name = "my_second_filter_output_signal"
!  numerator = [1, 2, 3, 4] # ordered in decreasing orders of s, ie. this line means s^3 + 2s^2 + 3s + 4
!  denominator = [1, 2, 3, 4, 5] # decreasing orders of s, order must be >= numerator
!  prewarp_frequency = 2.3 # Hz (optional)
!  [transfer_function]
!  input_signal_name = "another_input_signal" # this filter is not cascaded
!  output_signal_name = "another_output_signal"
!  numerator = [5, 4, 3]
!  denominator = [6, 7, 8]
!
!  Each filter is implemented as cascaded second-order sections, implemented in Type II direct form.
!  Conversion to discrete-time form is via Tustin's method with a single prewarp frequency (if desired).
!  Each filter generates an output signal, but only those filters that request an input signal that is not
!  provided by a preceding filter will generate a signal request to the other signal processing libraries.
!
module iir_filter_library

  use iir_filter, only: iir_filter_t
  use extf, only: itoa, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: signal_pointer_t, string_container_t, wp
  use iso_c_binding,  only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: config_t
    integer(c_int) :: context
    real(8) :: sample_period = 0.0_wp
    integer :: n_filter = 0
    integer :: n_offers = 0
    integer :: n_requests = 0
    logical,                  allocatable :: input_is_from_interface(:)
    type(signal_pointer_t),   allocatable :: input_pointers(:)
    real(wp),                 allocatable :: input_buffer(:)
    type(string_container_t), allocatable :: offers(:)
    type(string_container_t), allocatable :: requests(:)
    type(iir_filter_t),       allocatable :: iir_filters(:)
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
  integer :: stat, origin, i, j, n
  type iir_filter_data_t
    real(wp), allocatable :: num(:), den(:)
    real(wp) :: fpw = 0.0_wp
  end type
  type(iir_filter_data_t), allocatable :: iir_filter_data(:)
  type(string_container_t), allocatable :: input_signals(:)
  logical :: filter_input_is_internal
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
      ! get transfer_function table
      call get_value(table, 'transfer_function', array, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read [transfer_function]", origin, "at least one transfer function is required."))
        call wrap_error(error, '"transfer_function" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config(context)%n_filter = len(array)
      config(context)%n_offers = config(context)%n_filter
      allocate(config(context)%input_is_from_interface(config(context)%n_filter), source=.false.)
      allocate(config(context)%input_pointers(config(context)%n_filter))
      allocate(config(context)%input_buffer(config(context)%n_filter))
      allocate(config(context)%offers(config(context)%n_filter))
      allocate(input_signals(config(context)%n_filter))
      allocate(iir_filter_data(config(context)%n_filter))
      ! read input_signal_name, output_signal_name, numerator, denominator, prewarp_frequency
      do i = 1,config(context)%n_filter
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"transfer_function"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "input_signal_name", input_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"transfer_function"(' // itoa(i) // ') get_value("input_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "output_signal_name", config(context)%offers(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"transfer_function"(' // itoa(i) // ') get_value("output_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "numerator", tmp_arr, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"transfer_function"(' // itoa(i) // ') get_value("numerator") returned status: ' // toml_error_message(stat))
          exit try
        end if
        allocate(iir_filter_data(i)%num(len(tmp_arr)))
        do j = 1,len(tmp_arr)
          call get_value(tmp_arr, j, iir_filter_data(i)%num(j))
        end do
        nullify(tmp_arr)
        call get_value(child, "denominator", tmp_arr, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"transfer_function"(' // itoa(i) // ') get_value("denominator") returned status: ' // toml_error_message(stat))
          exit try
        end if
        allocate(iir_filter_data(i)%den(len(tmp_arr)))
        do j = 1,len(tmp_arr)
          call get_value(tmp_arr, j, iir_filter_data(i)%den(j))
        end do
        ! prewarp_frequency is an optional parameter - no error if not present
        call get_value(child, "prewarp_frequency", iir_filter_data(i)%fpw, stat=stat)
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library
    allocate(config(context)%iir_filters(config(context)%n_filter))
    config(context)%n_requests = 0
    do i = 1,config(context)%n_filter
      ! resolve signal connections between filter inputs and the outputs of other filters
      filter_input_is_internal = .false.
      filter_outputs: do j = i, 1, -1
        if (input_signals(i)%s .eq. config(context)%offers(j)%s) then
          filter_input_is_internal = .true.
          config(context)%input_pointers(i)%ptr => config(context)%iir_filters(j)%output
          exit filter_outputs
        end if
      end do filter_outputs
      ! request an input signal from the interface if none is provided by other filters
      if (.not. filter_input_is_internal) then
        config(context)%n_requests = config(context)%n_requests + 1
        config(context)%input_is_from_interface(i) = .true.
      end if
      ! if prewarp frequency was not specified, it is zero, in which case the discretization of the zpk form reduces to no prewarp
      config(context)%iir_filters(i) = iir_filter_t(iir_filter_data(i)%num,                &
                                                    iir_filter_data(i)%den,                &
                                                    1.0d0/config(context)%sample_period,   &
                                                    iir_filter_data(i)%fpw)
      if (allocated(config(context)%iir_filters(i)%error)) then
        error = config(context)%iir_filters(i)%error
        call wrap_error(error, "Error in construction of IIR filter")
        exit try
      end if
    end do
    allocate(config(context)%requests(config(context)%n_requests))
    j = 0
    do i = 1, config(context)%n_filter
      if (config(context)%input_is_from_interface(i)) then
        j = j + 1
        config(context)%requests(j)%s = input_signals(i)%s
      end if
    end do
    
    ! set values returned to caller
    ! context has been assigned a value above
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
  ! local variables
  integer :: i
  
  call string_container_array_to_delimited_string(config(context)%requests, ";", requests)
  
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
  integer :: i, input_index
  
  try: block
    ! copy input array and other-filter-output signals to the input buffer
    input_index = 0
    do i = 1, config(context)%n_filter
      if (config(context)%input_is_from_interface(i)) then
        input_index = input_index + 1
        config(context)%input_buffer(i) = input_array(input_index)
      else
        config(context)%input_buffer(i) = config(context)%input_pointers(i)%ptr
      end if
      ! sample each filter
      call config(context)%iir_filters(i)%sample(config(context)%input_buffer(i), output_array(i))
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
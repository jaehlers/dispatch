!  variable_bandwidth_recursive_filter_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from variable_bandwidth_recursive_filter_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  this library implements a variable-bandwidth IIR filter, based on second-order sections whose coefficients
!  vary as functions of a parameter signal. Filter stability is guaranteed as parameters vary. Based on:
!  Deng, T. B. (2023). Variable-bandwidth recursive-filter design employing cascaded stability-guaranteed
!  2nd-order sections using coefficient transformations. Journal of Information and Telecommunication, 8(2), 149–166.
!  https://doi.org/10.1080/24751839.2023.2267890
!
!  see /design/variable_bandwidth_recursive_filter_design.ipynb (Jupyter notebook) for implementation of the
!  example in that paper, and an example of preparing input to this library.
!
module variable_bandwidth_filter_library

  use variable_bandwidth_sos_class, only: variable_bandwidth_sos_t
  use extf, only: itoa, string_container_array_to_delimited_string, real_array_container_t
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: string_container_t, wp
  use iso_c_binding,  only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: config_t
    integer(c_int) :: context
    real(wp) :: sample_period = 0.0_wp
    real(wp) :: input_buffer = 0.0_wp
    type(string_container_t) :: offers(1)
    type(string_container_t) :: requests(2)
    character(len=:),  allocatable :: input_signal
    character(len=:),  allocatable :: psi_signal
    type(variable_bandwidth_sos_t) :: filter
  end type
  type(config_t), allocatable, target :: config(:)

  contains
  
  subroutine initialize(parameters, parameters_length, parameters_as_toml_string, context, sample_period, n_requests, n_offers, error_string) bind(C, name="initialize")
  #ifndef IMPLICIT_DLLEXPORT
  !DIR$ ATTRIBUTES DLLEXPORT::initialize
  #endif
  use hdf5_utils

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
  ! local variables
  integer(hid_t) :: fid
  class(error_t), allocatable :: error
  real(8) :: tmp_double
  character(len=:),   allocatable :: tmp_str
  type(config_t),     allocatable :: tmp_config(:)
  real(wp) :: lambda
  real(wp), allocatable :: psi_range(:)
  type(real_array_container_t) :: g
  type(real_array_container_t), allocatable :: b1(:)
  type(real_array_container_t), allocatable :: b2(:)
  type(real_array_container_t), allocatable :: x2(:)
  type(real_array_container_t), allocatable :: x1(:)
  integer :: i, n_section
  
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
      error = fail("Parameters must be specified via hdf5 file.")
      exit try
    else
      call hdf5_initialize(error)
      if (allocated(error)) then
        call wrap_error(error, "Failure to initialize hdf5 interface.")
        exit try
      end if
      call hdf5_open_file_read(parameters, fid, error)
      if (allocated(error)) then
        call wrap_error(error, "Failure to open hdf5 file for reading: " // parameters)
        exit try
      end if
      data_read: block
        ! get sample period
        call hdf5_read_array_auto(fid, "/filter/sample_period", sample_period, error)
        if (allocated(error)) exit data_read
        ! get output signal name
        call hdf5_read_string_auto(fid, "/filter/output_signal", config(context)%offers(1)%s, error)
        if (allocated(error)) exit data_read
        ! get input signal name
        call hdf5_read_string_auto(fid, "/filter/input_signal", config(context)%input_signal, error)
        if (allocated(error)) exit data_read
        ! get psi signal name
        call hdf5_read_string_auto(fid, "/filter/psi_signal", config(context)%psi_signal, error)
        if (allocated(error)) exit data_read
        config(context)%requests(1)%s = config(context)%input_signal
        config(context)%requests(2)%s = config(context)%psi_signal
        ! get psi range
        call hdf5_read_array_auto(fid, "/filter/psi_range", psi_range, error)
        if (allocated(error)) exit data_read
        ! get n_section
        call hdf5_read_array_auto(fid, "/filter/n_section", tmp_double, error)
        if (allocated(error)) exit data_read
        n_section = nint(tmp_double)
        allocate(b1(n_section))
        allocate(b2(n_section))
        allocate(x2(n_section))
        allocate(x1(n_section))
        ! get g
        call hdf5_read_array_auto(fid, "/filter/g", g%a, error)
        if (allocated(error)) exit data_read
        ! get lambda
        call hdf5_read_array_auto(fid, "/filter/lambda", lambda, error)
        if (allocated(error)) exit data_read
        ! get sos coefficient polynomials
        do i = 1,n_section
          call hdf5_read_array_auto(fid, "/filter/b" // itoa(i) // "1", b1(i)%a, error)
          if (allocated(error)) exit data_read
          call hdf5_read_array_auto(fid, "/filter/b" // itoa(i) // "2", b2(i)%a, error)
          if (allocated(error)) exit data_read
          call hdf5_read_array_auto(fid, "/filter/x" // itoa(i) // "2", x2(i)%a, error)
          if (allocated(error)) exit data_read
          call hdf5_read_array_auto(fid, "/filter/x" // itoa(i) // "1", x1(i)%a, error)
          if (allocated(error)) exit data_read
        end do
      end block data_read
      ! handle any errors that occurred during data reading
      if (allocated(error)) then
        call wrap_error(error, "Error reading data from hdf5 file: " // trim(parameters))
        exit try
      end if
      ! close the file (this could be done on termination if the file needs to be read in update())
      call hdf5_close_file(fid, error)
      if (allocated(error)) then
        call wrap_error(error, "Error closing hdf5 file: " // parameters)
        exit try
      end if
      ! free all memory associated with hdf objects and close the interface
      call hdf5_terminate(error)
      if (allocated(error)) then
        call wrap_error(error, "Failure to terminate hdf5 interface.")
        exit try
      end if
    end if
    ! initialize the variables needed by the library
    config(context)%filter = variable_bandwidth_sos_t(n_section, lambda, psi_range, g, b1, b2, x2, x1)
    if (allocated(config(context)%filter%error)) then
      error = config(context)%filter%error
      call wrap_error(error, "Error in construction of variable-bandwidth sos")
      exit try
    end if
    
    ! set values returned to caller
    ! context and sample_period have been assigned values above
    n_offers = 1
    n_requests = 2
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
  integer :: i
  
  try: block
    ! initialize the filter if this is its first sample (also initializes parameters)
    if (config(context)%filter%first_sample) then
      call config(context)%filter%initialize(input_array(1), input_array(2))
    else
      ! just update parameters on subsequent calls
      call config(context)%filter%update_parameters(input_array(2))
    end if
    
    ! sample the filter
    call config(context)%filter%sample(input_array(1), output_array(1))
    
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
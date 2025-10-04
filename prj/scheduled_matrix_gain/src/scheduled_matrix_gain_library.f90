!  scheduled_matrix_gain_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from dispatch_library.dll:
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  this library applies a matrix gain to a vector of input signals. The gain can be varied according
!  to a scheduling variable.
!

module scheduled_matrix_gain_library

  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp, ek
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  
  implicit none

  ! library variables
  type :: scheduled_gain_t
    integer :: nx = 0                                    ! number of gain-input signals
    integer :: nu = 0                                    ! number of gain-output signals
    integer :: no = 0                                    ! number of operating points for the gain
    logical :: first_call = .true.                       ! flag to indicate first call
    integer :: io = 0                                    ! index of the current operating point
    integer :: io_switch_count = 0                       ! counter for managing operating-point switching
    integer :: io_switch_threshold = 0                   ! number of sample steps the operating index is allowed to differ from the current index, before it is switched
    integer, allocatable :: i_x(:)                       ! indices of the gain-input-vector member signals within the library input vector
    integer :: i_op = 0                                  ! index of the operating point signal within the input vector
    real(8), allocatable :: operating_points(:)          ! vector of operating point signal values at which scheduled gains defined
    type(string_container_t), allocatable :: input_signals(:)       ! names of gain-input signals
    type(string_container_t), allocatable :: output_signals(:)      ! names of gain-output signals
    character(len=:), allocatable :: operating_point_signal         ! name of the operating point signal
    real(8), allocatable :: Ks(:,:,:)                    ! gain matrices at all operating points
    real(8), allocatable :: K(:,:)                       ! gain matrix at the current operating point
    real(8), allocatable :: x(:)                         ! gain-input vector
    real(8), allocatable :: u(:)                         ! gain-output vector
  end type
  
  type :: config_t
    integer(c_int) :: context
    real(wp) :: sample_period = 0.0_wp
    integer :: n_offers = 0
    integer :: n_requests = 0
    type(string_container_t), allocatable :: offers(:), requests(:)
    real(wp), allocatable  :: input_buffer(:), output_buffer(:)
    type(scheduled_gain_t) :: gain
  end type
  type(config_t), allocatable :: config(:)

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
  ! local variables related to hdf5 reading
  logical :: status
  integer(hid_t) :: fid
  class(error_t), allocatable :: error
  real(8) :: tmp_double
  character(len=:), allocatable, target :: tmp_str
  ! local variables related to the system model
  logical  :: match_found
  integer  :: n, info, i, j, k
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
    ! read parameters as hdf5 file
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
        call hdf5_read_array_auto(fid, "/interface/sample_period", config(context)%sample_period, error)
        if (allocated(error)) exit data_read
        ! gain data
        ! get input signal names
        call hdf5_read_varying_string_array_auto(fid, "/scheduled_gain/input_signals", config(context)%gain%input_signals, error)
        if (allocated(error)) exit data_read
        ! get output signal names
        call hdf5_read_varying_string_array_auto(fid, "/scheduled_gain/output_signals", config(context)%gain%output_signals, error)
        if (allocated(error)) exit data_read
        ! get operating point signal name
        call hdf5_read_string_auto(fid, "/scheduled_gain/operating_point_signal", config(context)%gain%operating_point_signal, error)
        if (allocated(error)) exit data_read
        ! get operating point switch threshold
        call hdf5_read_array_auto(fid, "/scheduled_gain/operating_point_switch_threshold", tmp_double, error)
        if (allocated(error)) exit data_read
        config(context)%gain%io_switch_threshold = nint(tmp_double)
        ! get operating points
        call hdf5_read_array_auto(fid, "/scheduled_gain/operating_points", config(context)%gain%operating_points, error)
        if (allocated(error)) exit data_read
        ! get K
        call hdf5_read_array_auto(fid, "/scheduled_gain/K", config(context)%gain%Ks, error)
        if (allocated(error)) exit data_read
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
    
    ! initialize Kalman filter variables
    config(context)%gain%nu = size(config(context)%gain%Ks, 1)
    config(context)%gain%nx = size(config(context)%gain%Ks, 2)
    config(context)%gain%no = size(config(context)%gain%operating_points, 1)
    allocate(config(context)%gain%i_x(config(context)%gain%nx))
    allocate(config(context)%gain%K(config(context)%gain%nu,config(context)%gain%nx), source=0.0d0)
    allocate(config(context)%gain%x(config(context)%gain%nx), source=0.0d0)
    allocate(config(context)%gain%u(config(context)%gain%nu), source=0.0d0)
    if (size(config(context)%gain%Ks, 3) .ne. config(context)%gain%no) then
      error = fail("Incorrect number of gains specified for operating point vector; no=" // itoa(config(context)%gain%no))
      exit try
    end if
    if (size(config(context)%gain%input_signals) .ne. config(context)%gain%nx) then
      error = fail("Incorrect number of input signals specified for gain matrix; nx=" // itoa(config(context)%gain%nx))
      exit try
    end if
    if (size(config(context)%gain%output_signals) .ne. config(context)%gain%nu) then
      error = fail("Incorrect number of output signals specified for gain matrix; nu=" // itoa(config(context)%gain%nu))
      exit try
    end if
    
    ! initialize input signals:
    config(context)%n_requests = size(config(context)%gain%input_signals) + 1
    allocate(config(context)%requests(config(context)%n_requests))
    allocate(config(context)%input_buffer(config(context)%n_requests), source=0.0_wp)
    do i = 1,config(context)%gain%nx
      config(context)%requests(i)%s = config(context)%gain%input_signals(i)%s
      config(context)%gain%i_x(i) = i
    end do
    config(context)%requests(config(context)%gain%nx + 1)%s = config(context)%gain%operating_point_signal
    config(context)%gain%i_op = 1+config(context)%gain%nx
    
    ! initialize output signals:
    config(context)%n_offers = config(context)%gain%nu
    allocate(config(context)%offers(config(context)%n_offers))
    allocate(config(context)%output_buffer(config(context)%n_offers), source=0.0_wp)
    do i = 1,config(context)%gain%nu
      config(context)%offers(i)%s = config(context)%gain%output_signals(i)%s
    end do
    
    ! set values returned to caller
    ! context was assigned a value above
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
  integer(kind=c_int),                       intent(in   ) :: context
  real(kind=c_double),                       intent(in   ) :: input_array(*)
  real(kind=c_double),                       intent(  out) :: output_array(*)
  character(kind=c_char,len=:), allocatable, intent(  out) :: error_string
  ! local variables
  class(error_t), allocatable   :: error
  integer :: i, j, k, io
  real(wp) :: operating_point
  
  try: block
    ! copy input signals
    do i = 1,config(context)%n_requests
      config(context)%input_buffer(i) = input_array(i)
    end do
    do i = 1,config(context)%gain%nx
      config(context)%gain%x(i) = config(context)%input_buffer(config(context)%gain%i_x(i))
    end do
    
    ! check if current operating point is closer to another operating point
    operating_point = config(context)%input_buffer(config(context)%gain%i_op)
    io = minloc(abs(config(context)%gain%operating_points - operating_point), dim=1)
    if (io .ne. config(context)%gain%io) then
      config(context)%gain%io_switch_count = config(context)%gain%io_switch_count + 1
    else
      config(context)%gain%io_switch_count = 0
    end if
    if ((config(context)%gain%io_switch_count .gt. config(context)%gain%io_switch_threshold) .or. config(context)%gain%first_call) then
      ! reset gain matrix
      config(context)%gain%K = config(context)%gain%Ks( :,:,io)
      ! switch operating point
      config(context)%gain%io = io
      config(context)%gain%first_call = .false.
    end if
    
    ! apply gain
    config(context)%gain%u = matmul(config(context)%gain%K, config(context)%gain%x)
    
    ! output the output signals
    do i = 1,config(context)%gain%nu
      output_array(i) = config(context)%gain%u(i)
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
  integer :: i, j
  
  end subroutine
end module
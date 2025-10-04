!  state_estimator_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from dispatch_library.dll:
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  this library implements a state estimator as an extended Kalman filter
!
module state_estimator_library

  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  use stdlib_linalg, only: inv
  use stdlib_linalg_state, only: linalg_state_type, LINALG_SUCCESS, operator(/=)

  implicit none

  ! library variables
  ! kalman filter variables
  type :: kalman_filter_t
    integer :: nx = 0                                    ! number of states
    integer :: nu = 0                                    ! number of control inputs
    integer :: ny = 0                                    ! number of system output signals
    integer :: no = 0                                    ! number of operating points for the model
    logical :: first_call = .true.                       ! flag to indicate first call to the kalman filter
    integer :: io = 0                                    ! index of the current operating point
    integer :: io_switch_count = 0                       ! counter for managing operating-point switching
    integer :: io_switch_threshold = 0                   ! number of sample steps the operating index is allowed to differ from the current index, before it is switched
    integer, allocatable :: i_y(:)                       ! indices of the measurement signals within the input vector
    integer, allocatable :: i_u(:)                       ! indices of the control signals within the input vector
    integer :: i_op = 0                                  ! index of the operating point signal within the input vector
    real(8), allocatable :: operating_points(:)          ! vector of operating point signal values at which linearized models are defined
    type(string_container_t), allocatable :: measurement_signals(:) ! names of modeled measurement signals
    type(string_container_t), allocatable :: control_signals(:)     ! names of modeled control signals
    type(string_container_t), allocatable :: state_signals(:)       ! names of modeled state signals
    character(len=:), allocatable :: operating_point_signal         ! name of the operating point signal
    real(8), allocatable :: A(:,:)                       ! the currently-active A matrix
    real(8), allocatable :: B(:,:)                       ! the currently-active B matrix
    real(8), allocatable :: C(:,:)                       ! the currently-active C matrix
    real(8), allocatable :: D(:,:)                       ! the currently-active D matrix
    real(8), allocatable :: Q(:,:)                       ! the currently-active Q matrix (process noise covariance)
    real(8), allocatable :: R(:,:)                       ! the currently-active R matrix (measurement noise covariance)
    real(8), allocatable :: invR(:,:)                    ! the inverse of the currently-active R matrix (measurement noise covariance)
    real(8), allocatable :: As(:,:,:)                    ! A matrices at all prescribed operating point signal values
    real(8), allocatable :: Bs(:,:,:)                    ! B matrices at all prescribed operating point signal values
    real(8), allocatable :: Cs(:,:,:)                    ! C matrices at all prescribed operating point signal values
    real(8), allocatable :: Ds(:,:,:)                    ! D matrices at all prescribed operating point signal values
    real(8), allocatable :: Qs(:,:,:)                    ! Q matrices at all prescribed operating point signal values
    real(8), allocatable :: Rs(:,:,:)                    ! R matrices at all prescribed operating point signal values
    real(8), allocatable :: x0(:)                        ! nominal state values of the currently-active model
    real(8), allocatable :: u0(:)                        ! nominal control signal values of the currently-active model
    real(8), allocatable :: ud0(:)                       ! nominal disturbance signal values of the currently-active model
    real(8), allocatable :: y0(:)                        ! nominal output signal values of the currently-active model
    real(8), allocatable :: x0s(:,:)                     ! nominal state values at all prescribed operating point signal values
    real(8), allocatable :: u0s(:,:)                     ! nominal control signal values at all prescribed operating point signal values
    real(8), allocatable :: ud0s(:,:)                    ! nominal disturbance signal values at all prescribed operating point signal values
    real(8), allocatable :: y0s(:,:)                     ! nominal output signal values at all prescribed operating point signal values
    real(8), allocatable :: K(:,:)                       ! Kalman filter gain matrix
    real(8), allocatable :: P(:,:)                       ! state covariance matrix
    real(8), allocatable :: x(:)                         ! state
    real(8), allocatable :: xbar(:)                      ! state
    real(8), allocatable :: xhat(:)                      ! state estimate
  end type
  
  type :: config_t
    integer(c_int) :: context
    real(8) :: sample_period = 0.0_wp
    integer :: n_offers = 0
    integer :: n_requests = 0
    type(string_container_t), allocatable :: offers(:), requests(:)
    real(wp), allocatable :: input_buffer(:), output_buffer(:)
    type(kalman_filter_t) :: kalman_filter
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
  ! local variables
  integer(hid_t) :: fid
  integer :: i, j
  class(error_t), allocatable :: error
  logical :: found
  real(8) :: tmp_double
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
        ! get measurement signal names
        call hdf5_read_varying_string_array_auto(fid, "/kalman_filter/measurement_signals", config(context)%kalman_filter%measurement_signals, error)
        if (allocated(error)) exit data_read
        ! get control input signal names
        call hdf5_read_varying_string_array_auto(fid, "/kalman_filter/control_signals", config(context)%kalman_filter%control_signals, error)
        if (allocated(error)) exit data_read
        ! get state signal names
        call hdf5_read_varying_string_array_auto(fid, "/kalman_filter/state_signals", config(context)%kalman_filter%state_signals, error)
        if (allocated(error)) exit data_read
        ! get operating point signal name
        call hdf5_read_string_auto(fid, "/kalman_filter/operating_point_signal", config(context)%kalman_filter%operating_point_signal, error)
        if (allocated(error)) exit data_read
        ! get operating point switch threshold
        call hdf5_read_array_auto(fid, "/kalman_filter/operating_point_switch_threshold", tmp_double, error)
        if (allocated(error)) exit data_read
        config(context)%kalman_filter%io_switch_threshold = nint(tmp_double)
        ! get operating points
        call hdf5_read_array_auto(fid, "/kalman_filter/operating_points", config(context)%kalman_filter%operating_points, error)
        if (allocated(error)) exit data_read
        ! get A
        call hdf5_read_array_auto(fid, "/kalman_filter/A", config(context)%kalman_filter%As, error)
        if (allocated(error)) exit data_read
        ! get B
        call hdf5_read_array_auto(fid, "/kalman_filter/B", config(context)%kalman_filter%Bs, error)
        if (allocated(error)) exit data_read
        ! get C
        call hdf5_read_array_auto(fid, "/kalman_filter/C", config(context)%kalman_filter%Cs, error)
        if (allocated(error)) exit data_read
        ! get C
        call hdf5_read_array_auto(fid, "/kalman_filter/D", config(context)%kalman_filter%Ds, error)
        if (allocated(error)) exit data_read
        ! get x0
        call hdf5_read_array_auto(fid, "/kalman_filter/x0", config(context)%kalman_filter%x0s, error)
        if (allocated(error)) exit data_read
        ! get u0
        call hdf5_read_array_auto(fid, "/kalman_filter/u0", config(context)%kalman_filter%u0s, error)
        if (allocated(error)) exit data_read
        ! get y0
        call hdf5_read_array_auto(fid, "/kalman_filter/y0", config(context)%kalman_filter%y0s, error)
        if (allocated(error)) exit data_read
        ! get Q
        call hdf5_read_array_auto(fid, "/kalman_filter/Q", config(context)%kalman_filter%Qs, error)
        if (allocated(error)) exit data_read
        ! get R
        call hdf5_read_array_auto(fid, "/kalman_filter/R", config(context)%kalman_filter%Rs, error)
        if (allocated(error)) exit data_read
      end block data_read
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
    config(context)%kalman_filter%nx = size(config(context)%kalman_filter%As, 2)
    config(context)%kalman_filter%nu = size(config(context)%kalman_filter%Bs, 2)
    config(context)%kalman_filter%ny = size(config(context)%kalman_filter%Cs, 1)
    config(context)%kalman_filter%no = size(config(context)%kalman_filter%operating_points, 1)
    if (config(context)%n_offers .ne. 2 * config(context)%kalman_filter%nx) then
      error = fail("Incorrect number of offered signals (" // itoa(config(context)%n_offers) // "); nx=" // itoa(config(context)%kalman_filter%nx))
      exit try
    end if
    allocate(config(context)%kalman_filter%i_y(config(context)%kalman_filter%ny))
    allocate(config(context)%kalman_filter%i_u(config(context)%kalman_filter%nu))
    allocate(config(context)%kalman_filter%A(config(context)%kalman_filter%nx,config(context)%kalman_filter%nx), source=0.0d0)
    allocate(config(context)%kalman_filter%B(config(context)%kalman_filter%nx,config(context)%kalman_filter%nu), source=0.0d0)
    allocate(config(context)%kalman_filter%C(config(context)%kalman_filter%ny,config(context)%kalman_filter%nx), source=0.0d0)
    allocate(config(context)%kalman_filter%D(config(context)%kalman_filter%ny,config(context)%kalman_filter%nu), source=0.0d0)
    allocate(config(context)%kalman_filter%x0(config(context)%kalman_filter%nx), source=0.0d0)
    allocate(config(context)%kalman_filter%xbar(config(context)%kalman_filter%nx), source=0.0d0)
    allocate(config(context)%kalman_filter%xhat(config(context)%kalman_filter%nx), source=0.0d0)
    allocate(config(context)%kalman_filter%u0(config(context)%kalman_filter%nu), source=0.0d0)
    allocate(config(context)%kalman_filter%y0(config(context)%kalman_filter%ny), source=0.0d0)
    if (size(config(context)%kalman_filter%Qs) .ne. config(context)%kalman_filter%no * (config(context)%kalman_filter%nx**2)) then
      error = fail("Incorrect dimensions for kalman filter Q; nx=" // itoa(config(context)%kalman_filter%nx))
      exit try
    end if
    if (size(config(context)%kalman_filter%Rs) .ne. config(context)%kalman_filter%no * (config(context)%kalman_filter%ny**2)) then
      error = fail("Incorrect dimensions for kalman filter R; ny=" // itoa(config(context)%kalman_filter%ny))
      exit try
    end if
    allocate(config(context)%kalman_filter%P(config(context)%kalman_filter%nx, config(context)%kalman_filter%nx), source = 0.0d0)
    allocate(config(context)%kalman_filter%Q(config(context)%kalman_filter%nx, config(context)%kalman_filter%nx), source = 0.0d0)
    allocate(config(context)%kalman_filter%R(config(context)%kalman_filter%ny, config(context)%kalman_filter%ny), source = 0.0d0)
    allocate(config(context)%kalman_filter%invR(config(context)%kalman_filter%ny, config(context)%kalman_filter%ny), source = 0.0d0)
    allocate(config(context)%kalman_filter%K(config(context)%kalman_filter%nx, config(context)%kalman_filter%ny), source = 0.0d0)
    
    ! initialize input signals:
    ! measurement signals, control signals, operating point
    config(context)%n_requests = size(config(context)%kalman_filter%measurement_signals) + &
      size(config(context)%kalman_filter%control_signals) + 1
    allocate(config(context)%requests(config(context)%n_requests))
    allocate(config(context)%input_buffer(config(context)%n_requests), source=0.0_wp)
    do i = 1,config(context)%kalman_filter%ny
      config(context)%requests(i)%s = config(context)%kalman_filter%measurement_signals(i)%s
      config(context)%kalman_filter%i_y(i) = i
    end do
    j = config(context)%kalman_filter%ny
    do i = 1,size(config(context)%kalman_filter%control_signals)
      config(context)%requests(i+j)%s = config(context)%kalman_filter%control_signals(i)%s
      config(context)%kalman_filter%i_u(i) = i + j
    end do
    j = config(context)%kalman_filter%ny + config(context)%kalman_filter%nu
    config(context)%requests(1+j)%s = config(context)%kalman_filter%operating_point_signal
    config(context)%kalman_filter%i_op = 1+j
    
    ! initialize output signals:
    ! state estimates, state estimates without operating point
    config(context)%n_offers = 2 * size(config(context)%kalman_filter%state_signals)
    allocate(config(context)%offers(config(context)%n_offers))
    allocate(config(context)%output_buffer(config(context)%n_offers), source=0.0_wp)
    do i = 1,config(context)%kalman_filter%nx
      config(context)%offers(i)%s = config(context)%kalman_filter%state_signals(i)%s
      config(context)%offers(i+config(context)%kalman_filter%nx)%s = "d" // config(context)%kalman_filter%state_signals(i)%s
    end do

    ! set values returned to caller
    ! context was assigned above
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
  integer :: info, i, io
  real(wp) :: u(config(context)%kalman_filter%nu)
  real(wp) :: y(config(context)%kalman_filter%ny)
  real(wp) :: operating_point
  real(wp) :: M(config(context)%kalman_filter%nx, config(context)%kalman_filter%nx)
  real(wp) :: CM(config(context)%kalman_filter%ny, config(context)%kalman_filter%nx)
  type(linalg_state_type) :: linalg_error
  
  try: block
    ! copy input signals
    do i = 1,config(context)%n_requests
      config(context)%input_buffer(i) = input_array(i)
    end do
    do i = 1,config(context)%kalman_filter%nu
      u(i) = config(context)%input_buffer(config(context)%kalman_filter%i_u(i))
    end do
    do i = 1,config(context)%kalman_filter%ny
      y(i) = config(context)%input_buffer(config(context)%kalman_filter%i_y(i))
    end do
    operating_point = config(context)%input_buffer(config(context)%kalman_filter%i_op)
    ! check if current operating point is closer to another operating point
    io = minloc(abs(config(context)%kalman_filter%operating_points - operating_point), dim=1)
    if (io .ne. config(context)%kalman_filter%io) then
      config(context)%kalman_filter%io_switch_count = config(context)%kalman_filter%io_switch_count + 1
    else
      config(context)%kalman_filter%io_switch_count = 0
    end if
    if ((config(context)%kalman_filter%io_switch_count .gt. config(context)%kalman_filter%io_switch_threshold) .or. &
      config(context)%kalman_filter%first_call) then
      ! reset x0, u0, y0
      config(context)%kalman_filter%x0 = config(context)%kalman_filter%x0s(:,io)
      config(context)%kalman_filter%u0 = config(context)%kalman_filter%u0s(:,io)
      config(context)%kalman_filter%y0 = config(context)%kalman_filter%y0s(:,io)
      ! reset model matrices A, B, C, D, Q, R
      config(context)%kalman_filter%A = config(context)%kalman_filter%As( :,:,io)
      config(context)%kalman_filter%B = config(context)%kalman_filter%Bs( :,:,io)
      config(context)%kalman_filter%C = config(context)%kalman_filter%Cs( :,:,io)
      config(context)%kalman_filter%D = config(context)%kalman_filter%Ds( :,:,io)
      config(context)%kalman_filter%Q = config(context)%kalman_filter%Qs( :,:,io)
      config(context)%kalman_filter%R = config(context)%kalman_filter%Rs( :,:,io)
      config(context)%kalman_filter%invR = inv(config(context)%kalman_filter%R, linalg_error)
      if (linalg_error /= LINALG_SUCCESS) then
        error = fail('stdlib_linalg error calculating R^-1: ' // linalg_error.print())
        exit try
      end if
      ! adjust the state estimate for the change in operating point
      config(context)%kalman_filter%xbar = config(context)%kalman_filter%xbar + &
        config(context)%kalman_filter%x0s(:,config(context)%kalman_filter%io) - &
        config(context)%kalman_filter%x0s(:,io)
      ! switch operating point
      config(context)%kalman_filter%io = io
      config(context)%kalman_filter%first_call = .false.
    end if
    ! compute one recursion of the Kalman filter equations
    u = u - config(context)%kalman_filter%u0
    y = y - config(context)%kalman_filter%y0
    ! measurement update
    config(context)%kalman_filter%xhat = config(context)%kalman_filter%xbar + &
      matmul(config(context)%kalman_filter%K, y - matmul(config(context)%kalman_filter%C, config(context)%kalman_filter%xbar) - &
                                                  matmul(config(context)%kalman_filter%D, u))
    ! time update
    config(context)%kalman_filter%xbar = matmul(config(context)%kalman_filter%A, config(context)%kalman_filter%xhat) + &
                                         matmul(config(context)%kalman_filter%B, u)
    M = matmul(config(context)%kalman_filter%A, matmul(config(context)%kalman_filter%P, transpose(config(context)%kalman_filter%A))) + config(context)%kalman_filter%Q ! technically, Bd*Q*Bd^T, but assume here that Bd=I
    CM = matmul(config(context)%kalman_filter%C, M)
    config(context)%kalman_filter%P = M - matmul(transpose(CM), matmul(inv(matmul(CM, transpose(config(context)%kalman_filter%C)) + config(context)%kalman_filter%R, linalg_error), CM))
    if (linalg_error /= LINALG_SUCCESS) then
      error = fail('stdlib_linalg error calculating (C * M * C^T + R)^-1: ' // linalg_error.print())
      exit try
    end if
    config(context)%kalman_filter%K = matmul(config(context)%kalman_filter%P, matmul(transpose(config(context)%kalman_filter%C), config(context)%kalman_filter%invR))
    ! output the state estimates
    do i = 1,config(context)%kalman_filter%nx
      output_array(i) = config(context)%kalman_filter%xhat(i) + config(context)%kalman_filter%x0(i)
      output_array(i + config(context)%kalman_filter%nx) = config(context)%kalman_filter%xhat(i)
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
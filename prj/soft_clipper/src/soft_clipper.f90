!  soft_clipper.f90 
!
!  FUNCTIONS/SUBROUTINES exported from soft_clipper.dll (conform to dispatch library API):
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
!  The cubic nonlinearity soft clipper has the slowest aliasing rate obtainable for an odd nonlinearity.
!  See https://ccrma.stanford.edu/~jos/pasp/Soft_Clipping.html for further information on the cubic nonlinearity soft clipper
!
module soft_clipper_library

  use extf, only: itoa, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: string_container_t, wp
  use iso_c_binding,  only: c_int, c_double, c_char

  implicit none

  ! library variables
  type :: soft_clipper_t
    real(wp) :: center_value
    real(wp) :: limits(2)
    real(wp) :: signal_range(2)
  end type
  
  type :: config_t
    integer(c_int) :: context
    real(wp)       :: sample_period = 0.0_wp
    integer        :: n_clipper = 0
    integer        :: n_offers = 0
    integer        :: n_requests = 0
    type(string_container_t), allocatable :: offers(:)
    type(string_container_t), allocatable :: requests(:)
    type(soft_clipper_t), allocatable :: soft_clipper(:)
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
      ! get soft_clipper table
      call get_value(table, 'soft_clipper', array, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read [soft_clipper]", origin, "at least one soft clipper is required."))
        call wrap_error(error, '"soft_clipper" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config(context)%n_clipper = len(array)
      config(context)%n_offers = config(context)%n_clipper
      config(context)%n_requests = config(context)%n_clipper
      allocate(config(context)%offers(config(context)%n_clipper))
      allocate(config(context)%requests(config(context)%n_clipper))
      allocate(config(context)%soft_clipper(config(context)%n_clipper))
      ! read input_signal_name, output_signal_name
      do i = 1,config(context)%n_clipper
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"soft_clipper"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "input_signal_name", config(context)%requests(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"soft_clipper"(' // itoa(i) // ') get_value("input_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "output_signal_name", config(context)%offers(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"soft_clipper"(' // itoa(i) // ') get_value("output_signal_name") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "center_value", config(context)%soft_clipper(i)%center_value, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"soft_clipper"(' // itoa(i) // ') get_value("center_value") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "saturation_limits", tmp_arr, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"soft_clipper"(' // itoa(i) // ') get_value("saturation_limits") returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(tmp_arr, 1, config(context)%soft_clipper(i)%limits(1))
        call get_value(tmp_arr, 2, config(context)%soft_clipper(i)%limits(2))
        nullify(tmp_arr)
        if (config(context)%soft_clipper(i)%limits(1) .ge. config(context)%soft_clipper(i)%center_value) then
          error = fail("Soft clipper " // itoa(i) // " lower saturation limit is >= center_value.")
          exit try
        elseif (config(context)%soft_clipper(i)%limits(2) .le. config(context)%soft_clipper(i)%center_value) then
          error = fail("Soft clipper " // itoa(i) // " upper saturation limit is <= center value.")
          exit try
        end if
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    
    ! initialize library variables
    do i = 1,config(context)%n_clipper
      ! scale the bounds to account for the saturation at +- 2/3
      config(context)%soft_clipper(i)%signal_range = 3.0_wp / 2.0_wp * abs(config(context)%soft_clipper(i)%limits - config(context)%soft_clipper(i)%center_value)
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
  real(wp) :: signal_range, normalized_input, normalized_output
  
  try: block
    ! sample each clipper
    do i = 1, config(context)%n_clipper
      if (input_array(i) .ge. config(context)%soft_clipper(i)%center_value) then
        signal_range = config(context)%soft_clipper(i)%signal_range(2)
      else
        signal_range = config(context)%soft_clipper(i)%signal_range(1)
      end if
      normalized_input = (input_array(i) - config(context)%soft_clipper(i)%center_value) / signal_range
      if (normalized_input .le. -1.0_wp) then
        normalized_output = -2.0_wp/3.0_wp
      elseif (normalized_input .le. 1) then
        normalized_output = normalized_input - (normalized_input ** 3.0_wp) / 3.0_wp
      else
        normalized_output = 2.0_wp/3.0_wp
      end if
      output_array(i) = (normalized_output * signal_range) + config(context)%soft_clipper(i)%center_value
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
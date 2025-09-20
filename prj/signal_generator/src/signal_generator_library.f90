!  signal_generator_library.f90 
!
!  FUNCTIONS/SUBROUTINES exported from signal_generator_library.dll (conform to dispatch library API):
!  initialize          - subroutine 
!  get_signal_offers   - subroutine 
!  get_signal_requests - subroutine 
!  update              - subroutine 
!  terminate           - subroutine
!
!  This library generates signals within a dispatch environment. Typical toml specification
!  for the library is as follows:
!
!  [interface]
!  sample_period = 0.02
!  [[signal]]
!  output_signal_name = "my_constant_signal"
!  waveform = "constant" # constant, sine, square_wave, prbs, ...
!  offset = 3
!  [[signal]]
!  trigger_signal_name = "loop_open"
!  input_signal_name = "pitch_reference"
!  input_signal_hold = true
!  abscissa_signal_name = "current_time"
!  output_signal_name = "my_sinusoid_signal"
!  waveform = "sine" # constant, sine, square_wave, prbs, ...
!  offset = 6
!  amplitude = 2
!  frequency = 4
!  phase = 1
!
!  There is no limit to the number of signals this generator can offer. 
!
module signal_generator_library
  
  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp
  use ifport
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  use mkl_vsl_type
  use mkl_vsl

  implicit none

  ! library variables
  type :: signal_t
    real(wp) :: offset
  end type

  type, extends(signal_t) :: constant_t
  end type

  type, extends(signal_t) :: ramp_t
    real(wp) :: increment                 ! the amount by which the signal changes each time it is sampled
    integer  :: sample_count = 0          ! a count of the number of times the signal generator has been sampled
  end type

  type, extends(signal_t) :: sinusoid_t
    real(wp) :: amplitude                 ! amplitude (half-wave; peak-to-peak is 2x this value)
    real(wp) :: frequency                 ! frequency (specified in units of cycles/[units of x])
    real(wp) :: phase                     ! phase (0 <= phase < 1; specified as a fraction of 2 * pi, i.e. pi/2 would be 0.25)
    real(wp) :: period                    ! period (=2pi)
  end type

  type, extends(signal_t) :: square_wave_t
    real(wp) :: amplitude                 ! amplitude (peak-to-peak)
    real(wp) :: frequency                 ! frequency (specified in units of cycles/[units of x])
    real(wp) :: phase                     ! phase (0 <= phase < 1; specified as a fraction of the period)
    real(wp) :: period                    ! period (1/frequency)
  end type

  type, extends(signal_t) :: rectangular_wave_t
    real(wp) :: amplitude                 ! amplitude (peak-to-peak)
    real(wp) :: period                    ! period (in units of x)
    real(wp) :: phase                     ! phase (0 <= phase < 1; specified as a fraction of the period)
    real(wp) :: pulse_width               ! in units of x, must be <= period
    real(wp) :: position_in_period = 0.0_wp ! position in the period (in units of x)
    real(wp) :: sign = 1.0_wp               ! whether the pulse is up (1) or not (-1)
  end type

  type, extends(signal_t) :: triangle_wave_t
    real(wp) :: amplitude                 ! amplitude (peak-to-peak)
    real(wp) :: frequency                 ! frequency (specified in units of cycles/[units of x])
    real(wp) :: phase                     ! phase (0 <= phase < 1; specified as a fraction of the period)
    real(wp) :: period                    ! period (1/frequency)
  end type

  type, extends(signal_t) :: gaussian_noise_t
    real(wp) :: sigma                     ! standard deviation
    integer  :: seed                      ! integer seed
    type(vsl_stream_state) :: stream = vsl_stream_state(0,0) ! random number generator
    real(wp) :: r(1) = 0.0_wp             ! realization of the noise process
  end type

  type, extends(signal_t) :: uniform_noise_t
    real(wp) :: width                     ! distribution width
    integer  :: seed                      ! integer seed
    type(vsl_stream_state) :: stream = vsl_stream_state(0,0) ! random number generator
    real(wp) :: r(1) = 0.0_wp             ! realization of the noise process
    real(wp) :: a = 0.0_wp                ! lower limit of the noise distribution
    real(wp) :: b = 0.0_wp                ! upper limit of the noise distribution
  end type

  type, extends(signal_t) :: prbs_t
    real(wp) :: amplitude                 ! amplitude of the prbs signal (peak to peak)
    integer  :: seed                      ! integer seed
    integer  :: order                     ! order of the monic polynomial (7, 9, 11, 13, 15, 20, 23, or 31)
    integer(kind=4) :: register           ! linear feedback shift register
    integer(kind=4) :: taps(5) = [0,0,0,0,0] ! elements of the register's monic polynomial
    integer(kind=4) :: r = 0              ! realization of the bit sequence
  end type

  type :: signal_array_t
    class(signal_t), allocatable :: signal
  end type

  type :: config_t
    integer(c_int) :: context
    real(wp) :: sample_period = 0.0_wp
    integer  :: n_signal = 0
    logical, allocatable :: has_trigger(:)
    logical, allocatable :: is_triggered(:)
    logical, allocatable :: has_input(:)
    logical, allocatable :: input_signal_hold(:)
    real(wp), allocatable :: input_signal_value(:)
    logical, allocatable :: has_abscissa(:)
    type(string_container_t), allocatable :: trigger_signals(:)
    type(string_container_t), allocatable :: input_signals(:)
    type(string_container_t), allocatable :: abscissa_signals(:)
    type(string_container_t), allocatable :: output_signals(:)
    type(string_container_t), allocatable :: waveform_names(:)
    type(signal_array_t),     allocatable :: waveforms(:)
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
  real(wp)                             :: offset, increment, amplitude, frequency, phase, period, pulse_width
  integer :: stat, origin, i, j, n, seed, order
  integer(kind=4) :: error_code
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
      allocate(config(context)%has_trigger(config(context)%n_signal), source = .false.)
      allocate(config(context)%is_triggered(config(context)%n_signal), source = .false.)
      allocate(config(context)%has_input(config(context)%n_signal), source = .false.)
      allocate(config(context)%input_signal_hold(config(context)%n_signal), source = .false.)
      allocate(config(context)%input_signal_value(config(context)%n_signal), source = 0.0_wp)
      allocate(config(context)%has_abscissa(config(context)%n_signal), source = .false.)
      allocate(config(context)%trigger_signals(config(context)%n_signal))
      allocate(config(context)%input_signals(config(context)%n_signal))
      allocate(config(context)%abscissa_signals(config(context)%n_signal))
      allocate(config(context)%output_signals(config(context)%n_signal))
      allocate(config(context)%waveform_names(config(context)%n_signal))
      allocate(config(context)%waveforms(config(context)%n_signal))
      n_requests = 0
      ! read waveform parameters
      do i = 1,config(context)%n_signal
        call get_value(array, i, child, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail('"signal"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "trigger_signal_name", config(context)%trigger_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          config(context)%has_trigger(i) = .false.
        else
          config(context)%has_trigger(i) = .true.
          n_requests = n_requests + 1
        end if
        config(context)%is_triggered(i) = .false. ! used with & without trigger, to indicate first sample in case of input hold
        call get_value(child, "input_signal_name", config(context)%input_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          config(context)%has_input(i) = .false.
        else
          config(context)%has_input(i) = .true.
          n_requests = n_requests + 1
        end if
        call get_value(child, "input_signal_hold", config(context)%input_signal_hold(i), stat=stat)
        if (stat .ne. toml_stat%success) then
          config(context)%input_signal_hold(i) = .false.
        end if
        call get_value(child, "output_signal_name", config(context)%output_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read output_signal_name", origin, "expected character value."))
          call wrap_error(error, '"signal(' // itoa(i) // ').output_name" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        call get_value(child, "waveform", config(context)%waveform_names(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read waveform", origin, "expected character value."))
          call wrap_error(error, '"signal(' // itoa(i) // ').waveform" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        ! get waveform-dependent parameters
        config(context)%has_abscissa(i) = .false. ! only non-constant function-based signals have abscissas
        select case(config(context)%waveform_names(i)%s)
        case ('constant')
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read offset", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').offset" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          config(context)%waveforms(i)%signal = constant_t(offset)
        case ('ramp')
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read offset", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').offset" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "increment", increment, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read increment", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').increment" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          config(context)%waveforms(i)%signal = ramp_t(offset, increment)
        case ('sine')
          call get_value(child, "abscissa_signal_name", config(context)%abscissa_signals(i)%s, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail("'sine' waveform requires abscissa signal; signal[" // itoa(i) // "]")
            exit try
          else
            config(context)%has_abscissa(i) = .true.
            n_requests = n_requests + 1
          end if
          call get_value(child, "amplitude", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read amplitude", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').amplitude" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "frequency", frequency, stat=stat)
          if (stat .ne. toml_stat%success) then
            frequency = 1.0_wp
          end if
          call get_value(child, "phase", phase, stat=stat)
          if (stat .ne. toml_stat%success) then
            phase = 0.0_wp
          end if
          config(context)%waveforms(i)%signal = sinusoid_t(offset, amplitude, frequency * 8.0_wp * atan(1.0_wp), phase, 8.0_wp * atan(1.0_wp))
        case ('square_wave')
          call get_value(child, "abscissa_signal_name", config(context)%abscissa_signals(i)%s, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail("'square_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
            exit try
          else
            config(context)%has_abscissa(i) = .true.
            n_requests = n_requests + 1
          end if
          call get_value(child, "amplitude", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read amplitude", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').amplitude" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "frequency", frequency, stat=stat)
          if (stat .ne. toml_stat%success) then
            frequency = 1.0_wp
          end if
          call get_value(child, "phase", phase, stat=stat)
          if (stat .ne. toml_stat%success) then
            phase = 0.0_wp
          end if
          config(context)%waveforms(i)%signal = square_wave_t(offset, amplitude, frequency, phase, 1.0_wp/frequency)
        case ('rectangular_wave')
          call get_value(child, "abscissa_signal_name", config(context)%abscissa_signals(i)%s, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail("'rectangular_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
            exit try
          else
            config(context)%has_abscissa(i) = .true.
            n_requests = n_requests + 1
          end if
          call get_value(child, "amplitude", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read amplitude", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').amplitude" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "period", period, stat=stat)
          if (stat .ne. toml_stat%success) then
            period = 1.0_wp
          end if
          call get_value(child, "phase", phase, stat=stat)
          if (stat .ne. toml_stat%success) then
            phase = 0.0_wp
          end if
          call get_value(child, "pulse_width", pulse_width, stat=stat)
          if (stat .ne. toml_stat%success) then
            pulse_width = 0.0_wp
          end if
          config(context)%waveforms(i)%signal = rectangular_wave_t(offset, amplitude, period, phase, pulse_width)
        case ('triangle_wave')
          call get_value(child, "abscissa_signal_name", config(context)%abscissa_signals(i)%s, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail("'triangle_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
            exit try
          else
            config(context)%has_abscissa(i) = .true.
            n_requests = n_requests + 1
          end if
          call get_value(child, "amplitude", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read amplitude", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').amplitude" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "frequency", frequency, stat=stat)
          if (stat .ne. toml_stat%success) then
            frequency = 1.0_wp
          end if
          call get_value(child, "phase", phase, stat=stat)
          if (stat .ne. toml_stat%success) then
            phase = 0.0_wp
          end if
          config(context)%waveforms(i)%signal = triangle_wave_t(offset, amplitude, frequency, phase + 1.0_wp/4.0_wp, 1.0_wp/frequency)
        case ('gaussian_noise')
          call get_value(child, "sigma", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read sigma", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').sigma" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "seed", seed, stat=stat)
          if (stat .ne. toml_stat%success) then
            seed = 1
          end if
          config(context)%waveforms(i)%signal = gaussian_noise_t(offset, amplitude, seed)
        case ('uniform_noise')
          call get_value(child, "width", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read width", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').width" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "seed", seed, stat=stat)
          if (stat .ne. toml_stat%success) then
            seed = 1
          end if
          config(context)%waveforms(i)%signal = uniform_noise_t(offset, amplitude, seed)
        case ('prbs')
          call get_value(child, "amplitude", amplitude, stat=stat)
          if (stat .ne. toml_stat%success) then
            error = fail(context_toml%report("Cannot read amplitude", origin, "expected numeric value."))
            call wrap_error(error, '"signal(' // itoa(i) // ').amplitude" get_value returned status: ' // toml_error_message(stat))
            exit try
          end if
          call get_value(child, "offset", offset, stat=stat)
          if (stat .ne. toml_stat%success) then
            offset = 0.0_wp
          end if
          call get_value(child, "seed", seed, stat=stat)
          if (stat .ne. toml_stat%success) then
            seed = 1
          end if
          call get_value(child, "order", order, stat=stat)
          if (stat .ne. toml_stat%success) then
            seed = 1
          end if
          config(context)%waveforms(i)%signal = prbs_t(offset, amplitude, seed, order, seed)
        case default
          error = fail('unsupported waveform: ' // config(context)%waveform_names(i)%s)
          exit try
        end select
      end do
    else
      error = fail("Parameters must be specified as toml string.")
      exit try
    end if
    ! initialize the variables needed by the library
    do i = 1,config(context)%n_signal
      select type(s => config(context)%waveforms(i)%signal)
      type is (gaussian_noise_t)
        error_code = vslnewstream(s%stream, VSL_BRNG_MCG31, s%seed)
        if (error_code .ne. 0) then
          error = fail("Cannot initialize gaussian noise generator (signal[" // itoa(i) //"]); error code: " // itoa(error_code))
          exit try
        end if
      type is (uniform_noise_t)
        s%a = s%offset - s%width / 2.0_wp
        s%b = s%a + s%width
        error_code = vslnewstream(s%stream, VSL_BRNG_MCG31, s%seed)
        if (error_code .ne. 0) then
          error = fail("Cannot initialize uniform noise generator (signal[" // itoa(i) //"]); error code: " // itoa(error_code))
          exit try
        end if
      type is (prbs_t)
        select case(s%order)
        case (7)
          s%taps = [7, 6, 0, 0, 0]
        case (9)
          s%taps = [9, 5, 0, 0, 0]
        case (11)
          s%taps = [11, 9, 0, 0, 0]
        case (13)
          s%taps = [13, 12, 2, 1, 0]
        case (15)
          s%taps = [15, 14, 0, 0, 0]
        case (20)
          s%taps = [20, 3, 0, 0, 0]
        case (23)
          s%taps = [23, 18, 0, 0, 0]
        case (31)
          s%taps = [31, 28, 0, 0, 0]
        case default
          error = fail("Invalid prbs generator order (signal[" // itoa(i) //"]): order must be [7, 9, 11, 13, 15, 20, 23, or 31]")
          exit try
        end select
      end select
    end do
    
    ! set values returned to caller
    ! context was assigned a value above
    sample_period = config(context)%sample_period
    n_offers = config(context)%n_signal
    ! n_requests was computed above
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
  type(string_container_t), allocatable :: input_signals(:)
  integer :: i, j
  
  j = 0
  do i = 1,config(context)%n_signal
    if (config(context)%has_trigger(i)) then
      j = j + 1
    end if
    if (config(context)%has_input(i)) then
      j = j + 1
    end if
    if (config(context)%has_abscissa(i)) then
      j = j + 1
    end if
  end do
  
  allocate(input_signals(j))
  
  j = 0
  do i = 1,config(context)%n_signal
    if (config(context)%has_trigger(i)) then
      j = j + 1
      input_signals(j)%s = config(context)%trigger_signals(i)%s
    end if
    if (config(context)%has_input(i)) then
      j = j + 1
      input_signals(j)%s = config(context)%input_signals(i)%s
    end if
    if (config(context)%has_abscissa(i)) then
      j = j + 1
      input_signals(j)%s = config(context)%abscissa_signals(i)%s
    end if
  end do
  
  call string_container_array_to_delimited_string(input_signals, ";", requests)
  
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
  integer :: i, j, k, error_code, random_bit
  integer :: n_realization = 1
  
  try: block
    j = 0
    do i = 1,config(context)%n_signal
      ! assign the appropriate input signal value to the signal generator
      ! no trigger, no input, no hold -> input_signal_value = 0
      ! no trigger, no input,    hold -> input_signal_value = 0
      ! no trigger,    input, no hold -> input_signal_value = input_array(j)
      ! no trigger,    input,    hold -> input_signal_value = input_array(j) from first call
      !    trigger, no input, no hold -> input_signal_value = 0
      !    trigger, no input,    hold -> input_signal_value = 0
      !    trigger,    input, no hold -> input_signal_value = input_array(j)
      !    trigger,    input,    hold -> input_signal_value = input_array(j) from call when triggered (0 before)
      if (config(context)%has_trigger(i)) then
        j = j + 1
        if (input_array(j) .gt. epsilon(input_array(j))) then
          if (config(context)%has_input(i)) then
            j = j + 1
            if (config(context)%input_signal_hold(i)) then
              if (.not. config(context)%is_triggered(i)) then
                config(context)%input_signal_value(i) = input_array(j)
              ! else input signal value does not update on this call
              end if
            else
              config(context)%input_signal_value(i) = input_array(j)
            end if
          else
            config(context)%input_signal_value(i) = 0.0_wp
          end if
          config(context)%is_triggered(i) = .true.
        else
          if (config(context)%has_input(i)) then
            j = j + 1
            config(context)%input_signal_value(i) = input_array(j)
          else
            config(context)%input_signal_value(i) = 0.0_wp
          end if
          config(context)%is_triggered(i) = .false.
        end if
      else
        if (config(context)%has_input(i)) then
          j = j + 1
          if (config(context)%input_signal_hold(i)) then
            if (.not. config(context)%is_triggered(i)) then
              config(context)%input_signal_value(i) = input_array(j)
            ! else input signal value does not update on this call
            end if
          else
            config(context)%input_signal_value(i) = input_array(j)
          end if
        else
          config(context)%input_signal_value(i) = 0.0_wp
        end if
        config(context)%is_triggered(i) = .true.
      end if
      if (config(context)%has_abscissa(i)) then
        ! set j so that input_array(j) is the requested abscissa signal for this generator
        j = j + 1
      end if
      ! generate the requested waveform
      if (config(context)%is_triggered(i)) then
        select type(s => config(context)%waveforms(i)%signal)
        type is (constant_t)
          output_array(i) = config(context)%input_signal_value(i) + s%offset
        type is (ramp_t)
          output_array(i) = config(context)%input_signal_value(i) + s%offset + s%increment * s%sample_count
          ! protect against integer overflow
          if (s%sample_count .gt. huge(0) - 10) then
            s%offset = output_array(i)
            s%sample_count = 0
          end if
          s%sample_count = s%sample_count + 1
        type is (sinusoid_t)
          output_array(i) = config(context)%input_signal_value(i) + s%offset + s%amplitude * sin(s%frequency * input_array(j) + s%phase * s%period)
        type is (square_wave_t)
          output_array(i) = config(context)%input_signal_value(i) + s%offset + sign(s%amplitude, 0.5_wp - s%frequency * mod(input_array(j) + s%phase * s%period, s%period))
        type is (rectangular_wave_t)
          s%position_in_period = mod(input_array(j) + s%phase * s%period, s%period)
          if (s%position_in_period .gt. s%pulse_width) then
            s%sign = -1.0_wp
          else
            s%sign = 1.0_wp
          end if
          output_array(i) = config(context)%input_signal_value(i) + s%offset + sign(s%amplitude, s%sign)
        type is (triangle_wave_t)
          output_array(i) = config(context)%input_signal_value(i) + s%offset + s%amplitude * 2.0_wp * (2.0_wp * abs((input_array(j) + s%phase * s%period) * s%frequency - floor((input_array(j) + s%phase * s%period) * s%frequency + 0.5_wp)) - 0.5_wp)
        type is (gaussian_noise_t)
          error_code = vdrnggaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF, s%stream, n_realization, s%r, s%offset, s%sigma)
          if (error_code .ne. 0) then
            error = fail("Cannot sample gaussian noise generator (signal[" // itoa(i) //"]); error code: " // itoa(error_code))
            exit try
          end if
          output_array(i) = config(context)%input_signal_value(i) + s%r(1)
        type is (uniform_noise_t)
          error_code = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, s%stream, n_realization, s%r, s%a, s%b)
          if (error_code .ne. 0) then
            error = fail("Cannot sample uniform noise generator (signal[" // itoa(i) //"]); error code: " // itoa(error_code))
            exit try
          end if
          output_array(i) = config(context)%input_signal_value(i) + s%r(1)
        type is (prbs_t)
          random_bit = 0
          do k = 1, 5
            random_bit = ieor(random_bit, iand(ishft(s%register, -s%taps(k)), 1))
          end do
          s%register = ishft(s%register, 1)
          s%register = ieor(s%register, random_bit)
          s%r = iand(s%register, 1)
          output_array(i) = config(context)%input_signal_value(i) + s%offset + (real(s%r, kind=8) - 0.5_wp) * s%amplitude
        end select
      else
        output_array(i) = config(context)%input_signal_value(i)
      end if
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
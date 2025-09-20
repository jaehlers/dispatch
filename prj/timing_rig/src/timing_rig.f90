!  dispatch_test_and_timing_rig.f90 
!
!  This library calls a dispatch environment with signals it generates based on parameters passed to the program. Typical toml specification
!  for the program is as follows (signal generation is specified exactly the same way as in signal_generator_library):
!
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
!  [test_and_timing]
!  start_time = 0.0
!  sample_period = 0.0125
!  dispatch_file = "foo.in"
!  n_test = 1000
!  n_sample_per_test = 10000
!
!  There is no limit to the number of signals that can be offered to the dispatch environment. 
!
program dispatch_test_and_timing_rig
  
  use extf, only: itoa, string_container_t, string_container_array_to_delimited_string
  use error_handling, only: error_t, fail, wrap_error
  use dispatch_types, only: wp, signal_descriptor_t
  use dispatch_manager, only: dispatch_initialize, dispatch_sample, dispatch_terminate
  use ifport, only: qsort
  use mkl_vsl_type
  use mkl_vsl, only: vslnewstream, vdrnggaussian, vdrnguniform
  use tomlf
  use tomlf_error, only: toml_error, toml_stat
  use tomlf_error_handler, only: toml_error_message

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
    real(wp) :: start_time = 0.0_wp
    real(wp) :: sample_period = 0.0_wp
    integer  :: n_test = 0
    integer  :: n_sample_per_test = 0
    integer  :: n_signal = 0
    integer  :: n_requests = 0
    logical,  allocatable :: has_trigger(:)
    logical,  allocatable :: is_triggered(:)
    logical,  allocatable :: has_input(:)
    logical,  allocatable :: input_signal_hold(:)
    real(wp), allocatable :: input_signal_value(:)
    logical,  allocatable :: has_abscissa(:)
    integer,  allocatable :: input_source(:)
    type(string_container_t), allocatable :: trigger_signals(:)
    type(string_container_t), allocatable :: input_signals(:)
    type(string_container_t), allocatable :: abscissa_signals(:)
    type(string_container_t), allocatable :: output_signals(:)
    type(string_container_t), allocatable :: waveform_names(:)
    type(signal_array_t),     allocatable :: waveforms(:)
    character(len=:),         allocatable :: dispatch_file
    type(signal_descriptor_t),allocatable :: generated_signals(:)
    type(signal_descriptor_t),allocatable :: received_signals(:)
  end type
  type(config_t) :: config

  ! program variables
  integer :: i, j, k, ii, kk, n, n_args, arg_length
  character(len=1) :: tmp_arg
  character(len=:), allocatable :: input_file
  character(len=:), allocatable :: error_string
  real(wp),         allocatable :: execution_time(:)
  real(wp)                      :: median_execution_time
  real(wp)                      :: tic, toc, current_time
  integer(kind=4)               :: error_code
  integer                       :: random_bit
  integer                       :: n_realization = 1
  integer                       :: n_dispatch_input, n_dispatch_output
  real(wp),         allocatable :: input_to_dispatch(:)
  real(wp),         allocatable :: output_from_dispatch(:)
  logical                       :: found_match
  integer,          allocatable :: input_indices(:), output_indices(:)
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
  real(wp)                             :: tmp_real
  real(wp)                             :: offset, increment, amplitude, frequency, phase, period, pulse_width
  logical                              :: file_exists
  integer :: stat, origin, seed, order, fid, rc

  try: block
    ! read command-line arguments
    n_args = command_argument_count()
    if (n_args .ne. 1) then
      error = fail('Incorrect number of command-line arguments to timing_rig: 1 required (input file name, with path if necessary)')
      exit try
    end if
    call get_command_argument(1, tmp_arg, arg_length)
    allocate(character(len=arg_length) :: input_file)
    call get_command_argument(1, input_file)

    ! check whether input_file exists
    inquire (file=input_file, exist=file_exists)
    if (.not. file_exists) then
      error = fail("TOML file " // trim(input_file) // " not found")
      exit try
    end if
    ! open input_file
    open (action='read', file=input_file, iostat=rc, newunit=fid)
    if (rc /= 0) then
      error = fail("Reading TOML file " // trim(input_file) // " failed")
      exit try
    end if
    ! read and parse input_file, then close
    call toml_parse(table, fid, error_toml)
    close (fid)
    if (allocated(error_toml)) then
      error = fail(error_toml%message)
      exit try
    end if
    ! get signal table
    call get_value(table, 'signal', array, stat=stat, origin=origin)
    if (stat .ne. toml_stat%success) then
      error = fail(context_toml%report("Cannot read [signal]", origin, "at least one signal is required."))
      call wrap_error(error, '"signal" get_value returned status: ' // toml_error_message(stat))
      exit try
    end if
    config%n_signal = len(array)
    allocate(config%has_trigger(config%n_signal), source = .false.)
    allocate(config%is_triggered(config%n_signal), source = .false.)
    allocate(config%has_input(config%n_signal), source = .false.)
    allocate(config%input_signal_hold(config%n_signal), source = .false.)
    allocate(config%input_signal_value(config%n_signal), source = 0.0_wp)
    allocate(config%has_abscissa(config%n_signal), source = .false.)
    allocate(config%trigger_signals(config%n_signal))
    allocate(config%input_signals(config%n_signal))
    allocate(config%abscissa_signals(config%n_signal))
    allocate(config%output_signals(config%n_signal))
    allocate(config%waveform_names(config%n_signal))
    allocate(config%waveforms(config%n_signal))
    allocate(config%generated_signals(config%n_signal))
    allocate(config%received_signals(0))
    config%n_requests = 0
    ! read waveform parameters
    do i = 1,config%n_signal
      call get_value(array, i, child, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail('"signal"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      call get_value(child, "trigger_signal_name", config%trigger_signals(i)%s, stat=stat)
      if (stat .ne. toml_stat%success) then
        config%has_trigger(i) = .false.
      else
        config%has_trigger(i) = .true.
        config%n_requests = config%n_requests + 1
      end if
      config%is_triggered(i) = .false. ! used with & without trigger, to indicate first sample in case of input hold
      call get_value(child, "input_signal_name", config%input_signals(i)%s, stat=stat)
      if (stat .ne. toml_stat%success) then
        config%has_input(i) = .false.
      else
        config%has_input(i) = .true.
        config%n_requests = config%n_requests + 1
      end if
      call get_value(child, "input_signal_hold", config%input_signal_hold(i), stat=stat)
      if (stat .ne. toml_stat%success) then
        config%input_signal_hold(i) = .false.
      end if
      call get_value(child, "output_signal_name", config%output_signals(i)%s, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read output_signal_name", origin, "expected character value."))
        call wrap_error(error, '"signal(' // itoa(i) // ').output_name" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      call get_value(child, "waveform", config%waveform_names(i)%s, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read waveform", origin, "expected character value."))
        call wrap_error(error, '"signal(' // itoa(i) // ').waveform" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! get waveform-dependent parameters
      config%has_abscissa(i) = .false. ! only non-constant function-based signals have abscissas
      select case(config%waveform_names(i)%s)
      case ('constant')
        call get_value(child, "offset", offset, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail(context_toml%report("Cannot read offset", origin, "expected numeric value."))
          call wrap_error(error, '"signal(' // itoa(i) // ').offset" get_value returned status: ' // toml_error_message(stat))
          exit try
        end if
        config%waveforms(i)%signal = constant_t(offset)
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
        config%waveforms(i)%signal = ramp_t(offset, increment)
      case ('sine')
        call get_value(child, "abscissa_signal_name", config%abscissa_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail("'sine' waveform requires abscissa signal; signal[" // itoa(i) // "]")
          exit try
        else
          config%has_abscissa(i) = .true.
          config%n_requests = config%n_requests + 1
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
        config%waveforms(i)%signal = sinusoid_t(offset, amplitude, frequency * 8.0_wp * atan(1.0_wp), phase, 8.0_wp * atan(1.0_wp))
      case ('square_wave')
        call get_value(child, "abscissa_signal_name", config%abscissa_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail("'square_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
          exit try
        else
          config%has_abscissa(i) = .true.
          config%n_requests = config%n_requests + 1
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
        config%waveforms(i)%signal = square_wave_t(offset, amplitude, frequency, phase, 1.0_wp/frequency)
      case ('rectangular_wave')
        call get_value(child, "abscissa_signal_name", config%abscissa_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail("'rectangular_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
          exit try
        else
          config%has_abscissa(i) = .true.
          config%n_requests = config%n_requests + 1
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
        config%waveforms(i)%signal = rectangular_wave_t(offset, amplitude, period, phase, pulse_width)
      case ('triangle_wave')
        call get_value(child, "abscissa_signal_name", config%abscissa_signals(i)%s, stat=stat)
        if (stat .ne. toml_stat%success) then
          error = fail("'triangle_wave' waveform requires abscissa signal; signal[" // itoa(i) // "]")
          exit try
        else
          config%has_abscissa(i) = .true.
          config%n_requests = config%n_requests + 1
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
        config%waveforms(i)%signal = triangle_wave_t(offset, amplitude, frequency, phase + 1.0_wp/4.0_wp, 1.0_wp/frequency)
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
        config%waveforms(i)%signal = gaussian_noise_t(offset, amplitude, seed)
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
        config%waveforms(i)%signal = uniform_noise_t(offset, amplitude, seed)
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
        config%waveforms(i)%signal = prbs_t(offset, amplitude, seed, order, seed)
      case default
        error = fail('unsupported waveform: ' // config%waveform_names(i)%s)
        exit try
      end select
    end do
    ! get values from [test_and_timing] section
    call get_value(table, 'test_and_timing', child, stat=stat, origin=origin)
    if (stat .ne. toml_stat%success) then
      error = fail(context_toml%report("Cannot read [test_and_timing]", origin, "section is required."))
      call wrap_error(error, '"test_and_timing" get_value returned status: ' // toml_error_message(stat))
      exit try
    end if
    if (associated(child)) then
      ! start_time
      call get_value(child, 'start_time', config%start_time, stat=stat, origin=origin, default = 0.0_wp)
      ! sample period
      call get_value(child, 'sample_period', config%sample_period, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read sample_period", origin, "expected real value."))
        call wrap_error(error, '"test_and_timing.sample_period" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! dispatch_file
      call get_value(child, 'dispatch_file', config%dispatch_file, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read dispatch_file", origin, "expected character string."))
        call wrap_error(error, '"test_and_timing.dispatch_file" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! n_test
      call get_value(child, 'n_test', tmp_real, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read n_test", origin, "expected integer value."))
        call wrap_error(error, '"test_and_timing.n_test" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config%n_test = nint(tmp_real)
      ! n_sample_per_test
      call get_value(child, 'n_sample_per_test', tmp_real, stat=stat, origin=origin)
      if (stat .ne. toml_stat%success) then
        error = fail(context_toml%report("Cannot read n_sample_per_test", origin, "expected integer value."))
        call wrap_error(error, '"test_and_timing.n_sample_per_test" get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      config%n_sample_per_test = nint(tmp_real)
    end if
    ! initialize the variables needed by the library
    do i = 1,config%n_signal
      select type(s => config%waveforms(i)%signal)
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

    ! resolve internal signal connections
    ii = 0
    do i = 1,config%n_signal
      if (config%has_trigger(i)) then
        ii = ii + 1
      end if
      if (config%has_input(i)) then
        ii = ii + 1
      end if
      if (config%has_abscissa(i)) then
        ii = ii + 1
      end if
    end do
    allocate(config%input_source(ii))
    ii = 1
    do i = 1,config%n_signal
      if (config%has_trigger(i)) then
        found_match = .false.
        trigger_source_search: do j = i-1,1,-1
          if (config%trigger_signals(i)%s .eq. config%output_signals(j)%s) then
            config%input_source(ii) = j
            ii = ii + 1
            found_match = .true.
            exit trigger_source_search
          end if
        end do trigger_source_search
        if (.not. found_match) then
          error = fail("Failed to find source for trigger signal '" // config%trigger_signals(i)%s // "' (input to generated signal '" // config%output_signals(j)%s // "')")
          exit try
        end if
      end if
      if (config%has_input(i)) then
        found_match = .false.
        input_source_search: do j = i-1,1,-1
          if (config%input_signals(i)%s .eq. config%output_signals(j)%s) then
            config%input_source(ii) = j
            ii = ii + 1
            found_match = .true.
            exit input_source_search
          end if
        end do input_source_search
        if (.not. found_match) then
          error = fail("Failed to find source for input signal '" // config%input_signals(i)%s // "' (input to generated signal '" // config%output_signals(j)%s // "')")
          exit try
        end if
      end if
      if (config%has_abscissa(i)) then
        found_match = .false.
        abscissa_source_search: do j = i-1,1,-1
          if (config%abscissa_signals(i)%s .eq. config%output_signals(j)%s) then
            config%input_source(ii) = j
            ii = ii + 1
            found_match = .true.
            exit abscissa_source_search
          end if
        end do abscissa_source_search
        if (.not. found_match) then
          error = fail("Failed to find source for abscissa signal '" // config%abscissa_signals(i)%s // "' (input to generated signal '" // config%output_signals(j)%s // "')")
          exit try
        end if
      end if
    end do

    ! make an array of signal_descriptor_t objects to be passed to dispatch_initialize()
    do i = 1,config%n_signal
      config%generated_signals(i) = signal_descriptor_t(config%output_signals(i)%s, "r", "rw", "-", "")
    end do
    allocate(input_to_dispatch(config%n_signal))
      
    ! run tests
    allocate(execution_time(config%n_test), source=0.0_wp)
    do i = 1,config%n_test
      ! initialize dispatch
      call dispatch_initialize(config%dispatch_file, n_dispatch_input, n_dispatch_output, error, config%generated_signals, config%received_signals, input_indices, output_indices)
      if (allocated(error)) then
        call wrap_error(error, "Failure in call to dispatch_initialize().")
        exit try
      end if
      if (.not. allocated(output_from_dispatch)) then
        allocate(output_from_dispatch(n_dispatch_output))
      end if
      ! reset all the signal generators
      ! sample dispatch repeatedly
      call cpu_time(tic)
      do j = 1,config%n_sample_per_test
        ii = 0
        do k = 1,config%n_signal
          ! assign the appropriate input signal value to the signal generator
          ! no trigger, no input, no hold -> input_signal_value = 0
          ! no trigger, no input,    hold -> input_signal_value = 0
          ! no trigger,    input, no hold -> input_signal_value = input_array(config%input_source(ii))
          ! no trigger,    input,    hold -> input_signal_value = input_array(config%input_source(ii)) from first call
          !    trigger, no input, no hold -> input_signal_value = 0
          !    trigger, no input,    hold -> input_signal_value = 0
          !    trigger,    input, no hold -> input_signal_value = input_array(config%input_source(ii))
          !    trigger,    input,    hold -> input_signal_value = input_array(config%input_source(ii)) from call when triggered (0 before)
          if (config%has_trigger(k)) then
            ii = ii + 1
            if (input_to_dispatch(config%input_source(ii)) .gt. epsilon(0.0_wp)) then
              if (config%has_input(k)) then
                ii = ii + 1
                if (config%input_signal_hold(k)) then
                  if (.not. config%is_triggered(k)) then
                    config%input_signal_value(k) = input_to_dispatch(config%input_source(ii))
                  ! else input signal value does not update on this call
                  end if
                else
                  config%input_signal_value(k) = input_to_dispatch(config%input_source(ii))
                end if
              else
                config%input_signal_value(k) = 0.0_wp
              end if
              config%is_triggered(k) = .true.
            else
              if (config%has_input(k)) then
                ii = ii + 1
                config%input_signal_value(k) = input_to_dispatch(config%input_source(ii))
              else
                config%input_signal_value(k) = 0.0_wp
              end if
              config%is_triggered(k) = .false.
            end if
          else
            if (config%has_input(k)) then
              ii = ii + 1
              if (config%input_signal_hold(k)) then
                if (.not. config%is_triggered(k)) then
                  config%input_signal_value(k) = input_to_dispatch(config%input_source(ii))
                ! else input signal value does not update on this call
                end if
              else
                config%input_signal_value(k) = input_to_dispatch(config%input_source(ii))
              end if
            else
              config%input_signal_value(k) = 0.0_wp
            end if
            config%is_triggered(k) = .true.
          end if
          if (config%has_abscissa(k)) then
            ! set ii so that input_to_dispatch(config%input_source(ii)) is the requested abscissa signal for this generator
            ii = ii + 1
          end if
          ! generate the requested waveform
          if (config%is_triggered(k)) then
            select type(s => config%waveforms(k)%signal)
            type is (constant_t)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset
            type is (ramp_t)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + s%increment * s%sample_count
              ! protect against integer overflow
              if (s%sample_count .gt. huge(0) - 10) then
                s%offset = input_to_dispatch(k)
                s%sample_count = 0
              end if
              s%sample_count = s%sample_count + 1
            type is (sinusoid_t)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + s%amplitude * sin(s%frequency * input_to_dispatch(config%input_source(ii)) + s%phase * s%period)
            type is (square_wave_t)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + sign(s%amplitude, 0.5_wp - s%frequency * mod(input_to_dispatch(config%input_source(ii)) + s%phase * s%period, s%period))
            type is (rectangular_wave_t)
              s%position_in_period = mod(input_to_dispatch(config%input_source(ii)) + s%phase * s%period, s%period)
              if (s%position_in_period .gt. s%pulse_width) then
                s%sign = -1.0_wp
              else
                s%sign = 1.0_wp
              end if
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + sign(s%amplitude, s%sign)
            type is (triangle_wave_t)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + s%amplitude * 2.0_wp * (2.0_wp * abs((input_to_dispatch(config%input_source(ii)) + s%phase * s%period) * s%frequency - floor((input_to_dispatch(config%input_source(ii)) + s%phase * s%period) * s%frequency + 0.5_wp)) - 0.5_wp)
            type is (gaussian_noise_t)
              error_code = vdrnggaussian(VSL_RNG_METHOD_GAUSSIAN_ICDF, s%stream, n_realization, s%r, s%offset, s%sigma)
              if (error_code .ne. 0) then
                error = fail("Cannot sample gaussian noise generator (signal[" // itoa(k) //"]); error code: " // itoa(error_code))
                exit try
              end if
              input_to_dispatch(k) = config%input_signal_value(k) + s%r(1)
            type is (uniform_noise_t)
              error_code = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, s%stream, n_realization, s%r, s%a, s%b)
              if (error_code .ne. 0) then
                error = fail("Cannot sample uniform noise generator (signal[" // itoa(k) //"]); error code: " // itoa(error_code))
                exit try
              end if
              input_to_dispatch(k) = config%input_signal_value(k) + s%r(1)
            type is (prbs_t)
              random_bit = 0
              do kk = 1, 5
                random_bit = ieor(random_bit, iand(ishft(s%register, -s%taps(kk)), 1))
              end do
              s%register = ishft(s%register, 1)
              s%register = ieor(s%register, random_bit)
              s%r = iand(s%register, 1)
              input_to_dispatch(k) = config%input_signal_value(k) + s%offset + (real(s%r, kind=8) - 0.5_wp) * s%amplitude
            end select
          else
            input_to_dispatch(k) = config%input_signal_value(k)
          end if
        end do
        ! update dispatch
        call dispatch_sample(input_to_dispatch, output_from_dispatch, error)
        if (allocated(error)) then
          call wrap_error(error, "Failure in call to dispatch_initialize().")
          exit try
        end if
      end do
      call cpu_time(toc)
      execution_time(i) = toc - tic
      call dispatch_terminate(error)
      if (allocated(error)) then
        call wrap_error(error, "Failure in call to dispatch_initialize().")
        exit try
      end if
    end do
    print *,execution_time
    print *,sum(execution_time) / config%n_test
    call qsort(execution_time, int(config%n_test, kind=8), int(sizeof(0.0_wp), kind=8), compare_reals)
    if (config%n_test .gt. 1) then
      if (mod(config%n_test, 2) .eq. 1) then
        median_execution_time = execution_time((config%n_test + 1) / 2)
      else
        median_execution_time = (execution_time(config%n_test/2) + execution_time(config%n_test/2 + 1)) / 2
      end if
    else
      median_execution_time = execution_time(1)
    end if
    print *,median_execution_time
    print *, median_execution_time / config%n_sample_per_test / config%sample_period
    stop ! normal (error-free) program stop
  end block try
  ! if we are here, then there has been an error: report it and stop
  error_string = error%to_chars()
  error stop error_string
  
contains
  ! A function for comparing real values, for use with qsort
  function compare_reals(a, b)
      real(wp), intent(in) :: a, b
      integer(2) :: compare_reals
      if (a .gt. b) then
        compare_reals = -1
      elseif (a .lt. b) then
        compare_reals = 1
      else
        compare_reals = 0
      end if
  end function compare_reals

end program
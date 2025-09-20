!  iir_filter.f90 
!
!  Implements a discrete-time IIR filter as cascaded second-order sections.
!  Conversion to discrete-time form is via Tustin's method with a single prewarp frequency (if desired)
!
!  FUNCTIONS/SUBROUTINES provided by iir_filter:
!  iir_filter_t              - function: iir_filter_t object constructor 
!  iir_filter_t%initialize   - subroutine: initialize IIR filter buffers to reduce start-up transients
!  iir_filter_t%sample       - subroutine: sample IIR filter
!
!  A typical lifetime for an iir_filter_t object would be as follows:
!  use dispatch_types, only: wp
!  type(iir_filter_t) :: my_iir_filter
!  real(wp) :: my_input_signal, my_output_signal
!  my_iir_filter = iir_filter_t(tf_numerator, tf_denominator, sample_frequency, prewarp_frequency)
!  call my_iir_filter%initialize(my_input_signal)
!  ! loop over time
!  call my_iir_filter%sample(my_input_signal, my_output_signal)
!  ! my_output_signal is the filtered version of my_input_signal
!
module iir_filter

  use dispatch_types, only: wp
  use transfer_function_class, only: transfer_function_t
  use zpk_class, only: zpk_t
  use sos_class, only: sos_t
  use error_handling, only: error_t, fail, wrap_error
  
  implicit none
  
  type :: iir_filter_t
    type(transfer_function_t) :: tf
    type(zpk_t) :: zpk
    type(sos_t), allocatable :: sos(:)
    real(8) :: sample_period, f_pw
    real(8) :: output
    class(error_t), allocatable :: error
  contains
    procedure :: sample
  end type
  
  interface iir_filter_t
    procedure :: constructor
  end interface
  
  contains
  function constructor(tf_num, tf_den, fs, fpw) result(this)
  ! builds a discrete-time IIR filter from continuous-time transfer function coefficients
  ! inputs:
  ! tf_num: continuous-time transfer function numerator coefficients in decreasing orders of s, i.e. tf_num(1)*s^2 + tf_num(2) * s + tf_num(3)
  ! tf_den: continuous-time transfer function denominator coefficients in decreasing orders of s, i.e. tf_den(1)*s^2 + df_den(2) * s + tf_den(3)
  ! fs: sample frequency [Hz]
  ! fpw: prewarp frequency [Hz], optional; if no prewarp frequency is specified, discrete-time implementation frequency response will match continous at f=0.
  !
  ! output: discrete-time IIR filter, implemented as cascade of second-order sections
  type(iir_filter_t) :: this
  real(wp) :: tf_num(:)
  real(wp) :: tf_den(:)
  real(wp) :: fs
  real(wp), optional :: fpw
  integer :: i
  
  try: block
    if (size(tf_num) .gt. size(tf_den)) then
      this%error = fail("Improper transfer function. Must have at least as many poles as zeros (denominator order must be >= numerator order).")
      exit try
    end if
    ! create continuous-time transfer function
    this%tf = transfer_function_t(tf_num, tf_den)
    if (allocated(this%tf%error)) then
      this%error = this%tf%error
      call wrap_error(this%error, "Error in transfer function construction")
      exit try
    end if
    ! convert to continuous-time zero-pole-gain
    call this%tf%tozpk(this%zpk, this%error)
    if (allocated(this%error)) then
      call wrap_error(this%error, "Error in conversion of continuous-time transfer function to zero-pole-gain")
      exit try
    end if
    ! convert to discrete-time zero-pole-gain
    if (present(fpw)) then
      call this%zpk%bilinear(fs, fpw, this%error)
    else
      call this%zpk%bilinear(fs, 0.0d0, this%error)
    end if
    if (allocated(this%error)) then
      call wrap_error(this%error, "Error in conversion of continuous-time zero-pole-gain system to discrete time")
      exit try
    end if
    ! convert discrete-time zero-pole-gain to cascade of second order sections
    call this%zpk%tosos(this%sos, this%error)
    if (allocated(this%error)) then
      call wrap_error(this%error, "Error in conversion of discrete-time zero-pole-gain system to second order sections")
      exit try
    end if
  end block try
  end function
  
  subroutine sample(this, x, y)
  ! samples the cascade of second-order-sections
  class(iir_filter_t), intent(inout) :: this
  real(wp),            intent(in   ) :: x
  real(wp),            intent(  out) :: y
  
  integer  :: i
  real(wp) :: v, x_cur, x_new
  
  x_cur = x
  do i = 1,size(this%sos)
    !call this%sos(i)%sample(xi, yi)
    x_new = x_cur * this%sos(i)%num(1) + this%sos(i)%state(1)
    this%sos(i)%state(1) = x_cur * this%sos(i)%num(2) - x_new * this%sos(i)%den(2) + this%sos(i)%state(2)
    this%sos(i)%state(2) = x_cur * this%sos(i)%num(3) - x_new * this%sos(i)%den(3)
    x_cur = x_new
  end do
  y = x_cur
  this%output = x_cur
  
  end subroutine
  
end module
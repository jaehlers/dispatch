!  sos_class.f90 
!
!  Implements a discrete-time second-order section with Type II direct sampling.
!
!  FUNCTIONS/SUBROUTINES provided by sos_class:
!  sos_t              - function: sos_t object constructor (either as transfer function, or zero-pole-gain)
!  sos_t%initialize   - subroutine: initialize sos buffer to reduce start-up transients
!  sos_t%sample       - subroutine: sample second-order section (Type II direct implementation)
!
!  A typical lifetime for an sos_t object (in a cascade of second order sections) would be as follows:
!  use dispatch_types
!  type(sos_t), allocatable :: my_sos(:)
!  real(kind=dispatch_signal_kind) :: my_input_signal, my_output_signal, v
!  integer :: i
!  allocate(my_sos(2)) ! cascade can be arbitrarily long
!  my_sos(1) = sos_t((/1,2,3/), (/1,2,3/)) % construction by transfer function
!  my_sos(2) = sos_t((/complex(0.9d0, 0.0d0)/), (/complex(0.5d0, 0.5d0), complex(0.5d0, -0.5d0)/)) % construction by zpk
!  ! initialize when signal becomes available
!  do i = 1,size(my_sos)
!    call my_sos(i)%initialize(my_input_signal)
!  end do
!  ! sample the cascade to filter the signal
!  v = my_input_signal ! copy the input signal to the cascade-intermediate signal
!  do i = 1,size(my_sos)
!    call my_sos(i)%sample(v)
!  end do
!  my_output_signal = v ! after the cascade, the value of the cascade-intermediate signal is the filtered signal
!
module sos_class

use dispatch_types, only: wp
use error_handling, only: error_t, fail
use extf, only: is_real, is_complex_conjugate_pair

implicit none

type :: sos_t
  real(wp) :: num(3), den(3)
  real(wp) :: state(2) = 0.0_wp
  class(error_t), allocatable :: error
contains
  procedure :: initialize
  procedure :: apply_gain
  procedure :: sample
end type
  
interface sos_t
  procedure :: constructor_tf
  procedure :: constructor_zpk
end interface
  
contains
  function constructor_tf(num, den) result(this)
  ! assign numerator and denominator to a discrete-time second-order-section
  ! coefficients are in increasing order of z, i.e. num(1) + num(2) * z + num(3) * z^2
  real(wp)    :: num(3), den(3)
  type(sos_t) :: this
  this%num = num
  this%den = den
  end function
  
  function constructor_zpk(zeros, poles, gain) result(this)
  ! construct a second-order-section from given zeros and poles
  complex(wp) :: zeros(:), poles(:)
  real(wp), optional :: gain
  type(sos_t) :: this
  
  ! generate numerator
  select case (size(zeros))
  case(0)
    ! if no zero is specified, the numerator is 1
    this%num = (/1.0_wp, 0.0_wp, 0.0_wp/)
  case(1)
    ! if a single zero is specified, it must be real; numerator is (1 - zeros(1)/z)
    if (.not. is_real(zeros(1), epsilon(1.0_wp))) then
      this%error = fail("cannot construct a second-order-section with a single complex zero")
      return
    end if
    this%num = (/1.0_wp, -real(zeros(1)), 0.0_wp/)
    
    ! gain of 1 if not specified
    if (present(gain)) this%num = this%num * gain
  case(2)
    ! second-order numerator polynomial (1 - zeros(1)/z) * (1 - zeros(2)/z)
    if (is_real(zeros(1), epsilon(1.0_wp))) then
      if (is_real(zeros(2), epsilon(1.0_wp))) then
        this%num = (/ 1.0_wp, -(real(zeros(1)) + real(zeros(2))), real(zeros(1)) * real(zeros(2)) /)
      else
        this%error = fail("if one sos zero is real, both zeros must be real")
        return
      end if
    else
      if (is_complex_conjugate_pair(zeros(1), zeros(2), epsilon(1.0_wp))) then
        this%num = (/ 1.0_wp, -2.0_wp * real(zeros(1)), (real(zeros(1))**2.0_wp + imag(zeros(1))**2.0_wp) /)
      else
        this%error = fail("complex sos zeros must be complex conjugates")
        return
      end if
    end if
  end select
  
  ! generate denominator
  select case (size(poles))
  case(0)
    ! if no pole is specified, the denominator is 1
    this%den = (/1.0_wp, 0.0_wp, 0.0_wp/)
  case(1)
    ! if a single pole is specified, it must be real; denominator is (1 - poles(1)/z)
    if (.not. is_real(poles(1), epsilon(1.0_wp))) then
      this%error = fail("cannot construct a second-order-section with a single complex pole")
      return
    end if
    this%den = (/1.0_wp, -real(poles(1)), 0.0_wp/)
  case(2)
    ! second-order denominator polynomial (1 - poles(1)/z) * (1 - poles(2)/z)
    if (is_real(poles(1), epsilon(1.0_wp))) then
      if (is_real(poles(2), epsilon(1.0_wp))) then
        this%den = (/ 1.0_wp, -(real(poles(1)) + real(poles(2))), real(poles(1)) * real(poles(2)) /)
      else
        this%error = fail("if one sos pole is real, both poles must be real")
        return
      end if
    else
      if (is_complex_conjugate_pair(poles(1), poles(2), epsilon(1.0_wp))) then
        this%den = (/ 1.0_wp, -2.0_wp * real(poles(1)), (real(poles(1))**2.0d0 + imag(poles(1))**2.0_wp) /)
      else
        this%error = fail("complex sos poles must be complex conjugates")
        return
      end if
    end if
  end select
  end function
  
  subroutine initialize(this, x)
  ! initialize filter buffer to reduce start-up transients
  class(sos_t), intent(inout) :: this
  real(wp),     intent(in   ) :: x
  !this%state = (/x, x/)
  this%state = (/0.0_wp, 0.0_wp/)
  end subroutine
  
  subroutine apply_gain(this, gain)
  ! apply a gain to the sos
  class(sos_t), intent(inout) :: this
  real(wp),     intent(in   ) :: gain
  this%num = this%num * gain
  end subroutine
  
  subroutine sample(this, x, y)
  ! type II direct implementation of a second order section
  ! x is the scalar input signal, y is the scalar output signal
  ! v is an intermediate value used as input, output, and intermediate variable;
  ! this allows the sos to be sampled conveniently in a cascade
  class(sos_t), intent(inout) :: this
  real(wp),     intent(in   ) :: x
  real(wp),     intent(  out) :: y
  real(wp) :: x_new, x_cur
  !v = (v * this%den(1)) - (this%state(1) * this%den(2)) - (this%state(2) * this%den(3))
  !y = (v * this%num(1)) + (this%state(1) * this%num(2)) + (this%state(2) * this%num(3))
  !this%state(2) = this%state(1)
  !this%state(1) = v
  !v = y
  x_cur = x
  x_new = x_cur * this%num(1) + this%state(1)
  this%state(1) = x_cur * this%num(2) - x_new * this%den(2) + this%state(2)
  this%state(2) = x_cur * this%num(3) - x_new * this%den(3)
  y = x_new
  end subroutine
end module
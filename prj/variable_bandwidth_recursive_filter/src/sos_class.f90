!  variable_bandwidth_sos_class.f90 
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
module variable_bandwidth_sos_class

use dispatch_types, only: wp
use extf, only: itoa, real_array_container_t
use error_handling, only: error_t, fail, wrap_error
use polynomial_class, only: polynomial_t

implicit none

type :: variable_bandwidth_sos_t
  integer  :: n_section = 0
  real(wp) :: lambda = 0.99999_wp
  real(wp) :: gain = 0.0_wp
  logical  :: first_sample = .true.
  real(wp), allocatable :: psi_range(:)
  real(wp), allocatable :: state(:,:)
  real(wp), allocatable :: sos(:,:)
  type(polynomial_t) :: g
  type(polynomial_t), allocatable :: b1(:)
  type(polynomial_t), allocatable :: b2(:)
  type(polynomial_t), allocatable :: x2(:)
  type(polynomial_t), allocatable :: x1(:)
  class(error_t), allocatable :: error
contains
  procedure :: initialize
  procedure :: update_parameters
  procedure :: sample
end type
  
interface variable_bandwidth_sos_t
  procedure :: constructor
end interface
  
contains
  function constructor(n_section, lambda, psi_range, g, b1, b2, x2, x1) result(this)
  ! assign numerator and denominator to a discrete-time second-order-section
  ! coefficients are in increasing order of z, i.e. num(1) + num(2) * z + num(3) * z^2
  integer  :: n_section
  real(wp) :: lambda
  real(wp) :: psi_range(2)
  type(real_array_container_t) :: g, b1(n_section), b2(n_section), x2(n_section), x1(n_section)
  type(variable_bandwidth_sos_t) :: this
  integer :: i
  
  this%n_section = n_section
  this%lambda = lambda
  this%psi_range = psi_range
  allocate(this%sos(this%n_section, 6), source=0.0_wp)
  allocate(this%state(this%n_section, 2), source=0.0_wp)
  this%g = polynomial_t(size(g%a), g%a)
  if (allocated(this%g%error)) then
    this%error = this%g%error
    call wrap_error(this%error, "Error in construction of g polynomial")
    return
  end if
  allocate(this%b1(this%n_section))
  allocate(this%b2(this%n_section))
  allocate(this%x2(this%n_section))
  allocate(this%x1(this%n_section))
  do i = 1,this%n_section
    this%b1(i) = polynomial_t(size(b1(i)%a), b1(i)%a)
    if (allocated(this%b1(i)%error)) then
      this%error = this%b1(i)%error
      call wrap_error(this%error, "Error in construction of b1 polynomial, section " // itoa(i))
      return
    end if
    this%b2(i) = polynomial_t(size(b2(i)%a), b2(i)%a)
    if (allocated(this%b2(i)%error)) then
      this%error = this%b2(i)%error
      call wrap_error(this%error, "Error in construction of b2 polynomial, section " // itoa(i))
      return
    end if
    this%x2(i) = polynomial_t(size(x2(i)%a), x2(i)%a)
    if (allocated(this%x2(i)%error)) then
      this%error = this%x2(i)%error
      call wrap_error(this%error, "Error in construction of x2 polynomial, section " // itoa(i))
      return
    end if
    this%x1(i) = polynomial_t(size(x1(i)%a), x1(i)%a)
    if (allocated(this%x1(i)%error)) then
      this%error = this%x1(i)%error
      call wrap_error(this%error, "Error in construction of x1 polynomial, section " // itoa(i))
      return
    end if
  end do
  end function
  
  subroutine initialize(this, x, psi)
  ! initialize filter buffer to reduce start-up transients
  class(variable_bandwidth_sos_t), intent(inout) :: this
  real(wp),     intent(in   ) :: x
  real(wp),     intent(in   ) :: psi
  real(wp) :: y, y_prev
  real(wp) :: relative_error, nominal_error
  
  call this%update_parameters(psi)
  relative_error = huge(0.0_wp)
  nominal_error = max(tiny(0.0_wp) * 1.0e5_wp, x * 1.0e-5_wp)
  y_prev = x
  do while (relative_error .gt. 1.0_wp)
    call this%sample(x, y)
    relative_error = abs(y - y_prev) / nominal_error
    y_prev = y
  end do
  end subroutine
  
  subroutine update_parameters(this, psi)
  ! update the psi-varying parameters (gain and sos table)
  class(variable_bandwidth_sos_t), intent(inout) :: this
  real(wp),     intent(in   ) :: psi
  integer  :: i
  real(wp) :: psi_sat
  psi_sat = max(this%psi_range(1), min(this%psi_range(2), psi))
  this%gain = this%g%evaluate(psi_sat)
  do i = 1,this%n_section
    this%sos(i,2) = this%b1(i)%evaluate(psi_sat)
    this%sos(i,3) = this%b2(i)%evaluate(psi_sat)
    this%sos(i,6) = this%lambda * sin(this%x2(i)%evaluate(psi_sat))
    this%sos(i,5) = this%lambda * sin(this%x1(i)%evaluate(psi_sat)) * (1.0_wp + this%sos(i,6))
  end do
  end subroutine
  
  subroutine sample(this, x, y)
  ! type II direct implementation of a second order section
  ! x is the scalar input signal, y is the scalar output signal
  class(variable_bandwidth_sos_t), intent(inout) :: this
  real(wp),                        intent(in   ) :: x
  real(wp),                        intent(  out) :: y
  real(wp) :: x_new, x_cur
  integer  :: i

  x_cur = x
  do i = 1,this%n_section
    x_new = x_cur * this%sos(i,1) + this%state(i,1)
    this%state(i,1) = x_cur * this%sos(i,2) - x_new * this%sos(i,5) + this%state(i,2)
    this%state(i,2) = x_cur * this%sos(i,3) - x_new * this%sos(i,6)
    x_cur = x_new
  end do
  y = x_cur * this%gain
  this%first_sample = .false.
  end subroutine
end module
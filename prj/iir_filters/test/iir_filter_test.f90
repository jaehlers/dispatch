!  iir_filter_test.f90 
!
!  FUNCTIONS:
!  iir_filter_test - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: iir_filter_test
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program iir_filter_test
    
    use dispatch_types, only: wp
    use iir_filter

    implicit none

    ! Variables
    type(iir_filter_t) :: f
    type(transfer_function_t) :: tf
    type(zpk_t) :: zpk
    type(sos_t), allocatable :: sos(:)
    integer :: i
    integer, parameter :: n_sample = 1000
    real(wp) :: t(n_sample), x(n_sample), y(n_sample)
    class(error_t), allocatable :: error
    real(wp), parameter :: pi  = 4.0_wp * atan(1.0_wp)

    ! Body of iir_filter_test
    print *, 'Hello World'
    ! full test code:
    ! make filters that cover all the sos cases
    ! - uneven # of poles (>2), one of them real, all other comp conjs; at least one real zero (this covers special cases 1 & 2)
    ! - 2 real poles, 2 zeros (complex or real - doesn't matter)
    ! - 4 complex poles, 2 complex zeros
    ! convert them from sos to tfs
    ! use the fortran code to convert them back to sos
    ! - check with breakpoints to make sure this hits all sos_t initialization cases in zpk%tosos()
    !
    ! do timing comparison with python on sample() loop
    !
    ! package for fpm
    
    ! equivalent python code
    ! from scipy import signal
    ! from scipy import constants
    ! sys_zpk = signal.tf2zpk([3, 0, 0], [1, 5, 13])
    ! sys_zpk_d = signal.bilinear_zpk(sys_zpk[0], sys_zpk[1], sys_zpk[2], 10.0)
    ! sys_sos_d = signal.zpk2sos(sys_zpk_d[0], sys_zpk_d[1], sys_zpk_d[2])
    !tf = transfer_function_t([3.0_wp, 0.0_wp, 0.0_wp], [1.0_wp, 5.0_wp, 13.0_wp])
    !call tf.tozpk(zpk, error) ! this gets the correct answer, below
    !if (allocated(error)) print *,error%to_chars()
    !! (array([0., 0.]), array([-2.5+2.59807621j, -2.5-2.59807621j]), np.float64(3.0))
    !call zpk.bilinear(10.0_wp, 0.0_wp, error) ! this gets the correct answer, below
    !if (allocated(error)) print *,error%to_chars()
    !! (array([1., 1.]), array([0.75438596+0.20257904j, 0.75438596-0.20257904j]), np.float64(2.3391812865497075))
    !call zpk.tosos(sos, error)
    !if (allocated(error)) print *,error%to_chars() ! this gets the correct answer, below
    !! array([[ 2.33918129, -4.67836257,  2.33918129,  1.        , -1.50877193, 0.61013645]])
    
    ! implement a chebyshev type 2 band pass filter centred at 2Hz, and sample it with a superposition of sine waves
    !f = iir_filter_t([18.04028_wp], [1.0_wp, 1.591179_wp, 129.175998_wp], 25.0_wp, 2.0_wp)
    ! equivalent python code
! on my computer, scipy is in a conda env
! conda activate scipy_env
! python
! ---- begin python -----
!from scipy import signal
!from scipy import constants
!import math
!sys_zpk = signal.tf2zpk([3.769911, 0.000000, 1235.290063, 0.000000, 89826.423225, 0.000000], 
!            [1.000000, 7.938708, 4.874873e+02, 2.522286e+03, 7.524883e+04, 1.891572e+05, 3.677981e+06])
!sys_zpk_d = signal.bilinear_zpk(sys_zpk[0], sys_zpk[1], sys_zpk[2], 25.0)
!sys_sos_d = signal.zpk2sos(sys_zpk_d[0], sys_zpk_d[1], sys_zpk_d[2])
!period = 0.04
!n_step = 1000
!t = [i * period for i in range(n_step)]
!x = [2.0 * math.sin(2.0 * constants.pi * t[i] * 0.3) + 0.5 * math.sin(2.0 * constants.pi * t[i] * 2.0) + 0.125 * math.sin(2.0 * constants.pi * t[i] * 4.0) for i in range(n_step)]
!y = signal.sosfilt(sys_sos_d, x)
!with open('c:/users/jaehl/Documents/Personal/Code/sandbox/sos_py.txt', 'w') as f:
!  for i in range(n_step):
!    f.write(" ".join(["%.6f" % j for j in [t[i], x[i], y[i]]]) + "\n");
! ---- end python -----
    tf = transfer_function_t([3.769911_wp, 0.000000_wp, 1235.290063_wp, 0.000000_wp, 89826.423225_wp, 0.000000_wp], &
      [1.000000_wp, 7.938708_wp, 4.874873e+02_wp, 2.522286e+03_wp, 7.524883e+04_wp, 1.891572e+05_wp, 3.677981e+06_wp])
    call tf.tozpk(zpk, error) ! this gets the correct answer, below
    if (allocated(error)) print *,error%to_chars()
    call zpk.bilinear(25.0_wp, 0.0_wp, error) ! this gets the correct answer, below
    if (allocated(error)) print *,error%to_chars()
    call zpk.tosos(sos, error)
    if (allocated(error)) print *,error%to_chars() ! this gets the correct answer, below

    f = iir_filter_t([3.769911_wp, 0.000000_wp, 1235.290063_wp, 0.000000_wp, 89826.423225_wp, 0.000000_wp], &
      [1.000000_wp, 7.938708_wp, 4.874873e+02_wp, 2.522286e+03_wp, 7.524883e+04_wp, 1.891572e+05_wp, 3.677981e+06_wp], 25.0_wp, 2.0_wp)
    do i = 1,size(f%sos)
      print *, f%sos(i)%num
      print *, f%sos(i)%den
    end do
    call f%initialize(0.0_wp)
    do i = 1,n_sample ! these results agree with the python code above: filter implementation appears to be correct
      t(i) = real(i-1, wp) / 25.0_wp
      x(i) = 2.0_wp * sin(2.0_wp * pi * t(i) * 0.3_wp) + 0.5_wp * sin(2.0_wp * pi * t(i) * 2.0_wp) + 0.125_wp * sin(2.0_wp * pi * t(i) * 4.0_wp)
      call f%sample(x(i), y(i))
    end do
    open (unit=10,file="out.txt",action="write")
    do i=1,n_sample
      write (10,*)  (/t(i), x(i), y(i)/)
    end do
    close (10)
    
    print *, 'Goodbye World'

    end program iir_filter_test


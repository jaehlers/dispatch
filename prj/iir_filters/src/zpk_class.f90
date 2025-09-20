!  zpk_class.f90 
!
!  Implements the zero-pole-gain (zpk) form of an IIR filter (discrete- and continuous-time).
!
!  FUNCTIONS/SUBROUTINES provided by zpk_class:
!  zpk_t%bilinear - subroutine: calculates discrete-time form of continous-time zpk
!  zpk_t%tosos    - subroutine: generate cascade of second order sections (sos_t) from discrete-time zpk
!
!  A typical lifetime for a zpk_t object would be as follows:
!  use dispatch_types
!  use sos_class, only: sos_t
!  type(zpk_t) :: my_zpk
!  type(sos_t), allocatable :: my_sos(:)
!  ! zpk_t has no constructor - just enter z, p, and k separately
!  my_zpk%z = (/ some real values and/or complex conjugate pairs in the left half plane /)
!  my_zpk%p = (/ some other real values or complex conjugate pairs in the left half plane /)
!  my_zpk%k = a real value
!  my_zpk%bilinear(fs, fpw, error) ! fs and fpw are in Hz, error is handed to an error handler
!  my_zpk%tosos(my_sos, error)
!  ! my_sos can now be sampled
!
module zpk_class

  use dispatch_types, only: wp
  use extf, only: is_real, is_complex_conjugate_pair
  use error_handling, only: error_t, fail, wrap_error
  use sos_class, only: sos_t
  
  implicit none
  
  type :: zpk_t
    complex(wp), allocatable :: z(:), p(:), z_z(:), p_z(:)
    real(wp) :: k, k_z
  contains
    procedure :: bilinear
    procedure :: tosos
  end type

  contains
  
  subroutine bilinear(this, fs, fpw, error)
  !Return a digital IIR filter from an analog one using a bilinear transform.
  !
  !This code is based on scipy's implementation of bilinear()
  !
  !Transform a set of poles and zeros from the analog s-plane to the digital
  !z-plane using Tustin's method, which substitutes ``2*fs*(z-1) / (z+1)`` for
  !``s``, maintaining the shape of the frequency response.
  !
  !Parameters
  !----------
  !z : array_like
  !    Zeros of the analog filter transfer function.
  !p : array_like
  !    Poles of the analog filter transfer function.
  !k : float
  !    System gain of the analog filter transfer function.
  !fs : float
  !    Sample rate, as ordinary frequency (e.g., hertz).
  !fpw: float
  !    Prewarp frequency, as ordinary frequency (e.g., hertz).
  !
  !Returns
  !-------
  !z : ndarray
  !    Zeros of the transformed digital filter transfer function.
  !p : ndarray
  !    Poles of the transformed digital filter transfer function.
  !k : float
  !    System gain of the transformed digital filter.
  !
  !See Also
  !--------
  !lp2lp_zpk, lp2hp_zpk, lp2bp_zpk, lp2bs_zpk
  !bilinear
  !
  !Notes
  !-----
  !.. versionadded:: 1.1.0
  !
  !Examples
  !--------
  !>>> import numpy as np
  !>>> from scipy import signal
  !>>> import matplotlib.pyplot as plt
  !
  !>>> fs = 100
  !>>> bf = 2 * np.pi * np.array([7, 13])
  !>>> filts = signal.lti(*signal.butter(4, bf, btype='bandpass', analog=True,
  !...                                   output='zpk'))
  !>>> filtz = signal.lti(*signal.bilinear_zpk(filts.zeros, filts.poles,
  !...                                         filts.gain, fs))
  !>>> wz, hz = signal.freqz_zpk(filtz.zeros, filtz.poles, filtz.gain)
  !>>> ws, hs = signal.freqs_zpk(filts.zeros, filts.poles, filts.gain,
  !...                           worN=fs*wz)
  !>>> plt.semilogx(wz*fs/(2*np.pi), 20*np.log10(np.abs(hz).clip(1e-15)),
  !...              label=r'$|H_z(e^{j \omega})|$')
  !>>> plt.semilogx(wz*fs/(2*np.pi), 20*np.log10(np.abs(hs).clip(1e-15)),
  !...              label=r'$|H(j \omega)|$')
  !>>> plt.legend()
  !>>> plt.xlabel('Frequency [Hz]')
  !>>> plt.ylabel('Magnitude [dB]')
  !>>> plt.grid(True)
  class(zpk_t),                intent(inout) :: this
  real(wp),                    intent(in   ) :: fs
  real(wp),                    intent(in   ) :: fpw
  class(error_t), allocatable, intent(  out) :: error
  ! local variables
  integer  :: degree, i
  real(wp) :: fs2
  complex(wp), allocatable :: z_z_aug(:)

  try: block
    degree = size(this%p) - size(this%z)
    if (degree .lt. 0) then
      error = fail("Improper transfer function: must have at least as many poles as zeros.")
      exit try
    end if
    
    if (fpw .gt. epsilon(fpw)) then
      ! prewarping
      fs2 = fpw / tan(fpw / 2.0_wp / fs)
    else
      ! no prewarping
      fs2 = 2.0d0 * fs
    end if
    
    allocate(this%z_z(size(this%z)))
    allocate(this%p_z(size(this%p)))

    ! bilinear transform the poles and zeros
    this%z_z = (fs2 + this%z) / (fs2 - this%z)
    this%p_z = (fs2 + this%p) / (fs2 - this%p)

    ! Any zeros that were at infinity get moved to the Nyquist frequency
    if (degree .gt. 0) then
      allocate(z_z_aug(size(this%p_z)))
      do i = 1,size(this%z_z)
        z_z_aug(i) = this%z_z(i)
      end do
      do i = 1,degree
        z_z_aug(i + size(this%z_z)) = -1.0_wp
      end do
      call move_alloc(z_z_aug, this%z_z)
    end if

    ! Compensate for gain change
    this%k_z = this%k * real(product(fs2 - this%z) / product(fs2 - this%p))

    return
  end block try
  ! if we are here, there has been an error
  end subroutine

  subroutine tosos(this, sos, error)
  !Return second-order sections from zeros, poles, and gain of a system
  !
  !This code is based on scipy's implementation of zpk2sos();
  !it implements only the "nearest" pairing method, and is only for discrete-time systems
  !
  !Parameters
  !----------
  !z : array_like
  !    Zeros of the transfer function.
  !p : array_like
  !    Poles of the transfer function.
  !k : float
  !    System gain.
  !
  !    .. versionadded:: 1.8.0
  !
  !Returns
  !-------
  !sos : ndarray
  !    Array of second-order filter coefficients, with shape
  !    ``(n_sections, 6)``. See `sosfilt` for the SOS filter format
  !    specification.
  !
  !See Also
  !--------
  !sosfilt
  !
  !Notes
  !-----
  !The algorithm used to convert ZPK to SOS format is designed to
  !minimize errors due to numerical precision issues. The pairing
  !algorithm attempts to minimize the peak gain of each biquadratic
  !section. This is done by pairing poles with the nearest zeros, starting
  !with the poles closest to the unit circle for discrete-time systems, and
  !poles closest to the imaginary axis for continuous-time systems.
  !
  !``pairing='minimal'`` outputs may not be suitable for `sosfilt`,
  !and ``analog=True`` outputs will never be suitable for `sosfilt`.
  !
  !*Algorithms*
  !
  !The steps in the ``pairing='nearest'``, ``pairing='keep_odd'``,
  !and ``pairing='minimal'`` algorithms are mostly shared. The
  !``'nearest'`` algorithm attempts to minimize the peak gain, while
  !``'keep_odd'`` minimizes peak gain under the constraint that
  !odd-order systems should retain one section as first order.
  !``'minimal'`` is similar to ``'keep_odd'``, but no additional
  !poles or zeros are introduced
  !
  !The algorithm steps are as follows:
  !
  !As a pre-processing step for ``pairing='nearest'``,
  !``pairing='keep_odd'``, add poles or zeros to the origin as
  !necessary to obtain the same number of poles and zeros for
  !pairing.  If ``pairing == 'nearest'`` and there are an odd number
  !of poles, add an additional pole and a zero at the origin.
  !
  !The following steps are then iterated over until no more poles or
  !zeros remain:
  !
  !1. Take the (next remaining) pole (complex or real) closest to the
  !   unit circle (or imaginary axis, for ``analog=True``) to
  !   begin a new filter section.
  !
  !2. If the pole is real and there are no other remaining real poles [#]_,
  !   add the closest real zero to the section and leave it as a first
  !   order section. Note that after this step we are guaranteed to be
  !   left with an even number of real poles, complex poles, real zeros,
  !   and complex zeros for subsequent pairing iterations.
  !
  !3. Else:
  !
  !    1. If the pole is complex and the zero is the only remaining real
  !       zero*, then pair the pole with the *next* closest zero
  !       (guaranteed to be complex). This is necessary to ensure that
  !       there will be a real zero remaining to eventually create a
  !       first-order section (thus keeping the odd order).
  !
  !    2. Else pair the pole with the closest remaining zero (complex or
  !       real).
  !
  !    3. Proceed to complete the second-order section by adding another
  !       pole and zero to the current pole and zero in the section:
  !
  !        1. If the current pole and zero are both complex, add their
  !           conjugates.
  !
  !        2. Else if the pole is complex and the zero is real, add the
  !           conjugate pole and the next closest real zero.
  !
  !        3. Else if the pole is real and the zero is complex, add the
  !           conjugate zero and the real pole closest to those zeros.
  !
  !        4. Else (we must have a real pole and real zero) add the next
  !           real pole closest to the unit circle, and then add the real
  !           zero closest to that pole.
  !
  !.. [#] This conditional can only be met for specific odd-order inputs
  !       with the ``pairing = 'keep_odd'`` or ``'minimal'`` methods.
  !
  !.. versionadded:: 0.16.0
  !
  !Examples
  !--------
  !
  !Design a 6th order low-pass elliptic digital filter for a system with a
  !sampling rate of 8000 Hz that has a pass-band corner frequency of
  !1000 Hz. The ripple in the pass-band should not exceed 0.087 dB, and
  !the attenuation in the stop-band should be at least 90 dB.
  !
  !In the following call to `ellip`, we could use ``output='sos'``,
  !but for this example, we'll use ``output='zpk'``, and then convert
  !to SOS format with `zpk2sos`:
  !
  !>>> from scipy import signal
  !>>> import numpy as np
  !>>> z, p, k = signal.ellip(6, 0.087, 90, 1000/(0.5*8000), output='zpk')
  !
  !Now convert to SOS format.
  !
  !>>> sos = signal.zpk2sos(z, p, k)
  !
  !The coefficients of the numerators of the sections:
  !
  !>>> sos[:, :3]
  !array([[0.0014152 , 0.00248677, 0.0014152 ],
  !       [1.        , 0.72976874, 1.        ],
  !       [1.        , 0.17607852, 1.        ]])
  !
  !The symmetry in the coefficients occurs because all the zeros are on the
  !unit circle.
  !
  !The coefficients of the denominators of the sections:
  !
  !>>> sos[:, 3:]
  !array([[ 1.        , -1.32544025,  0.46989976],
  !       [ 1.        , -1.26118294,  0.62625924],
  !       [ 1.        , -1.2570723 ,  0.8619958 ]])
  !
  !The next example shows the effect of the `pairing` option.  We have a
  !system with three poles and three zeros, so the SOS array will have
  !shape (2, 6). The means there is, in effect, an extra pole and an extra
  !zero at the origin in the SOS representation.
  !
  !>>> z1 = np.array([-1, -0.5-0.5j, -0.5+0.5j])
  !>>> p1 = np.array([0.75, 0.8+0.1j, 0.8-0.1j])
  !
  !With ``pairing='nearest'`` (the default), we obtain
  !
  !>>> signal.zpk2sos(z1, p1, 1)
  !array([[ 1.  ,  1.  ,  0.5 ,  1.  , -0.75,  0.  ],
  !       [ 1.  ,  1.  ,  0.  ,  1.  , -1.6 ,  0.65]])
  !
  !The first section has the zeros {-0.5-0.05j, -0.5+0.5j} and the poles
  !{0, 0.75}, and the second section has the zeros {-1, 0} and poles
  !{0.8+0.1j, 0.8-0.1j}. Note that the extra pole and zero at the origin
  !have been assigned to different sections.
  !
  !With ``pairing='keep_odd'``, we obtain:
  !
  !>>> signal.zpk2sos(z1, p1, 1, pairing='keep_odd')
  !array([[ 1.  ,  1.  ,  0.  ,  1.  , -0.75,  0.  ],
  !       [ 1.  ,  1.  ,  0.5 ,  1.  , -1.6 ,  0.65]])
  !
  !The extra pole and zero at the origin are in the same section.
  !The first section is, in effect, a first-order section.
  !
  !With ``pairing='minimal'``, the first-order section doesn't have
  !the extra pole and zero at the origin:
  !
  !>>> signal.zpk2sos(z1, p1, 1, pairing='minimal')
  !array([[ 0.  ,  1.  ,  1.  ,  0.  ,  1.  , -0.75],
  !       [ 1.  ,  1.  ,  0.5 ,  1.  , -1.6 ,  0.65]])
  !
  class(zpk_t),                intent(inout) :: this
  type(sos_t),    allocatable, intent(  out) :: sos(:)
  class(error_t), allocatable, intent(  out) :: error

  integer :: n_sections, system_order, idx_worst, idx_nearest, i, j
  logical, allocatable :: p_remaining(:), z_remaining(:), p_is_real(:), z_is_real(:)
  complex(wp), allocatable :: p(:), z(:)
  complex(wp) :: p1, p2, z1, z2
  logical :: conjugate_found
  character(len=45) :: message
  real(wp) :: min_distance
  
  try: block
    if (size(this%z_z) .eq. size(this%p_z) .eq. 0) then
      allocate(sos(1), source = sos_t((/this%k_z, 0.0_wp, 0.0_wp/), (/1.0_wp, 0.0_wp, 0.0_wp/)))
      if (allocated(sos(1)%error)) then
        error = sos(1)%error
        call wrap_error(error, "failure to construct second order section")
        exit try
      end if
      return
    end if
    
    ! ensure we have the same number of poles and zeros, and that the number is even, and make copies
    if (size(this%z_z) .ne. size(this%p_z)) then
      error = fail("tosos() requires an equal number of poles and zeros")
      exit try
    end if
    system_order = max(size(this%z_z), size(this%p_z))
    if (mod(system_order, 2) .eq. 1) then
      system_order = system_order + 1
    end if
    allocate(p(system_order), source = cmplx(0.0_wp, kind=wp))
    allocate(z(system_order), source = cmplx(0.0_wp, kind=wp))
    p(1:size(this%p_z)) = this%p_z
    z(1:size(this%z_z)) = this%z_z
    n_sections = system_order / 2

    ! ensure we have complex conjugate pairs
    allocate(p_remaining(system_order), source=.true.)
    allocate(z_remaining(system_order), source=.true.)
    allocate(p_is_real(system_order), source=.true.)
    allocate(z_is_real(system_order), source=.true.)
    do i = 1,system_order
      if (.not. is_real(p(i), epsilon(1.0_wp)) .and. p_remaining(i)) then
        p_is_real(i) = .false.
        conjugate_found = .false.
        conjugate_p_search: do j = 1,system_order
          if (i .ne. j) then
            if (is_complex_conjugate_pair(p(i), p(j), epsilon(1.0_wp) * 100.0_wp)) then
              p_is_real(j) = .false.
              ! exclude the conjugate from the sos construction: any complex pole is automatically paired with its conjugate
              p_remaining(j) = .false.
              ! average the two poles because we don't know which one is more accurate
              p(i) = cmplx((real(p(i)) + real(p(j))) / 2.0_wp, (imag(p(i)) - imag(p(j))) / 2.0_wp, kind=wp)
              conjugate_found = .true.
              exit conjugate_p_search
            end if
          end if
        end do conjugate_p_search
        if (.not. conjugate_found) then
          write(message, fmt = '(F0.10,SP,F0.10,"i")') p(i)
          error = fail("failed to find complex conjugate pole for pole at: " // trim(message))
          exit try
        end if
      end if
      if (.not. is_real(z(i), epsilon(1.0_wp)) .and. z_remaining(i)) then
        z_is_real(i) = .false.
        conjugate_found = .false.
        conjugate_z_search: do j = 1,system_order
          if (i .ne. j) then
            if (is_complex_conjugate_pair(z(i), z(j), epsilon(1.0_wp) * 100.0_wp)) then
              z_is_real(j) = .false.
              ! exclude from sos construction
              z_remaining(j) = .false.
              ! average
              z(i) = cmplx((real(z(i)) + real(z(j))) / 2.0_wp, (imag(z(i)) - imag(z(j))) / 2.0_wp, kind=wp)
              conjugate_found = .true.
              exit conjugate_z_search
            end if
          end if
        end do conjugate_z_search
        if (.not. conjugate_found) then
          write(message, fmt = '(F0.10,SP,F0.10,"i")') z(i)
          error = fail("failed to find complex conjugate zero for zero at: " // trim(message))
          exit try
        end if
      end if
    end do
          
    ! Construct the sos cascade, reversing order so the "worst" (nearest unit circle) are last
    sos_cascade: block
      allocate(sos(n_sections))
      do i = n_sections,1,-1
        ! Select the "worst" remaining pole (closest to the unit circle)
        idx_worst = 0
        min_distance = huge(1.0_wp)
        do j = 1,system_order
          if (p_remaining(j) .and. (abs(abs(p(j)) - 1.0_wp) .lt. min_distance)) then
            min_distance = abs(abs(p(j)) - 1.0_wp)
            idx_worst = j
          end if
        end do
        p1 = p(idx_worst)
        p_remaining(idx_worst) = .false.
        ! pair that pole with a zero
        if (p_is_real(idx_worst) .and. .not. any(p_remaining)) then
          ! special case 1: p1 is the last remaining single real pole; pair it with nearest real zero
          idx_nearest = 0
          min_distance = huge(1.0_wp)
          do j = 1,system_order
            if (z_remaining(j) .and. z_is_real(j) .and. (abs(z(j) - p1) .lt. min_distance)) then
              min_distance = abs(z(j) - p1)
              idx_nearest = j
            end if
          end do
          z1 = z(idx_nearest)
          ! construct sos with zeros: (nearest real zero); poles: (p1)
          sos(i) = sos_t((/z1/), (/p1/))
          if (allocated(sos(i)%error)) then
            error = sos(i)%error
            exit sos_cascade
          end if
          z_remaining(idx_nearest) = .false.
        elseif (((count(p_remaining) + 1) .eq. count(z_remaining)) .and. &
                (.not. p_is_real(idx_worst))                       .and. &
                (count(p_is_real .and. p_remaining) .eq. 1)        .and. &
                (count(z_is_real .and. z_remaining) .eq. 1)               ) then
          ! special case 2: one real pole and one real zero left, equal number of complex poles and zeros remaining
          idx_nearest = 0
          min_distance = huge(1.0d0)
          ! we must pair with a complex zero
          do j = 1,system_order
            if (z_remaining(j) .and. (.not. z_is_real(j)) .and. (abs(z(j) - p1) .lt. min_distance)) then
              min_distance = abs(z(j) - p1)
              idx_nearest = j
            end if
          end do
          z1 = z(idx_nearest)
          ! construct sos with zeros: (nearest complex zero and its conjugate); poles: (p1 and conjg(p1))
          sos(i) = sos_t((/z1, conjg(z1)/), (/p1, conjg(p1)/))
          if (allocated(sos(i)%error)) then
            error = sos(i)%error
            exit sos_cascade
          end if
          z_remaining(idx_nearest) = .false.
        else
          if (p_is_real(idx_worst)) then ! p1 is real
            ! find the next worst real pole
            idx_worst = 0
            min_distance = huge(1.0_wp)
            do j = 1,system_order
              if (p_remaining(j) .and. p_is_real(j) .and. (abs(abs(p(j)) - 1.0_wp) .lt. min_distance)) then
                min_distance = abs(abs(p(j)) - 1.0_wp)
                idx_worst = j
              end if
            end do
            p2 = p(idx_worst)
            p_remaining(idx_worst) = .false.
          else ! p1 is complex: pair with its conjugate
            p2 = conjg(p1)
          end if
        
          ! find closest zero(s)
          if (count(z_remaining) .gt. 0) then
            idx_nearest = 0
            min_distance = huge(1.0_wp)
            do j = 1,system_order
              if (z_remaining(j) .and. (abs(z(j) - p1) .lt. min_distance)) then
                min_distance = abs(z(j) - p1)
                idx_nearest = j
              end if
            end do
            z1 = z(idx_nearest)
            z_remaining(idx_nearest) = .false.
            if (.not. z_is_real(idx_nearest)) then
              sos(i) = sos_t((/z1, conjg(z1)/), (/p1, p2/))
            else
              if (count(z_remaining) .gt. 0) then
                idx_nearest = 0
                min_distance = huge(1.0_wp)
                do j = 1,system_order
                  if (z_remaining(j) .and. (abs(z(j) - p1) .lt. min_distance)) then
                    min_distance = abs(z(j) - p1)
                    idx_nearest = j
                  end if
                end do
                z2 = z(idx_nearest)
                z_remaining(idx_nearest) = .false.
                sos(i) = sos_t((/z1, z2/), (/p1, p2/))
                if (allocated(sos(i)%error)) then
                  error = sos(i)%error
                  exit sos_cascade
                end if
              else
                sos(i) = sos_t((/z1/), (/p1, p2/))
                if (allocated(sos(i)%error)) then
                  error = sos(i)%error
                  exit sos_cascade
                end if
              end if
            end if
          else
            ! no more zeros
            sos(i) = sos_t((/complex(wp)::/), (/p1, p2/))
            if (allocated(sos(i)%error)) then
              error = sos(i)%error
              exit sos_cascade
            end if
          end if
        end if
      end do
    end block sos_cascade
    if (allocated(error)) then
      call wrap_error(error, "failure to construct second order section")
      exit try
    end if
    if (.not. ((count(p_remaining) .eq. 0) .and. (count(z_remaining) .eq. 0))) then
      error = fail("failed to account for all zeros and poles in construction of sos cascade")
      exit try
    end if
    ! put gain in first sos
    call sos(1)%apply_gain(this%k_z)
    return
  end block try
  ! if we are here, there has been an error
  end subroutine

end module
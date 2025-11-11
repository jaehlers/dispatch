module extf
  !! helper classes, types, functions, and subroutines for fortran projects
  use, intrinsic :: iso_fortran_env
  
  implicit none
  
#ifdef REAL32
  integer, parameter, public :: extf_rk = real32   !! real kind used by this module [4 bytes]
#elif REAL64
  integer, parameter, public :: extf_rk = real64   !! real kind used by this module [8 bytes]
#elif REAL128
  integer, parameter, public :: extf_rk = real128  !! real kind used by this module [16 bytes]
#else
  integer, parameter, public :: extf_rk = real64   !! real kind used by this module [8 bytes]
#endif

  integer,parameter,public :: wp = extf_rk         !! copy of `extf_rk` with a shorter name
  
  type :: string_container_t
  !! container to hold an array of variable-length strings (jagged array)
    character(len=:), allocatable :: s
  end type

  type :: real_array_container_t
  !! container to hold an array of variable-length real arrays (jagged array)
    real(wp), allocatable :: a(:)
  end type
  
  contains
  
  function is_complex_conjugate_pair(a, b, tol) result(res)
  !! returns true if a and b are complex conjugates, within specified tolerance; false otherwise
    complex(wp) :: a, b
    real(wp), optional :: tol
    logical :: res
    res = .false.
    if (.not. present(tol)) tol = epsilon(1.0_wp)
    if (abs(real(a) - real(b)) .lt. tol) then
      if (abs(imag(a) + imag(b)) .lt. tol) then
        res = .true.
        return
      end if
    end if
    return
  end function
  
  function is_real(a, tol) result(res)
  !! returns tue if a has zero imaginary part, within specified tolerance; false otherwise
    complex(wp) :: a
    real(wp), optional :: tol
    logical :: res
    res = .false.
    if (.not. present(tol)) tol = epsilon(1.0_wp)
    if (abs(imag(a)) .lt. tol) then
      res = .true.
      return
    end if
    return
  end function
  
  function itoa(i) result(res)
  !! integer to string
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(tmp)
  end function
  
  subroutine delimited_string_to_string_container_array(sc, input_string, delimiter)
  !! fill a string_container_t array from a single delimited string
  use, intrinsic :: iso_c_binding, only: c_null_char
  
  type(string_container_t), allocatable, intent(inout) :: sc(:)
  character(len=*),                      intent(in   ) :: input_string
  character(len=*),                      intent(in   ) :: delimiter
  integer :: n_substring, i, first, last, len_delim, len_string
  logical :: prev_char_was_delim
    
  ! determine the number of substrings (could do this with a count() call, but this code handles the case of no delimiter following last substring)
  len_delim = len(delimiter)
  len_string = len(input_string)
  n_substring = 0
  i = 1
  prev_char_was_delim = .true.
  do while (((i + len_delim - 1) .le. len(input_string)) .and. (input_string(i:i) .ne. c_null_char))
    if (input_string(i: i + len_delim - 1) .eq. delimiter) then
      if (.not. prev_char_was_delim) then
        prev_char_was_delim = .true.
      end if
      i = i + len_delim
    else
      if (prev_char_was_delim) then
        n_substring = n_substring + 1
      end if
      prev_char_was_delim = .false.
      i = i + 1
    end if
  end do
  ! allocate the string array
  allocate(sc(n_substring))
  ! populate string array
  n_substring = 0
  i = 1
  first = 1
  prev_char_was_delim = .true.
  do while (((i + len_delim - 1) .le. len(input_string)) .and. (input_string(i:i) .ne. c_null_char))
    if (input_string(i: i + len_delim - 1) .eq. delimiter) then
      if ((i .gt. 1) .and. (.not. prev_char_was_delim)) then
        last = i - 1
        sc(n_substring)%s = input_string(first:last)
        prev_char_was_delim = .true.
      end if
      i = i + len_delim
    else
      if (prev_char_was_delim) then
        n_substring = n_substring + 1
        first = i
      end if
      prev_char_was_delim = .false.
      i = i + 1
    end if
  end do
  ! handle the case where the string is not terminated with a delimiter
  if (first .ge. last) then
    len_string = len_trim(input_string)
    ! if the string is null-terminated
    if (input_string(len_string:len_string) .eq. c_null_char) then
      last = len_string - 1
    else
      last = len_string
    end if
    sc(n_substring)%s = input_string(first:last)
  end if
  end subroutine
    
  subroutine string_container_array_to_delimited_string(sc, delimiter, output_string)
  !! write a string_container_t array as a single delimited string
  use, intrinsic :: iso_c_binding, only: c_null_char
  
  type(string_container_t),      intent(in   ) :: sc(:)
  character(len=*),              intent(in   ) :: delimiter
  character(len=:), allocatable, intent(inout) :: output_string
  integer :: n_substring, i, first, last, len_delim, total_substring_chars
    
  ! determine number of characters to allocate, ignoring zero-length strings
  len_delim = len(delimiter)
  n_substring = 0
  total_substring_chars = 0
  do i = 1,size(sc)
    if (allocated(sc(i)%s)) then
      if (len(sc(i)%s) .gt. 0) then
        total_substring_chars = total_substring_chars + len(sc(i)%s)
        n_substring = n_substring + 1
      end if
    end if
  end do
  ! allocate enough space for string to terminate with a delimiter followed by a null
  allocate(output_string, source=repeat(c_null_char, total_substring_chars + n_substring * len_delim + 1))
  ! fill the delimited string
  first = 1
  do i = 1,size(sc)
    if (allocated(sc(i)%s)) then
      if (len(sc(i)%s) .gt. 0) then
        last = first + len(sc(i)%s) - 1
        output_string(first:last) = sc(i)%s
        output_string((last+1):(last+len_delim)) = delimiter
        first = last + len_delim + 1
      end if
    end if
  end do
  end subroutine
end module
!! library_class handles loading, calling, and unloading of shared libraries
module library_class

use, intrinsic         :: iso_c_binding
use dispatch_types, only: string_container_t
#ifdef OS_WIN
use ifwinty,        only: handle, lpvoid, bool, false
use kernel32,       only: LoadLibrary, GetProcAddress, FreeLibrary
#endif
use error_handling, only: error_t, fail
   
implicit none

type :: library_instance_t
private

character(len=:), allocatable         :: file_name                         !< The name of the DLL file including the full path to the current working directory.
type(string_container_t), allocatable :: proc_name(:)                      !< The name of the procedure(s) in the DLL that will be called.
#ifdef OS_WIN
integer(c_intptr_t)                   :: file_addr  = int(0,C_INTPTR_T)    !< The address of file FileName.         (RETURN value from LoadLibrary ) [Windows]
#else
type(c_ptr)                           :: file_addr = c_null_ptr
#endif
type(c_funptr), allocatable, public   :: proc_addr(:)                      !< The address of procedure(s) ProcName.    (RETURN value from GetProcAddress or dlsym) [initialized to Null for pack/unpack]
class(error_t), allocatable           :: error

contains
  procedure, public :: get_error
  procedure, public :: unload
  final :: destructor
end type

interface library_instance_t
  procedure :: constructor
end interface

#ifndef OS_WIN
! interface to linux API
interface
  function dlopen(filename,mode) bind(c,name="dlopen")
      ! void *dlopen(const char *filename, int mode);
      use iso_c_binding
      implicit none
      type(c_ptr) :: dlopen
      character(c_char), intent(in) :: filename(*)
      integer(c_int), value :: mode
  end function

  function dlsym(handle,name) bind(c,name="dlsym")
      ! void *dlsym(void *handle, const char *name);
      use iso_c_binding
      implicit none
      type(c_funptr) :: dlsym
      type(c_ptr), value :: handle
      character(c_char), intent(in) :: name(*)
  end function

  function dlclose(handle) bind(c,name="dlclose")
      ! int dlclose(void *handle);
      use iso_c_binding
      implicit none
      integer(c_int) :: dlclose
      type(c_ptr), value :: handle
  end function
end interface
#endif

contains
  function constructor(file_name, n_proc, proc_names) result(this)
    ! interface variables
    type(library_instance_t)    :: this
    character(len=*)            :: file_name
    integer                     :: n_proc
    character(len=*)            :: proc_names(*)
    ! local variables
#ifdef OS_WIN
    integer(handle)               :: file_handle
    integer(lpvoid)               :: proc_ptr
#else
    integer(c_int), parameter     :: rtld_lazy=1
#endif
    integer                       :: i
    character(len=2) :: err_msg
    
    ! load the library
    try: block
      this%file_name = trim(file_name)
#ifdef OS_WIN
      file_handle = LoadLibrary( this%file_name//c_null_char )
      this%file_addr = transfer(file_handle, this%file_addr) !convert INTEGER(HANDLE) to INTEGER(C_INTPTR_T) [used only for compatibility with gfortran]
      if ( this%file_addr == int(0,C_INTPTR_T) ) then
#else
      this%file_addr = dlopen(this%file_name//c_null_char, rtld_lazy )
      if (.not. c_associated(this%file_addr)) then
#endif
        write(err_msg,'(I2)') c_intptr_t*8 ! number of bits used in the application's memory addresses
        this%error = fail('The dynamic library '//this%file_name// &
                     ' could not be loaded. Check that the file '// &
                     'exists in the specified location and that it is compiled for '// &
                     trim(err_msg)//'-bit applications.')
        exit try
      end if
      ! load library procedures
      allocate(this%proc_name(n_proc))
      allocate(this%proc_addr(n_proc), source=c_null_funptr)
      do i = 1,n_proc
        this%proc_name(i)%s = trim(proc_names(i))
#ifdef OS_WIN
        proc_ptr = GetProcAddress( this%file_addr, this%proc_name(i)%s//c_null_char )
        this%proc_addr(i) = transfer(proc_ptr, this%proc_addr(i)) !convert INTEGER(LPVOID) to INTEGER(C_FUNPTR) [used only for compatibility with gfortran]
#else
        this%proc_addr(i) = dlsym(this%file_addr, this%proc_name(i)%s//c_null_char)
#endif
        if(.not. c_associated(this%proc_addr(i))) then
          this%error = fail('The procedure '//this%proc_name(i)%s// &
                       ' in file '//this%file_name//' could not be loaded.')
          exit try
        end if
      end do
      return
    end block try
    ! if we are here, there was an error; the caller will have to inquire about errors because functions can't return errors
  end function
  
  subroutine get_error(this, error)
    class(library_instance_t),   intent(in   ) :: this
    class(error_t), allocatable, intent(  out) :: error
    if (allocated(this%error)) then
      error = this%error
    end if
  end subroutine
  
  subroutine unload(this)
    ! interface variables
    class(library_instance_t) :: this
    ! local variables
#ifdef OS_WIN
    integer(handle) :: file_handle   ! The address of file FileName.  (RETURN value from LoadLibrary in kernel32.f90)
    integer(bool)   :: success       ! Whether or not the call to FreeLibrary was successful
#else
    integer(c_int)  :: success
#endif

    ! free the library
#ifdef OS_WIN
    if ( this%file_addr == int(0,C_INTPTR_T) ) return
    file_handle = transfer(this%file_addr, file_handle) !convert INTEGER(C_INTPTR_T) to INTEGER(HANDLE) [used only for compatibility with gfortran]
    success = FreeLibrary( file_handle ) !If the function succeeds, the return value is nonzero. If the function fails, the return value is zero.
#else
    if (.not. c_associated(this%file_addr)) return
    success = dlclose(this%file_addr)
#endif

#ifdef OS_WIN
    if ( success == false ) then !note that this is the Windows BOOL type so FALSE isn't the same as the Fortran LOGICAL .FALSE.
#else
    if ( success .ne. 0 ) then
#endif
      ! get_error() will not work after the object is destroyed: simply print the error
      print *, "The following dynamic library could not be freed: " // this%file_name
    else
#ifdef OS_WIN
      this%file_addr = int(0,C_INTPTR_T)
#else
      this%file_addr = c_null_ptr
#endif
    end if

  end subroutine
  
  subroutine destructor(this)
    type(library_instance_t) :: this
    ! do nothing
  end subroutine
end module
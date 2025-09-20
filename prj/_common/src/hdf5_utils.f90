! download hdf5 libraries here:
! https://support.hdfgroup.org/releases/hdf5/index.html
!
  module hdf5_utils
  
  use hdf5
  use extf, only: itoa, string_container_t
  use error_handling, only: error_t, fail, wrap_error
  use, intrinsic :: iso_c_binding, only: c_loc, c_ptr
  
  implicit none
  
  private
  
  ! management of the hdf5 interface
  public :: hdf5_initialize
  public :: hdf5_terminate
  
  ! file management
  public :: hdf5_open_file_read
  public :: hdf5_close_file
  public :: hid_t
  
  ! data reading
  public :: hdf5_read_array_auto
  public :: hdf5_read_string_auto
  public :: hdf5_read_varying_string_array_auto

  interface hdf5_read_array_auto
    module procedure hdf5_alloc_read_0d_array_ieee_f64le
    module procedure hdf5_alloc_read_1d_array_ieee_f64le
    module procedure hdf5_alloc_read_2d_array_ieee_f64le
    module procedure hdf5_alloc_read_3d_array_ieee_f64le
    module procedure hdf5_alloc_read_4d_array_ieee_f64le
    module procedure hdf5_alloc_read_5d_array_ieee_f64le
    module procedure hdf5_alloc_read_6d_array_ieee_f64le
  end interface hdf5_read_array_auto
  
  interface hdf5_read_string_auto
    module procedure hdf5_alloc_read_fortran_s1
  end interface
  
  interface hdf5_read_varying_string_array_auto
    module procedure hdf5_alloc_read_varying_array_fortran_s1
  end interface
  
  contains
  
  subroutine hdf5_initialize(error)
  ! initialize the hdf5 interface
  class(error_t), allocatable, intent(out) :: error
  integer :: hdferr
  try: block
    ! initialize the hdf5 environment
    call h5open_f(hdferr)
    if (hdferr .lt. 0) then
      error = fail("Error opening hdf5 interface.")
      exit try
    end if
    ! disable automatic printing of errors to stderr
    call h5eset_auto_f(0, hdferr)
    if (hdferr .lt. 0) then
      error = fail("Error specifying hdf5 error printing behavior.")
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_terminate(error)
  ! terminate the hdf5 interface and do garbage collection
  class(error_t), allocatable, intent(out) :: error
  integer :: hdferr
  try: block
    call h5garbage_collect_f(hdferr)
    if (hdferr .lt. 0) then
      error = fail("Failed to garbage-collect hdf5 objects.")
      exit try
    end if
    call h5close_f(hdferr)
    if (hdferr .lt. 0) then
      error = fail("Error closing hdf5 interface.")
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_open_file_read(file_name, fid, error)
  ! open an hdf5 file with read-only permission
  character(len=*),            intent(in   ) :: file_name
  integer(hid_t),              intent(  out) :: fid
  class(error_t), allocatable, intent(  out) :: error
  logical :: is_hdf5 = .false.
  integer :: hdferr
  try: block
    ! check if file_name exists as an hdf5 file
    call h5fis_hdf5_f(file_name, is_hdf5, hdferr)
    if (hdferr .lt. 0) then
      error = fail("Failure to determine if file is hdf5.")
      exit try
    end if
    if (.not. is_hdf5) then
      error = fail("File does not exist or is not a valid hdf5 file.")
      exit try
    end if
    ! open the file for reading
    call h5fopen_f(file_name, h5f_acc_rdonly_f, fid, hdferr)
    if (hdferr .lt. 0) then
      error = fail("Error opening hdf file.")
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_close_file(fid, error)
  ! close an hdf5 file
  integer(hid_t),              intent(in   ) :: fid
  class(error_t), allocatable, intent(  out) :: error
  integer :: hdferr
  try: block
    call h5fclose_f(fid, hdferr)
    if (hdferr .lt. 0) then
      error = fail("Error closing hdf5 file.")
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
  ! read data from an hdf5 file once the necessary buffer (pointed to by f_ptr) has been allocated
  integer(hid_t),   intent(in) :: dset_id, mem_type, dspace_id
  type(c_ptr),      intent(in) :: f_ptr
  class(error_t), allocatable, intent(out) :: error
  integer :: hdferr
  try: block
    ! read the data
    call h5dread_f(dset_id, mem_type, f_ptr, hdferr, dspace_id)
    if (hdferr .lt. 0) then
      error = fail("h5dread_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! close and release resources.
    call h5dclose_f(dset_id, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dclose_f returned error status " // itoa(hdferr))
      exit try
    end if
    call h5tclose_f(mem_type, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5tclose_f returned error status " // itoa(hdferr))
      exit try
    end if
    call h5sclose_f(dspace_id, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5sclose_f returned error status " // itoa(hdferr))
      exit try
    end if
  end block try
  end subroutine
  
  subroutine hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
  ! open the necessary handles amd get memory type & sizes, in preparation for reading data from an hdf5 file
  integer(hid_t),   intent(in   ) :: fid
  character(len=*), intent(in   ) :: path
  integer(hid_t),   intent(  out) :: dset_id, dspace_id, mem_type
  integer(hsize_t), allocatable, intent(  out) :: dims(:)
  integer(hsize_t),              intent(  out) :: data_size
  class(error_t), allocatable,   intent(  out) :: error
  integer                         :: array_rank, hdferr
  integer(hid_t)                  :: data_type
  integer(hsize_t), allocatable   :: maxdims(:)
  
  try: block
    ! open the dataset
    call h5dopen_f(fid, path, dset_id, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dopen_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! get the datatype and its size.
    call h5dget_type_f(dset_id, data_type, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dget_type_f returned error status " // itoa(hdferr))
      exit try
    end if
    call h5tget_size_f(data_type, data_size, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dget_size_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! get the dataspace identifier for the dataset
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dget_space_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! get the number of dimensions (rank) of the dataspace
    call h5sget_simple_extent_ndims_f(dspace_id, array_rank, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dget_simple_extent_ndims_f returned error status " // itoa(hdferr))
      exit try
    end if
    array_rank = max(array_rank, 1) ! scalars are sometimes given array rank 0, depending on the hdf library implementation
    allocate(dims(array_rank))
    allocate(maxdims(array_rank))
    ! get the dimensions of the dataspace
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5dget_simple_extent_dims_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! create the memory datatype.
    call h5tcopy_f(data_type, mem_type, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5tcopy_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! set the size of the memory datatype
    call h5tset_size_f(mem_type, data_size, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5tset_size_f returned error status " // itoa(hdferr))
      exit try
    end if
    ! close and release resources
    call h5tclose_f(data_type, hdferr)
    if (hdferr .lt. 0) then
      error = fail("h5tclose_f returned error status " // itoa(hdferr))
      exit try
    end if
  end block try
  
  end subroutine
  
  subroutine hdf5_alloc_read_0d_array_ieee_f64le(fid, path, scalar, error)
  ! allocates and reads a scalar from an hdf5 file
  integer(hid_t),                               intent(in   ) :: fid
  character(len=*),                             intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), target, intent(  out) :: scalar
  class(error_t), allocatable,                  intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if ((size(dims) .ne. 1) .and. (dims(1) .ne. 1)) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    f_ptr = c_loc(scalar)
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_1d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 1) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1)))
    f_ptr = c_loc(array(1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_2d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:,:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 2) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1), dims(2)))
    f_ptr = c_loc(array(1,1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_3d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:,:,:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 3) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1), dims(2), dims(3)))
    f_ptr = c_loc(array(1,1,1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_4d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:,:,:,:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 4) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    f_ptr = c_loc(array(1,1,1,1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_5d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:,:,:,:,:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 5) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    f_ptr = c_loc(array(1,1,1,1,1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_6d_array_ieee_f64le(fid, path, array, error)
  ! allocates and reads an array from an hdf5 file
  integer(hid_t),                                            intent(in   ) :: fid
  character(len=*),                                          intent(in   ) :: path
  real(selected_real_kind(p=15,r=307)), allocatable, target, intent(  out) :: array(:,:,:,:,:,:)
  class(error_t), allocatable,                               intent(  out) :: error
  ! local variables
  integer(hid_t)   :: dset_id, dspace_id, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 6) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    f_ptr = c_loc(array(1,1,1,1,1,1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine
  
  subroutine hdf5_alloc_read_fortran_s1(fid, path, string, error)
  ! reads a string from an hdf5 file
  integer(hid_t),   intent(in   ) :: fid
  character(len=*), intent(in   ) :: path
  character(len=:), allocatable, target, intent(  out) :: string
  class(error_t), allocatable,           intent(  out) :: error
  ! local variables
  integer(hid_t) :: dspace_id, dset_id, data_type, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:), maxdims(:)
  type(c_ptr) :: f_ptr
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 1) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    string = repeat(c_null_char, data_size)
    f_ptr = c_loc(string(1:1))
    ! read the string into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    return
  end block try
  end subroutine

  subroutine hdf5_alloc_read_varying_array_fortran_s1(fid, path, array, error)
  ! reads an array of varying-length strings (string_container_t) from an hdf5 file
  integer(hid_t),   intent(in   ) :: fid
  character(len=*), intent(in   ) :: path
  type(string_container_t), allocatable, intent(  out) :: array(:)
  class(error_t), allocatable,           intent(  out) :: error
  ! local variables
  integer(hid_t) :: dspace_id, dset_id, data_type, mem_type
  integer(hsize_t) :: data_size
  integer(hsize_t), allocatable :: dims(:), maxdims(:)
  character(len=:), allocatable, target :: rdata(:) !uniform-length character array
  type(c_ptr) :: f_ptr
  integer :: i, term_char
  
  try: block
    ! prepare to read data from the hdf file
    call hdf5_prepare_read(fid, path, dset_id, dspace_id, mem_type, dims, data_size, error)
    if (allocated(error)) then
      call wrap_error(error, "Error preparing to read dataset: " // path)
      exit try
    end if
    ! check for rank mismatch
    if (size(dims) .ne. 1) then
      error = fail("Rank mismatch between hdf5 file and data container: " // path)
      exit try
    end if
    ! allocate the data-receiving buffer and assign a pointer to its first element
    allocate(rdata(dims(1)), source=repeat(c_null_char, data_size))
    f_ptr = c_loc(rdata(1)(1:1))
    ! read the array into process memory and clean up resources
    call hdf5_do_read(dset_id, mem_type, f_ptr, dspace_id, error)
    if (allocated(error)) then
      call wrap_error(error, "Error reading dataset: " // path)
      exit try
    end if
    ! transfer the uniform-length character array to the varying-length array
    allocate(array(dims(1)))
    do i = 1,dims(1)
      term_char = index(rdata(i), c_null_char)
      if (term_char .gt. 1) then
        array(i)%s = trim(rdata(i)(1:(term_char-1)))
      else
        array(i)%s = trim(rdata(i))
      end if
    end do
    return
  end block try
  end subroutine
  
end module
module dispatch_manager
  !! dispatch_manager
  use extf, only: delimited_string_to_string_container_array
  use dispatch_types
  use dispatch_library_types, only: library_config_t, library_interaction_t
  use library_class, only: library_instance_t
  use error_handling, only: error_t, fail, wrap_error
  use, intrinsic :: iso_c_binding, only: c_int
  
  implicit none
  
  private
  type(interface_config_t), target         :: interface_config
  type(library_config_t), allocatable      :: library_config(:)
  type(library_interaction_t), allocatable, target :: library(:)
  
  public :: dispatch_version
  public :: dispatch_initialize
  public :: dispatch_sample
  public :: dispatch_terminate
!  public :: dispatch_write_output ! todo: not implemented yet
  
  contains
  
  function dispatch_version() result(version)
  !! dispatch_version() returns the version string of the dispatch library
  character(len=:), allocatable :: version
  version = "0.0.0"
  end function dispatch_version
  
  subroutine read_input_file(input_file, interface_config, library_config, error)
  !! read_input_file() reads the dispatch input file
  use tomlf
  use tomlf_error, only: toml_error, toml_stat
  use tomlf_error_handler, only: toml_error_message
  use extf, only: itoa
  
  implicit none
  
  ! interface variables
  character(len=*),                    intent(in   ) :: input_file
  type(interface_config_t),            intent(  out) :: interface_config
  type(library_config_t), allocatable, intent(  out) :: library_config(:)
  class(error_t), allocatable,         intent(  out) :: error
  ! local variables
  integer :: fid, rc, stat, i, n_library
  logical :: file_exists
  type(toml_table), allocatable        :: table
  type(toml_table), pointer            :: child
  type(toml_array), pointer            :: array
  type(toml_error), allocatable        :: error_toml
  type(toml_table), pointer            :: tmp_table
  type(toml_array), pointer            :: tmp_arr
  character(len=:), allocatable        :: tmp_str
  
  ! read input file using tomlf functions
  try: block
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
      
    ! get values from [interface] section
    call get_value(table, 'interface', child, stat=stat)
    if (stat .ne. toml_stat%success) then
      error = fail('"interface" get_value returned status: ' // toml_error_message(stat))
      exit try
    end if
    if (associated(child)) then
      ! sample period
      call get_value(child, 'sample_period', interface_config%sample_period, stat=stat)
      ! signal offers
      call get_value(child, 'signal_offers', tmp_arr, stat=stat)
      if (associated(tmp_arr)) then
        if (len(tmp_arr) .gt. 0) then
          interface_config%n_offers = len(tmp_arr)
          allocate(interface_config%signal_offers(interface_config%n_offers))
          do i = 1,interface_config%n_offers
            call get_value(tmp_arr, i, interface_config%signal_offers(i)%s)
          end do
        end if
        nullify(tmp_arr)
      end if
      ! signal requests
      call get_value(child, 'signal_requests', tmp_arr, stat=stat)
      if (associated(tmp_arr)) then
        if (len(tmp_arr) .gt. 0) then
          interface_config%n_requests = len(tmp_arr)
          allocate(interface_config%signal_requests(interface_config%n_requests))
          do i = 1,interface_config%n_requests
            call get_value(tmp_arr, i, interface_config%signal_requests(i)%s)
          end do
        end if
        nullify(tmp_arr)
      end if
    end if

    ! get library configuration information
    call get_value(table, "library", array, stat=stat)
    if (stat .ne. toml_stat%success) then
      error = fail('"library" get_value returned status: ' // toml_error_message(stat))
      exit try
    end if
    n_library = len(array)
    allocate(library_config(n_library))
    ! get configuration information for each library
    do i = 1,n_library
      call get_value(array, i, child, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail('"library"(' // itoa(i) // ') get_value returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! given name
      call get_value(child, "name", library_config(i)%given_name, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail('"library"(' // itoa(i) // ') get_value("name") returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! file name
      call get_value(child, "library_file", library_config(i)%file_name, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail('"library"(' // itoa(i) // ') get_value("library_file") returned status: ' // toml_error_message(stat))
        exit try
      end if
      ! parameters (either in a separate file, or in-line)
      call get_value(child, "parameters", tmp_table, stat=stat)
      if (stat .ne. toml_stat%success) then
        error = fail('"library"(' // itoa(i) // ') get_value("parameters") returned status: ' // toml_error_message(stat))
        exit try
      end if
      if (associated(tmp_table)) then
        call get_value(tmp_table, "parameter_file_name", tmp_str, stat=stat)
        if (allocated(tmp_str)) then
          ! parameters in separate file
          library_config(i)%parameters = tmp_str
          library_config(i)%parameters_length = int(len(tmp_str), kind=c_int)
          library_config(i)%parameters_as_toml_string = 0
          deallocate(tmp_str)
        else
          ! parameters in-line
          call toml_dumps(tmp_table, library_config(i)%parameters, error_toml)
          if (allocated(error_toml)) then
            error = fail(error_toml%message)
            exit try
          end if
          library_config(i)%parameters_length = int(len(library_config(i)%parameters), kind=c_int)
          library_config(i)%parameters_as_toml_string = 1
        end if
        nullify(tmp_table)
      end if
    end do
    
    return
  end block try
  
  end subroutine read_input_file
  
  subroutine dispatch_initialize(input_file, n_input, n_output, error, input_offers, output_requests, input_indices, output_indices)
  !! dispatch_initialize() reads the dispatch input file, loads the libraries, and resolves 
  !! the signal connections between libraries
  use, intrinsic :: iso_c_binding
  implicit none
  
  ! interface variables
  character(len=*),            intent(in   ) :: input_file
  integer,                     intent(  out) :: n_input, n_output
  class(error_t), allocatable, intent(  out) :: error
  type(signal_descriptor_t), optional,  intent(in   ) :: input_offers(:)
  type(signal_descriptor_t), optional,  intent(in   ) :: output_requests(:)
  integer, allocatable,      optional,  intent(  out) :: input_indices(:)
  integer, allocatable,      optional,  intent(  out) :: output_indices(:)
  
  ! local variables
  integer :: i, j, k, l, n_library, n_input_offers, n_output_requests
  character(kind=c_char, len=:), allocatable :: tmp_string, error_string
  character(len=20), parameter  :: proc_names(5) = (/"initialize", &
                                                     "get_signal_requests", &
                                                     "get_signal_offers", &
                                                     "update", &
                                                     "terminate"/)
  
  try: block
    
    ! read the input file
    call read_input_file(input_file, interface_config, library_config, error)
    if (allocated(error)) exit try
    ! load signal processing libraries
    n_library = size(library_config)
    interface_config%n_library = n_library
    ! check that library given names are unique (needed for result-writing and error identification)
    do i = 1,n_library
      do j = 1,n_library
        if ((i .ne. j) .and. (library_config(i)%given_name .eq. library_config(j)%given_name)) then
          error = fail("Duplicate library given names: '" // library_config(i)%given_name // "'")
          exit try
        end if
      end do
    end do
    allocate(library(n_library))
    ! set up procedure information
    do i = 1,n_library
      library(i)%config = library_config(i)
      library(i)%handles = library_instance_t(library(i)%config%file_name, size(proc_names), proc_names)
      call library(i)%handles%get_error(error)
      if (allocated(error)) exit try
      call C_F_PROCPOINTER(library(i)%handles%proc_addr(1), library(i)%initialize)
      call C_F_PROCPOINTER(library(i)%handles%proc_addr(2), library(i)%get_signal_requests)
      call C_F_PROCPOINTER(library(i)%handles%proc_addr(3), library(i)%get_signal_offers)
      call C_F_PROCPOINTER(library(i)%handles%proc_addr(4), library(i)%update)
      call C_F_PROCPOINTER(library(i)%handles%proc_addr(5), library(i)%terminate)
    end do
  
    ! initialize signal processing libraries
    do i = 1,n_library
      call library(i)%initialize(library(i)%config%parameters,                &
                                 library(i)%config%parameters_length,         &
                                 library(i)%config%parameters_as_toml_string, &
                                 library(i)%context,                          &
                                 library(i)%sample_period,                    &
                                 library(i)%n_requests,                       &
                                 library(i)%n_offers,                         &
                                 error_string)
      if (allocated(error_string)) then
        error = fail(error_string)
        call wrap_error(error, "Initialization of library '" // library(i)%config%given_name // "' failed.")
        exit try
      end if
      ! determine whether library sample period is compatible with dispatch sample period
      if (library(i)%sample_period .lt. interface_config%sample_period) then
        ! if library sample period is zero, sample the library at each dispatch sample
        if (library(i)%sample_period .eq. 0) then
          library(i)%sample_period = interface_config%sample_period
        else
          ! otherwise the library sample period is too small
          error = fail("Library sample period must be >= dispatch sample period; library name: " // library(i)%config%given_name)
          exit try
        end if
      end if
      ! check to make sure that the library sample period is an integer multiple of the dispatch sample period
      if (mod(library(i)%sample_period + 2.0_wp * epsilon(library(i)%sample_period), interface_config%sample_period) .gt. 5.0_wp * epsilon(library(i)%sample_period)) then
        error = fail("Library sample period must be an integer multiple of dispatch sample period; library name: " // library(i)%config%given_name)
        exit try
      end if
      ! initialize the sample hit buffer (e.g. for a library sampled every 3 dispatch-samples, [.true., .false., .false])
      library(i)%n_hit = int(library(i)%sample_period / interface_config%sample_period)
      allocate(library(i)%hit_buffer(library(i)%n_hit), source=.false.)
      library(i)%hit_buffer(1) = .true.
      library(i)%hit_buffer_index = 0
      ! allocate input (requests) and output (offers) buffers
      allocate(library(i)%input_buffer(library(i)%n_requests))
      allocate(library(i)%input_pointers(library(i)%n_requests))
      allocate(library(i)%output_buffer(library(i)%n_offers))
      ! get signal offers and requests
      if (library(i)%n_requests .gt. 0) then
        call library(i)%get_signal_requests(library(i)%context, library(i)%signal_requests_string, error_string)
        if (allocated(error_string)) then
          error = fail(error_string)
          call wrap_error(error, "Failure in get_signal_requests() for library '" // library(i)%config%given_name // "'.")
          exit try
        end if
        call delimited_string_to_string_container_array(library(i)%signal_requests, library(i)%signal_requests_string, ";")
      end if
      if (library(i)%n_offers .gt. 0) then
        call library(i)%get_signal_offers(library(i)%context, library(i)%signal_offers_string, error_string)
        if (allocated(error_string)) then
          error = fail(error_string)
          call wrap_error(error, "Failure in get_signal_offers() for library '" // library(i)%config%given_name // "'.")
          exit try
        end if
        call delimited_string_to_string_container_array(library(i)%signal_offers, library(i)%signal_offers_string, ";")
      end if
    end do
    
    ! for callers that provide named signals, resolve signal connections between the caller and the interface 
    ! (this reconciles e.g. Bladed swap array with the signal requests/offers specified in the dispatch .toml file)
    ! The user may not specify additional signal offers or requests in this case: the input and output signal list
    ! is determined by the caller.
    if (present(input_offers)) then
      ! all of the optional arguments are needed in this case; check that they are present
      if (.not. (present(output_requests) .and. present(input_indices) .and. present(output_indices))) then
        error = fail("Dispatch_initialize was called with some but not all optional arguments (input/output offers and indices); all are needed if one is present.")
        exit try
      end if
      ! count the offered input signals
      n_input = 0
      do i = 1,size(input_offers)
        if (input_offers(i)%permission(1:1) .eq. "r") then
          n_input = n_input + 1
        end if
      end do
      ! register the offered input signals as signals that are available from the caller
      if (allocated(interface_config%signal_requests)) then
        error = fail("Dispatch interface may not request signals in addition to those provided by the caller. Remove signal_requests from dispatch .toml file.")
        exit try
      else
        interface_config%n_requests = n_input
        allocate(interface_config%signal_requests(n_input))
        allocate(input_indices(n_input))
      end if
      k = 0
      do i = 1,size(input_offers)
        if (input_offers(i)%permission(1:1) .eq. "r") then
          k = k + 1
          interface_config%signal_requests(k)%s = input_offers(i)%name
          input_indices(k) = i
        end if
      end do
      ! count the requested output signals
      n_output = 0
      do i = 1,size(output_requests)
        if (output_requests(i)%permission(2:2) .eq. "w") then
          n_output = n_output + 1
        end if
      end do
      ! register the requested output signals as signals that must be provided by the libraries
      if (allocated(interface_config%signal_offers)) then
        error = fail("Dispatch interface may not offer signals in addition to those requested by the caller. Remove signal_offers from dispatch .toml file.")
        exit try
      else
        interface_config%n_offers = n_output
        allocate(interface_config%signal_offers(n_output))
        allocate(output_indices(n_output))
      end if
      k = 0
      do i = 1,size(output_requests)
        if (output_requests(i)%permission(2:2) .eq. "w") then
          k = k + 1
          interface_config%signal_offers(k)%s = output_requests(i)%name
          output_indices(k) = i
        end if
      end do
    else
      n_input = interface_config%n_requests
      n_output = interface_config%n_offers
    end if
    allocate(interface_config%input_buffer(n_input))
    allocate(interface_config%output_pointers(n_output))
  
    ! resolve signal connections between the libraries
    ! for each signal request from each library, examine signal offers from preceding 
    ! libraries and the interface itself to find a match (name-based); error if unmatched
    do i = 1,n_library
      library_requests: do j = 1,library(i)%n_requests
        do l = (i-1),0,-1
          if (l .eq. 0) then ! find signal in dispatch inputs
            do k = interface_config%n_requests,1,-1 ! go in reverse order, in case of multiple copies of a signal in the interface requests
              if (library(i)%signal_requests(j)%s .eq. interface_config%signal_requests(k)%s) then
                library(i)%input_pointers(j)%ptr => interface_config%input_buffer(k)
                cycle library_requests
              end if
            end do
          else ! find signal in the outputs of preceding libraries
            do k = library(l)%n_offers,1,-1 ! go in reverse order, so that the last version of a signal is used, in case it gets output multiple times by the same library
              if (library(i)%signal_requests(j)%s .eq. library(l)%signal_offers(k)%s) then
                library(i)%input_pointers(j)%ptr => library(l)%output_buffer(k)
                cycle library_requests
              end if
            end do
          end if
        end do
        ! if no match found yet, then look for signal in the outputs of subsequent libraries (value will be delayed by one sample)
        do l = n_library, i, -1
          do k = library(l)%n_offers,1,-1 ! go in reverse order, so that the last version of a signal is used, in case it gets output multiple times by the same library
            if (library(i)%signal_requests(j)%s .eq. library(l)%signal_offers(k)%s) then
              library(i)%input_pointers(j)%ptr => library(l)%output_buffer(k)
              cycle library_requests
            end if
          end do
        end do
        ! error if this signal has no provider
        if (.not. associated(library(i)%input_pointers(j)%ptr)) then
          error = fail("No matching signal offers for signal request '" // library(i)%signal_requests(j)%s // "' from library '" // library(i)%config%given_name //"'")
          exit try
        end if
      end do library_requests
    end do
    ! resolve the dispatch output signal connections
    interface_offers: do i = 1,interface_config%n_offers
      do l = n_library, 0, -1
        if (l .eq. 0) then ! direct pass-through
          do k = 1,interface_config%n_requests
            if (interface_config%signal_offers(i)%s .eq. interface_config%signal_requests(k)%s) then
              interface_config%output_pointers(i)%ptr => interface_config%input_buffer(k)
              cycle interface_offers
            end if
          end do
        else ! dispatch output signal comes from one of the libraries
          do k = 1,library(l)%n_offers
            if (interface_config%signal_offers(i)%s .eq. library(l)%signal_offers(k)%s) then
              interface_config%output_pointers(i)%ptr => library(l)%output_buffer(k)
              cycle interface_offers
            end if
          end do
        end if
      end do
      ! error if this signal has no provider
      if (.not. associated(interface_config%output_pointers(i)%ptr)) then
        error = fail("No matching signal providers for interface signal offer '" // interface_config%signal_offers(i)%s //"'")
        exit try
      end if
    end do interface_offers
    return
    
  end block try
  ! if we are here, then there has been an error: pass it to the caller
  end subroutine dispatch_initialize

  subroutine dispatch_sample(input_buffer, output_buffer, error)
  implicit none
  ! interface variables
  real(wp),                    intent(in   ) :: input_buffer(*)
  real(wp),                    intent(  out) :: output_buffer(*)
  class(error_t), allocatable, intent(  out) :: error
  
  ! local variables
  integer :: i, j
  character(len=:), allocatable :: error_string
  
  try: block
    ! copy input signals into the interface_config input signal buffer
    do i = 1,interface_config%n_requests
      interface_config%input_buffer(i) = input_buffer(i)
    end do
    ! sample the libraries
    do i = 1,interface_config%n_library
      if (library(i)%hit_buffer(library(i)%hit_buffer_index + 1)) then
        ! copy most recent input signal values to this library's input buffer
        do j = 1,library(i)%n_requests
          library(i)%input_buffer(j) = library(i)%input_pointers(j)%ptr
        end do
        call library(i)%update(library(i)%context, library(i)%input_buffer, library(i)%output_buffer, error_string)
        if (allocated(error_string)) then
          error = fail(error_string)
          call wrap_error(error, "Failure in update() for library '" // library(i)%config%given_name // "'.")
          exit try
        end if
      end if
      library(i)%hit_buffer_index = mod(library(i)%hit_buffer_index + 1, library(i)%n_hit)
    end do
    ! copy the output signals into the output buffer
    do i = 1,interface_config%n_offers
      output_buffer(i) = interface_config%output_pointers(i)%ptr
    end do
    return
    
  end block try
  ! if we are here, then there has been an error: pass it to the caller
  end subroutine dispatch_sample

  subroutine dispatch_terminate(error)
  implicit none
  class(error_t), allocatable,     intent(  out) :: error
  
  ! local variables
  integer :: i
  character(len=:), allocatable :: error_string
  ! terminate all signal processing libraries
  try: block
    do i = 1,interface_config%n_library
      call library(i)%terminate(library(i)%context, error_string)
      if (allocated(error_string)) then
        error = fail(error_string)
        call wrap_error(error, "Failure in terminate() for library '" // library(i)%config%given_name // "'.")
        exit try
      end if
      ! unload each library after it has been terminated
      call library(i)%handles%unload()
    end do
    return
  end block try
  ! if we are here, then there has been an error: pass it to the caller
  end subroutine dispatch_terminate

end module dispatch_manager
module dispatch_types
  use extf, only: string_container_t
  use, intrinsic :: iso_c_binding, only: c_double
  use, intrinsic :: iso_fortran_env
  
  implicit none
  
#ifdef REAL32
  integer, parameter, public :: dispatch_rk = real32   !! real kind used by this module [4 bytes]
#elif REAL64
  integer, parameter, public :: dispatch_rk = real64   !! real kind used by this module [8 bytes]
#elif REAL128
  integer, parameter, public :: dispatch_rk = real128  !! real kind used by this module [16 bytes]
#else
  integer, parameter, public :: dispatch_rk = real64   !! real kind used by this module [8 bytes]
#endif

  integer,parameter,public :: wp = dispatch_rk         !! copy of `dispatch_rk` with a shorter name
  
  enum, bind(c)
    enumerator enum_kind                               !! dummy enumerator for determining enum kind
  end enum
  integer,parameter,public :: ek = kind(enum_kind)     !! enum integer kind
  
  ! pointers to signals
  type :: signal_pointer_t
    real(wp), pointer :: ptr => null()
  end type
  
  ! signal descriptor
  type :: signal_descriptor_t
    character(len=:), allocatable :: name
    character(1) :: data_type = '-'
    character(2) :: permission = '--'
    character(len=:), allocatable :: units
    character(len=:), allocatable :: description
  end type
  
  ! interface configuration
  type :: interface_config_t
    real(wp)                              :: sample_period
    integer                               :: n_offers, n_requests, n_library
    type(string_container_t), allocatable :: signal_offers(:)
    type(string_container_t), allocatable :: signal_requests(:)
    real(wp),                 allocatable :: input_buffer(:)
    type(signal_pointer_t),   allocatable :: output_pointers(:)
  end type

end module dispatch_types

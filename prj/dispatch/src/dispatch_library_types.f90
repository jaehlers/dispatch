module dispatch_library_types

use extf, only: string_container_t
use library_class, only: library_instance_t
use dispatch_types, only: signal_pointer_t, wp
use tomlf, only: toml_table
use error_handling, only: error_t
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  
implicit none
  
!private
  
public :: library_config_t, library_interaction_t

! library configuration
type :: library_config_t
  character(len=:),              allocatable :: given_name
  character(len=:),              allocatable :: file_name
  character(len=:, kind=c_char), allocatable :: parameters
  integer(kind=c_int)                        :: parameters_length
  integer(kind=c_int)                        :: parameters_as_toml_string
end type
  
! library procedure interfaces
abstract interface
  subroutine initialize_procedure(parameters, parameters_length, parameters_as_toml_string, context, sample_period, n_requests, n_offers, error) bind(C)
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double
  character(kind=c_char, len=*),              intent(in   ) :: parameters
  integer(kind=c_int),                        intent(in   ) :: parameters_length
  integer(kind=c_int),                        intent(in   ) :: parameters_as_toml_string
  integer(kind=c_int),                        intent(  out) :: context
  real(kind=c_double),                        intent(  out) :: sample_period
  integer(kind=c_int),                        intent(  out) :: n_requests
  integer(kind=c_int),                        intent(  out) :: n_offers
  character(kind=c_char, len=:), allocatable, intent(  out) :: error
  end subroutine initialize_procedure
  
  subroutine get_signal_requests_procedure(context, requests, error) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_char
  integer(kind=c_int),                        intent(in   ) :: context
  character(kind=c_char, len=:), allocatable, intent(  out) :: requests
  character(kind=c_char, len=:), allocatable, intent(  out) :: error
  end subroutine get_signal_requests_procedure

  subroutine get_signal_offers_procedure(context, offers, error) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_char
  integer(kind=c_int),                        intent(in   ) :: context
  character(kind=c_char, len=:), allocatable, intent(  out) :: offers
  character(kind=c_char, len=:), allocatable, intent(  out) :: error
  end subroutine get_signal_offers_procedure

  subroutine update_procedure(context, input_array, output_array, error) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
  integer(kind=c_int),                        intent(in   ) :: context
  real(kind=c_double),                        intent(in   ) :: input_array(*)
  real(kind=c_double),                        intent(  out) :: output_array(*)
  character(kind=c_char, len=:), allocatable, intent(  out) :: error
  end subroutine update_procedure

  subroutine terminate_procedure(context, error) bind(C)
  use, intrinsic :: iso_c_binding, only: c_int, c_char
  integer(kind=c_int),                        intent(in   ) :: context
  character(kind=c_char, len=:), allocatable, intent(  out) :: error
  end subroutine terminate_procedure
end interface
  
! library management and interaction
type library_interaction_t
  type(library_config_t)   :: config
  type(library_instance_t) :: handles
  integer(kind=c_int)      :: context
  real(kind=c_double)      :: sample_period
  logical, allocatable     :: hit_buffer(:)
  integer                  :: hit_buffer_index = 0 ! an index that cycles through the hit buffer to determine when to sample the library
  integer(kind=c_int)      :: n_hit, n_requests, n_offers
  character(kind=c_char, len=:), allocatable :: signal_requests_string
  character(kind=c_char, len=:), allocatable :: signal_offers_string
  type(string_container_t), allocatable :: signal_requests(:)
  type(string_container_t), allocatable :: signal_offers(:)
  type(signal_pointer_t),   allocatable :: input_pointers(:)
  real(kind=c_double),      allocatable :: input_buffer(:)
  real(kind=c_double),      allocatable :: output_buffer(:)
  ! library procedure pointers
  procedure(initialize_procedure), nopass, pointer :: initialize => null()
  procedure(get_signal_requests_procedure), nopass, pointer :: get_signal_requests => null()
  procedure(get_signal_offers_procedure), nopass, pointer :: get_signal_offers => null()
  procedure(update_procedure), nopass, pointer :: update => null()
  procedure(terminate_procedure), nopass, pointer :: terminate => null()
end type library_interaction_t
  
end module dispatch_library_types
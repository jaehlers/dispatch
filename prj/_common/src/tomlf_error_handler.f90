module tomlf_error_handler
!! tomlf_error_handler interprets tomlf error codes to error strings for use in higher-level error handlers
implicit none
private
public toml_error_message
contains

function toml_error_message(stat) result(message)
use :: tomlf_error, only: toml_stat
implicit none
integer, intent(in) :: stat
character(len=:), allocatable :: message

select case(stat)
case(toml_stat%success)
  message = "success"
case(toml_stat%fatal)
  message = "fatal"
case(toml_stat%duplicate_key)
  message = "duplicate key"
case(toml_stat%type_mismatch)
  message = "type mismatch"
case(toml_stat%conversion_error)
  message = "conversion error"
case(toml_stat%missing_key)
  message = "missing key"
case default
  message = "undefined toml error"
end select
end function toml_error_message
end module tomlf_error_handler

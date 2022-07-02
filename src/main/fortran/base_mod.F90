!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Basic parameters.
module base_mod
  implicit none

  !> @brief Byte integer kind (one byte)
  integer, parameter :: byte = selected_int_kind(2)

  !> @brief Long integer kind (eight bytes)
  integer, parameter :: long = selected_int_kind(18)

  !> @brief At least IEEE S_floating ("single precision")
  integer, parameter :: sp = selected_real_kind(p=6,r=37)

  !> @brief At least IEEE T_floating ("double precision")
  integer, parameter :: dp = selected_real_kind(p=15,r=307)

  !> @brief Working precision -- use this to set/switch precision globally
  integer, parameter :: wp = dp

  !> @brief Precision of coordinates or measurements
  integer, parameter :: cp = sp

  !> @brief Precision of time
  integer, parameter :: tp = dp

end module base_mod

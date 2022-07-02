!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Matrix addition: C = A + B
module addition_mod
  
  implicit none

  !> @brief At least IEEE T_floating ("double precision")
  integer, parameter :: wp = selected_real_kind(p=15,r=307)

  real(kind=wp), parameter :: EPS = 1.0E-06_wp

contains

  !> @brief The operation.
  subroutine op( n, a, b, c )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(out) :: c(n,n)

    c = a + b
  end subroutine op

  !> @brief The reference adjoint derivative code.
  subroutine op_ad( n, a, a_ad, b, b_ad, c, c_ad )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(inout) :: a_ad(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(inout) :: b_ad(n,n)
    real(kind=wp), intent(inout) :: c(n,n)
    real(kind=wp), intent(inout) :: c_ad(n,n)

    c = a + b
    a_ad = a_ad + c_ad
    b_ad = b_ad + c_ad
    ! intentionally omitted c_ad = 0.0
  end subroutine op_ad

  !> @brief The reference tangent derivative code.
  subroutine op_tl( n, a, a_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: a_tl(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(in) :: b_tl(n,n)
    real(kind=wp), intent(inout) :: c(n,n)
    real(kind=wp), intent(inout) :: c_tl(n,n)

    c = a + b
    c_tl = a_tl + b_tl
  end subroutine op_tl

  !> @brief The numeric tangent derivative code.
  subroutine op_fd( n, a, a_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: a_tl(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(in) :: b_tl(n,n)
    real(kind=wp), intent(out) :: c(n,n) 
    real(kind=wp), intent(out) :: c_tl(n,n)
    real(kind=wp) :: c1(n,n)
    real(kind=wp) :: c2(n,n)

    call op( n, a, b, c )
    call op( n, a - EPS * a_tl, b - EPS * b_tl, c1 )
    call op( n, a + EPS * a_tl, b + EPS * b_tl, c2 )
    c_tl = (c2 - c1) / (2.0_wp * EPS)
  end subroutine op_fd

end module addition_mod
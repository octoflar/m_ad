!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Evaluate the cost function c = 1/2 b^T d, where A d = b and A is symmetric and positive definite

module cost_mod
  use base_mod, only: wp
  use math_mod

  implicit none

  real(kind=wp), parameter :: EPS = 1.0E-07_wp

contains

  !> @brief Linear system solver, solves A d = b.
  subroutine solve( n, a, b, d )
    use solver_mod

    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: d(n)    
    
    call cg( n, a, b, d, EPS, 100 )
  end subroutine solve
    
  !> @brief The operation.
  subroutine op( n, a, b, c )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: c
    real(kind=wp) :: d(n)

    call solve( n, a, b, d )
    c = 0.5_wp * dot_product( b, d )
  end subroutine op

  !> @brief The reference adjoint derivative code.
  !! @remark AD code calls the solver twice, whereas this hand-written code requires only a single call.
  subroutine op_ad( n, a, a_ad, b, b_ad, c, c_ad )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(inout) :: a_ad(n,n)
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(inout) :: b_ad(n)
    real(kind=wp), intent(inout) :: c
    real(kind=wp), intent(inout) :: c_ad
    real(kind=wp) :: d(n)

    call solve( n, a, b, d )
    c = 0.5_wp * dot_product( b, d )
    b_ad = b_ad + c_ad * d
    a_ad = a_ad - 0.5_wp * c_ad * outer_product( n, d, d )
    ! intentionally omitted c_ad = 0.0
  end subroutine op_ad

  !> @brief The reference tangent derivative code.
  !! @remark AD code calls the solver twice, whereas this hand-written code requires only a single call.
  subroutine op_tl( n, a, a_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: a_tl(n,n)
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(in) :: b_tl(n)
    real(kind=wp), intent(inout) :: c
    real(kind=wp), intent(inout) :: c_tl
    real(kind=wp) :: d(n)

    call solve( n, a, b, d )
    c = 0.5_wp * dot_product( b, d )
    c_tl = dot_product( b_tl - 0.5_wp * matmul( a_tl, d ), d )
  end subroutine op_tl

  !> @brief The numeric tangent derivative code.
  subroutine op_fd( n, a, a_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: a_tl(n,n)
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(in) :: b_tl(n)
    real(kind=wp), intent(out) :: c 
    real(kind=wp), intent(out) :: c_tl
    real(kind=wp) :: c1
    real(kind=wp) :: c2

    call op( n, a, b, c )
    call op( n, a - EPS * a_tl, b - EPS * b_tl, c1 )
    call op( n, a + EPS * a_tl, b + EPS * b_tl, c2 )
    c_tl = (c2 - c1) / (2.0_wp * EPS)
  end subroutine op_fd

end module cost_mod

!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Evaluate the cost function c = 1/2 b^T d, where A(x) d = b and A(x) is a function

module cost_matrix_free_mod
  use base_mod, only: wp

  implicit none

  real(kind=wp), parameter :: EPS = 1.0E-07_wp

contains

  !> @brief Linear system solver, solves A d = b.
  subroutine solve( n, x, b, d )
    use solver_mod

    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: d(n)    
    
    call cg( n, a, b, d, m_inv, EPS, 100 )

  contains

    function a( n, d ) result (b)
      use matmul_mod

      integer, intent(in) :: n
      real(kind=wp), intent(in) :: d(n)
      real(kind=wp) :: b(n)

      call a_matmul( n, x, d, b )
    end function  

    function m_inv( n, b ) result (d)
      use matmul_mod
      
      integer, intent(in) :: n
      real(kind=wp), intent(in) :: b(n)
      real(kind=wp) :: d(n)

      call m_inv_matmul( n, x, b, d )
    end function  
  end subroutine solve

  !> @brief The operation.
  subroutine op( n, x, b, c )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: c
    real(kind=wp) :: d(n)

    call solve( n, x, b, d )
    c = 0.5_wp * dot_product( b, d )
  end subroutine op

  !> @brief The reference adjoint derivative code.
  !! @remark AD code calls the solver twice, whereas this hand-written code requires only a single call.
  subroutine op_ad( n, x, x_ad, b, b_ad, c, c_ad )
    use matmul_mod, only: a_matmul_xad

    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(inout) :: x_ad
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(inout) :: b_ad(n)
    real(kind=wp), intent(inout) :: c
    real(kind=wp), intent(inout) :: c_ad
    real(kind=wp) :: d(n)
    real(kind=wp) :: h(n)
    real(kind=wp) :: h_ad(n)

    call solve( n, x, b, d )
    c = 0.5_wp * dot_product( b, d )
    b_ad = b_ad + c_ad * d
    h_ad = -0.5_wp * c_ad * d
    call a_matmul_xad( n, x, x_ad, d, h, h_ad )    
    ! intentionally omitted c_ad = 0.0
  end subroutine op_ad

  !> @brief The reference tangent derivative code.
  !! @remark AD code calls the solver twice, whereas this hand-written code requires only a single call.
  subroutine op_tl( n, x, x_tl, b, b_tl, c, c_tl )
    use matmul_mod, only: a_matmul_xtl

    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: x_tl
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(in) :: b_tl(n)
    real(kind=wp), intent(inout) :: c
    real(kind=wp), intent(inout) :: c_tl
    real(kind=wp) :: d(n)
    real(kind=wp) :: h(n)
    real(kind=wp) :: h_tl(n)

    call solve( n, x, b, d )
    c = 0.5_wp * dot_product( b, d )
    call a_matmul_xtl( n, x, x_tl, d, h, h_tl )
    c_tl = dot_product( b_tl - 0.5_wp * h_tl, d )
  end subroutine op_tl

  !> @brief The numeric tangent derivative code.
  subroutine op_fd( n, x, x_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: x_tl
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(in) :: b_tl(n)
    real(kind=wp), intent(out) :: c 
    real(kind=wp), intent(out) :: c_tl
    real(kind=wp) :: c1
    real(kind=wp) :: c2

    call op( n, x, b, c )
    call op( n, x - EPS * x_tl, b - EPS * b_tl, c1 )
    call op( n, x + EPS * x_tl, b + EPS * b_tl, c2 )
    c_tl = (c2 - c1) / (2.0_wp * EPS)
  end subroutine op_fd

end module cost_matrix_free_mod

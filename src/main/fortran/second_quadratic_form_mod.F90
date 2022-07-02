!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Second quadratic forn: C = B^T A-1 B

module second_quadratic_form_mod

  implicit none

  !> @brief At least IEEE T_floating ("double precision")
  integer, parameter :: wp = selected_real_kind(p=15,r=307)

  real(kind=wp), parameter :: EPS = 1.0E-05_wp

contains

  !> @brief Matrix inversion.
  subroutine inv( n, a, c )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(out) :: c(n,n)
    real(kind=wp) :: work(n)
    integer :: ipiv(n)
    integer :: info  
    ! External procedures defined in LAPACK
    external dgetrf
    external dgetri
  
    c = a
    call dgetrf( n, n, c, n, ipiv, info )
    call dgetri( n, c, n, ipiv, work, n, info )
  end subroutine inv

  !> @brief The operation.
  subroutine op( n, a, b, c )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(out) :: c(n,n)
    real(kind=wp) :: a_inv(n,n)

    call inv( n, a, a_inv )
    c = matmul( transpose( b ), matmul( a_inv, b ) )
  end subroutine op

  !> @brief The reference adjoint derivative code.
  !! @remark AD code calls the matrix inversion twice, whereas this hand-written code requires only a single call.
  subroutine op_ad( n, a, a_ad, b, b_ad, c, c_ad )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(inout) :: a_ad(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(inout) :: b_ad(n,n)
    real(kind=wp), intent(inout) :: c(n,n)
    real(kind=wp), intent(inout) :: c_ad(n,n)
    real(kind=wp) :: a_inv(n,n)
    real(kind=wp) :: h1(n,n)
    real(kind=wp) :: h2(n,n)

    call inv( n, a, a_inv )
    h1 = matmul( a_inv, b )
    h2 = matmul( transpose( a_inv ), matmul( b, c_ad ) )
    c = matmul( transpose( b ), h1 )
    b_ad = b_ad + matmul( h1, transpose( c_ad ) ) + h2
    a_ad = a_ad - matmul( h2, transpose( h1 ) )
    ! intentionally omitted c_ad = 0.0
  end subroutine op_ad

  !> @brief The reference tangent derivative code.
  !! @remark AD code calls the matrix inversion twice, whereas this hand-written code requires only a single call.
  subroutine op_tl( n, a, a_tl, b, b_tl, c, c_tl )
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: a(n,n)
    real(kind=wp), intent(in) :: a_tl(n,n)
    real(kind=wp), intent(in) :: b(n,n)
    real(kind=wp), intent(in) :: b_tl(n,n)
    real(kind=wp), intent(inout) :: c(n,n)
    real(kind=wp), intent(inout) :: c_tl(n,n)
    real(kind=wp) :: a_inv(n,n)
    real(kind=wp) :: h1(n,n)
    real(kind=wp) :: h2(n,n)

    call inv( n, a, a_inv )
    h1 = matmul( a_inv, b )
    h2 = matmul( transpose( b ), a_inv )
    c = matmul( transpose( b ), h1 )
    c_tl = matmul( transpose( b_tl ), h1 ) + matmul( h2, b_tl ) - matmul( h2, matmul( a_tl, h1 ) ) 
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

end module second_quadratic_form_mod

!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Linear system solvers.

!> @brief Methods from [Barret et al. (1994)](http://www.netlib.org/linalg/html_templates/Templates.html)
module solver_mod
  use base_mod, only: dp
    
  implicit none
    
  private
    
  public cg
  public cg_ad
  public cg_tl
    
  interface cg
    module procedure cg__dp
    module procedure cg_diagonal_matrix__dp
    module procedure cg_symmetric_matrix__dp
  end interface

  interface cg_ad
    module procedure cg_diagonal_matrix_ad__dp
    module procedure cg_symmetric_matrix_ad__dp
  end interface

  interface cg_tl
    module procedure cg_diagonal_matrix_tl__dp
    module procedure cg_symmetric_matrix_tl__dp
  end interface

contains
      
  subroutine cg_diagonal_matrix__dp( n, a, b, x, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(out) :: x(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration
        
    call cg__dp( n, a_matmul, b, x, m_inv_matmul, accuracy_goal, max_iteration )
        
  contains
        
    pure function a_matmul( n, x ) result (y)
      use base_mod, only: dp
        
      integer, intent(in) :: n
      real(kind=dp), intent(in) :: x(n)
      real(kind=dp) :: y(n)
        
      y = x * a
    end function a_matmul
      
    !> @brief The inverse Jacobi preconditioning matrix.
    pure function m_inv_matmul( n, x ) result (y)
      use base_mod, only: dp
      
      integer, intent(in) :: n
      real(kind=dp), intent(in) :: x(n)
      real(kind=dp) :: y(n)
      
      y = x / a
    end function m_inv_matmul
  end subroutine cg_diagonal_matrix__dp

  subroutine cg_diagonal_matrix_ad__dp( n, a, a_ad, b, b_ad, x, x_ad, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n)
    real(kind=dp), intent(inout) :: a_ad(n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(inout) :: b_ad(n)
    real(kind=dp), intent(inout) :: x(n)
    real(kind=dp), intent(inout) :: x_ad(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration
    
    real(kind=dp) :: h(n)

    call cg( n, a, b, x, accuracy_goal, max_iteration )
    call cg( n, a, x_ad, h, accuracy_goal, max_iteration )
    b_ad = b_ad + h
    a_ad = a_ad - h * x
    x_ad = 0.0_dp
  end subroutine cg_diagonal_matrix_ad__dp

  subroutine cg_diagonal_matrix_tl__dp( n, a, a_tl, b, b_tl, x, x_tl, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n)
    real(kind=dp), intent(in) :: a_tl(n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(in) :: b_tl(n)
    real(kind=dp), intent(inout) :: x(n)
    real(kind=dp), intent(inout) :: x_tl(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration

    call cg( n, a, b, x, accuracy_goal, max_iteration )
    call cg( n, a, b_tl - a_tl * x, x_tl, accuracy_goal, max_iteration )
  end subroutine cg_diagonal_matrix_tl__dp

  subroutine cg_symmetric_matrix__dp( n, a, b, x, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(out) :: x(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration
    
    call cg__dp( n, a_matmul, b, x, m_inv_matmul, accuracy_goal, max_iteration )
    
  contains
    
    pure function a_matmul( n, x ) result (y)
      use base_mod, only: dp
    
      integer, intent(in) :: n
      real(kind=dp), intent(in) :: x(n)
      real(kind=dp) :: y(n)
    
      y = matmul( a, x )
    end function a_matmul

    !> @brief The inverse Jacobi preconditioning matrix.
    pure function m_inv_matmul( n, x ) result (y)
      use base_mod, only: dp

      integer,       intent(in) :: n
      real(kind=dp), intent(in) :: x(n)
      real(kind=dp) :: y(n)
      integer :: i

      do concurrent (i = 1:n)
        y(i) = x(i) / a(i,i)
      end do
    end function m_inv_matmul
  end subroutine cg_symmetric_matrix__dp

  subroutine cg_symmetric_matrix_ad__dp( n, a, a_ad, b, b_ad, x, x_ad, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp), intent(inout) :: a_ad(n,n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(inout) :: b_ad(n)
    real(kind=dp), intent(inout) :: x(n)
    real(kind=dp), intent(inout) :: x_ad(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration
    
    real(kind=dp) :: h(n)
    integer :: j

    call cg( n, a, b, x, accuracy_goal, max_iteration )
    call cg( n, a, x_ad, h, accuracy_goal, max_iteration )
    b_ad = b_ad + h
    do j = 1, n
      a_ad(:,j) = a_ad(:,j) - h(:) * x(j)
    end do
    ! intentionally omitted x_ad = 0.0_dp
  end subroutine cg_symmetric_matrix_ad__dp

  subroutine cg_symmetric_matrix_tl__dp( n, a, a_tl, b, b_tl, x, x_tl, accuracy_goal, max_iteration )
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp), intent(in) :: a_tl(n,n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(in) :: b_tl(n)
    real(kind=dp), intent(inout) :: x(n)
    real(kind=dp), intent(inout) :: x_tl(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration

    call cg( n, a, b, x, accuracy_goal, max_iteration )
    call cg( n, a, b_tl - matmul( a_tl, x ), x_tl, accuracy_goal, max_iteration )
  end subroutine cg_symmetric_matrix_tl__dp

  !> @brief The preconditioned Conjugate Gradient iteration (Barret et al. 1994, Fig. 2.5)
  !! to solve the linear problem A x = b.
  !> @param[in] n The dimension of the problem.
  !> @param[in] a_matmul The (symmetric and positive definite) matrix A.
  !> @param[in] b The vector b.
  !> @param[out] x The solution vector x.
  !> @param[in] m_inv_matmul The inverse of a (symmetric) preconditioning matrix M.
  !> @param[in] accuracy_goal The accuracy goal.
  !> @param[in] max_iteration The maximum number of iterations.
  subroutine cg__dp( n, a_matmul, b, x, m_inv_matmul, accuracy_goal, max_iteration )
    interface
      !> @brief The product of a square matrix with a vector.
      !> @param[in] n The dimension of the square matrix.
      !> @param[in] x The vector.
      !> @return the product of the square matrix with the vector.
      function a_matmul( n, x ) result (y)
        use base_mod, only: dp
        
        integer, intent(in) :: n
        real(kind=dp), intent(in) :: x(n)
        real(kind=dp) :: y(n)
      end function

      !> @brief The product of the inverse preconditioning matrix with a vector.
      !> @param[in] n The dimension of the preconditioning matrix.
      !> @param[in] x The vector.
      !> @return the product of the preconditioning matrix with the vector.
      function m_inv_matmul( n, x ) result (y)
        use base_mod, only: dp
        
        integer, intent(in) :: n
        real(kind=dp), intent(in) :: x(n)
        real(kind=dp) :: y(n)
      end function
    end interface
    
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp), intent(out) :: x(n)
    real(kind=dp), intent(in) :: accuracy_goal
    integer, intent(in) :: max_iteration
    real(kind=dp) :: b_m
    real(kind=dp) :: r(n)
    real(kind=dp) :: p(n)
    real(kind=dp) :: q(n)
    real(kind=dp) :: z(n)
    real(kind=dp) :: alpha
    real(kind=dp) :: beta
    real(kind=dp) :: rho, rho_pred
    integer :: i
    
    x = m_inv_matmul( n, b )
    b_m = inner_product__dp( n, b, x )
    r = b - a_matmul( n, x )
    z = m_inv_matmul( n, r )
    rho = inner_product__dp( n, r, z )
    p = z
    i = 0
    
    do while (i /= max_iteration .and. rho > accuracy_goal**2 * b_m) ! stopping criterion Barret et al. (1994, Sect. 4.2.2)
      q = a_matmul( n, p )
      alpha = rho / inner_product__dp( n, p, q )
      x = x + alpha * p
      r = r - alpha * q
      z = m_inv_matmul( n, r )
      rho_pred = rho
      rho = inner_product__dp( n, r, z )
      beta = rho / rho_pred
      p = z + beta * p
      i = i + 1
    end do
    if (i > max_iteration) then
      error stop "linear_systems_mod::cg(): The maximum number of iterations is exceeded."
    end if
  end subroutine cg__dp

  pure function inner_product__dp( n, x, y ) result (z)
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: x(n)
    real(kind=dp), intent(in) :: y(n)
    real(kind=dp) :: z

    z = dot_product( x, y )
  end function inner_product__dp

end module solver_mod
    

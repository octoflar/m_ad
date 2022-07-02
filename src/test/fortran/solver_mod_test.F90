!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
module solver_mod_test
  use solver_mod
  use base_mod
  use test_mod
  use math_mod
  
  implicit none
  private
  
  public run_testsuite
  
  integer, parameter :: n = 100
  real(kind=wp), parameter :: EPS = 1.0E-07_wp
  integer, parameter :: max_iteration = 100

  real(kind=wp) :: a(n,n), a_tl(n,n)
  real(kind=wp) :: b(n), b_tl(n)
  real(kind=wp) :: x(n), x_ad(n)
  
contains
  
  !> @brief Directives run once, before all tests.
  subroutine before_all
    call test_initialize
  end subroutine before_all
  
  !> @brief Directives run once, after all tests.
  subroutine after_all
    ! intentionally do nothing
  end subroutine after_all
  
  !> @brief Directives run before each test.
  subroutine before
    call seed( 5409 )

    a = srandn( n, 0.1_wp ) + diag( n, 1.0_wp, real( n, wp ) )
    b = vrandn( n, 1.0_wp )
    x = 0.0_wp
    a_tl = srandn( n, 1.0_wp )
    b_tl = vrandn( n, 1.0_wp )
    x_ad = vrandn( n, 1.0_wp )
  end subroutine before
  
  !> @brief Directives run after each test.
  subroutine after
    ! intentionally do nothing
  end subroutine after
  
  !> @brief Tests the reference adjoint code against the reference tangent code.
  subroutine test_ad
    real(kind=wp) :: a_ad(n,n)
    real(kind=wp) :: b_ad(n)
    real(kind=wp) :: x_tl(n)
    
    a_ad = 0.0_wp
    b_ad = 0.0_wp
    call cg_ad( n, a, a_ad, b, b_ad, x, x_ad, EPS, max_iteration )
    call assert_array_equals( "test_ad", 0.0_wp, b - matmul( a, x ), EPS )

    call cg_tl( n, a, a_tl, b, b_tl, x, x_tl, EPS, max_iteration )    
    call assert_equals( "test_ad", 0.0_wp, dot_product( x_tl, x_ad ) - fp( n, a_tl, a_ad ) - dot_product( b_tl, b_ad ), EPS )
  end subroutine test_ad
    
  !> @brief Tests the reference tangent code against numeric differentiation.
  subroutine test_tl
    real(kind=wp) :: x_tl(n)
    real(kind=wp) :: x_fd(n)
    
    call cg_tl( n, a, a_tl, b, b_tl, x, x_tl, EPS, max_iteration )
    call assert_array_equals( "test_tl", 0.0_wp, b - matmul( a, x ), EPS )

    call cg_fd( n, a, a_tl, b, b_tl, x, x_fd, EPS, max_iteration )
    call assert_equal_arrays( "test_tl", x_fd, x_tl, EPS )

    contains

      subroutine cg_fd( n, a, a_tl, b, b_tl, x, x_tl, accuracy_goal, max_iteration )
        integer, intent(in) :: n
        real(kind=wp), intent(in) :: a(n,n)
        real(kind=wp), intent(in) :: a_tl(n,n)
        real(kind=wp), intent(in) :: b(n)
        real(kind=wp), intent(in) :: b_tl(n)
        real(kind=wp), intent(out) :: x(n) 
        real(kind=wp), intent(out) :: x_tl(n)
        real(kind=dp), intent(in) :: accuracy_goal
        integer, intent(in) :: max_iteration
        
        real(kind=wp) :: x1(n)
        real(kind=wp) :: x2(n)
  
        call cg( n, a, b, x, accuracy_goal, 100 )
        call cg( n, a - EPS * a_tl, b - EPS * b_tl, x1, accuracy_goal, max_iteration )
        call cg( n, a + EPS * a_tl, b + EPS * b_tl, x2, accuracy_goal, max_iteration )
        x_tl = (x2 - x1) / (2.0_wp * EPS)
      end subroutine cg_fd
  end subroutine test_tl
    
  !! @brief Add all your test cases here.
  subroutine run_all
    call run( test_tl )
    call run( test_ad )
  end subroutine run_all
    
  !> @brief Runs a test case.
  subroutine run( test )
    interface
    subroutine test
    end subroutine test
    end interface
    
    call before
    call test
    call after
  end subroutine run

  !> @brief Runs the testsuite.
  subroutine run_testsuite
    call before_all
    call run_all
    call after_all
  end subroutine run_testsuite
  
end module solver_mod_test
  
  
!> @brief Runs the testsuite.
program main
  use solver_mod_test, only: run_testsuite
  
  call run_testsuite
end program main

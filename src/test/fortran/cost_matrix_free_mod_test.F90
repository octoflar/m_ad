!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
module cost_matrix_free_mod_test
  use cost_matrix_free_mod
  use test_mod
  use math_mod
  
  implicit none
  private
  
  public run_testsuite
  
  integer, parameter :: n = 100
  
  real(kind=wp) :: x, x_tl
  real(kind=wp) :: b(n), b_tl(n)
  real(kind=wp) :: c, c_ad
  
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

    x = randn( 0.1_wp ) + 1.0_wp
    b = vrandn( n, 1.0_wp )
    c = 0.0_wp
    x_tl = randn( 1.0_wp )
    b_tl = vrandn( n, 1.0_wp )
    c_ad = randn( 1.0_wp )
  end subroutine before
  
  !> @brief Directives run after each test.
  subroutine after
    ! intentionally do nothing
  end subroutine after
  
  !> @brief Tests the reference adjoint code against the reference tangent code.
  subroutine test_ad
    real(kind=wp) :: x_ad
    real(kind=wp) :: b_ad(n)
    real(kind=wp) :: c_tl
    
    x_ad = 0.0_wp
    b_ad = 0.0_wp
    call op_ad( n, x, x_ad, b, b_ad, c, c_ad )
    call op_tl( n, x, x_tl, b, b_tl, c, c_tl )
    
    call assert_equals( "test_ad", 0.0_wp, c_tl * c_ad - x_tl * x_ad - dot_product( b_tl, b_ad ), EPS )
  end subroutine test_ad

  !> @brief Tests the reference tangent code against numeric differentiation.
  subroutine test_tl
    real(kind=wp) :: c_tl
    real(kind=wp) :: c_fd
    
    call op_tl( n, x, x_tl, b, b_tl, c, c_tl )
    call op_fd( n, x, x_tl, b, b_tl, c, c_fd )
    
    call assert_equals( "test_tl", c_fd, c_tl, EPS )
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
  
end module cost_matrix_free_mod_test
  
  
!> @brief Runs the testsuite.
program main
  use cost_matrix_free_mod_test, only: run_testsuite
    
  call run_testsuite
end program main
  

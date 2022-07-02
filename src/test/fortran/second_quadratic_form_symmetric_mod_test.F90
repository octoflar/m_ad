!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
module second_quadratic_form_symmetric_mod_test
  use second_quadratic_form_symmetric_mod
  use test_mod
  use math_mod
  
  implicit none
  private
  
  public run_testsuite
  
  integer, parameter :: n = 100
  
  real(kind=wp) :: a(n,n), a_tl(n,n)
  real(kind=wp) :: b(n,n), b_tl(n,n)
  real(kind=wp) :: c(n,n), c_ad(n,n)
  
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
    b = mrandn( n, 1.0_wp )
    c = 0.0_wp
    a_tl = srandn( n, 1.0_wp )
    b_tl = mrandn( n, 1.0_wp )
    c_ad = srandn( n, 1.0_wp )
  end subroutine before
  
  !> @brief Directives run after each test.
  subroutine after
    ! intentionally do nothing
  end subroutine after
  
  !> @brief Tests the reference adjoint code against the reference tangent code.
  subroutine test_ad
    real(kind=wp) :: a_ad(n,n)
    real(kind=wp) :: b_ad(n,n)
    real(kind=wp) :: c_tl(n,n)
    
    a_ad = 0.0_wp
    b_ad = 0.0_wp
    call op_ad( n, a, a_ad, b, b_ad, c, c_ad )
    call op_tl( n, a, a_tl, b, b_tl, c, c_tl )
    
    call assert_equals( "test_ad", 0.0_wp, fp( n, c_tl, c_ad ) - fp( n, a_tl, a_ad ) - fp( n, b_tl, b_ad ), EPS )
  end subroutine test_ad
  
  !> @brief Tests the reference tangent code against numeric differentiation.
  subroutine test_tl
    real(kind=wp) :: c_tl(n,n)
    real(kind=wp) :: c_fd(n,n)
    
    call op_tl( n, a, a_tl, b, b_tl, c, c_tl )
    call op_fd( n, a, a_tl, b, b_tl, c, c_fd )
    
    call assert_equal_arrays( "test_tl", c_fd, c_tl, EPS )
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
  
end module second_quadratic_form_symmetric_mod_test
  
  
!> @brief Runs the testsuite.
program main
  use second_quadratic_form_symmetric_mod_test, only: run_testsuite
  
  call run_testsuite
end program main
  

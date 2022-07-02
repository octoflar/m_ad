!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Module to write tests in the style of [JUnit](http://junit.org/)
module test_mod
  implicit none

  private

  public assert_equals, assert_not_equals
  public assert_true, assert_false
  public assert_nan, assert_not_nan
  public assert_array_equals
  public assert_equal_arrays

  public test_initialize
  public test_finalize
  public test_set_error_stop_on_failure

  interface assert_equals
    module procedure assert_equals_char, assert_equals_real__dp, assert_equals_real__sp, assert_equals_int, assert_equivalent
  end interface

  interface assert_not_equals
    module procedure assert_not_equals_real__dp, assert_not_equals_real__sp, assert_not_equals_int, assert_not_equivalent
  end interface

  interface assert_nan
    module procedure assert_nan__dp, assert_nan__sp
  end interface

  interface assert_not_nan
    module procedure assert_not_nan__dp, assert_not_nan__sp
  end interface

  interface is_nan
    module procedure is_nan__dp, is_nan__sp
  end interface

  interface assert_array_equals
    module procedure assert_array_1D_equals_int, assert_array_1D_equals_real__dp, assert_array_1D_equals_real__sp
    module procedure assert_array_2D_equals_int, assert_array_2D_equals_real__dp, assert_array_2D_equals_real__sp
  end interface

  interface assert_equal_arrays
    module procedure assert_equal_arrays_1D_int, assert_equal_arrays_1D_real__dp, assert_equal_arrays_1D_real__sp
    module procedure assert_equal_arrays_2D_int, assert_equal_arrays_2D_real__dp, assert_equal_arrays_2D_real__sp
  end interface

  logical, parameter :: DEFAULT_ERROR_STOP_ON_FAILURE = .true.
  logical, parameter :: DEFAULT_MESSAGE_ON_SUCCESS    = .true.
  logical, parameter :: DEFAULT_MESSAGE_ON_FAILURE    = .true.
  integer, parameter :: DEFAULT_FAILURE_OUTPUT_UNIT   = 0
  integer, parameter :: DEFAULT_SUCCESS_OUTPUT_UNIT   = 0

  logical :: error_stop_on_failure = DEFAULT_ERROR_STOP_ON_FAILURE
  logical :: message_on_success    = DEFAULT_MESSAGE_ON_SUCCESS
  logical :: message_on_failure    = DEFAULT_MESSAGE_ON_FAILURE
  integer :: failure_output_unit   = DEFAULT_FAILURE_OUTPUT_UNIT
  integer :: success_output_unit   = DEFAULT_SUCCESS_OUTPUT_UNIT

  namelist /config/        &
    error_stop_on_failure, &
    message_on_success,    &
    message_on_failure,    &
    failure_output_unit,   &
    success_output_unit

contains

  !> @brief Initializes the module with properties read from 'utest.par', if present.
  subroutine test_initialize
    use, intrinsic :: iso_fortran_env, only: out => output_unit, err => error_unit

    logical                     :: ok
    integer,          parameter :: UNIT = 12
    character(len=*), parameter :: CONFIG_FILE = 'utest.par'

    call test_finalize

    inquire( file=CONFIG_FILE, exist=ok )
    if (ok) then
      open ( UNIT, file=CONFIG_FILE )
      read ( UNIT, nml=config )
      close( UNIT )
    end if
    select case (failure_output_unit)
      case (0)
        failure_output_unit = out
      case (1)
        failure_output_unit = err
    end select
    select case (success_output_unit)
      case (0)
        success_output_unit = out
      case (1)
        success_output_unit = err
    end select
  end subroutine test_initialize

  !> @brief Finalizes the module.
  subroutine test_finalize
    error_stop_on_failure = DEFAULT_ERROR_STOP_ON_FAILURE
    message_on_success    = DEFAULT_MESSAGE_ON_SUCCESS
    message_on_failure    = DEFAULT_MESSAGE_ON_FAILURE
    failure_output_unit   = DEFAULT_FAILURE_OUTPUT_UNIT
    success_output_unit   = DEFAULT_SUCCESS_OUTPUT_UNIT
  end subroutine test_finalize

  !> @brief Asserts that a logical expression is '.true.'.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value of the logical expression.
  subroutine assert_true( test, actual )
    character(len=*), intent(in) :: test
    logical, intent(in) :: actual

    if (.not. actual) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", .true.
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_true

  !> @brief Asserts that a logical expression is '.false.'.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value of the logical expression.
  subroutine assert_false( test, actual )
    character(len=*), intent(in) :: test
    logical, intent(in) :: actual

    if (actual) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", .false.
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_false

  !> @brief Asserts that two logical expressions are equivalent.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value of the logical expression.
  !> @param[in] actual The actual value of the logical expression.
  subroutine assert_equivalent( test, expected, actual )
    character(len=*), intent(in) :: test
    logical, intent(in) :: expected
    logical, intent(in) :: actual

    if (actual .neqv. expected) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equivalent

  !> @brief Asserts that two logical expressions are not equivalent.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value of the logical expression.
  !> @param[in] actual The actual value of the logical expression.
  subroutine assert_not_equivalent( test, expected, actual )
    character(len=*), intent(in) :: test
    logical, intent(in) :: expected
    logical, intent(in) :: actual

    if (actual .eqv. expected) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equivalent to ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_equivalent

  !> @brief Asserts that two integral numbers are equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  subroutine assert_equals_char( test, expected, actual )
    character(len=*), intent(in) :: test
    character(len=*), intent(in) :: expected
    character(len=*), intent(in) :: actual

    if (actual /= expected) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equals_char

  !> @brief Asserts that two integral numbers are equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  subroutine assert_equals_int( test, expected, actual )
    character(len=*), intent(in) :: test
    integer, intent(in) :: expected
    integer, intent(in) :: actual

    if (actual /= expected) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equals_int

  !> @brief Asserts that two integral numbers are not equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  subroutine assert_not_equals_int( test, expected, actual )
    character(len=*), intent(in) :: test
    integer, intent(in) :: expected
    integer, intent(in) :: actual

    if (actual == expected) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equal to ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_equals_int

  !> @brief Asserts that two real numbers are equal within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equals_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected
    real(kind=dp), intent(in) :: actual
    real(kind=dp), intent(in) :: tolerance

    if (abs( actual - expected ) > tolerance) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
        write(failure_output_unit, *) "   Expected tolerance: ", tolerance
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equals_real__dp

  !> @brief Asserts that two real numbers are equal within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equals_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected
    real(kind=sp), intent(in) :: actual
    real(kind=sp), intent(in) :: tolerance

    if (abs( actual - expected ) > tolerance) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
        write(failure_output_unit, *) "   Expected tolerance: ", tolerance
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equals_real__sp

  !> @brief Asserts that two real numbers are not equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_not_equals_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected
    real(kind=dp), intent(in) :: actual
    real(kind=dp), intent(in) :: tolerance

    if (abs( actual - expected ) < tolerance) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equal to ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
        write(failure_output_unit, *) "   Expected tolerance: ", tolerance
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_equals_real__dp

  !> @brief Asserts that two real numbers are not equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual value.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_not_equals_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected
    real(kind=sp), intent(in) :: actual
    real(kind=sp), intent(in) :: tolerance

    if (abs( actual - expected ) < tolerance) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equal to ", expected
        write(failure_output_unit, *) "   Actual   result: ", actual
        write(failure_output_unit, *) "   Expected tolerance: ", tolerance
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_equals_real__sp

  !> @brief Asserts that a real number is NaN.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value.
  subroutine assert_nan__dp( test, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: actual

    if (.not. is_nan(actual)) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", "NaN"
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_nan__dp

  !> @brief Asserts that a real number is NaN.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value.
  subroutine assert_nan__sp( test, actual )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: actual

    if (.not. is_nan(actual)) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: ", "NaN"
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_nan__sp

  !> @brief Asserts that a real number is not NaN.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value.
  subroutine assert_not_nan__dp( test, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: actual

    if (is_nan(actual)) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equal to ", "NaN"
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_nan__dp

  !> @brief Asserts that a real number is not NaN.
  !> @param[in] test The name of the caller.
  !> @param[in] actual The actual value.
  subroutine assert_not_nan__sp( test, actual )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: actual

    if (is_nan(actual)) then
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed"
        write(failure_output_unit, *) "   Expected result: not equal to ", "NaN"
        write(failure_output_unit, *) "   Actual   result: ", actual
      end if
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_not_nan__sp

  !> @brief Asserts that all integral numbers are equal to an expected value.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  subroutine assert_array_1D_equals_int( test, expected, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    integer, intent(in) :: expected
    integer, intent(in) :: actual(:)
    integer             :: i
    logical             :: failure = .false.

    do i = 1, size( actual )
      if (actual(i) /= expected) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_1D_equals_int

  !> @brief Asserts that all real numbers are equal to an expected value, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_array_1D_equals_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected
    real(kind=dp), intent(in) :: actual(:)
    real(kind=dp), intent(in) :: tolerance
    integer                   :: i
    logical                   :: failure = .false.

    do i = 1, size( actual )
      if (abs( actual(i) - expected ) > tolerance) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
          write(failure_output_unit, *) "   Expected tolerance: ", tolerance
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_1D_equals_real__dp

  !> @brief Asserts that all real numbers are equal to an expected value, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_array_1D_equals_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected
    real(kind=sp), intent(in) :: actual(:)
    real(kind=sp), intent(in) :: tolerance
    integer                   :: i
    logical                   :: failure = .false.

    do i = 1, size( actual )
      if (abs( actual(i) - expected ) > tolerance) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
          write(failure_output_unit, *) "   Expected tolerance: ", tolerance
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_1D_equals_real__sp

  !> @brief Asserts that all integral numbers are equal to an expected value.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  subroutine assert_array_2D_equals_int( test, expected, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    integer, intent(in) :: expected
    integer, intent(in) :: actual(:,:)
    integer             :: i, j
    logical             :: failure = .false.

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (actual(i,j) /= expected) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_2D_equals_int

  !> @brief Asserts that all real numbers are equal to an expected value, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_array_2D_equals_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected
    real(kind=dp), intent(in) :: actual(:,:)
    real(kind=dp), intent(in) :: tolerance
    integer                   :: i, j
    logical                   :: failure = .false.

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (abs( actual(i,j) - expected ) > tolerance) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
            write(failure_output_unit, *) "   Expected tolerance: ", tolerance
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_2D_equals_real__dp

  !> @brief Asserts that all real numbers are equal to an expected value, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected value.
  !> @param[in] actual The actual values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_array_2D_equals_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected
    real(kind=sp), intent(in) :: actual(:,:)
    real(kind=sp), intent(in) :: tolerance
    integer                   :: i, j
    logical                   :: failure = .false.

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (abs( actual(i,j) - expected ) > tolerance) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
            write(failure_output_unit, *) "   Expected tolerance: ", tolerance
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_array_2D_equals_real__sp

  !> @brief Asserts that two integral arrays are equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected array values.
  !> @param[in] actual The actual array values.
  subroutine assert_equal_arrays_1D_int( test, expected, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    integer, intent(in) :: expected(:)
    integer, intent(in) :: actual(:)
    integer             :: i
    logical             :: failure = .false.

    if (size( actual ) /= size( expected )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size: ", size( expected )
        write(failure_output_unit, *) "   Actual   size: ", size( actual )
      end if
    end if

    do i = 1, size( actual, 1 )
      if (actual(i) /= expected(i)) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected(i)
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_1D_int

  !> @brief Asserts that two real arrays are equal, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected array values.
  !> @param[in] actual The actual array values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equal_arrays_1D_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected(:)
    real(kind=dp), intent(in) :: actual(:)
    real(kind=dp), intent(in) :: tolerance
    integer                   :: i
    logical                   :: failure = .false.

    if (size( actual ) /= size( expected )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size: ", size( expected )
        write(failure_output_unit, *) "   Actual   size: ", size( actual )
      end if
    end if

    do i = 1, size( actual, 1 )
      if (abs( actual(i) - expected(i) ) > tolerance) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected(i)
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_1D_real__dp

  !> @brief Asserts that two real arrays are equal, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected array values.
  !> @param[in] actual The actual array values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equal_arrays_1D_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected(:)
    real(kind=sp), intent(in) :: actual(:)
    real(kind=sp), intent(in) :: tolerance
    integer                   :: i
    logical                   :: failure = .false.

    if (size( actual ) /= size( expected )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size: ", size( expected )
        write(failure_output_unit, *) "   Actual   size: ", size( actual )
      end if
    end if

    do i = 1, size( actual, 1 )
      if (abs( actual(i) - expected(i) ) > tolerance) then
        failure = .true.
        if (message_on_failure) then
          write(failure_output_unit, *) test, ": Assertion failed at index ", i
          write(failure_output_unit, *) "   Expected result: ", expected(i)
          write(failure_output_unit, *) "   Actual   result: ", actual(i)
        end if
      end if
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_1D_real__sp

  !> @brief Asserts that two integral matrices are equal.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected matrix values.
  !> @param[in] actual The actual matrix values.
  subroutine assert_equal_arrays_2D_int( test, expected, actual )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    integer, intent(in) :: expected(:,:)
    integer, intent(in) :: actual(:,:)
    integer             :: i, j
    logical             :: failure = .false.

    if (size( actual, 1 ) /= size( expected, 1 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(1): ", size( expected, 1 )
        write(failure_output_unit, *) "   Actual   size(1): ", size( actual, 1 )
      end if
    end if

    if (size( actual, 2 ) /= size( expected, 2 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(2): ", size( expected, 2 )
        write(failure_output_unit, *) "   Actual   size(2): ", size( actual, 2 )
      end if
    end if

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (actual(i,j) /= expected(i,j)) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected(i,j)
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_2D_int

  !> @brief Asserts that two real matrices are equal, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected matrix values.
  !> @param[in] actual The actual matrix values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equal_arrays_2D_real__dp( test, expected, actual, tolerance )
    use base_mod, only: dp

    character(len=*), intent(in) :: test
    real(kind=dp), intent(in) :: expected(:,:)
    real(kind=dp), intent(in) :: actual(:,:)
    real(kind=dp), intent(in) :: tolerance
    integer                   :: i, j
    logical                   :: failure = .false.

    if (size( actual, 1 ) /= size( expected, 1 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(1): ", size( expected, 1 )
        write(failure_output_unit, *) "   Actual   size(1): ", size( actual, 1 )
      end if
    end if

    if (size( actual, 2 ) /= size( expected, 2 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(2): ", size( expected, 2 )
        write(failure_output_unit, *) "   Actual   size(2): ", size( actual, 2 )
      end if
    end if

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (abs( actual(i,j) - expected(i,j) ) > tolerance) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected(i,j)
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_2D_real__dp

  !> @brief Asserts that two real matrices are equal, within a given tolerance.
  !> @param[in] test The name of the caller.
  !> @param[in] expected The expected matrix values.
  !> @param[in] actual The actual matrix values.
  !> @param[in] tolerance The absolute tolerance.
  subroutine assert_equal_arrays_2D_real__sp( test, expected, actual, tolerance )
    use base_mod, only: sp

    character(len=*), intent(in) :: test
    real(kind=sp), intent(in) :: expected(:,:)
    real(kind=sp), intent(in) :: actual(:,:)
    real(kind=sp), intent(in) :: tolerance
    integer                   :: i, j
    logical                   :: failure = .false.

    if (size( actual, 1 ) /= size( expected, 1 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(1): ", size( expected, 1 )
        write(failure_output_unit, *) "   Actual   size(1): ", size( actual, 1 )
      end if
    end if

    if (size( actual, 2 ) /= size( expected, 2 )) then
      failure = .true.
      if (message_on_failure) then
        write(failure_output_unit, *) test, ": Assertion failed:"
        write(failure_output_unit, *) "   Expected size(2): ", size( expected, 2 )
        write(failure_output_unit, *) "   Actual   size(2): ", size( actual, 2 )
      end if
    end if

    do j = 1, size( actual, 2 )
      do i = 1, size( actual, 1 )
        if (abs( actual(i,j) - expected(i,j) ) > tolerance) then
          failure = .true.
          if (message_on_failure) then
            write(failure_output_unit, *) test, ": Assertion failed at index (", i, ", ", j, ")"
            write(failure_output_unit, *) "   Expected result: ", expected(i,j)
            write(failure_output_unit, *) "   Actual   result: ", actual(i,j)
          end if
        end if
      end do
    end do
    if (failure) then
      call conditional_error_stop
    else
      call conditional_success_message( test )
    end if
  end subroutine assert_equal_arrays_2D_real__sp

  !> @brief Sets the error-stop-on-failure property to a new value.
  !> @param[in] b The new property value.
  subroutine test_set_error_stop_on_failure( b )
    logical, intent(in) :: b

    error_stop_on_failure = b
  end subroutine test_set_error_stop_on_failure

  pure function is_nan__dp( x ) result (nan)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use base_mod, only: dp

    real(kind=dp), intent(in) :: x
    logical                   :: nan

    nan = ieee_is_nan( x )
  end function is_nan__dp

  pure function is_nan__sp( x ) result (nan)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use base_mod, only: sp

    real(kind=sp), intent(in) :: x
    logical                   :: nan

    nan = ieee_is_nan( x )
  end function is_nan__sp

  subroutine conditional_error_stop
    if (error_stop_on_failure) then
      error stop "Unit test failed"
    end if
  end subroutine

  subroutine conditional_success_message( test )
    character(len=*), intent(in) :: test

    if (message_on_success) then
      write(success_output_unit, *) test, ": Assertion passed"
    end if
  end subroutine conditional_success_message

end module test_mod

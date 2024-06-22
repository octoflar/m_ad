!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Mathematical operations similar to Matlab functions.
module math_mod
  use base_mod, only: dp

  implicit none

  private

  !! Matrix and vector operations
  public :: diag
  public :: fp
  public :: outer_product
  public :: tr
  !! Random numbers
  public :: randn
  public :: drandn
  public :: mrandn
  public :: srandn
  public :: vrandn
  public :: seed

  interface diag
    module procedure new_diagonal_matrix_from_range__dp
    module procedure new_diagonal_matrix_from_scalar__dp
    module procedure new_diagonal_matrix_from_vector__dp
    module procedure new_diagonal_matrix_from_matrix__dp
  end interface

  interface fp
    module procedure frobenius_inner_product__dp
  end interface

  interface outer_product
    module procedure outer_product__dp
  end interface

  interface tr
    module procedure trace__dp
  end interface

  interface drandn
    module procedure new_diagonal_matrix_from_random_numbers_normal__dp
  end interface

  interface mrandn
    module procedure new_square_matrix_from_random_numbers_normal__dp
  end interface

  interface srandn
    module procedure new_symmetric_matrix_from_random_numbers_normal__dp
  end interface

  interface vrandn
    module procedure new_vector_from_random_numbers_normal__dp
  end interface

  interface randn
    module procedure new_random_number_normal__dp
  end interface

  interface seed
    module procedure set_random_seed
  end interface

contains

pure function new_diagonal_matrix_from_range__dp( n, first, last ) result (matrix) ! LCOV_EXCL_LINE
  integer,       intent(in) :: n
  real(kind=dp), intent(in) :: first, last
  real(kind=dp)             :: matrix(n,n)
  integer                   :: i
  real(kind=dp)             :: incr

  incr = (last - first) / (n - 1)

  matrix = 0.0_dp
  do concurrent (i = 1:n)
    matrix(i,i) = first + (i - 1) * incr
  end do
end function new_diagonal_matrix_from_range__dp

pure function new_diagonal_matrix_from_scalar__dp( n, d ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp)             :: matrix(n,n)
    integer                   :: i

    matrix = 0.0_dp
    do concurrent (i = 1:n)
      matrix(i,i) = d
    end do
  end function new_diagonal_matrix_from_scalar__dp

  pure function new_diagonal_matrix_from_vector__dp( n, d ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d(n)
    real(kind=dp)             :: matrix(n,n)
    integer                   :: i

    matrix = 0.0_dp
    do concurrent (i = 1:n)
      matrix(i,i) = d(i)
    end do
  end function new_diagonal_matrix_from_vector__dp

  pure function new_diagonal_matrix_from_matrix__dp( n, a ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp)             :: matrix(n,n)
    integer                   :: i

    matrix = 0.0_dp
    do concurrent (i = 1:n)
      matrix(i,i) = a(i,i)
    end do
  end function new_diagonal_matrix_from_matrix__dp
  
  function new_square_matrix_from_random_numbers_normal__dp( n, d ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp)             :: matrix(n,n)

    call get_random_matrix_normal__dp( n, d, matrix )
  end function new_square_matrix_from_random_numbers_normal__dp

  function new_diagonal_matrix_from_random_numbers_normal__dp( n, d ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp)             :: matrix(n,n)
    real(kind=dp)             :: vector(n)

    call get_random_vector_normal__dp( n, d, vector )
    matrix = new_diagonal_matrix_from_vector__dp( n, vector ) ! LCOV_EXCL_LINE
  end function new_diagonal_matrix_from_random_numbers_normal__dp

  function new_symmetric_matrix_from_random_numbers_normal__dp( n, d ) result (matrix) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp)             :: matrix(n,n)

    call get_random_matrix_normal__dp( n, d, matrix )
    matrix = 0.5_dp * (matrix + transpose( matrix )) ! LCOV_EXCL_LINE
  end function new_symmetric_matrix_from_random_numbers_normal__dp

  function new_random_number_normal__dp( d ) result (number) ! LCOV_EXCL_LINE
    real(kind=dp), intent(in) :: d
    real(kind=dp) :: number
    real(kind=dp) :: vector(2) 
    real(kind=dp), parameter :: square_root_of_two = sqrt( 2.0_dp )

    call get_random_vector_normal__dp( 2, square_root_of_two * d, vector )
    number = sum( vector )
  end function new_random_number_normal__dp

  function new_vector_from_random_numbers_normal__dp( n, d ) result (vector) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp)             :: vector(n)

    call get_random_vector_normal__dp( n, d, vector )
  end function new_vector_from_random_numbers_normal__dp

  subroutine get_random_vector_normal__dp( n, d, z ) ! LCOV_EXCL_LINE
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp), intent(out) :: z(n)
    real(kind=dp) :: t, x, y
    integer :: i

    do i = 1, n, 2
      do
        call random_number( x )
        call random_number( y )
        x = 2.0_dp * x - 1.0_dp
        y = 2.0_dp * y - 1.0_dp
        t = x**2 + y**2
        if (t > 0.0_dp .and. t < 1.0_dp) then
          exit
        end if
      end do

      t = sqrt( -2.0_dp * (log( t ) / t) )
      x = x * t
      y = y * t

      ! both x and y are normal deviates
      z(i) = x
      if (i < n) then
        z(i + 1) = y
      end if
    end do
    z = z * d
  end subroutine get_random_vector_normal__dp

  subroutine get_random_matrix_normal__dp( n, d, z ) ! LCOV_EXCL_LINE
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: d
    real(kind=dp), intent(out) :: z(n,n)
    integer :: j

    do j = 1, n
      call get_random_vector_normal__dp( n, d, z(:,j) )
    end do
  end subroutine get_random_matrix_normal__dp

  !> @brief Initializes the intrinsic random number generator.
  !> @param[in] seed The seed.
  subroutine set_random_seed( seed )
    integer, intent(in)  :: seed
    integer              :: i
    integer, parameter   :: m = 1812433253
    integer              :: n
    integer, allocatable :: seeds(:)

    call random_seed( size=n )
    allocate (seeds(n))
    seeds(1) = seed
    do i = 2, n
      seeds(i) = m * seeds(i - 1)
    end do
    call random_seed( put=seeds )
    deallocate (seeds)
  end subroutine set_random_seed

  function frobenius_inner_product__dp( n, a, b ) result (frobenius_inner_product) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp), intent(in) :: b(n,n)
    real(kind=dp) :: frobenius_inner_product

    frobenius_inner_product = trace__dp( n, matmul( transpose( a ), b ) )
  end function frobenius_inner_product__dp

  function outer_product__dp( n, a, b ) result (outer_product) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: a(n)
    real(kind=dp), intent(in) :: b(n)
    real(kind=dp) :: outer_product(n,n)
    integer :: j

    do concurrent (j = 1:n)
      outer_product(:,j) = a(:) * b(j)
    end do
  end function outer_product__dp

  function trace__dp( n, a ) result (trace) ! LCOV_EXCL_LINE
    integer,       intent(in) :: n
    real(kind=dp), intent(in) :: a(n,n)
    real(kind=dp) :: trace
    integer :: i

    trace = 0.0_dp
    do concurrent (i = 1:n)
      trace = trace + a(i,i)
    end do
  end function trace__dp

end module math_mod

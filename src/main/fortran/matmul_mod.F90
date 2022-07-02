!> @author Ralf Quast
!> @date 2022
!> @copyright MIT License
!> @brief Dummy matrix-free matrix-vector products.
module matmul_mod
  implicit none
  
contains
  
  subroutine a_matmul( n, x, b, c )
    use base_mod, only: wp
    
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: c(n)
    integer :: i, j
    
    c = 0.0_wp
    do i = 1, n
      do j = 1, n
        if (i == j) then
          c(i) = c(i) + (x * b(j)) * i
        else
          c(i) = c(i) + (x * b(j)) * 0.1_wp
        end if
      end do
    end do
  end subroutine a_matmul

  subroutine m_inv_matmul( n, x, b, c )
    use base_mod, only: wp
    
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: c(n)
    integer :: i
    
    do i = 1, n
      c(i) = b(i) / (x * i)
    end do
  end subroutine m_inv_matmul

  subroutine a_matmul_xad( n, x, x_xad, b, c, c_xad )
    use base_mod, only : wp

    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(inout) :: x_xad
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(out) :: c(n)
    real(kind=wp), intent(in) :: c_xad(n)
    integer :: i
    integer :: j
    
    do i = 1, n
      do j = n, 1, -1
        if (i == j) then
          x_xad = x_xad + c_xad(i) * b(j) * i
        else
          x_xad = x_xad + 0.1_wp * c_xad(i) * b(j)
        endif
      end do
    end do  
  end subroutine a_matmul_xad
    
  subroutine a_matmul_xtl( n, x, x_xtl, b, c, c_xtl )
    use base_mod, only : wp
    
    integer, intent(in) :: n
    real(kind=wp), intent(in) :: x
    real(kind=wp), intent(in) :: x_xtl
    real(kind=wp), intent(in) :: b(n)
    real(kind=wp), intent(inout) :: c(n)
    real(kind=wp), intent(inout) :: c_xtl(n)
    integer :: i
    integer :: j
    
    c_xtl = 0.0_wp
    c = 0.0_wp
    do i = 1, n
      do j = 1, n
        if (i == j) then
          c_xtl(i) = c_xtl(i) + x_xtl * b(j) * i
          c(i) = c(i) + x * b(j) * i
        else
          c_xtl(i) = c_xtl(i) + 0.1_wp * x_xtl * b(j)
          c(i) = c(i) + 0.1_wp * x * b(j)
        endif
      end do
    end do
    
  end subroutine a_matmul_xtl
    
end module matmul_mod

module readmatrices

implicit none 

integer, parameter :: dp = selected_real_kind(15,300)
integer :: i,j  

contains 

 function readmatA(n) result(A)
 real(kind=dp), dimension(:,:), allocatable :: A
 integer, intent(in) :: n

 allocate(A(n,n))


 do i = 1,n
  do j = 1,n
      A(i,j) = 0.5_dp * i ** 2.0_dp / j + 2.3_dp
  end do
 end do

 end function readmatA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 function readmatB(n) result(B) 
 real(kind=dp), dimension(:,:), allocatable :: B
 integer, intent(in) :: n

 allocate(B(n,n))


 do i = 1,n
  do j = 1,n
      B(i,j) = 0.3_dp * i ** 3.0_dp / j + 1.4_dp
  end do
 end do
 
 end function readmatB

end module readmatrices 

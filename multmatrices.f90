module multmatrices
use readmatrices
implicit none 



contains 

  function matrixmul(A,B,n) result(U)
   integer, intent(in) :: n
   real(kind=dp), dimension(n,n) :: A
   real(kind=dp), dimension(n,n) :: B
   real(kind=dp), dimension(:,:), allocatable :: U
       allocate(U(n,n))
        U = matmul(A,B)
  end function matrixmul







end module multmatrices 

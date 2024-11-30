module functions
 interface
      function mult(n,p,q,a,b) result(c)
         integer (kind=4), intent(in)::n,p,q
         real (kind=8), intent(in)::a(n,p), b(p,q)
         real (kind=8)::c(n,q)
         integer (kind=4)::i,j,k
      end function

 end interface
end module functions


program matrixmult
   use functions
   implicit none
   integer (kind=4), parameter::n=5, p=3, q=4
   real (kind=8)::a(n,p),b(p,q),c(n,q)
   integer (kind=4)::i,j,k

   do i=1,n
     do j=1,p
       a(i,j)= i+j
     end do
   end do

   do i=1,p
     do j=1,q
       b(i,j)=i-j
     end do
   end do

   c=0.0

   do i=1,n
     do j=1,q
       do k=1,p
          c(i,j)= c(i,j)+a(i,k)*b(k,j)
       end do
     end do
   end do
  
   c = mult(n,p,q,a,b)
   write(6,*)'Matrix C'
   do i=1,n
      do j=1,q
         write(6,'(f3.0)', advance='no') c(i,j)
      end do
      write(6,*)
   end do


end program matrixmult

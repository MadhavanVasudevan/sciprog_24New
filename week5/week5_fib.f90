! THIS IS THE MAIN PROGRAM



program fibonacci
   ! declaration of variables
   implicit none     
   integer (kind=4):: i,j,n
   real (kind=4)::n0, n1
   real (kind=4)::x(100)
   !entering the pairs of (x,y) data
   write(6,*) 'please enter the value of n'
   read *,n
   ! stopping the program if n exceeds our array sizes
   if(n>100) then
     write(6,*) 'Number of data points must be less than 100'
     stop
   end if

   n0=0
   n1=1
   x(1)=n0
   x(2)=n1
   write(6,*)'fibonacci series=',x(1),',',x(2) 
   do i=3,n
   
    call fibo_calc(x(i-2),x(i-1),x(i))
    write(6,*)',',x(i)
   end do
end program fibonacci

subroutine fibo_calc(arg1,arg2,arg3)
        real (kind=4),intent(in)::arg1
        real (kind=4),intent(in)::arg2
        real (kind=4),intent(out)::arg3
        arg3= arg1+arg2
end subroutine fibo_calc



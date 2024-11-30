! THIS IS THE MAIN PROGRAM

module global_access
        real (kind=4)::sum1
end module global_access


program week4
   implicit none     
   integer (kind=4):: i,n
   real (kind=4)::a1,b1,a2,b2,sum1
   n=12
   !real :: a,b, sum1
   a1 =0
   b1 =60.0
   call degtorad(a1,a2)
   call degtorad(b1,b2)
   write(6,*) 'a2=', a2
   write(6,*) 'b2=', b2

   call integral_calc(a2,b2,n,sum1)
   WRITE(6,*) 'sum =', sum1






end program week4


subroutine integral_calc(a2,b2,n,sum1)
   real (kind=4),intent(in)::a2
   real (kind=4),intent(in)::b2
   real (kind=4),intent(out)::sum1
   real (kind=4):: angles(12)
   real (kind=4)::  tan_angles(12)
   
   sum1 =0
   
   ! create a series of x values
   do i=1,n
     angles(i) = (b2-a2)*(i-1)/(n-1)
     tan_angles(i) = tan(angles(i))
     if (i==1) then
             sum1= sum1+ tan_angles(i)
     else if (i==n) then
             sum1 = sum1+ tan_angles(i)
     else
             sum1 = sum1+ 2*tan_angles(i)
     endif
   end do
   
   sum1 = sum1*(b2-a2)/(2*n)

end subroutine  integral_calc





subroutine degtorad(arg,arg2)
        real (kind=4),intent(in):: arg
        real (kind=4),intent(out):: arg2
        arg2 = 3.1415927*arg/180
end subroutine degtorad 



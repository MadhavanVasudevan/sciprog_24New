! THIS IS THE MAIN PROGRAM
program week3
   implicit none     
   integer (kind=4):: i,n
   real (kind=4)::a,b,sum1
   n=1000
   !real :: a,b, sum1
   a =0
   b =22.0/21.0
   sum1 =0
   ! create a series of x values
   do i=1,n
     if (i==1) then
             sum1= sum1+ tan(a)
     else if (i==n) then
             sum1 = sum1+ tan(b)
     else
             sum1 = sum1+ 2*tan((b-a)*(i-1)/(n-1))
     endif
   end do
   
   sum1 = sum1*(b-a)/(2*n)

   WRITE(6,*) 'sum =', sum1
end program  week3

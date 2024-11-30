! THIS IS THE MAIN PROGRAM
program calcs
   ! declaration of variables
   implicit none     
   integer (kind=4):: i
   real (kind=4)::x(200)
   real (kind=4)::result1,result2
   real (kind=4)::xval=-0.9
   real (kind=4)::delta=0.1
   do while(xval.le.0.9)
     call arctan1(xval,delta,result1)
     call arctan2(xval,result2)
     write(6,*)'result1=',result1,'result2=',result2
     xval=xval+0.01
   end do
end program calcs 

subroutine arctan1(x,delta,sum_total)
        real (kind=4),intent(in)::x
        real (kind=4),intent(in)::delta
        real (kind=4),intent(out)::sum_total
        real (kind=4)::n
        real (kind=4)::sum1=100
        sum_total=0.0
        n=1.0
        do while(sum1.gt.delta)
         sum1= (x**(2.0*n+1.0))/((2.0*n)+1.0)
         sum_total=sum_total+sum1
         n=n+1
        end do
end subroutine arctan1

subroutine arctan2(x,resultx)
        real (kind=4),intent(in)::x
        real (kind=4),intent(out)::resultx
        resultx = (0.5)*(log(1.0+x)-log(1.0-x)) 
end subroutine arctan2



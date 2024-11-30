module gcdfunctions
   implicit none
   contains
   function iterativeGCD(a,b) result(answer)
       implicit none
       integer(kind=4),intent(in)::a,b
       integer(kind=4)::temp, tempa,tempb, answer
       
       tempa= a
       tempb= b
       do while(tempb.ne.0)
         temp= tempb
         tempb=mod(tempa,tempb)
         tempa=temp
       end do
   end function iterativeGCD

   recursive function recursiveGCD(a,b) result(answer)
   implicit none
   integer(kind=4),intent(in)::a,b
   integer(kind=4)::answer

   if (b.eq.0) then
      answer=a
   else
      answer= recursiveGCD(b,mod(a,b))
   endif 
 
   return
   end function recursiveGCD
end module gcdfunctions

program gcd
   use gcdfunctions
   implicit none
   integer(kind=4)::a,b, error
   error=1
   write(6,*)'please enter positive number'
   do while(error.ne.0)
    read(5,*,iostat=error)a,b
    if(error.ne.0) then  
      write(6,*)'please try again'
    end if
   end do


   if(a.le.0 .or. b.le.0) then
       write(6,*) 'these numbers are not positive'
   endif 
  
   ! call functions 
   write(6,*)'iterativeGCD=',iterativeGCD(a,b)
   write(6,*)'recursiveGCD=',recursiveGCD(a,b)
end program gcd  


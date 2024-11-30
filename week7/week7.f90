program finde

 interface 
   function factorial(n)
       integer (kind=4)::factorial,n
   end function 
 end interface
 integer (kind =4)::i,n
 real (kind=8)::one
 real (kind=8), allocatable :: terms(:)
 one =dble(1.0)
 write(6,*)'please enter the n value'
 read(5,*)n
 allocate(terms(n+1))
 terms(1) =one
 do i=1,n
   terms(i+1)= one/real(factorial(i),kind=8)
 end do

 write(6,*)'e is estimated as ',sum(terms),sum(terms)-dexp(one)
 deallocate(terms)
 stop
end program finde


integer (kind=4) function factorial(n)
        implicit none
        integer (kind=4), intent(in)::n
        integer (kind=4)::i,x

        x=1
        do i=1,n
          x=x*i
        end do
        factorial =x
        return
end function factorial 

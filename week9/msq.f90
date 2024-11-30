include 'magicsquare.fh'

program magsq
        use msquare
        implicit none
        integer(kind=4),n,i,ierr,num(100)
        integer(kind=4),allocatable::magicsquare(:,:)
        character(len=6)::text
        character(len=100)::filename,line
        write(6,*)'enter the filename'
        read(5,*)filename

        ierr=0

        open(unit=1,file=filename,form='formatted',&
                & access='sequential', action='READ', status=old, iostat=ierr)

        if(ierr.ne.0) then
                write(6,*) 'sorry cannot find file', filename
        endif
        n=0
        write(6,*)'ierr=',ierr
        do while(ierr.eq.0)
           read(1,*,iostat=ierr)line
           write(6,*)'test',n,line,ierr
           n=n+1
        end do
        n=n-1
        write(6,*)'n='n
        allocate(magicsquare(n,n))


        do i=1,n
            read(1,*,iostat=ierr)magicsquare(i,:)
        end do

        if(ierr.ne.0) then
             write(6,*)'cannot read'
             goto 10
        endif

        ! inputting the integer data into the matrix

        do i=1,n
           read(5,*)magicsquare(i,:)
        end do
        
        if (isMagicsquare(magicsquare,n)) then
                text='is'
        else
                text='is not'
        endif
        write(6,*)'This sqaure ',trim(text),'magic.'

        deallocate(magicsquare)
10 continue
        close(unit=1,status='keep')
20 stop

end program magsq




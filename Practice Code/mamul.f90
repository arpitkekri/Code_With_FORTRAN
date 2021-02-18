program mamul
        implicit none
        integer::i,j,l,m,n,o
        real,dimension(3,3)::a,b,d

        print*,'enter the order and  values of the first matrix'
        read(*,*)l,m
        do i=1,l
          read(*,*)(a(i,j),j=1,m)
        end do

        print *, "Output of A is below"

        print *, A

       print*,'enter the order and values of second matrix'
       read(*,*)n,o
       do i=1,n
         read(*,*)(b(i,j),j=1,o)
       end do
      
      d=matmul(a,b)
      do i=1,l
        write(*,*)(d(i,j),j=1,o)
      enddo

end
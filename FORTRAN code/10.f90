Real function Negative(a)
    real:: a 
    Negative = -a 
    return 
End function
PROGRAM main
    IMPLICIT NONE
    ! REAL, dimension(:), allocatable:: a
    ! INTEGER:: n, i
    real:: x, Negative
    read (*, *) x 
    ! allocate(a(n))
    ! read (*, *) (a(i), i = 1, n)
    ! REAL::  sum = 0, ans = 0
    ! open(unit=10,file='input.txt')
    ! DO i = 1, n
    !     write(*, *) a(i)
    ! ENDDO
    ! ans = sum/12
    ! write(*, *) ans
    ! Close (10)
    print *, Negative(x)

END PROGRAM
    
! Real function Negative(a)
!     real:: a 
!     Negative = -a 
!     return 
! End function
PROGRAM rainfall
    IMPLICIT NONE
    REAL, dimension(:), allocatable:: a
    INTEGER:: n, i

    read (*, *) n 
    allocate(a(n))
    read (*, *) (a(i), i = 1, n)
    ! REAL::  sum = 0, ans = 0
    ! open(unit=10,file='input.txt')
    DO i = 1, n
        write(*, *) a(i)
    ENDDO
    ! ans = sum/12
    ! write(*, *) ans
    ! Close (10)

END
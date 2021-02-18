PROGRAM subroutines
    IMPLICIT NONE
    
    ! REAL :: x, y
    ! READ *, x
    ! CALL Negative(x, y)
    ! PRINT *, y
    ! CONTAINS
    ! SUBROUTINE Negative(x, y)
    !     REAL:: x, y
    !     y = -x
    !     Return
    ! END SUBROUTINE


    REAL, allocatable:: x(:)
    REAL:: z
    INTEGER:: y = 5
    allocate(x(y))
    CALL fill(x, y, z)
    print *, x
    print *, y
    print *, z
    
    
    ! CONTAINS 
    ! SUBROUTINE fill(array)
    !     REAL, DIMENSION(:) :: array
    !     array = 5
    !     array(4) = 7
    ! END SUBROUTINE

END

SUBROUTINE fill(x, y, z)
    IMPLICIT NONE
    INTEGER:: y
    REAL:: x(y)
    REAL:: z
    x = 3
    z = x(1) + x(2)

    ! REAL, DIMENSION(:), allocatable :: x
    ! allocate(x(5))
    x = 3
END SUBROUTINE

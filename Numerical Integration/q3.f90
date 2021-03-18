PROGRAM q2
    IMPLICIT NONE 
    REAL, PARAMETER:: PI = acos(-1.0) 
    REAL:: a, b, xmin, xmax, dx
    REAL:: x(500), J(500, 0:2)
    INTEGER::  N, m, i    ! N is number of segment
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of segment: "
    READ(*, *) N 
    
    a = 0
    b = PI

    xmin = 0
    xmax = 20
    dx = (xmax-xmin)/(500-1)
    DO m = 0, 2
        DO i = 1, 500
            x(i) = xmin + (i-1) * dx
            CALL SimpsonsRule(a, b, J(i, m), N, m, x(i))
        ENDDO
    ENDDO
    
    OPEN(UNIT = 20, FILE = 'output.txt')
        DO i = 1, 500
            WRITE(20, *) x(i), (1./PI) * J(i, 0), (1./PI) * J(i, 1), (1./PI) * J(i, 2)
        ENDDO
    CLOSE(20)

    CONTAINS
    SUBROUTINE SimpsonsRule(a, b, Area, N, m, x)
        IMPLICIT NONE
        REAL:: a, b, Area, h, x
        INTEGER:: N, i, m
        ! N is number of segment
        IF(N .LT. 2) THEN
            Area = 0.0
            RETURN  
        ENDIF

        h = (b-a)/N
        Area = f(m, a, x) + f(m, b, x)
        DO i = 1, N/2
            Area = Area + 4 * f(m, a + (2*i - 1) * h, x) 
        ENDDO
        DO i = 1, (N+1)/2 - 1
            Area = Area + 2 * f(m, a + 2 * i * h, x)
        ENDDO
        Area = (h/3) * Area
    END SUBROUTINE SimpsonsRule

    REAL FUNCTION f(m, theta, x)
        IMPLICIT NONE 
        REAL:: x, theta
        INTEGER:: m
        f = Cos(m * theta - x * sin(theta))
        RETURN
    END FUNCTION f

END PROGRAM q2

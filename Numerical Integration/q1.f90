PROGRAM q1
    IMPLICIT NONE 
    REAL:: a, b, AreaTR, AreaSR
    INTEGER::  N      ! N is number of segment
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of segment: "
    READ(*, *) N 
    WRITE(*, fmt = '(/A)') "Enter the value of a & b"
    READ(*, *) a, b

    CALL TrapezoidalRule(a, b, AreaTR, N)
    WRITE(*, *) "Area using Trapezoidal Rule is: ", AreaTR

    CALL SimpsonsRule(a, b, AreaSR, N)
    WRITE(*, *) "Area using Simpsons Rule is: ", AreaSR

    CONTAINS
    SUBROUTINE TrapezoidalRule(a, b, Area, N)
        IMPLICIT NONE 
        REAL:: a, b, Area, Xi, h
        INTEGER:: N, i
        ! N is number of segment
        IF(N .LT. 1) THEN
            Area = 0.0
            RETURN  
        ENDIF
        h = (b-a)/N
        Area = (f(a) + f(b)) / 2.0
        DO i = 1, N-1
            Xi = a + i * h
            Area = Area + f(Xi)
        ENDDO
        Area = h * Area
    END SUBROUTINE TrapezoidalRule

    SUBROUTINE SimpsonsRule(a, b, Area, N)
        IMPLICIT NONE 
        REAL:: a, b, Area, h
        INTEGER:: N, i
        ! N is number of segment
        IF(N .LT. 2) THEN
            Area = 0.0
            RETURN  
        ENDIF
        h = (b-a)/N
        Area = f(a) + f(b)
        DO i = 1, N/2
            Area = Area + 4 * f(a + (2*i - 1) * h) 
        ENDDO
        DO i = 1, (N+1)/2 - 1
            Area = Area + 2 * f(a + 2 * i * h)
        ENDDO
        Area = (h/3) * Area
    END SUBROUTINE SimpsonsRule

    REAL FUNCTION f(x)
        IMPLICIT NONE 
        REAL:: x 
        f = 1./(1 + x**2)
        return
    END FUNCTION f

END PROGRAM q1

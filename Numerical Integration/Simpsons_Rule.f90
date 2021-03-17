PROGRAM SimpsonsRuleForIntegration
    IMPLICIT NONE 
    REAL:: a, b, Area
    INTEGER::  N      ! N is number of segment
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of segment: "
    READ(*, *) N 
    WRITE(*, fmt = '(/A)') "Enter the value of a & b"
    READ(*, *) a, b

    CALL SimpsonsRule(a, b, Area, N)
    WRITE(*, *) "Area under the curve is: ", Area

    CONTAINS
    SUBROUTINE SimpsonsRule(a, b, Area, N)
        IMPLICIT NONE 
        REAL:: a, b, Area, h
        INTEGER:: N, i
        REAL, DIMENSION(N-1):: X
        ! N is number of segment
        IF(N .LT. 2) THEN
            Area = 0.0
            RETURN  
        ENDIF
        h = (b-a)/N
        DO i = 1, N-1
            X(i) = a + i * h
        ENDDO
        Area = f(a) + f(b)
        DO i = 1, N/2
            Area = Area + 4 * f(X(2*i-1))
        ENDDO
        DO i = 1, (N+1)/2 - 1
            Area = Area + 2 * f(X(2*i))
        ENDDO
        Area = (h/3) * Area
    END SUBROUTINE SimpsonsRule

    REAL FUNCTION f(x)
        IMPLICIT NONE 
        REAL:: x 
        f = EXP(x)
        return
    END FUNCTION f

END PROGRAM SimpsonsRuleForIntegration

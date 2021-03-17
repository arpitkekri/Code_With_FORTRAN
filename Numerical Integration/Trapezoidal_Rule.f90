PROGRAM TrapezoidalRuleForIntegration
    IMPLICIT NONE 
    REAL:: a, b, Area
    INTEGER::  N      ! N is number of segment
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of segment: "
    READ(*, *) N 
    WRITE(*, fmt = '(/A)') "Enter the value of a & b"
    READ(*, *) a, b

    CALL TrapezoidalRule(a, b, Area, N)
    WRITE(*, *) "Area under the curve is: ", Area

    CONTAINS
    SUBROUTINE TrapezoidalRule(a, b, Area, N)
        IMPLICIT NONE 
        REAL:: a, b, Area, Xi
        INTEGER:: N, i
        ! N is number of segment
        IF(N .LT. 1) THEN
            Area = 0.0
            RETURN  
        ENDIF
        Area = (f(a) + f(b)) / 2.0
        DO i = 1, N-1
            Xi = a + i * (b-a)/N
            Area = Area + f(Xi)
        ENDDO
        Area = ((b-a)/N) * Area
    END SUBROUTINE TrapezoidalRule

    REAL FUNCTION f(x)
        IMPLICIT NONE 
        REAL:: x 
        f = 1.0/x 
        return
    END FUNCTION f

END PROGRAM TrapezoidalRuleForIntegration
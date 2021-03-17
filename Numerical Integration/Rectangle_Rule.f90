PROGRAM RectangleRuleForIntegration
    IMPLICIT NONE 
    REAL:: a, b, Area
    INTEGER::  N      ! N is number of points 
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of points: "
    READ(*, *) N 
    WRITE(*, fmt = '(/A)') "Enter the value of a & b"
    READ(*, *) a, b

    CALL RectangleRule(a, b, Area, N)
    WRITE(*, *) "Area under the curve is: ", Area

    CONTAINS
    SUBROUTINE RectangleRule(a, b, Area, N)
        IMPLICIT NONE 
        REAL:: a, b, Area, h, Xi
        INTEGER:: N, i
        ! N is number of points
        h = (b-a)/(N-1)
        Area = 0.00
        DO i = 1, N-1
            Xi = a + (i-1)*h
            Area = Area + f(Xi) * h
        ENDDO
    END SUBROUTINE RectangleRule

    REAL FUNCTION f(x)
        IMPLICIT NONE 
        REAL:: x 
        f = 1.0/x 
        return
    END FUNCTION f

END PROGRAM RectangleRuleForIntegration
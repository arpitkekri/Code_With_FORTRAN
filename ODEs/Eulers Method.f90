! Code for Euler's Method
MODULE precision
    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Euler_Method
    USE precision
    IMPLICIT NONE

    ! x0 --> for starting point 
    ! y0 --> for value at starting point
    ! h  --> step size 
    ! x1 --> new value of x
    ! y1 --> new value of y 
    ! xfinal --> point at which we have to find value
    ! i --> iterator,, N --> number of iteration
    REAL(KIND = dp):: x0, y0, h, x1, y1, xfinal
    INTEGER:: i, N

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter value of starting point X0 =  "
    READ(*, *) x0
    WRITE(*, fmt = '(/A, f0.3, A)', ADVANCE = 'NO') "Enter value of value at this point Y(", X0, ") =  "
    READ(*, *) y0
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter step size h =  "
    READ(*, *) h 
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter point to find value =  "
    READ(*, *) xfinal
    
    N = int((xfinal - x0)/h) ! Calculate number of iteration 
    DO i = 1, N
        x1 = x0 + h
        y1 = y0 + h * Functn(x0, y0)

        ! Update values for next itration
        x0 = x1
        y0 = y1
    ENDDO
    WRITE(*, fmt = '(/A, f0.3, A, f0.6/)') "Value of Y(", xfinal, ") = ", y1

    CONTAINS
    REAL(KIND = dp) FUNCTION Functn(x, y)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x, y
        Functn = x - y
        RETURN 
    END FUNCTION Functn

END PROGRAM Euler_Method

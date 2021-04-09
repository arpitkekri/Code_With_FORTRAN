! Code for Euler's Second Order Solution
MODULE precision
    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Euler_Method
    USE precision
    IMPLICIT NONE

    ! x0 --> for starting point 
    ! y0 --> for value at starting point
    ! v0 --> for Y' at starting point
    ! h  --> step size 
    ! x1 --> new value of x
    ! y1 --> new value of y 
    ! v1 --> new value of v
    ! xfinal --> point at which we have to find value
    ! i --> iterator,, N --> number of iteration
    REAL(KIND = dp):: x0, y0, v0, x1, y1, v1, h, xfinal
    INTEGER:: i, N

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter value of starting point X0 = "
    READ(*, *) x0
    WRITE(*, fmt = '(/A, f0.3, A)', ADVANCE = 'NO') "Enter value at this point Y(", X0, ")  = "
    READ(*, *) y0
    WRITE(*, fmt = '(/A, f0.3, A)', ADVANCE = 'NO') "Enter value at this point Y'(", X0, ") = "
    READ(*, *) v0
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter step size h =  "
    READ(*, *) h 
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter point to find value = "
    READ(*, *) xfinal

    N = int((xfinal - x0)/h) ! Calculate number of iteration
    DO i = 1, N
        x1 = x0 + h
        y1 = y0 + h * v0
        v1 = v0 + h * (-v0 + Sin(x1 * y0))

        ! Update values for next itration
        x0 = x1
        y0 = y1
        v0 = v1
        WRITE(*, *) y1, v1
    ENDDO
    WRITE(*, fmt = '(/A, f0.3, A, f0.6/)') "Value of Y(", xfinal, ") = ", y1

END PROGRAM Euler_Method

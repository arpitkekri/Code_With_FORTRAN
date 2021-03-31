MODULE precision
    ! dp = double precision
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Newton_Raphson_Method
    USE precision
    IMPLICIT NONE

    REAL(KIND = dp):: x0, x1, f1, fun, fprime, error, root
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter value of x0 : "
    ! For this question enter x0
    READ *, x0

    error = 1e-6

    DO
        fun = Func(x0)
        fprime = FuncPrime(x0)
        x1 = x0 - fun / fprime
        f1 = Func(x0)
        
        IF (f1 .EQ. 0.0) THEN
            root = x0
            WRITE(*, *) "Root is: ", root
            RETURN
        ENDIF

        IF (abs((x1 - x0)/x1) .LT. error) THEN 
            root = x1
            WRITE(*, *) "Root is: ", root
            RETURN
        ELSE 
            x0 = x1
        ENDIF

    ENDDO 

    CONTAINS
    REAL(KIND = dp) FUNCTION Func(x)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x
        Func = x**2 - 3*x + 2
        RETURN 
    END FUNCTION Func

    REAL(KIND = dp) FUNCTION FuncPrime(x)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x
        FuncPrime = 2*x - 3
        RETURN 
    END FUNCTION FuncPrime

END PROGRAM Newton_Raphson_Method

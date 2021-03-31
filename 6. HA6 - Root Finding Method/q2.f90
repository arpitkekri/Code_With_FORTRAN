MODULE precision
    ! dp = double precision
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Secant_Method
    USE precision
    IMPLICIT NONE

    REAL(KIND = dp):: x1, x2, x3, f1, f2, f3, error, root
    WRITE(*, *) "Enter value of 2 initial point "
    ! For this question enter 2 initial point x1 and x2
    READ(*, *) x1, x2
    
    error = 1e-6
    f1 = Functn(x1)
    f2 = Functn(x2)

    DO
        x3 = (f2*x1 - f1*x2) / (f2 - f1)
        f3 = Functn(x3)
        IF (f3 .EQ. 0.0) THEN
            root = x3
            WRITE(*, *) "Root is: ", root
            RETURN
        ENDIF

        IF (abs((x3 - x2)/x3) .LT. error) THEN 
            root = x3
            WRITE(*, *) "Root is: ", root
            RETURN
        ELSE 
            x1 = x2
            f1 = f2
            x2 = x3
            f2 = f3
        ENDIF
    ENDDO 

    CONTAINS
    REAL(KIND = dp) FUNCTION Functn(x)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x, rho
        rho = 4.0_dp
        Functn = x*tan(x) - sqrt(rho**2 - x ** 2)
        RETURN 
    END FUNCTION Functn

END PROGRAM Secant_Method

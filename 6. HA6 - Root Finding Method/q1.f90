MODULE precision
    ! dp = double precision
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Newton_Raphson_Method
    USE precision
    IMPLICIT NONE

    REAL(KIND = dp):: x0, x1, f0, fun, fprime, error, root
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter value of x0 : "
    ! For this question enter x0
    READ(*, *) x0

    error = 1e-6
    
    DO
        fun = Functn(x0)
        fprime = Functn_Prime(x0)
        x1 = x0 - fun / fprime
        f0 = Functn(x1)

        IF (f0 .EQ. 0.0_dp) THEN
            root = x1
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
    REAL(KIND = dp) FUNCTION Functn(x)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x, rho
        rho = 4.0_dp
        Functn = x*tan(x) - sqrt(rho**2 - x**2)
        RETURN 
    END FUNCTION Functn

    REAL(KIND = dp) FUNCTION Functn_Prime(x)
        USE precision
        IMPLICIT NONE 
        REAL(KIND = dp):: x, rho
        rho = 4.0_dp
        Functn_Prime = (x/(cos(x)**2)) + tan(x) + (x/sqrt((rho**2) - (x**2))) 
        RETURN 
    END FUNCTION Functn_Prime

END PROGRAM Newton_Raphson_Method

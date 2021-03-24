MODULE precision
    ! dp = double precision
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM Bisection_Method
    IMPLICIT NONE 
    
    REAL:: x0, x1, x2, f0, f1, f2 


    f1 = F(x1)
    f2 = F(x2)
    IF (f1*f2 .GT. 0.0) THEN 
        ! WRITE(*, *) "x1 and x2 do not bracket any root"
    ELSE 

    ENDIF

    DO
        x0 = (x1 + x2)/2.0
        f0 = F(x0)
        IF (f0 .EQ. 0.0) THEN 
            EXIT
        ELSE 
            
        ENDIF
    ENDDO 

    CONTAINS
    REAL FUNCTION F(x)
        IMPLICIT NONE 
        REAL:: x
        f = x ! EDIT THIS
        RETURN 
    END FUNCTION F

END PROGRAM Bisection_Method
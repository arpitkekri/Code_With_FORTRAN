
MODULE precision
    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

MODULE RxnConstant
    USE precision
    IMPLICIT NONE

    INTEGER, PARAMETER:: n = 4
    REAL(KIND = dp), PARAMETER:: kab = 1, kba = 3, kbc = 4.2, kcb = 7.3, kcd = 0.4
ENDMODULE RxnConstant
  
PROGRAM Chemical_Kinetics_Rxn
    USE precision
    USE RxnConstant
    IMPLICIT NONE
  
    REAL(KIND = dp):: concentration(n)
    REAL(KIND = dp):: time, dt, tmax
    INTEGER:: nt, i
    
    time = 0;
    dt = 0.0001;
    tmax = 60;
    ! Number of iteration
    nt = int(tmax/dt);
    concentration = 0            ! Make all concentration = 0.0
    concentration(1) = 5.0       ! Now, [A] = 5.0
    OPEN(unit = 10, file = "output.txt")
    DO i = 1, nt
        time = time + dt;
        CALL Euler(concentration, dt)
        WRITE(10, *) time, concentration(1), concentration(2), concentration(3), concentration(4)
    ENDDO
    CLOSE(10)
    CONTAINS
    SUBROUTINE Euler(concentration, dt)
        USE precision
        USE RxnConstant
        IMPLICIT NONE
        
        REAL(KIND = dp):: concentration(n), Func(n)
        REAL(KIND = dp):: dt
        
        CALL DbyDT(concentration, 1, Func(1))
        CALL DbyDT(concentration, 2, Func(2))
        CALL DbyDT(concentration, 3, Func(3))
        CALL DbyDT(concentration, 4, Func(4))
        
        ! Euler Propagation
        concentration = concentration + dt * Func
    END SUBROUTINE Euler

    SUBROUTINE DbyDT(concentration, selector, val_fun)
        USE precision
        USE RxnConstant
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n)
        REAL(KIND = dp):: val_fun
        INTEGER:: selector

        IF(selector .EQ. 1) THEN 
            val_fun = -kab * concentration(1) + kba * concentration(2)
        ELSE IF(selector .EQ. 2) THEN 
            val_fun = kab * concentration(1) - kba * concentration(2) - kbc * concentration(2) + kcb * concentration(3)**2
        ELSE IF(selector .EQ. 3) THEN 
            val_fun = 2 * kbc * concentration(2) - 2 * kcb * concentration(3)**2 - kcd * concentration(3)
        ELSE 
            val_fun = kcd * concentration(3);
        ENDIF

    END SUBROUTINE DbyDT

END PROGRAM Chemical_Kinetics_Rxn

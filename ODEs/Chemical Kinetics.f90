
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
        CALL Euler(concentration, dt, Func(concentration))
        WRITE(10, *) time, concentration
    ENDDO
    CLOSE(10)

    CONTAINS
    SUBROUTINE Euler(concentration, dt, Func)
        USE precision
        IMPLICIT NONE
        
        REAL(KIND = dp):: concentration(n), Func(n)
        REAL(KIND = dp):: dt
        
        ! Euler Propagation
        concentration = concentration + dt * Func
    END SUBROUTINE Euler

    FUNCTION Func(concentration)
        USE precision
        USE RxnConstant
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n), Func(n)

        Func(1) = -kab * concentration(1) + kba * concentration(2)
        Func(2) = kab * concentration(1) - kba * concentration(2) - kbc * concentration(2) + kcb * concentration(3)**2
        Func(3) = 2 * kbc * concentration(2) - 2 * kcb * concentration(3)**2 - kcd * concentration(3)
        Func(4) = kcd * concentration(3)
    END FUNCTION Func

END PROGRAM Chemical_Kinetics_Rxn

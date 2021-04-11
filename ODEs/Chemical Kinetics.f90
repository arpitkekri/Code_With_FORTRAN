
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
        ! CALL Euler(concentration, dt)
        ! CALL RK2(concentration, dt)
        CALL RK4(concentration, dt)
        WRITE(10, *) time, concentration
    ENDDO
    CLOSE(10)

    CONTAINS
    SUBROUTINE Euler(concentration, dt)
        USE precision
        USE RxnConstant
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n), Func(n)
        REAL(KIND = dp):: dt

        ! Euler Propagation
        CALL DbyDT(concentration, Func)
        concentration = concentration + dt * Func
    END SUBROUTINE Euler

    SUBROUTINE RK2(concentration, dt)
        USE precision
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n), sl(n), sr(n)
        REAL(KIND = dp):: dt

        ! RK2 Propagation
        CALL DbyDT(concentration, sl)
        CALL DbyDT(concentration + dt * sl, sr)
        concentration = concentration + dt * (sl + sr)/2.0_dp
    END SUBROUTINE RK2

    SUBROUTINE RK4(concentration, dt)
        USE precision
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n), s1(n), s2(n), s3(n), s4(n)
        REAL(KIND = dp):: dt

        ! RK2 Propagation
        CALL DbyDT(concentration, s1)
        CALL DbyDT(concentration + dt * s1/2.0_dp, s2)
        CALL DbyDT(concentration + dt * s2/2.0_dp, s3)
        CALL DbyDT(concentration + dt * s3, s4)
        concentration = concentration + dt * (s1 + 2*s2 + 2*s3 + s4)/6.0_dp
    END SUBROUTINE RK4

    SUBROUTINE DbyDT(concentration, Func)
        USE precision
        USE RxnConstant
        IMPLICIT NONE

        REAL(KIND = dp):: concentration(n), Func(n)

        Func(1) = -kab * concentration(1) + kba * concentration(2)
        Func(2) = kab * concentration(1) - kba * concentration(2) - kbc * concentration(2) + kcb * concentration(3)**2
        Func(3) = 2 * kbc * concentration(2) - 2 * kcb * concentration(3)**2 - kcd * concentration(3)
        Func(4) = kcd * concentration(3)
    END SUBROUTINE DbyDT

END PROGRAM Chemical_Kinetics_Rxn

! Name = ARPIT KUMAR JAIN  ROLL No = 180122009
! Code for HA8 Time Dependent Schrodinger Equation (TDSE) --> Using Finite-difference scheme

MODULE precision
    IMPLICIT NONE

    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

MODULE constants
    USE precision
    IMPLICIT NONE

    REAL(KIND = dp), PARAMETER:: pi = acos(-1.0)
    ! k0 = p0/hbar,  hbar = 1 for a.u.
    ! So k0 = p0
    REAL(KIND = dp), PARAMETER:: alpha = 20.0, x0 = -0.5, p0 = 50.0, m = 14500.0
    REAL(KIND = dp), PARAMETER:: Xmin = -2.0, dx = 0.02, dt = 0.1
    INTEGER, PARAMETER:: numX = 256
    COMPLEX, PARAMETER::iota = (0.0, 1.0)
    CHARACTER*11:: filename
END MODULE constants

PROGRAM TDSE_Finite_Difference
    USE precision
    USE constants
    IMPLICIT NONE

    INTEGER:: i, j
    REAL(KIND = dp), DIMENSION(numX):: x, pot_V
    COMPLEX(KIND = dp), DIMENSION(numX):: psit0, psit1, psit2, dSqPsiBydx2

    OPEN(UNIT = 6789, FILE = 'potential.txt')

    ! First Set Up X-Grid
    DO i = 1, numX
        x(i) = Xmin + (i-1) * dx
    ENDDO

    ! Now Set Up Potential Grid
    DO i = 1, numX
        IF(x(i) .GE. 0.0_dp) THEN
            pot_V(i) = 0.1
        ELSE
            pot_V(i) = 0.0
        ENDIF
        WRITE(6789, 859) x(i), pot_V(i)
        859 FORMAT(f0.3, 3x, f0.6)
    ENDDO
    CLOSE(6789)

    ! Calculate Initial wavepacket ψ(x, t0) using given equation
    CALL InitialWavePacket(x, psit0)

    ! Calculate Wavepacket at time t = 1 ψ(x, t1) using Euler Method
    CALL EulerWavePacket(pot_V, psit0, psit1)

    ! Propagation of ψ from t = 2 to 5000 time step
    DO j = 2, 5000
        
        CALL FiniteDifferenceMethod(psit1, dSqPsiBydx2)
        CALL PropagateWavePacket(x, pot_V, psit0, psit1, psit2, dSqPsiBydx2, j)

        ! Update Values for next propagation
        psit0 = psit1
        psit1 = psit2

    ENDDO
END PROGRAM TDSE_Finite_Difference

! Subroutine for initial wavepacket at t = 0
SUBROUTINE InitialWavePacket(x, psit0)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), psiSquare
    COMPLEX(KIND = dp):: psit0(numX)
    INTEGER:: i

    OPEN(UNIT = 0, FILE = 'psi0.txt')
    DO i = 1, numX
        psit0(i) = ( (2.0_dp * alpha/pi)**0.25_dp ) * EXP(iota * p0 * (x(i)-x0)) * EXP(-alpha * (x(i)-x0)**2)
        
        ! ψ squre is real part of (ψ.ψ*)
        psiSquare = REAL(psit0(i) * CONJG(psit0(i)))
        WRITE(0, *) x(i), psiSquare
    ENDDO
    CLOSE(0)

END SUBROUTINE InitialWavePacket

! Subroutine for WavePacket at t = 1 step using Euler Method
SUBROUTINE EulerWavePacket(pot_V, psit0, psit1)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: pot_V(numX)
    COMPLEX(KIND = dp):: psit0(numX), psit1(numX), dSqPsiBydx2(numX)
    INTEGER:: i

    CALL FiniteDifferenceMethod(psit0, dSqPsiBydx2)

    DO i = 1, numX
        psit1(i) = psit0(i) + iota * dt * ( dSqPsiBydx2(i)/(2.0_dp * m) - pot_V(i) * psit0(i) )
    ENDDO

END SUBROUTINE EulerWavePacket

SUBROUTINE PropagateWavePacket(x, pot_V, psit0, psit1, psit2, dSqPsiBydx2, iterationNumber)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), pot_V(numX), psiSquare
    COMPLEX(KIND = dp):: psit0(numX), psit1(numX), psit2(numX), dSqPsiBydx2(numX)
    INTEGER:: i, iterationNumber

    DO i = 1, numX

        psit2(i) = psit0(i) - 2.0_dp * iota * dt * ( -dSqPsiBydx2(i)/(2.0_dp*m) + pot_V(i) * psit1(i) )
        psiSquare = REAL(psit2(i) * CONJG(psit2(i)))

        ! We have to plot for every 100th time stap so take mode with 100
        IF(MOD(iterationNumber, 100) .EQ. 0) THEN
            WRITE(filename, 8976) iterationNumber
            8976 FORMAT('psi',i0, '.txt')
            IF(i .EQ. 1) WRITE(*, 15) filename
            15 FORMAT("Generating ", A11, "...")
            OPEN(unit = 16, FILE = filename)
            WRITE(16, *) x(i), psiSquare
        ENDIF

    ENDDO
END SUBROUTINE PropagateWavePacket

SUBROUTINE FiniteDifferenceMethod(fn_psi, dSqPsiBydx2)
    USE precision
    USE constants
    IMPLICIT NONE

    COMPLEX(KIND = dp):: fn_psi(numX), dSqPsiBydx2(numX)
    INTEGER:: i

    dSqPsiBydx2 = 0

    DO i = 2, numX-1
        dSqPsiBydx2(i) = ( fn_psi(i+1) - 2 * fn_psi(i) + fn_psi(i-1)) / dx**2
    ENDDO

    ! Forward difference formula for t = 1
    dSqPsiBydx2(1) = (fn_psi(3) - 2.0 * fn_psi(2) + fn_psi(1)) / dx**2

    ! Backward difference formula for t = numX
    dSqPsiBydx2(numX) = (fn_psi(numX) - 2.0 * fn_psi(numX-1) + fn_psi(numX-2)) / dx**2

END SUBROUTINE FiniteDifferenceMethod

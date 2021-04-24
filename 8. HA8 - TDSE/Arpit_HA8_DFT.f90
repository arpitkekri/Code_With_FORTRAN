! Name = ARPIT KUMAR JAIN  ROLL No = 180122009
! Code for HA8 Time Dependent Schrodinger Equation (TDSE) --> Using Discrete Fourire Transformation

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
    COMPLEX, PARAMETER:: iota = (0.0, 1.0)
    CHARACTER*64:: filename
END MODULE constants

PROGRAM TDSE_Discrete_Fourier_Transformation
    USE precision
    USE constants
    IMPLICIT NONE

    INTEGER:: i, j
    REAL(KIND = dp), DIMENSION(numX):: x, pot_V, Km, KSquare
    COMPLEX(KIND = dp), DIMENSION(numX):: psit0, psit1, psit2, Tpsi

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

    ! Calculation of K-Grid
    CALL CalculateKgrid(Km, KSquare)

    ! Calculate Wavepacket at time t = 1 ψ(x, t1) using Euler Method
    CALL EulerWavePacket(x, pot_V, psit0, psit1, Km, KSquare)

    ! Propagation of ψ from t = 2 to 5000 time step
    DO j = 2, 5000
        Tpsi = psit1
        CALL DiscreteFourierTransformation(x, Tpsi, Km, KSquare)
        CALL PropagateWavePacket(x, pot_V, psit0, psit1, psit2, Tpsi, j)

        ! Update Values for next propagation
        psit0 = psit1
        psit1 = psit2
    ENDDO

END PROGRAM TDSE_Discrete_Fourier_Transformation

! Subroutine for initial wavepacket at t = 0
SUBROUTINE InitialWavePacket(x, psit0)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), psisq
    COMPLEX(KIND = dp):: psit0(numX)
    INTEGER:: i

    OPEN(UNIT = 0, FILE = 'psi0.txt')
    DO i = 1, numX
        psit0(i) = ( (2.0_dp * alpha/pi)**0.25_dp ) * EXP(iota * p0 * (x(i)-x0)) * EXP(-alpha * (x(i)-x0)**2)
        psisq = REAL(psit0(i) * CONJG(psit0(i)))
        WRITE(0, *) x(i), psisq
    ENDDO
    CLOSE(0)

END SUBROUTINE InitialWavePacket

! Subroutine for calculation of K and K^2
SUBROUTINE CalculateKgrid(Km, KSquare)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: Km(numX), KSquare(numX)
    INTEGER:: i

    DO i = 1, numX/2
        Km(i) = 2.0_dp * pi * dfloat(i-1) / (dfloat(numX) * dx)
    ENDDO
    
    DO i = numX/2+1, numX
        Km(i) = 2.0_dp * pi * dfloat(i-1-numX) / (dfloat(numX) * dx)
    ENDDO

    KSquare = Km * Km

END SUBROUTINE CalculateKgrid

! Subroutine for WavePacket at t = 1 step using Euler Method
SUBROUTINE EulerWavePacket(x, pot_V, psit0, psit1, Km, KSquare)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), pot_V(numX), Km(numX), KSquare(numX)
    COMPLEX(KIND = dp):: psit0(numX), psit1(numX), Tpsi(numX)
    INTEGER:: i

    Tpsi = psit0
    CALL DiscreteFourierTransformation(x, Tpsi, Km, KSquare)

    DO i = 1, numX
        psit1(i) = psit0(i) + iota * dt * ( Tpsi(i)/(2.0_dp * m) - pot_V(i) * psit0(i) )
    ENDDO

END SUBROUTINE EulerWavePacket

SUBROUTINE PropagateWavePacket(x, pot_V, psit0, psit1, psit2, Tpsi, iterationNumber)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), pot_V(numX), psisq
    COMPLEX(KIND = dp):: psit0(numX), psit1(numX), psit2(numX), Tpsi(numX)
    INTEGER:: i, iterationNumber

    DO i = 1, numX
        psit2(i) = psit0(i) - 2.0_dp * iota * dt * ( -Tpsi(i)/(2.0_dp*m) + pot_V(i) * psit1(i) )
        psisq = REAL(psit2(i) * CONJG(psit2(i)))

        ! We have to plot for every 100th time stap so take mode with 100
        IF(MOD(iterationNumber, 100) .EQ. 0) THEN
            WRITE(filename, 8976) iterationNumber
            8976 FORMAT('psi',i0,'.txt')
            IF(i .EQ. 1) WRITE(*, 15) filename
            15 FORMAT("Generating ", A11, "...")
            OPEN(unit = 16, FILE = filename)
            WRITE(16, *) x(i), psisq
        ENDIF
    ENDDO

END SUBROUTINE PropagateWavePacket

SUBROUTINE DiscreteFourierTransformation(x, psi, Km, KSquare)
    USE precision
    USE constants
    IMPLICIT NONE

    REAL(KIND = dp):: x(numX), Km(numX), KSquare(numX)
    COMPLEX(KIND = dp):: psi(numX), phi(numX)
    INTEGER:: i, j

    phi = 0.0
    DO j = 1, numX
        phi = phi + psi(j) * EXP(-iota * Km * x(j))
    ENDDO

    phi = phi / sqrt(dfloat(numX))
    phi = -KSquare * phi

    psi = 0.0
    DO i = 1, numX
        psi = psi + phi(i) * EXP(iota * Km(i) * x)
    ENDDO
    psi = psi / sqrt(dfloat(numX))

END SUBROUTINE DiscreteFourierTransformation

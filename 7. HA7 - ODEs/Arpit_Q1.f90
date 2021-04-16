! Name = ARPIT KUMAR JAIN  ROLL No = 180122009
! Code for Question 1 Radial Schrodinger Equation --> Second Order ODEs (R" = -2R'/d - 2ER - 2R/d)

MODULE precision
    IMPLICIT NONE

    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

! h  --> step size = 0.0005
MODULE constants
    USE precision
    IMPLICIT NONE
    
    REAL(KIND = dp):: h = 0.0005
    REAL(KIND = dp):: dinitial = 0.0005, Rinitial = 0.000001, vinitial = -1000.0
END MODULE constants

PROGRAM Radial_Schrodinger_Equation
    USE precision
    USE constants
    IMPLICIT NONE
    
    ! d --> Distance from center ==> basically small r (radius)
    ! R0 --> value at starting point
    ! v0 --> value of R' at starting point
    ! Enrg --> Energy
    ! R1 --> new value of R
    ! v1 --> new value of v
    REAL(KIND = dp):: d, R0, v0, Enrg, R1, v1

    ! i --> iterator,, Nt --> number of iteration or Grid Points
    INTEGER:: i, j, Nt

    Nt = 10000
    Enrg = -0.52_dp ! Initial value of energy

    ! ****************Propagating Runge-Kutta 4 Method Solution*******************

    ! Take energy from -0.52 to -0.48 ==> 5 energy levels with ΔE = 0.01
    OPEN(UNIT = 21, FILE = 'E-0.52.txt')
    OPEN(UNIT = 22, FILE = 'E-0.51.txt')
    OPEN(UNIT = 23, FILE = 'E-0.50.txt')
    OPEN(UNIT = 24, FILE = 'E-0.49.txt')
    OPEN(UNIT = 25, FILE = 'E-0.48.txt')
    DO j = 1, 5
        d = dinitial
        R0 = Rinitial
        v0 = vinitial
        DO i = 1, Nt
            CALL RK4_Method(d, R0, v0, R1, v1, Enrg)
            WRITE(20+j, 5) d, R1, abs(d*R1)**2.0_dp
            5 FORMAT(f9.6, 3x, f9.6, 3x, f9.6)
            
            ! Update values for next itration
            R0 = R1
            v0 = v1
            d = d + h
        ENDDO
        Enrg = Enrg + 0.01_dp
    ENDDO
    CLOSE(21)
    CLOSE(22)
    CLOSE(23)
    CLOSE(24)
    CLOSE(25)

    CONTAINS
    SUBROUTINE RK4_Method(d, R0, v0, R1, v1, Enrg)
        USE precision
        USE constants
        IMPLICIT NONE

        REAL(KIND = dp):: d, R0, v0, R1, v1, Enrg
        REAL(KIND = dp):: s1R, s2R, s3R, s4R, s1v, s2v, s3v, s4v

        s1R = h * dR_by_dd(d, R0, v0)
        s1v = h * dv_by_dd(d, R0, v0, Enrg)

        s2R = h * dR_by_dd(d + h/2.0_dp, R0 + s1R/2.0_dp, v0 + s1v/2.0_dp)
        s2v = h * dv_by_dd(d + h/2.0_dp, R0 + s1R/2.0_dp, v0 + s1v/2.0_dp, Enrg)

        s3R = h * dR_by_dd(d + h/2.0_dp, R0 + s2R/2.0_dp, v0 + s2v/2.0_dp)
        s3v = h * dv_by_dd(d + h/2.0_dp, R0 + s2R/2.0_dp, v0 + s2v/2.0_dp, Enrg)

        s4R = h * dR_by_dd(d + h, R0 + s3R, v0 + s3v)
        s4v = h * dv_by_dd(d + h, R0 + s3R, v0 + s3v, Enrg)

        R1 = R0 + (s1R + 2.0_dp * s2R + 2.0_dp * s3R + s4R)/6.0_dp
        v1 = v0 + (s1v + 2.0_dp * s2v + 2.0_dp * s3v + s4v)/6.0_dp
    END SUBROUTINE RK4_Method

    FUNCTION dR_by_dd(d, R, v)
        IMPLICIT NONE

        REAL(KIND = dp):: dR_by_dd, d, R, v

        ! No change in d and R
        d = d
        R = R

        ! R' = v
        dR_by_dd = v
        RETURN
    END FUNCTION dR_by_dd

    FUNCTION dv_by_dd(d, R, v, Enrg)
        IMPLICIT NONE

        REAL(KIND = dp):: dv_by_dd, d, R, v, Enrg

        ! v' = -2v/d - 2ER - 2R/d
        dv_by_dd = (-2.0_dp*v)/d - 2.0_dp * Enrg * R - 2.0_dp*R/d
        RETURN
    END FUNCTION dv_by_dd

END PROGRAM Radial_Schrodinger_Equation

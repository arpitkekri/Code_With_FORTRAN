! Name = ARPIT KUMAR JAIN  ROLL No = 180122009
! Code for Question 2 Simple Harmonic Oscillator --> Second Order ODEs (x" = -x)

MODULE precision
    IMPLICIT NONE

    ! dp = double precisiona
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

! Tp --> Time Period = 2π
! h  --> step size = 0.02T
MODULE constants
    USE precision
    IMPLICIT NONE
    
    REAL(KIND = dp), PARAMETER :: pi = acos(-1.0), Tp = 2 * pi
    REAL(KIND = dp):: h = 0.02 * Tp
    REAL(KIND = dp):: tinitial = 0.0, xinitial = 1.0, pinitial = 0.0
END MODULE constants

PROGRAM Simple_Harmonic_Oscillator
    USE precision
    USE constants
    IMPLICIT NONE
    
    ! t --> Time
    ! x0 --> value at starting point
    ! p0 --> value of X' at starting point
    ! x1 --> new value of x
    ! p1 --> new value of p
    REAL(KIND = dp):: t, x0, p0, x1, p1

    ! i --> iterator,, Nt --> number of iteration or Grid Points
    INTEGER:: i, Nt

    Nt = 500

    ! Propagating Euler Method Solution 
    t = tinitial
    x0 = xinitial
    p0 = pinitial
    OPEN(UNIT = 10, FILE = 'Euler_P_vs_X.txt')
    OPEN(UNIT = 11, FILE = 'Euler_2E_vs_time.txt')
    OPEN(UNIT = 12, FILE = 'Euler_X_vs_time.txt')
    DO i = 1, Nt

        t = t + h
        CALL Euler_Method(t, x0, p0, x1, p1)

        WRITE(10, *) x1, p1
        WRITE(11, *) t/Tp, p1**2.0_dp + x1**2.0_dp
        WRITE(12, *) t/Tp, x1

        ! Update values for next itration
        x0 = x1
        p0 = p1
    ENDDO
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)

    ! Propagating Runge-Kutta 4 Method Solution 
    t = tinitial
    x0 = xinitial
    p0 = pinitial
    OPEN(UNIT = 20, FILE = 'RK4_P_vs_X.txt')
    OPEN(UNIT = 21, FILE = 'RK4_2E_vs_time.txt')
    OPEN(UNIT = 22, FILE = 'RK4_X_vs_time.txt')
    DO i = 1, Nt

        t = t + h
        CALL RK4_Method(t, x0, p0, x1, p1)

        WRITE(20, 5) x1, p1
        WRITE(21, 5) t/Tp, p1**2.0_dp + x1**2.0_dp
        WRITE(22, 5) t/Tp, x1
        5 FORMAT(f7.4, 3x, f7.4)

        ! Update values for next itration
        x0 = x1
        p0 = p1
    ENDDO
    CLOSE(20)
    CLOSE(21)
    CLOSE(22)

    CONTAINS
    SUBROUTINE Euler_Method(t, x0, p0, x1, p1)
        USE precision
        USE constants
        IMPLICIT NONE

        REAL(KIND = dp):: t, x0, p0, x1, p1

        x1 = x0 + h * dx_by_dt(t, x0, p0)
        p1 = p0 + h * dp_by_dt(t, x0, p0)
    END SUBROUTINE Euler_Method

    SUBROUTINE RK4_Method(t, x0, p0, x1, p1)
        USE precision
        USE constants
        IMPLICIT NONE

        REAL(KIND = dp):: t, x0, p0, x1, p1
        REAL(KIND = dp):: s1x, s2x, s3x, s4x, s1p, s2p, s3p, s4p

        s1x = h * dx_by_dt(t, x0, p0)
        s1p = h * dp_by_dt(t, x0, p0)

        s2x = h * dx_by_dt(t + h/2.0_dp, x0 + s1x/2.0_dp, p0 + s1p/2.0_dp)
        s2p = h * dp_by_dt(t + h/2.0_dp, x0 + s1x/2.0_dp, p0 + s1p/2.0_dp)

        s3x = h * dx_by_dt(t + h/2.0_dp, x0 + s2x/2.0_dp, p0 + s2p/2.0_dp)
        s3p = h * dp_by_dt(t + h/2.0_dp, x0 + s2x/2.0_dp, p0 + s2p/2.0_dp)

        s4x = h * dx_by_dt(t + h, x0 + s3x, p0 + s3p)
        s4p = h * dp_by_dt(t + h, x0 + s3x, p0 + s3p)

        x1 = x0 + (s1x + 2.0_dp * s2x + 2.0_dp * s3x + s4x)/6.0_dp
        p1 = p0 + (s1p + 2.0_dp * s2p + 2.0_dp * s3p + s4p)/6.0_dp
    END SUBROUTINE RK4_Method

    FUNCTION dx_by_dt(t, x, p)
        IMPLICIT NONE

        REAL(KIND = dp):: dx_by_dt, t, x, p

        ! No change in t and x
        t = t
        x = x

        ! x' = p
        dx_by_dt = p
        RETURN
    END FUNCTION dx_by_dt

    FUNCTION dp_by_dt(t, x, p)
        IMPLICIT NONE

        REAL(KIND = dp):: dp_by_dt, t, x, p

        ! No change in t and p
        t = t
        p = p

        ! P' = -x
        dp_by_dt = -x
        RETURN
    END FUNCTION dp_by_dt

END PROGRAM Simple_Harmonic_Oscillator

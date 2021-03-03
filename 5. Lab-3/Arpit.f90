! Name: ARPIT KUMAR JAIN, Roll No: 180122009
PROGRAM ChebyshevAndLagrangeInterpolation
    IMPLICIT NONE
    REAL, PARAMETER :: pi = acos(-1.0)
    INTEGER:: n, k, i
    REAL:: point, FuncVal, LagVal, CheVal
    REAL, DIMENSION(:), ALLOCATABLE:: Lx, LF, Cx, CF

    n = 16
    ! we have to find 17 points so n = 16 and our index will be 0 to 17

    ! Lx is Lagrange equal distance point (17 points)
    ALLOCATE(Lx(0:n));

    ! LF is Lagrange Function value at Lagrange equal distance points
    ALLOCATE(LF(0:n));

    ! Cx is Chebychev Nodes (17)
    ALLOCATE(Cx(0:n));

    ! CF is Value of Function at Chebyshev Nodes
    ALLOCATE(CF(0:n));


    DO i = 0, n
        ! Finding 17 equal distance points
        Lx(i) = -1.0 + i * (2.0/16)

        ! Finding Function values at lagrange points
        LF(i) = 1./(1. + 25 * Lx(i) * Lx(i))
    ENDDO

    DO k = 0, n
        ! Calculating Chebyshev Nodes
        Cx(k) = cos(((2*k+1 )*pi)/(2*n+2))

        ! Calculating Function value at those nodes
        CF(k) = 1./(1. + 25 * Cx(k) * Cx(k))
    ENDDO

    ! Opening the file output.txt 
    OPEN(UNIT = 20, FILE = 'output.txt', FORM = "formatted")

        ! Calculating at 102 different values between -1 to 1
        DO i = 0, 101
            point = -1.0 + i * (2.0/101)

            !  FuncVal is a original function value at point 
            FuncVal = 1./(1. + 25 * point * point)

            ! LagVal is a Lagrange Interpolation Value at Lagrange Point
            CALL LagrangeInterpolation(point, n, LF, Lx, LagVal)

            ! CheVal is a Lagrange Interpolation Value at Chebyshev Points
            CALL LagrangeInterpolation(point, n, CF, Cx, CheVal)

            WRITE(20, *) point, FuncVal, LagVal, CheVal

        ENDDO

    CLOSE(20)

    CONTAINS 
    SUBROUTINE LagrangeInterpolation(point, n, F, X, ans)
        IMPLICIT NONE
        ! PnX is a Lagrange Polynomial
        REAL:: point, PnX, ans
        INTEGER:: n, i, j
        REAL, DIMENSION(0:n):: X, F, Lagrange

        PnX = 0.0
        DO i = 0, n
            Lagrange(i) = 1.0
            DO j = 0, n
                if(i .NE. j) Lagrange(i) = Lagrange(i) * ((point - x(j)) / (x(i) - x(j)));
            ENDDO
            PnX = PnX + F(i) * Lagrange(i)
        ENDDO

        ! stodting PnX value in the ans
        ans = PnX
    END SUBROUTINE LagrangeInterpolation

END PROGRAM ChebyshevAndLagrangeInterpolation
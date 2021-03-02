! Name: ARPIT KUMAR JAIN, Roll No: 180122009
PROGRAM ChebyshevInterpolationSinPiX
    IMPLICIT NONE
    REAL, PARAMETER :: pi = acos(-1.0)
    INTEGER:: n, k
    REAL:: Point 
    REAL, DIMENSION(:), ALLOCATABLE:: X, F

    WRITE(*, fmt = '(/A)', ADVANCE = "NO") "Enter Order of Interpolation Polynomial: "
    READ(*, *) n
    ALLOCATE(X(0:n));
    ALLOCATE(F(0:n));
    WRITE(*, 10, ADVANCE = "NO") n
    10 FORMAT(/, "Enter the X value to find Q", i0, "(X): ")
    READ(*, *) point

    DO k = 0, n
        X(k) = cos(((2*k+1 )*pi)/(2*n+2))
        F(k) = Sin(pi * X(k))
    ENDDO

    CALL NewtonInterpolation(point, n, F, X)

    CONTAINS 
    SUBROUTINE NewtonInterpolation(point, n, F, X)
        IMPLICIT NONE
        REAL:: point, QnX, polynomialTerm
        INTEGER:: n, i, j
        REAL, DIMENSION(0:n):: X, F
        REAL, DIMENSION(0:n, 0:n):: DD ! DD for Divided Difference
        DD = 0.0

        DO i = 0, n
            DD(i, i) = F(i)
        ENDDO

        DO j = 1, n
            DO i = 0, n-j
                DD(i, i+j) = ( DD(i+1, i+j) - DD(i, i+j-1) ) / ( X(i+j) - X(i) )
                ! WRITE(*, fmt = '(A, i0, A, i0, A, f0.9)') "DD[", i, "...", i+j, "] = ", DD(i, i+j)
            ENDDO
        ENDDO

        QnX = DD(0, 0) ! QnX = a0 (initialize)

        DO i = 0, n-1
            polynomialTerm = 1.0
            DO j = 0, i
                polynomialTerm = polynomialTerm * (point - X(j))
            ENDDO
            QnX = QnX + DD(0, i+1) * polynomialTerm
        ENDDO

        WRITE(*, fmt = '(/A)') "Result of Newton Interpolation"
        WRITE(*, 11) n, point, QnX
        11 FORMAT("Q", i0, "(", f0.3, ") = ", f0.8/)
    END SUBROUTINE NewtonInterpolation

END PROGRAM ChebyshevInterpolationSinPiX
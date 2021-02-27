! Name: ARPIT KUMAR JAIN, Roll No: 180122009
PROGRAM Interpolation
    IMPLICIT NONE

    INTEGER:: i, n
    REAL:: point
    REAL, DIMENSION(:), ALLOCATABLE:: X, F

    WRITE(*, fmt = '(A)', ADVANCE = "NO") "Enter Order of Interpolation Polynomial: "
    READ(*, *) n
    
    ALLOCATE(x(0:n));
    ALLOCATE(F(0:n));
    
    WRITE(*, fmt = '(A)', ADVANCE = "NO") "Enter the value of point: "
    READ(*, *) point
    
    OPEN(unit = 10, FILE = 'input.txt')
    DO i = 0, n
        READ(10, *) X(i), F(i)
    ENDDO
    CLOSE(10)

    CALL LagrangeInterpolation(point, n, F, X)
    CALL NewtonInterpolation(point, n, F, X)

    CONTAINS 
    SUBROUTINE LagrangeInterpolation(point, n, F, X)
        IMPLICIT NONE

        REAL:: point, PnX
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
        WRITE(*, *)
        WRITE(*, fmt = '(A)') "Result of Lagrange Interpolation"
        WRITE(*, 11) point, PnX
        11 FORMAT("f(", f0.3, ") = ", f0.8/)
    END SUBROUTINE LagrangeInterpolation

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

        WRITE(*, fmt = '(A)') "Result of Newton Interpolation"
        WRITE(*, 11) point, QnX
        11 FORMAT("f(", f0.3, ") = ", f0.8/)
    END SUBROUTINE NewtonInterpolation

END PROGRAM Interpolation
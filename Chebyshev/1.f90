! Name: ARPIT KUMAR JAIN, Roll No: 180122009
PROGRAM ChebyshevInterpolationExp
    IMPLICIT NONE
    REAL, PARAMETER :: pi = acos(-1.0)
    INTEGER:: n, k, j
    REAL:: Xk, TnX, X, Px
    REAL, DIMENSION(:), ALLOCATABLE:: C

    WRITE(*, fmt = '(/A)', ADVANCE = "NO") "Enter Order of Interpolation Polynomial: "
    READ(*, *) n
    ALLOCATE(C(0:n));

    WRITE(*, 10, ADVANCE = "NO") n
    10 FORMAT(/, "Enter the X value to find P", i0, "(X): ")
    READ(*, *) X

    Px = 0.0
    ! Calculating the coefficients and calculation of polynomial 
    DO j = 0, n
        C(j) = 0.0
        DO k = 0, n
            Xk = cos(((2*k+1 )*pi)/(2*n+2))
            C(j) = C(j) + EXP(Xk) * TnX(xk, j)
        ENDDO
        IF(j .EQ. 0) THEN
            C(j) = C(j) * (1./(N+1))
        ELSE
            C(j) = C(j) * (2./(N+1))
        ENDIF
        Px = Px + C(j) * TnX(X, j)
    ENDDO

    WRITE(*, 11) X, Px
    11 FORMAT(/,"P(", f0.3, ") = ", f0.8/)

END PROGRAM ChebyshevInterpolationExp

FUNCTION TnX(x, n) 
    IMPLICIT NONE
    REAL, PARAMETER :: pi = acos(-1.0) 
    REAL:: x, TnX 
    INTEGER:: n, i
    REAL, DIMENSION(0:n):: vector
    vector(0) = 1
    vector(1) = x 
    IF(n .LE. 1) THEN 
        TnX = vector(n)
    ELSE
        DO i = 1, n-1 
            vector(i+1) = 2.0*x*vector(i) - vector(i-1) 
        ENDDO
        TnX = vector(n)
    ENDIF
    RETURN
END FUNCTION TnX
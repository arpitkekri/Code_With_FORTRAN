PROGRAM PIB1D_INFINITE_WELL
    IMPLICIT NONE

    REAL, PARAMETER :: PI = acos(-1.0)
    REAL, ALLOCATABLE:: potential(:), x(:)
    DOUBLE PRECISION, ALLOCATABLE:: hmat(:,:), eigenVal(:), work(:)
    DOUBLE PRECISION:: Expect_X
    REAL:: xmax, xmin, dx, hbar = 1.0, m = 1.0, tconst
    INTEGER:: nx, lwork, i, ifail, Expect_N, n

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of grid points: "
    READ(*, *) nx

    xmax = 5.0
    xmin = 0.0
    dx = (xmax - xmin)/(nx - 1)
    
    ALLOCATE(potential(nx), hmat(nx,nx), eigenVal(nx), x(nx), work(64*nx))
    DO i = 1, nx
        x(i) = xmin + (i-1)*dx
    ENDDO

    tconst = (hbar**2) / (2.0 * m * dx**2)
    hmat = 0.0

    potential = 0.0

    DO i = 1, nx-1
        hmat(i, i) = potential(i) + 2.0 * tconst
        hmat(i, i+1) = -tconst
        hmat(i+1, i) = -tconst
    ENDDO
    hmat(nx, nx) = potential(nx) + 2.0 * tconst
    
    lwork = 64*nx
    CALL dsyev('V', 'U', nx, hmat, nx, eigenVal, work, lwork, ifail)
    
    ! Sorting but no need as eigenVal is already sorted
    ! DO i = 1, nx
    !     DO j = i, nx
    !         IF(eigenVal(i) .GT. eigenVal(j)) THEN
    !             tmp = eigenVal(j)
    !             eigenVal(j) = eigenVal(i)
    !             eigenVal(i) = tmp
    !         ENDIF 
    !     ENDDO
    ! ENDDO

    OPEN(UNIT = 20, FILE = 'output.txt')
        ! WRITE(20, *) 'Grid Point (Value of x)         eigenValue Calculated           Exect EigenValue        WaveFunction'
        DO i = 1, nx
            WRITE(20, 10) x(i), eigenVal(i), i**2 * pi**2/(2.0 * (xmax-xmin)**2), (hmat(i, n)/sqrt(dx), n = 1, 5)
            10 FORMAT(f8.3, 3x, f0.3, 3x, f0.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x)
        ENDDO
    CLOSE(20)
    
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Expectation value for n = "

    ! Expect_N is a expectation value of quantum number N
    READ(*, *) Expect_N
    
    ! Expect_X is a expectation value of position x
    Expect_X = 0.0
    
    DO i = 1, nx 
        Expect_X = Expect_X + hmat(i, Expect_N) * x(i) * hmat(i, Expect_N) !*dx
    ENDDO

    WRITE(*, *) Expect_X

END PROGRAM PIB1D_INFINITE_WELL

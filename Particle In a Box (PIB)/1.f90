PROGRAM PIB1D_INFINITE_WELL
    IMPLICIT NONE

    REAL, PARAMETER :: pi = acos(-1.0)
    REAL, ALLOCATABLE:: pot(:), x(:)
    DOUBLE PRECISION, ALLOCATABLE:: hmat(:,:), work(:), diag(:)
    DOUBLE PRECISION:: xexpect
    REAL:: xmax, xmin, dx, hbar = 1.0, m = 1.0, tconst
    INTEGER:: nx, lwork, i, ifail, nexpect, n

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of grid points: "
    READ(*, *) nx

    xmax = 5.0
    xmin = 0.0
    dx = (xmax - xmin)/(nx - 1)
    
    ALLOCATE(pot(nx), hmat(nx,nx), diag(nx), x(nx), work(64*nx))
    DO i = 1, nx
        x(i) = xmin + (i-1)*dx
    ENDDO

    tconst = (hbar**2) / (2.0 * m * dx**2)
    hmat = 0.0

    pot = 0.0

    DO i = 1, nx-1
        hmat(i, i) = pot(i) + 2.0 * tconst
        hmat(i, i+1) = -tconst
        hmat(i+1, i) = -tconst
    ENDDO
    hmat(nx, nx) = pot(nx) + 2.0 * tconst
    
    lwork = 64*nx
    CALL dsyev('V', 'U', nx, hmat, nx, diag, work, lwork, ifail)
    
    ! Sorting but no need as diag is already sorted
    ! DO i = 1, nx
    !     DO j = i, nx
    !         IF(diag(i) .GT. diag(j)) THEN
    !             tmp = diag(j)
    !             diag(j) = diag(i)
    !             diag(i) = tmp
    !         ENDIF 
    !     ENDDO
    ! ENDDO

    OPEN(UNIT = 20, FILE = 'output.txt')
        ! WRITE(20, *) '#x    E1  Eexact    WaveFunction'
        DO i = 1, nx
            WRITE(20, 10) x(i), diag(i), i**2 * pi**2/(2.0 * (xmax-xmin)**2), (hmat(i, n)/sqrt(dx), n = 1, 5)
            10 FORMAT(f8.3, 3x, f0.3, 3x, f0.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x)
        ENDDO
    CLOSE(20)
    
    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Expectation value for n = "
    READ(*, *) nexpect
    xexpect = 0.0
    
    DO i = 1, nx 
        xexpect = xexpect + hmat(i, nexpect) * x(i) * hmat(i, nexpect) !*dx
    ENDDO

    WRITE(*, *) xexpect

END PROGRAM PIB1D_INFINITE_WELL

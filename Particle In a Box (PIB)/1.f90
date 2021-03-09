PROGRAM PIB1D_INFINITE_WELL
    IMPLICIT NONE

    REAL, ALLOCATABLE:: pot(:), hmat(:,:), diag(:), eigvec(:,:), x(:), work(:) 
    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: xmax, xmin, dx, hbar = 1.0, m = 1.0, tconst, tmp, xexpect
    INTEGER:: nx, lwork, i, j, ifail, nexpect

    integer n

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of grid points: "
    READ(*, *) nx

    xmax = 5.0
    xmin = 0.0
    dx = (xmax - xmin)/(nx - 1)
    
    ALLOCATE(pot(nx), hmat(nx,nx), diag(nx), eigvec(nx,nx), x(nx), work(64*nx))
    DO i = 1, nx
        x(i) = xmin + (i-1)*dx
    ENDDO

    tconst = (hbar**2) / (2.0 * m * dx**2)
    hmat = 0.0
    pot = 0.0
    DO i = 1, nx
        hmat(i, i) = pot(i) + 2.0 * tconst 
    ENDDO

    DO i = 1, nx
        DO j = i+1, nx 
            IF(abs(i-j) .EQ. 1) hmat(i, j) = -tconst 
        ENDDO 
        DO j = i-1, nx 
            IF(abs(i-j) .EQ. 1) hmat(i, j) = -tconst
        ENDDO 
    ENDDO
    
    DO i = 1, nx 
        ! WRITE(*, "(100f10.6)")(hmat(i, j), j = 1, i)
    ENDDO  
    
    lwork = 64*nx
    CALL dsyev('v', 'u', nx, hmat, nx, diag, work, lwork, ifail)
    
    DO i = 1, nx
        DO j = i, nx
            IF(diag(i) .GT. diag(j)) THEN
                tmp = diag(j)
                diag(j) = diag(i)
                diag(i) = tmp
            ENDIF 
        ENDDO
    ENDDO
    
    OPEN(UNIT = 20, FILE = 'output.txt')
        WRITE(20, *) '# Results'
        WRITE(20, *) '#n    E1  Eexact  wf'

        DO i = 1, 5
            WRITE(20, 10) x(i), diag(i), i**2 * pi**2/(2.0 * (xmax-xmin)**2), (hmat(i, n)/sqrt(dx), n = 1, 5)
            10 FORMAT(f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x, f8.3, 3x)
        ENDDO 
    CLOSE(20)
    
    PRINT *, "Expectation value of x for n = ?"
    READ(*, *) nexpect
    xexpect = 0.0
    
    DO i = 1, nx 
        xexpect = xexpect + hmat(i, nexpect) * x(i) * hmat(i, nexpect) !*dx
    ENDDO
    PRINT *, xexpect

END PROGRAM PIB1D_INFINITE_WELL

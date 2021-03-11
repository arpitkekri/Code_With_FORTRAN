PROGRAM PIB1D_Hydrogen_Radial
    IMPLICIT NONE

    REAL, PARAMETER :: PI = acos(-1.0)
    REAL, ALLOCATABLE:: potential(:), x(:)
    DOUBLE PRECISION, ALLOCATABLE:: hmat(:,:), eigenVal(:), work(:)
    DOUBLE PRECISION:: WF
    REAL:: xmax, xmin, dx, hbar, tconst, m, l
    INTEGER:: nx, lwork, i, ifail

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of grid points: "
    READ(*, *) nx
 
    xmax = 10.0
    xmin = 0.0
    dx = (xmax - xmin)/(nx)
    
    ALLOCATE(potential(nx), hmat(nx,nx), eigenVal(nx), x(nx), work(64*nx))
    DO i = 1, nx
        x(i) = xmin + (i)*dx
    ENDDO

    m = 1.0
    hbar = 1.00
    l = 0
    tconst = (hbar**2) / (2.0 * m * dx**2)

    DO i = 1, nx
        potential(i) = (hbar**2 * l * (l+1))/(2 * m * x(i) * x(i)) - 1/x(i)
    ENDDO
    
    hmat = 0.0
    DO i = 1, nx-1
        hmat(i, i) = potential(i) + 2.0 * tconst
        hmat(i, i+1) = -tconst
        hmat(i+1, i) = -tconst
    ENDDO
    hmat(nx, nx) = potential(nx) + 2.0 * tconst
    
    lwork = 64*nx
    CALL dsyev('V', 'U', nx, hmat, nx, eigenVal, work, lwork, ifail)

    OPEN(UNIT = 20, FILE = 'hydrogen.txt')
        DO i = 1, nx 
            WF = hmat(i, 3)/sqrt(dx)
            WRITE(20, 10) x(i), potential(i), eigenVal(i), hmat(i, 1)/sqrt(dx)
            10 FORMAT(f0.6, 3x, f0.6, 3x, f0.8, 3x, f0.8, 3x)
        ENDDO
    CLOSE(20)

END PROGRAM PIB1D_Hydrogen_Radial

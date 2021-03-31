! Name: Arpit Kumar Jain,  Roll No. 180122009
PROGRAM PIB1D_Amonnia
    IMPLICIT NONE

    REAL, PARAMETER :: PI = acos(-1.0)
    REAL, ALLOCATABLE:: potential(:), x(:)
    DOUBLE PRECISION, ALLOCATABLE:: hmat(:,:), eigenVal(:), work(:)
    DOUBLE PRECISION:: EnergyDiff
    REAL:: xmax, xmin, dx, hbar, tconst, b, k, x0, c, mu
    INTEGER:: nx, lwork, i, ifail

    WRITE(*, fmt = '(/A)', ADVANCE = 'NO') "Enter the number of grid points: "
    READ(*, *) nx
 
    xmax = 10.0
    xmin = 0.0
    dx = (xmax - xmin)/(nx - 1)
    
    ALLOCATE(potential(nx), hmat(nx,nx), eigenVal(nx), x(nx), work(64*nx))
    DO i = 1, nx
        x(i) = xmin + (i-1)*dx
    ENDDO

    mu = 4668
    hbar = 1.00
    k = 0.08
    x0 = 5.00
    b = 0.06
    c = 1.37

    tconst = (hbar**2) / (2.0 * mu * dx**2)
    DO i = 1, nx
        potential(i) = 0.5 * k * (x(i)-x0)**2 + b * EXP(-c * (x(i)-x0)**2 ) 
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

    OPEN(UNIT = 20, FILE = 'potential.txt')
        DO i = 1, nx 
            WRITE(20, 10) x(i), potential(i)
            10 FORMAT(f0.6, 3x, f0.6)
        ENDDO
    CLOSE(20)

    OPEN(UNIT = 20, FILE = 'eigenstate.txt')
        DO i = 1, nx 
            WRITE(20, 11) x(i), hmat(i, 1)/sqrt(dx), eigenVal(i)
            11 FORMAT(f0.6, 3x, f0.6, 3x, f0.8)
        ENDDO
    CLOSE(20)

    EnergyDiff = 6579689.75 * (eigenVal(2) - eigenVal(1))
    WRITE(*, *) "The difference between the second lowest and lowest is: ", EnergyDiff, "GHz"

END PROGRAM PIB1D_Amonnia

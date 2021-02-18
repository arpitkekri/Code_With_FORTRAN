PROGRAM  Particle_1D_Box
    IMPLICIT NONE

    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: bl, x, psi         ! bl is bond length
    CHARACTER*10:: filename
    INTEGER:: i, n, outunit
    bl = 1.
    WRITE(*, *) "Enter the quantum number n"
    READ *, n

    ! WRITE(fiblame, 10) "psi.", n
    ! 10 FORMAT(a, i0)
    WRITE(filename, fmt = "(a, i0)") "psi.", n
    OPEN(UNIT = outunit, FILE = filename, FORM = "formatted")
    x = -0.001
    DO i = 1, 1001
        x = x + 0.001
        psi = sqrt(2.0/bl) * sin(n * pi * x/bl);
        WRITE(outunit, *) x, psi, psi**2
    ENDDO
    CLOSE(outunit)
END
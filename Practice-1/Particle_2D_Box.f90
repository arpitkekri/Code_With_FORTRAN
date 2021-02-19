PROGRAM  Particle_2D_Box
    IMPLICIT NONE

    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: bl1, bl2, x, y, psi         ! bl is bond length
    CHARACTER*10:: filename
    INTEGER:: i, j, n1, n2, outunit
    bl1 = 1.
    bl2 = 1.
    WRITE(*, *) "Enter the quantum number n1, n2"
    READ *, n1, n2

    ! WRITE(fiblame, 10) "psi.", n
    ! 10 FORMAT(a, i0)
    WRITE(filename, fmt = "(a, i0, i0)") "psi.", n1, n2
    OPEN(UNIT = outunit, FILE = filename, FORM = "formatted")
    x = -0.01
    DO i = 1, 101
        x = x + 0.01
        y = -0.01
        DO j = 1, 101
            y = y + 0.01
            psi = 2.0 * sqrt(1./bl1*bl2) * sin(n1 * pi * x/bl1) * sin(n2 * pi * y/bl2);
            WRITE(outunit, *) x, y, psi, psi**2
        ENDDO
        WRITE(outunit, *)
    ENDDO
    CLOSE(outunit)
END

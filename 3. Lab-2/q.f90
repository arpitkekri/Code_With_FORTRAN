! ARPIT KUMAR JAIN Roll No - 180122009
PROGRAM  Arpit
    IMPLICIT NONE

    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: x, x0, delta, k0, u, Etrans, Gx
    CHARACTER*10:: filename
    INTEGER:: i, outunit
    x0 = 10.0
    delta = 0.25
    Etrans = 0.008
    u = 4056.5

    k0 = sqrt(2 * u * Etrans - (1./(2 * delta * delta)))

    WRITE(filename, fmt = "(a, i0)") "quiz1.", 1
    OPEN(UNIT = outunit, FILE = filename, FORM = "formatted")
    x = 0.35                    ! Starting point is 0.5 so in first iteration 0.35 + 0.15 = 0.5
    DO i = 1, 128               ! Grid points = 128
        x = x + 0.15            ! With dexX = 0.15  
        Gx = ((1. / (pi * delta * delta)) ** (1./4.)) * EXP(-((x - x0) * (x - x0)) / (2 * delta * delta)) ! No need of iota term it will be 1 after squre
        WRITE(outunit, * ) x, Gx ** 2
    ENDDO
    CLOSE(outunit)
END

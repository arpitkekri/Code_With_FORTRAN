! ARPIT KUMAR JAIN Roll No - 180122009
PROGRAM  Arpit
    IMPLICIT NONE

    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: x, x0, delta, k0, u, Etrans
    INTEGER:: i, outunit
    COMPLEX:: iota, Gx
    iota = (0.0, 1.0)
    x0 = 10.0
    delta = 0.25
    Etrans = 0.008
    u = 4056.5

    k0 = sqrt(2 * u * Etrans - (1./(2 * delta * delta)))

    OPEN(UNIT = outunit, FILE = 'quiz1.txt', FORM = "formatted")
    x = 0.35                    ! Starting point is 0.5 so in first iteration 0.35 + 0.15 = 0.5
    DO i = 1, 128               ! Grid points = 128
        x = x + 0.15            ! With dexX = 0.15  
        Gx = ((1. / (pi * delta * delta)) ** (1./4.)) * EXP(-((x - x0) * (x - x0)) / (2 * delta * delta)) * EXP(-iota * k0 * x) ! No need of iota term it will be 1 after squre
    !
        !
            !!!!!!! Disclaimer --> Do not exclude iota term even it will become 1 after computation --> My 3 marks gone for this single mistake
        !
    !
        WRITE(outunit, * ) x, abs(Gx*Gx)
    ENDDO
    CLOSE(outunit)
END

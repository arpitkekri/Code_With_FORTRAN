PROGRAM  three
    IMPLICIT NONE
    REAL:: x, xe, beta, De, MorseVx, HarmonicVx
    CHARACTER*10:: filename
    INTEGER:: i, outunit
    xe = 0.74
    beta = 1.0
    De = 109.4 * 1.59 * (10**(-3.0))

    WRITE(filename, fmt = "(a, i0)") "output.", 1
    OPEN(UNIT = outunit, FILE = filename, FORM = "formatted")
    x = -4.2                   
    DO i = 1, 100           
        x = x + 0.20
        MorseVx = De * (1 - EXP(-beta * (x - xe))) * (1 - EXP(-beta * (x - xe)))
        HarmonicVx = De * beta * beta * (x-xe) * (x-xe)
        WRITE(outunit, * ) x, MorseVx, HarmonicVx
    ENDDO
    CLOSE(outunit)
END

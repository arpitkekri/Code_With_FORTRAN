PROGRAM two
    IMPLICIT NONE
    REAL:: sumlnX, sumlnY, sumlnXSqure, sumlnXY, b, lnA
    REAL, DIMENSION(5):: x, y
    INTEGER:: n, i
    n = 5
    sumlnX = 0.0
    sumlnY = 0.0
    sumlnXSqure = 0.0
    sumlnXY = 0.0
    DO i = 1, n
        READ(*, *) x(i), y(i)
        sumlnX = sumlnX + log(x(i))
        sumlnY = sumlnY + log(y(i))
        sumlnXY = sumlnXY + log(x(i)) * log(y(i))
        sumlnXSqure = sumlnXSqure + log(x(i)) * log(x(i))
    ENDDO

    b = (n*sumlnXY - sumlnX * sumlnY) / (n * sumlnXSqure - sumlnX * sumlnX)
    lnA = (1.0/n) * (sumlnY - b * sumlnX) 

    WRITE(*, *) b, lnA
END

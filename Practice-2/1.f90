PROGRAM one
    IMPLICIT NONE
    REAL:: sumX, sumY, sumXSqure, sumXY, m, c, St, Sr, meanY, rSqure
    REAL, DIMENSION(5):: x, y
    INTEGER:: n, i
    n = 5
    sumX = 0.0
    sumY = 0.0
    sumXSqure = 0.0
    sumXY = 0.0
    DO i = 1, n
        READ(*, *) x(i), y(i)
        sumX = sumX + x(i)
        sumY = sumY + y(i)
        sumXY = sumXY + x(i) * y(i)
        sumXSqure = sumXSqure + x(i) * x(i)
    ENDDO
    meanY = sumY/n

    m = (n * sumXY - sumX * sumY) / (n * sumXSqure - sumX * sumX)
    c = (1.0/n) * (sumY - m * sumX) 
    St = 0.0
    Sr = 0.0
    DO i = 1, n
        St = St + (y(i) - meanY) * (y(i) - meanY) 
        Sr = Sr + (y(i) - c - m * x(i)) * (y(i) - c - m * x(i))
    ENDDO
    rSqure = (St - Sr) / St
    WRITE(*, *) "Goodness of Fit is", rSqure 
    WRITE(*, *) m, c 
END

PROGRAM  question4
    IMPLICIT NONE

    INTEGER:: i, n 
    REAL, DIMENSION(:), ALLOCATABLE:: x 
    REAL:: sum = 0.0, mean, variance

    WRITE(*, *) "Enter dimention of the array"
    READ(*, *) n
    ALLOCATE(x(n))
    WRITE(*, 10) n
    10 FORMAT(/,"Enter ", i0, " real number for this array")
    READ(*, *) (x(i), i = 1, n)

    DO i = 1, n
        sum = sum + x(i)
    ENDDO

    mean = sum/n
    sum = 0.0
    DO i = 1, n
        sum = sum + (x(i) - mean)**2
    ENDDO
    variance = sum/(n-1)

    WRITE(*, 11) mean, variance
    11 FORMAT(/,"mean of data = ", f0.3/, "variance of the data = ", f0.3/)

END
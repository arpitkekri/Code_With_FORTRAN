PROGRAM factorial_program
    IMPLICIT NONE

    INTEGER :: n, factorial = 1, i
    PRINT *, "Enter number: " 
    READ *, n
    DO i = 1, n
        factorial = factorial * i 
    ENDDO
    WRITE(*, 10) factorial 
    10 FORMAT("Factorial is ", i5)

END
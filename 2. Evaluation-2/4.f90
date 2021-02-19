! Name = ARPIT KUMAR JAIN ROLLNO = 180122009
PROGRAM question4
    IMPLICIT NONE

    INTEGER :: n, factorial = 1, i
    PRINT *, "Enter number: " 

    ! reading integer from screen 
    READ *, n

    ! finding factorial by looping 1 to n and storing multiplication on factorial variable
    DO i = 1, n
        factorial = factorial * i
    ENDDO

    ! printing factorial value on screen
    print *, factorial

    STOP
END PROGRAM
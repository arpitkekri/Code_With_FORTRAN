PROGRAM  question2
    IMPLICIT NONE

    INTEGER:: i, j
    REAL, DIMENSION(3, 5):: matrix, tempMat

    OPEN(UNIT = 11, FILE = "q2_input.txt", STATUS = "OLD", ACTION = "READ")
    DO i = 1, 3
        READ (11, *) (matrix(i, j), j = 1, 5)
    ENDDO
    CLOSE(11)

    OPEN(UNIT = 12, FILE = "q2_output.txt", ACTION = "WRITE", POSITION = "APPEND")
    
    WRITE(12, *) "The Original Matrix is" 
    DO i = 1, 3
        WRITE(12, *) (matrix(i, j), j = 1, 5)
    ENDDO
    WRITE(12, *)
    
    ! 2a ==> Multiply each of these numbers by 10, and write the output to another text file.
    
    tempMat = matrix
    tempMat = tempMat * 10
    
    WRITE(12, *) "2a. After Multiply with 10 matrix is"
    DO i = 1, 3
        WRITE(12, *) (tempMat(i, j), j = 1, 5)
    ENDDO
    WRITE(12, *)
    ! -------------------------------------------------------------------------------------- 


    ! 2b ==> Multiply elements of the second row by 5.5, and print the whole array.

    DO j = 1, 5
        matrix(2, j) = matrix(2, j) * 5.5
    ENDDO

    WRITE(12, *) "2b. After Multiply Second Row with 5.5 the array is" 
    WRITE(12, *) (matrix(2, j), j = 1, 5)
    WRITE(12, *)
    ! -----------------------------------------------------------------------------


    ! 2c ==> Multiply elements of the 3rd column by 5.5, and print the whole array.
    
    DO j = 1, 5
        matrix(3, j) = matrix(3, j) * 5.5
    ENDDO

    WRITE(12, *) "2c. After Multiply Third Row with 5.5 the array is" 
    WRITE(12, *) (matrix(3, j), j = 1, 5)
    ! -------------------------------------------------------------------------------

    CLOSE(12)

END
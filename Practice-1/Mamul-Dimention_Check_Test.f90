PROGRAM  matmulCheck
    IMPLICIT NONE

    REAL, ALLOCATABLE :: A(:, :), B(:, :)
    REAL:: D(10, 10) = 0 
    INTEGER:: L, M, N, O,  i, j
    
    WRITE(*, *) "Please Enter L M" 
    READ(*, *) L, M
    ALLOCATE(A(L, M))
    
    WRITE(*, *) "Enter Matrix A()"
    DO i = 1, L
        READ(*, *) (A(i,j), j=1,M) ! A() is L-by-M
    ENDDO

    WRITE(*, *) "Please Enter N O" 
    READ(*, *) N, O 
    ALLOCATE(B(N, O))
    
    WRITE(*, *) "Enter Matrix B()"
    DO i = 1, N 
        READ(*, *) (B(i,j), j=1, O) ! B() is N-by-O
    ENDDO

    ! Matmul will be check for column(A) = row(B) or not => If not than give runtime error
    D = matmul(a, b)
    WRITE(*, *) "Matrix D calculated from matmul library is "
    DO i = 1, L
        Write(*, *) (D(i,j), j=1, O)
    ENDDO
    
END
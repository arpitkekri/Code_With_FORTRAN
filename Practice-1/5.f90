PROGRAM  question5
    IMPLICIT NONE

    REAL, ALLOCATABLE, DIMENSION(:, :) :: A, B, C, D
    INTEGER:: L, M, N, i, j, k
    
    WRITE(*, *) "A() is L by M .and. B() is M by N ==> So final result C() will be L by N"
    WRITE(*, *) "Please Enter L, M and N" 
    READ(*, *) L, M, N
    ALLOCATE(A(L, M))
    ALLOCATE(B(M, N))
    ALLOCATE(C(L, N))
    ALLOCATE(D(L, N))  

    WRITE(*, *) "Enter Matrix A()"
    DO i = 1, L
        READ(*, *) (A(i,j), j=1,M) ! A() is L-by-M
    ENDDO

    WRITE(*, *) "Enter Matrix B()"
    DO i = 1, M
        READ(*, *) (B(i,j), j=1, N) ! B() is M-by-N
    ENDDO

    DO i = 1, L
        DO j = 1, N
            C(i,j) = 0
            DO k = 1, M                         ! (row i of A)*(col j of B)
                C(i,j) = C(i,j) + A(i,k)*B(k,j)
            ENDDO
        ENDDO
    ENDDO

    WRITE(*, *)
    WRITE(*, *) "Matrix C:"
    DO i = 1, L
        Write(*, *) (C(i,j), j=1,N)
    ENDDO
    WRITE(*, *)

    D = matmul(a, b)
    WRITE(*, *) "Matrix D: (calculated from matmul library)"
    DO i = 1, L
        Write(*, *) (D(i,j), j=1,N)
    ENDDO

    WRITE(*, *) 
    IF(All(C == D)) WRITE(*, *) "C and D is same."
    
END
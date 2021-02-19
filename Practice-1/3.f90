PROGRAM  question3
    IMPLICIT NONE
    ! ----------------- Static Dimension Statements ------------------- !
    ! INTEGER :: i, j, k
    ! REAL, DIMENSION(3):: a, b, sum

    ! WRITE(*, *) "Enter 3 Real Number for first vector in a line"
    ! READ(*, *) (a(i), i = 1, 3)
    ! WRITE(*, *) "Enter 3 Real Number for Second vector in a line"
    ! READ(*, *) (b(j), j = 1, 3)
    
    ! DO k = 1, 3
    !     sum(k) = a(k) + b(k)
    ! ENDDO

    ! WRITE(*, *) "The sum of these vector is"
    ! WRITE(*, *) (sum(k), k = 1, 3)
    ! WRITE(*, *) "The lenghth of final vector is = 3"


    ! ----------------- Allocatable Arrays ------------------- ! 
    INTEGER :: i, j, k, n, m, sum_size
    REAL, DIMENSION(:), ALLOCATABLE:: a, b, sum
    
    10 FORMAT("Enter ", i0, " real number for this vector: ")

    WRITE(*, '(A)', ADVANCE = "NO") "Enter dimention of first vector: "
    READ(*, *) n
    WRITE(*, *)
    ALLOCATE(a(n))
    WRITE(*, 10, ADVANCE = "NO") n
    READ(*, *) (a(i), i = 1, n)
    
    WRITE(*, *)
    WRITE(*, '(A)', ADVANCE = "NO") "Enter dimention of second vector: "
    READ (*, *) m
    ALLOCATE(b(m))
    WRITE(*, 10, ADVANCE = "NO") m
    READ(*, *) (b(j), j = 1, m)
    
    sum_size = MAX(n, m)
    ALLOCATE(sum(sum_size))

    DO k = 1, sum_size
        IF(n == 0) THEN
            sum(k) = b(k)
        ELSE IF(m == 0) THEN
            sum(k) = a(k)
        ELSE 
            sum(k) = a(k) + b(k)
            n = n-1
            m = m-1
        ENDIF
    ENDDO

    WRITE(*, *)
    WRITE(*, '(A)', ADVANCE = "NO") "The sum of these vector is: "
    WRITE(*, *) (sum(k), k = 1, sum_size)

    WRITE(*, 11) sum_size
    11 FORMAT(/,"Length of final vector = ", i0/)

END
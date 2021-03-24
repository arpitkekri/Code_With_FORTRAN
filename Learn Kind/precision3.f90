MODULE precision
    ! dp = double precision
    INTEGER, PARAMETER:: dp = SELECTED_REAL_KIND(12)
END MODULE precision

PROGRAM test
    USE precision
    IMPLICIT NONE

    REAL(KIND = dp), DIMENSION(20000000):: A
    REAL(KIND = dp):: X
    X = SIZE(A)-1
    PRINT *, KIND(x), x
END PROGRAM 

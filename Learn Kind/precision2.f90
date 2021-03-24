PROGRAM accuracy
    IMPLICIT NONE

    INTEGER, PARAMETER  ::  double = SELECTED_REAL_KIND (13)
    REAL (KIND = double) :: A, B, C

    A = 2.0_double
    B = 0.1 * A
    C = 0.1_double * A

    PRINT *, B
    PRINT *, C

END PROGRAM accuracy

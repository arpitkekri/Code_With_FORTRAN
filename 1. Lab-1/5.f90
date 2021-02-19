PROGRAM question5
    IMPLICIT NONE
    real :: x = 2
    print *, (1./3)*log(1 + sqrt(x)) + (1./15.)*(2. + sqrt(x))
    STOP
END PROGRAM question5
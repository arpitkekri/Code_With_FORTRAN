PROGRAM  question1
    IMPLICIT NONE

    INTEGER :: i = 0
    REAL :: x, exp = 1.0, term = 1.0
    WRITE(*, '(A)', ADVANCE = "NO") "Enter x for calculate exp(x): "
    READ(*, *) x

    ! Infinite DO loop untill it terminates 
    DO
        term = term * (x/(i+1))     ! Calculating only one term
        if(term < 10**(-5.0)) exit  ! Exit from DO loop and continue after DO loop
        exp = exp + term            !  Adding new term in the exponent
        i = i + 1                   ! Increment i for next step
    ENDDO

    WRITE(*, 10) x, exp          ! Printing Output to the console
    10 FORMAT("exp(", f0.3, ") = ", f0.3)

END 

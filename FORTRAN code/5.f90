PROGRAM question5
    IMPLICIT NONE

    ! REAL:: A, B, C
    ! INTEGER:: I
    ! A = 1.5
    ! B = 2.0
    ! C = A/B
    ! I = A / B
    ! PRINT*, C,I

    ! INTEGER I,J,K
    ! REAL ANSWER
    ! I=5
    ! J=2
    ! K=4
    ! ANSWER=I/J*K
    ! PRINT*,ANSWER

    ! INTEGER :: n = -12345
    ! write(*, 10) n 
    ! 10 format(I6) ! same as 10 format(i6) // small 'i' and big 'I' is same (same for f/F and e/E)

    ! REAL:: r =  -14.7148, n = 1.8
    ! write(*, 10) r, n  
    ! 10 format("The area is = ", f8.4/, "The number = " f3.1)

    ! Real :: exp = -0.589178E-14
    ! write(*, 10) exp  
    ! 10 format(E13.7) 
    ! REAL :: exp = 1.00001234
    ! write(*, 10) exp
    ! 10 format(E14.8)
    ! REAL :: exp = .12345E8
    ! write(*, 10) exp
    ! 10 format(ES10.4)

    ! INTEGER:: a = 789, b = -34
    ! write(*, 10) a, b
    ! 10 format(i3, 2x, i3) 

    Character(6)::speech1, speech2
    Read(*,'(2a)')speech1,speech2
    Read(*, 10)speech1,speech2
    10 format(a6, a6)
    write(*, *) speech1
    write(*, *) speech2
END

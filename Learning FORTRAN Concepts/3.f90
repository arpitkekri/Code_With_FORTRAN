PROGRAM complex
    implicit none
    real, parameter :: pi = 3.141592 ! Declaration of a constant like a #define in c++
    ! real:: a = 5.90, b = 7.2
    complex :: cpm 
    ! cpm = a + (0, 1)*b  ! Assigning 2 real number to a complex number
    read(*, *) cpm ! In the console give input as ------>  (1.56, -8.3)
    print *, cpm, real(cpm), aimag(cpm)
        cpm = cpm*(0., 1.)
    print *, cpm
END
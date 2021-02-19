! Name = ARPIT KUMAR JAIN ROLLNO = 180122009
PROGRAM  question3
    IMPLICIT NONE
    
    integer :: i
    real:: sum = 0.0, j
    ! Looping from 1 to 40
    do i = 1, 40
        ! finding only even number 
        if(mod(i, 2) == 0) then
            ! converting ineger to real by multipling it to 1.0
            j = 1.0 * i 
            ! Taking squre root of real number j which is equal to i and 
            ! adding the squre root to sum 
            sum = sum + sqrt(j)
        endif 
    enddo
    ! printing sum on screen
    print *, sum

    STOP
END PROGRAM
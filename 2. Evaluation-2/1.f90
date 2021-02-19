! Name = ARPIT KUMAR JAIN ROLLNO = 180122009
PROGRAM  question1
    IMPLICIT NONE

    integer :: a, b, c, largest
    ! Reading 3 integer a, b and c from screen 
    read *, a, b, c 

    !  Finding maximum of these 3 integer 
    if(a >= b .and. a >= c) then
        largest = a 
    else if(b >= a .and. b >= c) then 
        largest = b 
    else 
        largest = c
    endif 

    ! printing the largest value on screen
    print *, largest

    STOP
END PROGRAM
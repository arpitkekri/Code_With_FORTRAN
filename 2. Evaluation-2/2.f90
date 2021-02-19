! Name = ARPIT KUMAR JAIN ROLLNO = 180122009
PROGRAM  question2
    IMPLICIT NONE
    
    character*2 :: grade  
    integer :: n
    ! Reading marks from console and storing it in variable n
    read *, n

    !  defining the grade by using select case 
    SELECT CASE(n)
    case(90:100)
        grade = "AA"
    case(75:89)
        grade = "AB"
    case(65:74)
        grade = "BB"
    case(55:64)
        grade = "BC"
    end select 

    ! Opening the file named output.txt
    open(unit=10,file='output.txt')
    ! Write grade on that file
    write(10, *) grade 
    ! Closing the file output.txt
    close(10)

    STOP
END PROGRAM
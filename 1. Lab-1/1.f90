PROGRAM question1
    IMPLICIT NONE
    character:: name*20
    INTEGER :: d, m, y
    PRINT *, 'type your name, up to 20 characters'
    PRINT *, "enclosed in quotes (for taking sapace as an input)"
    READ *, name
    print *, "Enter your date of birth date month year (separate by space)"
    read *, d, m, y
    PRINT *, d,".", m, ".", y, " is the date of birth of ", name
    STOP
END PROGRAM question1
PROGRAM CONVERT_TEMP
    IMPLICIT NONE
    REAL centigrade, fahrenheit
    PRINT *, "Enter the temprature in centigrade"
    READ *, centigrade
    ! Perform the calculation
    fahrenheit = (centigrade*9.0 / 5.0) + 32.0
    PRINT *, " Fahrenheit temprature is : ", fahrenheit
    PRINT *, 10**4 ! 
    STOP
END
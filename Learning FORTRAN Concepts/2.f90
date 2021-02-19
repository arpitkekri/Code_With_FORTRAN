PROGRAM AVRAGE
    ! This program reads in three numbers and Sums 
    ! and average them. 
    ! with :: we can assign variable while declaring 
    IMPLICIT NONE
    REAL :: Num1, Num2, NUM3, Average, Total = 0.0
    INTEGER :: N = 3
    PRINT *, "Type three numbers"
    READ *, Num1, Num2, NUM3
    Total = Num1 + Num2 + NUM3
    Average = Total / N
    PRINT *, "Total OF  NUMBERS IS = ", Total
    PRINT *, " AVERAGE IS = ", Average
END
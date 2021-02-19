PROGRAM inout
! This program reads in and prints out a name
! character*20 :: name
character (len = 20):: f_name, l_name
! PRINT *, 'type your name, upto 20 characters'
! PRINT *, "enclosed in quotes (for taking sapace as an input)"
! READ *, name
! PRINT *, name
READ *, f_name, l_name
PRINT *, "Hello ", trim(f_name), " ", trim(l_name)
! STOP  ! Like a {break;} in c/c++
END PROGRAM
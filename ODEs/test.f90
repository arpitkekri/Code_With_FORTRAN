Program test 
    integer:: n = 4 
    real:: x(4), y

    n = 4
    x = 2
    y = 1
    print *, fun(x, y)
    contains 

    function fun(x, y)
        real:: x(4)
        real :: y
        real, dimension(4) :: fun
        fun = x + y
    end 
END

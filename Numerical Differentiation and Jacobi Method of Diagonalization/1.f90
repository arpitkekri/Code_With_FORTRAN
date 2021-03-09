PROGRAM NumericalDifferentiation
    IMPLICIT NONE 
    REAL:: x, h  
    READ(*, *) x 
    
    h = 0.1
    WRITE(*, *) h, f(x+h), (f(x+h) - f(x))/h, h/(2*x*x)

    h = 0.05
    WRITE(*, *) h, f(x+h), (f(x+h) - f(x))/h, h/(2*x*x)

    h = 0.01
    WRITE(*, *) h, f(x+h), (f(x+h) - f(x))/h, h/(2*x*x)

    h = 0.001
    WRITE(*, *) h, f(x+h), (f(x+h) - f(x))/h, h/(2*x*x)


    CONTAINS 
    REAL FUNCTION f(x)
        IMPLICIT NONE 
        REAL:: x
        f = log(x)
        RETURN 
    END FUNCTION f
END PROGRAM NumericalDifferentiation
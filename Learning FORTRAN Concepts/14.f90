MODULE Circle
    REAL, PARAMETER :: Pi = 3.1415927
    REAL :: radius
END MODULE Circle

PROGRAM Area
    USE Circle, ONLY : radius
    IMPLICIT NONE
    real:: AreaCircle
    ! Prompt user for radius of circle
    write(*, '(A)', ADVANCE = "NO") "Enter the radius of the circle: "
    read(*, *) radius
    ! Write out area of circle using function call
    write(*, 100) "Area of circle with radius", radius, " is", AreaCircle(radius)
    100 format (A, 2x, F6.2, A, 2x, F11.2)
END PROGRAM Area

REAL FUNCTION AreaCircle(r)
    USE Circle, ONLY : Pi
    IMPLICIT NONE
    REAL :: r
    AreaCircle = Pi * r * r
END FUNCTION
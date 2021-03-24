program test
implicit none
!REAL, DIMENSION(20000000) :: A
!REAL :: X
REAL(kind=8), DIMENSION(20000000) :: A
REAL(kind=8) :: X
X = SIZE(A)-1
PRINT* , x,kind(x),kind(0),kind(0.0)
END

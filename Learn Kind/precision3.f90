module pres
integer,parameter::dp=selected_real_kind(12)
end module pres

program test
use pres
implicit none
REAL(kind=dp), DIMENSION(20000000) :: A
REAL(kind=dp) :: X
X = SIZE(A)-1
PRINT* , kind(x),x!,kind(0),kind(0.0)
END

! Name: ARPIT KUMAR JAIN, Roll No: 180122009
! Home Assignment Programme-1
! UV-vis & CD spectra
!
PROGRAM HA_Q1

    IMPLICIT NONE

    ! Declaration of variables 
    REAL, PARAMETER :: pi = acos(-1.0)
    REAL:: sigma, nu, nu_i, E_nu, deltaE_nu
    REAL, DIMENSION(40):: lamda_i, fosc, Rstr
    CHARACTER*11:: filename
    INTEGER:: i, lamda, outunit, inunit
    
    ! σ = 2480 cm−1
    sigma = 2480

    OPEN(UNIT = inunit, FILE = "input.dat")                 ! Open the input file
        DO i = 1, 40                                        ! loop over input dataset
            READ(inunit, *) lamda_i(i), fosc(i), Rstr(i)    ! Read the input
        ENDDO
    CLOSE(inunit)

    WRITE(filename, fmt = "(A)") "output1.dat"  ! filename = output1.dat

    OPEN(UNIT = outunit, FILE = filename, FORM = "formatted")   ! Open the output file
        DO lamda = 200, 600

            ! V = 1/λ, λ in "nm" and V in "cm" so multiply by 10^7
            nu = 10**7 / lamda               

            ! Set E_nu and deltaE_nu = 0.0 in each iteration so that for each nu we have new E and delE 
            E_nu = 0.0
            deltaE_nu = 0.0

            DO i = 1, 40
                nu_i = 10**7 / lamda_i(i)                   ! Vi = 1/λi, λi in "nm" and Vi in "cm" so multiply by 10^7

                ! Calculation for E_nu & deltaE_nu "SUM i = 1 to n" part
                E_nu = E_nu + fosc(i) * EXP(-( ( (nu - nu_i) / sigma) * ( (nu - nu_i) / sigma)))
                deltaE_nu = deltaE_nu + nu_i * Rstr(i) * EXP(-( ( (nu - nu_i) / sigma) * ( (nu - nu_i) / sigma)))
            ENDDO
            
            ! Multiply E_nu with constant part which is not effected by SUM over 1 to n 
            E_nu = E_nu * 1.3062974 * (10**8) / sigma

            ! Multiply deltaE_nu with its constant part 
            deltaE_nu = deltaE_nu * ( 1.0 / (22.96 * sqrt(pi) * sigma))     ! (10^-40) / (2.296 * 10^-39) = 1 / 22.96 

            WRITE(outunit, *) lamda, E_nu, deltaE_nu                        ! Write the Output in file
        ENDDO
    CLOSE(outunit)
END PROGRAM HA_Q1

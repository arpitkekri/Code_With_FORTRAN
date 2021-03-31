! Name: Arpit Kumar Jain Roll No: 180122009
PROGRAM PIB_Tunneling_HW5
    IMPLICIT NONE
    
    ! Declaration of Variables
    COMPLEX, PARAMETER:: iota = (0., 1.)
    REAL, ALLOCATABLE:: potential(:), x(:), probabilityDensity(:)
    COMPLEX, ALLOCATABLE:: psi(:)
    REAL:: xmax, xmin, dx, E, k, maxima, minima, Pavg, transmissionProbability
    INTEGER:: nx, i

    ! nx is number of grid points
    nx = 1000
    xmax = 10.000
    xmin = 0.000
    dx = 0.010
    ALLOCATE(potential(0:nx), x(0:nx), psi(0:nx), probabilityDensity(0:nx))

    ! Defining grid points
    DO i = 0, nx
        x(i) = xmin + (i)*dx
    ENDDO

    ! Potential Array with barrier
    DO i = 0, nx
        IF(x(i) .LT. 4) THEN 
            potential(i) = 0.00
        ELSE IF(x(i) .LE. 5) THEN 
            potential(i) = 9.00
        ELSE
            potential(i) = 0.00
        ENDIF
    ENDDO

    ! Calculation of transmission_probability for E = 1 eV to 26 eV with deltaE = 0.1 eV
    OPEN(UNIT = 20, FILE = 'transmission_probability.txt')
        
        ! Initialize E to 1.0 eV 
        E = 1.000
        ! Loop till E is less than 26.1 eV means till E = 26.000 eV 
        DO WHILE(E .LT. 26.100)
            
            ! k = sqrt(2mE/hbar**2) and m = hbar = 1 so
            k = sqrt(2*E)
            
            ! ψ0 = exp(−ikx) = 1 because x = 0.00
            psi(0) = (1., 0.)

            ! ψ1 = exp(−ik×dx) 
            psi(1) = EXP(-iota * k * dx)

            ! Calculate all other ψ(j+1) with ψ(j) and ψ(j-1) with finite-difference method
            DO i = 1, nx-1
                psi(i+1) = (2 - 2 * (E - potential(i)) * dx * dx) * psi(i) - psi(i-1)
            ENDDO

            ! probability density =  |ψ x ψ*|
            probabilityDensity = abs(psi * conjg(psi))

            ! Calculate maxima and minima of probabilityDensity curve after 600th point 
            ! (Far away point so no effect of barrier)
            maxima = 0.0
            minima = 100000.0
            DO i = 600, nx
                maxima = max(maxima, probabilityDensity(i))
                minima = min(minima, probabilityDensity(i))
            ENDDO

            Pavg = (maxima + minima) / 2.0
            transmissionProbability = 2.0 / (1 + Pavg)

            ! Write transmissionProbability in file named 'transmission_probability.txt'
            WRITE(20, 10) E, transmissionProbability
            10 FORMAT(f6.3, 3x, f5.3)

            ! If our Energy is 9.000 than write this probabilityDensity in file named 'probability_density.txt' 
            ! E = 9.0000012 there is some digit at 6th digit so direct (E .EQ. 9.0) does not work
            IF (abs(E - 9.000) < 0.0001) THEN
                OPEN(UNIT = 21, FILE = 'probability_density.txt')
                ! WRITE(*, *) "plot '-' using 1:2 with lines"
                    DO i = 0, nx
                        WRITE(21, 11) x(i), probabilityDensity(i)
                        11 FORMAT(f6.3, 3x, f6.3)
                    ENDDO
                CLOSE(21)
            ENDIF
            
            ! Increase E by 0.100 (deltaE)
            E = E + 0.100
        ENDDO
    CLOSE(20)
    ! CALL execute_command_line('gnuplot -p plot.gnu')

END PROGRAM PIB_Tunneling_HW5

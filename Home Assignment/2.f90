! Name: ARPIT KUMAR JAIN, Roll No: 180122009
! Home Assignment Programme-2
! Radial Distribution function (RDF) for 1s, 2s, 3s

MODULE RadialDistributionFunction
    ! Declaration of variables
    IMPLICIT NONE
    REAL, PARAMETER :: pi = acos(-1.0)
    INTEGER:: i, size
    REAL, DIMENSION(:), ALLOCATABLE:: RDF

    CONTAINS
    SUBROUTINE CalculateRDF(orbitalNumber, r, deltaR)
        IMPLICIT NONE
        INTEGER:: orbitalNumber
        REAL:: r, deltaR, psi
        CHARACTER*10:: filename

        size = INT(r/deltaR) + 1
        ALLOCATE(RDF(size))

        r = 0.0
        DO i = 1, size
            psi = waveFunction(orbitalNumber, r)
            RDF(i) = (psi * psi) * 4.0 * pi * r * r
            r = r + 0.05
        ENDDO

        CALL SCALE(RDF, size)
        WRITE(filename, fmt = "(a, i1, a)") "output.", orbitalNumber, "s"
        OPEN(UNIT = 20, FILE = filename, FORM = "formatted")
            r = 0.0
            DO i = 1, size 
                WRITE(20, *) r, RDF(i)
                r = r + 0.050
            ENDDO
        CLOSE(20)
    END SUBROUTINE CalculateRDF

    REAL FUNCTION waveFunction(orbitalNumber, r)
        IMPLICIT NONE 
        REAL:: r 
        INTEGER:: orbitalNumber
        IF(orbitalNumber .EQ. 1) waveFunction = EXP(-r/2.0)
        IF(orbitalNumber .EQ. 2) waveFunction = (1.0/SQRT(32.0)) * (2.0-R) * EXP(-R/2.0)
        IF(orbitalNumber .EQ. 3) waveFunction = (1.0/SQRT(972.0)) * (6 - 6 * R + R * R) * EXP(-R/2.0)
    END FUNCTION waveFunction

    SUBROUTINE SCALE(RDF, size)
        IMPLICIT NONE
        INTEGER:: size
        REAL, DIMENSION(size):: RDF
        REAL:: BIGGEST = 0.0
        DO i = 1, size 
            IF(BIGGEST .LT. RDF(i)) BIGGEST = RDF(i)
        ENDDO

        DO i = 1, size 
            RDF(i) = RDF(i) / BIGGEST
        ENDDO
    END SUBROUTINE SCALE

END MODULE RadialDistributionFunction

! Main Program 
PROGRAM HA_Q2
    USE RadialDistributionFunction

    IMPLICIT NONE
    INTEGER:: orbitalNumber
    REAL:: radius = 2.0                 ! Change this r value to 20 then you can see full graph
    REAL:: deltaR = 0.05

    1 PRINT *, "Enter Orbital Number to Calculate RDF...."
    PRINT *, "For 1s orbital Enter:  1"
    PRINT *, "For 2s orbital Enter:  2"
    PRINT *, "For 3s orbital Enter:  3" 
    READ (*, *) orbitalNumber

    IF((orbitalNumber >= 1) .AND. (orbitalNumber <= 3)) then 
        CALL CalculateRDF(orbitalNumber, radius, deltaR)
    ELSE
        Print *, "Please Enter Correct Orbital Number ... "
        Print *
        GOTO 1
    ENDIF
END PROGRAM HA_Q2

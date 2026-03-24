      SUBROUTINE UDIST(M, N, FRQNCY, CDF, LFR, WORK, LWRK, IFAULT)
!
!     AS 62 generates the frequencies for the Mann-Whitney U-statistic.
!     Users are much more likely to need the distribution function.
!     Code to return the distribution function has been added at the end
!     of AS 62 by Alan Miller.   Remove the C's in column 1 to activate
!     it.
!
!     Note: Dataplot computes the "W" statistic for the Mann-Whitney
!           rank sum test.  The U statistic is then
!
!             U = n1*n2 + 0.5*n1*(n1 + 1) - W
!
!           The U version of the test can use:
!
!              LET U = MANN WHITNEY U STATISTIC Y1 Y2
!              LET X FREQ CDF = MANN WHITNEY U STATISTIC FREQUENCY Y1 Y2
!
!
!     ALGORITHM AS 62  APPL. STATIST. (1973) VOL.22, NO.2
!
!     The distribution of the Mann-Whitney U-statistic is generated for
!     the two given sample sizes
!
      INTEGER M, N, LFR, LWRK, IFAULT
      REAL FRQNCY(LFR), CDF(LFR), WORK(LWRK)
!
!     Local variables
!
      INTEGER MINMN, MN1, MAXMN, N1, I, IN, L, K, J
      REAL ZERO, ONE, SUM
      DATA ZERO /0.0/, ONE /1.0/
!
!     Check smaller sample size
!
      IFAULT = 1
      MINMN = MIN(M, N)
      IF (MINMN .LT. 1) RETURN
!
!     Check size of results array
!
      IFAULT = 2
      MN1 = M * N + 1
      IF (LFR .LT. MN1) RETURN
!
!     Set up results for 1st cycle and return if MINMN = 1
!
      MAXMN = MAX(M, N)
      N1 = MAXMN + 1
      DO 1 I = 1, N1
         FRQNCY(I) = ONE
    1 CONTINUE
      IF (MINMN .EQ. 1) GO TO 4
!
!     Check length of work array
!
      IFAULT = 3
      IF (LWRK .LT. (MN1 + 1) / 2 + MINMN) RETURN
!
!     Clear rest of FREQNCY
!
      N1 = N1 + 1
      DO 2 I = N1, MN1
         FRQNCY(I) = ZERO
    2 CONTINUE
!
!     Generate successively higher order distributions
!
      WORK(1) = ZERO
      IN = MAXMN
      DO 3 I = 2, MINMN
        WORK(I) = ZERO
        IN = IN + MAXMN
        N1 = IN + 2
        L = 1 + IN / 2
        K = I
!
!     Generate complete distribution from outside inwards
!
        DO 33 J = 1, L
          K = K + 1
          N1 = N1 - 1
          SUM = FRQNCY(J) + WORK(J)
          FRQNCY(J) = SUM
          WORK(K) = SUM - FRQNCY(N1)
          FRQNCY(N1) = SUM
   33   CONTINUE
    3 CONTINUE
!
    4 IFAULT = 0
!
!     Code to overwrite the frequency function with the distribution
!     function.   N.B. The frequency in FRQNCY(1) is for U = 0, and
!     that in FRQNCY(I) is for U = I - 1.
!
      SUM = ZERO
      DO 10 I = 1, MN1
        SUM = SUM + FRQNCY(I)
        FRQNCY(I) = SUM
   10 CONTINUE
      DO 20 I = 1, MN1
        CDF(I) = FRQNCY(I) / SUM
   20 CONTINUE
!
      RETURN
      END SUBROUTINE UDIST
      DOUBLE PRECISION FUNCTION UNI(IRESET)
!
!     Uniform (0, 1) random number generator
!
!     Reference:
!     L'Ecuyer, Pierre (1996),
!     "Combined Multiple Recursive Random Number Generators"
!     Operations Research 44, pp. 816-822.
!
!     2019/11: ALLOW GENERATOR TO BE RE-INITIALIZED
!
      INTEGER A12, A13, A21, A23, P12, P13, P21, P23
      INTEGER Q12, Q13, Q21, Q23, R12, R13, R21, R23
      INTEGER X10, X11, X12, X20, X21, X22, Z, M1, M2, H
      DOUBLE PRECISION INVMP1
      PARAMETER (  M1 = 2147483647,  M2 = 2145483479 )
      PARAMETER ( A12 =   63308,    Q12 = 33921, R12 = 12979 )
      PARAMETER ( A13 = -183326,    Q13 = 11714, R13 =  2883 )
      PARAMETER ( A21 =   86098,    Q21 = 24919, R21 =  7417 )
      PARAMETER ( A23 = -539608,    Q23 =  3976, R23 =  2071 )
      PARAMETER ( INVMP1 = 4.656612873077392578125D-10 )
!                 INVMP1 = 1.0D0/DBLE(M1+1)
      SAVE X10, X11, X12, X20, X21, X22
      DATA       X10,      X11,      X12,      X20,      X21,      X22   &
          / 11111111, 22222223, 33333335, 44444447, 55555559, 66666661 /
!
!     RE-INITIALIZE
!
      IF(IRESET.EQ.1)THEN
        X10=11111111
        X11=22222223
        X12=33333335
        X20=44444447
        X21=55555559
        X22=66666661
      ENDIF
!
!     Component 1
!
      H = X10/Q13
      P13 = -A13*( X10 - H*Q13 ) - H*R13
      H = X11/Q12
      P12 =  A12*( X11 - H*Q12 ) - H*R12
      IF ( P13 .LT. 0 ) P13 = P13 + M1
      IF ( P12 .LT. 0 ) P12 = P12 + M1
      X10 = X11
      X11 = X12
      X12 = P12 - P13
      IF ( X12 .LT. 0 ) X12 = X12 + M1
!
!     Component 2
!
      H = X20/Q23
      P23 = -A23*( X20 - H*Q23 ) - H*R23
      H = X22/Q21
      P21 =  A21*( X22 - H*Q21 ) - H*R21
      IF ( P23 .LT. 0 ) P23 = P23 + M2
      IF ( P21 .LT. 0 ) P21 = P21 + M2
      X20 = X21
      X21 = X22
      X22 = P21 - P23
      IF ( X22 .LT. 0 ) X22 = X22 + M2
!
!     Combination
!
      Z = X12 - X22
      IF ( Z .LE. 0 ) Z = Z + M1
      UNI = Z*INVMP1
!
      RETURN
      END FUNCTION UNI
      SUBROUTINE UNERAN(N,ISEED,ALOWLM,AUPPLM,DIAM,X,XSORT)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM A UNIFORM DISTRIBUTION.  HOWEVER, THIS HAS A
!              TWIST IN THAT WHEN THE RANDOM NUMBER IS FOUND, WE
!              DEFINE AN EXCLUSION ZONE ABOUT THAT NUMBER FOR SUBSEQUENT
!              VALUES.  THIS HAS APPLICATION IN MATERIALS TESTING
!              WHERE WHEN A BREAK OCCURS, IT IS NOT POSSIBLE FOR ANOTHER
!              BREAK TO OCCUR IN A SMALL REGION AROUND THAT BREAK POINT.
!              SINCE THE EXCLUSION ZONE IS SPECIFIED IN UNITS OF THE
!              FIBER LENGTH, ALLOW THE LOWER AND UPPER LIMITS TO BE
!              SPECIFIED (THE LOWER LIMIT IS TYPICALLY ZERO AND THE
!              UPPER LIMIT IS TYPICALLY THE FIBER LENGTH).
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER OF RANDOM
!                                NUMBERS TO BE GENERATED.
!                     --ISEED  = AN INTEGER THAT SPECIFIES THE SEED FOR
!                                THE RANDOM NUMBER GENERATOR.
!                     --ALOWLM = A REAL NUMBER THAT SPECIFIES THE LOWER
!                                LIMIT FOR THE UNIFORM RANDOM NUMBER
!                     --AUPPLM = A REAL NUMBER THAT SPECIFIES THE UPPER
!                                LIMIT FOR THE UNIFORM RANDOM NUMBER
!                     --DIAM   = A REAL NUMBER THAT SPECIFIES THE DIAMETER
!                                OF THE EXCLUSION ZONE.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR (OF DIMENSION
!                                AT LEAST N) INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE EXCLUSION ZONE
!             UNIFORM DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKER
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/3
!     ORIGINAL VERSION--MARCH     2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XSORT(*)
!
      DIMENSION XTEMP(1)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR FOR EXCLUSION ZONE RANDOM NUMBERS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
    6   FORMAT('      THE REQUESTED NUMBER OF RANDOM NUMBERS IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('      THE REQUESTED NUMBER OF RANDOM NUMBERS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALOWLM.GE.AUPPLM)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)
   16   FORMAT('      THE LOWER LIMIT IS GREATER THAN THE UPPER LIMIT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,17)ALOWLM
   17   FORMAT('      THE LOWER LIMIT = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,18)AUPPLM
   18   FORMAT('      THE UPPER LIMIT = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(DIAM.LT.0.0)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)
   26   FORMAT('      THE EXCLUSION ZONE IS NEGATIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,28)DIAM
   28   FORMAT('      THE EXLUSION ZONE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     GENERATE N EXCLUSION ZONE UNIFORM RANDOM NUMBERS
!
      DO 100 I=1,N
        X(I)=CPUMIN
  100 CONTINUE
!
      ALOC=ALOWLM
      ASCALE=AUPPLM - ALOWLM
      NTEMP=1
      CALL UNIRAN(NTEMP,ISEED,X)
      X(1)=ALOC + ASCALE*X(1)
      NCNT=1
      XSORT(1)=X(1)
!
!     GENERATE UNIFORM RANDOM NUMBERS, BUT REJECT IF FALL IN THE
!     EXCLUSION ZONE.  AT SOME POINT, IF THE EXCLUSION ZONE COVERS
!     THE ENTIRE RANGE, NO MORE POINTS CAN BE GENERATED.
!
      RAD=DIAM/2.0
  200 CONTINUE
!
        CALL UNIRAN(NTEMP,ISEED,XTEMP)
        XVAL=ALOC + ASCALE*XTEMP(1)
        DO 300 I=1,NCNT
          XLOW=XSORT(I) - RAD
          XUPP=XSORT(I) + RAD
          IF(XVAL.GE.XLOW .AND. XVAL.LE.XUPP)GO TO 200
  300   CONTINUE
!
        NCNT=NCNT+1
        X(NCNT)=XVAL
        CALL SORT(X,NCNT,XSORT)
        IF(NCNT.GE.N)GO TO 8000
!
!       CHECK IF EXCLUSION ZONES COVERS FULL RANGE.  IF SO,
!       NO MORE RANDOM NUMBERS CAN BE GENERATED.
!
        IF(XSORT(1)-RAD.GT.ALOWLM)GO TO 200
        IF(XSORT(NCNT)+RAD.LT.AUPPLM)GO TO 200
        DO 400 I=1,NCNT-1
          XVAL1=XSORT(I) + RAD
          XVAL2=XSORT(I+1) - RAD
          IF(XVAL1.LT.XVAL2)GO TO 200
  400   CONTINUE
        GO TO 8000
!
 8000 CONTINUE
      N=NCNT
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNERAN
      SUBROUTINE UNICDF(X,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(X.LT.0.0)THEN
        CDF=0.0
      ELSEIF(X.GT.1.0)THEN
        CDF=1.0
      ELSE
        CDF=X
      ENDIF
!
!CCCC WRITE(ICOUT,2)
!CCC2 FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNICDF IS ',
!CCCC1       'OUTSIDE THE (0,1) INTERVAL')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,46)X
!CC46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC ENDIF
!
      RETURN
      END SUBROUTINE UNICDF
      SUBROUTINE UNICHA(X,HAZ)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE HAZARD
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1 AND CUMULATIVE HAZARD FUNCTION
!                               H(X) = -LOG(1-X)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE HAZARD
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--HAZ    = THE SINGLE PRECISION CUMULATIVE HAZARD
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE HAZARD
!             FUNCTION VALUE HAZ.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2899
!     ORIGINAL VERSION--APRIL     1998.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      HAZ=0.0
      IF(X.LT.0.0.OR.X.GE.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNICHAZ IS ',   &
               'OUTSIDE THE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      HAZ=-LOG(1.0-X)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNICHA
      SUBROUTINE UNIHAZ(X,HAZ)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE HAZARD
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1 AND HAZARD FUNCTION
!                               H(X) = 1/(1-X).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE HAZARD
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--HAZ    = THE SINGLE PRECISION HAZARD
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION HAZARD
!             FUNCTION VALUE HAZ.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--APRIL     1998.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      HAZ=0.0
      IF(X.LT.0.0.OR.X.GE.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNIHAZ IS ',   &
               'OUTSIDE THE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      HAZ=1.0/(1.0 - X)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIHAZ
      REAL FUNCTION UNIKMN()
!***BEGIN PROLOGUE  UNIKMN
!***DATE WRITTEN   810915 (YYMMDD)
!***REVISION DATE  871210 (YYMMDD)
!***CATEGORY NO.  L6A21
!***KEYWORDS  RANDOM NUMBERS, UNIFORM RANDOM NUMBERS
!***AUTHOR    KAHANER, DAVID, SCIENTIFIC COMPUTING DIVISION, NBS
!             MARSAGLIA, GEORGE, SUPERCOMPUTER RES. INST., FLORIDA ST. U.
!
!***PURPOSE  THIS ROUTINE GENERATES REAL (SINGLE PRECISION) UNIFORM
!             RANDOM NUMBERS ON [0,1)
!***DESCRIPTION
!        Computes real (single precision) uniform numbers on [0,1).
!           From the book, "Numerical Methods and Software" by
!                D. Kahaner, C. Moler, S. Nash
!                Prentice Hall, 1988
!
!       USAGE:
!              To initialize the generator
!                   USEED = USTART(ISEED)
!               where: ISEED is any NONZERO integer
!                  will return floating point value of ISEED.
!
!               Subsequently
!                       U = UNI()
!                  will return a real uniform on [0,1)
!
!                One initialization is necessary, but any number of evaluations
!                  of  UNI in any order, are allowed.
!
!           Note: Depending upon the value of K (see below), the output
!                       of UNI may differ from one machine to another.
!
!           Typical usage:
!
!               REAL U,UNI,USTART,USEED
!               INTEGER ISEED
!C                 Set seed
!               ISEED = 305
!               USEED = USTART(ISEED)
!               DO 1 I = 1,1000
!                   U = UNI()
!             1 CONTINUE
!C                 NOTE: If K=24 (the default, see below) the output value of
!C                           U will be 0.1570390462475...
!               WRITE(*,*) U
!               END
!
!          NOTE ON PORTABILITY: Users can choose to run UNI in its default
!               mode (requiring NO user action) which will generate the same
!               sequence of numbers on any computer supporting floating point
!               numbers with at least 24 bit mantissas, or in a mode that
!               will generate numbers with a longer period on computers with
!               larger mantissas.
!          TO EXERCISE THIS OPTION:  B E F O R E  invoking USTART insert
!               the instruction        UBITS = UNIB(K)      K >= 24
!               where K is the number of bits in the mantissa of your floating
!               point word (K=48 for Cray, Cyber 205). UNIB returns the
!               floating point value of K that it actually used.
!                    K input as .LE. 24, then UBITS=24.
!                    K input as .GT. 24, then UBITS=FLOAT(K)
!               If K>24 the sequence of numbers generated by UNI may differ
!               from one computer to another.
!
!
!
!***REFERENCES  MARSAGLIA G., "COMMENTS ON THE PERFECT UNIFORM RANDOM
!                 NUMBER GENERATOR", UNPUBLISHED NOTES, WASH S. U.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE UNI
!
!     UPDATED  2019/11: IN ORDER TO ALLOW RE-INITIALIZATION OF SEED
!                       WITHIN A SINGLE DATAPLOT SESSION, RESET VALUES
!                       OF U WITHIN INITIALIZATION CODE.
      PARAMETER(   &
          CSAVE=362436./16777216.  ,   &
          CD=7654321./16777216.,   &
          CM=16777213./16777216.  )
!                            2**24=16777216
      REAL U(17),S,T,USTART,C,UNIB
      INTEGER I,J,II,JJ,K,KK,I1,J1,K1,L1,M1,ISEED
!
      SAVE U,I,J,K,C
!      Load data array in case user forgets to initialize.
!      This array is the result of calling UNI 100000 times
!         with ISEED=305 and K=64.
      DATA U/   &
      0.8668672834288,  0.3697986366357,  0.8008968294805,   &
      0.4173889774680,  0.8254561579836,  0.9640965269077,   &
      0.4508667414265,  0.6451309529668,  0.1645456024730,   &
      0.2787901807898,  0.06761531340295, 0.9663226330820,   &
      0.01963343943798, 0.02947398211399, 0.1636231515294,   &
      0.3976343250467,  0.2631008574685/
      DATA I,J,K,C/17,5,24,CSAVE/
!
!   Basic generator is Fibonacci
!
      UNIKMN = U(I)-U(J)
      IF(UNIKMN.LT.0.0)UNIKMN = UNIKMN+1.0
      U(I) = UNIKMN
      I = I-1
      IF(I.EQ.0)I = 17
      J = J-1
      IF(J.EQ.0)J = 17
!
!   Second generator is congruential
!
      C = C-CD
      IF(C.LT.0.0) C=C+CM
!
!   Combination generator
!
      UNIKMN = UNIKMN-C
      IF(UNIKMN.LT.0.0)UNIKMN = UNIKMN+1.0
      RETURN
!
      ENTRY USTART(ISEED)
!
!     2019/11: RESET U VALUES
!
      U(1)=0.8668672834288
      U(2)=0.3697986366357
      U(3)=0.8008968294805
      U(4)=0.4173889774680
      U(5)=0.8254561579836
      U(6)=0.9640965269077
      U(7)=0.4508667414265
      U(8)=0.6451309529668
      U(9)=0.1645456024730
      U(10)=0.2787901807898
      U(11)=0.06761531340295
      U(12)=0.9663226330820
      U(13)=0.01963343943798
      U(14)=0.02947398211399
      U(15)=0.1636231515294
      U(16)=0.3976343250467
      U(17)=0.2631008574685
      I=17
      J=5
      K=24
      C=CSAVE
!
!          Set up ...
!          Convert ISEED to four smallish positive integers.
!
        I1 = MOD(ABS(ISEED),177)+1
        J1 = MOD(ABS(ISEED),167)+1
        K1 = MOD(ABS(ISEED),157)+1
        L1 = MOD(ABS(ISEED),147)+1
!
!              Generate random bit pattern in array based on given seed.
!
        DO 2 II = 1,17
          S = 0.0
          T = 0.5
!             Do for each of the bits of mantissa of word
!             Loop  over K bits, where K is defaulted to 24 but can
!               be changed by user call to UNIB(K)
          DO 3 JJ = 1,K
                  M1 = MOD(MOD(I1*J1,179)*K1,179)
                  I1 = J1
                  J1 = K1
                  K1 = M1
                  L1 = MOD(53*L1+1,169)
                  IF(MOD(L1*M1,64).GE.32)S=S+T
                  T = .5*T
    3     CONTINUE
          U(II) = S
    2   CONTINUE
        USTART = FLOAT(ISEED)
        RETURN
!
      ENTRY UNIB(KK)
        IF(KK.LE.24)THEN
             K=24
        ELSE
             K=KK
        ENDIF
        UNIB=FLOAT(K)
      RETURN
      END FUNCTION UNIKMN
      SUBROUTINE UNIML1(Y,N,   &
                        XMIN,XMAX,XMEAN,XSD,XRANG,XMIDR,   &
                        ALOWLI,AUPPLI,AHAT,HHAT,ALO2LI,AUP2LI,   &
                        ALOCMO,ASCAMO,ALOCML,ASCAML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MOMENT AND MAXIMUM LIKELIHOOD
!              ESTIMATES FOR THE UNIFORM DISTRIBUTION FOR THE RAW DATA
!              CASE (I.E., NO CENSORING AND NO GROUPING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLUN WILL GENERATE THE OUTPUT
!              FOR THE UNIFORM MLE COMMAND).
!
!     REFERENCE--EVANS, HASTINGS, AND PEACOCK.  "STATISTICAL
!                DISTRIBUTIONS", THIRD EDITION, WILEY, 2000,
!                PP. 170-174
!              --JOHNSON, KOTZ, AND BALAKRISHNAN.  "CONTINUOUS
!                UNIVARIATE DISTRIBUTIONS, VOLUME II", SECOND
!                EDITION, WILEY, 1994.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/10
!     ORIGINAL VERSION--OCTOBER   2009. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLUN)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      INTEGER IFLAG
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UNIM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF UNIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR UNIFORM MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'BML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='UNIFORM'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      CALL RANGDP(Y,N,IWRITE,XRANG,IBUGA3,IERROR)
      CALL MIDRAN(Y,N,IWRITE,XMIDR,IBUGA3,IERROR)
!
      HHAT=0.5*XRANG
      AHAT=XMIDR
      ALOWLI=AHAT - HHAT
      AUPPLI=AHAT + HHAT
      ALO2LI=XMEAN - SQRT(3.0)*XSD
      AUP2LI=XMEAN + SQRT(3.0)*XSD
      ALOCMO=ALO2LI
      ASCAMO=AUP2LI - ALO2LI
      ALOCML=ALOWLI
      ASCAML=AUPPLI - ALOWLI
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF UNIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XSD,XMIN,XMAX,XRANG,XMIDR
 9055   FORMAT('N,XMEAN,XSD,XMIN,XMAX,XRANG,XMIDR = ',I8,6G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9056)HHAT,AHAT
 9056   FORMAT('HHAT,AHAT = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9057)ALOWLI,AUPPLI,ALO2LI,AUP2LI
 9057   FORMAT('ALOWLI,AUPPLI,ALO2LI,AUP2LI = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9058)ALOCMO,ASCAMO,ALOCML,ASCAML
 9058   FORMAT('ALOCMO,ASCAMO,ALOCML,ASCAML = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE UNIML1
      SUBROUTINE UNILI1(Y,N,ALOWLI,AUPPLI,   &
                        ALIK,AIC,AICC,BIC,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE LIKELIHOOD FUNCTION FOR
!              THE UNIFORM DISTRIBUTION.  THIS IS FOR THE RAW DATA
!              CASE (I.E., NO GROUPING AND NO CENSORING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/10
!     ORIGINAL VERSION--OCTOBER   2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DNP
      DOUBLE PRECISION DLIK
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UNIL'
      ISUBN2='I1  '
      IERROR='NO'
!
      ALIK=-99.0
      AIC=-99.0
      AICC=-99.0
      BIC=-99.0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF UNILI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N,ALOWLI,AUPPLI
   55   FORMAT('N,ALOWLI,AUPPLI = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  COMPUTE LIKELIHOOD FUNCTION         **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
!
!     LOG-LIKELIHOOD FUNCTION IS:
!
!     -N*LOG(B - A)
!
!     WITH B AND A DENOTING THE UPPER AND LOWER LIMITS, RESPECTIVELY
!
      DN=DBLE(N)
      DLIK=-DN*DLOG(DBLE(AUPPLI) - DBLE(ALOWLI))
      ALIK=REAL(DLIK)
      DNP=2.0D0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF UNILI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)ALIK,AIC,AICC,BIC
 9014   FORMAT('ALIK,AIC,AICC,BIC = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE UNILI1
      SUBROUTINE UNIPDF(X,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      IF(X.LT.0.0.OR.X.GT.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNIPDF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      PDF=1.0
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIPDF
      SUBROUTINE UNIME2(N,I,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES THE I-TH ORDER STATISTIC MEDIAN
!              FROM A SAMPLE OF SIZE N
!              FROM THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!              THIS SUBROUTINE IS A SUPPORT SUBROUTINE FOR
!              ALL OF THE PROBABILITY PLOT SUBROUTINES
!              IN DATAPAC; IT IS RARELY USED BY THE
!              DATA ANALYST DIRECTLY.
!              A PROBABILITY PLOT FOR A GENERAL DISTRIBUTION
!              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
!              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
!              THE I-TH ORDER STATISTIC MEDIAN FOR A GENERAL
!              DISTRIBUTION IS OBTAINED BY TRANSFORMING
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              BY THE PERCENT POINT FUNCTION OF THE DESIRED
!              DISTRIBUTION--HENCE THE IMPORTANCE OF BEING ABLE TO
!              GENERATE UNIFORM ORDER STATISTIC MEDIANS.
!              IT IS OF THEROETICAL INTEREST TO NOTE THAT
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              IN A SAMPLE OF SIZE N IS IDENTICALLY THE
!              MEDIAN OF THE BETA DISTRIBUTION
!              WITH PARAMETERS I AND N-I+1.
!     INPUT  ARGUMENTS--N      = THE INTEGER NUMBER
!                                OF OBSERVATIONS
!                                IN A SAMPLE.
!                     --I      = THE INTEGER NUMBER
!                                OF THE ORDER STATISTIC
!                                FOR WHICH A UNIFORM ORDER
!                                STATISTIC MEDIAN IS TO BE GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VARIABLE
!                                INTO WHICH THE GENERATED
!                                UNIFORM ORDER STATISTIC MEDIAN
!                                WILL BE PLACED.
!     OUTPUT--A SINGLE ORDER STATISTIC MEDIAN
!             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIENT
!                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-117.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JANUARY   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
               'UNIME2 IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ELSEIF(N.EQ.1)THEN
!CCCC   WRITE(ICOUT, 8)
!CCC8   FORMAT('***** WARNING--THE FIRST ARGUMENT ',
!CCCC1         'TO UNIME2 HAS THE VALUE 1.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   RETURN
      ENDIF
!
      AN=N
!
!     COMPUTE THE MEDIANS FOR THE FIRST AND LAST ORDER STATISTICS
!
      IF(I.EQ.1)GO TO 110
      IF(I.EQ.N)GO TO 120
      GO TO 130
!
  110 CONTINUE
      IF(I.EQ.1)X=1.0-(0.5**(1.0/AN))
      GO TO 9000
!
  120 CONTINUE
      IF(I.EQ.N)X=0.5**(1.0/AN)
      GO TO 9000
!
  130 CONTINUE
      GAM=0.3175
      AI=I
      X=(AI-GAM)/(AN-2.0*GAM+1.0)
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIME2
      SUBROUTINE UNIMED(N,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES THE N ORDER STATISTIC MEDIANS
!              FROM THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!              THIS SUBROUTINE IS A SUPPORT SUBROUTINE FOR
!              ALL OF THE PROBABILITY PLOT SUBROUTINES
!              IN DATAPAC; IT IS RARELY USED BY THE
!              DATA ANALYST DIRECTLY.
!              A PROBABILITY PLOT FOR A GENERAL DISTRIBUTION
!              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
!              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
!              THE I-TH ORDER STATISTIC MEDIAN FOR A GENERAL
!              DISTRIBUTION IS OBTAINED BY TRANSFORMING
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              BY THE PERCENT POINT FUNCTION OF THE DESIRED
!              DISTRIBUTION--HENCE THE IMPORTANCE OF BEING ABLE TO
!              GENERATE UNIFORM ORDER STATISTIC MEDIANS.
!              IT IS OF THEROETICAL INTEREST TO NOTE THAT
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              IN A SAMPLE OF SIZE N IS IDENTICALLY THE
!              MEDIAN OF THE BETA DISTRIBUTION
!              WITH PARAMETERS I AND N-I+1.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF UNIFORM ORDER STATISTIC MEDIANS
!                                TO BE GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                UNIFORM ORDER STATISTIC MEDIANS
!                                WILL BE PLACED.
!     OUTPUT--THE N ORDER STATISTIC MEDIANS
!             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIENT
!                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-117.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)GO TO 50
      IF(N.EQ.1)GO TO 55
      GO TO 90
   50 WRITE(ICOUT, 5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      RETURN
   55 WRITE(ICOUT, 8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
    5 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'UNIMED SUBROUTINE IS NON-POSITIVE *****')
    8 FORMAT('***** NON-FATAL DIAGNOSTIC--THE 1ST INPUT ARGUMENT ',   &
      ' TO THE UNIMED SUBROUTINE HAS THE VALUE 1')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
!
      AN=N
!
!     COMPUTE THE MEDIANS FOR THE FIRST AND LAST ORDER STATISTICS
!
      X(N)=0.5**(1.0/AN)
      X(1)=1.0-X(N)
!
!     DETERMINE IF AN ODD OR EVEN SAMPLE SIZE
!
      NHALF=(N/2)+1
      NEVODD=2*(N/2)
      IF(N.NE.NEVODD)X(NHALF)=0.5
      IF(N.LE.3)RETURN
!
!     COMPUTE THE MEDIANS FOR THE OTHER ORDER STATISTICS
!
      GAM=0.3175
      IMAX=N/2
      DO 100 I=2,IMAX
      AI=I
      IREV=N-I+1
      X(I)=(AI-GAM)/(AN-2.0*GAM+1.0)
      X(IREV)=1.0-X(I)
  100 CONTINUE
!
      RETURN
      END SUBROUTINE UNIMED
      SUBROUTINE UNIME3(N,X,TAG,IMETH)
!
!     PURPOSE--THE UNIMED SUBROUTINE COMPUTES UNIFORM ORDER
!              STATISTIC MEDIANS FROM THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              FOR FULL SAMPLES, THIS IS USED TO GENERATE
!              PLOTTING POSITIONS FOR THE PROBABILITY PLOT.
!              THE UNIME2 SUBROUTINE IS A MODIFIED VERSION THAT
!              IS USED FOR THE CASE OF TIME CENSORED DATA.  IN
!              THIS CASE, THE TAG VARIABLE IDENTIFIES WHETHER
!              THE I-TH POINT REPRESENTS A FAILURE TIME OR A
!              TRUNCATION TIME.  THE BASIC IDEA IS THAT ORDER
!              STATISTIC MEDIANS ARE GENERATED BASED ON THE FULL
!              SAMPLE, BUT ONLY FAILURE TIMES ARE ACTUALLY PLOTTED
!              ON THE PROBABILITY PLOT.  ALTERNATIVELY, PLOTTING
!              POSITIONS CAN BE BASED ON THE MODIFIED KAPLAN-MIER
!              METHOD.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF UNIFORM ORDER STATISTIC MEDIANS
!                                TO BE GENERATED.
!                     --TAG    = A SINGLE PRECISION VECTOR WHERE
!                                1 INDICATES A FAILURE TIME AND
!                                0 INDICATES A TRUNCATION TIME
!                     --IMETH  = CHARACTER VARIABLE (EITHER
!                                "UNIM" OR "KAPM") TO SPECIFY
!                                WHICH PLOTTING POSITIONS TO USE
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                UNIFORM ORDER STATISTIC MEDIANS
!                                WILL BE PLACED.
!     OUTPUT--THE N ORDER STATISTIC MEDIANS
!             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIENT
!                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-117.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG< MD 20899-8980
!                 PHONE--301-75-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.10
!     ORIGINAL VERSION--OCTOBER   2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
!------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION TAG(*)
      CHARACTER*4 IMETH
!
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DCONST
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DN
      DOUBLE PRECISION DQ
!
!-----COMMON-------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT--------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        WRITE(ICOUT,8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    5 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',   &
      'UNIME2 SUBROUTINE IS NON-POSITIVE *****')
    8 FORMAT('***** WARNING--THE FIRST INPUT ARGUMENT ',   &
      ' TO THE UNIME2 SUBROUTINE HAS THE VALUE 1')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
!
      AN=N
!
!CCCC UNIFORM ORDER STATISTIC METHOD
!
      IF(IMETH.EQ.'UNIM')THEN
!
!     COMPUTE THE MEDIANS FOR THE FIRST AND LAST ORDER STATISTICS
!
        X(N)=0.5**(1.0/AN)
        X(1)=1.0-X(N)
!
!     DETERMINE IF AN ODD OR EVEN SAMPLE SIZE
!
        NHALF=(N/2)+1
        NEVODD=2*(N/2)
        IF(N.NE.NEVODD)X(NHALF)=0.5
        IF(N.LE.3)GO TO 9000
!
!     COMPUTE THE MEDIANS FOR THE OTHER ORDER STATISTICS
!
        GAM=0.3175
        IMAX=N/2
        DO 100 I=2,IMAX
          AI=I
          IREV=N-I+1
          X(I)=(AI-GAM)/(AN-2.0*GAM+1.0)
          X(IREV)=1.0-X(I)
  100   CONTINUE
!
!CCCC KAPLAM-MIER METHOD
!
      ELSE
        DPROD=1.0D0
        DN=DBLE(N)
        DCONST=(DN+0.7D0)/(DN+0.4D0)
        DO 200 I=1,N
          IF(TAG(I).EQ.1.0)THEN
            DQ=DBLE(I)
            DTERM1=(DN - DQ + 0.7D0)/(DN - DQ + 1.7D0)
            DPROD=DPROD*DTERM1
            X(I)=REAL(1.0D0 - DCONST*DPROD)
          ELSE
            X(I)=REAL(DPROD)
          ENDIF
          IF(X(I).GT.1.0)X(I)=1.0
          IF(X(I).LT.0.0)X(I)=0.0
  200   CONTINUE
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIME3
      SUBROUTINE UNIME4(N,I,X,IMETH,YFAIL,CENS,N2)
!
!     PURPOSE--THIS SUBROUTINE GENERATES THE I-TH ORDER STATISTIC MEDIAN
!              FROM A SAMPLE OF SIZE N
!              FROM THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!              THIS SUBROUTINE IS A SUPPORT SUBROUTINE FOR
!              ALL OF THE PROBABILITY PLOT SUBROUTINES
!              IN DATAPAC; IT IS RARELY USED BY THE
!              DATA ANALYST DIRECTLY.
!              A PROBABILITY PLOT FOR A GENERAL DISTRIBUTION
!              IS A PLOT OF THE ORDERED OBSERVATIONS VERSUS
!              THE ORDER STATISTIC MEDIANS FOR THAT DISTRIBUTION.
!              THE I-TH ORDER STATISTIC MEDIAN FOR A GENERAL
!              DISTRIBUTION IS OBTAINED BY TRANSFORMING
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              BY THE PERCENT POINT FUNCTION OF THE DESIRED
!              DISTRIBUTION--HENCE THE IMPORTANCE OF BEING ABLE TO
!              GENERATE UNIFORM ORDER STATISTIC MEDIANS.
!              IT IS OF THEROETICAL INTEREST TO NOTE THAT
!              THE I-TH UNIFORM ORDER STATISTIC MEDIAN
!              IN A SAMPLE OF SIZE N IS IDENTICALLY THE
!              MEDIAN OF THE BETA DISTRIBUTION
!              WITH PARAMETERS I AND N-I+1.
!
!              THIS IS AN UPDATED VERSION OF UNIME2 THAT
!              SUPPORTS CENSORED DATA.  THE BASIC IDEA IS THAT
!              ORDER STATISTIC MEDIANS ARE GENERATED BASED ON
!              THE FULL DATA SET, BUT ONLY FAILURE TIMES ARE
!              ACTUALLY PLOTTED.  ALTERNATIVELY, PLOTTING POSITIONS
!              CAN BE BASED ON THE MODIFIED KAPLAN-MIER METHOD.
!
!     INPUT  ARGUMENTS--N      = THE INTEGER NUMBER
!                                OF OBSERVATIONS
!                                IN A SAMPLE.
!                     --I      = THE INTEGER NUMBER
!                                OF THE ORDER STATISTIC
!                                FOR WHICH A UNIFORM ORDER
!                                STATISTIC MEDIAN IS TO BE GENERATED.
!                     --IMETH  = CHARACTER VARIABLE (EITHER
!                                "UNIM" OR "KAPM") TO SPECIFY
!                                WHICH PLOTTING POSITIONS TO USE
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VARIABLE
!                                INTO WHICH THE GENERATED
!                                UNIFORM ORDER STATISTIC MEDIAN
!                                WILL BE PLACED.
!     OUTPUT--A SINGLE ORDER STATISTIC MEDIAN
!             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, 'THE PROBABILITY PLOT CORRELATION COEFFICIENT
!                 TEST FOR NORMALITY', TECHNOMETRICS, 1975, PAGES 111-117.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JANUARY   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL YFAIL(*)
      REAL CENS(*)
!
      CHARACTER*4 IMETH
!
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DCONST
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DN
      DOUBLE PRECISION DQ
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO UNIME2 IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
!CCCC   WRITE(ICOUT, 8)
!CCC8 FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNIME2 HAS THE ',
!CCCC1       'VALUE 1.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
      ENDIF
!
      AN=N
!
      IF(IMETH.EQ.'UNIM')THEN
!
!       COMPUTE THE MEDIANS FOR THE FIRST AND LAST ORDER STATISTICS
!
        IF(I.EQ.1)THEN
          X=1.0-(0.5**(1.0/AN))
        ELSEIF(I.EQ.N)THEN
          IF(I.EQ.N)X=0.5**(1.0/AN)
        ELSE
          GAM=0.3175
          AI=I
          X=(AI-GAM)/(AN-2.0*GAM+1.0)
        ENDIF
      ELSE
        DPROD=1.0D0
        DN=DBLE(N)
        DCONST=(DN+0.7D0)/(DN+0.4D0)
!
        ITEMP=0
        DO 200 J=1,N2
          NI=INT(YFAIL(J)+0.1)
          NI2=INT(CENS(J)+0.1)
          IF(NI.GT.0)THEN
            DO 300 II=1,NI
              ITEMP=ITEMP+1
              DQ=DBLE(ITEMP)
              DTERM1=(DN - DQ + 0.7D0)/(DN - DQ + 1.7D0)
              DPROD=DPROD*DTERM1
              XI=REAL(1.0D0 - DCONST*DPROD)
              IF(ITEMP.EQ.I)THEN
                X=XI
                IF(X.GT.1.0)X=1.0
                IF(X.LT.0.0)X=0.0
                GO TO 9000
              ENDIF
  300       CONTINUE
          ENDIF
!
          IF(NI2.GT.0)THEN
            DO 400 II=1,NI2
              ITEMP=ITEMP+1
              XI=REAL(DPROD)
              IF(ITEMP.EQ.I)THEN
                X=XI
                IF(X.GT.1.0)X=1.0
                IF(X.LT.0.0)X=0.0
                GO TO 9000
              ENDIF
  400       CONTINUE
          ENDIF
  200   CONTINUE
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIME4
      SUBROUTINE UNIPPF(P,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGUALAR)
!       DISTRIBUTION FROM 0 TO 1
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!       F(X)=1
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --OCTOBER   1976.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNIPPF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      PPF=P
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNIPPF
      SUBROUTINE UNISF(P,SF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SPARSITY
!              FUNCTION VALUE FOR THE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
!              THIS DISTRIBUTION HAS MEAN = 0.5
!              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1.
!              NOTE THAT THE SPARSITY FUNCTION OF A DISTRIBUTION
!              IS THE DERIVATIVE OF THE PERCENT POINT FUNCTION,
!              AND ALSO IS THE RECIPROCAL OF THE PROBABILITY
!              DENSITY FUNCTION (BUT IN UNITS OF P RATHER THAN X).
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE SPARSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--SF     = THE SINGLE PRECISION SPARSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION
!             SPARSITY FUNCTION VALUE SF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      SF=0.0
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** WARNING--THE FIRST ARGUMENT TO UNISF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      SF=1.0
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UNISF
      SUBROUTINE UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,   &
      IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
!
!     PURPOSE--UPDATE PREDICTED VALUES AND RESIDUALS
!              AND ASSOCIATED INTERNAL TABLES.
!              ALSO ADJUST (IF CALLED FOR) THE
!                   REPLICATION STANDARD DEVIATION
!                   REPLICATION DEGREES OF FREEDOM
!                   RESIDUAL STANDARD DEVIATION
!                   RESIDUAL DEGREES OF FREEDOM
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1988.  ADD LOFCDF
!     UPDATED         --NOVEMBER  1989.  ADD DIMENSION IANS(*) (NELSON)
!     UPDATED         --APRIL     1992.  LOFCDF TO ALFCDF
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IOP
      CHARACTER*4 MESSAG
      CHARACTER*4 IFOUNN
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION ISUB(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
!CCCC THE FOLLOWING LINE WAS INSERTED NOVERMBER 1989
!CCCC (BUG UNCOVERED BY NELSON HSU)
      DIMENSION IANS(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UPDA'
      ISUBN2='PR  '
      IERROR='NO'
!
!               ***************************************
!               **  STEP 1--                         **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF UPDAPR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICOLPR,ICOLRE
   53 FORMAT('ICOLPR,ICOLRE = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NLEFT
   54 FORMAT('NLEFT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IREPU,REPSD,REPDF,IRESU,RESSD,RESDF
   55 FORMAT('IREPU,REPSD,REPDF,IRESU,RESSD,RESDF = ',   &
      A4,2E15.7,2X,A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
!CCCC THE FOLLOWING 2 LINES WERE FIXED   APRIL 1992
!CCCC WRITE(ICOUT,56)LOFCDF
!CC56 FORMAT('LOFCDF = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)ALFCDF
   56 FORMAT('ALFCDF = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 60 I=1,NLEFT
      WRITE(ICOUT,61)I,ISUB(I),PRED2(I),PRED(I),RES2(I),RES(I)
   61 FORMAT('I,ISUB(I),PRED2(I),PRED(I),RES2(I),RES(I) = ',   &
      I8,I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
   60 CONTINUE
   90 CONTINUE
!
      ISTEPN='15'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 7210 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLPR)GO TO 7215
      GO TO 7210
 7215 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLPR
      VALUE(J4)=ICOLPR
      IN(J4)=NLEFT
 7210 CONTINUE
!
      DO 7220 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLRE)GO TO 7225
      GO TO 7220
 7225 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLRE
      VALUE(J4)=ICOLRE
      IN(J4)=NLEFT
 7220 CONTINUE
!
      J=0
      DO 7300 I=1,NLEFT
      IF(ISUB(I).EQ.0)GO TO 7310
      J=J+1
      PRED(I)=PRED2(J)
      RES(I)=RES2(J)
      GO TO 7300
 7310 CONTINUE
 7300 CONTINUE
!
      IF(IREPU.EQ.'OFF')GO TO 7490
      IH1='REPS'
      IH2='D   '
      IOP='CHAD'
      MESSAG='NO'
      CALL UPDATP(IH1,IH2,REPSD,IOP,MESSAG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IH1='REPD'
      IH2='F   '
      IOP='CHAD'
      MESSAG='NO'
      CALL UPDATP(IH1,IH2,REPDF,IOP,MESSAG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
 7490 CONTINUE
!
      IF(IRESU.EQ.'OFF')GO TO 7590
      IH1='RESS'
      IH2='D   '
      IOP='CHAD'
      MESSAG='NO'
      CALL UPDATP(IH1,IH2,RESSD,IOP,MESSAG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IH1='RESD'
      IH2='F   '
      IOP='CHAD'
      MESSAG='NO'
      CALL UPDATP(IH1,IH2,RESDF,IOP,MESSAG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!CCCC THE FOLLOWING SECTION OF CODE WAS ADDED MARCH 1988.
      IH1='LOFC'
      IH2='DF  '
      IOP='CHAD'
      MESSAG='NO'
      CALL UPDATP(IH1,IH2,ALFCDF,IOP,MESSAG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
 7590 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF UPDAPR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IERROR
 9012 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGA3
 9013 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NLEFT
 9014 FORMAT('NLEFT = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NLEFT
      WRITE(ICOUT,9016)I,ISUB(I),PRED2(I),PRED(I),RES2(I),RES(I)
 9016 FORMAT('I,ISUB(I),PRED2(I),PRED(I),RES2(I),RES(I) = ',   &
      I8,I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE UPDAPR
      SUBROUTINE UPDATF(IHWORD,IHWOR2,IFUNC3,N3,IOP,MESSAG,   &
                        IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                        NUMNAM,MAXNAM,IANS,IWIDTH,ILISTL,NEWNAM,MAXN3,   &
                        IFUNC,NUMCHF,MAXCHF,IBUGS2,ILOCN,IFOUNN,IERRON)
!
!     PURPOSE--CHECK TO SEE IF THE FUNCTION NAME
!              IN   (IHWORD,IHWOR2)
!              EXISTS IN THE CURRENT TABLE OF AVAILABLE PARAMETER,
!              VARIABLE, AND FUNCTION NAMES AS GIVEN IN IHNAME(.) AND
!              IHNAME2(I).  IF FOUND,
!              PLACE THE N3-LENGTH STRING    IFUNC3      INTO THE
!              CORRESPONDING ELEMENT OF THE VECTORS IFUNC(.)
!              IF NOT FOUND AND IF SPECIFIED (VIA IOP),
!              ADD THE NAME TO THE TABLE
!              PLACING THE N3-LENGTH STRING    IFUNC3      INTO THE
!              CORRESPONDING ELEMENT OF THE VECTORS IFUNC(.)
!     OUTPUT ARGUMENTS--ILOCN  = THE LOCATION (THAT IS, THE LINE OR ROW)
!                                IN THE TABLE WHERE THE NAME WAS FOUND
!                                (IF FOUND).
!                     --IFOUNN = A HOLLERITH VARIABLE
!                                WITH THE VALUE 'YES' OR 'NO'
!                                DEPENDING ON WHETHER OR NOT
!                                THE NAME WAS FOUND IN THE EXISTING TABLE.
!                     --IERRON = A HOLLERITH VARIABLE
!                                WITH THE VALUE 'YES' OR 'NO'.
!                                IERRON WILL TAKE ON THE VALUE 'NO'
!                                UNDER ANY OF THE FOLLOWING CONDITIONS--
!                                1) IF THE NAME WAS FOUND IN THE EXISTING TABLE,
!                                2) IF THE NAME WAS NOT FOUND BUT
!                                   ONLY A CHECK WAS CALLED FOR AS
!                                   OPPOSED TO A CHECK & ADD,
!                                3) IF THE NAME WAS NOT FOUND BUT THE
!                                   TABLE WAS NOT FULL AND SO THERE
!                                   WAS ROOM TO ADD THE NAME TO THE TA
!                                IERRON WILL TAKE ON THE VALUE 'YES'
!                                ONLY WHEN THE FOLLOWING 3 CONDITIONS
!                                ALL HOLD SIMULTANEOUSLY--
!                                1) THE NAME WAS NOT FOUND IN THE TABLE;
!                                2) A CHECK & ADD WAS CALLED FOR;
!                                3) THE TABLE WAS ALREADY FULL.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--FEBRUARY   1993.
!     UPDATED       --NOVEMBER    1994. IERROR TO IERRON (BOMB ON VAX)
!     UPDATED       --MARCH       2015. ADD "IN" TO DPINFU CALL LIST
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHWORD
      CHARACTER*4 IHWOR2
      CHARACTER*4 IFUNC3
      CHARACTER*4 IFUNC
      CHARACTER*4 IOP
      CHARACTER*4 MESSAG
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUNN
      CHARACTER*4 IERRON
!CCCC OCTOBER 1993.  ADD FOLLOWING LINE
      CHARACTER*4 NEWNAM
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION IFUNC3(*)
      DIMENSION IFUNC(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
!
      DIMENSION IANS(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UPDA'
      ISUBN2='TF  '
      IFOUNN='NO'
      IERRON='NO'
!
      ILOCN=0
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF UPDATF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IHWORD,IHWOR2,IOP,MESSAG,IBUGS2
   52   FORMAT('IHWORD,IHWOR2,IOP,MESSAG,IBUGS2 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMNAM,MAXNAM,MAXN3
   53   FORMAT('NUMNAM,MAXNAM,MAXN3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I) = ',I8,3(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************
!               **  STEP 1--                          **
!               **  CHECK FOR THE FUNCTION NAME       **
!               **  IN THE CURRENT LIST.              **
!               **  IF FOUND, THEN COPY IN THE STRING **
!               **  AND EXIT.                         **
!               **  IF NOT FOUND, THEN CONTINUE.      **
!               ****************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1110 I=1,NUMNAM
        I2=I
        IF(IHWORD.EQ.IHNAME(I).AND.IHWOR2.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'F')THEN
          ILOCN=I2
          IFOUNN='YES'
          IERRON='NO'
          NEWNAM='NO'
          ILISTL=ILOCN
          CALL DPINFU(IFUNC3,N3,   &
                      IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                      NUMNAM,IANS,IWIDTH,IHWORD,IHWOR2,   &
                      ILISTL,NEWNAM,MAXNAM,   &
                      IFUNC,NUMCHF,MAXCHF,IBUGS2,IERRON)
          GO TO 9000
        ENDIF
 1110 CONTINUE
!
!               ******************************************
!               **  STEP 2--                            **
!               **  THE FUNCTION  NAME WAS NOT FOUND.   **
!               **  IF SPECIFIED(VIA    MESSAG),        **
!               **  GENERATE A MESSAGE TO THAT EFFECT.  **
!               ******************************************
!
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(MESSAG.EQ.'YES')THEN
        WRITE(ICOUT,1202)ISUBN1,ISUBN2
 1202   FORMAT('***** ERROR IN ',2A4,'--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      A FUNCTION/STRING NAME USED (OR NEEDED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1206)
 1206   FORMAT('      IN A COMMAND OR AN EXPRESSION WAS NOT FOUND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1210)
 1210   FORMAT('      IN THE CURRENT LIST OF AVAILABLE ',   &
               'FUNCTION/STRING NAMES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)IHWORD,IHWOR2
 1212   FORMAT('      THE FUNCTION/STRING IN QUESTION WAS ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *************************************
!               **  STEP 3--                       **
!               **  IF ONLY A CHECK                **
!               **  (AS OPPOSED TO A CHECK & ADD)  **
!               **  WAS SPECIFIED (VIA     IOP),   **
!               **  THEN EXIT.                     **
!               *************************************
!
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOP.EQ.'CHEC')THEN
        IFOUNN='NO'
        IERRON='NO'
        GO TO 9000
      ENDIF
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  IF A CHECK & ADD                                 **
!               **  WAS SPECIFIED (VIA    IOP), THEN ATTEMPT TO ADD  **
!               **  THE PARAMETER NAME TO THE LIST. IF THE LIST IS   **
!               **  NOT FULL, THEN ADD THE NAME, COPY IN THE         **
!               **  FUNCTION, AND EXIT.  IF THE LIST IS FULL,        **
!               **  THEN CONTINUE.                                   **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMNAM.LT.MAXNAM)THEN
!CCCC   NUMNAM=NUMNAM+1
!CCCC   ILOCN=NUMNAM
!CCCC   IHNAME(ILOCN)=IHWORD
!CCCC   IHNAM2(ILOCN)=IHWOR2
!CCCC   IUSE(ILOCN)='F'
        NEWNAM='YES'
        ILISTL=NUMNAM+1
        CALL DPINFU(IFUNC3,N3,   &
                    IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                    NUMNAM,IANS,IWIDTH,IHWORD,IHWOR2,ILISTL,   &
                    NEWNAM,MAXNAM,   &
                    IFUNC,NUMCHF,MAXCHF,IBUGS2,IERRON)
        IFOUNN='NO'
        IERRON='NO'
        GO TO 9000
      ENDIF
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  THE LIST IS FULL AND THEREFORE THE NAME COULD    **
!               **  BE ADDED.  GENERATE AN ERROR MESSAGE TO THAT     **
!               **  EFFECT AND EXIT.                                 **
!               *******************************************************
!
      ISTEPN='5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(ICOUT,1502)ISUBN1,ISUBN2
 1502 FORMAT('***** ERROR IN ',2A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1504)
 1504 FORMAT('      A FUNCTION/STRING NAME USED (OR NEEDED) IN A ',   &
             'COOMAND OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1508)
 1508 FORMAT('      AN EXPRESSION WAS NOT FOUND IN THE CURRENT LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1510)
 1510 FORMAT('      OF AVAILABLE FUNCTION/STRING NAMES AND COULD NOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1512)
 1512 FORMAT('      BE ADDED TO THE LIST BECAUSE THE LIST IS FULL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1516)IHWORD,IHWOR2
 1516 FORMAT('      THE FUNCTION/STRING IN QUESTION WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1517)NUMNAM
 1517 FORMAT('      THE CURRENT   NUMBER OF NAMES IN THE LIST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1518)NUMNAM
 1518 FORMAT('      THE ALLOWABLE NUMBER OF NAMES IN THE LIST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IFOUNN='NO'
      IERRON='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF UPDATF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUNN,IERRON,ILOCN
 9012   FORMAT('IFOUNN,IERRON,ILOCN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IHWORD,IHWOR2,IOP,MESSAG
 9013   FORMAT('IHWORD,IHWOR2,IOP,MESSAG = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMNAM,MAXNAM
 9014   FORMAT('NUMNAM,MAXNAM = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I) = ',I8,3(2X,A4))
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE UPDATF
      SUBROUTINE UPDATP(IHWORD,IHWOR2,SCALAR,IOP,MESSAG,   &
                        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                        NUMNAM,MAXNAM,   &
                        IANS,IWIDTH,IBUGA3,ILOCN,IFOUNN,IERRON)
!
!     PURPOSE--CHECK TO SEE IF THE HOLLERITH PARAMETER NAME
!              IN (IHWORD,IHWOR2)
!              EXISTS IN THE CURRENT TABLE OF AVAILABLE PARAMETER,
!              VARIABLE, AND FUNCTION NAMES AS GIVEN IN IHNAME(.) AND IHNAM2(I).
!              IF FOUND, PLACE THE VALUE   SCALAR    INTO THE
!              CORRESPONDING ELEMENT OF THE VECTORS VALUE(.) AND IVALUE(.).
!              IF NOT FOUND AND
!              IF SPECIFIED (VIA IOP), ADD THE NAME TO THE TABLE
!              BEFORE PLACING THE VALUE    SCALAR   INTO THE
!              CORRESPONDING ELEMENTS OF VALUE(.) AND IVALUE(.).
!     OUTPUT ARGUMENTS--ILOCN  = THE LOCATION (THAT IS, THE LINE OR ROW)
!                                IN THE TABLE WHERE THE NAME WAS FOUND
!                                (IF FOUND).
!                     --IFOUNN = A HOLLERITH VARIABLE
!                                WITH THE VALUE 'YES' OR 'NO'
!                                DEPENDING ON WHETHER OR NOT
!                                THE NAME WAS FOUND IN THE EXISTING TABLE.
!                     --IERRON = A HOLLERITH VARIABLE
!                                WITH THE VALUE 'YES' OR 'NO'.
!                                IERRON WILL TAKE ON THE VALUE 'NO'
!                                UNDER ANY OF THE FOLLOWING CONDITIONS--
!                                1) IF THE NAME WAS FOUND IN THE EXISTING TABLE,
!                                2) IF THE NAME WAS NOT FOUND BUT
!                                   ONLY A CHECK WAS CALLED FOR AS
!                                   OPPOSED TO A CHECK & ADD,
!                                3) IF THE NAME WAS NOT FOUND BUT THE
!                                   TABLE WAS NOT FULL AND SO THERE
!                                   WAS ROOM TO ADD THE NAME TO THE TA
!                                IERRON WILL TAKE ON THE VALUE 'YES'
!                                ONLY WHEN THE FOLLOWING 3 CONDITIONS
!                                ALL HOLD SIMULTANEOUSLY--
!                                1) THE NAME WAS NOT FOUND IN THE TABLE;
!                                2) A CHECK & ADD WAS CALLED FOR;
!                                3) THE TABLE WAS ALREADY FULL.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--FEBRUARY   1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHWORD
      CHARACTER*4 IHWOR2
      CHARACTER*4 IOP
      CHARACTER*4 MESSAG
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUNN
      CHARACTER*4 IERRON
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      DIMENSION IANS(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UPDA'
      ISUBN2='TP  '
      IFOUNN='NO'
      IERRON='NO'
!
      ILOCN=0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF UPDATP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IHWORD,IHWOR2,SCALAR,IOP,MESSAG,IBUGA3
   52   FORMAT('IHWORD,IHWOR2,SCALAR,IOP,MESSAG,IBUGA3 = ',   &
               2(A4,2X),G15.7,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMNAM,MAXNAM
   53   FORMAT('NUMNAM,MAXNAM = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),   &
                         VALUE(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),',   &
                 'VALUE(I) = ',I8,3(2X,A4),2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,59)(IANS(I),I=1,MIN(100,IWIDTH))
   59   FORMAT(100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!               ****************************************
!               **  STEP 1--                          **
!               **  CHECK FOR THE PARAMETER NAME      **
!               **  IN THE CURRENT LIST.              **
!               **  IF FOUND, THEN EQUATE THE SCALAR  **
!               **  AND EXIT.                         **
!               **  IF NOT FOUND, THEN CONTINUE.      **
!               ****************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1110 I=1,NUMNAM
      I2=I
      IF(IHWORD.EQ.IHNAME(I).AND.IHWOR2.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'P')GO TO 1120
 1110 CONTINUE
      GO TO 1190
 1120 CONTINUE
      ILOCN=I2
      VALUE(I2)=SCALAR
      IVALUE(I2)=INT(SCALAR+0.5)
      IFOUNN='YES'
      IERRON='NO'
      GO TO 9000
 1190 CONTINUE
!
!               ******************************************
!               **  STEP 2--                            **
!               **  THE PARAMETER NAME WAS NOT FOUND.   **
!               **  IF SPECIFIED(VIA    MESSAG),        **
!               **  GENERATE A MESSAGE TO THAT EFFECT.  **
!               ******************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(MESSAG.EQ.'NO')GO TO 1290
      WRITE(ICOUT,1202)ISUBN1,ISUBN2
 1202 FORMAT('***** ERROR IN ',2A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1204)
 1204 FORMAT('      A PARAMETER NAME USED (OR NEEDED)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1206)
 1206 FORMAT('      IN A COMMAND OR AN EXPRESSION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1208)
 1208 FORMAT('      WAS NOT FOUND IN THE CURRENT LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1210)
 1210 FORMAT('      OF AVAILABLE PARAMETER NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)IHWORD,IHWOR2
 1212 FORMAT('      THE PARAMETER IN QUESTION WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 1290 CONTINUE
!
!               *************************************
!               **  STEP 3--                       **
!               **  IF ONLY A CHECK                **
!               **  (AS OPPOSED TO A CHECK & ADD)  **
!               **  WAS SPECIFIED (VIA     IOP),   **
!               **  THEN EXIT.                     **
!               *************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOP.EQ.'CHEC')GO TO 1310
      GO TO 1390
 1310 CONTINUE
      IFOUNN='NO'
      IERRON='NO'
      GO TO 9000
 1390 CONTINUE
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  IF A CHECK & ADD                                 **
!               **  WAS SPECIFIED (VIA    IOP),                      **
!               **  THEN ATTEMPT TO ADD                              **
!               **  THE PARAMETER NAME TO THE LIST.                  **
!               **  IF THE LIST IS NOT FULL,                         **
!               **  THEN ADD THE NAME, EQUATE THE SCALAR, AND EXIT.  **
!               **  IF THE LIST IS FULL,                             **
!               **  THEN CONTINUE.                                   **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMNAM.LT.MAXNAM)GO TO 1410
      GO TO 1490
 1410 CONTINUE
      NUMNAM=NUMNAM+1
      ILOCN=NUMNAM
      IHNAME(ILOCN)=IHWORD
      IHNAM2(ILOCN)=IHWOR2
      IUSE(ILOCN)='P'
      VALUE(ILOCN)=SCALAR
      IVALUE(ILOCN)=INT(SCALAR+0.5)
      IFOUNN='NO'
      IERRON='NO'
      GO TO 9000
 1490 CONTINUE
!
!               **********************************************************
!               **  STEP 5--                                            **
!               **  THE LIST IS FULL                                    **
!               **  AND THEREFORE THE NAME COULD NOT BE ADDED.          **
!               **  GENERATE AN ERROR MESSAGE TO THAT EFFECT AND EXIT.  **
!               **********************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(ICOUT,1502)ISUBN1,ISUBN2
 1502 FORMAT('***** ERROR IN ',2A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1504)
 1504 FORMAT('      A PARAMETER NAME USED (OR NEEDED)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1506)
 1506 FORMAT('      IN A COMMAND OR AN EXPRESSION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1508)
 1508 FORMAT('      WAS NOT FOUND IN THE CURRENT LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1510)
 1510 FORMAT('      OF AVAILABLE PARAMETER NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1511)
 1511 FORMAT('      AND COULD NOT BE ADDED TO THE LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1512)
 1512 FORMAT('      BECAUSE THE LIST IS FULL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1516)IHWORD,IHWOR2
 1516 FORMAT('      THE PARAMETER IN QUESTION WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1517)NUMNAM
 1517 FORMAT('      THE CURRENT   NUMBER OF NAMES IN THE LIST = ',   &
      I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1518)NUMNAM
 1518 FORMAT('      THE ALLOWABLE NUMBER OF NAMES IN THE LIST = ',   &
      I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IFOUNN='NO'
      IERRON='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF UPDATP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUNN,IERRON,ILOCN
 9012 FORMAT('IFOUNN,IERRON,ILOCN = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IHWORD,IHWOR2,SCALAR,IOP,MESSAG
 9013 FORMAT('IHWORD,IHWOR2,SCALAR,IOP,MESSAG = ',   &
      A4,2X,A4,E15.7,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMNAM,MAXNAM
 9014 FORMAT('NUMNAM,MAXNAM = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NUMNAM
      WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),   &
      VALUE(I)
 9016 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),',   &
      'VALUE(I) = ',I8,2X,A4,2X,A4,2X,A4,I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE UPDATP
      SUBROUTINE UPPHIN(X,N,IWRITE,XTEMP,MAXNXT,XUPPHI,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE UPPER HINGE
!              OF THE DATA IN THE INPUT VECTOR X.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XUPPHI = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE UPPER HINGE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE UPPER HINGE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UPPH'
      ISUBN2='IN  '
      IERROR='NO'
!
      IARG1=0
      IARG2=0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF UPPHIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ***************************
!               **  COMPUTE UPPER HINGE  **
!               ***************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(1.LE.N.AND.N.LE.MAXNXT)GO TO 119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN UPPHIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE UPPER HINGE IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)MAXNXT
  115 FORMAT('      MUST BE BETWEEN 1 AND ',I8,' (INCLUSIVELY).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN UPPHIN--',   &
      'THE 2ND INPUT ARGUMENT (N) HAS THE VALUE 1')
      CALL DPWRST('XXX','BUG ')
      XUPPHI=X(1)
      GO TO 9000
  129 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN UPPHIN--',   &
      'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XUPPHI=HOLD
      GO TO 9000
  139 CONTINUE
!
!               ********************************
!               **  STEP 2--                  **
!               **  COMPUTE THE UPPER HINGE.  **
!               ********************************
!
!
      CALL SORT(X,N,XTEMP)
!
      N2=(N+1)/2
      IARG1=(N2+1)/2
      IARG2=(N2+1)-IARG1
      IARG1R=N-IARG1+1
      IARG2R=N-IARG2+1
      XUPPHI=(XTEMP(IARG1R)+XTEMP(IARG2R))/2.0
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,XUPPHI
  811 FORMAT('THE UPPER HINGE OF THE ',I8,' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF UPPHIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IARG1,IARG2
 9014 FORMAT('IARG1,IARG2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)XUPPHI
 9015 FORMAT('XUPPHI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE UPPHIN
      SUBROUTINE UPPQUA(X,N,IWRITE,XTEMP,MAXNXT,XUPPQU,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE UPPER QUARTILE
!              OF THE DATA IN THE INPUT VECTOR X.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!       `                        IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XUPPQU = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE UPPER QUARTILE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE UPPER QUARTILE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2008. WEIGHTING OF NEAREST TWO
!                                       POINTS IS REVERSED
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='UPPQ'
      ISUBN2='UA  '
      IERROR='NO'
!
      NI=0
      NIP1=0
!
      ANI=0.0
      A2NI=0.0
      REM=0.0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF UPPQUA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ******************************
!               **  COMPUTE UPPER QUARTILE  **
!               ******************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1 .OR. N.GT.MAXNXT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN UPPER QUARTILE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)MAXNXT
  115   FORMAT('      MUST BE BETWEEN 1 AND ',I8,' (INCLUSIVELY).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        XUPPQU=X(1)
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,136)HOLD
!C136 FORMAT('***** WARNING IN UPPQUA--',
!CCCC1'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
      XUPPQU=HOLD
      GO TO 9000
  139 CONTINUE
!
!               ***********************************
!               **  STEP 2--                     **
!               **  COMPUTE THE UPPER QUARTILE.  **
!               ***********************************
!
      CALL SORT(X,N,XTEMP)
!
      P=0.75
!
      ANI=P*(AN+1.0)
      NI=INT(ANI)
      A2NI=REAL(NI)
      REM=ANI-A2NI
      NIP1=NI+1
      IF(NI.LE.1)NI=1
      IF(NI.GE.N)NI=N
      IF(NIP1.LE.1)NIP1=1
      IF(NIP1.GE.N)NIP1=N
!CCCC 10/20/2008: WEIGHTING IS REVERSED
!CCCC XUPPQU=REM*XTEMP(NI)+(1.0-REM)*XTEMP(NIP1)
      XUPPQU=(1.0-REM)*XTEMP(NI)+REM*XTEMP(NIP1)
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XUPPQU
  811   FORMAT('THE UPPER QUARTILE OF THE ',I8,' OBSERVATIONS = ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF UPPQUA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ANI,NI,A2NI,REM,NIP1
 9014   FORMAT('ANI,NI,A2NI,REM,NIP1 = ',G15.7,I8,2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)XUPPQU
 9015   FORMAT('XUPPQU = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE UPPQUA
      SUBROUTINE UTSCDF(X,A,B,D,ANU1,ANU3,ALPHA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE UNEVEN TWO-SIDED POWER
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
!              F(X,A,B,D,N1,N2,ALPHA)
!              = 0                                    X < A
!              = [ALPHA*(B-A)*NU3/
!              (ALPHA*(B-A)*NU3+(D-B)*NU1)]
!              *((X-A)/(B-A))**NU1
!                                                     A <= X <  B
!              = 1 - [(D-B)*NU1/
!              (ALPHA*(B-A)*NU3+(D-B)*NU1)]
!              *((D-X)/(D-B))**NU3
!                                                     B <= X <  D
!              = 1                                    X >= D
!              WHERE
!                  A <= B <= D, NU1, NU3, ALPHA > 0
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
!
      INCLUDE 'DPCOP2.INC'
!
!-----COMMON----------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0
      DCDF=0.0D0
      IF(A.GT.B .OR. B.GT.D)THEN
        WRITE(ICOUT,12)
   12   FORMAT(   &
        '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
   13   FORMAT(   &
        '      THE THREE SHAPE PARAMETERS (A, B, D) MUST SATISFY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
   14   FORMAT('         A <= B <= D')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,D
   16   FORMAT('      A, B, D = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
   23   FORMAT('      THE ALPHA SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ALPHA
   47   FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
   33   FORMAT('      THE NU1 SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU1
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)
   43   FORMAT('      THE NU3 SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU3
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LE.A)THEN
        CDF=0.0
        GO TO 9000
      ELSEIF(X.GE.D)THEN
        CDF=1.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
!
      DTERM1=DALPHA*(DB-DA)*DNU3 + (DD-DB)*DNU1
!
      IF(A.LE.X .AND. X.LT.B)THEN
        DTERM2=DALPHA*(DB-DA)*DNU3
        DTERM3=((DX-DA)/(DB-DA))**DNU1
        DCDF=(DTERM2/DTERM1)*DTERM3
      ELSEIF(B.LE.X .AND. X.LT.D)THEN
        DTERM2=(DD-DB)*DNU1
        DTERM3=((DD-DX)/(DD-DB))**DNU3
        DCDF=1.0D0 - (DTERM2/DTERM1)*DTERM3
      ENDIF
      CDF=REAL(DCDF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UTSCDF
      SUBROUTINE UTSPDF(X,A,B,D,ANU1,ANU3,ALPHA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE UNEVEN TWO-SIDED POWER
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
!              f(X,A,B,D,N1,N2,ALPHA)
!              = [ALPHA*NU1*NU3/
!              (ALPHA*(B-A)*NU3+(D-B)*NU1)]
!              *((X-A)/(B-A))**(NU1-1)
!                                                     A <= X <  B
!              = [ALPHA*NU1*NU3/
!              (ALPHA*(B-A)*NU3+(D-B)*NU1)]
!              *((D-X)/(D-B))**(NU3-1)
!                                                     B <= X <  D
!                               = 0                   X < A, X >= D
!              WHERE
!                  A <= B <= D, NU1, NU3, ALPHA > 0
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      DPDF=0.0D0
      IF(A.GT.B .OR. B.GT.D)THEN
        WRITE(ICOUT,12)
   12   FORMAT(   &
        '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
   13   FORMAT(   &
        '      THE THREE SHAPE PARAMETERS (A, B, D) MUST SATISFY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
   14   FORMAT('         A <= B <= D')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,D
   16   FORMAT('      A, B, D = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
   23   FORMAT('      THE ALPHA SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ALPHA
   47   FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
   33   FORMAT('      THE NU1 SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU1
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)
   43   FORMAT('      THE NU3 SHAPE PARAMETER MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU3
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LT.A .OR. X.GE.D)THEN
        PDF=0.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
!
      IF(A.LE.X .AND. X.LT.B)THEN
        IF(A.EQ.X .AND. ANU1.LE.1.0)THEN
          WRITE(ICOUT,132)
  132     FORMAT(   &
          '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,133)ANU1
  133     FORMAT(   &
          '      WHEN X = A AND NU1 <= 1, THE PDF IS UNDEFINED.')
          CALL DPWRST('XXX','BUG ')
          PDF=0.0
          GO TO 9000
        ENDIF
!
        DTERM1=DALPHA*DNU1*DNU3/(DALPHA*(DB-DA)*DNU3 +(DD-DB)*DNU1)
        DTERM2=((DX-DA)/(DB-DA))**(DNU1-1.0D0)
        DPDF=DTERM1*DTERM2
      ELSEIF(B.LE.X .AND. X.LT.D)THEN
        IF(D.EQ.X .AND. ANU3.LE.1.0)THEN
          WRITE(ICOUT,232)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,233)ANU1
          CALL DPWRST('XXX','BUG ')
          PDF=0.0
          GO TO 9000
        ENDIF
  232 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
  233 FORMAT(   &
      '      WHEN X = D AND NU3 <= 1, THE PDF IS UNDEFINED.')
!
        DTERM1=DNU1*DNU3/(DALPHA*(DB-DA)*DNU3 +(DD-DB)*DNU1)
        DTERM2=((DD-DX)/(DD-DB))**(DNU3-1.0D0)
        DPDF=DTERM1*DTERM2
      ENDIF
      PDF=REAL(DPDF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UTSPDF
      SUBROUTINE UTSPPF(P,A,B,D,ANU1,ANU3,ALPHA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE UNEVEN TWO-SIDED POWER
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING PPF FUNCTION:
!              G(P,A,B,D,N1,N3,ALPHA)
!              = A + (B-A)*(P/(C1/C3))**(1/NU1)       0 <= P < CUTOFF
!
!              = D - (D-B)*((1-P)/(C2/C3))**(1/NU3)   CUTOFF <= P <= 1
!
!              WHERE
!                  A <= B <= D, NU1, NU3, ALPHA > 0
!                  C1 = ALPHA*(B-A)*NU3
!                  C2 = (D-B)*NU1
!                  C3 = ALPHA*(B-A)*NU3 + (D-B)*NU1
!                  CUTOFF = UTSCDF(B,A,B,D,NU1,NU3,ALPHA)
!
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UTSCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DC1
      DOUBLE PRECISION DC2
      DOUBLE PRECISION DC3
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.GT.B .OR. B.GT.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,D
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU1
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU3
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    2 FORMAT(   &
      '***** ERROR--THE FIRST ARGUMENT TO UTSPPF IS OUTSIDE THE ',   &
      'ALLOWABLE (0,1) INTERVAL.')
   12 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   13 FORMAT(   &
      '      THE THREE SHAPE PARAMETERS (A, B, D) MUST SATISFY')
   14 FORMAT(   &
      '         A <= B <= D')
   16 FORMAT(   &
      '      A, B, D = ',3G15.7)
   22 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   23 FORMAT(   &
      '      THE ALPHA SHAPE PARAMETER MUST BE POSITIVE.')
   32 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   33 FORMAT(   &
      '      THE NU1 SHAPE PARAMETER MUST BE POSITIVE.')
   42 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   43 FORMAT(   &
      '      THE NU3 SHAPE PARAMETER MUST BE POSITIVE.')
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      IF(P.LE.0.0)THEN
        PPF=0.0
      ELSEIF(P.GE.1.0)THEN
        PPF=1.0
      ELSE
        DP=DBLE(P)
        DA=DBLE(A)
        DB=DBLE(B)
        DD=DBLE(D)
        DNU1=DBLE(ANU1)
        DNU3=DBLE(ANU3)
        DALPHA=DBLE(ALPHA)
!
        DC3=DALPHA*(DB-DA)*DNU3 + (DD-DB)*DNU1
        CALL UTSCDF(B,A,B,D,ANU1,ANU3,ALPHA,CUTOFF)
!
        IF(P.LT.CUTOFF)THEN
          DC1=DALPHA*(DB-DA)*DNU3
          DPPF=DA + (DB-DA)*(DP/(DC1/DC3))**(1.0D0/DNU1)
          PPF=REAL(DPPF)
        ELSE
          DC2=(DD-DB)*DNU1
          DPPF=DD - (DD-DB)*((1.0D0-DP)/(DC2/DC3))**(1.0D0/DNU3)
          PPF=REAL(DPPF)
        ENDIF
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UTSPPF
      SUBROUTINE UTSRAN(N,A,B,D,ANU1,ANU3,ALPHA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE UNEVEN TWO-SIDED POWER DISTRIBUTION
!              WITH SHAPE PARAMETERS B,ANU1,ANU3,ALPHA AND
!              LOWER AND UPPER LIMIT PARAMETERS A AND B.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --A      = THE SINGLE PRECISION VALUE OF THE
!                                LOWER LIMIT PARAMETER A.
!                     --D      = THE SINGLE PRECISION VALUE OF THE
!                                UPPER LIMIT PARAMETER D.
!                     --B      = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE (THRESHOLD) PARAMETER B.
!                     --ANU1   = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER ANU1.
!                     --ANU3   = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER ANU3.
!                     --ALPHA  = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER ALPHA.
!                                AN SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE UNEVEN TWO-SIDED POWER DISTRIBUTION
!             WITH SHAPE PARAMETERS = A, B, D, NU1, NU3, AND ALPHA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --ANU1, ANU3, AND ALPHA SHOULD BE POSITIVE.
!                   A < B < D.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, UTSPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
!                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
!                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
!                 AMERICAN STATISTICIAN, VOLUME 56,
!                 NUMBER 2, MAY, 2002.
!               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 7.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR IN UNEVEN TWO-SIDED POWER RANDOM ',   &
               'NUMBERS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      THE REQUESTED NUMBER OF RANDOM NUMBERS IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.GT.B .OR. B.GT.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,D
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU1
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)ANU3
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   13 FORMAT(   &
      '      THE THREE SHAPE PARAMETERS (A, B, D) MUST SATISFY')
   14 FORMAT(   &
      '         A <= B <= D')
   16 FORMAT(   &
      '      A, B, D = ',3G15.7)
   22 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   23 FORMAT(   &
      '      THE ALPHA SHAPE PARAMETER MUST BE POSITIVE.')
   32 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   33 FORMAT(   &
      '      THE NU1 SHAPE PARAMETER MUST BE POSITIVE.')
   42 FORMAT(   &
      '***** ERROR--FOR THE UNEVEN TWO-SIDED POWER DISTRIBUTION,')
   43 FORMAT(   &
      '      THE NU3 SHAPE PARAMETER MUST BE POSITIVE.')
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TWO-SIDED POWER DISTRIBUTION RANDOM
!     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
        CALL UTSPPF(X(I),A,B,D,ANU1,ANU3,ALPHA,XTEMP)
        X(I)=XTEMP
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE UTSRAN
      SUBROUTINE VALCNT(X,N,XVAL,IWRITE,XCOUNT,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE NUMBER OF TIMES A SPECIFIC
!              VALUE OCCURS IN AN ARRAY.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --XVAL   = THE SINGLE PRECISION SCALAR VALUE BEING
!                                TESTED.
!     OUTPUT ARGUMENTS--XCOUNT = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED COUNT.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COUNT.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014.2
!     ORIGINAL VERSION--FEBRUARY  2014.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VALC'
      ISUBN2='NT  '
      IERROR='NO'
      XCOUNT=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'LCNT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF VALCNT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,XVAL
   52   FORMAT('IBUGA3,N,XVAL = ',A4,2X,I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN VALUE COUNT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ************************
!               **  STEP 2--          **
!               **  COMPUTE THE COUNT **
!               ************************
!
      DO 200 I=1,N
        IF(X(I).EQ.XVAL)XCOUNT=XCOUNT+1.0
  200 CONTINUE
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XVAL,XCOUNT
  811   FORMAT('THE COUNT OF THE ',I8,' OBSERVATIONS EQUAL TO ',G15.7,   &
               ' IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'LCNT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF VALCNT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,XCOUNT
 9012   FORMAT('IERROR,XCOUNT = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE VALCNT
      SUBROUTINE VAR(X,N,IWRITE,XVAR,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE VARIANCE (WITH
!              DENOMINATOR N-1) OF THE DATA IN THE INPUT VECTOR X.
!              THE SAMPLE VARIANCE = (THE SUM OF THE SQUARED DEVIATIONS
!              ABOUT THE SAMPLE MEAN)/(N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XVAR   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE VARIANCE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE VARIANCE (WITH DENOMINATOR N-1).
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SNEDECOR AND COCHRAN, STATISTICAL METHODS,
!                 EDITION 6, 1967, PAGE 44.
!               --DIXON AND MASSEY, INTRODUCTION TO STATISTICAL
!                 ANALYSIS, EDITION 2, 1957, PAGE 38.
!               --MOOD AND GRABLE, 'INTRODUCTION TO THE THEORY
!                 OF STATISTICS, EDITION 2, 1963, PAGE 171.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --JUNE      1979.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VAR '
      ISUBN2='    '
      IERROR='NO'
      DMEAN=0.0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF VAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ************************
!               **  COMPUTE VARIANCE  **
!               ************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN VAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        XVAR=0.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)
  136 FORMAT('***** WARNING IN VAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,137)HOLD
  137 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      XVAR=0.0
      GO TO 9000
  139 CONTINUE
!
!               *****************************
!               **  STEP 2--               **
!               **  COMPUTE THE VARIANCE.  **
!               *****************************
!
      DN=N
      DSUM=0.0D0
      DO 200 I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
!
      DSUM=0.0D0
      DO 300 I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      XVAR=DVAR
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XVAR
  811   FORMAT('THE VARIANCE OF THE ',I8,' OBSERVATIONS = ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF VAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,DMEAN,XVAR
 9012   FORMAT('IERROR,DMEAN,XVAR = ',A4,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE VAR
      SUBROUTINE VARCOV(AMAT1,AMAT2,MAXROM,MAXCOM,NR1,NC1,DMEAN,   &
      ICASE,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              VARIANCE-COVARIANCE MATRIX OF A MATRIX.  NOTE
!              THAT IT CAN BE COMPUTED FOR COVARIANCE BETWEEN
!              COLUMNS (I.E., VARIABLES, NORMAL CASE) OR BETWEEN
!              ROWS (I.E., DATA POINTS=COVARIANCE MATRIX OF THE
!              TRANSPOSE).
!     INPUT  ARGUMENTS--AMAT   = THE SINGLE PRECISION MATRIX
!                     --MAXROM = THE INTEGER ROW DIMENSION OF AMAT
!                     --MAXCOM = THE INTEGER COUMN DIMENSION OF AMAT
!                     --NR1    = THE INTEGER NUMBER OF ROWS OF AMAT
!                     --NC1    = THE INTEGER NUMBER OF COLUMNS OF AMAT
!     OUTPUT ARGUMENTS--AMAT2    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED VARIANCE-COVARIANCE MATRIX.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             VARIANCE-COVARIANCE MATRIX.
!     NOTE--THIS ROUTINE ASSUMES THE ERROR CHECKING (FOR EQUAL
!           ROWS AND COLUMNS, MATCHING DIMENSIONS FOR X AND AMAT)
!           IS DONE BT THE CALLING SUBROUTINE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.7
!     ORIGINAL VERSION--JULY      1998.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DYM2
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DCOV
      DOUBLE PRECISION D999
!
      DIMENSION AMAT1(MAXROM,MAXCOM)
      DIMENSION AMAT2(MAXROM,MAXCOM)
      DOUBLE PRECISION DMEAN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VARC'
      ISUBN2='OV  '
      IERROR='NO'
!
      D999=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF VARCOV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NR1,NC1
   53 FORMAT('NR1, NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               ******************************
!               **  COMPUTE QUADRATIC FORM  **
!               ******************************
!
      DNR1=DBLE(NR1)
      DNC1=DBLE(NC1)
!
      IF(ICASE.EQ.'COLU')THEN
      DO 5111 J=1,NC1
      DSUM1=0.0D0
      DO 5112 I=1,NR1
      DYM1=AMAT1(I,J)
      DSUM1=DSUM1+DYM1
 5112 CONTINUE
      DMEAN(J)=D999
      DDENOM=DNR1
      IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 5111 CONTINUE
!
      DO 5121 J=1,NC1
      DO 5122 K=J,NC1
      DSUM1=0.0D0
      DO 5123 I=1,NR1
      DYM1=AMAT1(I,J)
      DYM2=AMAT1(I,K)
      DDEL1=DYM1-DMEAN(J)
      DDEL2=DYM2-DMEAN(K)
      DSUM1=DSUM1+DDEL1*DDEL2
 5123 CONTINUE
      DCOV=D999
      DDENOM=DNR1-1.0D0
      IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
      AMAT2(J,K)=DCOV
      AMAT2(K,J)=DCOV
 5122 CONTINUE
 5121 CONTINUE
!
      ELSE
      DO 6111 J=1,NR1
      DSUM1=0.0D0
      DO 6112 I=1,NC1
      DYM1=AMAT1(J,I)
      DSUM1=DSUM1+DYM1
 6112 CONTINUE
      DMEAN(J)=D999
      DDENOM=DNC1
      IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 6111 CONTINUE
!
      DO 6121 J=1,NR1
      DO 6122 K=J,NR1
      DSUM1=0.0D0
      DO 6123 I=1,NC1
      DYM1=AMAT1(J,I)
      DYM2=AMAT1(K,I)
      DDEL1=DYM1-DMEAN(J)
      DDEL2=DYM2-DMEAN(K)
      DSUM1=DSUM1+DDEL1*DDEL2
 6123 CONTINUE
      DCOV=D999
      DDENOM=DNR1-1.0D0
      IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
      AMAT2(J,K)=DCOV
      AMAT2(K,J)=DCOV
 6122 CONTINUE
 6121 CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF VARCOV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE VARCOV
      SUBROUTINE VARDIS(X,N,IWRITE,VDIST,RDI,CHISQ,   &
                        ICASE,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE VARIATIONAL DISTANCE (FROM
!              UNIFORMITY) FOR AN ARRAY OF COUNTS.
!
!              THE VARIATIONAL DISTANCE FOR THE UNIFORM DISTRIBUTION IS:
!
!                 d = 0.5*SUM[k=0 to infinity][|p(unif=k) - p(data = k)|]
!
!              GIVEN AN ARRAY X OF N COUNTS, THIS BECOMES
!
!                 d = 0.5*SUM[k=1 to N][|(1/N) - X(k)/SUM[k=1 to N][X(i)]|]
!
!              THE VARIATIONAL DISTANCE WILL BE BETWEEN 0 AND 1.  THE
!              RELATIVE DISPERSION INDEX IS THEN
!
!                RDI = 100*(1 0
!
!              THE UNIFORM CHI-SQUARE STATISTIC IS
!
!                CHISQ = SUM[i=1 to k][(X(i) - EXP)**2/EXP]
!
!              WHERE
!
!                EXP = SUM[i=1 to k][X(i)]/k
!
!     REFERENCES--KASHIWAGI, FAGAN, DOUGLAS, YAMAMOTO, HECKERT, LEIGH,
!                 OBRZUT, DU, LIN-GIBSON, MU, WINEY, HAGGENMUELLER
!                 (2007), "Relationship between dispersion metric and
!                 properties of PMMA/SWNT nanocomposites", POLYMER
!                 JOURNAL, VOL. 48, PP. 4855 - 4866.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) COUNTS.
!                     --N      = THE INTEGER NUMBER OF COUNTS IN THE
!                                VECTOR X.
!     OUTPUT ARGUMENTS--VDIST  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED VARIATIONAL DISTANCE.
!                     --RDI    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED RELATIVE DISPERSION INDEX.
!                     --CHISQ  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED UNIFORM CHI-SQUARE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COUNT.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014.3
!     ORIGINAL VERSION--MARCH     2014.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTOT
      DOUBLE PRECISION DEXP
      DOUBLE PRECISION DUNIF
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VARD'
      ISUBN2='IS  '
      IERROR='NO'
      VDIST=0.0
      RDI=0.0
      CHISQ=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RDIS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF VARDIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASE,IBUGA3,ISUBRO,N
   52   FORMAT('ICASE,IBUGA3,ISUBRO,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN VARIATIONAL DISTANCE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 120 I=1,N
        IF(X(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)I,X(I)
  122     FORMAT('      ROW ',I8,' HAS A NEGATIVE COUNT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
  120 CONTINUE
!
!               ******************************
!               **  STEP 2--               **
!               **  COMPUTE THE STATISTICS **
!               *****************************
!
      DTOT=0.0D0
      DN=DBLE(N)
      DO 200 I=1,N
        IX=INT(X(I)+0.5)
        DX=DBLE(IX)
        DTOT=DTOT+DX
  200 CONTINUE
!
      DEXP=DTOT/DN
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 300 I=1,N
        IX=INT(X(I)+0.5)
        DX=DBLE(IX)
        DUNIF=1.0D0/DN
        DSUM1=DSUM1+DABS(DUNIF - (DX/DTOT))
        DSUM2=DSUM2+((DX - DEXP)**2/DEXP)
  300 CONTINUE
      VDIST=REAL(0.5D0*DSUM1)
      RDI=100.0*(1.0 - VDIST)
      CHISQ=REAL(DSUM2)
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE.EQ.'VDIS')THEN
          WRITE(ICOUT,811)VDIST,N
  811     FORMAT('THE VARIATIONAL DISTANCE OF THE ',I8,   &
                ' COUNTS IS EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'RDI ')THEN
          WRITE(ICOUT,813)RDI,N
  813     FORMAT('THE RELATIVE DISPERSION INDEX OF THE ',I8,   &
                ' COUNTS IS EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'UCHS')THEN
          WRITE(ICOUT,815)CHISQ,N
  815     FORMAT('THE UNIFORM CHI-SQUARE OF THE ',I8,   &
                ' COUNTS IS EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RDIS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF VARDIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,VDIST,RDI,CHISQ
 9012   FORMAT('IERROR,VDIST,RDI,CHISQ = ',A4,2X,3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DTOT,DSUM1,DSUM2,DEXP,DUNIF
 9014   FORMAT('DTOT,DSUM1,DSUM2,DEXP,DUNIF = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE VARDIS
      SUBROUTINE VARPOO(AMAT1,AMAT2,AMAT3,MAXROM,MAXCOM,NR1,NC1,NR2,   &
      DMEAN,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              POOLED VARIANCE-COVARIANCE MATRIX OF TWO MATRICES.
!                     Cpooled = [(n1-1)*C1 + (n2-1)*C2]/(n1+n2-2)
!              WHERE C1 AND C2 ARE THE VARIANCE-COVARIANCE MATRICES
!              FOR SAMPLE 1 AND 2 RESPECTIVELY.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             POOLED VARIANCE-COVARIANCE MATRIX.
!     NOTE--THIS ROUTINE ASSUMES THE ERROR CHECKING (FOR EQUAL
!           ROWS AND COLUMNS, MATCHING DIMENSIONS FOR X AND AMAT)
!           IS DONE BT THE CALLING SUBROUTINE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.7
!     ORIGINAL VERSION--JULY      1998.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DYM2
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DCOV
      DOUBLE PRECISION D999
!
      DIMENSION AMAT1(MAXROM,MAXCOM)
      DIMENSION AMAT2(MAXROM,MAXCOM)
      DIMENSION AMAT3(MAXROM,MAXCOM)
      DOUBLE PRECISION DMEAN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VARP'
      ISUBN2='OO  '
      IERROR='NO'
!
      D999=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF VARPOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NR1,NR2,NC1
   53 FORMAT('NR1, NR2, NC1 = ',3I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *************************************************
!               **  COMPUTE POOLED VARIANCE-COVARIANCE MATRIX  **
!               *************************************************
!
      DNR1=DBLE(NR1)
      DNR2=DBLE(NR2)
      DNC1=DBLE(NC1)
!
      DO 5111 J=1,NC1
      DSUM1=0.0D0
      DO 5112 I=1,NR1
      DYM1=AMAT1(I,J)
      DSUM1=DSUM1+DYM1
 5112 CONTINUE
      DMEAN(J)=D999
      DDENOM=DNR1
      IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 5111 CONTINUE
!
      DO 5121 J=1,NC1
      DO 5122 K=J,NC1
      DSUM1=0.0D0
      DO 5123 I=1,NR1
      DYM1=AMAT1(I,J)
      DYM2=AMAT1(I,K)
      DDEL1=DYM1-DMEAN(J)
      DDEL2=DYM2-DMEAN(K)
      DSUM1=DSUM1+DDEL1*DDEL2
 5123 CONTINUE
      DCOV=D999
      DDENOM=DNR1-1.0D0
      IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
      AMAT3(J,K)=(DNR1-1.0D0)*DCOV
      AMAT3(K,J)=(DNR1-1.0D0)*DCOV
 5122 CONTINUE
 5121 CONTINUE
!
      DO 6111 J=1,NC1
      DSUM1=0.0D0
      DO 6112 I=1,NR2
      DYM1=AMAT2(I,J)
      DSUM1=DSUM1+DYM1
 6112 CONTINUE
      DMEAN(J)=D999
      DDENOM=DNR2
      IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 6111 CONTINUE
!
      DO 6121 J=1,NC1
      DO 6122 K=J,NC1
      DSUM1=0.0D0
      DO 6123 I=1,NR2
      DYM1=AMAT2(I,J)
      DYM2=AMAT2(I,K)
      DDEL1=DYM1-DMEAN(J)
      DDEL2=DYM2-DMEAN(K)
      DSUM1=DSUM1+DDEL1*DDEL2
 6123 CONTINUE
      DCOV=D999
      DDENOM=DNR2-1.0D0
      IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
      AMAT3(J,K)=(AMAT3(J,K) + (DNR2-1.0D0)*DCOV)/REAL(NR1+NR2-2)
      AMAT3(K,J)=(AMAT3(K,J) + (DNR2-1.0D0)*DCOV)/REAL(NR1+NR2-2)
 6122 CONTINUE
 6121 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF VARPOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NR1,NC1
 9013 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE VARPOO
      SUBROUTINE VARPO2(AMAT1,AMAT2,AMAT3,MAXROM,MAXCOM,NR1,NC1,NR3,   &
      TAG,TAGDIS,NIJUNK,NK,DMEAN,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              POOLED VARIANCE-COVARIANCE MATRIX OF MORE THAN 2
!              MATRICES.
!                     Cpooled = [(n1-1)*C1 + (n2-1)*C2 + ... +(nk-1)*CK]/(n1+n2+ ... +nk-k)
!              WHERE C1, C2, ..., CK ARE THE VARIANCE-COVARIANCE MATRICES
!              FOR SAMPLE 1, 2, ... , K RESPECTIVELY.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             POOLED VARIANCE-COVARIANCE MATRIX.
!     NOTE--THIS ROUTINE ASSUMES THE ERROR CHECKING (FOR EQUAL
!           ROWS AND COLUMNS, MATCHING DIMENSIONS FOR X AND AMAT)
!           IS DONE BT THE CALLING SUBROUTINE.
!     NOTE--THE TAG VARIABLE IS A GROUP IDENTIFIER THAT DEFINES
!           WHAT MATRIX A GIVEN ROW BELONGS TO.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.7
!     ORIGINAL VERSION--JULY      1998.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DYM2
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DCOV
!
      DIMENSION AMAT1(MAXROM,MAXCOM)
      DIMENSION AMAT2(MAXROM,MAXCOM)
      DIMENSION AMAT3(NR3,MAXCOM)
      DIMENSION TAG(*)
      DIMENSION TAGDIS(*)
      DIMENSION NIJUNK(*)
      DOUBLE PRECISION DMEAN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VARP'
      ISUBN2='O2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF VARPO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NR1,NC1
   53 FORMAT('NR1, NC1 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,MIN(20,NR1)
      WRITE(ICOUT,56)I,TAG(I),AMAT1(I,1),AMAT1(I,2)
   56 FORMAT('I,TAG(I),Z(I,1),Z(I,2)=',I8,3E15.7)
   55 CONTINUE
   90 CONTINUE
!
!               *************************************************
!               **  COMPUTE NUMBER OF DISTINCT ELEMENTS OF TAG **
!               *************************************************
!
      IWRITE='OFF'
      CALL DISTIN(TAG,NR1,IWRITE,TAGDIS,NK,IBUGA3,IERROR)
!
!               *************************************************
!               **  COMPUTE POOLED VARIANCE-COVARIANCE MATRIX  **
!               *************************************************
!
      DO 95 J=1,NC1
        DO 98 I=1,NC1
          AMAT3(I,J)=0.0
   98   CONTINUE
   95 CONTINUE
      NSUM=0
!
      DO 100 IGROUP=1,NK
!
        ATEMP=TAGDIS(IGROUP)
        ICOUNT=0
        DO 200 J=1,NR1
          IF(TAG(J).EQ.ATEMP)THEN
            ICOUNT=ICOUNT+1
            DO 210 L=1,NC1
              AMAT2(ICOUNT,L)=AMAT1(J,L)
  210       CONTINUE
          ENDIF
  200   CONTINUE
        IF(ICOUNT.LT.1)GO TO 100
        NI=ICOUNT
        NIJUNK(IGROUP)=NI
        NSUM=NSUM + (NI - 1)
!
        DNR1=DBLE(NI)
        DNC1=DBLE(NC1)
!
        DO 5111 J=1,NC1
          DSUM1=0.0D0
          DO 5112 I=1,NI
            DYM1=AMAT2(I,J)
            DSUM1=DSUM1+DYM1
 5112     CONTINUE
          DMEAN(J)=0.0D0
          DDENOM=DNR1
          IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 5111   CONTINUE
!
        DO 5121 J=1,NC1
        DO 5122 K=J,NC1
        DSUM1=0.0D0
        DO 5123 I=1,NI
        DYM1=AMAT2(I,J)
        DYM2=AMAT2(I,K)
        DDEL1=DYM1-DMEAN(J)
        DDEL2=DYM2-DMEAN(K)
        DSUM1=DSUM1+DDEL1*DDEL2
 5123   CONTINUE
        DCOV=0.0D0
        DDENOM=DNR1-1.0D0
        IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
        AMAT3(J,K)=AMAT3(J,K) + REAL((DNR1-1.0D0)*DCOV)
        AMAT3(K,J)=AMAT3(J,K)
 5122   CONTINUE
 5121   CONTINUE
!
  100 CONTINUE
!
      ACONST=1.0/REAL(NSUM)
      DO 6100 J=1,NC1
        DO 6200 I=1,NC1
          AMAT3(I,J)=ACONST*AMAT3(I,J)
 6200   CONTINUE
 6100 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF VARPO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NR1,NC1
 9013 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE VARPO2
      SUBROUTINE VECARI(Y1,Y2,N1,IACASE,IWRITE,   &
                        Y3,N3,SCAL3,ITYP3,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT VECTOR     ARITHMETIC OPERATIONS
!              OF THE REAL DATA IN Y1 AND Y2.
!
!     OPERATIONS--ADDITION
!                 SUBTRACTION
!                 DOT PRODUCT
!                 CROSS PRODUCT
!                 LENGTH
!                 DISTANCE
!                 ANGLE
!
!     INPUT  ARGUMENTS--Y1 (REAL)
!                     --Y2 (REAL)
!     OUTPUT ARGUMENTS--Y3 (REAL)
!                       SCAL3
!                       ITYP3
!
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y3(.)
!           BEING IDENTICAL TO THE INPUT VECTOR Y1(.) OR Y2(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/9
!     ORIGINAL VERSION--AUGUST   1987.
!     UPDATED         --SEPTEMBER  1993.  ACTIVATE CROSS PRODUCT (ALAN)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IACASE
      CHARACTER*4 IWRITE
      CHARACTER*4 ITYP3
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DY1
      DOUBLE PRECISION DY2
      DOUBLE PRECISION DY3
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM12
      DOUBLE PRECISION DDEL
      DOUBLE PRECISION DARG1
      DOUBLE PRECISION DARG2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VECA'
      ISUBN2='RI  '
      IERROR='NO'
!
      SCAL3=(-999.0)
      ITYP3='VECT'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CARI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF VECARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IACASE,IWRITE,N1
   52   FORMAT('IBUGA3,ISUBRO,IACASE,IWRITE,N1 = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **************************************************
!               **  CARRY OUT VECTOR     ARITHMETIC OPERATIONS  **
!               **************************************************
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
!               ********************************************
!
      IF(N1.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1151)
 1151   FORMAT('***** ERROR IN VECARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1152)
 1152   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1153)
 1153   FORMAT('      IN THE VARIABLE FOR WHICH')
        CALL DPWRST('XXX','BUG ')
        IF(IACASE.EQ.'VEAD')THEN
          WRITE(ICOUT,1161)
 1161     FORMAT('      THE VECTOR     ADDITION IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VESU')THEN
          WRITE(ICOUT,1162)
 1162     FORMAT('      THE VECTOR     SUBTRACTION IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VEDP')THEN
          WRITE(ICOUT,1163)
 1163     FORMAT('      THE VECTOR     DOT PRODUCT IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VECP')THEN
          WRITE(ICOUT,1164)
 1164     FORMAT('      THE VECTOR     CROSS PRODUCT IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VELE')THEN
          WRITE(ICOUT,1165)
 1165     FORMAT('      THE VECTOR     LENGTH IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VEDI')THEN
          WRITE(ICOUT,1166)
 1166     FORMAT('      THE VECTOR     DISTANCE IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IACASE.EQ.'VEAN')THEN
          WRITE(ICOUT,1167)
 1167     FORMAT('      THE VECTOR     ANGLE IS TO BE COMPUTED')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,1181)
 1181   FORMAT('      MUST BE 1 OR LARGER.  SUCH WAS NOT THE CASE ',   &
               'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)N1
 1183   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *********************************
!               **  STEP 12--                  **
!               **  BRANCH TO THE PROPER CASE  **
!               *********************************
!
      IF(IACASE.EQ.'VEAD')THEN
!
!               *********************************************
!               **  STEP 21--                              **
!               **  TREAT THE VECTOR     ADDITION    CASE  **
!               *********************************************
!
        DO 2110 I=1,N1
          DY1=Y1(I)
          DY2=Y2(I)
          DY3=DY1+DY2
          Y3(I)=DY3
 2110   CONTINUE
        ITYP3='VECT'
        N3=N1
      ELSEIF(IACASE.EQ.'VESU')THEN
!
!               *********************************************
!               **  STEP 22--                              **
!               **  TREAT THE VECTOR     SUBTRACTION CASE  **
!               *********************************************
!
        DO 2210 I=1,N1
          DY1=Y1(I)
          DY2=Y2(I)
          DY3=DY1-DY2
          Y3(I)=DY3
 2210   CONTINUE
!
        ITYP3='VECT'
        N3=N1
      ELSEIF(IACASE.EQ.'VEDP')THEN
!
!               ************************************************
!               **  STEP 23--                                 **
!               **  TREAT THE VECTOR     DOT PRODUCT    CASE  **
!               ************************************************
!
        DSUM12=0.0D0
        DO 2310 I=1,N1
          DY1=Y1(I)
          DY2=Y2(I)
          DSUM12=DSUM12+DY1*DY2
 2310   CONTINUE
        SCAL3=DSUM12
        ITYP3='SCAL'
      ELSEIF(IACASE.EQ.'VECP')THEN
!
!               ************************************************
!               **  STEP 24--                                 **
!               **  TREAT THE VECTOR     CROSS PRODUCT  CASE  **
!               ************************************************
!
!CCCC   SEPTEMBER 1993.  IMPLEMENT THIS SECTION.  NOTE THAT THE
!CCCC   CROSS PRODUCT IS ONLY DEFINED FOR VECTORS OF LENGTH 3.
!
        IF(N1.NE.3)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1151)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2412)
 2412     FORMAT('        THE NUMBER OF ELEMENTS FOR THE CROSS PRODUCT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2413)N1
 2413     FORMAT('        MUST BE EXACTLY 3 (IT WAS ',I8,').')
          CALL DPWRST('XXX','BUG ')
        ELSE
          DARG1=Y1(2)
          DARG2=Y2(3)
          DY1=Y1(3)
          DY2=Y2(2)
          DY3=DARG1*DARG2-DY1*DY2
          Y3(1)=DY3
          DARG1=Y1(3)
          DARG2=Y2(1)
          DY1=Y1(1)
          DY2=Y2(3)
          DY3=DARG1*DARG2-DY1*DY2
          Y3(2)=DY3
          DARG1=Y1(1)
          DARG2=Y2(2)
          DY1=Y1(2)
          DY2=Y2(1)
          DY3=DARG1*DARG2-DY1*DY2
          Y3(3)=DY3
        ENDIF
        ITYP3='VECT'
        N3=N1
      ELSEIF(IACASE.EQ.'VELE')THEN
!
!               ***************************************************
!               **  STEP 25--                                    **
!               **  TREAT THE VECTOR     LENGTH            CASE  **
!               ***************************************************
!
        DSUM1=0.0D0
        DO 2510 I=1,N1
          DY1=Y1(I)
          DSUM1=DSUM1+DY1*DY1
 2510   CONTINUE
        SCAL3=0.0
        IF(DSUM1.GT.0.0D0)SCAL3=DSQRT(DSUM1)
        ITYP3='SCAL'
      ELSEIF(IACASE.EQ.'VEDI')THEN
!
!               ************************************************
!               **  STEP 26--                                 **
!               **  TREAT THE VECTOR     DISTANCE       CASE  **
!               ************************************************
!
        DSUM12=0.0D0
        DO 2610 I=1,N1
          DY1=Y1(I)
          DY2=Y2(I)
          DDEL=DY1-DY2
          DSUM12=DSUM12+DDEL*DDEL
 2610   CONTINUE
        SCAL3=0.0
        IF(DSUM12.GT.0.0D0)SCAL3=DSQRT(DSUM12)
        ITYP3='SCAL'
      ELSEIF(IACASE.EQ.'VEAN')THEN
!
!               ********************************************************
!               **  STEP 27--                                         **
!               **  TREAT THE VECTOR     ANGLE         CASE           **
!               **  THIS ANGLE MUST BE BETWEEN 0 AND 180              **
!               **  AND THIS ANGLE HAS THE PROPERTY THAT              **
!               **  ITS COSINE = INNER PRODUCT / (LENGTH1 * LENGTH2)  **
!               ********************************************************
!
        DSUM1=0.0D0
        DSUM2=0.0D0
        DSUM12=0.0D0
        DO 2710 I=1,N1
          DY1=Y1(I)
          DY2=Y2(I)
          DSUM1=DSUM1+DY1*DY1
          DSUM2=DSUM2+DY2*DY2
          DSUM12=DSUM12+DY1*DY2
 2710   CONTINUE
        DARG1=DSUM1*DSUM2-DSUM12*DSUM12
        IF(DARG1.LE.0.0)DARG1=0.0D0
        IF(DARG1.GT.0.0)DARG1=DSQRT(DARG1)
        DARG2=DSUM12
        SCAL3=DATAN2(DARG1,DARG2)
        ITYP3='SCAL'
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** INTERNAL ERROR IN VECARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      IACASE NOT EQUAL TO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)
 1213   FORMAT('      VEAD, VESU, VEDP, VECP, VELE, VEDI, OR VEAN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1215)
 1215   FORMAT('      IACASE = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CARI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF VECARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IACASE,IWRITE,IERROR,ITYPE3
 9012   FORMAT('IACASE,IWRITE,IERROR,ITYP3 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)N1,N3,SCAL3
 9017   FORMAT('N1,N3,SCAL3 = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(ITYP3.NE.'SCAL')THEN
          DO 9021 I=1,N1
            WRITE(ICOUT,9022)I,Y1(I),Y2(I)
 9022       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
          DO 9031 I=1,N3
            WRITE(ICOUT,9032)I,Y3(I)
 9032       FORMAT('I,Y3(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9031     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE VECARI
      FUNCTION VKAPPA(R)
!
!     THIS FUNCTION IS FROM:
!
! ACM ALGORITHM 571
!
! STATISTICS FOR VON MISES' AND FISCHER'S DISTRIBUTIONS
!
! BY G.W. HILL
!
! ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE, JUNE 1981
!
! ----------------------------------------------------------------
! RETURNS VKAPPA = THE MAXIMUM LIKELIHOOD ESTIMATE OF 'KAPPA', THE
! CONCENTRATION PARAMETER OF VON MISES' DISTRIBUTION OF DIRECTIONS
! IN 2 DIMENSIONS, CORRESPONDING TO A SAMPLE MEAN VECTOR MODULUS R.
! VKAPPA = K(A), THE INVERSE FUNCTION OF  A(K) = RATIO OF MODIFIED
! BESSEL FUNCTIONS OF THE FIRST KIND, VIZ., A(K) = I1(K)/I0(K).
! ----------------------------------------------------------------
!
!  FOR 8S (SIGNIFICANT DECIMAL DIGITS) PRECISION AUXILIARY ROUTINE
!  FUNCTION BESRAT(V) MUST BE SET TO AT LEAST 9.3S
      DATA V1 /0.642/, V2 /0.95/
      X = 0.0
      A = R
      S = -1.0
!
!   ERROR SIGNAL: VALUE -1.0 RETURNED IF ARGUMENT -VE OR 1.0 OR MORE.
      IF (A.LT.0.0 .OR. A.GE.1.0) GO TO 30
      Y = 2.0/(1.0-A)
      IF (A.GT.0.85) GO TO 10
!
!   FOR R BELOW 0.85 USE ADJUSTED INVERSE TAYLOR SERIES.
      X = A*A
      S = (((A-5.6076)*A+5.0797)*A-4.6494)*Y*X - 1.0
      S = ((((S*X+15.0)*X+60.0)*X/360.0+1.0)*X-2.0)*A/(X-1.0)
!CCCC IF (V1-A) 20, 20, 30
      IF (V1-A .LE. 0.0) THEN
         GO TO 20
      ELSE
         GO TO 30
      ENDIF
!
!   FOR R ABOVE 0.85 USE CONTINUED FRACTION APPROXIMATION.
   10 IF (A.GE.0.95) X = 32.0/(120.0*A-131.5+Y)
      IF (A.LT.0.95) X = (-2326.0*A+4317.5526)*A - 2001.035224
      S = (Y+1.0+3.0/(Y-5.0-12.0/(Y-10.0-X)))*0.25
      IF (A.GE.V2) GO TO 30
!
!   FOR R IN (0.642,0.95) APPLY NEWTON-RAPHSON, TWICE IF R IN
!   (0.75,0.875), FOR 8S PRECISION, USING APPROXIMATE DERIVATIVE -
   20 Y = ((0.00048*Y-0.1589)*Y+0.744)*Y - 4.2932
      IF (A.LE.0.875) S = (BESRAT(S)-A)*Y + S
      IF (A.GE.0.75) S = (BESRAT(S)-A)*Y + S
   30 VKAPPA = S
      RETURN
      END FUNCTION VKAPPA
      SUBROUTINE VONCDF(X,B,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE VON MISES DISTRIBUTION
!              WITH SCALE PARAMETER B AND LOCATION PARAMETER A.
!              THIS DISTRIBUTION IS DEFINED FOR ALL  X BETWEEN 0 AND
!              2*PI.  A MUST ALSO BE IN THE RANGE 0 TO 2*PI AND B
!              MUST BE POSITIVE.  WE CALCULATE FOR THE CASE A = 0
!              (VONCDF(X,B)+A FOR A <> 0 CASE).
!              IT HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP[B*COS(X-1)]/[2*PI*I0(B)]
!              WHERE I0 IS THE MODIFIED BESSEL FUNCTION OR ORDER 0.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                       B      = POSITIVE SCALE PARAMETER
!                       A      = LOCATION PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--  -PI <= X <= PI
!                     B >= 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--DBESI0.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS, PEACOCK
!                 "ALGORITHM 518 INCOMPLETE BESSEL FUNCTION I0: THE
!                 VON MISES DISTRIBUTION", GEOFFREY HILL, TRANSACTIONS
!                 OF THE ACM, MATHEMATICAL SOFTWARE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI / 3.1415926535898 /
      DATA TPI / 6.2831853071760 /
      DATA A1 /28.0 /
      DATA A2 /0.5  /
      DATA A3 /100.0/
      DATA A4 /5.0  /
      DATA CK /50.0 /
      DATA C1 /50.1 /
!
!---------------------------------------------------------------------
!
!     STEP 1--CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(B.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)B
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        RETURN
      ENDIF
   24 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
      'VONCDF IS NEGATIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      Z=B
!
!     STEP 2--CONVERT ANGLE X MODULO 2*PI TO (-PI,+PI) INTERVAL
!
      U=AMOD(X+PI,TPI)
      IF(U.LT.0.0)U=U+TPI
      Y=U-PI
      IF(Z.GT.CK)GO TO 300
      V=0.0
      IF(Z.LE.0.0)GO TO 200
!
!  STEP 3--FOR SMALL B, SUM IP TERMS BY BACKWARDS RECURSION
!
      IP=INT(Z*A2-A3/(Z+A4)+A1)
      P=REAL(IP)
      S=SIN(Y)
      C=COS(Y)
      Y=P*Y
      SN=SIN(Y)
      CN=COS(Y)
      R=0.0
      Z=2.0/Z
      DO 100 N=2,IP
        P=P-1.0
        Y=SN
        SN=SN*C - CN*S
        CN=CN*C + Y*S
        R=1.0/(P*Z+R)
        V=(SN/P+V)*R
 100  CONTINUE
 200  CONTINUE
      CDF=(U*0.5+V)/PI
      GO TO 400
!
!  STEP 4--FOR LARGE B, USE A NORMAL APPROXIMATION
!
  300 CONTINUE
      C=24.0*Z
      V=C-C1
      R=SQRT((54.0/(347.0/V+26.0-C)-6.0+C)/6.0)
      Z=SIN(Y*0.5)*R
      S=Z*Z
      V=V-S+3.0
      Y=(C-S-S-16.0)/3.0
      Y=((S+1.75)*S+83.5)/V - Y
      ARG1=Z-S/(Y*Y)*Z
      CALL NORCDF(ARG1,CDFN)
      CDF=CDFN
      GO TO 400
!
! STEP 5--
!
  400 CONTINUE
      IF(CDF.LT.0.0)CDF=0.0
      IF(CDF.GT.1.0)CDF=1.0
!
      RETURN
      END SUBROUTINE VONCDF
      SUBROUTINE VONML1(Y,N,   &
                        XMEAN,XSD,XVAR,XMIN,XMAX,   &
                        ALOCML,SHAPML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE 2-PARAMETER VON MISES DISTRIBUTION FOR THE RAW
!              DATA CASE (I.E., NO CENSORING AND NO GROUPING).  THIS
!              ROUTINE RETURNS ONLY THE POINT ESTIMATES (CONFIDENCE
!              INTERVALS WILL BE COMPUTED IN A SEPARATE ROUTINE).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLGX WILL GENERATE THE OUTPUT
!              FOR THE VON MISES MLE COMMAND).
!
!     REFERENCE--EVANS, HASTINGS, AND PEACOCK (2000), "STATISTICAL
!                DISTRIBUTIONS", THRID EDITION, WILEY, CHAPTER 41.
!              --HILL (1981), "STATISTICS FOR VON MISES' AND
!                FISHER'S DISTRIBUTIONS", ACM TRANSACTIONS ON
!                MATHEMATICAL SOFTWARE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/2
!     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLE1)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION CBAR
      DOUBLE PRECISION SBAR
      DOUBLE PRECISION DTERM1
!
      EXTERNAL VKAPPA
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*40 IDIST
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VONM'
      ISUBN2='L1  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF VONML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN VON MISES ',   &
               'MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N
 1115   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 1135 I=2,N
        IF(Y(I).NE.HOLD)GO TO 1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
!               *****************************************************
!               **  STEP 2--                                       **
!               **  CARRY OUT CALCULATIONS                         **
!               **  FOR VON MISES MLE ESTIMATE                     **
!               *****************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='VON MISES'
!
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
!
      SHAPML=CPUMIN
      ALOCML=CPUMIN
!
!
!     THE MAXIMUM LIKELIHOOD ESTIMATE OF LOCATION IS
!     SIMPLY THE SAMPLE MEAN.  THE MAXIMUM LIKELIHOOD
!     ESTIMATE OF KAPPA IS COMPUTED USING ACM ALGOIRTHM 571.
!     THIS IS A SOLUTION OF THE EQUATION:
!
!         RBAR = I1(KAPPA)/I0(KAPPA)
!
!     WHERE I0 AND I1 ARE MODIFIED BESSEL FUNCTIONS OF THE
!     FIRST KIND OF ORDER 0 AND 1, RESPECTIVELY.  RBAR IS
!     DEFINED BELOW.
!
!     COMPUTE:
!
!        CBAR = (1/N)*SUM[COS(Y(i))]
!        SBAR = (1/N)*SUM[SIN(Y(i))]
!
!        RBAR = SQRT(CBAR**2 + SBAR**2)
!
      ALOCML=XMEAN
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 2120 I=1,N
        DSUM1=DSUM1 + COS(Y(I))
        DSUM2=DSUM2 + SIN(Y(I))
 2120 CONTINUE
      CBAR=DSUM1/DBLE(N)
      SBAR=DSUM2/DBLE(N)
      DTERM1=DSQRT(CBAR**2 + SBAR**2)
      RBAR=REAL(DTERM1)
      SHAPML=VKAPPA(RBAR)
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF VONML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)N,XMEAN,XSD,XMIN,XMAX
 9013   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)DSUM1,DSUM2,CBAR,SBAR,DTERM1,RBAR
 9015   FORMAT('DSUM1,DSUM2,CBAR,SBAR,DTERM1,RBAR =  ',6G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)SHAPML,ALOCML
 9017   FORMAT('SHAPML,ALOCML =  ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE VONML1
      SUBROUTINE VONPDF(X,B,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE VON MISES DISTRIBUTION
!              WITH SCALE PARAMETER B AND LOCATION PARAMETER A.
!              THIS DISTRIBUTION IS DEFINED FOR ALL  X BETWEEN 0 AND
!              2*PI.  A MUST ALSO BE IN THE RANGE 0 TO 2*PI AND B
!              MUST BE POSITIVE.  WE CALCULATE FOR THE CASE A = 0
!              (VONPDF(X,B)+A FOR A <> 0 CASE).
!              IT HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP[B*COS(X-1)]/[2*PI*I0(B)]
!              WHERE I0 IS THE MODIFIED BESSEL FUNCTION OR ORDER 0.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                       B      = POSITIVE SCALE PARAMETER
!                       A      = LOCATION PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--  -PI <= X <= PI
!                     B >= 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--DBESI0.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS, PEACOCK
!                 "ALGORITHM 518 INCOMPLETE BESSEL FUNCTION I0: THE
!                 VON MISES DISTRIBUTION", GEOFFREY HILL, TRANSACTIONS
!                 OF THE ACM, MATHEMATICAL SOFTWARE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
      DOUBLE PRECISION DX, DB
      DOUBLE PRECISION DBESI0
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI / 3.1415926535898 /
      DATA TPI / 6.2831853071760 /
!CCCC DATA A1 /28.0 /
!CCCC DATA A2 /0.5  /
!CCCC DATA A3 /100.0/
!CCCC DATA A4 /5.0  /
!CCCC DATA CK /500.0 /
!CCCC DATA C1 /50.1 /
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(B.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)B
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ELSEIF(B.EQ.0.0)THEN
        PDF=1./(2*PI)
        GO TO 9999
      ENDIF
   24 FORMAT('***** ERROR--THE SECOND ARGUMENT TO VONPDF IS NEGATIVE.')
!CCCC IF(X.LT.-PI.OR.X.GT.PI)THEN
!CCCC   WRITE(ICOUT,4)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,5)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,46)X
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   PDF=0.0
!CCCC   RETURN
!CCCC ENDIF
!CCC4 FORMAT('***** FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO ')
!CCC5 FORMAT('      THE VONPDF SUBROUTINE IS OUTSIDE THE INTERVAL ',
!CCCC*       '(-PI,PI). *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
!
!-----START POINT-----------------------------------------------------
!
!     STEP 1--CONVERT ANGLE X MODULO 2*PI TO (-PI,+PI) INTERVAL
!
      U=AMOD(X+PI,TPI)
      IF(U.LT.0.0)U=U+TPI
      Y=U-PI
      DTERM1=DLOG(D1MACH(2))
      IF(B.GE.REAL(DTERM1))GO TO 300
!
!     STEP 2--COMPUTE BY EXACT FORMULA
!
      DX=DBLE(Y)
      DB=DBLE(B)
      DTERM1=DBESI0(DB)
      DTERM2=DB*DCOS(DX)
      DTERM3=DEXP(DTERM2)
      DTERM4=DTERM3/DTERM1
      PDF=REAL(DTERM4)/(2.0*PI)
      GO TO 9999
!
!     STEP 3--COMPUTE VIA NORMAL APPROXIMATION
!             NORMAL APPROXIMATION IN ACM ALGORITHM 518 IS PROBABLY
!             MORE ACCURATE.  HOWEVER, STANDARD DEVIATION NOT GIVEN
!             IN ORDER TO APPLY PROPER SCALING.  USE THE NORMALIZATION
!             FROM AS 86 (SD=SQRT(B-0.5)).  CAN REVERT TO ACM 518
!             ALGORITHM IF LOCATE STANDARD DEVIATION.
!
  300 CONTINUE
!CCCC Z=B
!CCCC C=24.0*Z
!CCCC V=C-C1
!CCCC R=SQRT((54.0/(347.0/V+26.0-C)-6.0+C)/6.0)
!
!  Z IN LINE BELOW IS B(K)*SIN(THETA/2) IN TERMS OF
!  THE HILL PAPER.
!
!CCCC Z=SIN(Y*0.5)*R
!CCCC S=Z*Z
!CCCC V=V-S+3.0
!CCCC Y=(C-S-S-16.0)/3.0
!CCCC Y=((S+1.75)*S+83.5)/V - Y
!
!CCCC ARG1=Z-S/(Y*Y)*Z
!CCCC CALL NORPDF(ARG1,PDFN)
!CCCC PDF=PDFN
      SD=SQRT(B-0.5)
      ARG1=SD*X
      CALL NORPDF(ARG1,PDFN)
      PDF=SD*PDFN
      GO TO 9999
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE VONPDF
      SUBROUTINE VONPPF(P,B,PPF)
!
!     PURPOSE   --PERCENT POINT FUNCTION FOR THE VON MISES
!                 DISTRIBUTION.  USES A BISECTION METHOD.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--94/9
!     ORIGINAL VERSION--SEPTEMBER 1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA EPS /0.00001/
      DATA SIG /1.0E-6/
      DATA ZERO /0./
      DATA MAXIT /500/
      DATA PI / 3.1415926535898 /
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LT.0.0.OR.P.GT.1.0)GO TO 50
      IF(B.LT.0.0)GO TO 70
      GO TO 90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      RETURN
   70 WRITE(ICOUT,35)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)B
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      GO TO 9999
!
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
      ' VONPPF IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
   35 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
      ' VONPPF IS NEGATIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
   90 CONTINUE
!
!  VON MISES DISTRIBUTION BRACKETED BY (-PI,PI).
!  SET TO -PI IF P=0, SET TO +PI IF P=1.
!
      IF(P.LE.0.0)THEN
        PPF=-PI
        GO TO 9999
      ELSEIF(P.GE.1.0)THEN
        PPF=PI
        GO TO 9999
      ENDIF
      XL=-PI
      XR=PI
!
!  BISECTION METHOD
!
      IC = 0
      FXL = -P
      FXR = 1.0 - P
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL VONCDF(X,B,CDF)
      P1=CDF
      PPF=X
      FCS = P1 - P
      IF(FCS*FXL.GT.ZERO)GO TO 110
      XR = X
      FXR = FCS
      GO TO 115
  110 CONTINUE
      XL = X
      FXL = FCS
  115 CONTINUE
      XRML = XR - XL
      IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GO TO 9999
      IC = IC + 1
      IF(IC.LE.MAXIT)GO TO 105
      WRITE(ICOUT,130)
      CALL DPWRST('XXX','BUG ')
  130 FORMAT('***** ERROR--THE VONPPF ROUTINE DID NOT CONVERGE. ***')
      GO TO 9999
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE VONPPF
      SUBROUTINE VONRAN(N,B,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE VON MISES DISTRIBUTION
!              WITH SHAPE PARAMETER VALUE = P.
!              THIS DISTRIBUTION IS DEFINED FOR ALL  X BETWEEN 0 AND
!              2*PI.  A MUST ALSO BE IN THE RANGE 0 TO 2*PI AND B
!              MUST BE POSITIVE.  WE CALCULATE FOR THE CASE A = 0
!              (VONPDF(X,B)+A FOR A <> 0 CASE).
!              IT HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP[B*COS(X-1)]/[2*PI*I0(B)]
!              WHERE I0 IS THE MODIFIED BESSEL FUNCTION OR ORDER 0.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --B      = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER.  B > 0.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE VON MISES DISTRIBUTION
!             WITH SHAPE PARAMETER VALUE = B.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --B > 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, VONPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--LUC DEVROYE, "NON-UNIFORM RANDOM VARIATE
!                 GENERATION", SPRINGER-VERLANG, 1986, PP. 473-476.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!               --JOHNSON, KOTZ, AND BALAKRISHNAN, CONTINUOUS
!                 UNIVARIATE DISTRIBUTIONS--1, 1994.  CAUCHY CHAPTER.
!               --EVANS, HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--THIRD EDITION, 2000.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--2003.6
!     ORIGINAL VERSION--JUNE      2003.
!     MODIFIED        --APRIL     2004. USE BEST-FISHER ALGORITHM,
!                                       BASED ON REJECTION FROM
!                                       WRAPPED CAUCHY.
!                                       ALGORITHM AS GIVEN BY
!                                       DEVROYE DOESN'T SEEM TO
!                                       BE GIVING REASONABLE RESULTS
!                                       (IN PARTICULAR, THE WRAPPED
!                                       CAUCHY ALGORITH), SO LEAVE
!                                       PERCENT POINT ALGORITHM FOR
!                                       NOW.
!     MODIFIED        --JULY      2008. RESET THE BEST-FISHER
!                                       ALGORITHM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
      DIMENSION U(3)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI / 3.1415926535/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(B.LT.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** ERROR--THE NUMBER OF VON MISES RANDOM ',   &
      'NUMBERS IS NON-POSITIVE.')
   15 FORMAT('***** ERROR--THE SHAPE PARAMETER FOR THE VON ',   &
      'MISES RANDOM NUMBERS IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N VON MISES DISTRIBUTION RANDOM NUMBERS
!     USING THE BEST-FISHER ALGORITHM.
!
!     SETUP:
!        P = SHAPE PARAMETER FOR WRAPPED CAUCHY
!          = (TAU - SQRT(2*PI))/2*B
!     WITH
!        TAU = 1 + SQRT(1 + 4*B**2)
!     THEN
!        R = (1 + P**2)/(2*P)
!
      IF(B.EQ.0.0)THEN
        CALL UNIRAN(N,ISEED,X)
        DO 210 I=1,N
          X(I)=-PI + (2.0*PI)*X(I)
  210   CONTINUE
        GO TO 9000
      ENDIF
!
      TAU=1.0 + SQRT(1.0 + 4.0*B*B)
      P=(TAU-SQRT(2.0*TAU))/(2.0*B)
      R=(1.0 + P*P)/(2.0*P)
      NTEMP=3
!
      DO 100 I=1,N
!
!       GENERATE 3 UNIFORM (0,1) RANDOM NUMBERS;
!
  110   CONTINUE
        CALL UNIRAN(NTEMP,ISEED,U)
        U1=U(1)
        U2=U(2)
        U3=U(3)
        Z=COS(PI*U1)
        F=(1.0+R*Z)/(R+Z)
        C=B*(R-F)
        ATEMP=C*(2.0-C)-U2
        IF(ATEMP.GT.0.0)GO TO 190
        ATEMP=LOG(C/U2) + 1.0 - C
        IF(ATEMP.LT.0.0)GO TO 110
!
  190   CONTINUE
        U3=U3-0.5
        IF(U3.EQ.0.0)THEN
          U1SIGN=0.0
        ELSEIF(U3.LT.0.0)THEN
          U1SIGN=-1.0
        ELSE
          U1SIGN=1.0
        ENDIF
!CCCC   X(I)=U1SIGN/COS(F)
        ARG=F
        IF(ARG.EQ.-1.0)THEN
          RESULT=PI
        ELSEIF(ARG.EQ.0.0)THEN
          RESULT=PI/2.0
        ELSEIF(ARG.EQ.1.0)THEN
          RESULT=0.0
        ELSE
          ARG2=(SQRT(1.0-ARG*ARG))/ARG
          RESULT=ATAN(ARG2)
          IF(RESULT.LT.0.0)RESULT=RESULT+PI
        ENDIF
        X(I)=U1SIGN*RESULT
!
!CCCC   CALL VONPPF(X(I),B,XOUT)
!CCCC   X(I)=XOUT
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE VONRAN
      SUBROUTINE VSCORE(X,N,IWRITE,XNOS,XS,MAXOBV,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE VAN DER WAERDEN SCORES
!              OF THE N ELEMENTS OF THE SINGLE PRECISION VECTOR X,
!              AND PUTS THE RESULTING N SCORES INTO THE
!              SINGLE PRECISION VECTOR XNOS.
!
!              THE VAN DER WAERDEN SCORES ARE DEFINED AS:
!
!                  V(i) = NORPPF(R(X(i)/(N+1))
!
!               THIS IS SOMETIMES GIVEN AS
!
!                  V(i) = NORPPF(i/(N+1))
!
!              WE USE THE FIRST FORM AS IT ACCOMODATES TIED RANKS
!              AND DOES NOT ASSUME THE DATA IS PRE-SORTED.
!
!              WITH R(X(i)) AND NORPPF DENOTING THE RANK OF X(i)
!              AND THE NORMAL PERCENT POINT FUNCTION, RESPECTIVELY.
!
!              THE VAN DER WAERDEN SCORES ARE AN APPROXIMATION TO
!              THE NORMAL ORDER SCORES WHICH ARE
!
!                  NOS(i) =E(Z(i))
!
!              WHERE E(Z(i)) IS THE EXPECTED VALUE OF THE i-TH
!              ORDER STATISTIC FOR A STANDARD NORMAL DISTRIBUTION.
!
!              VAN DER WAERDEN AND NORMAL ORDER SCORES ARE TYPICALLY
!              USED TO TRANSFORM THE RAW DATA BEFORE APPLYING A
!              PARAMETERIC TEST.  THESE ARE SIMILAR TO RANK
!              TRANSFORMATION PROCEDURES.  THEY ARE SOMETIMES PREFERRED
!              TO RANK TRANSFORMATIONS AS THEY SOMETIMES HAVE BETTER
!              PROPERTIES THAN RANK TRANSFORMATION WHEN THE DATA
!              ARE APPROXIMATELY NORMAL WHILE STILL PROVIDING
!              PROTECTION FOR NON-NORMAL DATA.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS TO BE SCORED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XNOS   = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE SCORES FROM X WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR XNOS CONTAINING THE NORMAL
!             ORDER SCORES (IN ASCENDING ORDER) OF THE VALUES
!             IN THE SINGLE PRECISION VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, NORPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE FIRST AND THIRD ARGUMENTS IN THE CALLING SEQUENCE MAY
!              BE IDENTICAL; THAT IS, AN 'IN PLACE' SCORING IS
!              PERMITTED.
!     REFERENCES--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                 THIRD EDITION, WILEY, PP. 397-399, PP. 417-420.
!               --HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                 STATISTICS", DUXBURY ADVANCED SERIES, P. 50.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2022.07
!     ORIGINAL VERSION--JULY      2022.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XNOS(*)
      DIMENSION XS(MAXOBV)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='VSCO'
      ISUBN2='RE  '
      IERROR='NO'
      IUPPER=MAXOBV
!
      K=0
      RPREV=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CORE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF VSCORE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,IUPPER,IBUGA3,ISUBRO
   53   FORMAT('N,IUPPER,IBUGA3,ISUBRO = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN VAN DER WAERDEN SCORES (VSCORE)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)IUPPER
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 1 OR ',   &
               'OR LARGER THAN ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  GENERATE THE RANKS                    **
!               ********************************************
!
      CALL RANK(X,N,IWRITE,XNOS,XS,MAXOBV,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 3--                              **
!               **  NORPPF OF R(i)/(N+1)                  **
!               ********************************************
!
      ADEN=AN + 1.0
      DO 300 I=1,N
        CALL NODPPF(DBLE(XNOS(I)/ADEN),DTERM1)
        XNOS(I)=REAL(DTERM1)
  300 CONTINUE
!
!               ******************************
!               **  STEP 4--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IINDX=1
        WRITE(ICOUT,912)IINDX,XNOS(IINDX)
  912   FORMAT('THE VAN DER WAERDEN SCORE OF ROW ',I10,' = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IINDX=N
        WRITE(ICOUT,912)IINDX,XNOS(IINDX)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CORE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF VSCORE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),XNOS(I)
 9016     FORMAT('I,X(I),XNOS(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE VSCORE
      SUBROUTINE VVLA(VA,X,PV)
!
!       ===================================================
!       Purpose: Compute parabolic cylinder function Vv(x)
!                for large argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PV --- Vv(x)
!       Routines called:
!             (1) DVLA for computing Dv(x) for large |x|
!             (2) GAMMA for computing â(x)
!                 SUBSTITUTE CMLIB "DGAMMA" FUNCTION
!       ===================================================
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PI=3.141592653589793D0
        EPS=1.0D-12
        QE=DEXP(0.25*X*X)
        A0=DABS(X)**(-VA-1.0D0)*DSQRT(2.0D0/PI)*QE
        R=1.0D0
        PV=1.0D0
        DO 10 K=1,18
           R=0.5D0*R*(2.0*K+VA-1.0)*(2.0*K+VA)/(K*X*X)
           PV=PV+R
           IF (DABS(R/PV).LT.EPS) GO TO 15
10      CONTINUE
15      PV=A0*PV
        IF (X.LT.0.0D0) THEN
           X1=-X
           CALL DVLA(VA,X1,PDL)
!CCCC      CALL GAMMA(-VA,GL)
           GL=DGAMM2(-VA)
           DSL=DSIN(PI*VA)*DSIN(PI*VA)
           PV=DSL*GL/PI*PDL-DCOS(PI*VA)*PV
        ENDIF
        RETURN
        END
        SUBROUTINE VVSA(VA,X,PV)
!
!       ===================================================
!       Purpose: Compute parabolic cylinder function Vv(x)
!                for small argument
!       Input:   x  --- Argument
!                va --- Order
!       Output:  PV --- Vv(x)
!       Routine called : GAMMA for computing â(x)
!                SUBSTITUTE CMLIB DGAMMA FUNCTION
!       ===================================================
!
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        EPS=1.0D-15
        PI=3.141592653589793D0
        EP=DEXP(-.25D0*X*X)
        VA0=1.0D0+0.5D0*VA
        IF (X.EQ.0.0) THEN
           IF (VA0.LE.0.0.AND.VA0.EQ.INT(VA0).OR.VA.EQ.0.0) THEN
              PV=0.0D0
           ELSE
              VB0=-0.5D0*VA
              SV0=DSIN(VA0*PI)
!CCCC         CALL GAMMA(VA0,GA0)
              GA0=DGAMM2(VA0)
              PV=2.0D0**VB0*SV0/GA0
           ENDIF
        ELSE
           SQ2=DSQRT(2.0D0)
           A0=2.0D0**(-.5D0*VA)*EP/(2.0D0*PI)
           SV=DSIN(-(VA+.5D0)*PI)
           V1=-.5D0*VA
!CCCC      CALL GAMMA(V1,G1)
           G1=DGAMM2(V1)
           PV=(SV+1.0D0)*G1
           R=1.0D0
           FAC=1.0D0
           DO 10 M=1,250
              VM=.5D0*(M-VA)
!CCCC         CALL GAMMA(VM,GM)
              GM=DGAMM2(VM)
              R=R*SQ2*X/M
              FAC=-FAC
              GW=FAC*SV+1.0D0
              R1=GW*R*GM
              PV=PV+R1
              IF (DABS(R1/PV).LT.EPS.AND.GW.NE.0.0) GO TO 15
10         CONTINUE
15         PV=A0*PV
        ENDIF
        RETURN
        END
      SUBROUTINE WAKML1(Y,N,   &
                        DTEMP1,XMOM,NMOM,   &
                        XMEAN,XSD,XVAR,XMIN,XMAX,   &
                        ALOCLM,SCALLM,SHA1LM,SHA2LM,SHA3LM,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE L-MOMENTS ESTIMATES FOR THE
!              WAKEBY DISTRIBUTION FOR THE RAW DATA CASE (I.E.,
!              NO CENSORING AND NO GROUPING).  THIS ROUTINE RETURNS ONLY
!              THE POINT ESTIMATES.
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLWK WILL GENERATE THE OUTPUT
!              FOR THE WAKEBY MLE COMMAND).
!
!     REFERENCE--FORTRAN CODE WRITTEN FOR INCLUSION IN IBM
!                RESEARCH REPORT RC20525, 'FORTRAN ROUTINES FOR
!                USE WITH THE METHOD OF L-MOMENTS, VERSION 3',
!                J. R. M. HOSKING, IBM RESEARCH DIVISION,
!                T. J. WATSON RESEARCH CENTER, YORKTOWN HEIGHTS
!                NEW YORK 10598, U.S.A., VERSION 3     AUGUST 1996
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/7
!     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLKP)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION XMOM(*)
      DOUBLE PRECISION XPAR(5)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WAKM'
      ISUBN2='L1  '
      IERROR='NO'
      IWRITE='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WAKML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               *******************************************
!               **  STEP 2--                             **
!               **  CARRY OUT CALCULATIONS               **
!               **  FOR WAKEBY MLE ESTIMATE              **
!               *******************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='WAKEBY'
      ALOCLM=CPUMIN
      SCALLM=CPUMIN
      SHA1LM=CPUMIN
      SHA2LM=CPUMIN
      SHA3LM=CPUMIN
!
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL SORT(Y,N,Y)
      NMOM=5
      DO 2110 I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2110 CONTINUE
      CALL SAMLMU(DTEMP1,N,XMOM,NMOM)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KML1')THEN
        WRITE(ICOUT,2120)XMOM(1),XMOM(2),XMOM(3),XMOM(4),XMOM(5)
 2120   FORMAT('XMOM(1),XMOM(2),XMOM(3),XMOM(4),XMOM(5) = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      CALL PELWAK(XMOM,XPAR,IFAIL)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MLKP')THEN
        WRITE(ICOUT,2130)XPAR(1),XPAR(2),XPAR(3),XPAR(4),XPAR(5)
 2130   FORMAT('XPAR(1),XPAR(2),XPAR(3),XPAR(4),XPAR(5) = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(IFAIL.GE.1)GO TO 9000
!
      ALOCLM=REAL(XPAR(1))
      SCALLM=REAL(XPAR(2))
      SHA1LM=REAL(XPAR(3))
      SHA2LM=REAL(XPAR(4))
      SHA3LM=REAL(XPAR(5))
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WAKML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)N,XMEAN,XSD,XMIN,XMAX
 9013   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)SHA1LM,SHA2LM,SHA3LM,SCALLM,ALOCLM
 9017   FORMAT('SHA1LM,SHA2LM,SHA3LM,SCALLM,ALOCLM =  ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WAKML1
      SUBROUTINE WAKRAN(N,BETA,GAMMA,DELTA,ALPHA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE WAKEBY DISTRIBUTION
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --BETA   = THE FIRST SHAPE PARAMETER
!                     --GAMMA  = THE SECOND SHAPE PARAMETER
!                     --DELTA  = THE THIRD SHAPE PARAMETER
!                     --ALPHA  = THE FOURTH SHAPE PARAMETER
!                                (BASICALLY A SCALE PARAMETER)
!                     --SEED   = THE SEED FOR THE RANDOM NUMBER
!                                GENERATOR
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE WAKEBY DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, QUAWAK.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/2
!     ORIGINAL VERSION--FEBRUARY  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
!
      DOUBLE PRECISION XPAR(5)
      DOUBLE PRECISION QUAWAK
      DOUBLE PRECISION DX
      DOUBLE PRECISION DPPF
!
!-----COMMON---------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF WAKEBY ',   &
             'RANDOM NUMBERS IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N WAKEBY RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD
!
      XPAR(1)=0.0D0
      XPAR(2)=DBLE(ALPHA)
      XPAR(3)=DBLE(BETA)
      XPAR(4)=DBLE(GAMMA)
      XPAR(5)=DBLE(DELTA)
!
      DO 100 I=1,N
        DX=DBLE(X(I))
        DPPF=QUAWAK(DX,XPAR)
        X(I)=REAL(DPPF)
  100 CONTINUE
!
      RETURN
      END SUBROUTINE WAKRAN
      SUBROUTINE WARCDF(X,C,A,CDF)
!CCCC SUBROUTINE WARCDF(X,C,A,CDF,IFLAG2)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE WARING
!              DISTRIBUTION WITH SHAPE PARAMETERS = C AND A.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>=0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              F(X,C,A)=(C-A)(A+X-1)!C!/[C(A-1)!(C+X)!]
!              CASE WHERE A = 1 IS THE YULE DISTRIBUTION
!     NOTE--THE WARING DISTRIBUTION IS MATHEMATICALLY EQUIVALENT
!           TO SHIFTED (I.E., START AT X = 0) BETA GEOMETRIC
!           DISTRIBUTION.  SPECIFICALLY, SET
!
!              BETA = A
!              ALPHA = C - A
!
!           AND CALL THE BG2CDF ROUTINE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --C    = THE SHAPE PARAMETER
!                     --A    = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION DENSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --C > A
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, PP. 276-279.
!               --SUDHIR R. PAUL (2004).  "APPLICATIONS OF THE
!                 BETA DISTRIBUTION" in "HANDBOOK OF THE BETA
!                 DISTRIBUTION", EDITED BY GUPTA AND NADARAJAH,
!                 MARCEL-DEKKER, PP.431-436.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!     MODIFIED        --JUNE      1995. FOR BETTER PERFORMANCE, INCLUDE
!                                       A FLAG FOR TRUNCATING IF
!                                       INDIVIDUAL TERMS BELOW SOME
!                                       EPS VALUE.  THIS IS SET FOR
!                                       PPF FUNCTION, PROB PLOT, BUT NOT
!                                       FOR CDF FUNCTION
!     UPDATED         --MAY       2006. USE RELATION TO BETA
!                                       GEOMETRIC DISTRIBUTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
!CCCC DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
!CCCC DOUBLE PRECISION DTERM6, DTERM7
!CCCC DOUBLE PRECISION DX, DC, DA
!CCCC DOUBLE PRECISION DPDF, DSUM
!CCCC DOUBLE PRECISION DLNGAM
!CCCC DOUBLE PRECISION DEPS
!
!CCCC CHARACTER*4 IFLAG2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA DEPS /1.0D-12/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0
      IX=INT(X+0.5)
      IF(C.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WARCDF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(A.LE.0.0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARCDF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(C.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARCDF ',   &
               'IS LARGER THAN THE SECOND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)C
   47   FORMAT('***** THE VALUE OF THE FIRST ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE SECOND ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** WARNING--THE FIRST ARGUMENT TO WARCDF ',   &
               'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
!CCCC DC=DBLE(C)
!CCCC DA=DBLE(A)
!CCCC DSUM=0.0D0
!
!CCCC IF(A.EQ.1.0)THEN
!CCCC   DTERM1=DLOG(DC)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DO1000I=1,IX
!CCCC     DX=DBLE(I)
!CCCC     DTERM2=DLNGAM(DX)
!CCCC     DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC     DTERM7=DTERM1+DTERM2+DTERM3-DTERM6
!CCCC     DPDF=DEXP(DTERM7)
!CCCC     DSUM=DSUM+DPDF
!CCCC     IF(IFLAG2.EQ.'TRUN'.AND.DPDF.LT.DEPS)GO TO 1099
!1000   CONTINUE
!
!CCCC ELSE
!CCCC   DTERM1=DLOG(DC-DA)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DTERM4=DLOG(DC)
!CCCC   DTERM5=DLNGAM(DA)
!CCCC   DO2000I=0,IX
!CCCC     DX=DBLE(I)
!CCCC     DTERM2=DLNGAM(DA+DX)
!CCCC     DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC     DTERM7=DTERM1+DTERM2+DTERM3-DTERM4-DTERM5-DTERM6
!CCCC     DPDF=DEXP(DTERM7)
!CCCC     DSUM=DSUM+DPDF
!CCCC     IF(IFLAG2.EQ.'TRUN'.AND.DPDF.LT.DEPS)GO TO 1099
!2000   CONTINUE
!CCCC ENDIF
!
!CCCC CDF=REAL(DSUM)
!
      BETA=A
      ALPHA=C-A
      CALL BG2CDF(X,ALPHA,BETA,CDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WARCDF
      SUBROUTINE WARFU2(NPAR,XPAR,FVEC,IFLAG,XTEMP,NCLASS)
!
!     PURPOSE--DPMLWA CALLS DNSQE TO SOLVE THE MAXIMUM LIKELIHOOD
!              EQUATIONS.  WARFU2 IS CALLED TO EVALUATE THE EQUATIONS
!              AT A GIVEN SET OF PARAMETERS.  THE LIKELIHOOD EQUATIONS
!              ARE
!                 N/(X*(X-A)) - SUM[K=2 to LAMBDA][V(K)/(X+K-1)]
!                 N/(X-A)) - SUM[K=2 to LAMBDA][V(K)/(A+K-2)]
!              WITH V(K) DENOTING THE CUMULATIVE FREQUENCY FROM
!              K UPWARDS AND X AND A DENOTING THE SHAPE PARAMETERS
!              OF THE WARING DISTRIBUTION.
!     INPUT  ARGUMENTS--XPAR   = THE SINGLE PRECISION VECTOR
!                                CONTAINING THE VALUES OF THE SHAPE
!                                PARAMETERS.
!                       NPAR   = THE NUMBER OF PARAMETERS.
!                       IFLAG  = NOT USED
!                       XTEMP  = ROWS 1 TO NCLASS CONTAIN THE VALUES
!                                OF THE FREQUENCIES AND ROWS
!                                (NCLASS+1) TO 2*NCLASS CONTAIN THE
!                                PRECOMPUTED VALUES OF VK.
!                       NCLASS = THE NUMBER OF FREQUENCY CLASSES.
!     OUTPUT ARGUMENTS--THE VECTOR FVEC CONTAINS THE COMPUTED VALUES
!                       OF THE LIKELIHOOD EQUATIONS.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--IRWIN, "MATHEMATICS IN MEDICAL AND BIOLOGICAL
!                 STATISTICS", JOURNAL OF THE ROYAL STATISTICAL
!                 SOCIETY, SERIES A, VOL. 126, PP. 1-44.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.4
!     ORIGINAL VERSION--APRIL     2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL XTEMP(*)
      DOUBLE PRECISION XPAR(*)
      DOUBLE PRECISION FVEC(*)
!
      COMMON/WARCOM/NTOT
!
      DOUBLE PRECISION TERM1
      DOUBLE PRECISION TERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION XFREQ
      DOUBLE PRECISION VK
      DOUBLE PRECISION X
      DOUBLE PRECISION A
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      NPAR=2
      IFLAG=0
!
      A=XPAR(1)
      X=XPAR(2)
      TERM1=DBLE(NTOT)/(X*(X-A))
      TERM2=DBLE(NTOT)/(X-A)
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 100 K=1,NCLASS
        XFREQ=DBLE(XTEMP(K))
        VK=DBLE(XTEMP(NCLASS+K))
        IF(XFREQ.GE.0.99999D0)THEN
          DSUM1=DSUM1 + VK/(X+DBLE(K)-1.0D0)
        ENDIF
        IF(XFREQ.GE.0.99999D0)THEN
          DSUM2=DSUM2 + VK/(A+DBLE(K)-2.0D0)
        ENDIF
  100 CONTINUE
      FVEC(1)=TERM1 - DSUM1
      FVEC(2)=TERM2 - DSUM2
!
      RETURN
      END SUBROUTINE WARFU2
      SUBROUTINE WARPDF(X,C,A,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE WARING
!              DISTRIBUTION WITH SHAPE PARAMETERS = C AND A.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>=0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              F(X,C,A)=(C-A)(A+X-1)!C!/[C(A-1)!(C+X)!]
!     NOTE--THE WARING DISTRIBUTION IS MATHEMATICALLY EQUIVALENT
!           TO SHIFTED (I.E., START AT X = 0) BETA GEOMETRIC
!           DISTRIBUTION.  SPECIFICALLY, SET
!
!              BETA = A
!              ALPHA = C - A
!
!           AND CALL THE BG2PDF ROUTINE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --C      = THE FIRST SHAPE PARAMETER
!                     --A      = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION DENSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --C > A; C, A > 0
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, PP. 276-279.
!               --SUDHIR R. PAUL (2004).  "APPLICATIONS OF THE
!                 BETA DISTRIBUTION" in "HANDBOOK OF THE BETA
!                 DISTRIBUTION", EDITED BY GUPTA AND NADARAJAH,
!                 MARCEL-DEKKER, PP.431-436.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!     UPDATED         --MAY       2006. USE RELATION TO BETA
!                                       GEOMETRIC DISTRIBUTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
!CCCC DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
!CCCC DOUBLE PRECISION DTERM6, DTERM7
!CCCC DOUBLE PRECISION DX, DC, DA
!CCCC DOUBLE PRECISION DPDF
!CCCC DOUBLE PRECISION DLNGAM
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      IX=INT(X+0.5)
      IF(C.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WARPDF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(A.LE.0.0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARPDF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(C.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARPDF ',   &
               'IS LARGER THAN THE SECOND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)C
   47   FORMAT('***** THE VALUE OF THE FIRST ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE SECOND ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO WARPDF ',   &
              'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
!CCCC DX=DBLE(IX)
!CCCC DC=DBLE(C)
!CCCC DA=DBLE(A)
!
!CCCC IF(A.EQ.1.0)THEN
!CCCC   DTERM1=DLOG(DC)
!CCCC   DTERM2=DLNGAM(DX)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC   DTERM7=DTERM1+DTERM2+DTERM3-DTERM6
!CCCC   DPDF=DEXP(DTERM7)
!CCCC ELSE
!CCCC   DTERM1=DLOG(DC-DA)
!CCCC   DTERM2=DLNGAM(DA+DX)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DTERM4=DLOG(DC)
!CCCC   DTERM5=DLNGAM(DA)
!CCCC   DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC   DTERM7=DTERM1+DTERM2+DTERM3-DTERM4-DTERM5-DTERM6
!CCCC   DPDF=DEXP(DTERM7)
!CCCC ENDIF
!
!CCCC PDF=REAL(DPDF)
!
      BETA=A
      ALPHA=C-A
      CALL BG2PDF(X,ALPHA,BETA,PDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WARPDF
      SUBROUTINE WARPPF(P,C,A,PPF)
!CCCC SUBROUTINE WARPPF(P,C,A,PPF,IFLAG2)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
!              FOR THE WARING DISTRIBUTION (IF A = 1, THIS REDUCES
!              TO THE YULE DISTRIBUTION)
!     NOTE--THE WARING DISTRIBUTION IS MATHEMATICALLY EQUIVALENT
!           TO SHIFTED (I.E., START AT X = 0) BETA GEOMETRIC
!           DISTRIBUTION.  SPECIFICALLY, SET
!
!              BETA = A
!              ALPHA = C - A
!
!           AND CALL THE BG2PPF ROUTINE.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                IT SHOULD BE IN THE INTERVAL (0,1).
!                     --C  = THE FIRST SHAPE PARAMETER
!                     --A  = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1 (EXCLUSIVELY FOR 1).
!                 --C SHOULD BE IN THE INTERVAL (0,1) (EXCLUSIVELY)
!                 --NN SHOULD BE A POSITIVE INTEGER BETWEEN 1 AND MM.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
!             FUNCTION VALUE PPF
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--WARCDF.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--NOTE THAT EVEN THOUGH THE OUTPUT
!              FROM THIS DISCRETE DISTRIBUTION
!              PERCENT POINT FUNCTION
!              SUBROUTINE MUST NECESSARILY BE A
!              DISCRETE INTEGER VALUE,
!              THE OUTPUT VARIABLE PPF IS SINGLE
!              PRECISION IN MODE.
!              PPF HAS BEEN SPECIFIED AS SINGLE
!              PRECISION SO AS TO CONFORM WITH THE DATAPAC
!              CONVENTION THAT ALL OUTPUT VARIABLES FROM ALL
!              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
!              THIS CONVENTION IS BASED ON THE BELIEF THAT
!              1) A MIXTURE OF MODES (FLOATING POINT
!              VERSUS INTEGER) IS INCONSISTENT AND
!              AN UNNECESSARY COMPLICATION
!              IN A DATA ANALYSIS; AND
!              2) FLOATING POINT MACHINE ARITHMETIC
!              (AS OPPOSED TO INTEGER ARITHMETIC)
!              IS THE MORE NATURAL MODE FOR DOING
!              DATA ANALYSIS.
!     REFERENCES--JOHNSON, KOTZ, AND KEMP. DISCRETE
!                 DISTRIBUTIONS, SECOND EDITION, 1992,
!                 PP. 276-279.
!               --SUDHIR R. PAUL (2004).  "APPLICATIONS OF THE
!                 BETA DISTRIBUTION" in "HANDBOOK OF THE BETA
!                 DISTRIBUTION", EDITED BY GUPTA AND NADARAJAH,
!                 MARCEL-DEKKER, PP.431-436.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!     MODIFIED        --JUNE      1995. FOR BETTER PERFORMANCE, INCLUDE
!                                       A FLAG FOR TRUNCATING IF
!                                       INDIVIDUAL TERMS BELOW SOME
!                                       EPS VALUE.
!     MODIFIED        --FEBRUARY  1996. ROUTINE REWRITTEN FOR BETTER
!                                       PERFORMANCE
!     UPDATED         --MAY       2006. USE RELATION TO BETA
!                                       GEOMETRIC DISTRIBUTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*4 IFLAG
!CCCC CHARACTER*4 IFLAG2
!
!---------------------------------------------------------------------
!
!CCCC DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
!CCCC DOUBLE PRECISION DTERM6, DTERM7
!CCCC DOUBLE PRECISION DX, DC, DA
!CCCC DOUBLE PRECISION DPDF, DSUM
!CCCC DOUBLE PRECISION DLNGAM
!CCCC DOUBLE PRECISION DEPS
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO WARPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(C.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WARPPF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(A.LE.0.0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARPPF ',   &
               'IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(C.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WARPPF ',   &
               'IS LARGER THAN THE SECOND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)C
   47   FORMAT('***** THE VALUE OF THE SECOND ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE THIRD ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
!CCCC PPF=1.0
!CCCC IFLAG2='TRUN'
!CCCC IFLAG2='OFF'
!CCCC IFLAG='WARI'
!CCCC IF(A.EQ.1.0)IFLAG='YULE'
!
!     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
!     1) P = 0.0
!
!CCCC IF(P.EQ.0.0)THEN
!CCCC   PPF=0.0
!CCCC   IF(IFLAG.EQ.'YULE')PPF=1.0
!CCCC   GO TO 9999
!CCCC ENDIF
!
!     USE BRUTE FORCE METHOD WHERE CALCULATE CDF UNTIL CUMULATIVE
!     PROBABILITY IS GREATER THAN INPUT PROBABILITY.  DO THIS SINCE
!     WARING CDF DOES NOT CURRENTLY UTILIZE MORE EFFICIENT
!     APPROXIMATIONS.
!
!CCCC IUPPER=2000000
!
!CCCC DC=DBLE(C)
!CCCC DA=DBLE(A)
!CCCC DSUM=0.0D0
!
!CCCC IF(A.EQ.1.0)THEN
!CCCC   DTERM1=DLOG(DC)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DO1000I=0,IUPPER
!CCCC     DX=DBLE(I)
!CCCC     IF(I.EQ.0)GO TO 1000
!CCCC     DTERM2=DLNGAM(DX)
!CCCC     DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC     DTERM7=DTERM1+DTERM2+DTERM3-DTERM6
!CCCC     DPDF=DEXP(DTERM7)
!CCCC     DSUM=DSUM+DPDF
!CCCC     IF(DSUM.GE.DBLE(P))THEN
!CCCC       PPF=REAL(I)
!CCCC       GO TO 9999
!CCCC     ENDIF
!1000   CONTINUE
!CCCC ELSE
!CCCC   DTERM1=DLOG(DC-DA)
!CCCC   DTERM3=DLNGAM(DC+1.0D0)
!CCCC   DTERM4=DLOG(DC)
!CCCC   DTERM5=DLNGAM(DA)
!CCCC   DO2000I=0,IUPPER
!CCCC     DX=DBLE(I)
!CCCC     DTERM2=DLNGAM(DA+DX)
!CCCC     DTERM6=DLNGAM(DC+DX+1.0D0)
!CCCC     DTERM7=DTERM1+DTERM2+DTERM3-DTERM4-DTERM5-DTERM6
!CCCC     DPDF=DEXP(DTERM7)
!CCCC     DSUM=DSUM+DPDF
!CCCC     IF(DSUM.GE.DBLE(P))THEN
!CCCC       PPF=REAL(I)
!CCCC       GO TO 9999
!CCCC     ENDIF
!2000   CONTINUE
!CCCC ENDIF
!
!CCCC PPF=REAL(IUPPER)
!CCCC WRITE(ICOUT,3000)IUPPER,IUPPER
!3000 FORMAT('****** PPF VALUE EXCEEDS ',I8,' .  TRUNCATED AT ',
!CCCC1'THIS VALUE.')
!CCCC CALL DPWRST('XXX','BUG ')
!
      BETA=A
      ALPHA=C-A
      CALL BG2PPF(P,ALPHA,BETA,PPF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WARPPF
      SUBROUTINE WBLEST(X,NOBS,ALPHA,BETA,IERROR)
!
!   Written by Fred Todt, Battelle Columbus, Sept. 1985
!
!  COMPUTE MLES FOR SHAPE PARAMETER (BETA) AND SCALE
!  PARAMETER (ALPHA) BY SOLVING THE EQUATION  G(BETA)=0, WHERE G IS
!  A MONOTONICALLY INCREASING FUNCTION OF BETA.
!  THE INITIAL ESTIMATE IS:  RI=(1.28)/(STD. DEV. OF LOG(X)'S)
!  AND THE TOLERANCE IS   :   2*RI/(10**6).
!
      DIMENSION X(*)
      DOUBLE PRECISION SUMY
      DOUBLE PRECISION SUMYSQ
      CHARACTER*4 IERROR
!
      IERROR='NO'
      RN=REAL(NOBS)
      SUMY=0.0
      SUMYSQ=0.0
      BETAL=0.0
      BETAH=0.0
!
      DO  2 I=1,NOBS
         Y=LOG(X(I))
         SUMY=SUMY+DBLE(Y)
         SUMYSQ=SUMYSQ+(DBLE(Y)**2)
    2 CONTINUE
!
      YSTD=SQRT((SNGL(SUMYSQ)-(SNGL(SUMY)**2)/RN)/(RN-1.0))
      XGM=EXP(SNGL(SUMY)/RN)
      RI=1.28/YSTD
      TOL=2.0*.000001*RI
      BETAM=RI
      GFM=GFUNCT(X,NOBS,BETAM,XGM)
!CCCC WRITE (*,*) ' XGM, RI, GFM ',XGM, RI, GFM
!
!  IF G(BETAM) .GE. 0, DIVIDE THE INITIAL ESTIMATE BY 2 UNTIL
!  THE ROOT IS BRACKETED BY BETAL AND BETAH.
      IF(GFM.GE.0.0)THEN
           DO 3 J=1,20
                BETAH=BETAM
                BETAM=BETAM/2.0
                GFM=GFUNCT(X,NOBS,BETAM,XGM)
                IF(GFM.LE.0.0)GO TO 4
    3      CONTINUE
!CCCC      STOP 'GFM NEVER LE 0'
           IERROR='YES'
           GO TO 9999
    4      CONTINUE
           BETAL=BETAM
      ENDIF
!
!  IF G(BETAM) .LT. 0, MULTIPLY THE INITIAL ESTIMATE BY 2 UNTIL
!  THE ROOT IS BRACKETED BY BETAL AND BETAH.
      IF(GFM.LT.0.0)THEN
           DO 7 J=1,20
                BETAL=BETAM
                BETAM=BETAM*2.0
                GFM=GFUNCT(X,NOBS,BETAM,XGM)
                IF(GFM.GE.0.0)GO TO 8
    7      CONTINUE
!CCCC      STOP 'GFM NEVER GE 0'
           IERROR='YES'
           GO TO 9999
    8      CONTINUE
           BETAH=BETAM
      ENDIF
!
! SOLVE THE EQUATION G(BETA)=0 FOR BETA BY BISECTING THE
!   INTERVAL (BETAL,BETAH) UNTIL THE TOLERANCE IS MET
   10 CONTINUE
      BETAM=(BETAL+BETAH)/2.0
      GFM=GFUNCT(X,NOBS,BETAM,XGM)
      IF(GFM.GE.0.0)THEN
           BETAH=BETAM
      ENDIF
      IF(GFM.LT.0.0)THEN
           BETAL=BETAM
      ENDIF
      IF(BETAH-BETAL.GT.TOL)GO TO 10
!
      BETA=(BETAL+BETAH)/2.0
      ALPHA=FNALPH(X,NOBS,BETA,XGM)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WBLEST
      SUBROUTINE WBLES2(X,N,IR,ALPHA,GAMMA,IERROR)
!
!  COMPUTE MLES FOR SHAPE PARAMETER (GAMMA) AND SCALE
!  PARAMETER (ALPHA) BY SOLVING THE EQUATION  G(GAMMA)=0, WHERE G IS
!  A MONOTONICALLY INCREASING FUNCTION OF GAMMA.
!  THE INITIAL ESTIMATE IS:  RI=(1.28)/(STD. DEV. OF LOG(X)'S)
!  AND THE TOLERANCE IS   :   2*RI/(10**6).
!
      DIMENSION X(*)
      CHARACTER*4 IERROR
      PARAMETER (MAXIT=20000)
!
      IERROR='NO'
      RN=REAL(IR)
      GAMMAL=0.0
      GAMMAH=0.0
      CALL WBLEST(X,IR,ALPHA,GAMMA,IERROR)
!
      RI=GAMMA
      TOL=2.0*.000001*RI
      GAMMAM=RI
      CALL GFUNC2(X,N,IR,ALPHA,GAMMAM,GFM)
!
!  IF G(GAMMAM) .GE. 0, DIVIDE THE INITIAL ESTIMATE BY 2 UNTIL
!  THE ROOT IS BRACKETED BY GAMMAL AND GAMMAH.
      IF(GFM.GE.0.0)THEN
           DO 3 J=1,20
                GAMMAH=GAMMAM
                GAMMAM=GAMMAM/2.0
                CALL GFUNC2(X,N,IR,ALPHA,GAMMAM,GFM)
                IF(GFM.LE.0.0)GO TO 4
    3      CONTINUE
!CCCC      STOP 'GFM NEVER LE 0'
           IERROR='YES'
           GO TO 9999
    4      CONTINUE
           GAMMAL=GAMMAM
      ENDIF
!
!  IF G(GAMMAM) .LT. 0, MULTIPLY THE INITIAL ESTIMATE BY 2 UNTIL
!  THE ROOT IS BRACKETED BY GAMMAL AND GAMMAH.
      IF(GFM.LT.0.0)THEN
           DO 7 J=1,20
                GAMMAL=GAMMAM
                GAMMAM=GAMMAM*2.0
                CALL GFUNC2(X,N,IR,ALPHA,GAMMAM,GFM)
                IF(GFM.GE.0.0)GO TO 8
    7      CONTINUE
!CCCC      STOP 'GFM NEVER GE 0'
           IERROR='YES'
           GO TO 9999
    8      CONTINUE
           GAMMAH=GAMMAM
      ENDIF
!
! SOLVE THE EQUATION G(GAMMA)=0 FOR GAMMA BY BISECTING THE
!   INTERVAL (GAMMAL,GAMMAH) UNTIL THE TOLERANCE IS MET
      NUMIT=0
   10 CONTINUE
      NUMIT=NUMIT+1
      IF(NUMIT.GT.MAXIT)THEN
        IERROR='YES'
        GO TO 9999
      ENDIF
      GAMMAM=(GAMMAL+GAMMAH)/2.0
      CALL GFUNC2(X,N,IR,ALPHA,GAMMAM,GFM)
      IF(GFM.GE.0.0)THEN
           GAMMAH=GAMMAM
      ENDIF
      IF(GFM.LT.0.0)THEN
           GAMMAL=GAMMAM
      ENDIF
      IF(GAMMAH-GAMMAL.GT.TOL)GO TO 10
!
      GAMMA=(GAMMAL+GAMMAH)/2.0
      ALPHA=FNALP2(X,N,IR,GAMMA)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WBLES2
      SUBROUTINE WCACDF(X,P,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE WRAPPED CAUCHY DISTRIBUTION
!              THIS DISTRIBUTION IS DEFINED FOR 0<=X<=2*PI AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/(2*PI)*(1-P**2)/(1+P**2-2*P*COS(X))
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CAUCHY DISTRIBUTION CHAPTER
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL P
      DOUBLE PRECISION DX
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DP
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DPI/3.14159265358979D0/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!
      IF(X.LT.0.0.OR.X.GT.SNGL(2.0D0*DPI))THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
      'WCACDF IS OUTSIDE THE (0,2*PI) INTERVAL.')
    2 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
      'WCACDF IS OUTSIDE THE (0,1) INTERVAL.')
   46 FORMAT('      THE ARGUMENT HAS THE VALUE ',G15.7)
!
      DX=DBLE(X)
      DP=DBLE(P)
      IF(DX.LE.DPI)THEN
        DTERM3=-DCOS(DX/2.0D0)+DP*DCOS(DX/2.0D0)
        DTERM1=(-DSIN(DX/2.0D0)-DP*DSIN(DX/2.0D0))/DTERM3
        DTERM2=(DSIN(DX/2.0D0)+DP*DSIN(DX/2.0D0))/DTERM3
        DCDF=(DATAN(DTERM1) - DATAN(DTERM2))/(2.0D0*DPI)
        CDF=REAL(DCDF)
      ELSE
        DX=2.0D0*DPI - DX
        DTERM3=-DCOS(DX/2.0D0)+DP*DCOS(DX/2.0D0)
        DTERM1=(-DSIN(DX/2.0D0)-DP*DSIN(DX/2.0D0))/DTERM3
        DTERM2=(DSIN(DX/2.0D0)+DP*DSIN(DX/2.0D0))/DTERM3
        DCDF=(DATAN(DTERM1) - DATAN(DTERM2))/(2.0D0*DPI)
        DCDF=1.0D0 - DCDF
        CDF=REAL(DCDF)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WCACDF
      SUBROUTINE WCACD2(X,DP,DCDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE WRAPPED CAUCHY DISTRIBUTION
!              THIS DISTRIBUTION IS DEFINED FOR 0<=X<=2*PI AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/(2*PI)*(1-P**2)/(1+P**2-2*P*COS(X))
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--CDF    = THE DOULE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMUALATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     NOTE--THIS IS A DOUBLE PRECSION VERSION OF WCACDF THAT IS
!           USED BY THE WCAPPF ROUTINE FOR GREATER ACCURACY.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CAUCHY DISTRIBUTION CHAPTER
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JANUARY   2005.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION DX
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DP
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DPI/3.14159265358979D0/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!
      DX=X
!
      IF(DX.LE.0.0D0)THEN
        DCDF=0.0D0
      ELSEIF(DX.GE.2.0D0*DPI)THEN
        DCDF=1.0D0
      ENDIF
      IF(DX.LT.0.0D0.OR.DX.GT.2.0D0*DPI)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(DP.LT.0.0D0.OR.DP.GE.1.0D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DP
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
      'WCACD2 IS OUTSIDE THE (0,2*PI) INTERVAL.')
    2 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
      'WCACD2 IS OUTSIDE THE (0,1) INTERVAL.')
   46 FORMAT('      THE ARGUMENT HAS THE VALUE ',G15.7)
!
      IF(DX.LE.DPI)THEN
        DTERM3=-DCOS(DX/2.0D0)+DP*DCOS(DX/2.0D0)
        DTERM1=(-DSIN(DX/2.0D0)-DP*DSIN(DX/2.0D0))/DTERM3
        DTERM2=(DSIN(DX/2.0D0)+DP*DSIN(DX/2.0D0))/DTERM3
        DCDF=(DATAN(DTERM1) - DATAN(DTERM2))/(2.0D0*DPI)
      ELSE
        DX=2.0D0*DPI-DX
        DTERM3=-DCOS(DX/2.0D0)+DP*DCOS(DX/2.0D0)
        DTERM1=(-DSIN(DX/2.0D0)-DP*DSIN(DX/2.0D0))/DTERM3
        DTERM2=(DSIN(DX/2.0D0)+DP*DSIN(DX/2.0D0))/DTERM3
        DCDF=1.0D0-(DATAN(DTERM1) - DATAN(DTERM2))/(2.0D0*DPI)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WCACD2
      SUBROUTINE WCAPDF(X,P,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE WRAPPED CAUCHY DISTRIBUTION
!              THIS DISTRIBUTION IS DEFINED FOR 0<=X<=2*PI AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/(2*PI)*(1-P**2)/(1+P**2-2*P*COS(X))
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON, KOTZ, AND BALAKRISHNAN, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CAUCHY DISTRIBUTION CHAPTER
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER   1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL P
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA C/0.1591549/
      DATA TWOPI/6.283185/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!
      IF(X.LT.0.0.OR.X.GT.TWOPI)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
      'WCAPDF IS OUTSIDE THE (0,2*PI) INTERVAL.')
    2 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
      'WCAPDF IS OUTSIDE THE (0,1) INTERVAL.')
   46 FORMAT('      THE ARGUMENT HAS THE VALUE ',G15.7)
!
      IF(P.EQ.0.0)THEN
        PDF=1.0/TWOPI
      ELSE
        PDF=C*(1.0-P*P)/(1+P*P-2.0*P*COS(X))
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE WCAPDF
      SUBROUTINE WCAPPF(P,AP,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE WRAPPED CAUCHY DISTRIBUTION
!              THIS DISTRIBUTION IS DEFINED FOR 0<=X<=2*PI AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/(2*PI)*(1-P**2)/(1+P**2-2*P*COS(X))
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CAUCHY DISTRIBUTION CHAPTER
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/10
!     ORIGINAL VERSION--OCTOBER   1995.
!     UPDATED         --JANUARY   2005. CONVERT TO DOUBLE PRECISION
!                                       FOR GREATER ACCURACY
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DP
      DOUBLE PRECISION DAP
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DPI
      DOUBLE PRECISION TWOPI
      DOUBLE PRECISION EPS
      DOUBLE PRECISION SIG
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION XL
      DOUBLE PRECISION XR
      DOUBLE PRECISION FXL
      DOUBLE PRECISION FXR
      DOUBLE PRECISION X
      DOUBLE PRECISION CDF
      DOUBLE PRECISION P1
      DOUBLE PRECISION FCS
      DOUBLE PRECISION XRML
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA TWOPI/6.283185/
      DATA DPI/ 3.14159265358979D+00/
      DATA EPS /1.0D-6/
      DATA SIG /1.0D-6/
      DATA ZERO /0.0D0/
      DATA MAXIT /2000/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      DPPF=0.0D0
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
               'WCAPPF IS OUTSIDE THE ALLOWABLE [0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(AP.LT.0.0.OR.AP.GE.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
               'WCAPPF IS OUTSIDE THE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)AP
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      IF(P.EQ.0.0)THEN
        DPPF=0.0
        GO TO 9999
      ELSEIF(P.EQ.0.5)THEN
        DPPF=REAL(DPI)
        GO TO 9999
      ELSEIF(P.EQ.1.0)THEN
        DPPF=TWOPI
        GO TO 9999
      ENDIF
!
!CCCC EPS=1.0D-6
!CCCC SIG=1.0D-6
!CCCC IF(AP.GE.0.9 .AND. P.GE.0.9)THEN
!CCCC   EPS=1.0D-5
!CCCC   SIG=1.0D-5
!CCCC ENDIF
      TWOPI=2.0D0*DPI
      DP=DBLE(P)
      DAP=DBLE(AP)
!
      IERR=0
      IC = 0
      IF(P.LE.0.5)THEN
        XL = 0.0D0
        XR = DPI
      ELSE
        XL = DPI
        XR = TWOPI
      ENDIF
      FXL=-DP
      FXR=1.0D0 - DP
!
!  BISECTION METHOD
!
  105 CONTINUE
      X = (XL+XR)*0.5D0
      CALL WCACD2(X,DAP,CDF)
      P1=CDF
      DPPF=X
      FCS = P1 - DP
      IF(FCS*FXL.GT.ZERO)GO TO 110
      XR = X
      FXR = FCS
      GO TO 115
  110 CONTINUE
      XL = X
      FXL = FCS
  115 CONTINUE
      XRML = XR - XL
      IF(XRML.LE.SIG .AND. DABS(FCS).LE.EPS)GO TO 9999
      IC = IC + 1
      IF(IC.LE.MAXIT)GO TO 105
      WRITE(ICOUT,130)
      CALL DPWRST('XXX','BUG ')
  130 FORMAT('***** ERROR--WCAPPF ROUTINE DID NOT CONVERGE.')
      GO TO 9999
!
 9999 CONTINUE
      PPF=REAL(DPPF)
      RETURN
      END SUBROUTINE WCAPPF
      SUBROUTINE WCARAN(N,P,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE WRAPPED CAUCHY DISTRIBUTION
!              WITH SHAPE PARAMETER VALUE = P.
!              THIS DISTRIBUTION IS DEFINED FOR 0<=X<=2*PI AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = (1/(2*PI)*(1-P**2)/(1+P**2-2*P*COS(X))
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --P      = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER.  0 <= P <= 1
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE WRAPPED CAUCHY DISTRIBUTION
!             WITH SHAPE PARAMETER VALUE = P.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --0 <= P <= 1
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!               --JOHNSON, KOTZ, AND BALAKRISHNAN, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994.  CAUCJY CHAPTER.
!               --EVANS, HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--THIRD EDITION, 2000.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.6
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL P
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** ERROR--THE NUMBER OF WRAPPED CAUCHY RANDOM ',   &
      'NUMBERS IS NON-POSITIVE.')
   15 FORMAT('***** ERROR--THE SHAPE PARAMETER FOR THE WRAPPED ',   &
      'CAUCHY RANDOM NUMBERS IS OUTSIDE THE (0,1] INTERVAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N WRAPPED CAUCHY DISTRIBUTION RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
        CALL WCAPPF(X(I),P,XOUT)
        X(I)=XOUT
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WCARAN
      SUBROUTINE WECODI(X,Y,W,N,ICASE,IWRITE,WCOSDI,WCOSSI,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED COSINE DISTANCE
!              OF THE DATA IN X AND Y WITH THE WEIGHTS IN W.  THE WEIGHTED
!              COSINE SIMILARITY WILL BE A SINGLE PRECISION VALUE CALCULATED AS:
!
!                  S = SUM[i=1 to N][W(i)*X(i)*Y(i)]/
!                      SQRT{SUM[i=1 to N][W(i)*X(i)**2]*
!                      SUM[i=1 to N][W(i)*Y(i)**2]}
!
!              THE WEIGHTED COSINE DISTANCE IS THEN
!
!                   D = 1 - S
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF UNSORTED
!                                OBSERVATIONS FOR THE FIRST RESPONSE
!                                VARIABLE
!                     --Y      = THE SINGLE PRECISION VECTOR OF UNSORTED
!                                OBSERVATIONS FOR THE SECOND RESPONSE
!                                VARIABLE
!                     --W      = THE SINGLE PRECISION VECTOR OF WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X, Y AND W.
!     OUTPUT ARGUMENTS--WCODI  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED COSINE DISTANCE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE WEIGHTED
!             COSINE DISTANCE OF THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2018/08
!     ORIGINAL VERSION--AUGUST    2018.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMY
      DOUBLE PRECISION DSUMXY
      DOUBLE PRECISION DSUMW
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WECO'
      ISUBN2='DI  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMW=0.0D0
      WCOSSI=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CODI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WECODI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I),W(I)
   56     FORMAT('I,X(I),Y(I),W(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED COSINE DISTANCE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 8000
      ENDIF
!
      DSUMW=0.0D0
      DO 120 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)I,W(I)
  122     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DSUMW=DSUMw + DBLE(W(I))
!
  120 CONTINUE
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED COSINE DISTANCE **
!               ************************************************
!
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMXY=0.0D0
      DO 1100 I=1,N
!
        DX=DBLE(X(I))
        DY=DBLE(Y(I))
        DW=DBLE(W(I))
        DSUMX=DSUMX+DX*DX*DW
        DSUMY=DSUMY+DY*DY*DW
        DSUMXY=DSUMXY+DX*DY*DW
!
 1100 CONTINUE
!
      IF(DSUMW.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1147)
 1147   FORMAT('      THE SUM OF THE WEIGHTS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(DSUMX.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1157)
 1157   FORMAT('      THE SUM OF SQUARES FOR THE FIRST RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(DSUMY.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1167)
 1167   FORMAT('      THE SUM OF SQUARES FOR THE SECOND RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSE
        DTERM1=DSUMXY/SQRT(DSUMX*DSUMY)
        WCOSSI=REAL(DTERM1)
      ENDIF
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
 8000 CONTINUE
!
      WCOSDI=1.0 - WCOSSI
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE.EQ.'WCDI')THEN
          WRITE(ICOUT,1211)N,WCOSDI
 1211     FORMAT('THE WEIGHTED COSINE DISTANCE OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,1213)N,WCOSSI
 1213     FORMAT('THE WEIGHTED COSINE SIMILARITY OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      WCOSDI=1.0 - WCOSSI
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CODI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WECODI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DSUMX,DSUMY,DSUMXY,DSUMW
 9014   FORMAT('IERROR,DSUMX,DSUMY,DSUMXY,DSUMW = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WECODI
      SUBROUTINE WECOVA(X,Y,W,N,ICASE,IWRITE,WCOVA,WCORR,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED COVARIANCE
!              OF THE DATA IN X AND Y WITH THE WEIGHTS IN W.  THE WEIGHTED
!              COVARIANCE WILL BE A SINGLE PRECISION VALUE CALCULATED AS:
!
!                  SUM[i=1 to N][W(i)*((X(i) - M(X;W))*((Y(i) - M(Y;W))]/
!                  SUM[i=1 to N][W(i)]
!
!              WHERE M(X;W) IS THE WEIGHTED MEAN
!
!              IN ADDITION, THE WEIGHTED CORRELATION IS ALSO COMPUTED.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF UNSORTED
!                                OBSERVATIONS FOR THE FIRST RESPONSE
!                                VARIABLE
!                     --Y      = THE SINGLE PRECISION VECTOR OF UNSORTED
!                                OBSERVATIONS FOR THE SECOND RESPONSE
!                                VARIABLE
!                     --W      = THE SINGLE PRECISION VECTOR OF WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X, Y AND W.
!     OUTPUT ARGUMENTS--WCOVA  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED COVARIANCE.
!                     --WCORR  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED CORRELATION.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE WEIGHTED
!             COVARIANCE/CORRELATION OF THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2018/07
!     ORIGINAL VERSION--JULY      2018.
!     UPDATED         --AUGUST    2020. CORRECT COMPUTATION OF
!                                       OF WEIGHTED MEAN
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      DOUBLE PRECISION DW
      DOUBLE PRECISION DMX
      DOUBLE PRECISION DMY
      DOUBLE PRECISION DSDX
      DOUBLE PRECISION DSDY
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMY
      DOUBLE PRECISION DSUMW
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WECO'
      ISUBN2='VA  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
      WCOVA=0.0
      WCORR=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'COVA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WECOVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,N
   52   FORMAT('IBUGA3,ISUBRO,ICASE,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I),W(I)
   56     FORMAT('I,X(I),Y(I),W(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED COVARIANCE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        WCOVA=0.0
        WCORR=1.0
        GO TO 9000
      ENDIF
!
      DSUMW=0.0D0
      DO 120 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)I,W(I)
  122     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DSUMW=DSUMW + DBLE(W(I))
!
  120 CONTINUE
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED COVARIANCE      **
!               ************************************************
!
      DN=DBLE(N)
      DSUMX=0.0D0
      DSUMY=0.0D0
      DO 1100 I=1,N
!
        DX=DBLE(X(I))
        DW=DBLE(W(I))
        DSUMX=DSUMX+DX*DW
!
        DY=DBLE(Y(I))
        DSUMY=DSUMY+DY*DW
!
 1100 CONTINUE
!CCCC DMX=DSUMX/DN
!CCCC DMY=DSUMY/DN
      DMX=DSUMX/DSUMW
      DMY=DSUMY/DSUMW
!
      IF(DSUMW.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1147)
 1147   FORMAT('      THE SUM OF THE WEIGHTS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WCOVA=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO 1200 I=1,N
        DX=DBLE(X(I)) - DMX
        DY=DBLE(Y(I)) - DMY
        DW=DBLE(W(I))
        DSUM1=DSUM1+DW*DX*DY
        DSUM2=DSUM2+DW*DX*DX
        DSUM3=DSUM3+DW*DY*DY
 1200 CONTINUE
!
       DCOVA=DSUM1/DSUMW
       WCOVA=REAL(DCOVA)
       DSDX=DSQRT(DSUM2/DSUMW)
       DSDY=DSQRT(DSUM3/DSUMW)
       DCORR=DCOVA/(DSDX*DSDY)
       WCORR=REAL(DCORR)
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE.EQ.'WCOV')THEN
          WRITE(ICOUT,1211)N,WCOVA
 1211     FORMAT('THE WEIGHTED COVARIANCE OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'WCOR')THEN
          WRITE(ICOUT,1213)N,WCORR
 1213     FORMAT('THE WEIGHTED CORRELATION OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'COVA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WECOVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUMX,DSUMY,DSUMW
 9014   FORMAT('IERROR,DN,DSUMX,DSUMY,DSUMW = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DSUM1,DSUM2,DSUM3,DCOVA,DCORR
 9015   FORMAT('DSUM1,DSUM2,DSUM3,DCOVA,DCORR = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)DMX,DMY,DSDX,DSDY
 9016   FORMAT('DMX,DMY,DSDX,DSDY = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)WCORR,WCOVA
 9018   FORMAT('WCORR,WCOVA = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WECOVA
      SUBROUTINE WEIBAR(X,N,IWRITE,Y,IBUGA3,IERROR)
!
!
!     PURPOSE--THIS SUBROUTINE GENERATES THE N ADJUSTED RANKS
!              FOR A WEIBULL PLOT
!     INPUT  ARGUMENTS--X        = A FLOATING POINT TAG VARIABLE
!                                  CONSISTING OF 1'S AND 0'S
!                                  IN WHICH 1 IMPLIES
!                                  DATA POINT IS TO BE INLCUDED IN ANALYSIS
!                                  AND 0 IMPLIES THE DATA POINT IS A
!                                  SUSPENDED (= CENSORED) ITEM.
!                     --N      = THE INTEGER NUMBER (VALID + SUSPENDED)
!                                OF DATA POINTS (AND VALUES IN    TAG).
!     OUTPUT ARGUMENTS--Y      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                ADJUSTED RANKS
!                                WILL BE PLACED.
!     OUTPUT--THE N ADJUSTED RANKS FOR A WEIBULL PLOT
!     NOTE--THE ADJUSTED RANKS AT X=0 ELEMENTS
!           ARE NEVER USED IN FURTHER ANALYSES.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--ABERNATHY ET AL, WEIBULL ANALYSIS HANDBOOK
!                PAGES 20-21.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     VERSION NUMBER--85.6
!     ORIGINAL VERSION--APRIL     1985.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIB'
      ISUBN2='AR  '
      IERROR='NO'
!
      NVALID=(-999)
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF WEIBAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************
!               **  COMPUTE WEIBULL ADJUSTED RANKS.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.GE.1)GO TO 119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN WEIBAR--',   &
      'THE 2ND INPUT ARGUMENT (N) IS SMALLER THAN 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,118)N
  118 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
!               *******************************
!               **  STEP 2--                 **
!               **  FORM THE ADJUSTED RANKS  **
!               *******************************
!
      AN=N
!
!     SET INITIAL VALUE FOR SAVED ADJUSTED RANK.
!     SET INITIAL VALUE FOR RANK INCREMENT.
!
      SAVEAR=0.0
!
      I=0
      ANUM=(AN+1.0)-SAVEAR
      ADENOM=1+(N-I)
      RANINC=ANUM/ADENOM
!
      NVALID=0
      DO 1100 I=1,N
      ITAGI=INT(X(I)+0.5)
      IF(ITAGI.EQ.1)GO TO 1200
      GO TO 1300
!
!     TREAT THE VALID (TO BE INCLUDED) ITEM CASE.
!     COMPUTE THE ADJUSTED RANK.
!     SAVE THE ADJUSTED RANK.
!     DO NOT RECOMPUTE THE RANK INCREMENT.
!
 1200 CONTINUE
      NVALID=NVALID+1
      Y(I)=SAVEAR+RANINC
      SAVEAR=Y(I)
      GO TO 1190
!
!     TREAT THE SUSPENDED (= CENSORED) ITEM CASE
!     RECOMPUTE THE RANK INCREMENT.
!     DO NOT RECOMPUTE THE SAVED ADJUSTED RANK.
!
 1300 CONTINUE
      ANUM=(AN+1.0)-SAVEAR
      ADENOM=1+(N-I)
      RANINC=ANUM/ADENOM
      GO TO 1190
!
 1190 CONTINUE
!CCCC WRITE(ICOUT,1191)I,ITAGI,SAVEAR,ANUM,ADENOM,RANINC,Y(I)
!1191 FORMAT('I,ITAGI,SAVEAR,ANUM,ADENOM,RANINC,Y(I) = ',
!CCCC12I8,5E12.5)
!CCCC CALL DPWRST('XXX','BUG ')
 1100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE ADJUSTED RANKS.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 1890
      IF(IWRITE.EQ.'OFF')GO TO 1890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1811)N
 1811 FORMAT('TOTAL NUMBER OF VALUES (VALID + SUSPENDED) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1812)NVALID
 1812 FORMAT('TOTAL NUMBER OF VALUES (VALID ONLY       ) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1813)Y(1)
 1813 FORMAT('THE FIRST ELEMENT IN OUTPUT VARIABLE = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1814)Y(N)
 1814 FORMAT('THE LAST  ELEMENT IN OUTPUT VARIABLE = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF WEIBAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N,NVALID
 9013 FORMAT('N,NVALID = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,N
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE WEIBAR
      SUBROUTINE WEIAFR(X1,X2,GAMMA,ALOC,SCALE,MINMAX,AFR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE AVERAGE FAILURE RATE
!              (AFR) FUNCTION VALUE FOR THE WEIBULL DISTRIBUTION.
!              THE AFR IS DEFINED AS:
!
!              AFR(X1,X2,SHAPE,LOC,SCALE) = (H(X2,SHAPE,LOC,SCALE) -
!                                            H(X1,LOC,SCALE))/(X2-X1)
!
!              WHERE
!
!              H(X,SHAPE,LOC,SCALE) = H((X-LOC)/SCALE,SHAPE)
!
!              FOR THE WEIBULL (MINIMUM ORDER STATISTIC),
!
!              AFR(X1,X2) = [((X2-LOC)/SCALE)**GAMMA -
!                            ((X1-LOC)/SCALE)**GAMMA]/(X2-X1)
!
!              FOR THE WEIBULL (MAXIMUM ORDER STATISTIC),
!
!              AFR(X1,X2) = [((-X2-LOC)/SCALE)**GAMMA -
!                            ((-X1-LOC)/SCALE)**GAMMA]/(X2-X1)
!
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VALUE AT
!                                WHICH THE AFR FUNCTION IS TO BE
!                                EVALUATED.
!     INPUT  ARGUMENTS--X2     = THE SINGLE PRECISION VALUE AT
!                                WHICH THE AFR FUNCTION IS TO BE
!                                EVALUATED.
!                     --GAMMA  = THE (POSITIVE) SHAPE PARAMETER
!                     --ALOC   = THE LOCATION PARAMETER
!                     --SCALE  = THE (POSITIVE) SCALE PARAMETER
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--AFR    = THE SINGLE PRECISION AVERAGE
!                                FAILURE RATE FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION AVERAGE FAILURE RATE FOR THE
!             WEIBULL DISTRIBUTION.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA AND SCALE SHOULD BE POSITIVE, X2 NOT EQUAL X1.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--TOBIAS AND TRINDALE, "APPLIED RELIABILITY", SECOND
!                 EDITION, CHAPMAN AND HALL/CRC, 1995.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2005.3
!     ORIGINAL VERSION--MARCH     2005.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DLOC
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DG
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DAFR
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      X1MN=MIN(X1,X2)
      X1MX=MAX(X1,X2)
      AFR=0.0
      IF(X1MN.EQ.X1MX)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FIRST AND SECOND ARGUMENTS TO ',   &
               'WEIAFR ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)X1MN
   47   FORMAT('***** THE VALUE OF THE FIRST ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)X1MX
   48   FORMAT('***** THE VALUE OF THE SECOND ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
!CCCC ELSEIF(X1MN.LT.ALOC)THEN
!CCCC   WRITE(ICOUT,4)
!CCC4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO WEIAFR ',
!CCC 1         'IS LESS THAN THE LOCATION')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,46)X1MN
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,49)ALOC
!CC49   FORMAT('***** THE VALUE OF THE LOCATION PARAMETER IS ',
!CCCC1         G15.7,'.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
      ELSEIF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,8)
    8   FORMAT('***** ERROR--THE THIRD ARGUMENT TO WEIAFR ',   &
               '(THE SHAPE) IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(SCALE.LE.0.0)THEN
        WRITE(ICOUT,6)
    6   FORMAT('***** ERROR--THE FIFTH ARGUMENT TO WEIAFR ',   &
               '(THE SCALE) IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SCALE
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(MINMAX.EQ.2)THEN
         IF(X1MX.GT.ALOC)THEN
            AFR=0.0
         ELSEIF(X1MX.EQ.ALOC)THEN
            DX2=DBLE(-X1MN)
            DX1=DBLE(-X1MX)
            DG=DBLE(GAMMA)
            DLOC=DBLE(ALOC)
            DSCALE=DBLE(SCALE)
            DTERM1=((DX2-DLOC)/DSCALE)**DG
            DAFR=DTERM1/(DX2-DX1)
            AFR=REAL(DAFR)
         ELSE
            DX2=DBLE(-X1MN)
            DX1=DBLE(-X1MX)
            DG=DBLE(GAMMA)
            DLOC=DBLE(ALOC)
            DSCALE=DBLE(SCALE)
            DTERM1=((DX2-DLOC)/DSCALE)**DG - ((DX1-DLOC)/DSCALE)**DG
            DAFR=DTERM1/(DX2-DX1)
            AFR=REAL(DAFR)
         ENDIF
!
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         IF(X1MN.LT.ALOC)THEN
            AFR=0.0
         ELSE IF(X1MN.EQ.ALOC)THEN
            DX1=DBLE(X1MN)
            DX2=DBLE(X1MX)
            DG=DBLE(GAMMA)
            DLOC=DBLE(ALOC)
            DSCALE=DBLE(SCALE)
            DTERM1=((DX2-DLOC)/DSCALE)**DG
            DAFR=DTERM1/(DX2-DX1)
            AFR=REAL(DAFR)
         ELSE
            DX1=DBLE(X1MN)
            DX2=DBLE(X1MX)
            DG=DBLE(GAMMA)
            DLOC=DBLE(ALOC)
            DSCALE=DBLE(SCALE)
            DTERM1=((DX2-DLOC)/DSCALE)**DG - ((DX1-DLOC)/DSCALE)**DG
            DAFR=DTERM1/(DX2-DX1)
            AFR=REAL(DAFR)
         ENDIF
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEIAFR--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEIAFR
      SUBROUTINE WEICDF(X,GAMMA,MINMAX,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE WEIBULL
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THERE ARE 2 SUCH WEIBULL FAMILIES--
!                 ONE FOR THE MIN ORDER STAT (THE USUAL) AND
!                 ONE FOR THE MAX ORDER STAT.
!              (SEE SARHAN & GREENBERG, PAGE 69)
!              THE WEIBULL TYPE IS SPECIFIED VIA   MINMAX
!              FOR MINMAX = 1  (FOR THE DEFAULT MINIMUM)
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL POSITIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!              FOR MINMAX = 2 (FOR THE MAXIMUM),
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL NEGATIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = ...
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
!                 --GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 124.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87.7
!     ORIGINAL VERSION--NOVEMBER  1987.
!     UPDATED         --MAY       1992. REWRITTEN--ADD WEIB. FOR MAX
!     UPDATED         --JANUARY   1994. ADD MINMAX ERROR MESSAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0
      IF(GAMMA.LE.0.)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEICDF IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS REWRITTEN      JANUARY 1994
      IF(MINMAX.EQ.2)THEN
         IF(X.GE.0.0)CDF=1.0
         IF(X.LT.0.0)CDF=EXP(-((-X)**GAMMA))
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         IF(X.LE.0.0)CDF=0.0
         IF(X.GT.0.0)CDF=1.0-EXP(-(X**GAMMA))
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEICDF--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEICDF
      SUBROUTINE WEICEN(CYCLE,CENSOR,Y,X,CASE,N,SHAPE,SCALE,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        ISUBRO,IBUGA3,IERROR)
!
!****************************************************
!* FORTRAN PROGRAM USES MAXIMUM LIKELIHOOD TO       *
!* ESTIMATE THE PARAMETERS OF TWO-PARAMETER WEIBULL *
!* DISTRIBUTION. THE PROGRAM CAN BE USED FOR ALL    *
!* CENSORING CASES:                                 *
!*     CASE 1: MULTIPLY CENSORED DATA               *
!*     CASE 2: SINGLY CENSORED DATA                 *
!*     CASE 3: COMPLETE DATA                        *
!*                                                  *
!* PROGRAM INPUT CAN BE INTERACTIVE OR FROM A USER  *
!* SPECIFIED FILE                                   *
!*                                                  *
!* THE INPUT FILE FORMAT IS AS FOLLOWS:             *
!* COL.1: CYCLE TIME                                *
!* COL.2: CENSORED TYPE (1: FAILURE; 0: NON-FAILURE)*
!****************************************************
!
!CCCC CODE FROM:
!CCCC NOVEMBER 2003. ADD SUPPORT FOR HTML/LATEX OUTPUT.
      PARAMETER(NLEV=6)
      INTEGER I,R,S,J,L,CC,CASE
      INTEGER N,CENSOR(*),K,K1
      DOUBLE PRECISION BETA,THETA,CYCLE(*)
      DOUBLE PRECISION X(*),Y(*),PI
      DOUBLE PRECISION T2,T3,T4,ST1,ST2,NUM,NUM1
      DOUBLE PRECISION VARB,VART,COVBT,DEM1,DEM2,DEM3
      DOUBLE PRECISION S1R,S2R,S3R,S4R,S2S,S3S,S4S
      DOUBLE PRECISION DENOM,DENOM1,DENOM2,DENOM3,DENOM4
      DOUBLE PRECISION CON,IB,IT,COV
      DOUBLE PRECISION ALPHA,NORM,DELTA,L1,L2
!
      REAL LOW1(NLEV),LOW2(NLEV),UP1(NLEV),UP2(NLEV),LEVEL(NLEV)
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 ILIKFL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOST.INC'
!
      PARAMETER (MAXROW=25)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*40 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI / 3.141592653589793238462643383279503D0 /
!
!-----START POINT-----------------------------------------------------
!
      LEVEL(1)=0.50
      LEVEL(2)=0.75
      LEVEL(3)=0.90
      LEVEL(4)=0.95
      LEVEL(5)=0.99
      LEVEL(6)=0.999
!
      THETA=0.0D0
      BETA=0.0D0
      T1=0.0D0
      T2=0.0D0
      T3=0.0D0
      T4=0.0D0
      R = 0
      S = 0
      J = 1
      L = 1
      DO 30 I = 1,N
        IF (CENSOR(I) .EQ. 1) THEN
          X(J) = CYCLE(I)
          J = J + 1
          R = R + 1
        ELSE
          Y(L) = CYCLE(I)
          L = L + 1
          S = S + 1
        END IF
30      CONTINUE
!
!       MENON'S ESTIMATE OF BETA AS INITIAL APPROXIMATION OF BETA
!
      ST1 = 0.0D0
      ST2 = 0.0D0
      DO 40 I = 1, R
        ST1 = ST1 +  DLOG(X(I))
        ST2 = ST2 + ( DLOG(X(I)))**2
40    CONTINUE
      S1R = ST1
      ST1 = (ST1**2)/(DBLE(R))
      BETA = (6.0D0 * (ST2 - ST1))/((PI**2)*(DBLE(R - 1.0D0)))
      IF (BETA .EQ. 0.0D0) BETA = .0001D0
      BETA = 1.0D0 / SQRT(BETA)
      CC = 0
      DELTA = 0.0D0
!
! NEWTON-RAPHSON ITERATIVE ESTIMATE OF BETA
!
      MAXIT=500
      DO 100 K = 1,MAXIT
        S2R = 0.0D0
        S3R = 0.0D0
        S4R = 0.0D0
        S2S = 0.0D0
        S3S = 0.0D0
        S4S = 0.0D0
        DO 80 I = 1,R
          S2R = S2R + X(I)**BETA
          S3R = S3R + (X(I)**BETA) *  DLOG(X(I))
          S4R = S4R + (X(I)**BETA) * ( DLOG(X(I))**2)
80      CONTINUE
        DO 85 I = 1,S
           S2S = S2S + Y(I)**BETA
           S3S = S3S + (Y(I)**BETA) *  DLOG(Y(I))
           S4S = S4S + (Y(I)**BETA) * ( DLOG(Y(I))**2)
85      CONTINUE
        NUM1 = (S3R + S3S) / (S2R + S2S)
        NUM = (1.0D0 / BETA) + (S1R / DBLE(R)) - NUM1
        DENOM1 = (S3R + S3S)**2
        DENOM2 = (S2R + S2S) * (S4R + S4S)
        DENOM3 = (S2R + S2S)**2
        DENOM4 = 1.0D0 / BETA**2
        DENOM = DENOM4 - ((DENOM1 - DENOM2) / DENOM3)
        DELTA = NUM / DENOM
        BETA = BETA + DELTA
        K1 = K
!
! TEST FOR CONVERGENCE
!
        IF (ABS(DELTA) .LT. 0.000001D0) THEN
          CC = 1
          GO TO  105
        END IF
100     CONTINUE
!
! INDICATE NON-CONVERGENCE
!
105   CONTINUE
      IF (CC .EQ. 0) THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,108)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
  999 FORMAT(1X)
  108 FORMAT('****** ERROR: WEIBULL MAXIMUM LIKELIHOOD ESTIMATE IS ',   &
             'NOT CONVERGING.')
      ELSE
!
! IF CONVERGENCE HAS OCCURRED CALCULATE THETA & BETA
!
        S2R = 0.0D0
        S3R = 0.0D0
        S4R = 0.0D0
        S2S = 0.0D0
        S3S = 0.0D0
        S4S = 0.0D0
        DO 90 I = 1,R
          S2R = S2R + X(I)**BETA
          S3R = S3R + (X(I)**BETA) *  DLOG(X(I))
          S4R = S4R + (X(I)**BETA) * ( DLOG(X(I))**2)
90      CONTINUE
        DO 95 I = 1,S
          S2S = S2S + Y(I)**BETA
          S3S = S3S + (Y(I)**BETA) *  DLOG(Y(I))
          S4S = S4S + (Y(I)**BETA) * ( DLOG(Y(I))**2)
95      CONTINUE
        T2 = S2R + S2S
        T3 = S3R + S3S
        T4 = S4R + S4S
        THETA = (T2 / DBLE(R))**(1.0D0 / BETA)
      END IF
!
! COMPUTE THE CONFIDENCE INTERVAL OF THE PARAMETERS
! THETA & BETA
!
      DEM1 = THETA**BETA
      DEM2 = THETA**(1.0D0 + BETA)
      DEM3 = THETA**(2.0D0 + BETA)
      L1 =  DLOG(THETA)
      L2 = ( DLOG(THETA))**2
      IB = DBLE(R) / BETA**2 + (L2 * T2 - 2.0D0 * L1 * T3 + T4)   &
      / DEM1
      IT = (BETA * (BETA + 1.0D0) / DEM3 * T2) -   &
      (DBLE(R) * BETA / THETA**2)
      COV = DBLE(R) / THETA - (T2 - BETA * L1 * T2 + BETA * T3) / DEM2
      CON = (IB *IT) - COV**2
      VARB = IT / CON
      VART = IB / CON
      COVBT = COV / CON
      DO 500 I=1,NLEV
        ALPHA = 1.0D0-((1.0D0 - DBLE(LEVEL(I))) / 2.0D0)
        CALL NODPPF(ALPHA,NORM)
        LOW1(I) = REAL(BETA - NORM * SQRT(VARB))
        UP1(I)  = REAL(BETA + NORM * SQRT(VARB))
        LOW2(I) = REAL(THETA - NORM * SQRT(VART))
        UP2(I)  = REAL(THETA + NORM * SQRT(VART))
  500 CONTINUE
!
!               *************************************
!               **   STEP 42--                     **
!               **   WRITE OUT EVERYTHING          **
!               **   FOR WEIBULL MLE ESTIMATE      **
!               *************************************
!
!     PRINT SUMMARY STATISTICS TABLE
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      NUMDIG=7
      IF(IFORSW.EQ.'1')NUMDIG=1
      IF(IFORSW.EQ.'2')NUMDIG=2
      IF(IFORSW.EQ.'3')NUMDIG=3
      IF(IFORSW.EQ.'4')NUMDIG=4
      IF(IFORSW.EQ.'5')NUMDIG=5
      IF(IFORSW.EQ.'6')NUMDIG=6
      IF(IFORSW.EQ.'7')NUMDIG=7
      IF(IFORSW.EQ.'8')NUMDIG=8
      IF(IFORSW.EQ.'9')NUMDIG=9
      IF(IFORSW.EQ.'0')NUMDIG=0
      IF(IFORSW.EQ.'E')NUMDIG=-2
      IF(IFORSW.EQ.'-2')NUMDIG=-2
      IF(IFORSW.EQ.'-3')NUMDIG=-3
      IF(IFORSW.EQ.'-4')NUMDIG=-4
      IF(IFORSW.EQ.'-5')NUMDIG=-5
      IF(IFORSW.EQ.'-6')NUMDIG=-6
      IF(IFORSW.EQ.'-7')NUMDIG=-7
      IF(IFORSW.EQ.'-8')NUMDIG=-8
      IF(IFORSW.EQ.'-9')NUMDIG=-9
!
      ITITLE='Two-Parameter Weibull (Minimum) Parameter Estimation:'
      NCTITL=53
      IF(CASE.EQ.1)THEN
        ITITLZ='Multiply Censored Case'
        NCTITZ=22
      ELSEIF(CASE.EQ.2)THEN
        ITITLZ='Singly Censored Case'
        NCTITZ=20
      ELSEIF(CASE.EQ.3)THEN
        ITITLZ='Full Sample Case'
        NCTITZ=16
      ENDIF
      ITEXT(1)='Summary Statistics:'
      NCTEXT(1)=19
      AVALUE(1)=0.0
      IDIGIT(1)=-99
      ITEXT(2)='Number of Observations:'
      NCTEXT(2)=23
      AVALUE(2)=REAL(N)
      IDIGIT(2)=0
      ICNT=2
      IF(CASE.EQ.3)THEN
        ITEXT(3)='Number of Uncensored Observations:'
        NCTEXT(3)=34
        AVALUE(3)=REAL(R)
        IDIGIT(3)=0
        ITEXT(4)='Number of Censored Observations:'
        NCTEXT(4)=32
        AVALUE(4)=REAL(S)
        IDIGIT(4)=0
        ICNT=4
      ENDIF
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=-1.0
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Maximum Likelihood:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Estimate of Shape (Gamma):'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=BETA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Error of Shape:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=SQRT(VARB)
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Estimate of Scale:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=THETA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Error of Scale:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=SQRT(VART)
      IDIGIT(ICNT)=NUMDIG
      NUMROW=ICNT
      DO 2310 I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ILIKFL='OFF'
      CALL DPDTA8(LOW2,UP2,LOW2,UP2,   &
                  LOW1,UP1,LOW1,UP1,LEVEL,NLEV,   &
                  ICAPSW,ICAPTY,NUMDIG,ILIKFL,   &
                  ISUBRO,IBUGA3,IERROR)
!
 9000 CONTINUE
      SCALE=REAL(THETA)
      SHAPE=REAL(BETA)
      RETURN
      END SUBROUTINE WEICEN
      DOUBLE PRECISION FUNCTION WEIFUN (GHAT,X)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE MAXIMUM LIKELIHOOD
!              ESTIMATE OF GAMMA FOR THE 2-PARAMETER WEIBULL
!              MODEL FOR FULL SAMPLE DATA (NO CENSORING).  THIS
!              FUNCTION FINDS THE ROOT OF THE EQUATION:
!
!                 (1/GHAT) -
!                 SUM[i=1 to n][Y(I)**GHAT*LN(Y(I))]/
!                 SUM[i=1 to n][[Y(I)**GHAT] +
!                 (1/N)*SUM[i=1 to n][LN(Y(I))] = 0
!
!              WITH
!
!                 GHAT     = POINT ESTIMATE OF GAMMA (THIS IS THE
!                            PARAMETER WE ARE ITERATING OVER)
!
!              NOTE THAT THE THIRD TERM DDOES NOT DEPENDE ON GHAT,
!              SO THIS IS A CONSTANT.  FOR EFFICIENCY, SAVE THIS AS
!              A CONSTANT IN A COMMON BLOCK.
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN
!                ENGINEERING", CAMBRIDGE UNIVERSITY PRESS,
!                1999, CHAPTER 17.
!              --JOHNSON, KOTZ, AND BALAKRISNAN, "CONTINUOUS
!                UNIVARIATE DISTRIBUTIONS--VOLUME 1", SECOND EDITION,
!                WILEY, 1994, CHAPTER xx.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION GHAT
      DOUBLE PRECISION X(*)
!
      INTEGER IN
      DOUBLE PRECISION DWEISM
      COMMON/WEICOM/DWEISM,IN
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DG
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DG=GHAT
!
      DTERM1=1.0D0/DG
      DO 100 I=1,IN
        DX1=X(I)
        DSUM1=DSUM1 + (DX1**DG)*DLOG(DX1)
        DSUM2=DSUM2 + DX1**DG
  100 CONTINUE
      DTERM2=DSUM1/DSUM2
!
      WEIFUN=DTERM1 - DTERM2 + DWEISM
!
      RETURN
      END FUNCTION WEIFUN 
      DOUBLE PRECISION FUNCTION WEIFU2 (DA,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDECE INTERVAL FOR THE 2-PARAMETER WEIBULL
!              MODEL (FULL SAMPLE).  THIS FUNCTION FINDS THE ROOT
!              OF THE EQUATION:
!
!                 2*LL(ALPHA,GAMMA) - 2*LL(S(a),,a) - CHSPPF(alpha,1)
!
!              WITH
!
!                 LL(ALPHA,GAMMA) = N*LN(GAMMA) - N*GAMMA*LN(ALPHA) +
!                          (GAMMA-1)*SUM[i=1 to n][LN(X(i))] -
!                          ALPHA**(-GAMMA)*SUM[i=1 to n][(X(i)**GAMA]
!                 ALPHA    = POINT ESTIMATE OF SCALE PARAMETER
!                 GAMMA    = POINT ESTIMATE OF SHAPE PARAMETER
!                 A        = PARAMETER WE ARE FINDING ROOT FOR
!                 K        = CHSPPF(ALPHA,1) (HERE, ALPHA IS THE
!                            SIGNIFICANCE LEVEL, NOT THE SCALE PARAMETER)
!
!              NOTE THAT QUANTITIES THAT DO NOT DEPEND ON A ARE
!              COMPUTED ONCE IN DPMLW1 AND PASSED VIA COMMON BLOCK.
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER2 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 12.4).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      COMMON/WEICO2/DK,DTERM1,DTERM2,N
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DG
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DG=DA
!
      DSUM1=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DX(I)**DG
  100 CONTINUE
      DSCALE=(DSUM1/DN)**(1.0D0/DG)
!
      DTERM3=DN*DLOG(DG) - DN*DG*DLOG(DSCALE)
      DTERM4=(DG-1.0D0)*DTERM2
      DTERM5=DSCALE**(-DG)*DSUM1
!
      WEIFU2=DTERM1 - 2.0D0*(DTERM3 + DTERM4 - DTERM5) - DK
!
      RETURN
      END FUNCTION WEIFU2 
      DOUBLE PRECISION FUNCTION WEIFU3 (DB,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDENCE INTERVAL FOR THE SCALE PARAMETER OF A
!              2-PARAMETER WEIBULL MODEL (FULL SAMPLE).  THIS FUNCTION
!              FINDS THE ROOT OF THE EQUATION:
!
!                 2*LL(ALPHA,GAMMA) - 2*LL(b,I(b)) - CHSPPF(alpha,1)
!
!              WITH
!
!                 LL(ALPHA,GAMMA) = N*LN(GAMMA) - N*GAMMA*LN(ALPHA) +
!                          (GAMMA-1)*SUM[i=1 to n][LN(X(i))] -
!                          ALPHA**(-GAMMA)*SUM[i=1 to n][(X(i)**GAMA]
!                 ALPHA    = POINT ESTIMATE OF SCALE PARAMETER
!                 GAMMA    = POINT ESTIMATE OF SHAPE PARAMETER
!                 B        = PARAMETER (SCALE) WE ARE FINDING ROOT FOR
!                 K        = CHSPPF(ALPHA,1) (HERE, ALPHA IS THE
!                            SIGNIFICANCE LEVEL, NOT THE SCALE
!                            PARAMETER)
!
!              NOTE THAT QUANTITIES THAT DO NOT DEPEND ON B ARE
!              COMPUTED ONCE IN DPMLW1 AND PASSED VIA COMMON BLOCK.
!
!              GIVEN A VALUE FOR THE SCALE PARAMETER (DB), WE NEED
!              TO CALL A ROOT FINDING ROUTINE TO DETERMINE THE VALUE
!              OF THE SHAPE PARAMETER (A).
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER2 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 17.7).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DB
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTERM7
      DOUBLE PRECISION DGAMMA
      COMMON/WEICO3/DK,DTERM6,DTERM7,DGAMMA,N
!
      DOUBLE PRECISION DBTEMP
      COMMON/WEICO4/DBTEMP,N2
!
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
      DOUBLE PRECISION XSTRT
      DOUBLE PRECISION DA
      DOUBLE PRECISION DG
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
!
      DOUBLE PRECISION WEIFU4
      EXTERNAL WEIFU4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: GIVEN VALUE OF SCALE PARAMETER (DB), NEED TO COMPUTE
!          THE SHAPE PARAMETER (WHICH IN TURN INVOLVES FINDING A
!          ROOT).
!
      N2=N
      DBTEMP=DB
      AE=1.D-7
      RE=1.D-7
      XSTRT=DGAMMA
      XLOW=XSTRT/5.0D0
      XUP=XSTRT*5.0D0
      CALL DFZER3(WEIFU4,XLOW,XUP,XSTRT,RE,AE,IFLAG,DX)
      DA=XLOW
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DG=DA
      DSCALE=DB
!
      DSUM1=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DX(I)**DG
  100 CONTINUE
!
      DTERM3=DN*DLOG(DG) - DN*DG*DLOG(DSCALE)
      DTERM4=(DG-1.0D0)*DTERM7
      DTERM5=DSCALE**(-DG)*DSUM1
!
      WEIFU3=DTERM6 - 2.0D0*(DTERM3 + DTERM4 - DTERM5) - DK
!
      RETURN
      END FUNCTION WEIFU3 
      DOUBLE PRECISION FUNCTION WEIFU4 (DA,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDENCE INTERVAL FOR THE SCALE PARAMETER OF
!              THE 2-PARAMETER WEIBULL MODEL (FULL SAMPLE).
!              SPECIFICALLY, IT IS USED TO DETERMINE AN ESTIMATE
!              OF THE SHAPE PARAMETER GIVEN A VALUE OF THE SCALE
!              PARAMETER.  IT FINDS THE ROOT OF THE FOLLOWING
!              EQUATION:
!
!                 (N/A) - N*LOG(B) + SUM[LOG(X)] -
!                       SUM[(X/B)**A*LOG)X/B)]
!
!              WITH A DENOTING THE SHAPE PARAMETER, B THE SCALE
!              PARAMETER, AND THE ROOT IS WITH RESPECT TO A.
!
!              CALLED BY DFZER3 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER3 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 17.7).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DB
      COMMON/WEICO4/DB,N
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DTERM1=(DN/DA) - DN*DLOG(DB)
!CCCC print *,'weifu4: dn,da,db,dterm1=',dn,da,db,dterm1
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DLOG(DX(I))
        DSUM2=DSUM2 + ((DX(I)/DB)**DA)*DLOG(DX(I)/DB)
  100 CONTINUE
!
      WEIFU4=DTERM1 + DSUM1 - DSUM2
!
      RETURN
      END FUNCTION WEIFU4 
      DOUBLE PRECISION FUNCTION WEIFU5 (DA,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDECE INTERVAL FOR THE 2-PARAMETER WEIBULL
!              MODEL (FULL SAMPLE).  THIS FUNCTION FINDS THE ROOT
!              OF THE EQUATION:
!
!                 2*LL(ALPHA,GAMMA) - 2*LL(S(a),,a) - CHSPPF(alpha,1)
!
!              WITH
!
!                 LL(ALPHA,GAMMA) = N*LN(GAMMA) - N*GAMMA*LN(ALPHA) +
!                          (GAMMA-1)*SUM[i=1 to n][LN(X(i))] -
!                          ALPHA**(-GAMMA)*SUM[i=1 to n][(X(i)**GAMA]
!                 ALPHA    = POINT ESTIMATE OF SCALE PARAMETER
!                 GAMMA    = POINT ESTIMATE OF SHAPE PARAMETER
!                 A        = PARAMETER WE ARE FINDING ROOT FOR
!                 K        = CHSPPF(ALPHA,1) (HERE, ALPHA IS THE
!                            SIGNIFICANCE LEVEL, NOT THE SCALE PARAMETER)
!
!              NOTE THAT QUANTITIES THAT DO NOT DEPEND ON A ARE
!              COMPUTED ONCE IN DPMLW1 AND PASSED VIA COMMON BLOCK.
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER2 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y X
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 12.4).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      COMMON/WEICO5/DK,DTERM1,DTERM2,N,IR
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DR
      DOUBLE PRECISION DG
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DR=DBLE(IR)
      DG=DA
!
      DSUM1=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DX(I)**DG
  100 CONTINUE
      DSCALE=(DSUM1/DR)**(1.0D0/DG)
!
      DTERM3=DR*DLOG(DG) - DR*DG*DLOG(DSCALE)
      DTERM4=(DG-1.0D0)*DTERM2
      DTERM5=DSCALE**(-DG)*DSUM1
!
      WEIFU5=DTERM1 - 2.0D0*(DTERM3 + DTERM4 - DTERM5) - DK
!
      RETURN
      END FUNCTION WEIFU5 
      DOUBLE PRECISION FUNCTION WEIFU6 (DB,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDENCE INTERVAL FOR THE SCALE PARAMETER OF A
!              2-PARAMETER WEIBULL MODEL (TIME CENSORED).  THIS
!              FUNCTION FINDS THE ROOT OF THE EQUATION:
!
!                 2*LL(ALPHA,GAMMA) - 2*LL(b,I(b)) - CHSPPF(alpha,1)
!
!              WITH
!
!                 LL(ALPHA,GAMMA) = N*LN(GAMMA) - N*GAMMA*LN(ALPHA) +
!                          (GAMMA-1)*SUM[i=1 to n][LN(X(i))] -
!                          ALPHA**(-GAMMA)*SUM[i=1 to n][(X(i)**GAMA]
!                 ALPHA    = POINT ESTIMATE OF SCALE PARAMETER
!                 GAMMA    = POINT ESTIMATE OF SHAPE PARAMETER
!                 B        = PARAMETER (SCALE) WE ARE FINDING ROOT FOR
!                 K        = CHSPPF(ALPHA,1) (HERE, ALPHA IS THE
!                            SIGNIFICANCE LEVEL, NOT THE SCALE
!                            PARAMETER)
!
!              NOTE THAT QUANTITIES THAT DO NOT DEPEND ON B ARE
!              COMPUTED ONCE IN DPMLW2 AND PASSED VIA COMMON BLOCK.
!
!              GIVEN A VALUE FOR THE SCALE PARAMETER (DB), WE NEED
!              TO CALL A ROOT FINDING ROUTINE TO DETERMINE THE VALUE
!              OF THE SHAPE PARAMETER (A).
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER2 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y X
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 17.7).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DB
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTERM7
      DOUBLE PRECISION DGAMMA
      COMMON/WEICO6/DK,DTERM6,DTERM7,DGAMMA,N,IR
!
      DOUBLE PRECISION DBTEMP
      COMMON/WEICO7/DBTEMP,N2,IR2
!
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
      DOUBLE PRECISION XSTRT
      DOUBLE PRECISION DA
      DOUBLE PRECISION DG
      DOUBLE PRECISION DN
      DOUBLE PRECISION DR
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
!
      DOUBLE PRECISION WEIFU7
      EXTERNAL WEIFU7
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: GIVEN VALUE OF SCALE PARAMETER (DB), NEED TO COMPUTE
!          THE SHAPE PARAMETER (WHICH IN TURN INVOLVES FINDING A
!          ROOT).
                                                                                                                                  
      N2=N
      IR2=IR
      DBTEMP=DB
      AE=1.D-7
      RE=1.D-7
      XSTRT=DGAMMA
      XLOW=XSTRT/5.0D0
      XUP=XSTRT*5.0D0
      CALL DFZER3(WEIFU7,XLOW,XUP,XSTRT,RE,AE,IFLAG,DX)
      DA=XLOW
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DR=DBLE(IR)
      DG=DA
      DSCALE=DB
!
      DSUM1=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DX(I)**DG
  100 CONTINUE
!
      DTERM3=DR*DLOG(DG) - DR*DG*DLOG(DSCALE)
      DTERM4=(DG-1.0D0)*DTERM7
      DTERM5=DSCALE**(-DG)*DSUM1
!
      WEIFU6=DTERM6 - 2.0D0*(DTERM3 + DTERM4 - DTERM5) - DK
!
      RETURN
      END FUNCTION WEIFU6 
      DOUBLE PRECISION FUNCTION WEIFU7 (DA,DX)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE LIKELIHOOD RATIO
!              BASED CONFIDENCE INTERVAL FOR THE SCALE PARAMETER OF
!              THE 2-PARAMETER WEIBULL MODEL (FULL SAMPLE).
!              SPECIFICALLY, IT IS USED TO DETERMINE AN ESTIMATE
!              OF THE SHAPE PARAMETER GIVEN A VALUE OF THE SCALE
!              PARAMETER.  IT FINDS THE ROOT OF THE FOLLOWING
!              EQUATION:
!
!                 (N/A) - N*LOG(B) + SUM[LOG(X)] -
!                       SUM[(X/B)**A*LOG)X/B)]
!
!              WITH A DENOTING THE SHAPE PARAMETER, B THE SCALE
!              PARAMETER, AND THE ROOT IS WITH RESPECT TO A.
!
!              FOR CENSORED SAMPLES, RELACE N WITH R (= NUMBER OF
!              FAILURE TIMES).
!
!              CALLED BY DFZER3 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.  DFZER3 IS MODIFIED VERSION OF DFZERO THAT
!              PASSES ALONG THE DATA ARRAY.
!
!     EXAMPLE--WEIBULL MAXIMUM LIKELIHOOD Y X
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17 (SEE
!                EXAMPLE 17.7).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/11
!     ORIGINAL VERSION--NOVEMBER   2004.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DX(*)
!
      DOUBLE PRECISION DB
      COMMON/WEICO7/DB,N,IR
!
      DOUBLE PRECISION DR
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      DN=DBLE(N)
      DR=DBLE(IR)
      DTERM1=(DR/DA) - DR*DLOG(DB)
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 100 I=1,N
        DSUM1=DSUM1 + DLOG(DX(I))
        DSUM2=DSUM2 + ((DX(I)/DB)**DA)*DLOG(DX(I)/DB)
  100 CONTINUE
!
      WEIFU7=DTERM1 + DSUM1 - DSUM2
!
      RETURN
      END FUNCTION WEIFU7 
      DOUBLE PRECISION FUNCTION WEIFU8 (BETAHT,X)
!
!     PURPOSE--THIS ROUTINE IS USED TO FIND A COMMON SHAPE PARAMETER
!              FOR A 2-PARAMETER WEIBULL DISTRIBUTION FROM MULTIPLE
!              SAMPLES.  IT IS ASSUMED THAT A TEST HAS BEEN PERFORMED
!              TO ASSESS THE VALIDITY OF THE ASSUMPTION THAT THE GROUPS
!              HAVE EQUAL SHAPE PARAMETERS.
!
!              THIS ROUTINE CAN BE USED FOR EITHER THE FULL SAMPLE CASE
!              OR THE CENSORED CASE.
!
!              GIVEN K GROUPS, SOLVE THE FOLLOWING EQUATION FOR Betahat:
!
!                 SUM[i=1 to k][r(i)/Betahat] +
!                 SUM[i=1 to k][SUM[j=1 to r(i)][LOG(X(ij))]] -
!                 SUM[i=1 to k][r(i)*SUM[j=1 to n(i)]
!                              [X(ij)**Betahat*LOG(X(ij))]]/
!                              SUM[j=1 to n(i)][X(ij)**Betahat] = 0
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.
!     REFERENCES--MCCOOL (2012), "USING THE WEIBULL DISTRIBUTION:
!                 RELIABILITY, MODELING, AND INFERENCE", WILEY, PP.
!                 236-238.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/04
!     ORIGINAL VERSION--APRIL      2014.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION BETAHT
      DOUBLE PRECISION X(*)
!
      INTEGER IN
      INTEGER NDIST
      INTEGER NSIZE(50)
      INTEGER NR(50)
      REAL    SCALE(50)
      COMMON/CWSHAP/IN,NDIST,NSIZE,NR,SCALE,BETAMN,BETAMX,BETAMD
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DHOLD
      DOUBLE PRECISION DNR
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZD.INC'
!
      DOUBLE PRECISION DTAG(MAXOBV)
      DOUBLE PRECISION DCEN(MAXOBV)
      DOUBLE PRECISION XIDTEM(MAXOBV)
      EQUIVALENCE(DGARBG(IDGAR2),DTAG(1))
      EQUIVALENCE(DGARBG(IDGAR3),DCEN(1))
      EQUIVALENCE(DGARBG(IDGAR4),XIDTEM(1))
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     NOTE THAT THE RESPONSE VALUES WILL BE PASSED IN THE ARGUMENT
!     VARIABLE X.  HOWEVER, THE GROUP-ID AND CENSORING VARIABLES
!     WILL BE PASSED VIA THE DPCOZD.INC COMMON BLOCK.
!
      N=IN
      NGROUP=NDIST
!
!     TERM 1: SUM[i=1 to k][r(i)/Betahat]
!
      DTERM1=0.0D0
      DO 100 I=1,NGROUP
        DNR=DBLE(NR(I))
        DTERM1=DTERM1 + (DNR/BETAHT)
  100 CONTINUE
!
!     TERM 2: SUM[i=1 to k][SUM[j=1 to r(i)][LOG(X(ij))]]
!
!     TERM 3: SUM[i=1 to k][r(i)*SUM[j=1 to n(i)]
!             [X(ij)**Betahat*LOG(X(ij))]]/
!             SUM[j=1 to n(i)][X(ij)**Betahat]
!
      DTERM2=0.0D0
      DTERM3=0.0D0
      DO 210 I=1,NGROUP
        DHOLD=XIDTEM(I)
        DSUM1=0.0D0
        DSUM2=0.0D0
        DNR=DBLE(NR(I))
        DO 220 J=1,N
          IF(DTAG(J).EQ.DHOLD)THEN
            IF(DCEN(J).EQ.1.0D0)THEN
              DTERM2=DTERM2 + DLOG(X(J))
            ENDIF
            DSUM1=DSUM1 + X(J)**BETAHT*DLOG(X(J))
            DSUM2=DSUM2 + X(J)**BETAHT
          ENDIF
  220   CONTINUE
        DTERM3=DTERM3 + (DNR*DSUM1/DSUM2)
  210 CONTINUE
!
      WEIFU8=DTERM1 + DTERM2 - DTERM3
!
      RETURN
      END FUNCTION WEIFU8 
      SUBROUTINE WEIGHH(IT,I1,I2,XS,N,XMAXHF,   &
      WH,ISUBRO,IBUGA3,IERROR)
!     PURPOSE--DETERMINE THE HORIZONTAL WEIGHTS
!              WH(I1) THROUGH WH(I2).
!              THESE WILL BE THE WEIGHTS FOR THE NN = I2-I1+1 POINTS
!              OF THE NEIGHBORHOOD SURROUNDING POINT XS(IT).
!     NOTE--XS(IT) IS CONSIDERED A NEIGHBOR OF ITSELF.
!     NOTE--WEIGHT FUNCTION IS TRICUBE.
!     REFERENCE--CHAMBERS, ET AL.  GRAPHICAL METHODS FOR DATA ANALYSIS.
!                WADSWORTH, 1983, PAGES 94-98, 121-122.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY   1988
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION XS(*)
      DIMENSION WH(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIG'
      ISUBN2='HH  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'IGHH')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF WEIGHH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISUBRO,IBUGA3,IERROR
   52 FORMAT('ISUBRO,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IT,I1,I2,N
   53 FORMAT('IT,I1,I2,N = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)XMAXHF
   54 FORMAT('XMAXHF = ',E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.GE.1)GO TO 119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN WEIGHH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT FULL SAMPLE SIZE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      FOR WHICH LOWESS HORIZONTAL WEIGHTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      ARE TO BE COMPUTED, MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)N
  116 FORMAT('      THE FULL SAMPLE SIZE N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  119 CONTINUE
!
      IF(IT.GE.1)GO TO 129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** ERROR IN WEIGHH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)
  122 FORMAT('      THE INPUT TARGET OBSERVATION INDEX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,123)
  123 FORMAT('      FOR WHICH A LOWESS IS TO BE CARRIED OUT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,124)N
  124 FORMAT('      MUST BE BETWEEN 1 AND ',I8,' (INCLUSIVE).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,125)
  125 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,126)IT
  126 FORMAT('      THE TARGET OBSERVATION INDEX IT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  129 CONTINUE
!
      IF(I1.LE.I2)GO TO 139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)
  131 FORMAT('***** ERROR IN WEIGHH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,132)
  132 FORMAT('      THE  NEIGHBORHOOD LOWER INDEX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,133)
  133 FORMAT('      FOR WHICH A LOWESS IS TO BE CARRIED OUT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,134)
  134 FORMAT('      MUST NOT EXCEED THE NEIGHBORHOOD UPPER INDEX.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,135)
  135 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)IT
  136 FORMAT('      THE NEIGHBORHOOD INDICES I1 AND I2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  139 CONTINUE
!
!               ***********************************************
!               **  STEP 11--                                **
!               **  COMPUTE THE  HORIZONTAL WEIGHTS FOR THE  **
!               **  NEIGHBORHOOD SUURROUNDING XS(IT)         **
!               ***********************************************
!
      IF(XMAXHF.LE.0.0)GO TO 1190
!
      DO 1100 I=I1,I2
      U=(XS(I)-XS(IT))/XMAXHF
      U2=ABS(U)
      WH(I)=(1.0-U2**3)**3
 1100 CONTINUE
!
 1190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'IGHH')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF WEIGHH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ISUBRO,IBUGA3,IERROR
 9012 FORMAT('ISUBRO,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IT,I1,I2,N
 9013 FORMAT('IT,I1,I2,N = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)XMAXHF
 9014 FORMAT('XMAXHF = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9021 I=I1,I2
      WRITE(ICOUT,9022)I,XS(I),WH(I)
 9022 FORMAT('I,XS(I),WH(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE WEIGHH
      SUBROUTINE WEIGHV(RES,N,XTEMP1,XTEMP2,MAXNXT,   &
      WV,ISUBRO,IBUGA3,IERROR)
!     PURPOSE--DETERMINE THE N VERTICAL (ROBUST) WEIGHTS
!              WV(1) THROUGH WV(N)
!              BASED ON THE NATURE OF THE RESIDUALS IN RES(.).
!     NOTE--WEIGHT FUNCTION IS BIWEIGHT.
!     NOTE--IF ALL INPUT RESIDUALS ARE ZERO, THIS SUBROUTINE
!           WILL OUTPUT ALL WEIGHTS AS UNITY.
!     REFERENCE--CHAMBERS, ET AL.  GRAPHICAL METHODS FOR DATA ANALYSIS.
!                WADSWORTH, 11013, PAGES 98-101, 122-123.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY   1988
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION RES(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION WV(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIG'
      ISUBN2='HV  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'IGHV')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF WEIGHV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISUBRO,IBUGA3,IERROR
   52 FORMAT('ISUBRO,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N.LE.0)GO TO 63
      DO 61 I=1,N
      WRITE(ICOUT,62)I,RES(I)
   62 FORMAT('I,RES(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   61 CONTINUE
   63 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.GE.1)GO TO 119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN WEIGHV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT FULL SAMPLE SIZE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      FOR WHICH LOWESS VERTICAL (ROBUST) WEIGHTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      ARE TO BE COMPUTED, MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)N
  116 FORMAT('      THE FULL SAMPLE SIZE N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  119 CONTINUE
!
!               ***********************************************
!               **  STEP 11--                                **
!               **  COMPUTE THE  VERTICAL (ROBUST) WEIGHTS   **
!               **  FOR THE FULL DATA SET--ALL N POINTS      **
!               ***********************************************
!
      DO 1100 I=1,N
      XTEMP1(I)=ABS(RES(I))
 1100 CONTINUE
!
      IWRITE='OFF'
      CALL MEDIAN(XTEMP1,N,IWRITE,XTEMP2,MAXNXT,AMEDAR,IBUGA3,IERROR)
!
      IF(AMEDAR.EQ.0.0)GO TO 1110
      GO TO 1120
!
 1110 CONTINUE
      CONST=(-999.0)
      DO 1111 I=1,N
      WV(I)=1.0
 1111 CONTINUE
      GO TO 1190
!
 1120 CONTINUE
      CONST=6.0*AMEDAR
      DO 1121 I=1,N
      U=RES(I)/CONST
      WV(I)=0.0
      IF(-1.0.LE.U.AND.U.LE.1.0)WV(I)=(1.0-U**2)**2
 1121 CONTINUE
      GO TO 1190
!
 1190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'IGHV')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF WEIGHV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ISUBRO,IBUGA3,IERROR
 9012 FORMAT('ISUBRO,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)AMEDAR
 9014 FORMAT('AMEDAR = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(N.LE.0)GO TO 9023
      DO 9021 I=1,N
      WRITE(ICOUT,9022)I,RES(I),WV(I)
 9022 FORMAT('I,RES(I),WV(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9023 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE WEIGHV
      SUBROUTINE WEICHA(X,GAMMA,MINMAX,HAZ)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE HAZARD
!              FUNCTION VALUE FOR THE WEIBULL
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THERE ARE 2 SUCH WEIBULL FAMILIES--
!                 ONE FOR THE MIN ORDER STAT (THE USUAL) AND
!                 ONE FOR THE MAX ORDER STAT.
!              (SEE SARHAN & GREENBERG, PAGE 69)
!              THE WEIBULL TYPE IS SPECIFIED VIA   MINMAX
!              FOR MINMAX = 1  (FOR THE DEFAULT MINIMUM)
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL POSITIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!              FOR MINMAX = 2 (FOR THE MAXIMUM),
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL NEGATIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = ...
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE HAZARD
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--HAZ    = THE SINGLE PRECISION
!                                CUMULATIVE HAZARD FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE HAZARD
!             FUNCTION VALUE PDF FOR THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 124.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.4
!     ORIGINAL VERSION--APRIL     1998.
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      HAZ=0.0
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEICHA IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(MINMAX.EQ.2)THEN
         IF(X.GT.0.0)THEN
            HAZ=0.0
         ELSE IF(X.EQ.0.0)THEN
            HAZ=0.0
         ELSE
            HAZ=(-X)**GAMMA
         ENDIF
!
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         IF(X.LT.0.0)THEN
            HAZ=0.0
         ELSE IF(X.EQ.0.0)THEN
            HAZ=0.0
         ELSE
            HAZ=X**GAMMA
         ENDIF
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEICHA--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEICHA
      SUBROUTINE WEIHAZ(X,GAMMA,MINMAX,HAZ)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE HAZARD
!              FUNCTION VALUE FOR THE WEIBULL
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THERE ARE 2 SUCH WEIBULL FAMILIES--
!                 ONE FOR THE MIN ORDER STAT (THE USUAL) AND
!                 ONE FOR THE MAX ORDER STAT.
!              (SEE SARHAN & GREENBERG, PAGE 69)
!              THE WEIBULL TYPE IS SPECIFIED VIA   MINMAX
!              FOR MINMAX = 1  (FOR THE DEFAULT MINIMUM)
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL POSITIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!              FOR MINMAX = 2 (FOR THE MAXIMUM),
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL NEGATIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = ...
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE HAZARD
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--HAZ    = THE SINGLE PRECISION
!                                HAZARD FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION HAZARD
!             FUNCTION VALUE PDF FOR THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 124.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.4
!     ORIGINAL VERSION--APRIL     1998.
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      HAZ=0.0
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEIHAZ IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(MINMAX.EQ.2)THEN
         IF(X.GT.0.0)THEN
            HAZ=0.0
         ELSE IF(X.EQ.0.0)THEN
            IF(GAMMA.EQ.1.0)THEN
              HAZ=1.0
            ELSEIF(GAMMA.LT.1.0)THEN
              HAZ=0.0
              WRITE(ICOUT,1700)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(GAMMA.GT.1.0)THEN
              HAZ=0.0
            ENDIF
         ELSE
            HAZ=GAMMA*((-X)**(GAMMA-1.0))
         ENDIF
!
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         IF(X.LT.0.0)THEN
            HAZ=0.0
         ELSE IF(X.EQ.0.0)THEN
            IF(GAMMA.EQ.1.0)THEN
              HAZ=1.0
            ELSEIF(GAMMA.LT.1.0)THEN
              HAZ=0.0
              WRITE(ICOUT,1700)
 1700    FORMAT('*****WARNING IN WEIHAZ--FOR GAMMA < 1 AND X = 0 ',   &
                'HAZARD VALUE IS UNDEFINED (SET TO 0).')
         CALL DPWRST('XXX','BUG ')
            ELSEIF(GAMMA.GT.1.0)THEN
              HAZ=0.0
            ENDIF
         ELSE
            HAZ=GAMMA*(X**(GAMMA-1.0))
         ENDIF
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEIHAZ--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEIHAZ
      SUBROUTINE WEILI1(Y,N,ICASPL,MINMAX,ALOC,SCALE,SHAPE,   &
      ALIK,AIC,AICC,BIC,   &
      ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE LIKELIHOOD FUNCTION FOR
!              THE WEIBULL DISTRIBUTION.  THIS IS FOR THE RAW DATA
!              CASE (I.E., NO GROUPING AND NO CENSORING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 17.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/4
!     ORIGINAL VERSION--APRIL     2010.
!     UPDATED         --JUNE      2010. DISTINGUISH BETWEEN
!                                       2-PARAMETER WEIBULL AND
!                                       3-PARAMETER WEIBULL
!     UPDATED         --SEPTEMBER 2014. CORRECTIONS FOR "MAXIMUM"
!                                       CASE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DS
      DOUBLE PRECISION DU
      DOUBLE PRECISION DG
      DOUBLE PRECISION DN
      DOUBLE PRECISION DNP
      DOUBLE PRECISION DLIK
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM3
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIL'
      ISUBN2='I1  '
      IERROR='NO'
!
      ALIK=-99.0
      AIC=-99.0
      AICC=-99.0
      BIC=-99.0
!
      IF(ALOC.EQ.CPUMIN)THEN
        ALOCT=0.0
      ELSE
        ALOCT=ALOC
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEILI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N,ALOC,SCALE
   55   FORMAT('N,ALOC,SCALE = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  COMPUTE LIKELIHOOD FUNCTION         **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
      IF(ICASPL.EQ.'WEIB')ALOC=0.0
!
!     LOG-LIKELIHOOD FUNCTION IS:
!
!     N*(LOG(SHAPE) - SHAPE*LOG(SCALE)) +
!     (SHAPE-1)*SUM[i=1 to n][LOG(X(i) - LOC] -
!     SUM[i=1 to n][((X(i) - LOC)/SCALE)**SHAPE]
!
      DN=DBLE(N)
      DS=DBLE(SCALE)
      DU=DBLE(ALOC)
      DG=DBLE(SHAPE)
      DTERM1=DN*(DLOG(DG) - DG*DLOG(DS))
      DSUM1=0.0D0
      DSUM2=0.0D0
!
      IF(MINMAX.EQ.2)THEN
        DO 1010 I=1,N
          DX=DBLE(Y(I))
          DSUM1=DSUM1 + DLOG(DU - DX)
          DSUM2=DSUM2 + ((DU-DX)/DS)**DG
 1010   CONTINUE
      ELSE
        DO 1020 I=1,N
          DX=DBLE(Y(I))
          DSUM1=DSUM1 + DLOG(DX - DU)
          DSUM2=DSUM2 + ((DX-DU)/DS)**DG
 1020   CONTINUE
      ENDIF
!
      DLIK=DTERM1 + (DG-1.0D0)*DSUM1 - DSUM2
      ALIK=REAL(DLIK)
      DNP=2.0D0
      IF(ICASPL.EQ.'3WEI')DNP=3.0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
!
!     FOR MAXIMUM CASE, RESTORE THE DATA
!
      IF(MINMAX.EQ.2)THEN
        DO 1129 I=1,N
          Y(I)=-Y(I)
 1129   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ILI1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEILI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)DSUM1,DSUM2,DTERM1,DTERM3
 9013   FORMAT('DSUM1,DSUM2,DTERM1,DTERM3 = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)ALIK,AIC,AICC,BIC
 9014   FORMAT('ALIK,AIC,AICC,BIC = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEILI1
      SUBROUTINE WEIML1(Y,N,IWEIBC,IWEIFL,MINMAX,   &
                        TEMP1,DTEMP1,   &
                        XMEAN,XSD,XVAR,XMIN,XMAX,   &
                        ZMEAN,ZSD,   &
                        SCALML,SCALSE,SHAPML,SHAPSE,   &
                        SHAPBC,SHABSE,COVSE,COVBSE,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE 2-PARAMETER WEIBULL DISTRIBUTION FOR THE RAW DATA
!              CASE (I.E., NO CENSORING AND NO GROUPING).  THIS ROUTINE
!              RETURNS ONLY THE POINT ESTIMATES (CONFIDENCE INTERVALS
!              WILL BE COMPUTED IN A SEPARATE ROUTINE).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLW1 WILL GENERATE THE OUTPUT
!              FOR THE WEIBULL MLE COMMAND).
!
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE UNIVERSITY PRESS, 1999, CHAPTER 12.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/1
!     ORIGINAL VERSION--JANUARY   2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLE1)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DOUBLE PRECISION DTEMP1(*)
!
      CHARACTER*4 IWEIBC
      CHARACTER*4 IWEIFL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION DXSTRT
      DOUBLE PRECISION DAE
      DOUBLE PRECISION DRE
      DOUBLE PRECISION DXLOW
      DOUBLE PRECISION DXUP
      DOUBLE PRECISION XLOWSV
      DOUBLE PRECISION XUPSV
      DOUBLE PRECISION DSUM
!
      DOUBLE PRECISION WEIFUN
      EXTERNAL WEIFUN
      INTEGER IN
      DOUBLE PRECISION DWEISM
      COMMON/WEICOM/DWEISM,IN
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='L1  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,IWEIBC,IWEIFL
   52   FORMAT('IBUGA3,ISUBRO,ICASE,IWEIBC,IWEIFL = ',4(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR WEIBULL MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      IDIST='WEIBULL'
      IF(IWEIFL.EQ.'IWEI')IDIST='INVERTED WEIBULL'
!
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IWEIFL.EQ.'IWEI')THEN
        DO 1118 I=1,N
          Y(I)=1.0/Y(I)
 1118   CONTINUE
        CALL MEAN(Y,N,IWRITE,ZMEAN,IBUGA3,IERROR)
        CALL SD(Y,N,IWRITE,ZSD,IBUGA3,IERROR)
      ELSEIF(MINMAX.EQ.2)THEN
        DO 1119 I=1,N
          Y(I)=-Y(I)
 1119   CONTINUE
      ENDIF
!
      IF(MINMAX.NE.2 .OR. IWEIFL.EQ.'IWEI')THEN
        DO 1125 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1121)IDIST(1:16)
 1121       FORMAT('***** ERROR FROM ',A16,' MAXIMUM LIKELIHOOD--')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1122)
 1122       FORMAT('      NON-POSITIVE VALUE ENCOUNTERED.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1123)I,Y(I)
 1123       FORMAT('      ROW ',I8,' HAS THE VALUE = ',E15.7)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSE
            TEMP1(I)=LOG(Y(I))
          ENDIF
 1125   CONTINUE
      ELSE
        DO 1135 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1131)IDIST(1:16)
 1131       FORMAT('***** ERROR FROM ',A16,' MAXIMUM LIKELIHOOD--')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1132)
 1132       FORMAT('      NON-NEGATIVE VALUE ENCOUNTERED.')
            CALL DPWRST('XXX','WRIT')
            ATEMP=-Y(I)
            WRITE(ICOUT,1133)I,ATEMP
 1133       FORMAT('      ROW ',I8,' HAS THE VALUE = ',E15.7)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSE
            TEMP1(I)=LOG(Y(I))
          ENDIF
 1135   CONTINUE
      ENDIF
!
      SHAPML=CPUMIN
      SHAPBC=CPUMIN
      SHAPSE=CPUMIN
      SHABSE=CPUMIN
      SCALML=CPUMIN
      SCALSE=CPUMIN
      COVSE=CPUMIN
      COVBSE=CPUMIN
!
!     FOR THE SHAPE PARAMETER, SOLVE THE EQUATION:
!
!       (1/GHAT) -
!       SUM[i=1 to n][Y(I)**GHAT*LN(Y(I))]/SUM[i=1 to n][[Y(I)**GHAT] +
!       (1/N)*SUM[i=1 to n][LN(Y(I))] = 0
!
!     THEN
!
!       SCALE = ((1/N)*SUM[i=1 to n][Y(I)**GHAT])
!
!     FOR STARTING VALUE, USE
!
!       GHAT = 1.28/(STD DEV OF LOG(Y))
!
      IWRITE='OFF'
      AN=REAL(N)
      CALL SD(TEMP1,N,IWRITE,XLOGSD,IBUGA3,IERROR)
      CALL SUMDP(TEMP1,N,IWRITE,XLOGSM,IBUGA3,IERROR)
!
!     ESTIMATES FOR 2-PARAMETER MODEL.  USE DFZER2 TO FIND ROOT OF
!     THE EQUATION GIVEN ABOVE.
!
      DO 2101 I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2101 CONTINUE
      DWEISM=DBLE(XLOGSM/AN)
!
      DXSTRT=1.28D0/DBLE(XLOGSD)
      DAE=2.0*0.000001D0*DXSTRT
      DRE=DAE
      IN=N
      IFLAG=0
      DXLOW=DXSTRT/2.0D0
      DXUP=2.0D0*DXSTRT
      ITBRAC=0
 2105 CONTINUE
      XLOWSV=DXLOW
      XUPSV=DXUP
      CALL DFZER2(WEIFUN,DXLOW,DXUP,DXSTRT,DRE,DAE,IFLAG,DTEMP1)
!
      IF(IFLAG.EQ.4 .AND. ITBRAC.LE.100)THEN
         DXLOW=XLOWSV/2.0D0
         DXUP=2.0D0*XUPSV
         ITBRAC=ITBRAC+1
         GO TO 2105
      ENDIF
!
      IF(IFLAG.EQ.2)THEN
!
!       NOTE: SUPPRESS THIS MESSAGE FOR NOW.
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2111)
!2111   FORMAT('***** WARNING FROM WEIBULL MAXIMUM LIKELIHOOD--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2113)
!2113   FORMAT('      ESTIMATE OF SIGMA MAY NOT BE COMPUTED TO ',
!CCCC1         'DESIRED TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2121)
 2121   FORMAT('***** WARNING FROM WEIBULL MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2123)
 2123   FORMAT('      ESTIMATE OF SHAPE PARAMETER MAY BE NEAR ',   &
               'A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2131)
 2131   FORMAT('***** ERROR FROM WEIBULL MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2133)
 2133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2141)
 2141   FORMAT('***** WARNING FROM WEIBULL MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2143)
 2143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      SHAPML=REAL(DXLOW)
      DSUM=0.0D0
      DO 2108 I=1,N
        DSUM=DSUM + DBLE(Y(I)**SHAPML)
 2108 CONTINUE
      SCALML=REAL((DSUM/DBLE(N))**DBLE(1.0D0/DBLE(SHAPML)))
      IF(IWEIFL.EQ.'IWEI')SCALML=1.0/SCALML
!
      BN=1.0 + 2.2/AN**1.13
      SHAPBC=SHAPML/BN
      SCALSE=1.05293*SCALML/(SHAPML*SQRT(AN))
      SHAPSE=0.77970*SHAPML/SQRT(AN)
      SHABSE=0.77970*SHAPBC/(BN*SQRT(AN))
      COVSE=0.50697*SQRT(SCALML/AN)
      COVBSE=0.50697*SQRT(SCALML/(AN*BN))
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)N,XMEAN,XSD,XMIN,XMAX
 9013   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)SHAPML,SCALML,SHAPSE,SCALSE
 9017   FORMAT('SHAPML,SCALML,SHAPSE,SCALSE =  ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9019)SHAPBC,SHABSE,COVSE,COVBSE
 9019   FORMAT('SHAPBC,SHABSE,COVSE,COVBSE =  ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML1
      SUBROUTINE WEIML2(Y,TAG,N,IWEIBC,IWEIFL,MINMAX,MAXNXT,   &
                        ICASE,ICASE2,IDIST,   &
                        TEMP1,DTEMP1,ITEMP,   &
                        XMEAN,XSD,XVAR,XMIN,XMAX,   &
                        ZMEAN,ZSD,   &
                        SCALML,SCALSE,SHAPML,SHAPSE,   &
                        SHAPBC,SHABSE,COVSE,COVBSE,   &
                        IR,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE 2-PARAMETER WEIBULL DISTRIBUTION FOR THE RAW DATA
!              CASE WITH CENSORING (BUT NO GROUPING).  THIS ROUTINE
!              RETURNS ONLY THE POINT ESTIMATES (CONFIDENCE INTERVALS
!              WILL BE COMPUTED IN A SEPARATE ROUTINE).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLW2 WILL GENERATE THE OUTPUT
!              FOR THE WEIBULL MLE COMMAND).
!
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN
!                ENGINEERING", CAMBRIDGE UNIVERSITY PRESS,
!                1999, CHAPTER 17.
!              --JOHNSON, KOTZ, AND BALAKRISNAN, "CONTINUOUS
!                UNIVARIATE DISTRIBUTIONS--VOLUME 1", SECOND EDITION,
!                WILEY, 1994, CHAPTER xx.
!              --KEATS, LAWRENCE, AND WANG, "WEIBULL MAXIMUM
!                LIKELIHOOD PARAMETER ESTIMATES WITH CENSORED
!                DATA", JOURNAL OF QUALITY TECHNOLOGY, 29,
!                PP. 105-110.
!              --MURTHY, XIE, AND JIANG, "WEIBULL MODELS", WILEY,
!                2004, PP. 114-118 (FOR INVERTED WEIBULL).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/4
!     ORIGINAL VERSION--ARPIL     2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLW2)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
      DOUBLE PRECISION DTEMP1(*)
      INTEGER ITEMP(*)
!
      CHARACTER*4 ICASE
      CHARACTER*40 IDIST
      CHARACTER*7 ICASE2
      CHARACTER*4 IWEIBC
      CHARACTER*4 IWEIFL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
!
      DOUBLE PRECISION WEIFUN
      EXTERNAL WEIFUN
!
      INTEGER IN
      DOUBLE PRECISION DWEISM
      COMMON/WEICOM/DWEISM,IN
!
      INTEGER IN2
      INTEGER IR2
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      COMMON/WEICO5/DK,DTERM1,DTERM2,IN2,IR2
      INTEGER IN3
      INTEGER IR3
      DOUBLE PRECISION DK2
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTERM7
      DOUBLE PRECISION DGAMMA
      COMMON/WEICO6/DK2,DTERM6,DTERM7,DGAMMA,IN3,IR3
!
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION XSTART
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSCALE
!
      REAL FISH(2,2)
      REAL COV(2,2)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='L2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,ICASE2,IWEIBC
   52   FORMAT('IBUGA3,ISUBRO,ICASE,ICASE2,IWEIBC = ',   &
               3(A4,2X),A7,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)N,MINMAX,MAXNXT
   54   FORMAT('N,MINMAX,MAXNXT = ',3I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR WEIBULL MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      IDIST='WEIBULL'
      IF(IWEIFL.EQ.'ON')IDIST='INVERTED WEIBULL'
!
      CALL CKCENS(TAG,TEMP1,N,IDIST,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IWEIFL.EQ.'IWEI')THEN
        DO 1118 I=1,N
          Y(I)=1.0/Y(I)
 1118   CONTINUE
        CALL MEAN(Y,N,IWRITE,ZMEAN,IBUGA3,IERROR)
        CALL SD(Y,N,IWRITE,ZSD,IBUGA3,IERROR)
      ELSEIF(MINMAX.EQ.2)THEN
        DO 1119 I=1,N
          Y(I)=-Y(I)
 1119   CONTINUE
      ENDIF
!
      IF(MINMAX.NE.2 .OR. IWEIFL.EQ.'IWEI')THEN
        DO 1125 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1121)IDIST(1:16)
 1121       FORMAT('***** ERROR FROM ',A16,' MAXIMUM LIKELIHOOD--')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1122)
 1122       FORMAT('      NON-POSITIVE VALUE ENCOUNTERED.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1123)I,Y(I)
 1123       FORMAT('      ROW ',I8,' HAS THE VALUE = ',G15.7)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSE
            TEMP1(I)=LOG(Y(I))
          ENDIF
 1125   CONTINUE
      ELSE
        DO 1135 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1131)IDIST(1:16)
 1131       FORMAT('***** ERROR FROM ',A16,' MAXIMUM LIKELIHOOD--')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1132)
 1132       FORMAT('      NON-NEGATIVE VALUE ENCOUNTERED.')
            CALL DPWRST('XXX','WRIT')
            ATEMP=-Y(I)
            WRITE(ICOUT,1133)I,ATEMP
 1133       FORMAT('      ROW ',I8,' HAS THE VALUE = ',E15.7)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSE
            TEMP1(I)=LOG(Y(I))
          ENDIF
 1135   CONTINUE
      ENDIF
!
      CALL SORTC(Y,TAG,N,Y,TAG)
      IR=0
      DO 2120 I=1,N
        IF(TAG(I).EQ.1.0)IR=IR+1
 2120 CONTINUE
      IM=N-IR
      IR1=IR
      IR2=IR
      IR3=IR
!
      AR=REAL(IR)
      DR=DBLE(IR)
      AN=REAL(N)
      AM=REAL(IM)
!
      IF(IM.EQ.0)THEN
        ICASE='NONE'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2131)IDIST
 2131   FORMAT('***** WARNING FROM ',A16,' MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2133)
 2133   FORMAT('      NO CENSORING TIMES DETECTED.  IT IS RECOMMENDED')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2135)
 2135   FORMAT('      THAT THE FULL SAMPLE SYNTAX BE USED:')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2137)IDIST
 2137   FORMAT('      ',A16,' MAXIMUM LIKELIHOOD  Y')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ELSE
        ICASE='SING'
        AHOLD=Y(IR+1)
        DO 2140 I=IR+1,N
          IF(Y(I).NE.AHOLD)THEN
            ICASE='MULT'
            GO TO 2149
          ENDIF
 2140   CONTINUE
 2149   CONTINUE
      ENDIF
!
!               *******************************************************
!               **  STEP 3--                                         **
!               **  CARRY OUT CALCULATIONS FOR CENSORED WEIBULL MLE  **
!               *******************************************************
!
!     FOR THE SHAPE PARAMETER, SOLVE THE EQUATION:
!
!        (1/GHAT) -
!        SUM[i=1 to n][Y(I)**GHAT*LN(Y(I))]/SUM[i=1 to n][[Y(I)**GHAT] +
!        (1/R)*SUM[i=1 to r][LN(Y(I))] = 0
!
!     THEN
!
!        SCALE = ((1/N)*SUM[i=1 to n][Y(I)**GHAT])
!
!     FOR STARTING VALUE, USE
!
!        GHAT = 1.28/(STD DEV OF LOG(Y))
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,2191)IR,IM,ICASE
 2191   FORMAT('IR,IM,ICASE = ',2I8,A4)
        CALL DPWRST('XXX','BUG ')
        DO 2199 I=1,N
          WRITE(ICOUT,2197)I,Y(I),TAG(I)
 2197     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 2199   CONTINUE
      ENDIF
!
      CALL SD(TEMP1,N,IWRITE,XLOGSD,IBUGA3,IERROR)
!
!     ESTIMATES FOR 2-PARAMETER MODEL.  USE DFZER2 TO FIND ROOT OF
!     THE EQUATION GIVEN ABOVE.
!
      DSUM1=0.0D0
      DO 3101 I=1,N
        DTEMP1(I)=DBLE(Y(I))
        IF(TAG(I).EQ.1.0)DSUM1=DSUM1 + DLOG(DTEMP1(I))
 3101 CONTINUE
!
      DWEISM=DSUM1/DBLE(AR)
      XSTART=DBLE(1.28/XLOGSD)
      AE=2.0*0.000001*XSTART
      RE=AE
      IN=N
      IFLAG=0
      XLOW=XSTART/2.0D0
      XUP=2.0D0*XSTART
      ITBRAC=0
 3105 CONTINUE
      XLOWSV=XLOW
      XUPSV=XUP
      CALL DFZER2(WEIFUN,XLOW,XUP,XSTART,RE,AE,IFLAG,DTEMP1)
!
      IF(IFLAG.EQ.4 .AND. ITBRAC.LE.100)THEN
        XLOW=XLOWSV/2.0D0
        XUP=2.0D0*XUPSV
        ITBRAC=ITBRAC+1
        GO TO 3105
      ENDIF
!
      IF(IFLAG.EQ.2)THEN
!
!  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,3111)
!3111   FORMAT('***** WARNING FROM WEIBULL MAXIMUM ',
!CCCC1         'LIKELIHOOD--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,3113)
!3113   FORMAT('      ESTIMATE OF SIGMA MAY NOT BE COMPUTED TO ',
!CCCC1         'DESIRED TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3121)IDIST
 3121   FORMAT('***** WARNING FROM ',A16,' MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3123)
 3123   FORMAT('      ESTIMATE OF GAMMA MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3131)IDIST
 3131   FORMAT('***** ERROR FROM ',A16,' MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3133)
 3133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3141)IDIST
 3141   FORMAT('***** WARNING FROM ',A16,' MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3143)
 3143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      GAMMA=XLOW
      DSUM1=0.0D0
      DO 3108 I=1,N
        DSUM1=DSUM1 + DBLE(Y(I)**GAMMA)
 3108 CONTINUE
      SCALE=REAL((DSUM1/DBLE(IR))**DBLE(1.0D0/DBLE(GAMMA)))
      IF(IWEIFL.EQ.'IWEI')THEN
        SCALE=1.0/SCALE
        GO TO 9000
      ENDIF
!
      BN=1.0 + 2.2/AR**1.13
      GAMMBC=GAMMA/BN
!
!     COMPUTE STANDARD ERRORS.  DO FOR BOTH THE NO BIAS CORRECTION
!     ESTIMATES AND THE BIAS CORRECTED ESTIMATES.
!
      DSUM1=0.0D0
      DSCALE=DBLE(SCALE)
      DO 3210 I=1,N
        DX=DBLE(Y(I))
        DSUM1=DSUM1 +  ((DX/DSCALE)**GAMMA)*(DLOG(DX/DSCALE))**2
 3210 CONTINUE
      DSUM2=0.0D0
      DO 3220 I=1,N
        IF(TAG(I).EQ.1.0)THEN
          DX=DBLE(Y(I))
          DSUM2=DSUM2 + DLOG(DX)
        ENDIF
 3220 CONTINUE
!
      FISH(1,1)=AR*(GAMMA/SCALE)**2
      FISH(2,2)=AR/(GAMMA**2) + REAL(DSUM1)
      FISH(2,1)=(GAMMA/SCALE)*(AR*LOG(SCALE) - (AR/GAMMA) -   &
                REAL(DSUM2))
      FISH(1,2)=FISH(2,1)
!
      NDIM=2
      CALL SGECO(FISH,NDIM,NDIM,ITEMP,RCOND,TEMP1)
      IJOB=1
      CALL SGEDI(FISH,NDIM,NDIM,ITEMP,TEMP1,TEMP1(MAXNXT/2),IJOB)
      DO 3230 J=1,NDIM
        DO 3240 I=1,NDIM
          COV(I,J)=FISH(I,J)
 3240   CONTINUE
 3230 CONTINUE
      IF(COV(1,1).GE.0.0)THEN
        SCALSE=SQRT(COV(1,1))
      ELSE
        SCALSE=0.0
      ENDIF
      IF(COV(2,2).GE.0.0)THEN
        GAMMSE=SQRT(COV(2,2))
      ELSE
        GAMMSE=0.0
      ENDIF
      IF(COV(2,1).GE.0.0)THEN
        COVSE=SQRT(COV(2,1))
      ELSE
        COVSE=0.0
      ENDIF
!
      SHABSE=GAMMSE/BN
      COVBSE=COVSE/SQRT(BN)
!
      SHAPML=GAMMA
      SCALML=SCALE
      SHAPBC=GAMMBC
      SHAPSE=GAMMSE
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)N,XMEAN,XSD,XMIN,XMAX
 9013   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)SHAPML,SCALML,SHAPSE,SCALSE
 9017   FORMAT('SHAPML,SCALML,SHAPSE,SCALSE =  ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9019)SHAPBC,SHABSE,COVSE,COVBSE
 9019   FORMAT('SHAPBC,SHABSE,COVSE,COVBSE =  ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML2
      SUBROUTINE WEIM2B(Y,TAG,N,MAXNXT,   &
                        TEMP1,DTEMP1,ITEMP,   &
                        SHAPML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATE
!              FOR THE SHAPE PARAMETER OF THE 2-PARAMETER WEIBULL
!              DISTRIBUTION FOR THE RAW DATA CASE WITH CENSORING (BUT NO
!              GROUPING).
!
!              THIS ROUTINE IS A SIMPLIFIED VERSION OF WEIML2 THAT
!              IS USED IN TESTING WHETHER THE LOCATION PARAMETER IS
!              ZERO FOR THE WEIBULL DISTRIBUTION.  NOTE THAT THIS TEST
!              ONLY REQUIRES AN ESTIMATE OF THE SHAPE PARAMETER, SO
!              NO NEED TO COMPUTE THE SCALE PARAMETER.
!
!              THIS ROUTINE IS CALLED TWICE.  THE FIRST TIME IS FOR THE
!              FULL SAMPLE.  THE SECOND TIME IS FOR THE FIRST R1
!              UNCENSORED OBSERVATIONS.  THIS ROUTINE ASSUMES THAT THE
!              CALLING ROUTINE HAS CREATED THE APPROPRIATE Y AND TAG
!              VARIABLES FOR THESE TWO CASES.  NOTE ALSO THAT WE ARE
!              CURRENTLY ONLY SUPPORTING THIS TEST FOR THE STANDARD
!              WEIBULL CODE, SO REMOVE THE CODE FOR INVERTED WEIBULL
!              AND REVERSE WEIBULL.  ALSO, IGNORE THE BIAS CORRECTION.
!
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN
!                ENGINEERING", CAMBRIDGE UNIVERSITY PRESS,
!                1999, CHAPTER 17.
!              --HORST RINNE (2009), "THE WEIBULL DISTRIBUTION: A
!                HANDBOOK", CRC PRESS, PP. 640-642.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/8
!     ORIGINAL VERSION--AUGUST    2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
      DOUBLE PRECISION DTEMP1(*)
      INTEGER ITEMP(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
!
      DOUBLE PRECISION WEIFUN
      EXTERNAL WEIFUN
!
      INTEGER IN
      DOUBLE PRECISION DWEISM
      COMMON/WEICOM/DWEISM,IN
!
      INTEGER IN2
      INTEGER IR2
      DOUBLE PRECISION DK
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      COMMON/WEICO5/DK,DTERM1,DTERM2,IN2,IR2
      INTEGER IN3
      INTEGER IR3
      DOUBLE PRECISION DK2
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTERM7
      DOUBLE PRECISION DGAMMA
      COMMON/WEICO6/DK2,DTERM6,DTERM7,DGAMMA,IN3,IR3
!
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION XSTART
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
!
      CHARACTER*40 IDIST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='2B  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IM2B')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIM2B--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR WEIBULL MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IM2B')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      IDIST='WEIBULL'
      CALL CKCENS(TAG,TEMP1,N,IDIST,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CALL SORTC(Y,TAG,N,Y,TAG)
!
      DO 1125 I=1,N
        IF(Y(I).LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,1121)
 1121     FORMAT('***** ERROR FROM WEIBULL MAXIMUM LIKELIHOOD--')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,1122)
 1122     FORMAT('      NON-POSITIVE VALUE ENCOUNTERED.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,1123)I,Y(I)
 1123     FORMAT('      ROW ',I8,' HAS THE VALUE = ',G15.7)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GO TO 9000
        ELSE
          TEMP1(I)=LOG(Y(I))
        ENDIF
 1125 CONTINUE
!
      IR=0
      DO 2120 I=1,N
        AVAL=ABS(TAG(I))
        IF(AVAL.LE.0.5)THEN
          AVAL=0.0
        ELSE
          AVAL=1.0
        ENDIF
        TAG(I)=AVAL
        IF(AVAL.GE.0.5)IR=IR+1
 2120 CONTINUE
      IM=N-IR
      IR1=IR
      IR2=IR
      IR3=IR
!
      AR=REAL(IR)
      DR=DBLE(IR)
      AN=REAL(N)
      AM=REAL(IM)
!
      IF(IM.EQ.0)THEN
        ICASE='NONE'
      ELSE
        ICASE='SING'
        AHOLD=Y(IR+1)
        DO 2140 I=IR+1,N
          IF(Y(I).NE.AHOLD)THEN
            ICASE='MULT'
            GO TO 2149
          ENDIF
 2140   CONTINUE
 2149   CONTINUE
      ENDIF
!
!               *******************************************************
!               **  STEP 3--                                         **
!               **  CARRY OUT CALCULATIONS FOR CENSORED WEIBULL MLE  **
!               *******************************************************
!
!     FOR THE SHAPE PARAMETER, SOLVE THE EQUATION:
!
!        (1/GHAT) -
!        SUM[i=1 to n][Y(I)**GHAT*LN(Y(I))]/SUM[i=1 to n][[Y(I)**GHAT] +
!        (1/R)*SUM[i=1 to r][LN(Y(I))] = 0
!
!     FOR STARTING VALUE, USE
!
!        GHAT = 1.28/(STD DEV OF LOG(Y))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IM2B')THEN
        ISTEPN='31'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,2191)IR,IM,ICASE
 2191   FORMAT('IR,IM,ICASE = ',2I8,A4)
        CALL DPWRST('XXX','BUG ')
        DO 2199 I=1,N
          WRITE(ICOUT,2197)I,Y(I),TAG(I)
 2197     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 2199   CONTINUE
      ENDIF
!
      CALL SD(TEMP1,N,IWRITE,XLOGSD,IBUGA3,IERROR)
!
!     ESTIMATES FOR 2-PARAMETER MODEL.  USE DFZER2 TO FIND ROOT OF
!     THE EQUATION GIVEN ABOVE.
!
      DSUM1=0.0D0
      DO 3101 I=1,N
        DTEMP1(I)=DBLE(Y(I))
        IF(TAG(I).EQ.1.0)DSUM1=DSUM1 + DLOG(DTEMP1(I))
 3101 CONTINUE
!
      DWEISM=0.0D0
      IF(IR.GT.0)THEN
        DWEISM=DSUM1/DBLE(AR)
      ENDIF
      XSTART=DBLE(1.28/XLOGSD)
      AE=2.0*0.000001*XSTART
      RE=AE
      IN=N
      IFLAG=0
      XLOW=XSTART/2.0D0
      XUP=2.0D0*XSTART
      ITBRAC=0
 3105 CONTINUE
      XLOWSV=XLOW
      XUPSV=XUP
      CALL DFZER2(WEIFUN,XLOW,XUP,XSTART,RE,AE,IFLAG,DTEMP1)
!
      IF(IFLAG.EQ.4 .AND. ITBRAC.LE.100)THEN
        XLOW=XLOWSV/2.0D0
        XUP=2.0D0*XUPSV
        ITBRAC=ITBRAC+1
        GO TO 3105
      ENDIF
!
      IF(IFLAG.EQ.2)THEN
!
!  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,3111)
!3111   FORMAT('***** WARNING FROM WEIBULL MAXIMUM ',
!CCCC1         'LIKELIHOOD--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,3113)
!3113   FORMAT('      ESTIMATE OF SIGMA MAY NOT BE COMPUTED TO ',
!CCCC1         'DESIRED TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3121)
 3121   FORMAT('***** WARNING FROM WEIBULL MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3123)
 3123   FORMAT('      ESTIMATE OF GAMMA MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3133)
 3133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3143)
 3143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      SHAPML=XLOW
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IM2B')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIM2B--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)N,IR,IM,ITEMP(1),SHAPML,IERROR
 9017   FORMAT('N,IR,IM,ITEMP(1),SHAPML,IERROR  =  ',4I8,G15.7,2X,A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIM2B
      SUBROUTINE WEIML3(Y,N,IWEIFL,IWEIML,IWEIMM,IWEIMO,IWEIEP,IWEILM,   &
                        MINMAX,MAXNXT,ISEED,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,DTEMP1,   &
                        XMEAN,XSD,XVAR,XMIN,XMAX,XSKEW,   &
                        ZMEAN,ZSD,   &
                        ALOCPE,SCALPE,SHAPPE,   &
                        ALOCWB,SCALWB,SHAPWB,   &
                        ALOCMO,SCALMO,SHAPMO,   &
                        ALOCM2,SCALM2,SHAPM2,   &
                        ALOCML,SCALML,SHAPML,   &
                        ALOCLM,SCALLM,SHAPLM,   &
                        ALOCEP,SCALEP,SHAPEP,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE 3-PARAMETER WEIBULL DISTRIBUTION FOR THE RAW DATA
!              CASE (I.E., NO CENSORING AND NO GROUPING).
!
!              THE DEFAULT IS FOR THE REGULAR (= "MINIMUM") WEIBULL.
!              THE REVERSE ("MAXIMUM") WEIBULL IS HANDLED BY TAKING THE
!              NEGATIVE OF THE ORIGINAL DATA AND THEN FITTING THE
!              MINIMUM WEIBULL CASE.  AFTER OBTAINING THE PARAMETER
!              ESTIMATES, TAKE THE NEGATIVE OF THE LOCATION PARAMETER.
!
!              GENERATE ESTIMATES BASED ON:
!
!                 1. ZANAKIS PERCENTILE ESTIMATES
!                 2. WYCOFF-BAIN-ENGLEHART PERCENTILES
!                 3. MOMENT ESTIMATES
!                 4. MODIFIED MOMENTS
!                 5. MAXIMUM LIKELIHOOD
!                 6. L-MOMENTS
!                 7. ELEMENTAL PERCENTILES
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLW1 WILL GENERATE THE OUTPUT
!              FOR THE WEIBULL MLE COMMAND).
!
!     REFERENCES--COHEN AND WHITTEN, "PARAMETER ESTIMATION IN RELIABILITY
!                 AND LIFE SPAN MODELS", MARCEL DEKKER, INC.
!               --BURY (1999). "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                 CAMBRIDGE, PP. 326-329.
!               --CASTILLO, HADI, BALAKRISHNAN, AND SARABIA (2005),
!                 "EXTREME VALUE AND RELATED MODELS WITH APPLICATIONS
!                 IN ENGINEERING AND SCIENCE", WILEY.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/5
!     ORIGINAL VERSION--APARIL    2010
!     UPDATED         --SEPTEMBER 2014 SUPPORT FOR L-MOMENTS AND
!                                      ELEMENTAL PERCENTILES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DOUBLE PRECISION DTEMP1(*)
!
      DOUBLE PRECISION DG(3)
      DOUBLE PRECISION V(6)
      DOUBLE PRECISION DL
      DOUBLE PRECISION DU
      DOUBLE PRECISION D
      DOUBLE PRECISION FL
      DOUBLE PRECISION FU
      DOUBLE PRECISION FD
      DOUBLE PRECISION F
      DOUBLE PRECISION DN
      DOUBLE PRECISION DEPS
      DOUBLE PRECISION VAL
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DXN
      DOUBLE PRECISION S1
      DOUBLE PRECISION S2
      DOUBLE PRECISION SS1
      DOUBLE PRECISION SS2
      DOUBLE PRECISION D0
      DOUBLE PRECISION BOUND
      DOUBLE PRECISION STEP
      DOUBLE PRECISION T
      DOUBLE PRECISION TU
      DOUBLE PRECISION TL
      DOUBLE PRECISION THETA
      DOUBLE PRECISION B
      DOUBLE PRECISION S12
      DOUBLE PRECISION S13
      DOUBLE PRECISION S3
      DOUBLE PRECISION FT
      DOUBLE PRECISION XPAR(3)
      DOUBLE PRECISION FVEC(2)
      DOUBLE PRECISION TOL
      DOUBLE PRECISION XMOM(3)
!
      CHARACTER*4 IWEIFL
      CHARACTER*4 IWEIML
      CHARACTER*4 IWEIMM
      CHARACTER*4 IWEIMO
      CHARACTER*4 IWEIEP
      CHARACTER*4 IWEILM
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IGEPDF
      CHARACTER*4 ICASPL
      CHARACTER*4 IFLAGN
!
      DOUBLE PRECISION DGAMMA
      EXTERNAL DGAMMA
      DOUBLE PRECISION WEIMO2
      EXTERNAL WEIMO2
      EXTERNAL WEIML6
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*40 IDIST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='L3  '
!
      ALOCMO=CPUMIN
      SCALMO=CPUMIN
      SHAPMO=CPUMIN
      ALOCM2=CPUMIN
      SCALM2=CPUMIN
      SHAPM2=CPUMIN
!
      ALOCML=CPUMIN
      SCALML=CPUMIN
      SHAPML=CPUMIN
!
      ALOCPE=CPUMIN
      SCALPE=CPUMIN
      SHAPPE=CPUMIN
!
      ALOCWB=CPUMIN
      SCALWB=CPUMIN
      SHAPWB=CPUMIN
!
      ALOCLM=CPUMIN
      SCALLM=CPUMIN
      SHAPLM=CPUMIN
!
      ALOCEP=CPUMIN
      SCALEP=CPUMIN
      SHAPEP=CPUMIN
!
      DO 11 I=1,6
        V(I)=0.0
   11 CONTINUE
!
      IWRITE='OFF'
      IERROR='NO'
      DEPS=0.1D-5
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IWEIFL,IWEILM,MINMAX
   52   FORMAT('IBUGA3,ISUBRO,IWEIFL,IWEILM,MINMAX = ',4(A4,2X),I5)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
      IF(N.LE.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN 3-PARAMETER WEIBULL MAXIMUM ',   &
               'LIKELIHOOD--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 5.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  SET FLAGS THAT SPECIFY WHETHER TO   **
!               **  TAKE RECIPROCALS AND/OR NEGATIVE    **
!               **  OF DATA.                            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOTE: TAKING RECIPROCAL OF DATA TO ESTIMATE THE
!           INVERTED WEIBULL (= FRECHET) ONLY WORKS WITH
!           THE 2-PARAMETER CASE, NOT THE 3-PARAMETER CASE.
      IFLAGN='NO'
      IDIST='WEIBULL'
      IF(MINMAX.EQ.2)IFLAGN='YES'
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE SUMMARY STATISTICS AND      **
!               **  TRANSFORM THE DATA IF NEEDED        **
!               ******************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      CALL STMOM3(Y,N,IWRITE,XSKEW,IBUGA3,IERROR)
!
!     NEGATION IF NEEDED
!
      IF(IFLAGN.EQ.'YES')THEN
        DO 119 I=1,N
          Y(I)=-Y(I)
  119   CONTINUE
      ENDIF
      CALL SORT(Y,N,Y)
!
!     SUMMARY STATISTICS FOR TRANSFORMED DATA
!
      IF(IFLAGN.EQ.'YES')THEN
        CALL MINIM(Y,N,IWRITE,ZMIN,IBUGA3,IERROR)
        CALL MEAN(Y,N,IWRITE,ZMEAN,IBUGA3,IERROR)
        CALL SD(Y,N,IWRITE,ZSD,IBUGA3,IERROR)
        CALL STMOM3(Y,N,IWRITE,ZSKEW,IBUGA3,IERROR)
      ELSE
         ZMIN=XMIN
         ZMEAN=XMEAN
         ZSD=XSD
         ZSKEW=XSKEW
      ENDIF
!
!               ******************************************
!               **  STEP 3--                            **
!               **  L-MOMENT ESTIMATES                  **
!               ******************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       1) COMPUTE THE L-MOMENT ESTIMATORS FOR THE GENERALIZED
!          EXTREME VALUE.
!
!       2) CONVERT TO WEIBULL ESTIMATES USING FORMULAS FROM P. 195
!          OF HOSKINGS AND WALLIS:
!
      NMOM=3
      DO 310 I=1,N
        TEMP1(I)=-Y(I)
!CCCC   TEMP1(I)=Y(I)
  310 CONTINUE
      CALL SORT(TEMP1,N,TEMP1)
      DO 320 I=1,N
        DTEMP1(I)=DBLE(TEMP1(I))
  320 CONTINUE
      CALL SAMLMU(DTEMP1,N,XMOM,NMOM)
      CALL GEVPEL(XMOM,XPAR)
      ALOCGV=REAL(XPAR(1))
      SCALGV=REAL(XPAR(2))
      SHAPGV=REAL(XPAR(3))
      SHAPLM=1.0/SHAPGV
      SCALLM=SCALGV*SHAPLM
      ALOCLM=ALOCGV + SCALLM
      ALOCLM=-ALOCLM
!
!     L-MOMENTS CAN OCCASSIONALLY GENERATE LOCATION ESTIMATES THAT
!     ARE GREATER THAN THE SAMPLE MINIMUM.  CHECK FOR THIS AND SET
!     LOCATION VALUE TO MINIMUM VALUE MINUS DIFFERENCE BETWEEN MINIMUM
!     NEXT SMALLEST TO MINIMUM.
!
      IF(ALOCLM.GT.TEMP1(1))THEN
        IF(TEMP1(2).GT.TEMP1(1))THEN
          EPS=TEMP1(2) - TEMP1(1)
        ELSE
          EPS=0.000001
          DO 330 I=2,N
            IF(TEMP1(I).GT.TEMP1(1))THEN
              EPS=TEMP1(2) - TEMP1(1)
              GO TO 339
            ENDIF
  330     CONTINUE
  339     CONTINUE
        ENDIF
      ENDIF
      IF(IFLAGN.EQ.'YES')ALOCLM=-ALOCLM
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,311)ALOCGV,SCALGV,SHAPGV
  311   FORMAT('ALOCGV,SCALGV,SHAPGV = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,312)ALOCLM,SCALLM,SHAPLM
  312   FORMAT('ALOCLM,SCALLM,SHAPLM = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,MINMAX
      ENDIF
!
!               ******************************************
!               **  STEP 4--                            **
!               **  ELEMENTAL PERCENTILE ESTIMATES      **
!               ******************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IWEIEP.EQ.'OFF')GO TO 499
      IGEPDF='NULL'
      ICASPL='GEV'
      ITEMP=2
      NSAMP=20*N
      IF(NSAMP.GT.5000)NSAMP=5000
      MINMX2=2
      DO 401 I=1,N
        Y(I)=-Y(I)
  401 CONTINUE
      CALL DPEPM2(Y,N,ICASPL,MAXNXT,MINMX2,IGEPDF,   &
                  ISEED,NSAMP,   &
                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                  ALOCGV,SCALGV,SHAPGV,   &
                  IBUGA3,ISUBRO,IERROR)
      SHAPEP=1.0/SHAPGV
      SCALEP=SCALGV*SHAPEP
      ALOCEP=ALOCGV + SCALEP
      ALOCEP=-ALOCEP
      IF(IFLAGN.EQ.'YES')ALOCEP=-ALOCEP
      DO 403 I=1,N
        Y(I)=-Y(I)
  403 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,421)ALOCGV,SCALGV,SHAPGV
  421   FORMAT('ALOCGV,SCALGV,SHAPGV = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,422)ALOCEP,SCALEP,SHAPEP
  422   FORMAT('ALOCEP,SCALEP,SHAPEP = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,MINMAX
      ENDIF
!
  499 CONTINUE
!
!               ******************************************
!               **  STEP 5--                            **
!               **  PERCENTILE METHODS                  **
!               ******************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE ZANAKIS ESTIMATORS FOUND ON P. 45 OF COHEN/WHITTEN BOOK.
!     THESE ARE PERCENTILE ESTIMATORS THAT DO NOT REQUIRE
!     ANY ITERATION.  USE AS STARTING VALUES FOR ML ESTIMATION.
!
!     NOTE SEPTEMBER 2010: IF SEVERAL VALUES ARE EQUAL TO THE
!          MINIMUM VALUE (AS CAN HAPPEN WHEN TAKE A BOOTSTRAP
!          SAMPLE), CAN HAVE ISSUES WITH THE DLOG(DTERM1/DTERM2)
!          TERM.  ALSO, LOCATION WILL BE EQUAL TO MINIMUM.
!
      EPS=1.0E-6
      CALL SORT(Y,N,Y)
      IF(Y(1).EQ.Y(2))THEN
        ALOCPE=Y(1) - EPS
      ELSE
        DX1=DBLE(Y(1))
        DX2=DBLE(Y(2))
        DXN=DBLE(Y(N))
        DTERM1=DX1*DXN - DX2**2
        DTERM2=DX1 + DXN - 2.0D0*DX2
        ALOCPE=REAL(DTERM1/DTERM2)
      ENDIF
      PI=0.16731
      PK=0.97366
      IVAL1=INT(REAL(N)*PK+1.0)
      IVAL2=INT(REAL(N)*PI+1.0)
      IVAL3=INT(0.63*REAL(N)+1.0)
      DTERM1=DBLE(Y(IVAL1) - ALOCPE)
      DTERM2=DBLE(Y(IVAL2) - ALOCPE)
      SHAPPE=REAL(2.989D0/DLOG(DTERM1/DTERM2))
      SCALPE=-ALOCPE + Y(IVAL3)
!
      IF(IFLAGN.EQ.'YES')ALOCPE=-ALOCPE
!
!     COMPUTE WYCOFF-BAIN-ENGLEHARDT ESTIMATORS USING ROUTINE FOUND
!     ON P. 350 OF COHEN/WHITTEN BOOK.  THESE ARE PERCENTILE ESTIMATORS.
!
      DN=DBLE(N)
      S=0.84*REAL(N)
      S1=0.0D0
      S2=0.0
      D0=2.989D0/LOG((Y(IVAL1)- Y(1))/(Y(IVAL2)-Y(1)))
      ALOCWB=(Y(1) - DBLE(ZMEAN)/DN**(1.0D0/D0))/   &
             (1.0D0 - 1.0D0/DN**(1.0D0/D0))
      IS=INT(S)
      DO 1300 I=1,IS
        SS1=LOG(Y(I)-ALOCWB)
        S1=S1+SS1
 1300 CONTINUE
      IS1=INT(S+1.0)
      DO 1400 I=IS1,N
        SS2=LOG(Y(I)-ALOCWB)
        S2=S2+SS2
 1400 CONTINUE
      SHAPWB=DN*1.4192/(-S1+DBLE(IS)/(DN-DBLE(IS))*S2)
      SCALWB=EXP((0.5772/SHAPWB) + (S1+S2)/DN)
!
      IF(IFLAGN.EQ.'YES')ALOCWB=-ALOCWB
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,151)XMIN,XMEAN,XSD,XSKEW
  151   FORMAT('XMIN,XMEAN,XSD,XSKEW = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,153)ZMIN,ZMEAN,ZSD,ZSKEW
  153   FORMAT('ZMIN,ZMEAN,ZSD,ZSKEW = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,155)ALOCPE,SCALPE,SHAPPE
  155   FORMAT('ALOCPE,SCALPE,SHAPPE = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,157)ALOCWB,SCALWB,SHAPWB
  157   FORMAT('ALOCWB,SCALWB,SHAPWB = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               ******************************************
!               **  STEP 6--                            **
!               **  MOMENT ESTIMATES                    **
!               ******************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOW COMPUTE THE STANDARD MOMENT ESTIMATES BASED ON THE
!     EQUATIONS GIVEN ON PAGE 31 OF WHITTEN AND COHEN.
!     BASED ON CODE GIVEN ON PAGE 341-342 OF THAT BOOK.
!
      IF(IWEIMO.EQ.'OFF')GO TO 2099
      DN=DBLE(N)
      DEPS=0.1D-7
      DL=0.1
      DU=50.0
      DU=100.0
!
!     COMPUTE FUNCTION AT INTERVAL END-POINTS
!
      DO 2011 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DL)
 2011 CONTINUE
      FL=WEIMO2(DG(1),DG(2),DG(3),DBLE(ZSKEW))
      DO 2013 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DU)
 2013 CONTINUE
      FU=WEIMO2(DG(1),DG(2),DG(3),DBLE(ZSKEW))
!
!     PROVIDED A ZERO EXISTS IN THE GIVEN INTERVAL, BISECT THE
!     INTERVAL AND CHOOSE THE SUBINTERVAL WITH A SIGN CHANGE UNTIL
!     THE INTERVAL WIDTH IS WITHIN TOLERANCE.
!
      IF(FL*FU.GT.0.0)THEN
!
!       NO SOLUTION WITHIN DEFINED INTERVAL
        GO TO 2099
      ENDIF
!
 2014 CONTINUE
      D=(DU+DL)/2.0
      DO 2015 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 2015 CONTINUE
      FD=WEIMO2(DG(1),DG(2),DG(3),DBLE(ZSKEW))
      V(3)=ZSD/DSQRT(DG(2) - DG(1)**2)
      V(1)=ZMEAN - V(3)*DG(1)
      V(2)=D
      V(4)=V(1) + V(3)**DG(1)
      V(5)=V(3)*DSQRT(DG(2) - DG(1)**2)
      V(6)=(DG(3)-3.0*DG(2)*DG(1)+2.0*DG(1)**3)/   &
           (DG(2)-DG(1)**2)**1.5
!
      ALOCM2=V(1)
      SHAPM2=V(2)
      SCALM2=V(3)
!
      IF(ABS(DL-D).GT.DEPS)THEN
        IF(FD*FL.LE.0.0D0)THEN
          DU=D
        ELSE
          DL=D
          FL=FD
        ENDIF
        GO TO 2014
      ENDIF
!
      IF(IFLAGN.EQ.'YES')ALOCM2=-ALOCM2
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,2091)FL,FU
 2091   FORMAT('FL,FU = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2093)ALOCM2,SCALM2,SHAPM2
 2093   FORMAT('ALOCM2,SCALM2,SHAPM2 = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
 2099 CONTINUE
!
!               ******************************************
!               **  STEP 7--                            **
!               **  MODIFIED MOMENT ESTIMATES           **
!               ******************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE MODIFIED MOMENT ESTIMATORS USING CODE FOUND ON
!     PP. 343-344 OF COHEN/WHITTEN BOOK.
!
!     NOTE: FOR VALUES OF SKEWNESS LESS THAN 0.1 (AND PARTICULARLY
!           NEGATIVE SKEWNESS, COHEN'S DL AND DU DO NOT BOUND
!           PROPERLY.  SO CHECK VALUE OF SKEWNES AND ADJUST
!           CONSTANTS ACCORDINGLY.  SOME QUICK SIMULATIONS SHOW
!           THAT ESTIMATES BECOME UNREASONABLE FOR SMALL POSITIVE
!           OR NEGATIVE SKEWNESS (GAMMA > 6 OR SO), SO DON'T
!           COMPUTE MODIFIED MOMENT ESTIMATES IN THIS CASE.
!           THESE CASES SHOULD HAVE CONVERGENT MAXIMUM LIKELIHOOD
!           ESTIMATES, SO USE ZANAKIS ESTIMATES AS STARTING
!           VALUES FOR MAXIMUM LIKELIHOOD.
!
      IF(IWEIMM.EQ.'OFF')GO TO 1199
!
      DN=DBLE(N)
      DEPS=0.1D-7
      VAL=(ZSD/(ZMEAN - ZMIN))**2
!CCCC DU=3.22D0
!CCCC DL=0.5D0
      DL=0.1D0
      IF(ZSKEW.GT.0.5)THEN
        DU=3.22D0
      ELSEIF(ZSKEW.GT.0.1)THEN
        DU=20.00D0
      ELSE
        DU=50.0
        DU=100.0
      ENDIF
!
      DO 1011 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DU)
 1011 CONTINUE
      FU=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/DU))*DG(1))**2
      DO 1111 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DL)
 1111 CONTINUE
      FL=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/DL))*DG(1))**2
!
!     NOW PERFORM BISECTION (FIRST CHECK THAT WE HAVE FOUND A
!     BOUNDING INTERVAL).
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1151)DL,DU,FL,FU
 1151   FORMAT('DL,DU,FL,FU = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(FL*FU.GT.0.0D0)THEN
        GO TO 1199
      ENDIF
!
      D=(DU+DL)/2.0D0
      F=FL
  100 CONTINUE
      IF(ABS(D-DL).GT.DEPS)THEN
        DO 1112 I=1,3
          DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 1112   CONTINUE
        F=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/D))*DG(1))**2
        IF(F*FL.LE.0.0D0)THEN
          DU=D
        ELSE
          DL=D
          FL=F
        ENDIF
        D=(DU+DL)/2.0
        GO TO 100
      ELSE
        DO 1115 I=1,3
          DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 1115   CONTINUE
        V(2)=D
        V(3)=ZSD/DSQRT(DG(2) - DG(1)**2)
        V(1)=ZMEAN - V(3)*DG(1)
        V(4)=V(1) + V(3)*DG(1)
        V(5)=V(3)*(DG(2) - DG(1)**2)**0.5D0
        V(6)=(DG(3) - 3.0D0*DG(2)*DG(1) + 2.0D0*DG(1)**3)/   &
             (DG(2) - DG(1)**2)**1.5D0
      ENDIF
      ALOCMO=V(1)
      SHAPMO=V(2)
      SCALMO=V(3)
!
      IF(IFLAGN.EQ.'YES')ALOCMO=-ALOCMO
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1181)ALOCMO,SCALMO,SHAPMO
 1181   FORMAT('ALOCMO,SCALMO,SHAPMO = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1151)DL,DU,FL,FU
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
 1199 CONTINUE
!
!               ******************************************
!               **  STEP 8--                            **
!               **  MAXIMUM LIKELIHOOD ESTIMATES        **
!               ******************************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IWEIML.EQ.'OFF')GO TO 9000
!
!     ONLY TRY SOLVING ML EQUATIONS IF L-MOMENTS AND WBE VALUES FOR
!     SHAPE PARAMETER IS LARGER THAN 1.05.
!
!     USE COHEN ALGORITHM FIRST.  IF THIS FAILS, THEN TRY LIKELIHOOD
!     EQUATIONS FROM BURY.
!
      IF(SHAPLM.LT.1.05 .AND. SHAPWB.LT.1.05)GO TO 9000
!
!     TRY USING COHEN'S ALGORITHM FIRST
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,4991)
 4991   FORMAT('TRYING COHENS ML METHOD')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      BOUND=ZMIN - 6.0D0*ZSD
      STEP=ZSD/50.0D0
      T=ZMIN - STEP/10.0D0
 5000 CONTINUE
        IF(T.GT.BOUND)THEN
          CALL WEIML4(Y,N,T,D,S3,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')THEN
            T=T-STEP
            GO TO 5000
          ELSE
            TU=T
            THETA=S3/DN
            B=THETA**(1.0D0/D)
            S12=0.0D0
            S13=0.0D0
            DO 5013 I=1,N
              S13=S13+(Y(I)-TU)**(D-1.0D0)
              S12=S12+1.0D0/(Y(I)-TU)
 5013       CONTINUE
            FU=D/THETA*S13 - (D-1.0D0)*S12
            FL=FU
 5100       CONTINUE
            IF(FU*FL.GE.0.0D0 .AND. IERROR.EQ.'NO')THEN
              T=T-STEP
              IF(T.GT.BOUND)THEN
                CALL WEIML4(Y,N,T,D,S3,ISUBRO,IBUGA3,IERROR)
                IF(IERROR.EQ.'NO')THEN
                  THETA=S3/DN
                  B=THETA**(1.0D0/D)
                  S12=0.0D0
                  S13=0.0D0
                  DO 5113 I=1,N
                    S13=S13+(Y(I)-T)**(D-1.0D0)
                    S12=S12+1.0D0/(Y(I)-T)
 5113             CONTINUE
                  FL=D/THETA*S13 - (D-1.0D0)*S12
                ENDIF
              ELSE
!CCCC           IERROR='YES'
                GO TO 4000
              ENDIF
              GO TO 5100
            ELSE
              IF(IERROR.EQ.'NO')THEN
                TL=T
                FT=FU
 5200           CONTINUE
                CALL WEIML4(Y,N,T,D,S3,ISUBRO,IBUGA3,IERROR)
                IF(IERROR.EQ.'YES')THEN
                  GO TO 4000
                ELSE
                  THETA=S3/DN
                  B=THETA**(1.0D0/D)
                  S12=0.0D0
                  S13=0.0D0
                  DO 5114 I=1,N
                    S13=S13+(Y(I)-T)**(D-1.0D0)
                    S12=S12+1.0D0/(Y(I)-T)
 5114             CONTINUE
                  FT=D/THETA*S13 - (D-1.0D0)*S12
                  V(1)=T
                  V(2)=D
                  V(3)=THETA**(1.0D0/D)
                  DO 5115 I=1,3
                    DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 5115             CONTINUE
                  V(4)=V(1) + V(3)*DG(1)
                  V(5)=V(3)*(DG(2) - DG(1)**2)**0.5D0
                  V(6)=(DG(3) - 3.0D0*DG(2) + 2.0D0*DG(1)**3)/   &
                       (DG(2) - DG(1)**2)**1.5D0
                  IF(ABS(TU-T).GT.DEPS .AND. IERROR.EQ.'NO')THEN
                    IF(FT*FU.LE.0.0D0)THEN
                      TL=T
                    ELSE
                      TU=T
                      FU=FT
                    ENDIF
                    T=(TL+TU)/2.0D0
                    GO TO 5200
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
!CCCC     IERROR='YES'
          GO TO 4000
        ENDIF
        ALOCML=V(1)
        SHAPML=V(2)
        SCALML=V(3)
!
        IF(IFLAGN.EQ.'YES')ALOCML=-ALOCML
!
        GO TO 9000
!
!     TRY LIKLIHOOD EQUATIONS
!
 4000 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,4001)
 4001   FORMAT('TRYING BURY ML METHOD')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!CCCC XPAR(1)=DBLE(SHAPPP)
!CCCC XPAR(2)=DBLE(ALOCPP)
      XPAR(1)=DBLE(SHAPWB)
      XPAR(2)=DBLE(ALOCWB)
      DO 4111 I=1,MAXNXT
        DTEMP1(I)=0.0D0
 4111 CONTINUE
      IOPT=2
      TOL=1.0D-5
      NVAR=2
      NPRINT=-1
      INFO=-1
      LWA=MAXNXT
      CALL DNSQE(WEIML6,JAC,IOPT,NVAR,XPAR,FVEC,TOL,NPRINT,INFO,   &
                 DTEMP1,MAXNXT,Y,N)
!
      MLFLAG=0
      IF(INFO.EQ.0)MLFLAG=1
      IF(INFO.EQ.2)MLFLAG=1
      IF(INFO.EQ.4)MLFLAG=1
      IF(MLFLAG.EQ.0)THEN
        SHAPML=REAL(XPAR(1))
        ALOCML=REAL(XPAR(2))
        DSUM1=0.0D0
        DO 4190 I=1,N
          DSUM1=DSUM1 + (DBLE(Y(I)) - XPAR(2))**XPAR(1)
 4190   CONTINUE
        DTERM1=(DSUM1/DBLE(N))**(1.0D0/XPAR(1))
        SCALML=REAL(DTERM1)
        GO TO 9000
      ENDIF
!
      IF(IFLAGN.EQ.'YES')ALOCML=-ALOCML
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,4191)ALOCML,SCALML,SHAPML
 4191   FORMAT('ALOCML,SCALML,SHAPML = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
 9000 CONTINUE
!
      IF(IFLAGN.EQ.'YES')THEN
        DO 9003 I=1,N
          Y(I)=-Y(I)
 9003   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)ALOCPE,SCALPE,SHAPPE
 9013   FORMAT('ZANAKIS: ALOCPE,SCALPE,SHAPPE = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)ALOCWB,SCALWB,SHAPWB
 9015   FORMAT('WYCOFF: ALOCWB,SCALWB,SHAPWB = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)ALOCMO,SCALMO,SHAPMO
 9017   FORMAT('MODIFIED MOMENTS: ALOCMO,SCALMO,SHAPMO = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9019)ALOCM2,SCALM2,SHAPM2
 9019   FORMAT('MOMENTS: ALOCM2,SCALM2,SHAPM2 = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9021)ALOCML,SCALML,SHAPML
 9021   FORMAT('MLE: ALOCML,SCALML,SHAPML = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML3
      SUBROUTINE WEIML4(X,N,T,D,S3,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--UTIITY ROUTINE USED BY WEIML3.  THIS IS THE MLDEL
!              ROUTINE ON PP. 348-369 OF COHEN.
!
!     REFERENCE--COHEN AND WHITTEN, "PARAMETER ESTIMATION IN RELIABILITY
!                AND LIFE SPAN MODELS", MARCEL DEKKER, INC.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/4
!     ORIGINAL VERSION--APARIL    2010
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION T
      DOUBLE PRECISION D
      DOUBLE PRECISION DL
      DOUBLE PRECISION DU
      DOUBLE PRECISION FL
      DOUBLE PRECISION FU
      DOUBLE PRECISION F
      DOUBLE PRECISION DN
      DOUBLE PRECISION DEPS
      DOUBLE PRECISION S1
      DOUBLE PRECISION S2
      DOUBLE PRECISION S3
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML4--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IERROR='NO'
      DN=DBLE(N)
      DEPS=0.1D-7
      DU=3.0D0
      DL=1.01D0
!
!     CALCULATE FUNCTION AT UPPER BOUND
!
      S1=0.0D0
      S2=0.0D0
      S3=0.0D0
      DO 11 I=1,N
        DX=DBLE(X(I))
        S3=S3 + (DX - T)**DU
        S2=S2 + (DX - T)**DU*LOG(DX - T)
        S1=S1 + LOG(DX - T)
   11 CONTINUE
      FU=S2/S3 - 1.0D0/DU - S1/DN
!
!     CALCULATE FUNCTION AT LOWER BOUND
!
      S1=0.0D0
      S2=0.0D0
      S3=0.0D0
      DO 12 I=1,N
        DX=DBLE(X(I))
        S3=S3 + (DX - T)**DL
        S2=S2 + (DX - T)**DL*LOG(DX - T)
        S1=S1 + LOG(DX - T)
   12 CONTINUE
      FL=S2/S3 - 1.0D0/DL - S1/DN
!
!     CHECK TO SEE IF BOUNDING INTERVAL FOUND.  IF SO, USE
!     BISECTION METHOD
!
      IF(FL*FU.GT.0.0D0)THEN
        IERROR='YES'
      ELSE
        D=(DU+DL)/2.0D0
        F=FL
  100   CONTINUE
        S2=0.0D0
        S3=0.0D0
        DO 13 I=1,N
          DX=DBLE(X(I))
          S3=S3 + (DX - T)**D
          S2=S2 + (DX - T)**D*LOG(DX - T)
   13   CONTINUE
        F=S2/S3 - 1.0D0/D - S1/DN
        IF(ABS(D-DL) .GT. DEPS)THEN
          IF(F*FL .LE. 0.0D0)THEN
            DU=D
          ELSE
            DL=D
            FL=F
          ENDIF
          D=(DU+DL)/2.0D0
          GO TO 100
        ENDIF
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML4--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML4
      SUBROUTINE WEIML5(ALOC,SCALE,SHAPE,Y,N,COV,   &
                        XTEMP,ITEMP,MAXNXT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE PARAMETER VARIANCE-COVARIANCE
!              MATRIX FOR THE 3-PARAMETER WEIBULL DISTRIBUTION.  NOTE
!              THAT THESE ARE ONLY CONSIDERED VALID IF THE VALUE OF
!              THE SHAPE PARAMETER IS AT LEAST 2.2.  THE FISHER
!              INFORMATION MATRIX IS GIVEN ON PAGE 46 OF COHEN AND
!              WHITTEN.
!
!     REFERENCE--BURY (1999). "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE, PP. 326-329.
!              --COHEN AND WHITTEN, "PARAMETER ESTIMATION IN RELIABILITY
!                AND LIFE SPAN MODELS", MARCEL DEKKER, INC., PP. 45-46.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/5
!     ORIGINAL VERSION--APRIL     2010
!
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      INTEGER   ITEMP(*)
!
      REAL COV(3,3)
      REAL FISH(3,3)
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DLOC
      DOUBLE PRECISION DSCALE
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DZ
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DSUM4
      DOUBLE PRECISION DSUM5
      DOUBLE PRECISION DSUM6
      DOUBLE PRECISION DC
      DOUBLE PRECISION DJ
      DOUBLE PRECISION DK
      DOUBLE PRECISION A11
      DOUBLE PRECISION A12
      DOUBLE PRECISION A13
      DOUBLE PRECISION A21
      DOUBLE PRECISION A22
      DOUBLE PRECISION A23
      DOUBLE PRECISION A31
      DOUBLE PRECISION A32
      DOUBLE PRECISION A33
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION DGAMMA
      DOUBLE PRECISION DPSI
      DOUBLE PRECISION TRIGAM
      EXTERNAL DGAMMA
      EXTERNAL DPSI
      EXTERNAL TRIGAM
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='L5  '
      IWRITE='OFF'
      IERROR='NO'
!
      A11=0.0
      A12=0.0
      A13=0.0
      A21=0.0
      A22=0.0
      A23=0.0
      A31=0.0
      A32=0.0
      A33=0.0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML5')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML5--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)ALOC,SCALE,SHAPE
   54   FORMAT('ALOC,SCALE,SHAPE = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR VARIANCE-COVARIANCE MATRIX      **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML5')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 101 J=1,3
        DO 103 I=1,3
          COV(I,J)=CPUMIN
  103   CONTINUE
  101 CONTINUE
!
!     USE FORMULAS GIVEN IN BURY BY DEFAULT.  OPTIONALLY,
!     USE COHEN ALGORITHM.  NOTE THAT BURY GIVES A DIFFERENT
!     APPROXIMATION FOR CASE WHERE SHAPE PARAMETER IS BETWEEN
!     1 AND 2.
!
!CCCC IF(SHAPE.LT.2.1 .OR. SCALE.LT.0.0)GO TO 9000
!
      IFLAGC=0
      DN=REAL(N)
      DLOC=DBLE(ALOC)
      DSCALE=DBLE(SCALE)
      DSHAPE=DBLE(SHAPE)
!
      IF(SHAPE.GT.2.001 .AND. IFLAGC.EQ.0)THEN
!
        DTERM1=(DN/DSCALE**2)*(DSHAPE - 1.0D0)**2
        DTERM2=DGAMMA(1.0D0 - (2.0D0/DSHAPE))
        A11=DTERM1*DTERM2
!
        A22=(DN/DSCALE**2)*DSHAPE**2
        A33=(DN/DSHAPE**2)*1.82368D0
        A32=-(DN/DSCALE)*0.42278D0
!
        DTERM1=(DN/DSCALE**2)*DSHAPE*(DSHAPE - 1.0D0)
        DTERM2=DGAMMA(1.0D0 - (1.0D0/DSHAPE))
        A21=DTERM1*DTERM2
!
        DTERM1=-(DN/(DSCALE*DSHAPE))*(DSHAPE - 1.0D0)
        DTERM2=DGAMMA(1.0D0 - (1.0D0/DSHAPE))
        DTERM3=1.0D0 + DPSI(1.0D0 - (1.0D0/DSHAPE))
        A31=DTERM1*DTERM2*DTERM3
!
      ELSEIF(SHAPE.GT.1.0 .AND. IFLAGC.EQ.0)THEN
!
         DSUM1=0.0D0
         DSUM2=0.0D0
         DSUM3=0.0D0
         DSUM4=0.0D0
         DSUM5=0.0D0
         DSUM6=0.0D0
         DO 110 I=1,N
           DX=DBLE(Y(I))
           DZ=(DX-DLOC)/DSCALE
           DSUM1=DSUM1 + 1.0D0/DZ
           DSUM2=DSUM2 + 1.0D0/DZ**2
           DSUM3=DSUM3 + DLOG(DZ)
           DSUM4=DSUM4 + (DZ**DSHAPE)*(DLOG(DZ)**2)
           DSUM5=DSUM5 + DZ**(DSHAPE-2.0D0)
           DSUM6=DSUM6 + DZ**(DSHAPE-1.0D0)*DLOG(DZ)
  110    CONTINUE
         A11=((DSHAPE-1.0D0)/DSCALE**2)*(DSUM2 + DSUM5*DSHAPE)
         A22=DN*(DSHAPE/DSCALE)**2
         A33=(DN/DSHAPE**2) + DSUM4
         A21=DSHAPE*(DSHAPE-1.0D0)*DSUM1/DSCALE**2
         A31=(DSUM1/(DSCALE*DSHAPE)) - (DSHAPE/DSCALE)*DSUM6
         A32=-(DSHAPE/DSCALE)*(DSUM3 + (DN/DSHAPE))
!
      ELSEIF(SHAPE.GT.2.01 .AND. IFLAGC.EQ.1)THEN
!
        DTERM1=2.0D0 - 1.0D0/DSHAPE
        DA=1.0D0 + DPSI(DTERM1)
        DTERM1=DGAMMA(1.0D0 - 2.0D0/DSHAPE)
        DTERM2=DGAMMA(2.0D0 - 2.0D0/DSHAPE)
        DC=(DTERM1 + DSHAPE*DTERM2)*(DSHAPE-1.0D0)/DSHAPE**2
        DTERM1=DGAMMA(1.0D0 - 1.0D0/DSHAPE)
        DTERM2=DGAMMA(2.0D0 - 1.0D0/DSHAPE)
        DJ=DTERM1 - DA*DTERM2
        DK=TRIGAM(1.0D0,IFAULT) + DPSI(2.0D0)**2
!
        A11=DN*DC*(DSHAPE/DSCALE)**2
        A22=DN*(DSHAPE/DSCALE)**2
        A33=(DN/DSHAPE**2)*DK
        A31=(DN/DSCALE)*DJ
        A21=DN*(DSHAPE/DSCALE)**2*DTERM2
        A32=-(DN/DSCALE)*DPSI(2.0D0)
!
      ENDIF
!
      FISH(1,1)=REAL(A11)
      FISH(1,2)=REAL(A21)
      FISH(1,3)=REAL(A31)
      FISH(2,1)=REAL(A21)
      FISH(2,2)=REAL(A22)
      FISH(2,3)=REAL(A32)
      FISH(3,1)=REAL(A31)
      FISH(3,2)=REAL(A32)
      FISH(3,3)=REAL(A33)
!
      CALL SGECO(FISH,3,3,ITEMP,RCOND,XTEMP)
      IJOB=1
      CALL SGEDI(FISH,3,3,ITEMP,XTEMP,XTEMP(MAXNXT/2),IJOB)
      DO 2810 J=1,3
        DO 2815 I=1,3
          COV(I,J)=FISH(I,J)
 2815   CONTINUE
 2810 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML5--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)DA,DC,DJ,DK
 9012   FORMAT('DA,DC,DJ,DK = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)DTERM1,DTERM2,RCOND
 9014   FORMAT('DTERM1,DTERM2,RCOND = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9016)A11,A12,A13
 9016   FORMAT('A11,A12,A13 = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9018)A21,A22,A23,A33
 9018   FORMAT('A21,A22,A23,A33 = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9020)COV(1,1),COV(2,2),COV(3,3)
 9020   FORMAT('COV(1,1),COV(2,2),COV(3,3) = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML5
      SUBROUTINE WEIML6(N,X,FVEC,IFLAG,XDATA,NOBS)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE FUNCTIONS FOR THE
!              3-PARAMETER WEIBULL MAXIMUM LIKELIHOOD EQUATIONS.
!
!              WE HAD PREVIOUSLY IMPLEMENTED SOME CODES FROM
!              COHEN.  HOWEVER, THIS SEEMS TO FAIL IN SOME CASES
!              WHERE IT PROBABLY SHOULD NOT.  SO USE AN EQUATION
!              SOLVER FOR THE LIKELIHOOD EQUATIONS (USE THE
!              ESTIMATES OBTAINED BY WB AS THE STARTING VALUES).
!
!              SPECIFICALLY, SOLVE THE 2 EQUATIONS (Ghat, Uhat, Shat
!              ARE THE CURRENT ESTIMATES OF THE SHAPE, LOCATION, AND
!              SCALE PARAMETERS, RESPECTIVELY.
!
!              (NUM/DENOM] - Ghat/(Ghat - 1) = 0
!
!              WHERE
!
!              NUM = SUM[i=1 to N][(X(i) - Uhat)**Ghat]*
!                    SUM[i=1 to N][1/(X(i) - Uhat)]
!
!
!              DENOM = N*SUM[i=1 to N][(X(i) - Uhat)**(Ghat - 1)]
!
!              AND
!
!              (1/Ghat) - (NUM2/DENOM2) +
!              (1/N)*SUM[i=1 to N][LN(X(i) - Uhat)] = 0
!
!              WHERE
!
!              NUM2 = SUM[i=1 to N][(X(i) - Uhat)**Ghat*
!                     LOG(X(i) - Uhat)]
!
!
!              DENOM2 = SUM[i=1 to N][X(i) - Uhat)**Ghat]
!
!              NOTE THAT MAXIMUM LIKELIHOOD ESTIMATION IS UNDEFINED
!              IF THE SHAPE PARAMETER IS LESS THAN 1.
!
!              ONCE Ghat AND Uhat ARE FOUND, USE THE FOLLOWING FOR
!              THE SCALE PARAMETER (NOT DONE HERE)
!
!              Shat = ((1/N)*SUM[i=1 to N][(X(i) - Uhat)**Ghat)])**(1/Ghat)
!
!              CALLED BY DNSQE ROUTINE FOR SOLVING SIMULTANEOUS
!              NONLINEAR EQUATIONS.  NOTE THAT THE CALLING SEQUENCE
!              DID NOT ACCOMODATE A DATA ARRAY (AND ASSCIATED NUMBER OF
!              OBSERVATIONS), SO THESE WERE ADDED TO THE CALL LIST.
!     EXAMPLE--3-PARAMETER MAXIMUM LIKELIHOOD Y
!     REFERENCE--BURY (1999). "STATISTICAL DISTRIBUTIONS IN ENGINEERING",
!                CAMBRIDGE, PP. 326-329.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/03
!     ORIGINAL VERSION--MARCH     2013.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION FVEC(*)
      REAL XDATA(*)
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DG
      DOUBLE PRECISION DU
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DSUM4
      DOUBLE PRECISION DSUM5
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      N=2
      IFLAG=0
!
      DG=X(1)
      DU=X(2)
      DN=DBLE(NOBS)
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DSUM4=0.0D0
      DSUM5=0.0D0
!
      DO 200 I=1,NOBS
        DX=DBLE(XDATA(I))
        DSUM1=DSUM1 + (DX - DU)**DG
        DSUM2=DSUM2 + 1.0D0/(DX - DU)
        DSUM3=DSUM3 + (DX - DU)**(DG-1.0D0)
        DSUM4=DSUM4 + ((DX - DU)**DG)*DLOG(DX - DU)
        DSUM5=DSUM5 + DLOG(DX - DU)
  200 CONTINUE
!
      DTERM1=DG/(DG - 1.0D0)
      DTERM2=(DSUM1*DSUM2)/(DN*DSUM3)
      FVEC(1)=DTERM2 - DTERM1
      DTERM3=(1.0D0/DG)
      DTERM4=DSUM4/DSUM1
      FVEC(2)=DTERM3 - DTERM4 + (DSUM5/DN)
!
      RETURN
      END SUBROUTINE WEIML6
      SUBROUTINE WEIML8(Y,N,ICASPL,IWEIBC,IWEIFL,MINMAX,MAXNXT,P3WEMI,   &
                        IOPFLG,   &
                        TEMP1,DTEMP1,Y2,   &
                        ALOCML,SCALML,SHAPML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--MAXIMUM LIKELIHOOD ESTIMATION IS KNOW TO BE PROBLEMATIC
!              FOR THE 3-PARAMETER WEIBULL DISTRIBUTION.  THE METHODS
!              FOR SOLVING THE NON-LINEAR EQUATIONS MAY NOT ALWAYS
!              CONVERGE.  IN PARTICULAR, THE COHEN-WHITTEN ALGORITHM
!              WE USE CAN SOMETIMES FAIL.
!
!              THIS SUBROUTINE IMPLEMENTS THE PROFILE LOG-LIKELIHOOD
!              METHOD ORIGINALLY PROPOSED BY LAWLESS.  THIS METHOD
!              DOES THE FOLLOWING:
!
!                1. CREATE A GRID FOR THE LOCATION PARAMETER FROM 0 TO
!                   THE DATA MINIMUM (MINUS AN EPSILON) VALUE.
!
!                2. ITERATE THROUGH THE GRID AND DO THE FOLLOWING:
!
!                    A. SET THE LOCATION PARAMETER TO THE GRID VALUE.
!                       CALL THIS VALUE A0.
!
!                    B. LET Y2 = Y - A0.
!
!                    C. ESTIMATE THE SCALE AND SHAPE PARAMETER USING
!                       STANDARD 2-PARAMETER WEIBULL ML METHODS.
!
!                    D. COMPUTE THE LOG-LIKELIHOOD OF THE 3-PARAMETER
!                       WEIBULL BASED ON THESE PARAMETER ESTIMATES.
!
!              THE PARAMETER ESTIMATES THAT GENERATE THE MAXIMUM
!              LIKELIHOOD VALUE ARE THE ESTIMATES USED.
!
!     REFERENCES--LAWLESS (2003), "STATISTICAL MODELS AND METHODS FOR
!                 LIFETIME DATA", SECOND EDITION, WILEY, PP. 187-190.
!               --RINNE (2009), "THE WEIBULL DISTRIBUTION: A HANDBOOK",
!                 CRC PRESS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/3
!     ORIGINAL VERSION--MARCH     2014
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION Y(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DOUBLE PRECISION DTEMP1(*)
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IWEIBC
      CHARACTER*4 IWEIFL
      CHARACTER*4 IOPFLG
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IOP
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*40 IDIST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='L8  '
      IWRITE='OFF'
      IERROR='NO'
!
      EPS=0.1E-5
      DSUM1=0.0D0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML8')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIML8--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IWEIFL,MINMAX,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASE,MINMAX,MAXNXT = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!
!               **************************************************
!               **  STEP 0--OPEN THE STORAGE FILES              **
!               **************************************************
!
      ISTEPN='1.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML8')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     ONLY WRITE TO DPST2F.DAT IF CALLING FROM DPMLW3.  CALLING
!     FROM OTHER LOCATIONS (E.G., BOOTSTRAP) CAN CAUSE CONFLICT
!     FOR USAGE OF DPST2F.DAT FILE.
!
      IF(IOPFLG.EQ.'ON' .OR. IOPFLG.EQ.'YES')THEN
        IOP='OPEN'
        IFLAG1=0
        IFLAG2=1
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR WEIBULL MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML8')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='WEIBULL'
      IF(IWEIFL.EQ.'IWEI')IDIST='INVERTED WEIBULL'
!
      IF(N.LE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)IDIST(1:16)
  111   FORMAT('***** ERROR IN ',A16,' MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 3.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     THIS METHOD ASSUMES LOCATION IS NON-NEGATIVE (IT CAN BE EXTENDED
!     TO A NEGATIVE LOWER LIMITS, BUT DEFER ON IMPLEMENTING THAT FOR
!     NOW).
!
      DO 120 I=1,N
        IF(MINMAX.EQ.2 .AND. IWEIFL.EQ.'WEIB')THEN
          IF(Y(I).GE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)IDIST(1:16)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,122)I,Y(I)
  122       FORMAT('      ROW ',I8,' IS NON-NEGATIVE.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ELSE
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)IDIST(1:16)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,127)I,Y(I)
  127       FORMAT('      ROW ',I8,' IS NON-POSITIVE (',G15.7,')')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
  120 CONTINUE
!
!     STEP 1: COMPUTE 2-PARAMETER ML ESTIMATES FOR ORIGINAL DATA SET
!
      CALL WEIML1(Y,N,IWEIBC,IWEIFL,MINMAX,   &
                  TEMP1,DTEMP1,   &
                  XMEAN,XSD,XVAR,XMIN,XMAX,   &
                  ZMEAN,ZSD,   &
                  SCALML,SCALSE,SHAPML,SHAPSE,   &
                  SHAPBC,SHABSE,COVSE,COVBSE,   &
                  ISUBRO,IBUGA3,IERROR)
      YMIN=P3WEMI
      IF(YMIN.GE.XMIN)YMIN=0.0
      ALOCML=YMIN
      CALL WEILI1(Y,N,ICASPL,MINMAX,ALOCML,SCALML,SHAPML,   &
                  ALIK,AIC,AICC,BIC,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IOPFLG.EQ.'ON' .OR. IOPFLG.EQ.'YES')THEN
        WRITE(IOUNI2,151)ALOCML,ALIK,SCALML,SHAPML
  151   FORMAT(4E15.7)
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML8')THEN
        WRITE(ICOUT,131)SCALML,SHAPML,ALIK
  131   FORMAT('ZERO CASE: SCALML,SHAPML,ALIK = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!     STEP 2: NOW ITERATE THROUGH VALUES OF THE LOCATION PARAMETER
!
!             IF MINIMUM VALUE IS SUFFICIENTLY SMALL, JUST DEFINE A
!             SINGLE GRID.  IF IT IS LARGE, THEN DO 2 PASSES.  ONCE
!             WITH A BROAD GRID AND THEN WITH A FINER GRID.
!
      XMINT=XMIN - EPS
      IF(XMIN.LE.100.0)THEN
        NITER=1
        IPASS=1
        IF(XMIN.LE.10)THEN
          AINC=0.001
        ELSE
          AINC=0.01
        ENDIF
        NLOOP=INT(((XMINT-YMIN)/AINC)+0.1)
      ELSE
        NITER=2
        IPASS=1
        NLOOP=100
        AINC=(XMIN-YMIN)/REAL(NLOOP)
      ENDIF
!
 1000 CONTINUE
!
      IF(IPASS.EQ.2)THEN
        XSTRT=ALOCML - AINC
        IF(XSTRT.LE.0.0)XSTRT=0.0
        XSTOP=ALOCML + AINC
        NLOOP=100
        AINC=XMIN/REAL(NLOOP)
      ELSE
        XSTRT=YMIN
      ENDIF
!
      DO 1100 ILOOP=1,NLOOP
!
!       STEP 2A: SUBTRACT OFF CONDITIONAL VALUE OF LOCATION
!
        ALOC=XSTRT + REAL(ILOOP)*AINC
        IF(ALOC.GE.XMIN)GO TO 1100
        DO 1110 I=1,N
          Y2(I)=Y(I) - ALOC
 1110   CONTINUE
!
!       STEP 2B: COMPUTE 2-PARAMETER ML ESTIMATES FOR MODIFIED DATA SET
!                (BUT NOT THAT LIKELIHOOD IS COMPUTED FOR ORIGINAL DATA)
!
        CALL WEIML1(Y2,N,IWEIBC,IWEIFL,MINMAX,   &
                    TEMP1,DTEMP1,   &
                    XMEANT,XSDT,XVART,XMINT,XMAXT,   &
                    ZMEANT,ZSDT,   &
                    SCALMT,SCALSE,SHAPMT,SHAPSE,   &
                    SHAPBC,SHABSE,COVSE,COVBSE,   &
                    ISUBRO,IBUGA3,IERROR)
        CALL WEILI1(Y,N,ICASPL,MINMAX,ALOC,SCALMT,SHAPMT,   &
                    ALIKT,AIC,AICC,BIC,   &
                    ISUBRO,IBUGA3,IERROR)
!
      IF(IOPFLG.EQ.'ON' .OR. IOPFLG.EQ.'YES')THEN
        WRITE(IOUNI2,151)ALOC,ALIKT,SCALMT,SHAPMT
      ENDIF
!
!
!       STEP 2C: COMPARE LIKELIHOOD TO CURRENT MAXIMUM
!
        IF(ALIKT.GT.ALIK)THEN
          ALIK=ALIKT
          ALOCML=ALOC
          SCALML=SCALMT
          SHAPML=SHAPMT
          ITEMP=ILOOP
        ENDIF
!
 1100 CONTINUE
!
      IF(NITER.EQ.2 .AND. IPASS.EQ.1)THEN
        IPASS=2
        GO TO 1000
      ENDIF
!
!       STEP 3: COMPARE TO "CORNER-POINT" LIKELIHOOD
!
!
      ALOCT=XMIN
      SHAPMT=1.0
      DO 1200 I=1,N
        DSUM1=DSUM1 + DBLE(Y(I) - ALOCT)
 1200 CONTINUE
      SCALMT=REAL(DSUM1/DBLE(N))
!
      CALL WEILI1(Y,N,ICASPL,MINMAX,ALOCT,SCALMT,SHAPMT,   &
                  ALIKT,AIC,AICC,BIC,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IOPFLG.EQ.'ON' .OR. IOPFLG.EQ.'YES')THEN
        WRITE(IOUNI2,151)ALOCT,ALIKT,SCALMT,SHAPMT
      ENDIF
!
      IF(ALIKT.GT.ALIK)THEN
        ALIK=ALIKT
        ALOCML=ALOCT
        SCALML=SCALMT
        SHAPML=SHAPMT
      ENDIF
!
 9000 CONTINUE
!
      IF(IOPFLG.EQ.'ON' .OR. IOPFLG.EQ.'YES')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML8')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIML8--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)NITER,NLOOP,IPASS,IERROR
 9013   FORMAT('NITER,NLOOP,IPASS,IERROR = ',3I8,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9021)ALOCML,SCALML,SHAPML,ALIK
 9021   FORMAT('MLE: ALOCML,SCALML,SHAPML,ALIK = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIML8
      SUBROUTINE WEIMO1(XMEAN,XSD,XMIN,XSKEW,N,PSTAMV,   &
                        ALOCMO,SCALMO,SHAPMO,   &
                        ALOCMM,SCALMM,SHAPMM,   &
                        SCALM2,SHAPM2,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES VARIOUS MOMENT ESTIMATES FOR THE
!              WEIBULL DISTRIBUTION WHEN ONLY SUMMARY DATA IS AVAILABLE.
!              ONLY THE REGULAR WEIBULL FOR UNCENSORED DATA IS SUPPORTED
!              (I.E., THE REVERSE WEIBILL AND INVERTED WEIBULL ARE NOT
!              CURRRENTLY SUPPORTED).
!
!     REFERENCE--COHEN AND WHITTEN, "PARAMETER ESTIMATION IN RELIABILITY
!                AND LIFE SPAN MODELS", MARCEL DEKKER, INC., P. 31 AND
!                PP. 341-344.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/6
!     ORIGINAL VERSION--JUNE      2012
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DG(3)
      DOUBLE PRECISION V(6)
      DOUBLE PRECISION DL
      DOUBLE PRECISION DU
      DOUBLE PRECISION D
      DOUBLE PRECISION FL
      DOUBLE PRECISION FU
      DOUBLE PRECISION FD
      DOUBLE PRECISION F
      DOUBLE PRECISION DEPS
      DOUBLE PRECISION VAL
      DOUBLE PRECISION DN
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION DGAMMA
      EXTERNAL DGAMMA
      DOUBLE PRECISION WEIMO2
      EXTERNAL WEIMO2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEIM'
      ISUBN2='O1  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IMO1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF WEIMO1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)XMEAN,XSD,XMIN,XSKEW
   54   FORMAT('XMEAN,XSD,XMIN,XSKEW = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR WEIBULL MLE ESTIMATE            **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IMO1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(XMEAN.EQ.CPUMIN .OR. XMEAN.EQ.PSTAMV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN WEIBULL MOMENT ESTIMATION--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE SAMPLE MEAN IS UNDEFINED.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(XSD.EQ.CPUMIN .OR. XSD.EQ.PSTAMV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,107)
  107   FORMAT('      THE SAMPLE STANDARD DEVIATION IS UNDEFINED.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(XSD.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIBULL MOMENT ESTIMATION--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE SAMPLE STANDARD DEVIATION IS NON-POSTIVE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)XSD
  113   FORMAT('      STANDARD DEVIATION = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DEPS=0.1D-7
!
      ALOCMO=CPUMIN
      SCALMO=CPUMIN
      SHAPMO=CPUMIN
!
      ALOCMM=CPUMIN
      SCALMM=CPUMIN
      SHAPMM=CPUMIN
!
      SCALM2=CPUMIN
      SHAPM2=CPUMIN
!
!     IN CODE BELOW:
!
!        V(1) = ESTIMATE OF LOCATION
!        V(2) = ESTIMATE OF SHAPE
!        V(3) = ESTIMATE OF SCALE
!        V(4) = WEIBULL MEAN (BASED ON ESTIMATED PARAMETERS)
!        V(5) = WEIBULL SD (BASED ON ESTIMATED PARAMETERS)
!        V(6) = WEIBULL SKEWNESS (BASED ON ESTIMATED PARAMETERS)
!
!     COMPUTE MODIFIED MOMENT ESTIMATORS USING CODE FOUND ON
!     PP. 343-344 OF COHEN/WHITTEN BOOK.
!
!     NOTE: FOR VALUES OF SKEWNESS LESS THAN 0.1 (AND PARTICULARLY
!           NEGATIVE SKEWNESS, COHEN'S DL AND DU DO NOT BOUND
!           PROPERLY.  SO CHECK VALUE OF SKEWNESS AND ADJUST
!           CONSTANTS ACCORDINGLY.  SOME QUICK SIMULATIONS SHOW
!           THAT ESTIMATES BECOME UNREASONABLE FOR SMALL POSITIVE
!           OR NEGATIVE SKEWNESS (GAMMA > 6 OR SO), SO DON'T
!           COMPUTE MODIFIED MOMENT ESTIMATES IN THIS CASE.
!
!     IF XMIN PARAMETER NOT GIVEN, THEN SKIP THIS CASE.
!
      IF(XMIN.EQ.CPUMIN)GO TO 1199
      IF(N.LT.1)GO TO 1199
!
      DN=DBLE(N)
      VAL=(XSD/(XMEAN - XMIN))**2
!CCCC DU=3.22D0
!CCCC DL=0.5D0
      DL=0.1D0
      IF(XSKEW.GT.0.5)THEN
        DU=3.22D0
      ELSEIF(XSKEW.GT.0.1)THEN
        DU=20.00D0
      ELSE
        DU=50.0
      ENDIF
!
      DO 1011 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DU)
 1011 CONTINUE
      FU=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/DU))*DG(1))**2
      DO 1111 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DL)
 1111 CONTINUE
      FL=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/DL))*DG(1))**2
!
!     NOW PERFORM BISECTION (FIRST CHECK THAT WE HAVE FOUND A
!     BOUNDING INTERVAL).
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IMO1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1151)DL,DU,FL,FU
 1151   FORMAT('DL,DU,FL,FU = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(FL*FU.GT.0.0D0)THEN
        GO TO 1199
      ENDIF
!
      D=(DU+DL)/2.0D0
      F=FL
  100 CONTINUE
      IF(ABS(D-DL).GT.DEPS)THEN
        DO 1112 I=1,3
          DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 1112   CONTINUE
        F=VAL - (DG(2) - DG(1)**2)/((1.0D0 - DN**(-1.0D0/D))*DG(1))**2
        IF(F*FL.LE.0.0D0)THEN
          DU=D
        ELSE
          DL=D
          FL=F
        ENDIF
        D=(DU+DL)/2.0
        GO TO 100
      ELSE
        DO 1115 I=1,3
          DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 1115   CONTINUE
!
        V(2)=D
        V(3)=XSD/DSQRT(DG(2) - DG(1)**2)
        V(1)=XMEAN - V(3)*DG(1)
        V(4)=V(1) + V(3)*DG(1)
        V(5)=V(3)*(DG(2) - DG(1)**2)**0.5D0
        V(6)=(DG(3) - 3.0D0*DG(2)*DG(1) + 2.0D0*DG(1)**3)/   &
             (DG(2) - DG(1)**2)**1.5D0
      ENDIF
      ALOCMM=V(1)
      SHAPMM=V(2)
      SCALMM=V(3)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IMO1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1181)ALOCMM,SCALMM,SHAPMM
 1181   FORMAT('ALOCMM,SCALMM,SHAPMM = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
 1199 CONTINUE
!
!     NOW COMPUTE THE STANDARD MOMENT ESTIMATES BASED ON THE
!     EQUATIONS GIVEN ON PAGE 31 OF WHITTEN AND COHEN.
!     BASED ON CODE GIVEN ON PAGE 341-342 OF THAT BOOK.
!
!     IF XSKEW PARAMETER NOT GIVEN, THEN SKIP THIS CASE.
!
      IF(XSKEW.EQ.CPUMIN)GO TO 9000
!
      DL=0.1
      DU=50.0
!
!     COMPUTE FUNCTION AT INTERVAL END-POINTS
!
      DO 2011 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DL)
 2011 CONTINUE
      FL=WEIMO2(DG(1),DG(2),DG(3),DBLE(XSKEW))
      DO 2013 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/DU)
 2013 CONTINUE
      FU=WEIMO2(DG(1),DG(2),DG(3),DBLE(XSKEW))
!
!     PROVIDED A ZERO EXISTS IN THE GIVEN INTERVAL, BISECT THE
!     INTERVAL AND CHOOSE THE SUBINTERVAL WITH A SIGN CHANGE UNTIL
!     THE INTERVAL WIDTH IS WITHIN TOLERANCE.
!
      IF(FL*FU.GT.0.0)THEN
!
!       NO SOLUTION WITHIN DEFINED INTERVAL
        GO TO 2099
      ENDIF
!
 2014 CONTINUE
      D=(DU+DL)/2.0
      DO 2015 I=1,3
        DG(I)=DGAMMA(1.0D0 + DBLE(I)/D)
 2015 CONTINUE
      FD=WEIMO2(DG(1),DG(2),DG(3),DBLE(XSKEW))
      V(3)=XSD/DSQRT(DG(2) - DG(1)**2)
      V(1)=XMEAN - V(3)*DG(1)
      V(2)=D
      V(4)=V(1) + V(3)**DG(1)
      V(5)=V(3)*DSQRT(DG(2) - DG(1)**2)
      V(6)=(DG(3)-3.0*DG(2)*DG(1)+2.0*DG(1)**3)/   &
           (DG(2)-DG(1)**2)**1.5
!
      ALOCMO=V(1)
      SHAPMO=V(2)
      SCALMO=V(3)
!
      IF(ABS(DL-D).GT.DEPS)THEN
        IF(FD*FL.LE.0.0D0)THEN
          DU=D
        ELSE
          DL=D
          FL=FD
        ENDIF
        GO TO 2014
      ENDIF
!
 2099 CONTINUE
!
!     NOW COMPUTE THE MOMENT ESTIMATES FOR THE 2-PARAMETER
!     WEIBULL DISTRIBUTION.  THIS IS BASED ON P. 322 OF BURY.
!
      XCOEFV=XSD/XMEAN
!
!     SHAPE PARAMETER CAN BE DETERMINED FROM SOLVING FOLLOWING
!     EQUATION (WHERE CV IS THE COEFFICIENT OF VARIATION):
!
!          CV = SQRT{(GAMMA(1 + 2/SHAPE)/GAMMA(1 + 1/SHAPE)**2 - 1)}
!
!     AN INITIAL APPROXIMATION IS
!
!          SHAPE = CV**(-1.0852)
!
      DL=0.1
      DU=50.0
      D=DBLE(XCOEFV**(-1.0852))
!
!     FOR DL AND DU, USE PRE-COMPUTED VALUES FOR ALL EXCEPT CV
!     PART OF EQUATION
!
      FL=DBLE(XCOEFV) - 0.4298314D+03
      FU=DBLE(XCOEFV) - 0.2528897D-01
      DG(1)=DGAMMA(1.0D0 + 2.0D0/D)
      DG(2)=DGAMMA(1.0D0 + 1.0D0/D)
      FD=DBLE(XCOEFV) - DSQRT(DG(1)/DG(2)**2 - 1.0D0)
      IF(FL*FD.LT.0.0)THEN
        DU=D
        FU=FD
      ELSEIF(FU*FD.LT.0.0)THEN
        DL=D
        FL=FD
      ELSE
        GO TO 3099
      ENDIF
!
 3014 CONTINUE
      D=(DU+DL)/2.0D0
      DG(1)=DGAMMA(1.0D0 + 2.0D0/D)
      DG(2)=DGAMMA(1.0D0 + 1.0D0/D)
      FD=DBLE(XCOEFV) - DSQRT(DG(1)/DG(2)**2 - 1.0D0)
!
      IF(ABS(DL-D).GT.DEPS)THEN
        IF(FD*FL.LE.0.0D0)THEN
          DU=D
        ELSE
          DL=D
          FL=FD
        ENDIF
        GO TO 3014
      ENDIF
!
      SHAPM2=D
      SCALM2=REAL(DBLE(XMEAN)/DGAMMA(1.0D0 + 1.0D0/D))
!
 3099 CONTINUE
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IMO1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF WEIMO1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)ALOCMO,SCALMO,SHAPMO
 9012   FORMAT('ALOCMO,SCALMO,SHAPMO = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)ALOCMM,SCALMM,SHAPMM
 9013   FORMAT('ALOCMM,SCALMM,SHAPMM = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)SCALM2,SHAPM2
 9014   FORMAT('SCALM2,SHAPM2 = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE WEIMO1
      DOUBLE PRECISION FUNCTION WEIMO2(A,B,C,D1)
!
!     PURPOSE--COMPUTE A FUNCTION NEEDED BY WEIMO1 (USED TO COMPUTE
!              MOMENT ESTIMATES FOR THE 3-PARAMETER WEIBULL
!              DISTRIBUTION).
!
!     REFERENCE--COHEN AND WHITTEN, "PARAMETER ESTIMATION IN RELIABILITY
!                AND LIFE SPAN MODELS", MARCEL DEKKER, INC., PP. 341-342.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/6
!     ORIGINAL VERSION--JUNE      2012
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION C
      DOUBLE PRECISION D1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      WEIMO2=(C - 3.0D0*B*A + 2.0D0*A**3)/(B - A**2)**1.5 - D1
!
      RETURN
      END FUNCTION WEIMO2
      SUBROUTINE WEIPDF(X,GAMMA,MINMAX,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE WEIBULL
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THERE ARE 2 SUCH WEIBULL FAMILIES--
!                 ONE FOR THE MIN ORDER STAT (THE USUAL) AND
!                 ONE FOR THE MAX ORDER STAT.
!              (SEE SARHAN & GREENBERG, PAGE 69)
!              THE WEIBULL TYPE IS SPECIFIED VIA   MINMAX
!              FOR MINMAX = 1  (FOR THE DEFAULT MINIMUM)
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL POSITIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!              FOR MINMAX = 2 (FOR THE MAXIMUM),
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL NEGATIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = ...
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 124.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--87.7
!     ORIGINAL VERSION--NOVEMBER  1987.
!     UPDATED         --MAY       1992. REWRITTEN--ADD WEIB/MAX DIST.
!     UPDATED         --JANUARY   1994. ADD MINMAX ERROR MESSAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(GAMMA.LE.0.)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEIPDF IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS REWRITTEN      JANUARY 1994
      IF(MINMAX.EQ.2)THEN
         IF(X.GT.0.0)THEN
            PDF=0.0
         ELSE IF(X.EQ.0.0)THEN
            IF(GAMMA.LE.1.0)PDF=1.0
            IF(GAMMA.GT.1.0)PDF=0.0
         ELSE
            PDF=GAMMA*((-X)**(GAMMA-1.0))*EXP(-((-X)**GAMMA))
         ENDIF
!
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         IF(X.LT.0.0)THEN
            PDF=0.0
         ELSE IF(X.EQ.0.0)THEN
            IF(GAMMA.LE.1.0)PDF=1.0
            IF(GAMMA.GT.1.0)PDF=0.0
         ELSE
            PDF=GAMMA*(X**(GAMMA-1.0))*EXP(-(X**GAMMA))
         ENDIF
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEICDF--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEIPDF
      SUBROUTINE WEIPPF(P,GAMMA,MINMAX,PPF)
!CCCC MINMAX ADDED TO ABOVE ARGUMENT LIST   MAY 1993
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE WEIBULL
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THERE ARE 2 SUCH WEIBULL FAMILIES--
!                 ONE FOR THE MIN ORDER STAT (THE USUAL) AND
!                 ONE FOR THE MAX ORDER STAT.
!              (SEE SARHAN & GREENBERG, PAGE 69)
!              THE WEIBULL TYPE IS SPECIFIED VIA   MINMAX
!              FOR MINMAX = 1  (FOR THE DEFAULT MINIMUM)
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL POSITIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!              FOR MINMAX = 2 (FOR THE MAXIMUM),
!                 THE WEIBULL DISTRIBUTION USED
!                 HEREIN IS DEFINED FOR ALL NEGATIVE X,
!                 AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = ...
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 (INCLUSIVELY)
!                                AND 1.0 (EXCLUSIVELY))
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SINGLE PRECISION VALUE
!                                OF THE TAIL LENGTH PARAMETER.
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
!             VALUE PPF FOR THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!                 --P SHOULD BE BETWEEN 0.0 (INCLUSIVELY)
!                   AND 1.0 (EXCLUSIVELY).
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 124.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MAY       1993. REWRITTEN--ADD WEIB/MAX DIST.
!     UPDATED         --JANUARY   1994. ADD MINMAX ERROR MESSAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO WEIPPF IS OUTSIDE ',   &
               'THE ALLOWABLE (0,1) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEIPPF IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!CCCC THE FOLLOWING LINE WAS REWRITTEN   MAY 1993
!CCCC PPF=(-LOG(1.0-P))**(1.0/GAMMA)
!
!CCCC THE FOLLOWING SECTION WAS REWRITTEN      JANUARY 1994
      IF(MINMAX.EQ.2)THEN
         PPF=(-((LOG(1.0/P))**(1.0/GAMMA)))
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         PPF=(LOG(1.0/(1.0-P)))**(1.0/GAMMA)
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEICDF--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEIPPF
      SUBROUTINE WEIRAN(N,GAMMA,MINMAX,ISEED,X)
!CCCC MINMAX WAS ADDED TO THE ABOVE ARGUMENT LIST   MAY 1993
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE WEIBULL DISTRIBUTION
!              WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!              THE PROTOTYPE WEIBULL DISTRIBUTION USED
!              HEREIN IS DEFINED FOR ALL POSITIVE X,
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = GAMMA * (X**(GAMMA-1)) * EXP(-(X**GAMMA)).
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
!                                TAIL LENGTH PARAMETER.
!                                GAMMA SHOULD BE POSITIVE.
!                     --MINMAX = THE INTEGER VALUE
!                                IDENTIFYING THE
!                                CHOSEN WEIBULL DISTRIBUTION.
!                                1 = MIN, 2 = MAX.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE WEIBULL DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--SARHAN & GREENBERG,
!                 CONTRIBUTIONS TO ORDER STATISTICS,
!                 1962, WILEY, PAGE 69.
!               --TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 250-271.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 128.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MAY       1993. REWRITTEN--ADD WEIB/MAX DIST.
!     UPDATED         --JANUARY   1994. ADD MINMAX ERROR MESSAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO WEIRAN IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO WEIRAN IS ',   &
               'NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N WEIBULL DISTRIBUTION RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
!CCCC THE FOLLOWING WAS REWRITTEN    MAY 1993
!CCCC DO100I=1,N
!CCCC X(I)=(-LOG(1.0-X(I)))**(1.0/GAMMA)
!C100 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS REWRITTEN      JANUARY 1994
      IF(MINMAX.EQ.2)THEN
         DO 100 I=1,N
         X(I)=(-((-LOG(X(I)))**(1.0/GAMMA)))
  100    CONTINUE
      ELSE IF(MINMAX.EQ.1 .OR. MINMAX.EQ.0)THEN
         DO 200 I=1,N
         X(I)=(-LOG(1.0-X(I)))**(1.0/GAMMA)
  200    CONTINUE
      ELSE
         WRITE(ICOUT,1800)
 1800    FORMAT('*****ERROR IN WEICDF--MINMAX NOT 1 OR 2')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE WEIRAN
      SUBROUTINE WEMEAN(X,W,N,IWRITE,WMEAN,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED MEAN
!              OF THE DATA IN X WITH THE WEIGHTS IN W.  THE WEIGHTED MEAN
!              WILL BE A SINGLE PRECISION VALUE CALCULATED AS:
!
!                  SUM[i=1 to N][W(i)*X(i)]/SUM[i=1 to N][W(i)]
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--WMEAN  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED MEAN IN THE INPUT VECTOR X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/9
!     ORIGINAL VERSION--AUGUST    1988.
!     UPDATED         --JUNE      2012. SOME COSMETIC CHANGES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEME'
      ISUBN2='AN  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
      WMEAN=0.0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WEMEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED MEANS          **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        WMEAN=X(1)
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WMEAN=X(1)
      GO TO 9000
  139 CONTINUE
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED MEAN.           **
!               ************************************************
!
      DN=N
      DO 1100 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1102)I,W(I)
 1102     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DX=X(I)
        DW=W(I)
        DSUMX=DSUMX+DX*DW
        DSUMW=DSUMW+DW
 1100 CONTINUE
!
      IF(DSUMW.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1147)
 1147   FORMAT('      THE SUM OF THE WEIGHTS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WMEAN=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      WMEAN=DSUMX/DSUMW
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)N,WMEAN
 1211   FORMAT('THE WEIGHTED MEAN OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WEMEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUMX,DSUMW,WMEAN
 9014   FORMAT('IERROR,DN,DSUMX,DSUMW,WMEAN = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WEMEAN
      SUBROUTINE WEMEDI(X,W,N,IWRITE,WMED,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE WEIGHTED MEDIAN
!              OF THE DATA IN X WITH THE WEIGHTS IN W.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--WMED  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED MEDIAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COVARIANCE COEFFICIENT BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/9
!     ORIGINAL VERSION--AUGUST    1988.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
!CCCC DOUBLE PRECISION DX
!CCCC DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEME'
      ISUBN2='DI  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WEMEDI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED MEDIANS        **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEMEDI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      FOR THE WEIGHTED MEDIAN MUST BE AT LEAST 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        WMED=X(1)
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WMED=X(1)
      GO TO 9000
  139 CONTINUE
!
      SUM=0.0
      DO 145 I=1,N
        SUM=SUM+W(I)
  145 CONTINUE
      IF(SUM.EQ.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)HOLD
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)
  147   FORMAT('      IN ATTEMPTING TO COMPUTE A WEIGHTED MEDIAN,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,148)
  148   FORMAT('      THE INPUT WEIGHTS SUMMED TO 0.')
        CALL DPWRST('XXX','BUG ')
        WMED=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED MEDIAN.           **
!               ************************************************
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('       WEIGHED MEDIAN NOT YET IMPLEMENTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('     (AMBIGUITY IN DEFINITION)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
!CCCC IF(IFEEDB.EQ.'OFF')GO TO 1290
!CCCC IF(IWRITE.EQ.'OFF')GO TO 1290
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1211)N,WMED
!1211 FORMAT('THE WEIGHTED MEDIAN OF THE ',I8,
!CCCC1' OBSERVATIONS = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!1290 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WEMEDI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IERROR,WMED
 9015   FORMAT('IERROR,WMED = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WEMEDI
      SUBROUTINE WEOSME(X,W,N,IWRITE,WMEAN,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED ORDER
!              STATISTIC MEAN OF THE DATA IN X WITH THE WEIGHTS IN W.
!              THE WEIGHTED MEAN WILL BE A SINGLE PRECISION VALUE
!              CALCULATED AS:
!
!                  SUM[i=1 to N][W(i)*X(i)]/SUM[i=1 to N][W(i)]
!
!              NOTE THAT THE VALUES IN X ARE SORTED BEFORE THE ABOVE
!              FORMULA IS APPLIED.  HOWEVER, THE WEIGHTS ARE NOT SORTED.
!              THIS SORTING OF THE X VALUES IS THE DISTINCTION BETWEEM
!              THIS STATISTIC AND THE REGULAR WEIGHTED MEAN.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--WMEAN  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED MEAN IN THE INPUT VECTOR X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/10
!     ORIGINAL VERSION--OCTOBER   2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEME'
      ISUBN2='AN  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
      WMEAN=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'OSME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WEOSME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED MEANS          **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED ORDER STATISTIC MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        WMEAN=X(1)
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED MEAN.           **
!               ************************************************
!
      CALL SORT(X,N,X)
!
      DN=N
      DO 1100 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1102)I,W(I)
 1102     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DX=X(I)
        DW=W(I)
        DSUMX=DSUMX+DX*DW
        DSUMW=DSUMW+DW
 1100 CONTINUE
!
      IF(DSUMW.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1147)
 1147   FORMAT('      THE SUM OF THE WEIGHTS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WMEAN=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      WMEAN=DSUMX/DSUMW
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)N,WMEAN
 1211   FORMAT('THE WEIGHTED MEAN OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'OSME')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WEOSME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUMX,DSUMW,WMEAN
 9014   FORMAT('IERROR,DN,DSUMX,DSUMW,WMEAN = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WEOSME
      SUBROUTINE WESD(X,W,N,IWRITE,WSD,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE WEIGHTED STANDARD DEVIATION
!              OF THE DATA IN X WITH THE WEIGHTS IN W.
!              THE SAMPLE COVARIANCE COEFFICIENT WILL BE A SINGLE
!              PRECISION VALUE CALCULATED AS THE
!              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--WSD  = THE SINGLE PRECISION VALUE OF THE
!                              COMPUTED SAMPLE WEIGHTED STANDARD DEVIATION.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE
!             WEIGHTED STANDARD DEVIATION.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/9
!     ORIGINAL VERSION--AUGUST    1988.
!     UPDATED         --APRIL     1992. DEFINE DMEAN
!     UPDATED         --DECEMBER  1992. FIX DMEAN AND ** BUGS
!     UPDATED         --DECEMBER  1994. FIX FORMULA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!CCCC THE FOLLOWING LINE WAS ADDED DECEMBER 1992
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DDEL
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DVAR
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WESD'
      ISUBN2='    '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WESD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE WEIGHTED STANDARD DEVIATIONS **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED STANDARD DEVIATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING IN WEIGHTED STANDARD DEVIATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THE NUMBER OF OBSERVATIONS IS EQUAL TO 1.')
        CALL DPWRST('XXX','BUG ')
        WSD=0.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WSD=0.0
      GO TO 9000
  139 CONTINUE
!
!CCCC DECEMBER 1994.  UPDATE FOLLOWING LOOP TO:
!CCCC 1) CHECK FOR NEGATIVE WEIGHTS (THIS IS AN ERROR CONDITION)
!CCCC 2) COUNT THE NUMBER OF ZERO WEIGHTS
!
      NUMZER=0
      SUM=0.0
      DO 145 I=1,N
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,141)
  141     FORMAT('      NEGATIVE WEIGHT ENCOUNTERED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)I,W(I)
  142     FORMAT('      WEIGHT ',I7,' =  ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          WSD=0.0
          GO TO 9000
        ENDIF
        IF(W(I).EQ.0.0)NUMZER=NUMZER+1
        SUM=SUM+W(I)
  145 CONTINUE
!
      IF(SUM.EQ.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,148)
  148   FORMAT('      THE WEIGHTS SUMMED TO 0.')
        CALL DPWRST('XXX','BUG ')
        WSD=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE WEIGHTED STANDARD DEVIATION.  **
!               ************************************************
!
      DN=N
      DSUMX=0.0D0
      DSUMW=0.0D0
      DO 1100 I=1,N
        DX=X(I)
        DW=W(I)
        DSUMX=DSUMX+DX*DW
        DSUMW=DSUMW+DW
 1100 CONTINUE
!
!CCCC THE FOLLOWING LINE WAS FIXED    APRIL 1992
!CCCC WMEAN=DSUMX/DSUMW
      DMEAN=DSUMX/DSUMW
!CCCC THE FOLLOWING LINE WAS ADDED    APRIL 1992
      WMEAN=DMEAN
!CCCC THE FOLLOWING LINE WAS ADDED    DECEMBER 1994
      WADJ=DSUMW/REAL(N-NUMZER)
!
      DSUMX=0.0D0
      DO 1300 I=1,N
        DX=X(I)
        DDEL=DX-DMEAN
        DW=W(I)
!CCCC   THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC   DSUMX=DSUMX+DW*DDEL
        DSUMX=DSUMX+DW*DDEL**2
 1300 CONTINUE
!
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC DDENOM=N-1
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1994
!CCCC DDENOM=DSUMW-1.0D0
      DDENOM=WADJ*REAL(N-NUMZER-1)
      IF(DDENOM.EQ.0.0D0)DVAR=0.0
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC IF(DDENOM.NE.0.0)DVAR=DSUMX/DDENOM
      IF(DDENOM.NE.0.0D0)DVAR=DSUMX/DDENOM
      WSD=0.0
      IF(DVAR.GT.0.0D0)WSD=DSQRT(DVAR)
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)N,WSD
 1211   FORMAT('THE WEIGHTED STANDARD DEVIATION OF THE ',I8,   &
               ' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WESD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DN,DSUMX,DSUMW
 9014   FORMAT('DN,DSUMX,DSUMW = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DDEL,DDENOM,DMEAN,DVAR
 9015   FORMAT('DDEL,DDENOM,DMEAN,DVAR = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)WSD
 9016   FORMAT('WSD = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WESD
      SUBROUTINE WESKEW(X,W,N,IWRITE,WSKEW,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED SKEWNESS
!              OF THE DATA IN X WITH THE WEIGHTS IN W.  THE WEIGHTED
!              SKEWNESS IS COMPUTED AS:
!
!                  SKEWNESS = M3/M2**(3/2)
!
!              WHERE
!
!                  M3 = SUM[i=1 to N][W(i)*(X(i) - XMEAN)**3]
!                  M2 = SUM[i=1 to N][W(i)*(X(i) - XMEAN)**2]
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--WMEAN  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED SKEWNESS.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED SKEWNESS IN THE INPUT VECTOR X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--WEMEAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/4
!     ORIGINAL VERSION--APRIL     2014.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRIT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSKEW
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WESK'
      ISUBN2='EW  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
      WSKEW=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKEW')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WESKEW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED MEANS          **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED SKEWNESS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        WSKEW=0.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WSKEW=0.0
      GO TO 9000
  139 CONTINUE
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED SKEWNESS        **
!               ************************************************
!
      IWRIT2='OFF'
      CALL WEMEAN(X,W,N,IWRIT2,WMEAN,IBUGA3,IERROR)
      DMEAN=DBLE(WMEAN)
      DN=N
      DO 1100 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1102)I,W(I)
 1102     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DX=X(I)
        DW=W(I)
        DSUM1=DSUM1 + DW*(DX-DMEAN)**3
        DSUM2=DSUM2 + DW*(DX-DMEAN)**2
 1100 CONTINUE
!
      DSKEW=(DSUM1/DN)/(DSUM2/DN)**(3.0D0/2.0D0)
      WSKEW=REAL(DSKEW)
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)N,WSKEW
 1211   FORMAT('THE WEIGHTED SKEWNESS OF THE ',I8,' OBSERVATIONS = ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WESKEW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUM1,DSUM2,DMEAN
 9014   FORMAT('IERROR,DN,DSUM1,DSUM2,DMEAN = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WESKEW
      SUBROUTINE WESKE2(XLOW,XHIGH,W,N,IWRITE,WSKEW,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE SKEWNESS FOR THE CASE
!              OF UNEQUAL BIN SIZES.  THE SKEWNESS IS COMPUTED AS:
!
!                  SKEWNESS = M3/M2**(3/2)
!
!              WHERE
!
!                  M3 = SUM[i=1 to N][W(i)*(X(i) - XMEAN)**3]
!                  M2 = SUM[i=1 to N][W(i)*(X(i) - XMEAN)**2]
!                  X(i) = BIN MID-POINT
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--WMEAN  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED SKEWNESS.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED SKEWNESS IN THE INPUT VECTOR X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--WEMEAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/4
!     ORIGINAL VERSION--APRIL     2014.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSKEW
!
      DIMENSION XLOW(*)
      DIMENSION XHIGH(*)
      DIMENSION W(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WESK'
      ISUBN2='E2  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
      WSKEW=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKEW')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WESKEW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,XLOW(I),XHIGH(I),W(I)
   56     FORMAT('I,XLOW(I),XHIGH(I),W(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED MEANS          **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED SKEWNESS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        WSKEW=0.0
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED SKEWNESS        **
!               ************************************************
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 1010 I=1,N
        DW=W(I)
        DX=(XLOW(I) + XHIGH(I))/2.0
        DSUM1=DSUM1 + DX*DW
        DSUM2=DSUM2 + DW
 1010 CONTINUE
      DMEAN=DSUM2/DSUM1
      DN=N
!
      DO 1100 I=1,N
!
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1102)I,W(I)
 1102     FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',G15.7,').')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DX=(XLOW(I) + XHIGH(I))/2.0
        DW=W(I)
        DSUM1=DSUM1 + DW*(DX-DMEAN)**3
        DSUM2=DSUM2 + DW*(DX-DMEAN)**2
 1100 CONTINUE
!
      DSKEW=(DSUM1/DN)/(DSUM2/DN)**(3.0D0/2.0D0)
      WSKEW=REAL(DSKEW)
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)N,WSKEW
 1211   FORMAT('THE WEIGHTED SKEWNESS OF THE ',I8,' OBSERVATIONS = ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WESKEW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUM1,DSUM2,DMEAN
 9014   FORMAT('IERROR,DN,DSUM1,DSUM2,DMEAN = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WESKE2
      SUBROUTINE WESUM(X,W,N,IFLAG,IWRITE,WSUM,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES ONE OF THE FOLLOWIG:
!
!                 IFLAG = 1  => THE SAMPLE WEIGHTED SUM
!                               SUM[i=1 to N][W(i)*X(i)]
!                 IFLAG = 2  => THE SAMPLE WEIGHTED SUM OF SQUARES
!                               SUM[i=1 to N][W(i)*X(i)**2]
!                 IFLAG = 3  => THE SAMPLE WEIGHTED SUM OF ABSOLUTE
!                               VALUES
!                               SUM[i=1 to N][W(i)*|X(i)|]
!                 IFLAG = 4  => THE SAMPLE WEIGHTED AVERAGE OF ABSOLUTE
!                               VALUES
!                               SUM[i=1 to N][W(i)*|X(i)|]/
!                               SUM[i=1 to N][W(i)]
!                 IFLAG = 5  => THE SAMPLE WEIGHTED SUM OF DEVIATIONS
!                               FROM MEAN
!                               SUM[i=1 to N][W(i)*(X(i) - XBAR)]
!                 IFLAG = 6  => THE SAMPLE WEIGHTED SUM OF SQUARED DEVIATIONS
!                               FROM MEAN
!                               SUM[i=1 to N][W(i)*(X(i) - XBAR)**2]
!
!              OF THE DATA IN X WITH THE WEIGHTS IN W.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--WSUM   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED SUM.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED SUM IN THE INPUT VECTOR X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/6
!     ORIGINAL VERSION--JUNE      2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WESU'
      ISUBN2='M   '
      IERROR='NO'
!
      DN=DBLE(N)
      DSUMX=0.0D0
      DSUMW=0.0D0
      WSUM=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ESUM')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WESUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IFLAG,N
   52   FORMAT('IBUGA3,ISUBRO,IFLAG,N = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE      WEIGHTED SUMS           **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFLAG.EQ.1)THEN
          WRITE(ICOUT,111)
  111     FORMAT('***** ERROR IN WEIGHTED SUM--')
        ELSEIF(IFLAG.EQ.2)THEN
          WRITE(ICOUT,112)
  112     FORMAT('***** ERROR IN WEIGHTED SUM OF SQUARES--')
        ELSEIF(IFLAG.EQ.3)THEN
          WRITE(ICOUT,113)
  113     FORMAT('***** ERROR IN WEIGHTED SUM OF ABSOLUTE VALUES--')
        ELSEIF(IFLAG.EQ.4)THEN
          WRITE(ICOUT,114)
  114     FORMAT('***** ERROR IN WEIGHTED AVERAGE OF ABSOLUTE VALUES--')
        ELSEIF(IFLAG.EQ.5)THEN
          WRITE(ICOUT,115)
  115     FORMAT('***** ERROR IN WEIGHTED SUM OF DEVIATIONS ',   &
                 'FROM THE UNWEIGHTED MEAN--')
        ELSEIF(IFLAG.EQ.6)THEN
          WRITE(ICOUT,116)
  116     FORMAT('***** ERROR IN WEIGHTED SUM OF SQUARED DEVIATIONS ',   &
                 'FROM THE UNWEIGHTED MEAN--')
        ENDIF
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IS LESS THAN ',   &
               'ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)N
  119   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE      WEIGHTED SUM.            **
!               ************************************************
!
      DN=N
      IF(IFLAG.EQ.1)THEN
        DO 1100 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
 1102       FORMAT('      ROW ',I8,' HAS A NEGATIVE WEIGHT (',   &
                   G15.7,').')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=X(I)
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1100   CONTINUE
        WSUM=DSUMX
      ELSEIF(IFLAG.EQ.2)THEN
        DO 1200 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,112)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=X(I)
          DX=DX**2
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1200   CONTINUE
        WSUM=DSUMX
      ELSEIF(IFLAG.EQ.3)THEN
        DO 1300 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,113)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=X(I)
          IF(DX.LT.0.0D0)DX=-DX
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1300   CONTINUE
        WSUM=DSUMX
!
      ELSEIF(IFLAG.EQ.4)THEN
        DO 1400 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,114)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=X(I)
          IF(DX.LT.0.0D0)DX=-DX
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1400   CONTINUE
        WSUM=DSUMX/DSUMW
      ELSEIF(IFLAG.EQ.5)THEN
        DSUMX=0.0D0
        DO 1500 I=1,N
          DSUMX=DSUMX + DBLE(X(I))
 1500   CONTINUE
        DXBAR=DSUMX/DN
        DSUMX=0.0D0
!
        DO 1510 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,115)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=DBLE(X(I)) - DXBAR
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1510   CONTINUE
        WSUM=DSUMX
      ELSEIF(IFLAG.EQ.6)THEN
        DSUMX=0.0D0
        DO 1600 I=1,N
          DSUMX=DSUMX + DBLE(X(I))
 1600   CONTINUE
        DXBAR=DSUMX/DN
        DSUMX=0.0D0
!
        DO 1610 I=1,N
!
          IF(W(I).LT.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,116)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1102)I,W(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          DX=(DBLE(X(I)) - DXBAR)**2
          DW=W(I)
          DSUMX=DSUMX+DX*DW
          DSUMW=DSUMW+DW
 1610   CONTINUE
        WSUM=DSUMX
      ENDIF
!
      IF(DSUMW.LE.0.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFLAG.EQ.1)THEN
          WRITE(ICOUT,111)
        ELSEIF(IFLAG.EQ.2)THEN
          WRITE(ICOUT,112)
        ELSEIF(IFLAG.EQ.3)THEN
          WRITE(ICOUT,113)
        ELSEIF(IFLAG.EQ.4)THEN
          WRITE(ICOUT,114)
        ELSEIF(IFLAG.EQ.5)THEN
          WRITE(ICOUT,115)
        ELSEIF(IFLAG.EQ.6)THEN
          WRITE(ICOUT,116)
        ENDIF
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1147)
 1147   FORMAT('      THE SUM OF THE WEIGHTS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WSUM=0.0
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *******************************
!               **  STEP 15--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFLAG.EQ.1)THEN
          WRITE(ICOUT,1611)N,WSUM
 1611     FORMAT('THE WEIGHTED SUM OF THE ',I8,' OBSERVATIONS = ',G15.7)
        ELSEIF(IFLAG.EQ.2)THEN
          WRITE(ICOUT,1612)N,WSUM
 1612     FORMAT('THE WEIGHTED SUM OF SQUARES OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
        ELSEIF(IFLAG.EQ.3)THEN
          WRITE(ICOUT,1613)N,WSUM
 1613     FORMAT('THE WEIGHTED SUM OF ABSOLUTE VALUES OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
        ELSEIF(IFLAG.EQ.4)THEN
          WRITE(ICOUT,1614)N,WSUM
 1614     FORMAT('THE WEIGHTED AVERAGE OF ABSOLUTE VALUES OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
        ELSEIF(IFLAG.EQ.5)THEN
          WRITE(ICOUT,1615)N,WSUM
 1615     FORMAT('THE WEIGHTED SUM OF DEVIATIONS FROM THE MEAN OF THE ',   &
                 I8,' OBSERVATIONS = ',G15.7)
        ELSEIF(IFLAG.EQ.6)THEN
          WRITE(ICOUT,1616)N,WSUM
 1616     FORMAT('THE WEIGHTED SUM OF SQUARED DEVIATIONS FROM THE ',   &
                 'MEAN OF THE ',I8,' OBSERVATIONS = ',G15.7)
        ENDIF
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ESUM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WESUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,DN,DSUMX,DSUMW,WSUM
 9014   FORMAT('IERROR,DN,DSUMX,DSUMW,WSUM = ',A4,2X,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WESUM
      SUBROUTINE WETRME(X,W,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,   &
                        XTEMP,STEP,   &
                        IUPPER,XTRIM,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WEIGHTED TRIMMED MEAN
!              OF THE DATA IN THE INPUT VECTOR X.
!      NOTE--PROP1 % OF THE DATA IS WTTRMED FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS WTTRMED FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS.
!                     --STEP   = A COMPUTED CUMULATIVE WEIGHTS
!                                VECTOR.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WTTRMED FROM LEFT SIDE.
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WTTRMED FROM RIGHT SIDE.
!     OUTPUT ARGUMENTS--XTRIM  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WTTRMED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WEIGHTED TRIMMED MEAN.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORTC.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
!               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 1967, PAGES 357, 387.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY, 1969).
!               --ADAPTED FROM CODE PROVIDED BY JAMES YEN OF THE
!                 NIST STATISITICAL ENGINEERING DIVISION.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.5
!     ORIGINAL VERSION--MAY       2003.
!     UPDATED         --OCTOBER   2012. ALLOW TRIMMING TO BE SPECIFIED
!                                       IN TERMS OF THE NUMBER OF VALUES.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
      DIMENSION W(*)
      DIMENSION STEP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TRIM'
      ISUBN2='ME  '
      IERROR='NO'
!
      DSUM=0.0D0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WTTRME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2,NTRIM1,NTRIM2
   54   FORMAT('PROP1,PROP2,NTRIM1,NTRIM2 = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),W(I)
   56     FORMAT('I,X(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************************
!               **  COMPUTE THE WEIGHTED TRIMMED MEAN  **
!               *****************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1.OR.N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WEIGHTED TRIMMED MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS MUST BE BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      1 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        XTRIM=X(1)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING IN WEIGHTED TRIMMED MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THE NUMBER OF OBSERVATIONS IS EQUAL TO 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)XTRIM
  125   FORMAT('      WEIGHTED TRIMMED MEAN SET TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     2012/10: TRIMMING MAY NOW BE SPECIFIED EITHER IN TERMS OF
!              THE PROPORTION OR IN A SPECIFIC NUMBER OF OBSERVATIONS
!              TO BE TRIMMED.
!
      IFLAG1=0
      IF(NTRIM1.GE.1)THEN
        IFLAG1=1
        IF(NTRIM1.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,147)IUPPER
  147     FORMAT('      NTRIM1 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,149)NTRIM1
  149     FORMAT('      THE VALUE OF NTRIM1 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP1=100.*REAL(NTRIM1)/REAL(N)
      ELSE
        IF(PROP1.LT.0.0 .OR. PROP1.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)
  142     FORMAT('      PROP1 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,143)PROP1
  143     FORMAT('      THE VALUE OF PROP1 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IFLAG2=0
      IF(NTRIM2.GE.1)THEN
        IFLAG2=1
        IF(NTRIM2.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,157)IUPPER
  157     FORMAT('      NTRIM2 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,159)NTRIM2
  159     FORMAT('      THE VALUE OF NTRIM2 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP2=100.*REAL(NTRIM2)/REAL(N)
      ELSE
        IF(PROP2.LT.0.0 .OR. PROP2.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,152)
  152     FORMAT('      PROP2 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,153)PROP2
  153     FORMAT('      THE VALUE OF PROP2 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE WEIGHTED TRIMMED MEAN.  **
!               ******************************************
!
      IF(IFLAG1.EQ.0)THEN
        NPROP1=INT((PROP1/100.0)*AN+0.0001)
        ISTART=NPROP1+1
      ELSE
        NPROP1=NTRIM1
        ISTART=NPROP1+1
      ENDIF
!
      IF(IFLAG2.EQ.0)THEN
        NPROP2=INT((PROP2/100.0)*AN+0.0001)
        ISTOP=N-NPROP2
      ELSE
        NPROP2=NTRIM2
        ISTOP=N-NPROP2
      ENDIF
!
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)
  252   FORMAT('      START INDEX IS HIGHER THAN STOP INDEX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,253)ISTART,ISTOP
  253   FORMAT('      ISTART,ISTOP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      A1=PROP1/100.0
      A2=PROP2/100.0
      IF(A1.LT.0.0001 .AND. A2.LT.0.0001)THEN
        CALL WEMEAN(X,W,N,IWRITE,XTRIM,IBUGA3,IERROR)
        GO TO 800
      END IF
!
      CALL SORTC(X,W,N,XTEMP,W)
!
      DSUM=0.0D0
      DO 200 I=1,N
        DSUM=DSUM+DBLE(W(I))
  200 CONTINUE
!
      W(1)=REAL(DBLE(W(1))/DSUM)
      STEP(1)=W(1)
      DO 210 I=1,N
        W(I)=REAL(DBLE(W(I))/DSUM)
        IF(I.GE.2)THEN
          STEP(I)=STEP(I-1)+W(I)
        ELSE
          STEP(I)=W(I)
        ENDIF
  210 CONTINUE
!
      IF(STEP(1).LE.A1)THEN
        W(1)=0.0
      ELSEIF(STEP(1).GE.(1.0-A2))THEN
        W(1)=1.0-(A1+A2)
      ELSE
        W(1)=STEP(1) - A1
      ENDIF
!
      DO 310 I=2,N
        IF(STEP(I-1).GE.A1 .AND. STEP(I).LE.(1.0-A2))THEN
          W(I)=W(I)
        ELSEIF(STEP(I).LE.A1 .OR. STEP(I-1).GE.(1.0-A2))THEN
          W(I)=0.0
        ELSEIF((STEP(I-1).LE.A1 .AND. STEP(I).GE.A1) .AND.   &
               STEP(I).LE.(1.0-A2))THEN
          W(I)=STEP(I)-A1
        ELSEIF((STEP(I-1).LE.(1.0-A2).AND.STEP(I).GE.(1.0-A2)).AND.   &
               STEP(I-1).GE.A1)THEN
          W(I)=(1.0-A2) - STEP(I-1)
        ELSEIF(STEP(I-1).LE.A1 .AND. STEP(I).GE.(1.0-A2))THEN
          W(I)=1.0 - (A1 + A2)
        ENDIF
  310 CONTINUE
!
      IF(STEP(N-1).GE.(1.0-A2))THEN
        W(N)=0.0
      ELSEIF(STEP(N-1).LE.A1)THEN
        W(N)=1.0-(A1 + A2)
      ELSE
        W(N)=(1.0-A2) - STEP(N-1)
      ENDIF
!
      NPROP1=INT((PROP1/100.0)*AN+0.0001)
      ISTART=NPROP1+1
!
      DSUM=0.0D0
      DO 410 I=1,N
        DSUM=DSUM + (W(I)*X(I))
  410 CONTINUE
      XTRIM=DSUM/(1.0 - (A1 + A2))
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
  800 CONTINUE
      IF(IFEEDB.EQ.'ON' .OR. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRIM
  821   FORMAT('THE WEIGHTED TRIMMED MEAN OF THE ',I8,   &
               ' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRME')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WTTRME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PROP1,PROP2
 9014   FORMAT('PROP1,PROP2 = ',23E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)XTRIM
 9018   FORMAT('XTRIM = ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WETRME
      SUBROUTINE WEVARI(X,W,N,IWRITE,WVAR,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE WEIGHTED VARIANCE
!              OF THE DATA IN X WITH THE WEIGHTS IN W.
!              THE SAMPLE COVARIANCE COEFFICIENT WILL BE A SINGLE
!              PRECISION VALUE CALCULATED AS THE
!              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --W      = THE SINGLE PRECISION VECTOR OF
!                                WEIGHTS
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--WVAR = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WEIGHTED VARIANCE.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COVARIANCE COEFFICIENT BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--88/9
!     ORIGINAL VERSION--AUGUST    1988.
!     UPDATED         --APRIL     1992. DEFINE DMEAN
!     UPDATED         --DECEMBER  1992. FIX DMEAN AND ** BUGS
!     UPDATED         --DECEMBER  1994. FIX DEFINITION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DW
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMW
!CCCC THE FOLLOWING LINE WAS ADDED DECEMBER 1992
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DDEL
      DOUBLE PRECISION DDENOM
!CCCC DOUBLE PRECISION DVAR
!
      DIMENSION X(*)
      DIMENSION W(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WEVA'
      ISUBN2='RI  '
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMW=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF WEVARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I),W(I)
   56 FORMAT('I,X(I),W(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *******************************************
!               **  COMPUTE WEIGHTED VARIANCES           **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
      IF(N.GE.1)GO TO 119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN WEVARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE WEIGHTED VARIANCE IS TO BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      COMPUTED, MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN WEVARI--',   &
      'THE THIRD INPUT ARGUMENT (N) HAS THE VALUE 1')
      CALL DPWRST('XXX','BUG ')
      WVAR=0.0
      GO TO 9000
  129 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN WEVARI--',   &
      'THE 1ST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WVAR=X(1)
      GO TO 9000
  139 CONTINUE
!
      SUM=0.0
!CCCC DECEMBER 1994.  UPDATE FOLLOWING LOOP TO:
!CCCC 1) CHECK FOR NEGATIVE WEIGHTS (THIS IS AN ERROR CONDITION)
!CCCC 2) COUNT THE NUMBER OF ZERO WEIGHTS
!
      NUMZER=0
      SUM=0.0
      DO 145 I=1,N
      IF(W(I).LT.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,142)I,W(I)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        WVAR=0.0
        GO TO 9000
      ENDIF
      IF(W(I).EQ.0.0)NUMZER=NUMZER+1
      SUM=SUM+W(I)
  145 CONTINUE
  141 FORMAT('***** FATAL ERROR IN WEVAR--NEGATIVE WEIGHT ',   &
      'ENCOUNTERED.')
  142 FORMAT('      WEIGHT ',I7,' =  ',E15.7)
      IF(SUM.NE.0.0)GO TO 149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,146)HOLD
  146 FORMAT('***** FATAL ERROR IN WEVARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,147)
  147 FORMAT('      IN ATTEMPTING TO COMPUTE A WEIGHTED ST. DEV.,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,148)
  148 FORMAT('      THE INPUT WEIGHTS SUMMED TO 0.')
      CALL DPWRST('XXX','BUG ')
      WVAR=0.0
      IERROR='YES'
      GO TO 9000
  149 CONTINUE
!
!               ************************************************
!               **  STEP 11--                                 **
!               **  COMPUTE THE WEIGHTED VARIANCE.            **
!               ************************************************
!
      DN=N
      DSUMX=0.0D0
      DO 1100 I=1,N
      DX=X(I)
      DW=W(I)
      DSUMX=DSUMX+DX*DW
 1100 CONTINUE
!
      DSUMW=0.0D0
      DO 1200 I=1,N
      DW=W(I)
      DSUMW=DSUMW+DW
 1200 CONTINUE
!
!CCCC THE FOLLOWING LINE WAS FIXED    APRIL 1992
!CCCC WMEAN=DSUMX/DSUMW
      DMEAN=DSUMX/DSUMW
!CCCC THE FOLLOWING LINE WAS ADDED    APRIL 1992
      WMEAN=DMEAN
!CCCC THE FOLLOWING LINE WAS ADDED    DECEMBER 1994
      WADJ=DSUMW/REAL(N-NUMZER)
!
      DSUMX=0.0D0
      DO 1300 I=1,N
      DX=X(I)
      DDEL=DX-DMEAN
      DW=W(I)
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC DSUMX=DSUMX+DW*DDEL
      DSUMX=DSUMX+DW*DDEL**2
 1300 CONTINUE
!
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC DDENOM=N-1
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1994
!CCCC DDENOM=DSUMW-1.0D0
      DDENOM=WADJ*REAL(N-NUMZER-1)
      IF(DDENOM.EQ.0.0D0)WVAR=0.0
!CCCC THE FOLLOWING LINE WAS CHANGED DECEMBER 1992
!CCCC IF(DDENOM.NE.0.0)WVAR=DSUMX/DDENOM
      IF(DDENOM.NE.0.0D0)WVAR=DSUMX/DDENOM
!
!               *******************************
!               **  STEP 12--                **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 1290
      IF(IWRITE.EQ.'OFF')GO TO 1290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)N,WVAR
 1211 FORMAT('THE WEIGHTED VARIANCE OF THE ',I8,   &
      ' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1290 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF WEVARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
!CCCC THE FOLLOWING 2 LINES WERE AUGMENTED DECEMBER 1992
!CCCC WRITE(ICOUT,9014)DN,DSUMX,DSUMW,DDEL
!9014 FORMAT('DN,DSUMX,DSUMW,DDEL = ',4D15.7)
!CCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DN,DSUMX,DSUMW
 9014 FORMAT('DN,DSUMX,DSUMW = ',3D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)DDEL,DDENOM,DMEAN
 9015 FORMAT('DDEL,DDENOM,DMEAN = ',3D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)WVAR
 9016 FORMAT('WVAR = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE WEVARI
      function whimed(a,iw,n,acand,iwcand)
!c
!c  Algorithm to compute the weighted high median in O(n) time.
!c
!c  The whimed is defined as the smallest a(j) such that the sum
!c  of the weights of all a(i) <= a(j) is strictly greater than
!c  half of the total weight.
!c
!c  Parameters of this function:
!c        a: real array containing the observations
!c        n: number of observations
!c       iw: array of integer weights of the observations.
!c
!c  This function uses the function pull.
!c
!c  The size of acand, iwcand must be at least n.
!c
      dimension a(*),iw(*)
!cccc dimension acand(500),iwcand(500)
      dimension acand(*),iwcand(*)
      integer wtotal,wrest,wleft,wmid,wright
!
      nn=n
      wtotal=0
      do 20 i=1,nn
          wtotal=wtotal+iw(i)
20    continue
      wrest=0
100   continue
      trial=pull(a,nn,nn/2+1,acand)
      do 22 i=1,n
        acand(i)=0.0
22    continue
      wleft=0
      wmid=0
      wright=0
      do 30 i=1,nn
          if (a(i).lt.trial) then
              wleft=wleft+iw(i)
          else
              if (a(i).gt.trial) then
                  wright=wright+iw(i)
              else
                  wmid=wmid+iw(i)
              endif
          endif
30    continue
      if ((2*wrest+2*wleft).gt.wtotal) then
          kcand=0
          do 40 i=1,nn
              if (a(i).lt.trial) then
                  kcand=kcand+1
                  acand(kcand)=a(i)
                  iwcand(kcand)=iw(i)
              endif
40        continue
          nn=kcand
      else
          if ((2*wrest+2*wleft+2*wmid).gt.wtotal) then
              whimed=trial
              return
          else
              kcand=0
              do 50 i=1,nn
                  if(a(i).gt.trial) then
                      kcand=kcand+1
                      acand(kcand)=a(i)
                      iwcand(kcand)=iw(i)
                  endif
50            continue
              nn=kcand
              wrest=wrest+wleft+wmid
          endif
      endif
      do 60 i=1,nn
          a(i)=acand(i)
          iw(i)=iwcand(i)
60    continue
      go to 100
      end
      SUBROUTINE WINDME(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,   &
                        XTEMP,IUPPER,XWIND,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE WINSORIZED MEAN = THE
!              SAMPLE (ON EACH SIDE) WINDSORIZED MEAN OF THE DATA IN THE
!              INPUT VECTOR X.
!      NOTE--PROP1 % OF THE DATA IS WINDSORIZED FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS WINDSORIZED FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WINDSORIZED FROM LEFT
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WINDSORIZED FROM RIGHT
!     OUTPUT ARGUMENTS--XWIND  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE WINDSORIZED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE WINDSORIZED MEAN.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
!               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 1967, PAGES 357, 387.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY, 1969).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION--JULY      1973.
!     UPDATED         --OCTOBER   2012. ALLOW TRIMMING TO BE SPECIFIED
!                                       IN TERMS OF THE NUMBER OF VALUES.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DK
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WIND'
      ISUBN2='ME  '
      IERROR='NO'
!
      NPROP1=0
      NPROP2=0
      NPROP3=0
      ISTART=0
      ISTOP=0
      DSUM=0.0D0
      DK=0.0D0
      PROP3=0.0
      XWIND=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NDME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WINDME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NTRIM1,NTRIM2
   52   FORMAT('IBUGA3,ISUBRO,N,NTRIM1,NTRIM2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2
   54   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  COMPUTE THE WINSORIZED MEAN  **
!               ***********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
      IF(N.EQ.1)THEN
        XWIND=X(1)
        IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,91)
   91     FORMAT('DATA HAS ONLY A SINGLE OBSERVATION.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,93)XWIND
   93     FORMAT('THE WINSORIZED MEAN SET EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WINSORIZED MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS MUST BE BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      1 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      XWIND=HOLD
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)
  137   FORMAT('ALL DATA VALUES HAVE THE SAME VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)XWIND
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  139 CONTINUE
!
!     2012/10: TRIMMING MAY NOW BE SPECIFIED EITHER IN TERMS OF
!              THE PROPORTION OR IN A SPECIFIC NUMBER OF OBSERVATIONS
!              TO BE TRIMMED.
!
      IFLAG1=0
      IF(NTRIM1.GE.1)THEN
        IFLAG1=1
        IF(NTRIM1.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,147)IUPPER
  147     FORMAT('      NTRIM1 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,149)NTRIM1
  149     FORMAT('      THE VALUE OF NTRIM1 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP1=100.*REAL(NTRIM1)/REAL(N)
      ELSE
        IF(PROP1.LT.0.0 .OR. PROP1.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)
  142     FORMAT('      PROP1 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,143)PROP1
  143     FORMAT('      THE VALUE OF PROP1 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IFLAG2=0
      IF(NTRIM2.GE.1)THEN
        IFLAG2=1
        IF(NTRIM2.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,157)IUPPER
  157     FORMAT('      NTRIM2 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,159)NTRIM2
  159     FORMAT('      THE VALUE OF NTRIM2 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP2=100.*REAL(NTRIM2)/REAL(N)
      ELSE
        IF(PROP2.LT.0.0 .OR. PROP2.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,152)
  152     FORMAT('      PROP2 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,153)PROP2
  153     FORMAT('      THE VALUE OF PROP2 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               *************************************
!               **  STEP 2--                       **
!               **  COMPUTE THE WINDSORIZED MEAN.  **
!               *************************************
!
      CALL SORT(X,N,XTEMP)
!
      IF(IFLAG1.EQ.0)THEN
        NPROP1=INT((PROP1/100.0)*AN+0.0001)
        ISTART=NPROP1+1
      ELSE
        NPROP1=NTRIM1
        ISTART=NPROP1+1
      ENDIF
!
      IF(IFLAG2.EQ.0)THEN
        NPROP2=INT((PROP2/100.0)*AN+0.0001)
        ISTOP=N-NPROP2
      ELSE
        NPROP2=NTRIM2
        ISTOP=N-NPROP2
      ENDIF
!
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)
  252   FORMAT('      START INDEX IS HIGHER THAN STOP INDEX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,253)ISTART,ISTOP
  253   FORMAT('      ISTART,ISTOP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      DSUM=0.0
      DO 200 I=1,N
        IF(I.LT.ISTART)THEN
          DX=XTEMP(ISTART)
          DSUM=DSUM+DX
        ELSEIF(I.GT.ISTOP)THEN
          DX=XTEMP(ISTOP)
          DSUM=DSUM+DX
        ELSE
          DX=XTEMP(I)
          DSUM=DSUM+DX
        ENDIF
  200 CONTINUE
      DK=AN
      XWIND=DSUM/DK
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        PROP3=100.00-PROP1-PROP2
        NPROP3=N-NPROP1-NPROP2
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE WINSORIZED   FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE WINDSORIZED   FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA REMAINING IN MIDDLE BEFORE WINSORIZING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XWIND
  821   FORMAT('THE WINSORIZED MEAN OF THE ',I8,' OBSERVATIONS = ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NDME')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WINDME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PROP1,PROP2,PROP3
 9014   FORMAT('PROP1,PROP2,PROP3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)N,NPROP1,NPROP2,NPROP3
 9015   FORMAT('N,NPROP1,NPROP2,NPROP3 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISTART,ISTOP,DSUM,DK
 9016   FORMAT('ISTART,ISTOP,DSUM,DK = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IERROR,XWIND
 9018   FORMAT('IERROR,XWIND = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WINDME
      SUBROUTINE WINSOR(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,   &
                        XTEMP,IUPPER,Y,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE WINSORIZES THE DATA
!      NOTE--PROP1 % OF THE DATA IS WINSORIZED FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS WINSORIZED FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WINSORIZED FROM LEFT
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE WINSORIZED FROM RIGHT
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF THE
!                                COMPUTED SAMPLE WINSORIZED DATA.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF
!             SAMPLE WINSORIZED DATA.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
!               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 1967, PAGES 357, 387.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY, 1969).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002.7
!     ORIGINAL VERSION--JULY      2002.
!     UPDATED         --OCTOBER   2012. ALLOW WINSORIZING TO BE SPECIFIED
!                                       IN TERMS OF THE NUMBER OF VALUES.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='WINS'
      ISUBN2='OR  '
      IERROR='NO'
!
      NPROP1=0
      NPROP2=0
      NPROP3=0
      ISTART=0
      ISTOP=0
      PROP3=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NSOR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF WINSOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NTRIM1,NTRIM2
   52   FORMAT('IBUGA3,ISUBRO,N,NTRIM1,NTRIM2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2
   54   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  PERFORM THE WINSORIZATION    **
!               ***********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
      IF(N.EQ.1)THEN
        Y(1)=X(1)
        IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,91)
   91     FORMAT('DATA HAS ONLY A SINGLE OBSERVATION.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,93)XWIND
   93     FORMAT('THE WINSORIZED VALUES SET EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN WINSOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS MUST BE BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      1 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      DO 136 I=1,N
        Y(I)=X(I)
  136 CONTINUE
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)
  137   FORMAT('ALL DATA VALUES HAVE THE SAME VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,138)
  138   FORMAT('WINSORIZED VALUES SET EQUAL TO INPUT VALUES.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  139 CONTINUE
!
!     2012/10: TRIMMING MAY NOW BE SPECIFIED EITHER IN TERMS OF
!              THE PROPORTION OR IN A SPECIFIC NUMBER OF OBSERVATIONS
!              TO BE TRIMMED.
!
      IFLAG1=0
      IF(NTRIM1.GE.1)THEN
        IFLAG1=1
        IF(NTRIM1.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,147)IUPPER
  147     FORMAT('      NTRIM1 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,149)NTRIM1
  149     FORMAT('      THE VALUE OF NTRIM1 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP1=100.*REAL(NTRIM1)/REAL(N)
      ELSE
        IF(PROP1.LT.0.0 .OR. PROP1.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)
  142     FORMAT('      PROP1 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,143)PROP1
  143     FORMAT('      THE VALUE OF PROP1 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IFLAG2=0
      IF(NTRIM2.GE.1)THEN
        IFLAG2=1
        IF(NTRIM2.GT.IUPPER)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,157)IUPPER
  157     FORMAT('      NTRIM2 MUST BE LESS THAN OR EQUAL TO ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,159)NTRIM2
  159     FORMAT('      THE VALUE OF NTRIM2 IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        PROP2=100.*REAL(NTRIM2)/REAL(N)
      ELSE
        IF(PROP2.LT.0.0 .OR. PROP2.GT.100.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,152)
  152     FORMAT('      PROP2 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,153)PROP2
  153     FORMAT('      THE VALUE OF PROP2 IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  COMPUTE THE WINSORIZED VARIABLE.  **
!               ****************************************
!
      CALL SORT(X,N,XTEMP)
!
      IF(IFLAG1.EQ.0)THEN
        NPROP1=INT((PROP1/100.0)*AN+0.0001)
        ISTART=NPROP1+1
      ELSE
        NPROP1=NTRIM1
        ISTART=NPROP1+1
      ENDIF
!
      IF(IFLAG2.EQ.0)THEN
        NPROP2=INT((PROP2/100.0)*AN+0.0001)
        ISTOP=N-NPROP2
      ELSE
        NPROP2=NTRIM2
        ISTOP=N-NPROP2
      ENDIF
!
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)
  252   FORMAT('      START INDEX IS HIGHER THAN STOP INDEX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,253)ISTART,ISTOP
  253   FORMAT('      ISTART,ISTOP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      ALOW=XTEMP(ISTART)
      AHIGH=XTEMP(ISTOP)
!
      DO 200 I=1,N
        IF(X(I).LT.ALOW)THEN
          Y(I)=ALOW
        ELSEIF(X(I).GT.AHIGH)THEN
          Y(I)=AHIGH
        ELSE
          Y(I)=X(I)
        ENDIF
  200 CONTINUE
!
!               *******************************
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        PROP3=100.00-PROP1-PROP2
        NPROP3=N-NPROP1-NPROP2
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE WINSORIZED         FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE WINSORIZED         FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA REMAINING IN MIDDLE BEFORE WINSORIZING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NSO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WINSOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)N,PROP1,PROP2,PROP3
 9014   FORMAT('N,PROP1,PROP2,PROP3 = ',I8,3E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NPROP1,NPROP2,NPROP3
 9015   FORMAT('NPROP1,NPROP2,NPROP3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISTART,ISTOP
 9016   FORMAT('ISTART,ISTOP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9017 I=1,N
          WRITE(ICOUT,9018)XTEMP(I),Y(I)
 9018     FORMAT('XTEMP(I),Y = ',2E15.7)
          CALL DPWRST('XXX','BUG ')
 9017   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE WINSOR
      SUBROUTINE WINSTA(Y1,Y2,Y3,N,NUMV,ICASS7,ISTARA,MAXNXT,   &
                        ISEED,NSIZE,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        YOUT,NOUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE A "WINDOW" STATISTIC.  ALTHOUGH THIS IS TYPICALLY
!              USED FOR A LOCATION STATISTIC, IN CAN BE USED FOR ANY
!              SUPPORTED STATISTIC.  THIS IS INTENDED FOR VERY LARGE
!              DATA SETS.  WE TAKE USER SPECIFIED INTERVALS OF THE DATA
!              AND COMPUTE THE SPECIFIED STATISTIC FOR EACH OF THESE
!              INTERVALS.  FOR EXAMPLE, WE CAN COMPUTE THE MEAN FOR
!              POINTS 1 TO 1,000, THEN FOR POINTS 1,001 TO 2,000, AND
!              SO ON.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2016/06
!     ORIGINAL VERSION--JUNE        2016.
!     ORIGINAL VERSION--AUGUST      2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
      CHARACTER*4 ICASC2
      CHARACTER*4 ICASS7
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION YOUT(*)
!
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='MOVS'
      ISUBN2='TA  '
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NSTA')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF WINSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASCT,ICASC2,ICASS7,N,NSIZE
   71   FORMAT('ICASCT,ICASC2,ICASS7,N,NSIZE = ',   &
               3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 75 I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),Y3(I)
   73     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN WINDOW <STAT> COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  CASE 1: DIRECTION = CENTER                      **
!               ******************************************************
!
      NOUT=0
      ICNT=0
      NSTRT=1
!
      DO 1010 I=1,N
        ICNT=ICNT+1
        IF(ICNT.GE.NSIZE .OR. I.EQ.N)THEN
          NSTOP=I
          NTEMP=NSTOP-NSTRT+1
          CALL CMPSTA(Y1(NSTRT),Y2(NSTRT),Y3(NSTRT),TEMP1,TEMP2,TEMP3,   &
                      TEMP4,TEMP5,   &
                      MAXNXT,NTEMP,NTEMP,NTEMP,NUMV,ICASS7,ISTARA,   &
                      ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      STAT,   &
                      ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          NOUT=NOUT+1
          YOUT(NOUT)=STAT
          ICNT=0
          NSTRT=I+1
        ENDIF
 1010 CONTINUE
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NSTA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF WINSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NOUT
 9013   FORMAT('NOUT = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NOUT.GE.1)THEN
          DO 9021 I=1,NOUT
            WRITE(ICOUT,9023)I,YOUT(I)
 9023       FORMAT('I,YOUT(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE WINSTA
      SUBROUTINE WNOPEN(I1,I2,I3,I4)
!     THIS IS A DUMMY SUBROUTINE TO BE "USED"
!     IN PLACE OF THE WNOPEN SUBROUTINE
!     IN THE OTG INTERACTOR WINDOW MANAGER LIBRARY.
!     IF YOU DO HAVE THAT LIBRARY, THEN DELETE THIS DUMMY SUBROUTINE.
!     IF YOU DO NOT HAVE THAT LIBRARY, THEN LEAVE THIS DUMMY
!     SUBROUTINE IN SO AS TO AVOID A MISSING EXTERNAL REFERENCE
!     AT LINK TIME.
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'OPEN')THEN
        WRITE(ICOUT,52)I1,I2,I3,I4
   52   FORMAT('I1,I2,I3,I4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WNOPEN
      SUBROUTINE WNCLOS(IJUNK)
!     THIS IS A DUMMY SUBROUTINE TO BE "USED"
!     IN PLACE OF THE WNCLOS SUBROUTINE
!     IN THE OTG INTERACTOR WINDOW MANAGER LIBRARY.
!     IF YOU DO HAVE THAT LIBRARY, THEN DELETE THIS DUMMY SUBROUTINE.
!     IF YOU DO NOT HAVE THAT LIBRARY, THEN LEAVE THIS DUMMY
!     SUBROUTINE IN SO AS TO AVOID A MISSING EXTERNAL REFERENCE
!     AT LINK TIME.
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'OPEN')THEN
        WRITE(ICOUT,52)IJUNK
   52   FORMAT('IJUNK = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE WNCLOS
      SUBROUTINE WSHRT(D, N, NP, NNP, SB, SA, ISEED)
!
!     ALGORITHM AS 53  APPL. STATIST. (1972) VOL.21, NO.3
!
!     Wishart variate generator.  On output, SA is an upper-triangular
!     matrix of size NP * NP (written in linear form, column ordered)
!     whose elements have a Wishart(N, SIGMA) distribution.
!
!     D is an upper-triangular array such that SIGMA = D'D (see AS 6)
!
!     Auxiliary function required: a random no. generator called RAND.
!     The Wichmann & Hill generator is included here.   It should be
!     initialized in the calling program.
!
      INTEGER N, NP, NNP
      REAL D(NNP), SB(NNP), SA(NNP)
!
!     Local variables
!
      INTEGER K, NS, I, J, NR, IP, NQ, II
      REAL DF, U1, U2, RN, C
      REAL ZERO, ONE, TWO, NINE
      DATA ZERO /0.0/, ONE /1.0/, TWO /2.0/, NINE /9.0/
!
      K = 1
    1 CONTINUE
      CALL RNORM(U1, U2, ISEED)
!
!     Load SB with independent normal (0, 1) variates
!
      SB(K) = U1
      K = K + 1
      IF (K .GT. NNP) GO TO 2
      SB(K) = U2
      K = K + 1
      IF (K .LE. NNP) GO TO 1
    2 NS = 0
!
!     Load diagonal elements with square root of chi-square variates
!
      DO 3 I = 1, NP
        DF = N - I + 1
        NS = NS + I
        U1 = TWO / (NINE * DF)
        U2 = ONE - U1
        U1 = SQRT(U1)
!
!     Wilson-Hilferty formula for approximating chi-square variates
!
        SB(NS) = SQRT(DF * (U2 + SB(NS) * U1)**3)
    3 CONTINUE
!
      RN = N
      NR = 1
      DO 5 I = 1, NP
        NR = NR + I - 1
        DO 55 J = I, NP
          IP = NR
          NQ = (J*J - J) / 2 + I - 1
          C = ZERO
          DO 4 K = I, J
            IP = IP + K - 1
            NQ = NQ + 1
            C = C + SB(IP) * D(NQ)
    4     CONTINUE
          SA(IP) = C
   55   CONTINUE
    5 CONTINUE
!
      DO 7 I = 1, NP
        II = NP - I + 1
        NQ = NNP - NP
        DO 8 J = 1, I
          IP = (II*II - II) / 2
          C = ZERO
          DO 6 K = I, NP
            IP = IP + 1
            NQ = NQ + 1
            C = C + SA(IP) * SA(NQ)
    6     CONTINUE
          SA(NQ) = C / RN
          NQ = NQ - 2 * NP + I + J - 1
    8   CONTINUE
    7 CONTINUE
!
      RETURN
      END SUBROUTINE WSHRT
      DOUBLE PRECISION FUNCTION Y0INT(XVALUE)
!
!   DESCRIPTION:
!
!      This function calculates the integral of the Bessel
!      function Y0, defined as
!
!        Y0INT(x) = {integral 0 to x} Y0(t) dt
!
!      The code uses Chebyshev expansions whose coefficients are
!      given to 20 decimal places.
!
!
!   ERROR RETURNS:
!
!      If x < 0.0, the function is undefined. An error message
!      is printed and the function returns the value 0.0.
!
!      If the value of x is too large, it is impossible to
!      accurately compute the trigonometric functions used. An
!      error message is printed, and the function returns the
!      value 1.0.
!
!
!   MACHINE-DEPENDENT CONSTANTS:
!
!      NTERM1 - The no. of terms to be used from the array
!                ARJ01. The recommended value is such that
!                   ABS(ARJ01(NTERM1)) < EPS/100
!
!      NTERM2 - The no. of terms to be used from the array
!                ARY01. The recommended value is such that
!                   ABS(ARY01(NTERM2)) < EPS/100
!
!      NTERM3 - The no. of terms to be used from the array
!                ARY0A1. The recommended value is such that
!                   ABS(ARY0A1(NTERM3)) < EPS/100
!
!      NTERM4 - The no. of terms to be used from the array
!                ARY0A2. The recommended value is such that
!                   ABS(ARY0A2(NTERM4)) < EPS/100
!
!      XLOW - The value of x below which
!                  Y0INT(x) = x*(ln(x) - 0.11593)*2/pi
!             to machine-precision. The recommended value is
!                 sqrt(9*EPSNEG)
!
!      XHIGH - The value of x above which it is impossible
!              to calculate (x-pi/4) accurately. The recommended
!              value is      1/EPSNEG
!
!      For values of EPS and EPSNEG, refer to the file MACHCON.TXT
!
!      The machine-dependent constants are computed internally by
!      using the D1MACH subroutine.
!
!
!   INTRINSIC FUNCTIONS USED:
!
!      COS , LOG , SIN , SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!   AUTHOR:
!          Dr. Allan J. MacLeod,
!          Dept. of Mathematics and Statistics,
!          University of Paisley,
!          Paisley,
!          SCOTLAND
!          PA1 2BE
!
!          (e-mail: macl_ms0@paisley.ac.uk)
!
!
!   LATEST REVISION:
!                    23 January, 1996
!
      INTEGER NTERM1,NTERM2,NTERM3,NTERM4
      DOUBLE PRECISION ARJ01(0:23),ARY01(0:24),ARY0A1(0:21),   &
           ARY0A2(0:18),CHEVAL,FIVE12,GAL2M1,GAMLN2,   &
           NINE,ONE,ONEHUN,ONE28,PIB41,PIB411,PIB412,   &
           PIB42,RT2BPI,SIXTEN,T,TEMP,TWOBPI,X,XHIGH,   &
           XLOW,XMPI4,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERMSG1*14,ERMSG2*18
!CCCC DATA FNNAME/'Y0INT '/
!CCCC DATA ERMSG1/'ARGUMENT < 0.0'/
!CCCC DATA ERMSG2/'ARGUMENT TOO LARGE'/
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ZERO,ONE/ 0.0D0 , 1.0D0 /
      DATA NINE,SIXTEN/ 9.0D0 , 16.0D0 /
      DATA ONEHUN,ONE28,FIVE12/ 100.0D0 , 128.0D0 , 512.0D0 /
      DATA RT2BPI/0.79788456080286535588D0/
      DATA PIB411,PIB412/ 201.0D0 , 256.0D0/
      DATA PIB42/0.24191339744830961566D-3/
      DATA TWOBPI/0.63661977236758134308D0/
      DATA GAL2M1/-1.11593151565841244881D0/
      DATA GAMLN2/-0.11593151565841244881D0/
      DATA ARJ01(0)/  0.38179279321690173518D0/
      DATA ARJ01(1)/ -0.21275636350505321870D0/
      DATA ARJ01(2)/  0.16754213407215794187D0/
      DATA ARJ01(3)/ -0.12853209772196398954D0/
      DATA ARJ01(4)/  0.10114405455778847013D0/
      DATA ARJ01(5)/ -0.9100795343201568859D-1/
      DATA ARJ01(6)/  0.6401345264656873103D-1/
      DATA ARJ01(7)/ -0.3066963029926754312D-1/
      DATA ARJ01(8)/  0.1030836525325064201D-1/
      DATA ARJ01(9)/ -0.255670650399956918D-2/
      DATA ARJ01(10)/ 0.48832755805798304D-3/
      DATA ARJ01(11)/-0.7424935126036077D-4/
      DATA ARJ01(12)/ 0.922260563730861D-5/
      DATA ARJ01(13)/-0.95522828307083D-6/
      DATA ARJ01(14)/ 0.8388355845986D-7/
      DATA ARJ01(15)/-0.633184488858D-8/
      DATA ARJ01(16)/ 0.41560504221D-9/
      DATA ARJ01(17)/-0.2395529307D-10/
      DATA ARJ01(18)/ 0.122286885D-11/
      DATA ARJ01(19)/-0.5569711D-13/
      DATA ARJ01(20)/ 0.227820D-14/
      DATA ARJ01(21)/-0.8417D-16/
      DATA ARJ01(22)/ 0.282D-17/
      DATA ARJ01(23)/-0.9D-19/
      DATA ARY01(0)/  0.54492696302724365490D0/
      DATA ARY01(1)/ -0.14957323588684782157D0/
      DATA ARY01(2)/  0.11085634486254842337D0/
      DATA ARY01(3)/ -0.9495330018683777109D-1/
      DATA ARY01(4)/  0.6820817786991456963D-1/
      DATA ARY01(5)/ -0.10324653383368200408D0/
      DATA ARY01(6)/  0.10625703287534425491D0/
      DATA ARY01(7)/ -0.6258367679961681990D-1/
      DATA ARY01(8)/  0.2385645760338293285D-1/
      DATA ARY01(9)/ -0.644864913015404481D-2/
      DATA ARY01(10)/ 0.131287082891002331D-2/
      DATA ARY01(11)/-0.20988088174989640D-3/
      DATA ARY01(12)/ 0.2716042484138347D-4/
      DATA ARY01(13)/-0.291199114014694D-5/
      DATA ARY01(14)/ 0.26344333093795D-6/
      DATA ARY01(15)/-0.2041172069780D-7/
      DATA ARY01(16)/ 0.137124781317D-8/
      DATA ARY01(17)/-0.8070680792D-10/
      DATA ARY01(18)/ 0.419883057D-11/
      DATA ARY01(19)/-0.19459104D-12/
      DATA ARY01(20)/ 0.808782D-14/
      DATA ARY01(21)/-0.30329D-15/
      DATA ARY01(22)/ 0.1032D-16/
      DATA ARY01(23)/-0.32D-18/
      DATA ARY01(24)/ 0.1D-19/
      DATA ARY0A1(0)/  1.24030133037518970827D0/
      DATA ARY0A1(1)/ -0.478125353632280693D-2/
      DATA ARY0A1(2)/  0.6613148891706678D-4/
      DATA ARY0A1(3)/ -0.186042740486349D-5/
      DATA ARY0A1(4)/  0.8362735565080D-7/
      DATA ARY0A1(5)/ -0.525857036731D-8/
      DATA ARY0A1(6)/  0.42606363251D-9/
      DATA ARY0A1(7)/ -0.4211761024D-10/
      DATA ARY0A1(8)/  0.488946426D-11/
      DATA ARY0A1(9)/ -0.64834929D-12/
      DATA ARY0A1(10)/ 0.9617234D-13/
      DATA ARY0A1(11)/-0.1570367D-13/
      DATA ARY0A1(12)/ 0.278712D-14/
      DATA ARY0A1(13)/-0.53222D-15/
      DATA ARY0A1(14)/ 0.10844D-15/
      DATA ARY0A1(15)/-0.2342D-16/
      DATA ARY0A1(16)/ 0.533D-17/
      DATA ARY0A1(17)/-0.127D-17/
      DATA ARY0A1(18)/ 0.32D-18/
      DATA ARY0A1(19)/-0.8D-19/
      DATA ARY0A1(20)/ 0.2D-19/
      DATA ARY0A1(21)/-0.1D-19/
      DATA ARY0A2(0)/  1.99616096301341675339D0/
      DATA ARY0A2(1)/ -0.190379819246668161D-2/
      DATA ARY0A2(2)/  0.1539710927044226D-4/
      DATA ARY0A2(3)/ -0.3114508832810D-6/
      DATA ARY0A2(4)/  0.1110850971321D-7/
      DATA ARY0A2(5)/ -0.58666787123D-9/
      DATA ARY0A2(6)/  0.4139926949D-10/
      DATA ARY0A2(7)/ -0.365398763D-11/
      DATA ARY0A2(8)/  0.38557568D-12/
      DATA ARY0A2(9)/ -0.4709800D-13/
      DATA ARY0A2(10)/ 0.650220D-14/
      DATA ARY0A2(11)/-0.99624D-15/
      DATA ARY0A2(12)/ 0.16700D-15/
      DATA ARY0A2(13)/-0.3028D-16/
      DATA ARY0A2(14)/ 0.589D-17/
      DATA ARY0A2(15)/-0.122D-17/
      DATA ARY0A2(16)/ 0.27D-18/
      DATA ARY0A2(17)/-0.6D-19/
      DATA ARY0A2(18)/ 0.1D-19/
!
      XLOW = 0.0D0
!
!   Start computation
!
      X = XVALUE
!
!   First error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERMSG1)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         Y0INT = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM I0INT--ARGUMENT MUST BE ',   &
              'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      TEMP = D1MACH(3)
      XHIGH = ONE / TEMP
!
!   Second error test
!
      IF ( X .GT. XHIGH ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERMSG2)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,201)X
         CALL DPWRST('XXX','BUG ')
         Y0INT = ZERO
         RETURN
      ENDIF
  201 FORMAT('***** ERROR FROM Y0INT--SIZE OF THE INPUT ARGUMENT ',   &
              'IS TOO LARGE, ARGUMENT = ',G15.7)
!
!   continue with machine constants
!
      T = TEMP / ONEHUN
      IF ( X .LE. SIXTEN ) THEN
         DO 10 NTERM1 = 23 , 0 , -1
            IF ( ABS(ARJ01(NTERM1)) .GT. T ) GO TO  19
 10      CONTINUE
 19      DO 20 NTERM2 = 24 , 0 , -1
            IF ( ABS(ARY01(NTERM2)) .GT. T ) GO TO  29
 20      CONTINUE
 29      XLOW = SQRT ( NINE * TEMP )
      ELSE
         DO 40 NTERM3 = 21 , 0 , -1
            IF ( ABS(ARY0A1(NTERM3)) .GT. T ) GO TO  49
 40      CONTINUE
 49      DO 50 NTERM4 = 18 , 0 , -1
            IF ( ABS(ARY0A2(NTERM4)) .GT. T ) GO TO  59
 50      CONTINUE
 59     CONTINUE
      ENDIF
!
!   Code for 0 <= x <= 16
!
      IF ( X .LE. SIXTEN ) THEN
         IF ( X .LT. XLOW ) THEN
            IF ( X .EQ. ZERO ) THEN
               Y0INT = ZERO
            ELSE
               Y0INT = ( LOG(X) + GAL2M1 ) * TWOBPI * X
            ENDIF
         ELSE
            T = X * X / ONE28 - ONE
            TEMP = ( LOG(X) + GAMLN2 ) * CHEVAL(NTERM1,ARJ01,T)
            TEMP = TEMP - CHEVAL(NTERM2,ARY01,T)
            Y0INT = TWOBPI * X * TEMP
         ENDIF
      ELSE
!
!   Code for x > 16
!
         T = FIVE12 / ( X * X ) - ONE
         PIB41 = PIB411 / PIB412
         XMPI4 = ( X - PIB41 ) - PIB42
         TEMP = SIN(XMPI4) * CHEVAL(NTERM3,ARY0A1,T) / X
         TEMP = TEMP + COS(XMPI4) * CHEVAL(NTERM4,ARY0A2,T)
         Y0INT = - RT2BPI * TEMP / SQRT(X)
      ENDIF
      RETURN
      END FUNCTION Y0INT
      SUBROUTINE YULCDF(DX,DP,DCDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE YULE
!              DISTRIBUTION WITH SHAPE PARAMETERS = P.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!                 P(X,P)=P*P!*X!/(X+P+1)!         X = 0, 1, 2, ...
!                       =P*GAMMA(P+1)*GAMMA(X+1)/GAMMA(X+P+2)
!              NOTE THAT THE YULE IS ALSO SOMETIME DEFINED AS
!                 P(X,P)=P*P!*(X-1)!/(X+P)!       X = 1, 2, ...
!              THE YULE IS ALSO A SPECIAL CASE OF THE WARING
!              DISTRIBUTION:
!                 YULCDF(X,P) = WARCDF(X,P-1,1)
!
!              NOTE: THE YULE DISTRIBUTION CAN ALSO BE GIVEN AS:
!
!                    f(X,P) = P*BETA(X+1,P+1)  X = 0, 1, 2, ...
!
!                    FROM THIS FORMULATION, THE CDF IS:
!
!                    F(X,P) = 1 - (X+1)*BETA(X+1,P+1)
!
!                    WE WILL USE THIS BETA FORMULATION TO COMPUTE
!                    THE YULE CDF.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --P    = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --P SHOULD BE POSITIVIE
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--SECOND EDITION, 1992, PP. 276-279.
!               --HERBERT A. SIMON (1955) "ON A CLASS OF SKEW
!                 DISTRIBUTIONS", BIOMETRIKA, 42(3/4), PP. 425-440.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
!     UPDATED         --MAY       2006. USE BETA FORMUALTION TO
!                                       COMPUTE THE CDF
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DTERM1, DTERM2
      DOUBLE PRECISION DX, DP
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DLBETA
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      DCDF=0.0D0
      IF(DP.LE.0.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      IX=INT(DX+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
             'YULCDF IS LESS THAN 0.')
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
             'YULCDF IS LESS THAN 0.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
!
      DTERM1=DLOG(DX+1.0D0)
      DTERM2=DLBETA(DX+1.0D0,DP+1.0D0)
      DCDF=1.0D0 - DEXP(DTERM1+DTERM2)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE YULCDF
      REAL FUNCTION YULFU2(X,XFREQ,VK)
!
!     PURPOSE--DPMLYU CALLS FZERO TO FIND A ROOT FOR THE LIKELIHOOD
!              FUNCTION.  YULFU2 IS THE FUNCTION FOR WHICH
!              THE ZERO IS FOUND.  IT IS:
!                 N/(X*(X-1)) -SUM[K=2 to LAMBDA][V(K)/(X+K-1)]
!              WITH V(K) DENOTING THE CUMULATIVE FREQUENCY FROM
!              K UPWARDS.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE YULFU2.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, BALKRISHNAN, "CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS, VOLUME 1", WILEY, 1994 (PAGE 63).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.12
!     ORIGINAL VERSION--DECEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL XFREQ(*)
      REAL VK(*)
      COMMON/YULCOM/NTOT,NCLASS
!
      REAL TERM1
      DOUBLE PRECISION DSUM1
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      TERM1=REAL(NTOT)/(X*(X-1.0))
      DSUM1=0.0D0
      DO 100 K=1,NCLASS
        IF(XFREQ(K).GE.1)THEN
          DSUM1=DSUM1 + VK(K)/(X+REAL(K)-1.0)
        ENDIF
  100 CONTINUE
      YULFU2=TERM1 - REAL(DSUM1)
!
      RETURN
      END FUNCTION YULFU2
      SUBROUTINE YULPDF(X,P,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE YULE
!              DISTRIBUTION WITH SHAPE PARAMETER = P.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!                 P(X,P)=P*P!*X!/(X+P+1)!         X = 0, 1, 2, ...
!                       =P*GAMMA(P+1)*GAMMA(X+1)/GAMMA(X+P+2)
!              NOTE THAT THE YULE IS ALSO SOMETIME DEFINED AS
!                 P(X,P)=P*P!*(X-1)!/(X+P)!       X = 1, 2, ...
!              THE YULE IS ALSO A SPECIAL CASE OF THE WARING
!              DISTRIBUTION:
!                 YULPDF(X,P) = WARPDF(X,P-1,1)
!              CURRENTLY, WE ONLY SUPPORT THE CASE WHERE P > 0.1
!              (TAIL GETS INFINITELY LONG AS P GOES TO 0).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --P    = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION DENSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --P SHOULD BE POSITIVIE
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--SECOND EDITION, 1992, PP. 276-279.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
      DOUBLE PRECISION DX, DP
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DLNGAM
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
!  FOR NOW, DO NOT ACCEPT VALUES OF P < 0.1
!
      IF(P.LT.0.1)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
!
      IX=INT(X+0.5)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
!
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO ',   &
             'YULPDF IS LESS THAN 0.')
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO ',   &
             'YULPDF IS < 0.1.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
!
      DX=DBLE(IX)
      DP=DBLE(P)
!
      DTERM1=DLOG(DP)
      DTERM2=DLNGAM(DX+1.0)
      DTERM3=DLNGAM(DP+1.0D0)
      DTERM4=DLNGAM(DP+DX+2.0D0)
      DTERM5=DTERM1+DTERM2+DTERM3-DTERM4
      DPDF=EXP(DTERM5)
!
      PDF=REAL(DPDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE YULPDF
      SUBROUTINE YULPPF(P,PPAR,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
!              FOR THE YULE DISTRIBUTION.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!                 P(X,P)=P*P!*X!/(X+P+1)!         X = 0, 1, 2, ...
!                       =P*GAMMA(P+1)*GAMMA(X+1)/GAMMA(X+P+2)
!              NOTE THAT THE YULE IS ALSO SOMETIME DEFINED AS
!                 P(X,P)=P*P!*(X-1)!/(X+P)!       X = 1, 2, ...
!              THE YULE IS ALSO A SPECIAL CASE OF THE WARING
!              DISTRIBUTION:
!                 YULPDF(X,P) = WARPDF(X,P-1,1)
!              CURRENTLY, WE ONLY SUPPORT THE CASE WHERE P > 0.1
!              (TAIL GETS INFINITELY LONG AS P GOES TO 0).
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                IT SHOULD BE IN THE INTERVAL (0,1).
!                     --PPAR   = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1 (EXCLUSIVELY FOR 1).
!                 --PPAR SHOULD BE GREATER THAN 0.1
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
!             FUNCTION VALUE PPF
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--SECOND EDITION, 1992, PP. 276-279.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
!     UPDATED         --MAY       2006. YULCDF NOW USES AN EXPLICIT
!                                       FORMULA RATHER THAN BRUTE
!                                       FORCE SUMMATION.  MODIFY
!                                       THIS ROUTINE TO USE
!                                       BISECTION METHOD.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DP, DPPAR
      DOUBLE PRECISION P0, P1, P2
      DOUBLE PRECISION X0, X1, X2
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSD
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      ITER=0
      PPF=0.0
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO YULPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(PPAR.LT.0.1)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO YULPPF ',   &
               'IS < 0.1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)PPAR
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
!     1) P = 0.0
!     2) P <= YULCDF(0,PPAR)
!
      IF(P.EQ.0.0)THEN
        PPF=0.0
        GO TO 9000
      ENDIF
!
      DPPAR=DBLE(PPAR)
      DP=DBLE(P)
      CALL YULCDF(DP,DPPAR,P0)
!
      IF(DP.LE.P0)THEN
        PPF=0.0
        GO TO 9000
      ENDIF
!
!     USE BRUTE FORCE METHOD WHERE CALCULATE CDF UNTIL CUMULATIVE
!     PROBABILITY IS GREATER THAN INPUT PROBABILITY.  DO THIS SINCE
!     YULE CDF DOES NOT CURRENTLY UTILIZE MORE EFFICIENT
!     APPROXIMATIONS.
!
!CCCC IUPPER=2000000
!
!CCCC DP=DBLE(PPAR)
!CCCC DCDF=0.0D0
!
!CCCC DTERM1=DLOG(DP)
!CCCC DTERM3=DLNGAM(DP+1.0D0)
!CCCC DO1000I=0,IUPPER
!CCCC   DX=DBLE(I)
!CCCC   DTERM2=DLNGAM(DX+1.0D0)
!CCCC   DTERM4=DLNGAM(DP+DX+2.0D0)
!CCCC   DTERM5=DTERM1+DTERM2+DTERM3-DTERM4
!CCCC   DCDF=DCDF + DEXP(DTERM5)
!CCCC   IF(DCDF.GE.DBLE(P))THEN
!CCCC     PPF=REAL(I)
!CCCC     GO TO 9000
!CCCC   ENDIF
!1000 CONTINUE
!
!CCCC PPF=REAL(IUPPER)
!CCCC WRITE(ICOUT,3000)IUPPER,IUPPER
!3000 FORMAT('****** PPF VALUE EXCEEDS ',I8,' .  TRUNCATED AT ',
!CCCC1'THIS VALUE.')
!CCCC CALL DPWRST('XXX','BUG ')
!
      PPF=0.0
      IX0=0
      IX1=0
      IX2=0
      P0=0.0
      P1=0.0
      P2=0.0
!
      IF(DPPAR.GT.1.0D0)THEN
        DMEAN=DPPAR/(DPPAR-1.0D0)
      ELSEIF(DPPAR.GE.0.5D0)THEN
        DMEAN=100.0D0
      ELSEIF(DPPAR.GE.0.2D0)THEN
        DMEAN=1000.0D0
      ELSE
        DMEAN=50000.0D0
      ENDIF
!
      IF(DPPAR.GT.2.0D0)THEN
        DSD=DSQRT(DPPAR**2/((DPPAR-1.0D0)**2*(DPPAR-2.0D0)))
      ELSEIF(DPPAR.GE.1.0D0)THEN
        DSD=1000.0D0
      ELSEIF(DPPAR.GE.0.5D0)THEN
        DSD=10000.0D0
      ELSEIF(DPPAR.GE.0.2D0)THEN
        DSD=100000.0D0
      ELSE
        DSD=1000000.0D0
      ENDIF
!
!  USE THE MEAN AS AN INITIAL APPROXIMATION TO THE YULE
!  PERCENT POINT.
!
      ISD=INT(DSD+1.0D0)
      IX2=INT(DMEAN+0.5)
      IX1=IX2+3*ISD
      X0=IX0
      X1=IX1
      X2=IX2
      CALL YULCDF(X0,DPPAR,P0)
      CALL YULCDF(X1,DPPAR,P1)
      CALL YULCDF(X2,DPPAR,P2)
!
!     LOWER BOUND IS ZERO.  NEED TO DETERMINE AN UPPER BOUND.
!     AFTER THIS BLOCK, SHOULD HAVE P0 <= P <= P1.
!
      MAXIT=100000
  201 CONTINUE
!
      IF(DP.GT.P1)THEN
        ITER=ITER+1
        IX1=IX1 + ISD
        X1=X1 + DSD
        IF(X1.GT.DBLE(I1MACH(9)))THEN
          IX1=I1MACH(9)
          X1=IX1
          CALL YULCDF(X1,DPPAR,P1)
          IF(DP.GT.P1 .OR. ITER.GT.MAXIT)THEN
            WRITE(ICOUT,221)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,222)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,222)
            CALL DPWRST('XXX','BUG ')
            GO TO 950
          ELSE
            GO TO 229
          ENDIF
  221     FORMAT('***** ERROR IN YULPPF ROUTINE--NO UPPER ',   &
                 'BOUND FOUND')
  222     FORMAT('      UPPER BOUND EXCEEDS MAXIMUM MACHINE ',   &
                 'INTEGER OR')
!C223     FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        ELSE
          IX1=INT(X1 + 0.0001D0)
          X1=REAL(IX1)
          CALL YULCDF(X1,DPPAR,P1)
          IF(P1.LT.DP)GO TO 201
        ENDIF
      ENDIF
!
  229 CONTINUE
!
      IF(P2.LT.DP)THEN
!
!       CASE WHERE P0 <= P2 <= DP <= P1
!
!                  SET IX0 TO IX2 AS LOWER BOUND
!
                                                                                                                                  
        IX0=IX2
        X0=X2
        P0=P2
!
      ELSE
!
!       CASE WHERE P0 <= DP <= P2 <= P1
!
!                  SET IX1 TO IX2 AS UPPER BOUND
!
        IX1=IX2
        X1=X2
        P1=P2
      ENDIF
!
!     IF LOWER BOUND = UPPER BOUND, SET TO PPF AND RETURN
!
      IF(IX0.EQ.IX1)THEN
        PPF=X0
        GO TO 9000
      ENDIF
!
!     CHECK THE PROBABILITIES FOR PROPER ORDERING
!
      IF(DP.EQ.P0)THEN
        PPF=IX0
        GO TO 9000
      ELSEIF(DP.EQ.P1)THEN
        PPF=IX1
        GO TO 9000
      ELSEIF(P0.GT.P1)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,431)
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ELSEIF(DP.LT.P0)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,441)
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ELSEIF(DP.GT.P1)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,451)
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ENDIF
  249 FORMAT('***** ERROR IN YULPPF ROUTINE')
  431 FORMAT('      LOWER BOUND PROBABILITY (P0) GREATER THAN ',   &
             'UPPER BOUND PROBABILITY (P1)')
  441 FORMAT('      LOWER BOUND PROBABILITY (P0) GREATER THAN ',   &
             'INPUT PROBABILITY (P)')
  451 FORMAT('      UPPER BOUND PROBABILITY (P1) LESS    THAN ',   &
             'INPUT PROBABILITY (P)')
!C461 FORMAT('      IMPOSSIBLE BRANCH ENCOUNTERED')
!
!     THE STOPPING CRITERION IS THAT THE LOWER BOUND
!     AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
!     CHECK TO SEE IF IX1 = IX0 + 1;
!     IF SO, THE ITERATIONS ARE COMPLETE;
!     IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
!     CHECK PROBABILITIES, AND CONTINUE ITERATING
!     UNTIL IX1 = IX0 + 1.
!
  300 CONTINUE
      IX0P1=IX0+1
      IF(IX1.EQ.IX0P1)THEN
        PPF=IX1
        IF(P0.EQ.DP)PPF=IX0
        GO TO 9000
      ENDIF
      X2=(DBLE(IX0)+DBLE(IX1))/2.0D0
      IX2=INT(X2+ 0.0001D0)
      IF(IX2.EQ.IX0)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,611)
  611   FORMAT('      BISECTION VALUE (X2) = LOWER BOUND (X0)')
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ELSEIF(IX2.EQ.IX1)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,621)
  621   FORMAT('      BISECTION VALUE (X2) = UPPER BOUND (X1)')
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ENDIF
      X2=IX2
      CALL YULCDF(X2,DPPAR,P2)
      IF(P0.LT.P2 .AND. P2.LT.P1)THEN
        IF(P2.LE.P)THEN
          IX0=IX2
          P0=P2
          GO TO 300
        ENDIF
        IX1=IX2
        P1=P2
        GO TO 300
      ELSEIF(P2.LE.P0)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,641)
  641   FORMAT('      BISECTION VALUE PROBABILITY (P2) ',   &
               'LESS THAN LOWER BOUND PROBABILITY (P0)')
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ELSEIF(P2.GE.P1)THEN
        WRITE(ICOUT,249)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,651)
  651   FORMAT('      BISECTION VALUE PROBABILITY (P2) ',   &
               'GREATER THAN UPPER BOUND PROBABILITY (P1)')
        CALL DPWRST('XXX','BUG ')
        GO TO 950
      ENDIF
!
  950 CONTINUE
      WRITE(ICOUT,240)IX0,P0
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,241)IX1,P1
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,242)IX2,P2
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,244)DP
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,245)DPPAR
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  240 FORMAT('      IX0  = ',I8,10X,'P0 = ',F14.7)
  241 FORMAT('      IX1  = ',I8,10X,'P1 = ',F14.7)
  242 FORMAT('      IX2  = ',I8,10X,'P2 = ',F14.7)
  244 FORMAT('      P    = ',F14.7)
  245 FORMAT('      DPPAR = ',F14.7)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE YULPPF
      SUBROUTINE YULRAN(N,P,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE YULE DISTRIBUTION
!              WITH SINGLE PRECISION SHAPE PARAMETER P.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY DENSITY FUNCTION IS:
!                 P(X,P)=P*P!*X!/(X+P+1)!         X = 0, 1, 2, ...
!                       =P*GAMMA(P+1)*GAMMA(X+1)/GAMMA(X+P+2)
!              NOTE THAT THE YULE IS ALSO SOMETIME DEFINED AS
!                 P(X,P)=P*P!*(X-1)!/(X+P)!       X = 1, 2, ...
!              THE YULE IS ALSO A SPECIAL CASE OF THE WARING
!              DISTRIBUTION:
!                 YULPDF(X,P) = WARPDF(X,P-1,1)
!              CURRENTLY, WE ONLY SUPPORT THE CASE WHERE P > 0.1
!              (TAIL GETS INFINITELY LONG AS P GOES TO 0).
!     ALGORITHM--FROM PAGE 553 OF
!                "NON-UNIFORM RANDOM VARIATE GENERATION",
!                LUC DEVROYE, SPRINGER-VERLAG, 1986.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --P      = THE SINGLE PRECISION VALUE
!                                OF THE SHAPE PARAMETER FOR THE
!                                YULE DISTRIBUTION.
!                                P SHOULD BE >= 0.1.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE LOGARITHMIC SERIES DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --P SHOULD BE >= 0.1
!     OTHER DATAPAC   SUBROUTINES NEEDED--EXPRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--LUC DEVROYE, "NON-UNIFORM RANDOM VARIATE
!                 GENERATION", SPRINGER-VERLAG, 1986, P. 553.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION U(2)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(P.LT.0.1)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
!
    5 FORMAT('***** ERROR--NUMBER OF YULE RANDOM ',   &
      'NUMBERS REQUESTED IS LESS THAN 1')
   15 FORMAT('***** ERROR--THE SHAPE PARAMETER FOR THE YULE ',   &
      'DISTRIBUTION IS < 0.1')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N YULE  RANDOM NUMBERS
!
!     ALGORITHM:
!     X = -E/[LOG(1 - EXP(-E*/P))]
!
!     WITH E AND E* DENOTING INDEPENDENT EXPONENTIAL RANDOM
!     VARIABLES.
!
      NTEMP=2
      DO 100 I=1,N
  110   CONTINUE
        CALL EXPRAN(NTEMP,ISEED,U)
        E1=U(1)
        E2=U(2)
        DENOM=LOG(1.0 - EXP(-E2/P))
        ATEMP=-E1/DENOM
        ITEMP=INT(ATEMP)
        X(I)=REAL(ITEMP)
        IF(X(I).LT.0.0)GO TO 110
  100 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE YULRAN
      SUBROUTINE XTXINV(AMAT1,AMAT2,Y1,Y2,INDX,   &
      MAXROM,MAXCOM,NR1,NC1,   &
      IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              (X'X)**(-1) MATRIX:
!              THIS MATRIX IS USEFUL FOR SOME REGRESSION DIAGNOSTIC
!              CAPABILITIES (E.G., THE CONDITION INDICES).
!     INPUT  ARGUMENTS--AMAT1  = THE DESIGN MATRIX (X)
!                     --Y1     = A SCRATCH VECTOR
!                     --Y2     = A SCRATCH VECTOR
!                     --INDX   = A SCRATCH INTEGER) VECTOR
!                     --MAXROM = THE INTEGER ROW DIMENSION OF AMAT1
!                     --MAXCOM = THE INTEGER COUMN DIMENSION OF AMAT1
!                     --NR1    = THE INTEGER NUMBER OF ROWS OF AMAT1
!                     --NC1    = THE INTEGER NUMBER OF COLUMNS OF AMAT1
!     OUTPUT ARGUMENTS--AMAT2  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CATCHER MATRTIX
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUES OF THE
!             CATCHER MATRIX.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002.6
!     ORIGINAL VERSION--JUNE      2002.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION AMAT1(MAXROM,MAXCOM)
      DIMENSION AMAT2(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      INTEGER   INDX(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ZERO /0.0/
      DATA ONE  /1.0/
      DATA EPS  /1.0E-20/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='XTXI'
      ISUBN2='NV  '
!
      IWRITE='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF XTXINV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXROM,MAXCOM,NR1,NC1
   53 FORMAT('MAXROM, MAXCOM, NR1, NC1 = ',4I8)
      CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  COMPUTE CATCHER MATRIX      **
!               **  1) COMPUTE X'X              **
!               **  2) COMPUTE INVERSE OF X'X   **
!               **  3) COMPUTE X TIMES INVERSE  **
!               **********************************
!
      DO 110 J=1,MAXCOM
        DO 120 I=1,MAXROM
          AMAT2(I,J)=ZERO
  120   CONTINUE
  110 CONTINUE
!
      CALL SGEMM ('T', 'N', NC1, NC1, NR1, ONE, AMAT1, MAXROM,   &
                   AMAT1, MAXROM, ZERO, AMAT2, MAXROM, IERROR)
      IF(IERROR.EQ.'YES')RETURN
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** IN XTXINV, AFTER CALL SGEMM--')
        CALL DPWRST('XXX','BUG ')
        DO 152 I=1,NC1
          WRITE(ICOUT,153)I,(AMAT2(I,J),J=1,MIN(5,NC1))
  153     FORMAT('***** I,AMAT2(I,1..MIN(NC1,5)',I8,5E15.7)
          CALL DPWRST('XXX','BUG ')
  152   CONTINUE
      ENDIF
!
      RCOND=0.0
      CALL SGECO(AMAT2,MAXROM,NC1,INDX,RCOND,Y1)
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,171)RCOND
  171   FORMAT('***** IN XTXINV, AFTER CALL SGECO, RCOND=',E15.7)
        CALL DPWRST('XXX','BUG ')
        DO 172 I=1,NC1
          WRITE(ICOUT,173)I,(AMAT2(I,J),J=1,MIN(5,NC1))
  173     FORMAT('***** I,AMAT2(I,1..MIN(NC1,5)',I8,5E15.7)
          CALL DPWRST('XXX','BUG ')
  172   CONTINUE
      ENDIF
!
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5171)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,5172)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,5173)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
        GO TO 9000
      ENDIF
 5171 FORMAT('*** ERROR FROM XTXINV: UNABLE TO COMPUTE THE INVERSE OF ',   &
             'THE X-TRANSPOSE*X MATRIX.')
 5172 FORMAT('    PROBLEM: SOME COLUMNS ARE LINEARLY DEPDENDENT ON ',   &
             ' OTHER COLUMNS.')
 5173 FORMAT('    SUGGESTED SOLUTION: WORK WITH A SUBSET OF THE ',   &
             'ORIGINAL COLUMNS.')
!
      IJOB=1
      CALL SGEDI(AMAT2,MAXROM,NC1,INDX,Y1,Y2,IJOB)
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** IN XTXINV, AFTER CALL SGEDI')
        CALL DPWRST('XXX','BUG ')
        DO 182 I=1,NC1
          WRITE(ICOUT,183)I,(AMAT2(I,J),J=1,MIN(5,NC1))
  183     FORMAT('***** I,AMAT2(I,1..MIN(NC1,5)',I8,5E15.7)
          CALL DPWRST('XXX','BUG ')
  182   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF XTXINV--')
      CALL DPWRST('XXX','BUG ')
      DO 9022 I=1,NR1
        WRITE(ICOUT,9023)I,(AMAT2(I,J),J=1,MIN(5,NC1))
 9023   FORMAT('***** I,AMAT2(I,1..MIN(NC1,5)',I8,5E15.7)
        CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE XTXINV
      SUBROUTINE YATES(IR,IC,OUT)
!
!     PURPOSE--DETERMINE IF THE ELEMENT IN ROW IR AND COLUMN IC
!              OF A MATRIX IN STANDARD YATES ORDER IS -1 OR +1.
!     OUTPUT--THE FLOATING POINT SCALAR OUT CONSISTING OF -1 OR +1.
!     DATE--SEPTEMBER 1993
!     ORIGINAL VERSION--SEPTEMBER 1993.
!
!---------------------------------------------------------------------
!
!-----START POINT-----------------------------------------------------
!
      N=2**IC
      NHALF=N/2
!
      OUT=+1.0
      I=MOD(IR,N)
      IF(1.LE.I.AND.I.LE.NHALF)OUT=-1.0
!
      RETURN
      END SUBROUTINE YATES
      SUBROUTINE YAIRY (X, RX, C, BI, DBI)
!***BEGIN PROLOGUE  YAIRY
!***SUBSIDIARY
!***PURPOSE  Subsidiary to BESJ and BESY
!***LIBRARY   SLATEC
!***TYPE      SINGLE PRECISION (YAIRY-S, DYAIRY-D)
!***AUTHOR  Amos, D. E., (SNLA)
!           Daniel, S. L., (SNLA)
!***DESCRIPTION
!
!                  YAIRY computes the Airy function BI(X)
!                   and its derivative DBI(X) for ASYJY
!
!                                     INPUT
!
!         X  - Argument, computed by ASYJY, X unrestricted
!        RX  - RX=SQRT(ABS(X)), computed by ASYJY
!         C  - C=2.*(ABS(X)**1.5)/3., computed by ASYJY
!
!                                    OUTPUT
!        BI  - Value of function BI(X)
!       DBI  - Value of the derivative DBI(X)
!
!***SEE ALSO  BESJ, BESY
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   750101  DATE WRITTEN
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900328  Added TYPE section.  (WRB)
!   910408  Updated the AUTHOR section.  (WRB)
!***END PROLOGUE  YAIRY
!
      INTEGER I, J, M1, M1D, M2, M2D, M3, M3D, M4D, N1, N1D, N2, N2D,   &
       N3, N3D, N4D
      REAL AA, AX, BB, BI, BJN, BJP, BK1, BK2, BK3, BK4, C, CON1, CON2,   &
       CON3, CV, DAA, DBB, DBI, DBJN, DBJP, DBK1, DBK2, DBK3, DBK4, D1,   &
       D2, EX, E1, E2, FPI12, F1, F2, RTRX, RX, SPI12, S1, S2, T, TC,   &
       TEMP1, TEMP2, TT, X
      DIMENSION BK1(20), BK2(20), BK3(20), BK4(14)
      DIMENSION BJP(19), BJN(19), AA(14), BB(14)
      DIMENSION DBK1(21), DBK2(20), DBK3(20), DBK4(14)
      DIMENSION DBJP(19), DBJN(19), DAA(14), DBB(14)
      SAVE N1, N2, N3, M1, M2, M3, N1D, N2D, N3D, N4D,   &
       M1D, M2D, M3D, M4D, FPI12, SPI12, CON1, CON2, CON3,   &
       BK1, BK2, BK3, BK4, BJP, BJN, AA, BB, DBK1, DBK2, DBK3, DBK4,   &
       DBJP, DBJN, DAA, DBB
      DATA N1,N2,N3/20,19,14/
      DATA M1,M2,M3/18,17,12/
      DATA N1D,N2D,N3D,N4D/21,20,19,14/
      DATA M1D,M2D,M3D,M4D/19,18,17,12/
      DATA FPI12,SPI12,CON1,CON2,CON3/   &
       1.30899693899575E+00, 1.83259571459405E+00, 6.66666666666667E-01,   &
       7.74148278841779E+00, 3.64766105490356E-01/
      DATA BK1(1),  BK1(2),  BK1(3),  BK1(4),  BK1(5),  BK1(6),   &
           BK1(7),  BK1(8),  BK1(9),  BK1(10), BK1(11), BK1(12),   &
           BK1(13), BK1(14), BK1(15), BK1(16), BK1(17), BK1(18),   &
           BK1(19), BK1(20)/ 2.43202846447449E+00, 2.57132009754685E+00,   &
       1.02802341258616E+00, 3.41958178205872E-01, 8.41978629889284E-02,   &
       1.93877282587962E-02, 3.92687837130335E-03, 6.83302689948043E-04,   &
       1.14611403991141E-04, 1.74195138337086E-05, 2.41223620956355E-06,   &
       3.24525591983273E-07, 4.03509798540183E-08, 4.70875059642296E-09,   &
       5.35367432585889E-10, 5.70606721846334E-11, 5.80526363709933E-12,   &
       5.76338988616388E-13, 5.42103834518071E-14, 4.91857330301677E-15/
      DATA BK2(1),  BK2(2),  BK2(3),  BK2(4),  BK2(5),  BK2(6),   &
           BK2(7),  BK2(8),  BK2(9),  BK2(10), BK2(11), BK2(12),   &
           BK2(13), BK2(14), BK2(15), BK2(16), BK2(17), BK2(18),   &
           BK2(19), BK2(20)/ 5.74830555784088E-01,-6.91648648376891E-03,   &
       1.97460263052093E-03,-5.24043043868823E-04, 1.22965147239661E-04,   &
      -2.27059514462173E-05, 2.23575555008526E-06, 4.15174955023899E-07,   &
      -2.84985752198231E-07, 8.50187174775435E-08,-1.70400826891326E-08,   &
       2.25479746746889E-09,-1.09524166577443E-10,-3.41063845099711E-11,   &
       1.11262893886662E-11,-1.75542944241734E-12, 1.36298600401767E-13,   &
       8.76342105755664E-15,-4.64063099157041E-15, 7.78772758732960E-16/
      DATA BK3(1),  BK3(2),  BK3(3),  BK3(4),  BK3(5),  BK3(6),   &
           BK3(7),  BK3(8),  BK3(9),  BK3(10), BK3(11), BK3(12),   &
           BK3(13), BK3(14), BK3(15), BK3(16), BK3(17), BK3(18),   &
           BK3(19), BK3(20)/ 5.66777053506912E-01, 2.63672828349579E-03,   &
       5.12303351473130E-05, 2.10229231564492E-06, 1.42217095113890E-07,   &
       1.28534295891264E-08, 7.28556219407507E-10,-3.45236157301011E-10,   &
      -2.11919115912724E-10,-6.56803892922376E-11,-8.14873160315074E-12,   &
       3.03177845632183E-12, 1.73447220554115E-12, 1.67935548701554E-13,   &
      -1.49622868806719E-13,-5.15470458953407E-14, 8.75741841857830E-15,   &
       7.96735553525720E-15,-1.29566137861742E-16,-1.11878794417520E-15/
      DATA BK4(1),  BK4(2),  BK4(3),  BK4(4),  BK4(5),  BK4(6),   &
           BK4(7),  BK4(8),  BK4(9),  BK4(10), BK4(11), BK4(12),   &
           BK4(13), BK4(14)/ 4.85444386705114E-01,-3.08525088408463E-03,   &
       6.98748404837928E-05,-2.82757234179768E-06, 1.59553313064138E-07,   &
      -1.12980692144601E-08, 9.47671515498754E-10,-9.08301736026423E-11,   &
       9.70776206450724E-12,-1.13687527254574E-12, 1.43982917533415E-13,   &
      -1.95211019558815E-14, 2.81056379909357E-15,-4.26916444775176E-16/
      DATA BJP(1),  BJP(2),  BJP(3),  BJP(4),  BJP(5),  BJP(6),   &
           BJP(7),  BJP(8),  BJP(9),  BJP(10), BJP(11), BJP(12),   &
           BJP(13), BJP(14), BJP(15), BJP(16), BJP(17), BJP(18),   &
           BJP(19)         / 1.34918611457638E-01,-3.19314588205813E-01,   &
       5.22061946276114E-02, 5.28869112170312E-02,-8.58100756077350E-03,   &
      -2.99211002025555E-03, 4.21126741969759E-04, 8.73931830369273E-05,   &
      -1.06749163477533E-05,-1.56575097259349E-06, 1.68051151983999E-07,   &
       1.89901103638691E-08,-1.81374004961922E-09,-1.66339134593739E-10,   &
       1.42956335780810E-11, 1.10179811626595E-12,-8.60187724192263E-14,   &
      -5.71248177285064E-15, 4.08414552853803E-16/
      DATA BJN(1),  BJN(2),  BJN(3),  BJN(4),  BJN(5),  BJN(6),   &
           BJN(7),  BJN(8),  BJN(9),  BJN(10), BJN(11), BJN(12),   &
           BJN(13), BJN(14), BJN(15), BJN(16), BJN(17), BJN(18),   &
           BJN(19)         / 6.59041673525697E-02,-4.24905910566004E-01,   &
       2.87209745195830E-01, 1.29787771099606E-01,-4.56354317590358E-02,   &
      -1.02630175982540E-02, 2.50704671521101E-03, 3.78127183743483E-04,   &
      -7.11287583284084E-05,-8.08651210688923E-06, 1.23879531273285E-06,   &
       1.13096815867279E-07,-1.46234283176310E-08,-1.11576315688077E-09,   &
       1.24846618243897E-10, 8.18334132555274E-12,-8.07174877048484E-13,   &
      -4.63778618766425E-14, 4.09043399081631E-15/
      DATA AA(1),   AA(2),   AA(3),   AA(4),   AA(5),   AA(6),   &
           AA(7),   AA(8),   AA(9),   AA(10),  AA(11),  AA(12),   &
           AA(13),  AA(14) /-2.78593552803079E-01, 3.52915691882584E-03,   &
       2.31149677384994E-05,-4.71317842263560E-06, 1.12415907931333E-07,   &
       2.00100301184339E-08,-2.60948075302193E-09, 3.55098136101216E-11,   &
       3.50849978423875E-11,-5.83007187954202E-12, 2.04644828753326E-13,   &
       1.10529179476742E-13,-2.87724778038775E-14, 2.88205111009939E-15/
      DATA BB(1),   BB(2),   BB(3),   BB(4),   BB(5),   BB(6),   &
           BB(7),   BB(8),   BB(9),   BB(10),  BB(11),  BB(12),   &
           BB(13),  BB(14) /-4.90275424742791E-01,-1.57647277946204E-03,   &
       9.66195963140306E-05,-1.35916080268815E-07,-2.98157342654859E-07,   &
       1.86824767559979E-08, 1.03685737667141E-09,-3.28660818434328E-10,   &
       2.57091410632780E-11, 2.32357655300677E-12,-9.57523279048255E-13,   &
       1.20340828049719E-13, 2.90907716770715E-15,-4.55656454580149E-15/
      DATA DBK1(1), DBK1(2), DBK1(3), DBK1(4), DBK1(5), DBK1(6),   &
           DBK1(7), DBK1(8), DBK1(9), DBK1(10),DBK1(11),DBK1(12),   &
           DBK1(13),DBK1(14),DBK1(15),DBK1(16),DBK1(17),DBK1(18),   &
           DBK1(19),DBK1(20),   &
           DBK1(21)        / 2.95926143981893E+00, 3.86774568440103E+00,   &
       1.80441072356289E+00, 5.78070764125328E-01, 1.63011468174708E-01,   &
       3.92044409961855E-02, 7.90964210433812E-03, 1.50640863167338E-03,   &
       2.56651976920042E-04, 3.93826605867715E-05, 5.81097771463818E-06,   &
       7.86881233754659E-07, 9.93272957325739E-08, 1.21424205575107E-08,   &
       1.38528332697707E-09, 1.50190067586758E-10, 1.58271945457594E-11,   &
       1.57531847699042E-12, 1.50774055398181E-13, 1.40594335806564E-14,   &
       1.24942698777218E-15/
      DATA DBK2(1), DBK2(2), DBK2(3), DBK2(4), DBK2(5), DBK2(6),   &
           DBK2(7), DBK2(8), DBK2(9), DBK2(10),DBK2(11),DBK2(12),   &
           DBK2(13),DBK2(14),DBK2(15),DBK2(16),DBK2(17),DBK2(18),   &
          DBK2(19),DBK2(20)/ 5.49756809432471E-01, 9.13556983276901E-03,   &
      -2.53635048605507E-03, 6.60423795342054E-04,-1.55217243135416E-04,   &
       3.00090325448633E-05,-3.76454339467348E-06,-1.33291331611616E-07,   &
       2.42587371049013E-07,-8.07861075240228E-08, 1.71092818861193E-08,   &
      -2.41087357570599E-09, 1.53910848162371E-10, 2.56465373190630E-11,   &
      -9.88581911653212E-12, 1.60877986412631E-12,-1.20952524741739E-13,   &
      -1.06978278410820E-14, 5.02478557067561E-15,-8.68986130935886E-16/
      DATA DBK3(1), DBK3(2), DBK3(3), DBK3(4), DBK3(5), DBK3(6),   &
           DBK3(7), DBK3(8), DBK3(9), DBK3(10),DBK3(11),DBK3(12),   &
           DBK3(13),DBK3(14),DBK3(15),DBK3(16),DBK3(17),DBK3(18),   &
          DBK3(19),DBK3(20)/ 5.60598509354302E-01,-3.64870013248135E-03,   &
      -5.98147152307417E-05,-2.33611595253625E-06,-1.64571516521436E-07,   &
      -2.06333012920569E-08,-4.27745431573110E-09,-1.08494137799276E-09,   &
      -2.37207188872763E-10,-2.22132920864966E-11, 1.07238008032138E-11,   &
       5.71954845245808E-12, 7.51102737777835E-13,-3.81912369483793E-13,   &
      -1.75870057119257E-13, 6.69641694419084E-15, 2.26866724792055E-14,   &
       2.69898141356743E-15,-2.67133612397359E-15,-6.54121403165269E-16/
      DATA DBK4(1), DBK4(2), DBK4(3), DBK4(4), DBK4(5), DBK4(6),   &
           DBK4(7), DBK4(8), DBK4(9), DBK4(10),DBK4(11),DBK4(12),   &
          DBK4(13),DBK4(14)/ 4.93072999188036E-01, 4.38335419803815E-03,   &
      -8.37413882246205E-05, 3.20268810484632E-06,-1.75661979548270E-07,   &
       1.22269906524508E-08,-1.01381314366052E-09, 9.63639784237475E-11,   &
      -1.02344993379648E-11, 1.19264576554355E-12,-1.50443899103287E-13,   &
       2.03299052379349E-14,-2.91890652008292E-15, 4.42322081975475E-16/
      DATA DBJP(1), DBJP(2), DBJP(3), DBJP(4), DBJP(5), DBJP(6),   &
           DBJP(7), DBJP(8), DBJP(9), DBJP(10),DBJP(11),DBJP(12),   &
           DBJP(13),DBJP(14),DBJP(15),DBJP(16),DBJP(17),DBJP(18),   &
           DBJP(19)        / 1.13140872390745E-01,-2.08301511416328E-01,   &
       1.69396341953138E-02, 2.90895212478621E-02,-3.41467131311549E-03,   &
      -1.46455339197417E-03, 1.63313272898517E-04, 3.91145328922162E-05,   &
      -3.96757190808119E-06,-6.51846913772395E-07, 5.98707495269280E-08,   &
       7.44108654536549E-09,-6.21241056522632E-10,-6.18768017313526E-11,   &
       4.72323484752324E-12, 3.91652459802532E-13,-2.74985937845226E-14,   &
      -1.95036497762750E-15, 1.26669643809444E-16/
      DATA DBJN(1), DBJN(2), DBJN(3), DBJN(4), DBJN(5), DBJN(6),   &
           DBJN(7), DBJN(8), DBJN(9), DBJN(10),DBJN(11),DBJN(12),   &
           DBJN(13),DBJN(14),DBJN(15),DBJN(16),DBJN(17),DBJN(18),   &
           DBJN(19)        /-1.88091260068850E-02,-1.47798180826140E-01,   &
       5.46075900433171E-01, 1.52146932663116E-01,-9.58260412266886E-02,   &
      -1.63102731696130E-02, 5.75364806680105E-03, 7.12145408252655E-04,   &
      -1.75452116846724E-04,-1.71063171685128E-05, 3.24435580631680E-06,   &
       2.61190663932884E-07,-4.03026865912779E-08,-2.76435165853895E-09,   &
       3.59687929062312E-10, 2.14953308456051E-11,-2.41849311903901E-12,   &
      -1.28068004920751E-13, 1.26939834401773E-14/
      DATA DAA(1),  DAA(2),  DAA(3),  DAA(4),  DAA(5),  DAA(6),   &
           DAA(7),  DAA(8),  DAA(9),  DAA(10), DAA(11), DAA(12),   &
           DAA(13), DAA(14)/ 2.77571356944231E-01,-4.44212833419920E-03,   &
       8.42328522190089E-05, 2.58040318418710E-06,-3.42389720217621E-07,   &
       6.24286894709776E-09, 2.36377836844577E-09,-3.16991042656673E-10,   &
       4.40995691658191E-12, 5.18674221093575E-12,-9.64874015137022E-13,   &
       4.90190576608710E-14, 1.77253430678112E-14,-5.55950610442662E-15/
      DATA DBB(1),  DBB(2),  DBB(3),  DBB(4),  DBB(5),  DBB(6),   &
           DBB(7),  DBB(8),  DBB(9),  DBB(10), DBB(11), DBB(12),   &
           DBB(13), DBB(14)/ 4.91627321104601E-01, 3.11164930427489E-03,   &
       8.23140762854081E-05,-4.61769776172142E-06,-6.13158880534626E-08,   &
       2.87295804656520E-08,-1.81959715372117E-09,-1.44752826642035E-10,   &
       4.53724043420422E-11,-3.99655065847223E-12,-3.24089119830323E-13,   &
       1.62098952568741E-13,-2.40765247974057E-14, 1.69384811284491E-16/
!***FIRST EXECUTABLE STATEMENT  YAIRY
      AX = ABS(X)
      RX = SQRT(AX)
      C = CON1*AX*RX
      IF (X.LT.0.0E0) GO TO 120
      IF (C.GT.8.0E0) GO TO 60
      IF (X.GT.2.5E0) GO TO 30
      T = (X+X-2.5E0)*0.4E0
      TT = T + T
      J = N1
      F1 = BK1(J)
      F2 = 0.0E0
      DO 10 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK1(J)
        F2 = TEMP1
   10 CONTINUE
      BI = T*F1 - F2 + BK1(1)
      J = N1D
      F1 = DBK1(J)
      F2 = 0.0E0
      DO 20 I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK1(J)
        F2 = TEMP1
   20 CONTINUE
      DBI = T*F1 - F2 + DBK1(1)
      RETURN
   30 CONTINUE
      RTRX = SQRT(RX)
      T = (X+X-CON2)*CON3
      TT = T + T
      J = N1
      F1 = BK2(J)
      F2 = 0.0E0
      DO 40 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK2(J)
        F2 = TEMP1
   40 CONTINUE
      BI = (T*F1-F2+BK2(1))/RTRX
      EX = EXP(C)
      BI = BI*EX
      J = N2D
      F1 = DBK2(J)
      F2 = 0.0E0
      DO 50 I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK2(J)
        F2 = TEMP1
   50 CONTINUE
      DBI = (T*F1-F2+DBK2(1))*RTRX
      DBI = DBI*EX
      RETURN
!
   60 CONTINUE
      RTRX = SQRT(RX)
      T = 16.0E0/C - 1.0E0
      TT = T + T
      J = N1
      F1 = BK3(J)
      F2 = 0.0E0
      DO 70 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK3(J)
        F2 = TEMP1
   70 CONTINUE
      S1 = T*F1 - F2 + BK3(1)
      J = N2D
      F1 = DBK3(J)
      F2 = 0.0E0
      DO 80 I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK3(J)
        F2 = TEMP1
   80 CONTINUE
      D1 = T*F1 - F2 + DBK3(1)
      TC = C + C
      EX = EXP(C)
      IF (TC.GT.35.0E0) GO TO 110
      T = 10.0E0/C - 1.0E0
      TT = T + T
      J = N3
      F1 = BK4(J)
      F2 = 0.0E0
      DO 90 I=1,M3
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK4(J)
        F2 = TEMP1
   90 CONTINUE
      S2 = T*F1 - F2 + BK4(1)
      BI = (S1+EXP(-TC)*S2)/RTRX
      BI = BI*EX
      J = N4D
      F1 = DBK4(J)
      F2 = 0.0E0
      DO 100 I=1,M4D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK4(J)
        F2 = TEMP1
  100 CONTINUE
      D2 = T*F1 - F2 + DBK4(1)
      DBI = RTRX*(D1+EXP(-TC)*D2)
      DBI = DBI*EX
      RETURN
  110 BI = EX*S1/RTRX
      DBI = EX*RTRX*D1
      RETURN
!
  120 CONTINUE
      IF (C.GT.5.0E0) GO TO 150
      T = 0.4E0*C - 1.0E0
      TT = T + T
      J = N2
      F1 = BJP(J)
      E1 = BJN(J)
      F2 = 0.0E0
      E2 = 0.0E0
      DO 130 I=1,M2
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + BJP(J)
        E1 = TT*E1 - E2 + BJN(J)
        F2 = TEMP1
        E2 = TEMP2
  130 CONTINUE
      BI = (T*E1-E2+BJN(1)) - AX*(T*F1-F2+BJP(1))
      J = N3D
      F1 = DBJP(J)
      E1 = DBJN(J)
      F2 = 0.0E0
      E2 = 0.0E0
      DO 140 I=1,M3D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DBJP(J)
        E1 = TT*E1 - E2 + DBJN(J)
        F2 = TEMP1
        E2 = TEMP2
  140 CONTINUE
      DBI = X*X*(T*F1-F2+DBJP(1)) + (T*E1-E2+DBJN(1))
      RETURN
!
  150 CONTINUE
      RTRX = SQRT(RX)
      T = 10.0E0/C - 1.0E0
      TT = T + T
      J = N3
      F1 = AA(J)
      E1 = BB(J)
      F2 = 0.0E0
      E2 = 0.0E0
      DO 160 I=1,M3
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + AA(J)
        E1 = TT*E1 - E2 + BB(J)
        F2 = TEMP1
        E2 = TEMP2
  160 CONTINUE
      TEMP1 = T*F1 - F2 + AA(1)
      TEMP2 = T*E1 - E2 + BB(1)
      CV = C - FPI12
      BI = (TEMP1*COS(CV)+TEMP2*SIN(CV))/RTRX
      J = N4D
      F1 = DAA(J)
      E1 = DBB(J)
      F2 = 0.0E0
      E2 = 0.0E0
      DO 170 I=1,M4D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DAA(J)
        E1 = TT*E1 - E2 + DBB(J)
        F2 = TEMP1
        E2 = TEMP2
  170 CONTINUE
      TEMP1 = T*F1 - F2 + DAA(1)
      TEMP2 = T*E1 - E2 + DBB(1)
      CV = C - SPI12
      DBI = (TEMP1*COS(CV)-TEMP2*SIN(CV))*RTRX
      RETURN
      END SUBROUTINE YAIRY 
      double precision function zeroin(ax,bx,f,tol,ierror)
      double precision ax,bx,f,tol
!
!      NOTE: This subroutine used in computing the consensus mean
!            using the Iyer and Wang generalized tolerance interval
!            approach.
!
!            Modified for Dataplot 3/2006.
!
!      a zero of the function  f(x)  is computed in the
!      interval ax,bx .
!
!  input..
!
!  ax     left endpoint of initial interval
!  bx     right endpoint of initial interval
!  f      function subprogram which evaluates f(x) for any x in
!         the interval  ax,bx
!  tol    desired length of the interval of uncertainty of the
!         final result ( .ge. 0.0d0)
!
!
!  output..
!
!  zeroin abcissa approximating a zero of  f  in the interval ax,bx
!
!
!      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
!  without  a  check.  zeroin  returns a zero  x  in the given interval
!  ax,bx  to within a tolerance  4*macheps*abs(x) + tol, where macheps
!  is the relative machine precision.
!
!  this function subprogram is a slightly  modified  translation  of
!  the algol 60 procedure  zero  given in  richard brent, algorithms
!  for c  minimization without derivatives, prentice - hall,
!  inc. (1973).
!
      integer           nstep, maxfn
      double precision  a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s
      double precision  dabs,dsign
!
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOP2.INC'
!
      data maxfn /100/
      data eps   /1.1102230246252D-16/
!
! initialization
!
      nstep = 0
      a = ax
      b = bx
      fa = f(a)
      fb = f(b)
!
      if (fa*fb .gt. 0.0d0) then
!cccc    write (*, '(A)') 'f(a) and f(b) not opposite signs'
!cccc    write (*, *) fa, fb
         WRITE(ICOUT,10)
   10    FORMAT('*****ERROR FROM CONSENSUS MEANS--')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,12)
   12    FORMAT('     IN ZEROIN (ROOT FINDER), THE END POINTS')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,14)
   14    FORMAT('     (FA AND FB) DO NOT HAVE OPPOSITE SIGNS.')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,16)
   16    FORMAT('     FA = ',G15.7)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,18)
   18    FORMAT('     FB = ',G15.7)
         CALL DPWRST('XXX','WRIT')
         ZEROIN=0.0
         IERROR='YES'
         GO TO 9000
      end if
!
! begin step
!
   20 c = a
      fc = fa
      d = b - a
      e = d
   30 if (dabs(fc) .ge. dabs(fb)) go to 40
      a = b
      b = c
      c = a
      fa = fb
      fb = fc
      fc = fa
!
! convergence test
!
   40 tol1 = 2.0d0*eps*dabs(b) + 0.5d0*tol
      xm = .5*(c - b)
      if (dabs(xm) .le. tol1) go to 90
      if (fb .eq. 0.0d0) go to 90
!
! is bisection necessary
!
      if (dabs(e) .lt. tol1) go to 70
      if (dabs(fa) .le. dabs(fb)) go to 70
!
! is quadratic interpolation possible
!
      if (a .ne. c) go to 50
!
! linear interpolation
!
      s = fb/fa
      p = 2.0d0*xm*s
      q = 1.0d0 - s
      go to 60
!
! inverse quadratic interpolation
!
   50 q = fa/fc
      r = fb/fc
      s = fb/fa
      p = s*(2.0d0*xm*q*(q - r) - (b - a)*(r - 1.0d0))
      q = (q - 1.0d0)*(r - 1.0d0)*(s - 1.0d0)
!
! adjust signs
!
   60 if (p .gt. 0.0d0) q = -q
      p = dabs(p)
!
! is interpolation acceptable
!
      if ((2.0d0*p) .ge. (3.0d0*xm*q - dabs(tol1*q))) go to 70
      if (p .ge. dabs(0.5d0*e*q)) go to 70
      e = d
      d = p/q
      go to 80
!
! bisection
!
   70 d = xm
      e = d
!
! complete step
!
   80 nstep = nstep + 1
      a = b
      fa = fb
      if (dabs(d) .gt. tol1) b = b + d
      if (dabs(d) .le. tol1) b = b + dsign(tol1, xm)
      fb = f(b)
      if (nstep .gt. maxfn) go to 90
      if ((fb*(fc/dabs(fc))) .gt. 0.0d0) go to 20
      go to 30
!
! done
!
   90 zeroin = b
!
 9000 CONTINUE
      return
      end
      SUBROUTINE ZETA(DX,DZETA)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE ZETA FUNCTION
!              FOR REAL ARGUMENTS GREATER THAN 1 USING
!              EULER-MACMACLAURIN SUMMATION.
!              ZETA(X)=SUM(1/K**X)  WHERE THE SUM IS FROM
!                      1 TO INFINITY
!              FOR BETTER COMPUTATIONAL ACCURACY, ACTUALLY
!              COMPUTE ZETA(X) - 1.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE ZETA
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--DZETA  = THE DOUBLE PRECISION ZETA
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION ZETA
!             FUNCTION VALUE DZETA.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHEMATICS
!                 SERIES 55, 1964.
!               --THOMPSON, "ATLAS FOR COMPUTING MATHEMATICAL
!                 FUNCTIONS", WILEY, 1997.  THIS ROUTINE IS A
!                 FORTRAN TRANSLATION OF THE C FUNCTION ON PAGE 146
!                 OF THIS BOOK.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--97.9
!     ORIGINAL VERSION--SEPTEMBER 1997.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DEPS/1.0D-20/
!
!-----START POINT-----------------------------------------------------
!
      DSTERM=DX*(DX+1.0D0)*(DX+2.0D0)*   &
             (DX+3.0D0)*(DX+4.0D0)/30240.0D0
      DTERM1=DSTERM*(2.0D0**DX)/DEPS
      DTERM2=DTERM1**(1.0D0/(DX+5.0D0))
      IF(DTERM2.LE.10.01)THEN
        N=10
      ELSEIF(DTERM2.GE.9999.99D0)THEN
        N=10000
      ELSE
        N=INT(DTERM2)
      ENDIF
!
      FN=DBLE(N)
      DNEGX=-DX
      DSUM=0.D0
      DO 100 K=2,N-1
        DSUM=DSUM + DBLE(K)**DNEGX
 100  CONTINUE
!
      DSUM = DSUM +   &
             (FN**DNEGX)*(0.5D0 + FN/(DX-1.0D0)   &
             + DX*(1.0D0 -   &
             (DX+1.0D0)*(DX+2.0D0)/(60.0D0*FN*FN))/(12.0D0*FN))   &
             + DSTERM/(FN**(DX+0.5D0))
!
!CCCC COMPUTE ZETA(X) - 1 FOR BETTER ACCURACY.
!CCCC DZETA=DSUM+1.0D0
      DZETA=DSUM
      RETURN
      END SUBROUTINE ZETA
      SUBROUTINE ZETA2(DX,DZETA)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE ZETA FUNCTION
!              FOR REAL ARGUMENTS GREATER THAN 1 USING
!              EULER-MACMACLAURIN SUMMATION.
!              ZETA(X)=SUM(1/K**X)  WHERE THE SUM IS FROM
!                      1 TO INFINITY
!              FOR BETTER COMPUTATIONAL ACCURACY, ACTUALLY
!              COMPUTE ZETA(X) - 1.
!     NOTE--THIS IS A DUPLICATE OF THE ZETA SUBROUTINE, NEEDED
!           BY DPCHS2 ROUTINE TO AVOID A NAME CONFLICT.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE ZETA
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--DZETA  = THE DOUBLE PRECISION ZETA
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION ZETA
!             FUNCTION VALUE DZETA.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHEMATICS
!                 SERIES 55, 1964.
!               --THOMPSON, "ATLAS FOR COMPUTING MATHEMATICAL
!                 FUNCTIONS", WILEY, 1997.  THIS ROUTINE IS A
!                 FORTRAN TRANSLATION OF THE C FUNCTION ON PAGE 146
!                 OF THIS BOOK.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--97.9
!     ORIGINAL VERSION--SEPTEMBER 1997.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DEPS/1.0D-20/
!
!-----START POINT-----------------------------------------------------
!
      DSTERM=DX*(DX+1.0D0)*(DX+2.0D0)*   &
             (DX+3.0D0)*(DX+4.0D0)/30240.0D0
      DTERM1=DSTERM*(2.0D0**DX)/DEPS
      DTERM2=DTERM1**(1.0D0/(DX+5.0D0))
      IF(DTERM2.LE.10.01)THEN
        N=10
      ELSEIF(DTERM2.GE.9999.99D0)THEN
        N=10000
      ELSE
        N=INT(DTERM2)
      ENDIF
!
      FN=DBLE(N)
      DNEGX=-DX
      DSUM=0.D0
      DO 100 K=2,N-1
        DSUM=DSUM + DBLE(K)**DNEGX
 100  CONTINUE
!
      DSUM = DSUM +   &
             (FN**DNEGX)*(0.5D0 + FN/(DX-1.0D0)   &
             + DX*(1.0D0 -   &
             (DX+1.0D0)*(DX+2.0D0)/(60.0D0*FN*FN))/(12.0D0*FN))   &
             + DSTERM/(FN**(DX+0.5D0))
!
!CCCC COMPUTE ZETA(X) - 1 FOR BETTER ACCURACY.
!CCCC DZETA=DSUM+1.0D0
      DZETA=DSUM
      RETURN
      END SUBROUTINE ZETA2
      SUBROUTINE ZETCDF(X,ALPHA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE ZETA
!              DISTRIBUTION WITH SHAPE PARAMETER = ALPHA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 1.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
!              F(X,ALPHA)=1/[ZETA(ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                     ALPHA > 1
!              WITH ZETA DENOTING THE RIEMANN ZETA FUNCTION.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE ZETA
!             DISTRIBUTION WITH SHAPE PARAMETER = ALPHA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --ALPHA > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JAMES E. GENTLE (2003). 'RANDOM NUMBER GENERATION
!                 AND MONTE CARLO METHODS', SPRINGER-VERLANG, P. 192.
!                 USE HIS DESCRIPTION OF AN ALGORITHM DUE TO DEVROYE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DHNM
      DOUBLE PRECISION DCDF
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0
      IX=INT(X+0.5)
      IF(ALPHA.LE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR THE ',   &
               'ZETA CDF IS <= 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT ',   &
               'TO ZETCDF IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      DX=DBLE(IX)
      DALPHA=DBLE(ALPHA)
!
      CALL HNM(IX,DALPHA,DHNM)
      CALL ZETA(DALPHA,DZETA)
      DZETA=DZETA+1.0D0
      DCDF=DHNM/DZETA
      CDF=REAL(DCDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZETCDF
      REAL FUNCTION ZETFUN(ALPHA)
!
!     PURPOSE--DPMLZE CALLS FZERO TO FIND A ROOT FOR THE MLE
!              FUNCTION.  ZETFUN IS THE FUNCTION FOR WHICH
!              THE ZERO IS FOUND.  IT IS:
!                 SUM[i=1 to N][LN(X(i)] +
!                 ZETA'(ALPHAHAT)/ZETA(ALPHAHAT) = 0
!              THE VALUE FOR THE ZETA'()/ZETA() TERM
!              WILL BE APPROXIMATED FROM A TABLE GIVEN IN JOHNSON,
!              KOTZ, AND KEMP.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE ZETFUN.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--ZETA.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, BALKRISHNAN, "DISCRETE UNIVARIATE
!                 DISTRIBUTIONS", SECOND EDITION, WILEY, 1992
!                 (PP. 465-469).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006.5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ALPHA
      COMMON/ZETCOM/XBAR,SUM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      TERM3=CPUMIN
      IF(ALPHA.LE.1.1)THEN
        TERM3=9.441
      ELSE IF(ALPHA.GE.1.1 .AND. ALPHA.LT.1.2)THEN
        TERM1=9.441
        TERM2=4.458
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.1)/(1.2 - 1.1)
      ELSE IF(ALPHA.GE.1.2 .AND. ALPHA.LT.1.3)THEN
        TERM1=4.458
        TERM2=2.808
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.2)/(1.3 - 1.2)
      ELSE IF(ALPHA.GE.1.3 .AND. ALPHA.LT.1.4)THEN
        TERM1=2.808
        TERM2=1.990
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.3)/(1.4 - 1.3)
      ELSE IF(ALPHA.GE.1.4 .AND. ALPHA.LT.1.5)THEN
        TERM1=1.990
        TERM2=1.505
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.4)/(1.5 - 1.4)
      ELSE IF(ALPHA.GE.1.6 .AND. ALPHA.LT.1.5)THEN
        TERM1=1.505
        TERM2=1.186
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.6)/(1.6 - 1.5)
      ELSE IF(ALPHA.GE.1.7 .AND. ALPHA.LT.1.6)THEN
        TERM1=1.186
        TERM2=0.961
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.7)/(1.7 - 1.6)
      ELSE IF(ALPHA.GE.1.8 .AND. ALPHA.LT.1.7)THEN
        TERM1=0.961
        TERM2=0.796
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.8)/(1.8 - 1.7)
      ELSE IF(ALPHA.GE.1.9 .AND. ALPHA.LT.1.8)THEN
        TERM1=0.796
        TERM2=0.669
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 1.9)/(1.9 - 1.8)
      ELSE IF(ALPHA.GE.2.0 .AND. ALPHA.LT.1.9)THEN
        TERM1=0.669
        TERM2=0.570
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.0)/(2.0 - 1.9)
      ELSE IF(ALPHA.GE.2.1 .AND. ALPHA.LT.2.0)THEN
        TERM1=0.570
        TERM2=0.490
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.1)/(2.1 - 2.0)
      ELSE IF(ALPHA.GE.2.2 .AND. ALPHA.LT.2.1)THEN
        TERM1=0.490
        TERM2=0.425
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.2)/(2.2 - 2.1)
      ELSE IF(ALPHA.GE.2.3 .AND. ALPHA.LT.2.2)THEN
        TERM1=0.425
        TERM2=0.372
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.3)/(2.3 - 2.2)
      ELSE IF(ALPHA.GE.2.4 .AND. ALPHA.LT.2.3)THEN
        TERM1=0.372
        TERM2=0.327
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.4)/(2.4 - 2.3)
      ELSE IF(ALPHA.GE.2.5 .AND. ALPHA.LT.2.4)THEN
        TERM1=0.327
        TERM2=0.289
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.5)/(2.5 - 2.4)
      ELSE IF(ALPHA.GE.2.6 .AND. ALPHA.LT.2.5)THEN
        TERM1=0.289
        TERM2=0.256
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.6)/(2.6 - 2.5)
      ELSE IF(ALPHA.GE.2.7 .AND. ALPHA.LT.2.6)THEN
        TERM1=0.256
        TERM2=0.228
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.7)/(2.7 - 2.6)
      ELSE IF(ALPHA.GE.2.8 .AND. ALPHA.LT.2.7)THEN
        TERM1=0.228
        TERM2=0.204
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.8)/(2.8 - 2.7)
      ELSE IF(ALPHA.GE.2.9 .AND. ALPHA.LT.2.8)THEN
        TERM1=0.204
        TERM2=0.183
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 2.9)/(2.9 - 2.8)
      ELSE IF(ALPHA.GE.3.0 .AND. ALPHA.LT.2.9)THEN
        TERM1=0.183
        TERM2=0.164
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 3.0)/(3.0 - 2.9)
      ELSE IF(ALPHA.GE.3.2 .AND. ALPHA.LT.3.0)THEN
        TERM1=0.164
        TERM2=0.134
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 3.2)/(3.2 - 3.0)
      ELSE IF(ALPHA.GE.3.4 .AND. ALPHA.LT.3.2)THEN
        TERM1=0.134
        TERM2=0.110
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 3.4)/(3.4 - 3.2)
      ELSE IF(ALPHA.GE.3.6 .AND. ALPHA.LT.3.4)THEN
        TERM1=0.110
        TERM2=0.0914
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 3.6)/(3.6 - 3.4)
      ELSE IF(ALPHA.GE.3.8 .AND. ALPHA.LT.3.6)THEN
        TERM1=0.0914
        TERM2=0.0761
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 3.8)/(3.8 - 3.6)
      ELSE IF(ALPHA.GE.4.0 .AND. ALPHA.LT.3.8)THEN
        TERM1=0.0761
        TERM2=0.0637
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 4.0)/(4.0 - 3.8)
      ELSE IF(ALPHA.GE.4.2 .AND. ALPHA.LT.4.0)THEN
        TERM1=0.0637
        TERM2=0.0535
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 4.2)/(4.2 - 4.0)
      ELSE IF(ALPHA.GE.4.4 .AND. ALPHA.LT.4.2)THEN
        TERM1=0.0535
        TERM2=0.0451
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 4.4)/(4.4 - 4.2)
      ELSE IF(ALPHA.GE.4.6 .AND. ALPHA.LT.4.4)THEN
        TERM1=0.0451
        TERM2=0.0382
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 4.6)/(4.6 - 4.4)
      ELSE IF(ALPHA.GE.4.8 .AND. ALPHA.LT.4.6)THEN
        TERM1=0.0382
        TERM2=0.0324
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 4.8)/(4.8 - 4.6)
      ELSE IF(ALPHA.GE.5.0 .AND. ALPHA.LT.4.8)THEN
        TERM1=0.0324
        TERM2=0.0276
        AFACT1=TERM1 - TERM2
        TERM3=TERM1 - AFACT1*(ALPHA - 5.0)/(5.0 - 4.8)
      ELSE IF(ALPHA.GT.5.0)THEN
        TERM3=LOG(2.0)/(1.0+2.0**ALPHA)
      ENDIF
      ZETFUN=TERM3 - SUM1
!
      RETURN
      END FUNCTION ZETFUN
      REAL FUNCTION ZETFU2(ALPHA)
!
!     PURPOSE--DPMLZE CALLS FZERO TO FIND A ROOT FOR THE MOMENT
!              FUNCTION.  ZETFU2 IS THE FUNCTION FOR WHICH
!              THE ZERO IS FOUND.  IT IS:
!                 XBAR - ZETA(ALPHAHAT-1)/ZETA(ALPHAHAT) = 0
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE ZETFU2.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--ZETA.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, BALKRISHNAN, "DISCRETE UNIVARIATE
!                 DISTRIBUTIONS", SECOND EDITION, WILEY, 1992
!                 (PP. 465-469).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006.5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ALPHA
      COMMON/ZETCOM/XBAR,SUM1
!
      DOUBLE PRECISION DZETA1
      DOUBLE PRECISION DZETA2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CALL ZETA(DBLE(ALPHA)-1.0D0,DZETA1)
      DZETA1=DZETA1+1.0D0
      CALL ZETA(DBLE(ALPHA),DZETA2)
      DZETA2=DZETA2+1.0D0
      ZETFU2=XBAR - REAL(DZETA1/DZETA2)
!
      RETURN
      END FUNCTION ZETFU2
      SUBROUTINE ZETPDF(X,ALPHA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE ZETA
!              DISTRIBUTION WITH SHAPE PARAMETER = ALPHA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 1.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              F(X,ALPHA)=1/[ZETA(ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                     ALPHA > 1
!              WITH ZETA DENOTING THE RIEMANN ZETA FUNCTION.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE ZIPF
!             DISTRIBUTION WITH SHAPE PARAMETER = ALPHA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --ALPHA > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JAMES E. GENTLE (2003). 'RANDOM NUMBER GENERATION
!                 AND MONTE CARLO METHODS', SPRINGER-VERLANG, P. 192.
!                 USE HIS DESCRIPTION OF AN ALGORITHM DUE TO DEVROYE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/11
!     ORIGINAL VERSION--NOVEMBER  2003.
!     UPDATED         --MAY       2006. RENAME FROM ZIPPDF TO
!                                       ZETPDF SINCE SOME SOURCES
!                                       MAKE A DISTINCTION BETWEEN
!                                       ZETA AND ZIPF DISTRIBUTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DPDF
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      IX=INT(X+0.5)
      IF(ALPHA.LE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR THE ',   &
               'ZIPF PDF IS <= 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT ',   &
               'TO ZETPDF IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      DX=DBLE(IX)
      DALPHA=DBLE(ALPHA)
!
      CALL ZETA(DALPHA,DZETA)
      DZETA=DZETA+1.0D0
      DPDF=DLOG(1.0D0) - DLOG(DZETA) - DALPHA*DLOG(DX)
      DPDF=DEXP(DPDF)
      PDF=REAL(DPDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZETPDF
      SUBROUTINE ZETPPF(P,ALPHA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
!              FOR THE ZETA DISTRIBUTION WITH SINGLE PRECISION
!              SHAPE PARAMETER ALPHA.
!              THIS DISTRIBUTION IS DEFINED FOR 0 <= P < 1.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!              P(X;ALPHA)=1/[ZETA(ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                     ALPHA > 1
!              WITH ZETA DENOTING THE RIEMANN ZETA FUNCTION.
!
!              WE CURRENTLY COMPUTE THE PERCENT POINT FUNCTION
!              VIA BRUTE FORCE.  THAT IS, WE COMPUTE THE
!              CUMULATIVE DISTRIBUTION FUNCTION UNTIL IT EXCEEDS
!              THE SPECIFIED VALUE OF P.
!
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                0 <= P < 1.
!                     --ALPHA  = THE SINGLE PRECISION VALUE
!                                OF THE FIRST SHAPE PARAMETER.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF
!             FOR THE ZETA DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--0 <= P < 1
!                 --ALPHA > 1
!     OTHER DATAPAC   SUBROUTINES NEEDED--ZETA.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JAMES E. GENTLE (2003). 'RANDOM NUMBER GENERATION
!                 AND MONTE CARLO METHODS', SPRINGER-VERLANG, P. 192.
!               --JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS", SECOND EDITION, WILEY.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DP
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DEPS
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
      PPF=0.0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ALPHA.LE.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO ZETPPF ',   &
               'IS <= 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZETPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DALPHA=DBLE(ALPHA)
      DSUM=0.0D0
      DP=DBLE(P)
      CALL ZETA(DALPHA,DZETA)
      DZETA=DZETA+1.0D0
      DEPS=1.0D-7
!
!     COMPUTE PDF FOR X = 1
!
      DPDF=DLOG(1.0D0) - DLOG(DZETA) - DALPHA*DLOG(1.0D0)
      DPDF=DEXP(DPDF)
!
      DCDF=DPDF
      IF(DCDF.GE.DP-DEPS)THEN
        PPF=1.0
        GO TO 9999
      ENDIF
      I=1
!
  100 CONTINUE
        I=I+1
        IF(DBLE(I).GE.DBLE(I1MACH(9)))THEN
          WRITE(ICOUT,55)
   55     FORMAT('***** ERROR--THE COMPUTED PERCENT POINT VALUE ',   &
                 'EXCEEDS THE LARGEST MACHINE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          PPF=0.0
          GO TO 9999
        ENDIF
        DPDF=DLOG(1.0D0) - DLOG(DZETA) - DALPHA*DLOG(DBLE(I))
        DPDF=DEXP(DPDF)
        DCDF=DCDF + DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          PPF=REAL(I)
          GO TO 9999
        ENDIF
      GO TO 100
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZETPPF
      SUBROUTINE ZETRAN(N,ALPHA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE ZETA DISTRIBUTION
!              WITH SHAPE PARAMETER ALPHA
!              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE INTEGERS
!              X, AND HAS THE PROBABILITY MASS FUNCTION
!              F(X) = 1/[ZETA(ALPHA)*X**ALPHA]   X = 1, 2, 3, ...
!                                                ALPHA > 1
!              WITH ZETA DENOTING THE RIEMANN ZETA FUNCTION.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --ALPHA  = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER, ALPHA > 1
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE ZETA DISTRIBUTION
!             WITH SHAPE LENGTH PARAMETER VALUE = ALPHA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --ALPHA SHOULD BE > 1.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JAMES E. GENTLE (2003). 'RANDOM NUMBER GENERATION
!                 AND MONTE CARLO METHODS', SPRINGER-VERLANG, P. 192.
!                 USE HIS DESCRIPTION OF AN ALGORITHM DUE TO DEVROYE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/11
!     ORIGINAL VERSION--NOVEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
!
      DIMENSION U(2)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(ALPHA.LE.1.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ZETA',   &
      ' RANDOM NUMBERS IS NON-POSITIVE')
   15 FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR THE ',   &
      'ZETA RANDOM NUMBERS IS <= 1')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N ZETA DISTRIBUTION RANDOM NUMBERS
!
      NTEMP=2
      DO 100 I=1,N
  199   CONTINUE
        CALL UNIRAN(NTEMP,ISEED,U)
        XTEMP=U(1)**(-1.0/(ALPHA-1.0))
!
        IF(XTEMP.LE.0.0)THEN
          IARG2=INT(XTEMP)
          ARG3=REAL(IARG2)
          ARG4=XTEMP-ARG3
          TERM=ARG3
          IF(ARG4.NE.0.0)TERM=TERM-1.0
        ELSE
          IARG2=INT(XTEMP)
          TERM=REAL(IARG2)
        ENDIF
        XTEMP=TERM
!
        TTEMP=(1.0 + 1.0/XTEMP)**(ALPHA-1.0)
        TERM1=TTEMP/(TTEMP-1.0)
        TERM2=(2.0**(ALPHA-1.0) - 1.0)/(2.0**(ALPHA-1.0)*U(2))
        IF(XTEMP.LE.TERM1*TERM2)THEN
          X(I)=XTEMP
        ELSE
          GO TO 199
        ENDIF
  100 CONTINUE
!
 9999 CONTINUE
!
      RETURN
      END SUBROUTINE ZETRAN
      SUBROUTINE ZETRCH(IC,IC1,IC2)
!
!     PURPOSE--TRANSLATE ANY OF THE 128 ASCII CHARACTERS
!              INTO A 2-CHARACTER REPRESENTATION
!              THAT WILL BE UNDERSTOOD BY A ZETA
!              (MODEL 3600SX AND MODEL 3653SX)
!              GRAPHICS DEVICE,
!              FOR USE WITH A CHARACTER VECTOR (= OP CODE 3) COMMAND
!
!     THE INPUT  CONSISTS OF 1 CHARACTER*1 VARIABLE--
!           IC.
!     THE OUTPUT CONSISTS OF 2 CHARACTER*1 VARIABLES--
!           IC1 AND IC2.
!
!     NOTE--THE ZETA CONVERSION SCHEME IS AS FOLLOWS--
!           TAKE THE INPUT CHARACTER.
!           NOTE THE EBCDIC (UGH!) NUMERIC EQUIVALENT.
!           SPLIT IT INTO 2 5-BIT BYTES.
!           CONVERT EACH BYTE INTO ITS INTEGER EQUIVALENT.
!           APPLY FINAL CONVERSION SCHEME OF
!                0 TO  7 MAPS INTO THE CHARACTERS 0 TO 7
!                8 TO 31 MAPS INTO THE CHARACTERS A TO X
!
!     NOTE--IN GENERAL, THE ZETA ONLY ACCEPTS AS INPUT
!           THE 32 CHARACTERS--0 TO 7 AND A TO X.
!
!     REFERENCE--ZETA REFERENCE MANUAL
!                FUNDAMENTAL PLOTTING ROUTINES (FORTRAN)
!                PAGE A-2.
!     REFERENCE--ZETA USER MANUAL
!                DIGITAL PLOTTER, MODELS 3600SC, 3653SX
!                PAGE B-1.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED--NOVEMBER           1996. LINUX COMPILE PROBLEM
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*1 IC
      CHARACTER*1 IC1
      CHARACTER*1 IC2
!
      CHARACTER*4 ICTAB
      CHARACTER*1 IC1TAB
      CHARACTER*1 IC2TAB
!
      DIMENSION ICTAB(128)
      DIMENSION IC1TAB(128)
      DIMENSION IC2TAB(128)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS----------------------------------------
!
      DATA ICTAB(128),IC1TAB(128),IC2TAB(128)/'NU  ','?','?'/
      DATA ICTAB(  1),IC1TAB(  1),IC2TAB(  1)/'SH  ','?','?'/
      DATA ICTAB(  2),IC1TAB(  2),IC2TAB(  2)/'SX  ','?','?'/
      DATA ICTAB(  3),IC1TAB(  3),IC2TAB(  3)/'EX  ','?','?'/
      DATA ICTAB(  4),IC1TAB(  4),IC2TAB(  4)/'ET  ','?','?'/
      DATA ICTAB(  5),IC1TAB(  5),IC2TAB(  5)/'EQ  ','?','?'/
      DATA ICTAB(  6),IC1TAB(  6),IC2TAB(  6)/'AK  ','?','?'/
      DATA ICTAB(  7),IC1TAB(  7),IC2TAB(  7)/'BL  ','?','?'/
      DATA ICTAB(  8),IC1TAB(  8),IC2TAB(  8)/'BS  ','?','?'/
      DATA ICTAB(  9),IC1TAB(  9),IC2TAB(  9)/'HT  ','?','?'/
      DATA ICTAB( 10),IC1TAB( 10),IC2TAB( 10)/'LF  ','?','?'/
      DATA ICTAB( 11),IC1TAB( 11),IC2TAB( 11)/'VT  ','?','?'/
      DATA ICTAB( 12),IC1TAB( 12),IC2TAB( 12)/'FF  ','?','?'/
      DATA ICTAB( 13),IC1TAB( 13),IC2TAB( 13)/'CR  ','?','?'/
      DATA ICTAB( 14),IC1TAB( 14),IC2TAB( 14)/'SO  ','?','?'/
      DATA ICTAB( 15),IC1TAB( 15),IC2TAB( 15)/'SI  ','?','?'/
!
      DATA ICTAB( 16),IC1TAB( 16),IC2TAB( 16)/'DL  ','?','?'/
      DATA ICTAB( 17),IC1TAB( 17),IC2TAB( 17)/'D1  ','?','?'/
      DATA ICTAB( 18),IC1TAB( 18),IC2TAB( 18)/'D2  ','?','?'/
      DATA ICTAB( 19),IC1TAB( 19),IC2TAB( 19)/'D3  ','?','?'/
      DATA ICTAB( 20),IC1TAB( 20),IC2TAB( 20)/'D4  ','?','?'/
      DATA ICTAB( 21),IC1TAB( 21),IC2TAB( 21)/'NK  ','?','?'/
      DATA ICTAB( 22),IC1TAB( 22),IC2TAB( 22)/'SY  ','?','?'/
      DATA ICTAB( 23),IC1TAB( 23),IC2TAB( 23)/'EB  ','?','?'/
      DATA ICTAB( 24),IC1TAB( 24),IC2TAB( 24)/'CN  ','?','?'/
      DATA ICTAB( 25),IC1TAB( 25),IC2TAB( 25)/'EM  ','?','?'/
      DATA ICTAB( 26),IC1TAB( 26),IC2TAB( 26)/'SB  ','?','?'/
      DATA ICTAB( 27),IC1TAB( 27),IC2TAB( 27)/'EC  ','?','?'/
      DATA ICTAB( 28),IC1TAB( 28),IC2TAB( 28)/'FS  ','?','?'/
      DATA ICTAB( 29),IC1TAB( 29),IC2TAB( 29)/'GS  ','?','?'/
      DATA ICTAB( 30),IC1TAB( 30),IC2TAB( 30)/'RS  ','?','?'/
      DATA ICTAB( 31),IC1TAB( 31),IC2TAB( 31)/'US  ','?','?'/
!
      DATA ICTAB( 32),IC1TAB( 32),IC2TAB( 32)/'    ','4','0'/
      DATA ICTAB( 33),IC1TAB( 33),IC2TAB( 33)/'!   ','2','S'/
      DATA ICTAB( 34),IC1TAB( 34),IC2TAB( 34)/'"   ','3','X'/
      DATA ICTAB( 35),IC1TAB( 35),IC2TAB( 35)/'#   ','3','T'/
      DATA ICTAB( 36),IC1TAB( 36),IC2TAB( 36)/'$   ','2','T'/
      DATA ICTAB( 37),IC1TAB( 37),IC2TAB( 37)/'%   ','3','E'/
      DATA ICTAB( 38),IC1TAB( 38),IC2TAB( 38)/'&   ','2','I'/
      DATA ICTAB( 39),IC1TAB( 39),IC2TAB( 39)/'SQUO','?','?'/
      DATA ICTAB( 40),IC1TAB( 40),IC2TAB( 40)/'(   ','2','F'/
      DATA ICTAB( 41),IC1TAB( 41),IC2TAB( 41)/')   ','2','V'/
      DATA ICTAB( 42),IC1TAB( 42),IC2TAB( 42)/'*   ','2','U'/
      DATA ICTAB( 43),IC1TAB( 43),IC2TAB( 43)/'+   ','2','G'/
      DATA ICTAB( 44),IC1TAB( 44),IC2TAB( 44)/',   ','3','D'/
      DATA ICTAB( 45),IC1TAB( 45),IC2TAB( 45)/'-   ','3','0'/
      DATA ICTAB( 46),IC1TAB( 46),IC2TAB( 46)/'.   ','2','D'/
      DATA ICTAB( 47),IC1TAB( 47),IC2TAB( 47)/'/   ','3','1'/
!
      DATA ICTAB( 48),IC1TAB( 48),IC2TAB( 48)/'0   ','3','I'/
      DATA ICTAB( 49),IC1TAB( 49),IC2TAB( 49)/'1   ','3','J'/
      DATA ICTAB( 50),IC1TAB( 50),IC2TAB( 50)/'2   ','3','K'/
      DATA ICTAB( 51),IC1TAB( 51),IC2TAB( 51)/'3   ','3','L'/
      DATA ICTAB( 52),IC1TAB( 52),IC2TAB( 52)/'4   ','3','M'/
      DATA ICTAB( 53),IC1TAB( 53),IC2TAB( 53)/'5   ','3','N'/
      DATA ICTAB( 54),IC1TAB( 54),IC2TAB( 54)/'6   ','3','O'/
      DATA ICTAB( 55),IC1TAB( 55),IC2TAB( 55)/'7   ','3','P'/
      DATA ICTAB( 56),IC1TAB( 56),IC2TAB( 56)/'8   ','3','Q'/
      DATA ICTAB( 57),IC1TAB( 57),IC2TAB( 57)/'9   ','3','R'/
      DATA ICTAB( 58),IC1TAB( 58),IC2TAB( 58)/':   ','2','H'/
      DATA ICTAB( 59),IC1TAB( 59),IC2TAB( 59)/';   ','2','W'/
      DATA ICTAB( 60),IC1TAB( 60),IC2TAB( 60)/'<   ','2','E'/
      DATA ICTAB( 61),IC1TAB( 61),IC2TAB( 61)/'=   ','3','W'/
      DATA ICTAB( 62),IC1TAB( 62),IC2TAB( 62)/'>   ','3','G'/
      DATA ICTAB( 63),IC1TAB( 63),IC2TAB( 63)/'?   ','3','H'/
!
      DATA ICTAB( 64),IC1TAB( 64),IC2TAB( 64)/'@   ','3','U'/
      DATA ICTAB( 65),IC1TAB( 65),IC2TAB( 65)/'A   ','2','1'/
      DATA ICTAB( 66),IC1TAB( 66),IC2TAB( 66)/'B   ','2','2'/
      DATA ICTAB( 67),IC1TAB( 67),IC2TAB( 67)/'C   ','2','3'/
      DATA ICTAB( 68),IC1TAB( 68),IC2TAB( 68)/'D   ','2','4'/
      DATA ICTAB( 69),IC1TAB( 69),IC2TAB( 69)/'E   ','2','5'/
      DATA ICTAB( 70),IC1TAB( 70),IC2TAB( 70)/'F   ','2','6'/
      DATA ICTAB( 71),IC1TAB( 71),IC2TAB( 71)/'G   ','2','7'/
      DATA ICTAB( 72),IC1TAB( 72),IC2TAB( 72)/'H   ','2','A'/
      DATA ICTAB( 73),IC1TAB( 73),IC2TAB( 73)/'I   ','2','B'/
      DATA ICTAB( 74),IC1TAB( 74),IC2TAB( 74)/'J   ','2','J'/
      DATA ICTAB( 75),IC1TAB( 75),IC2TAB( 75)/'K   ','2','K'/
      DATA ICTAB( 76),IC1TAB( 76),IC2TAB( 76)/'L   ','2','L'/
      DATA ICTAB( 77),IC1TAB( 77),IC2TAB( 77)/'M   ','2','M'/
      DATA ICTAB( 78),IC1TAB( 78),IC2TAB( 78)/'N   ','2','N'/
      DATA ICTAB( 79),IC1TAB( 79),IC2TAB( 79)/'O   ','2','O'/
!
      DATA ICTAB( 80),IC1TAB( 80),IC2TAB( 80)/'P   ','2','P'/
      DATA ICTAB( 81),IC1TAB( 81),IC2TAB( 81)/'Q   ','2','Q'/
      DATA ICTAB( 82),IC1TAB( 82),IC2TAB( 82)/'R   ','2','R'/
      DATA ICTAB( 83),IC1TAB( 83),IC2TAB( 83)/'S   ','3','2'/
      DATA ICTAB( 84),IC1TAB( 84),IC2TAB( 84)/'T   ','3','3'/
      DATA ICTAB( 85),IC1TAB( 85),IC2TAB( 85)/'U   ','3','4'/
      DATA ICTAB( 86),IC1TAB( 86),IC2TAB( 86)/'V   ','3','5'/
      DATA ICTAB( 87),IC1TAB( 87),IC2TAB( 87)/'W   ','3','6'/
      DATA ICTAB( 88),IC1TAB( 88),IC2TAB( 88)/'X   ','3','7'/
      DATA ICTAB( 89),IC1TAB( 89),IC2TAB( 89)/'Y   ','3','A'/
      DATA ICTAB( 90),IC1TAB( 90),IC2TAB( 90)/'Z   ','3','B'/
      DATA ICTAB( 91),IC1TAB( 91),IC2TAB( 91)/'[   ','1','N'/
!LINX FOLLOWING LINE MODIFIED FOR LINIX G77 COMPILER NOVEMBER 1996
!LINX DATA ICTAB( 92),IC1TAB( 92),IC2TAB( 92)/'\   ','1','P'/
      DATA IC1TAB( 92),IC2TAB( 92)/'1','P'/
      DATA ICTAB( 93),IC1TAB( 93),IC2TAB( 93)/']   ','1','O'/
      DATA ICTAB( 94),IC1TAB( 94),IC2TAB( 94)/'CARA','0','K'/
      DATA ICTAB( 95),IC1TAB( 95),IC2TAB( 95)/'_   ','?','?'/
!
      DATA ICTAB( 96),IC1TAB( 96),IC2TAB( 96)/'`   ','3','V'/
      DATA ICTAB( 97),IC1TAB( 97),IC2TAB( 97)/'a   ','4','1'/
      DATA ICTAB( 98),IC1TAB( 98),IC2TAB( 98)/'b   ','4','2'/
      DATA ICTAB( 99),IC1TAB( 99),IC2TAB( 99)/'c   ','4','3'/
      DATA ICTAB(100),IC1TAB(100),IC2TAB(100)/'d   ','4','4'/
      DATA ICTAB(101),IC1TAB(101),IC2TAB(101)/'e   ','4','5'/
      DATA ICTAB(102),IC1TAB(102),IC2TAB(102)/'f   ','4','6'/
      DATA ICTAB(103),IC1TAB(103),IC2TAB(103)/'g   ','4','7'/
      DATA ICTAB(104),IC1TAB(104),IC2TAB(104)/'h   ','4','A'/
      DATA ICTAB(105),IC1TAB(105),IC2TAB(105)/'i   ','4','B'/
      DATA ICTAB(106),IC1TAB(106),IC2TAB(106)/'j   ','4','J'/
      DATA ICTAB(107),IC1TAB(107),IC2TAB(107)/'k   ','4','K'/
      DATA ICTAB(108),IC1TAB(108),IC2TAB(108)/'l   ','4','L'/
      DATA ICTAB(109),IC1TAB(109),IC2TAB(109)/'m   ','4','M'/
      DATA ICTAB(110),IC1TAB(110),IC2TAB(110)/'n   ','4','N'/
      DATA ICTAB(111),IC1TAB(111),IC2TAB(111)/'o   ','4','O'/
!
      DATA ICTAB(112),IC1TAB(112),IC2TAB(112)/'p   ','4','P'/
      DATA ICTAB(113),IC1TAB(113),IC2TAB(113)/'q   ','4','Q'/
      DATA ICTAB(114),IC1TAB(114),IC2TAB(114)/'r   ','4','R'/
      DATA ICTAB(115),IC1TAB(115),IC2TAB(115)/'s   ','5','2'/
      DATA ICTAB(116),IC1TAB(116),IC2TAB(116)/'t   ','5','3'/
      DATA ICTAB(117),IC1TAB(117),IC2TAB(117)/'u   ','5','4'/
      DATA ICTAB(118),IC1TAB(118),IC2TAB(118)/'v   ','5','5'/
      DATA ICTAB(119),IC1TAB(119),IC2TAB(119)/'w   ','5','6'/
      DATA ICTAB(120),IC1TAB(120),IC2TAB(120)/'x   ','5','7'/
      DATA ICTAB(121),IC1TAB(121),IC2TAB(121)/'y   ','5','A'/
      DATA ICTAB(122),IC1TAB(122),IC2TAB(122)/'z   ','5','B'/
      DATA ICTAB(123),IC1TAB(123),IC2TAB(123)/'{   ','1','1'/
      DATA ICTAB(124),IC1TAB(124),IC2TAB(124)/'|   ','2','H'/
      DATA ICTAB(125),IC1TAB(125),IC2TAB(125)/'}   ','1','0'/
      DATA ICTAB(126),IC1TAB(126),IC2TAB(126)/'~   ','0','W'/
      DATA ICTAB(127),IC1TAB(127),IC2TAB(127)/'DT  ','?','?'/
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRCH')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF ZETRCH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IC
   52 FORMAT('IC = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!LINX FOLLOWING LINE TO ACCOMODATE LINUX G77 COMPILER  NOVEMBER 1996
      CALL DPCONA(92,ICTAB(92))
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  DETERMINE THE ASCII NUMERIC EQUIVALENT OF THE   **
!               **  INPUT CHARACTER.                                **
!               **  THEN DO A TABLE LOOK-UP TO EXTRACT              **
!               **  THE 2 CODED CHARACTERS THAT THE ZETA EXPECTS.   **
!               ******************************************************
!
!CCCC INDEX=ICHAR(IC)
      CALL DPCOAN(IC,INDEX)
      IF(INDEX.LE.0)INDEX=0
      IF(INDEX.GE.128)INDEX=0
      IC1=IC1TAB(INDEX)
      IC2=IC2TAB(INDEX)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRCH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF ZETRCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERRG4,IC,IC1,IC2,INDEX
 9012   FORMAT('IERRG4,IC,IC1,IC2,INDEX = ',A4,2X,3(A1,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICTAB(INDEX),IC1TAB(INDEX),IC2TAB(INDEX)
 9014   FORMAT('ICTAB(INDEX),IC1TAB(INDEX),IC2TAB(INDEX) = ',   &
               A4,2X,A1,2X,A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE ZETRCH
      SUBROUTINE ZETRPT(IXC,IYC,ICSTR,NCSTR,ISUBN0)
!
!     PURPOSE--TRANSLATE AN INTEGER PAIR OF COORDINATES
!              INTO A PACKED CHARACTER REPRESENTATION
!              THAT WILL BE UNDERSTOOD BY A ZETA
!              (MODEL 3600SX AND MODEL 3653SX)
!              GRAPHICS DEVICE.
!
!     NOTE--THE RESULTING PACKED WORDS
!           WILL BE PLACED IN SPECIFIC ELEMENTS
!           OF THE CHARACTER*130 VARIABLE ICSTR(.:.).
!           THE VALUE OF THE VARIABLE    NCSTR
!           REPRESENTS THE NUMBER OF ELEMENTS IN ICSTR(.:.)
!           THAT HAVE ALREADY BEEN FILLED.
!           THE RESULTRING CHARACTER STING WILL GO INTO
!           THE NEXT AVAILABLE ELEMENTS OF ICSTR(.:.)
!           AND THE VALUE OF    NCSTR    WILL BE
!           UPDATED ACCORDINGLY.
!     NOTE--THE 32 VALID INPUT VALUES THAT THE ZETA EXPECTS ARE--
!           0 TO 7 AND A TO X
!     REFERENCE--ZETA MANUAL, PAGE B-1.
!
!     NOTE--THE ZETA HAS AN ACCURACY OF 1/400 OF AN INCH
!           THE RAW UNITS ARE IN INCHES, BUT THE INPUT TO THIS
!           SUBROUTINE IS INCHES X 400 AND THEN ROUNDED TO CLOSEST INTEGER.
!
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED ZETRPT
!                    (AND THEREBY HAVE WALKBACK INFORMATION).
!     REFERENCE--4105 PROGRAMMER'S REFERENCE MANUAL
!                PAGE 5-4
!     REFERENCE--MAHLON KELLY, BYTE, OCTOBER 1983,
!                PAGES 439 TO 442.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 ISUBN0
!
      CHARACTER*130 ICSTR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS----------------------------------------
!
      DATA K5/32/
      DATA K10/1024/
      DATA K15/32768/
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRPT')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF ZETRPT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISUBN0
   52 FORMAT('ISUBN0 (NAME OF THE CALLING SUBROUTINE) = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IXC,IYC
   53 FORMAT('IXC,IYC = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)K5,K10,K15
   55 FORMAT('K5,K10,K15 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)IGUNIT
   56 FORMAT('IGUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)NCSTR
   63 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCSTR.LE.0)GO TO 67
      DO 65 I=1,NCSTR
!CCCC IASCNE=ICHAR(ICSTR(I:I))
      CALL DPCOAN(ICSTR(I:I),IASCNE)
      WRITE(ICOUT,66)I,ICSTR(I:I),IASCNE
   66 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
   67 CONTINUE
      WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
      IVX=IXC
      IVY=IYC
      IF(IVX.LT.0)IVX=0
      IF(IVY.LT.0)IVY=0
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  FORM THE CODED X VALUE.                         **
!               **  PRODUCE 4 5-BIT BYTES.                          **
!               **  SPLIT THE 20 RIGHT-MOST BITS OF THE BINARY      **
!               **  REPRESENTATION FOR THE INTEGER                  **
!               **  INTO 40 5-BIT BYTES.                            **
!               **  THEN CONVERT EACH BYTE INTO THE ASCII           **
!               **  NUMERIC EQUIVALENT OF THE 32 SPECIAL            **
!               **  CHARACTERS THAT THE ZETA EXPECTS, NAMELY,       **
!               **  0, 1, 2, ..., 7, A, B, C, ..., W, X.            **
!               **  FORM THE LEFT-MOST 5-BIT BYTE--                 **
!               **       SHIFT THE X VALUE TO THE RIGHT 15 PLACES;  **
!               **       THEN KEEP ONLY THE RIGHT 5 PLACES;         **
!               **  FORM THE LEFT-MIDDLE 5-BIT BYTE--               **
!               **       SHIFT THE X VALUE TO THE RIGHT 10 PLACES;  **
!               **       THEN KEEP ONLY THE RIGHT 5 PLACES;         **
!               **  FORM THE RIGHT-MIDDLE 5-BIT BYTE--              **
!               **       SHIFT THE X VALUE TO THE RIGHT 5 PLACES;   **
!               **       THEN KEEP ONLY THE RIGHT 5 PLACES;         **
!               **  FORM THE LEFT-MOST 5-BIT BYTE--                 **
!               **       SHIFT THE X VALUE TO THE RIGHT 0 PLACES;   **
!               **       THEN KEEP ONLY THE RIGHT 5 PLACES;         **
!               ******************************************************
!
!
      IHOLD=MOD(IVX/K15,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVX/K10,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVX/K5,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVX,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
!               *******************************************
!               **  STEP 2--                             **
!               **  FORM THE CODED Y VALUE.              **
!               **  PRODUCE 4 5-BIT BYTES.               **
!               **  USE THE SAME PROCEDURE AS IN STEP 1  **
!               *******************************************
!
      IHOLD=MOD(IVY/K15,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVY/K10,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVY/K5,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
      IHOLD=MOD(IVY,K5)
      IARG=IHOLD+48
      IF(IHOLD.GE.8)IARG=IHOLD+57
      NCSTR=NCSTR+1
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IARG)
      CALL DPCONA(IARG,ICSTR(NCSTR:NCSTR))
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRPT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF ZETRPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IXC,IYC,IVX,IVY
 9012   FORMAT('IXC,IYC,IVX,IVY = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)K5,K10,K15
 9015   FORMAT('K5,K10,K15 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IGUNIT,IARG,NCSTR
 9016   FORMAT('IGUNIT,IARG,NCSTR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GE.1)THEN
          DO 9025 I=1,NCSTR
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,9026)I,ICSTR(I:I),IASCNE
 9026       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
        WRITE(ICOUT,9029)IBUGG4,ISUBG4,IERRG4
 9029   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE ZETRPT
      SUBROUTINE ZIPCDF(X,ALPHA,N,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE ZIPF
!              DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND N.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 1.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
!              p(X;ALPHA,N)=Hn(X,ALPHA)/Hn(N,ALPHA)    X=1,2,3,...
!                                                      ALPHA > 1
!              WITH Hn DENOTING THE GENERALIZED HARMONIC NUMBER
!              FUNCTION (Hn(N,ALPHA) = SUM[i=1 to N][1/i**ALPHA]).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --N      = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE ZIPF
!             DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND N
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --ALPHA > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS", SECOND EDITION,
!                 WILEY, PP. 465-471.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!     UPDATED  VERSION--DECEMBER  2006. HANDLE ALPHA = 1, ALPHA = 2
!                                       AS SPECIAL CASES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
!------------------------------------------------------------------
!
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DH1
      DOUBLE PRECISION DH2
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DPSI
      DOUBLE PRECISION DX
      DOUBLE PRECISION DN
!
      EXTERNAL DPSI
!
!-----COMMON-------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS----------------------------------------------
!
      DATA DPI / 3.14159265358979D+00/
!
!-----START POINT--------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5)
      DX=DBLE(X)
      CDF=0.0
      IF(ALPHA.LT.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR ',   &
               'ZIPCDF IS < 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(DBLE(N).GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,24)
   24   FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (N) TO ZIPCDF ',   &
               'IS GREATER THAN THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.1)THEN
        CDF=0.0
        GO TO 9999
      ELSEIF(DX.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZIPCDF ',   &
               'IS GREATER THAN THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      IF(IX.GE.N)THEN
        CDF=1.0
        GO TO 9999
      ENDIF
!
!     HANDLE ALPHA = 1 AND ALPHA = 2 CASES AS SPECIAL CASES.
!     THESE ARE KNOWN AS THE ESTOUP AND LOTKA DISTRIBUTIONS,
!     RESPECTIVELY.
!
      DN=DBLE(N)
      IF(ALPHA.EQ.1.0)THEN
        DCDF=0.0D0
        DO 100 I=1,IX
          DPDF=1.0D0/(-DX*(DPSI(1.0D0) - DPSI(DN+1.0D0)))
          DCDF=DCDF + DPDF
  100   CONTINUE
        CDF=REAL(DCDF)
      ELSEIF(ALPHA.EQ.2.0)THEN
        DPDF=DLOG(6.0D0) - 2.0D0*DLOG(DPI) - 2.0D0*DLOG(DX)
        DPDF=DEXP(DPDF)
        DCDF=0.0D0
        DO 200 I=1,IX
          DPDF=1.0D0/(-DX*(DPSI(1.0D0) - DPSI(DN+1.0D0)))
          DCDF=DCDF + DPDF
  200   CONTINUE
        CDF=REAL(DCDF)
      ELSE
        DALPHA=DBLE(ALPHA)
        CALL HNM(N,DALPHA,DH1)
        CALL HNM(IX,DALPHA,DH2)
        DCDF=DH2/DH1
        CDF=REAL(DCDF)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZIPCDF
      SUBROUTINE ZIPPDF(X,ALPHA,N,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE ZIPF
!              DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND N.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 1.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              p(X;ALPHA,N)=1/[Hn(N,ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                       ALPHA > 1
!              WITH Hn DENOTING THE GENERALIZED HARMONIC NUMBER
!              FUNCTION (= SUM[i=1 to N][1/i**ALPHA].
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --N      = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE ZIPF
!             DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND N
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --ALPHA > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS", SECOND EDITION,
!                 WILEY, PP. 465-471.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!     UPDATED  VERSION--DECEMBER  2006. HANDLE ALPHA = 1, ALPHA = 2
!                                       AS SPECIAL CASES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
!------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DPSI
      DOUBLE PRECISION DN
!
      EXTERNAL DPSI
!
!-----COMMON-------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS----------------------------------------------
!
      DATA DPI / 3.14159265358979D+00/
!
!-----START POINT--------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      IX=INT(X+0.5)
      DX=DBLE(X)
      IF(ALPHA.LT.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR ',   &
               'ZIPPDF IS < 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(DBLE(N).GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,24)
   24   FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (N) TO ',   &
               'ZIPPDF IS GREATER THAN THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZIPPDF ',   &
               'IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(DX.GT.DBLE(N))THEN
        WRITE(ICOUT,34)
   34   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZIPPDF ',   &
               'IS GREATER THAN THE SECOND SHAPE PARAMETER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(DX.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZIPPDF ',   &
               'IS GREATER THAN THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      DX=DBLE(IX)
      DALPHA=DBLE(ALPHA)
      DN=DBLE(N)
!
!     HANDLE ALPHA = 1 AND ALPHA = 2 CASES AS SPECIAL CASES.
!     THESE ARE KNOWN AS THE ESTOUP AND LOTKA DISTRIBUTIONS,
!     RESPECTIVELY.
!
      IF(ALPHA.EQ.1)THEN
        DPDF=1.0D0/(-DX*(DPSI(1.0D0) - DPSI(DN+1.0D0)))
        PDF=REAL(DPDF)
      ELSEIF(ALPHA.EQ.2)THEN
        DPDF=DLOG(6.0D0) - 2.0D0*DLOG(DPI) - 2.0D0*DLOG(DX)
        DPDF=DEXP(DPDF)
        PDF=REAL(DPDF)
      ELSE
        CALL HNM(N,DALPHA,DZETA)
        DPDF=DLOG(1.0D0) - DLOG(DZETA) - DALPHA*DLOG(DX)
        DPDF=DEXP(DPDF)
        PDF=REAL(DPDF)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZIPPDF
      SUBROUTINE ZIPPPF(P,ALPHA,N,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
!              FOR THE ZIPF DISTRIBUTION WITH SINGLE PRECISION
!              SHAPE PARAMETERS ALPHA AND N.
!              THIS DISTRIBUTION IS DEFINED FOR 0 <= P <= 1.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!              p(X;ALPHA,N)=1/[Hn(N,ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                       ALPHA > 1
!              WITH Hn DENOTING THE GENERALIZED HARMONIC NUMBER
!              FUNCTION (= SUM[i=1 to N][1/i**ALPHA].
!
!              WE CURRENTLY COMPUTE THE PERCENT POINT FUNCTION
!              VIA BRUTE FORCE.  THAT IS, WE COMPUTE THE
!              CUMULATIVE DISTRIBUTION FUNCTION UNTIL IT EXCEEDS
!              THE SPECIFIED VALUE OF P.
!
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                0 <= P < 1.
!                     --ALPHA  = THE SINGLE PRECISION VALUE
!                                OF THE FIRST SHAPE PARAMETER.
!                     --N      = THE SINGLE PRECISION VALUE
!                                OF THE SECOND SHAPE PARAMETER.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF
!             FOR THE ZETA DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--0 <= P < 1
!                 --ALPHA > 1
!     OTHER DATAPAC   SUBROUTINES NEEDED--HNM.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS", SECOND EDITION, WILEY,
!                 PP. 465-471.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!     UPDATED  VERSION--DECEMBER  2006. HANDLE ALPHA = 1, ALPHA = 2
!                                       AS SPECIAL CASES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DP
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DH1
      DOUBLE PRECISION DH2
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DPSI
      DOUBLE PRECISION DX
      DOUBLE PRECISION DN
      DOUBLE PRECISION DEPS
!
      EXTERNAL DPSI
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DPI / 3.14159265358979D+00/
!
!-----START POINT---------------------------------------------------
!
      PPF=0.0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ALPHA.LT.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO ZIPPPF ',   &
               'IS < 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(DBLE(N).GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,24)
   24   FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (N) TO ',   &
               'ZIPPPF IS GREATER THAN THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO ZIPPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      IF(P.LE.0.0)THEN
        PPF=0.0
        GO TO 9999
      ELSEIF(P.GE.1.0)THEN
        PPF=REAL(N)
        GO TO 9999
      ENDIF
!
      DALPHA=DBLE(ALPHA)
      DP=DBLE(P)
      DN=DBLE(N)
      DH2=0.0D0
      IF(ALPHA.EQ.1.0)THEN
        CONTINUE
      ELSEIF(ALPHA.EQ.2.0)THEN
        CONTINUE
      ELSE
        CALL HNM(N,DALPHA,DH1)
      ENDIF
      DEPS=1.0D-7
!
!     COMPUTE PDF FOR X = 1
!
      I=1
      IF(ALPHA.EQ.1.0)THEN
        X=1.0
        CALL ZIPPDF(X,ALPHA,N,PDF)
        DCDF=DBLE(PDF)
      ELSEIF(ALPHA.EQ.2.0)THEN
        X=1.0
        CALL ZIPPDF(X,ALPHA,N,PDF)
        DCDF=DBLE(PDF)
      ELSE
        DCDF=1.0D0/DH1
      ENDIF
!
      IF(DCDF.GE.DP-DEPS)THEN
        PPF=1.0
        GO TO 9999
      ENDIF
!
      IF(ALPHA.EQ.1.0)THEN
        CONTINUE
      ELSEIF(ALPHA.EQ.2.0)THEN
        CONTINUE
      ELSE
        DH2=DLOG(1.0D0) - DLOG(DH1)
       ENDIF
!
  100 CONTINUE
        I=I+1
        IF(I.GE.N)THEN
          PPF=REAL(N)
          GO TO 9999
        ENDIF
        IF(DBLE(I).GE.DBLE(I1MACH(9)))THEN
          WRITE(ICOUT,55)
   55     FORMAT('***** ERROR--THE COMPUTED PERCENT POINT VALUE ',   &
                 'EXCEEDS THE LARGEST MACHINE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          PPF=0.0
          GO TO 9999
        ENDIF
        IF(ALPHA.EQ.1.0)THEN
          DX=DBLE(I)
          DPDF=1.0D0/(-DX*(DPSI(1.0D0) - DPSI(DN+1.0D0)))
          DCDF=DCDF + DPDF
        ELSEIF(ALPHA.EQ.2.0)THEN
          DX=DBLE(I)
          DPDF=DLOG(6.0D0) - 2.0D0*DLOG(DPI) - 2.0D0*DLOG(DX)
          DPDF=DEXP(DPDF)
          DCDF=DCDF + DPDF
        ELSE
          DPDF=DH2 - DALPHA*DLOG(DBLE(I))
          DCDF=DCDF + DEXP(DPDF)
        ENDIF
        IF(DCDF.GE.DP-DEPS)THEN
          PPF=REAL(I)
          GO TO 9999
        ENDIF
      GO TO 100
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE ZIPPPF
      SUBROUTINE ZIPRAN(N,ALPHA,NPAR,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE ZIPF DISTRIBUTION
!              WITH SHAPE PARAMETERS ALPHA AND N.
!              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE INTEGERS
!              X, AND HAS THE PROBABILITY MASS FUNCTION
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!              p(X;ALPHA,N)=1/[Hn(N,ALPHA)*X**ALPHA]    X=1,2,3,...
!                                                       ALPHA > 1
!              WITH Hn DENOTING THE GENERALIZED HARMONIC NUMBER
!              FUNCTION (= SUM[i=1 to N][1/i**ALPHA].
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --ALPHA  = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER, ALPHA > 1
!                     --NPAR   = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER N
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE ZIPF DISTRIBUTION
!             WITH SHAPE LENGTH PARAMETERS ALPHA AND N.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --ALPHA SHOULD BE > 1, NPAR IS A POSITIVE INTEGER
!                   (LESS THAN MACHINE MAXIMUM INTEGER).
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JAMES E. GENTLE (2003). 'RANDOM NUMBER GENERATION
!                 AND MONTE CARLO METHODS', SPRINGER-VERLANG, P. 192.
!                 USE HIS DESCRIPTION OF AN ALGORITHM DUE TO DEVROYE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DH1
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DP
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(ALPHA.LE.1.0)THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9999
      ENDIF
      IF(DBLE(NPAR).GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)REAL(NPAR)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9999
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ZIPF',   &
      ' RANDOM NUMBERS IS NON-POSITIVE')
   11 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
      ' ZIPPPF SUBROUTINE IS <= 1')
   24 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (N) TO THE ',   &
      'ZIPPPF SUBROUTINE IS GREATER THAN THE LARGEST MACHINE INTEGER')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N ZIPF DISTRIBUTION RANDOM NUMBERS
!
      CALL UNIRAN(N,ISEED,X)
      DALPHA=DBLE(ALPHA)
      CALL HNM(NPAR,DALPHA,DH1)
!
      DO 100 I=1,N
        DP=DBLE(X(I))
        DSUM=0.0D0
        DO 200 J=1,NPAR
          DSUM=DSUM + (1.0D0/DBLE(J)**DALPHA)/DH1
          IF(DSUM.GE.DP)THEN
            X(I)=REAL(J)
            GO TO 299
          ENDIF
  200   CONTINUE
        X(I)=REAL(NPAR)
  299   CONTINUE
  100 CONTINUE
!
 9999 CONTINUE
!
      RETURN
      END SUBROUTINE ZIPRAN
      double precision function ztran (var)
!
! *   AUTHORS: Necip Doganaksoy and Wayne Nelson
! *   PURPOSE: Maximum likelihood fitting of the power-normal and
! *            -lognormal models to censored life or strength data
! *            from specimens of various sizes
! *   DOCUMENTATION: Wayne Nelson and Necip Doganaksoy, "A Computer
! *                  Program POWNOR for Fitting the Power-Normal and
! *                  -Lognormal Models to Life or Strength Data from
! *                  Specimens of Various Sizes", NISTIR 4760, 3/1992.
! *   PROJECT: 1990-91 ASA/NIST/NSF Fellowship
!
! TRANSFORMATION OF OBSERVATIONS TO AVOID NUMERICAL PROBLEMS DURING
! OPTIMIZATION
!
      implicit double precision (a-h,o-z)
      logical trans
      common /pnrlst/trans
!
      data one,xkp,xkm,txkp,txkm / 1.0d0,4.0d0,-4.0d0,7.9d0,-7.9d0/
!
      if (.not.trans)then
         ztran=var
         if (var.gt.txkp)ztran=txkp
         if (var.lt.txkm)ztran=txkm
      elseif (var.gt.xkp)then
         zmxkp=var-xkp
         ztran=xkp+zmxkp/(one+zmxkp/xkp)
      elseif (var.gt.xkm)then
         ztran=var
         if (var.gt.txkp)ztran=txkp
         if (var.lt.txkm)ztran=txkm
      else
         zmxkm=var-xkm
         ztran=xkm+zmxkm/(one+zmxkm/xkm)
      endif
!
      return
      end
      SUBROUTINE ZSCORE(X,ULAB,XREF,UREF,SIGMA,N,ICASE,IWRITE,Y,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES SEVERAL Z-SCORE VARIANTS
!              DEFINED BY THE ISO-13528 STANDARD.
!
!              1) Z-SCORE:
!
!                   Y(i) = (X(I) - XREF)/SIGMA
!
!                 WHERE XREF IS THE "ASSIGNED VALUE" AND SIGMA IS
!                 THE STANDARD DEVIATION OF THE PROFICIENCY ASSESSMENT.
!
!              2) Z'-SCORE:
!
!                   Y(i) = (X(I) - XREF)/SQRT(SIGMA**2 + U(ref)**2)
!
!                 WHERE XREF IS THE "ASSIGNED VALUE", SIGMA IS
!                 THE STANDARD DEVIATION OF THE PROFICIENCY ASSESSMENT,
!                 AND U(ref) IS THE STANDARD UNCERTAINTY OF THE
!                 ASSIGNED VALUE.
!
!              3) ZETA-SCORE
!
!                    ZETA(i) = (X(i) - XREF)/SQRT(U(lab)**2 + U(ref)**2)
!
!                  WHERE
!
!                    XREF    = THE ASSIGNED VALUE (DETERMINED FROM A
!                              REFERENCE LABORATORY)
!                    U(ref)  = THE STANDARD UNCERTAINTY FOR THE ASSIGNED
!                              VALUE
!                    U(lab)  = THE STANDARD UNCERTAINTY FOR THE LAB
!
!                  NOTE THAT SINCE THE LAB UNCERTAINTY CAN VARY DEPENDING
!                  ON THE LAB, THIS IS INPUT AS A VECTOR RATHER THAN A
!                  PARAMETER.  XREF AND UREF ARE INPUT AS PARAMETERS SINCE
!                  THEY ARE FIXED FOR ALL LABS.  ULAB EFFECTIVELY ACTS AS
!                  AS A PROXY FOR LAB-ID, SO NO NEED TO INPUT THIS AS A
!                  SEPARATE VALUE.
!
!                  NOTE ALSO THAT THE ZETA-SCORE IS SIMILAR TO THE
!                  EN NUMBERS.  THE DISTINCTION IS THAT ZETA-SCORES
!                  USE STANDARD UNCERTAINTIES WHILE THE EN NUMBERS
!                  USE EXPANDED UNCERTAINTIES.
!
!              4) Ez SCORES
!
!                    Ez-(i) = (X(i) - (X-U(ref)))/U(lab)
!                    Ez+(i) = (X(i) - (X+U(ref)))/U(lab)
!
!                 WHERE
!
!                    X       = THE ASSIGNED VALUE
!                    U(ref)  = THE EXPANDED UNCERTAINTY FOR THE ASSIGNED
!                              VALUE
!                    U(lab)  = THE EXPANDED UNCERTAINTY FOR THE LAB
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --ULAB   = THE SINGLE PRECISION VECTOR OF
!                                LAB UNCERTAINTIES
!                     --XREF   = THE SINGLE PRECISION VALUE CONTAINING
!                                THE ASSIGNED VALUE
!                     --UREF   = THE SINGLE PRECISION VALUE CONTAINING
!                                THE UNCERTAINTY FOR THE
!                                ASSIGNED VALUE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF THE
!                                COMPUTED VALUES.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF THE SAMPLE
!             Z-SCORE VALUES.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--ISO 13528, FIRST EDITION, STATISTICAL METHODS FOR USE
!                IN PROFICIENCY TESTING BY INTERLABORATORY COMPARISONS,
!                2005, PP. 25-30.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012.1
!     ORIGINAL VERSION--JANUARY   2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*10 INAME
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION ULAB(*)
      DIMENSION Y(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='EN  '
      ISUBN2='    '
!
      IERROR='NO'
      IF(ICASE.EQ.1)THEN
        INAME='Z-SCORE'
      ELSEIF(ICASE.EQ.2)THEN
        INAME='ZP-SCORE'
      ELSEIF(ICASE.EQ.3)THEN
        INAME='ZETA-SCORE'
      ELSEIF(ICASE.EQ.4)THEN
        INAME='Ez- SCORE'
      ELSEIF(ICASE.EQ.5)THEN
        INAME='Ez+ SCORE'
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CORE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF EN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ICASE,XREF,UREF
   52   FORMAT('IBUGA3,N,ICASE,XREF,UREF = ',A4,2X,2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),ULAB(I)
   56     FORMAT('I,X(I),ULAB(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)INAME
  111   FORMAT('***** ERROR IN ISO 13528 ',A10,'--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE VARIABLE FOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)
  114   FORMAT('      WHICH THE Z-SCORE IS TO BE COMPUTED MUST BE AT ',   &
               'LEAST 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(UREF.LE.0.0 .AND. ICASE.GE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)INAME
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,122)
  122   FORMAT('      THE REFERENCE UNCERTAINTY IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,127)UREF
  127   FORMAT('      THE REFERENCE UNCERTAINTY = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(SIGMA.LE.0.0 .AND. ICASE.LE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)INAME
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE PROFICIENCY ASSESSMENT STANDARD DEVIATION ',   &
               '(SIGMA) IS NOT POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)SIGMA
  137   FORMAT('      SIGMA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************
!               **  STEP 2A-               **
!               **  COMPUTE THE Z-SCORE    **
!               *****************************
!
      IF(ICASE.EQ.1)THEN
        DO 210 I=1,N
          Y(I)=(X(I) - XREF)/SIGMA
  210   CONTINUE
!
!               *****************************
!               **  STEP 2B-               **
!               **  COMPUTE THE Z'-SCORE   **
!               *****************************
!
      ELSEIF(ICASE.EQ.2)THEN
        DO 220 I=1,N
          Y(I)=(X(I) - XREF)/SQRT(SIGMA**2 + UREF**2)
  220   CONTINUE
!
!               *****************************
!               **  STEP 2C-               **
!               **  COMPUTE THE ZETA-SCORE **
!               *****************************
!
      ELSEIF(ICASE.EQ.3)THEN
        DO 230 I=1,N
          UTEMP1=ULAB(I)
          UTEMP2=UTEMP1**2 + UREF**2
!
          IF(UTEMP1.LT.0.0 .OR. UTEMP2.LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)INAME
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,232)
  232       FORMAT('      EITHER THE LAB STANDARD UNCERTAINTY IS ',   &
                   'NEGATIVE OR')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,234)
  234       FORMAT('      BOTH THE LAB STANDARD UNCERTAINTY AND THE ',   &
                   'STANDARD')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,236)
  236       FORMAT('      REFERENCE UNCERTAINTY ARE ZERO.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,127)UREF
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,237)ULAB(I)
  237       FORMAT('      THE LAB EXPANDED UNCERTAINTY = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          Y(I)=(X(I) - XREF)/SQRT(UTEMP1**2 + UREF**2)
  230   CONTINUE
!
!               *****************************
!               **  STEP 2D-               **
!               **  COMPUTE THE Ez--SCORE  **
!               *****************************
!
      ELSEIF(ICASE.EQ.4)THEN
        DO 240 I=1,N
!
          IF(ULAB(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)INAME
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,242)
  242       FORMAT('      THE LAB EXPANDED UNCERTAINTY IS NEGATIVE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,247)ULAB(I)
  247       FORMAT('      THE LAB EXPANDED UNCERTAINTY = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          Y(I)=(X(I) - (XREF - UREF))/ULAB(I)
  240   CONTINUE
!
!               *****************************
!               **  STEP 2E-               **
!               **  COMPUTE THE Ez+-SCORE  **
!               *****************************
!
      ELSEIF(ICASE.EQ.5)THEN
        DO 250 I=1,N
!
          IF(ULAB(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)INAME
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,252)
  252       FORMAT('      THE LAB EXPANDED UNCERTAINTY IS NEGATIVE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,257)ULAB(I)
  257       FORMAT('      THE LAB EXPANDED UNCERTAINTY = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          Y(I)=(X(I) - (XREF + UREF))/ULAB(I)
  250   CONTINUE
!
      ENDIF
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)INAME,N
  811   FORMAT('THE NUMBER OF ISO-13528 ',A9,' VALUES GENERATED = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CORE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF ZSCORE--')
        CALL DPWRST('XXX','BUG ')
        DO 9012 I=1,N
          WRITE(ICOUT,9015)I,X(I),ULAB(I),Y(I)
 9015     FORMAT('I,X(I),ULAB(I),Y(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9012   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE ZSCORE

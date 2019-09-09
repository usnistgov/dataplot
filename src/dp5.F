      SUBROUTINE CNP(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,IWRITE,ICNPKD,
     1               XCNP,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE NON-PARAMETRIC CP
C              (PROCESS CAPABILITY INDEX) OF THE DATA IN THE INPUT
C              VECTOR X.
C                 CNP = (ENGUSL - ENGLSL)/(P(.99865) - P(0.00135))
C           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
C           COMPARABLE TO THE NORMAL-BASED CP STATISTIC (+/- 3*SIGMA).
C           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND HAS THE
C           FORMULA
C                 CNP = (ENGUSL - ENGLSL)/(P(.995) - P(0.005))
C     NOTE--CP IS A MEASURE OF PROCESS PRECISION--
C           IT CONTAINS NO BIAS INFORMATION.
C     NOTE--THE CP INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CP WHICH ARE
C           LARGE (ABOVE 2); VALUES OF CP FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CP     = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CP
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CP INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-28999
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.4
C     ORIGINAL VERSION--APRIL     2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL NUM
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CNP '
      ISUBN2='    '
      IERROR='NO'
C
      XCNP=0.0
      DMEAN=0.0D0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CNP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,MAXNXT,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,MAXNXT,ENGUSL,ENGLSL = ',A4,2X,2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CP  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CNP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ****************************************
C               **  STEP 2--                          **
C               **  COMPUTE THE MEDIAN AND PERCENTILE **
C               **  POINTS                            **
C               ****************************************
C
      IWRITE='OFF'
      IF(ICNPKD.EQ.'PEAR')THEN
        P=99.865
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.135
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ELSE
        P=99.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ENDIF
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CNP RATIO                       **
C               **************************************************
C
      NUM=ENGUSL-ENGLSL
      IF(NUM.LE.0.0)NUM=0.0D0
      DEN=P995-P005
      IF(DEN.GT.0.0)XCNP=NUM/DEN
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCNP
  811   FORMAT('THE CNP OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)NUM,DEN,XCNP
 9017   FORMAT('NUM,DEN,XCNP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CNPK(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,IWRITE,ICNPKD,
     1                XCNPK,
     1                IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPK
C              (PROCESS CAPABILITY INDEX) OF THE DATA IN THE INPUT
C              VECTOR X.
C              CNPK = MIN(A,B)
C              WHERE:
C                  A = (USL-MEDIAN)/(P(.995)-MEDIAN)
C                  B = (MEDIAN-LSL)/(MEDIAN-P(.005))
C                  P = THE PERCENTILE FUNCTION
C     NOTE--SUPPORT OPTIONAL FORMULA THAT SEEMS TO BE MORE
C           PREVALENT IN THE LITERATURE:
C
C              CNPK = MIN(A,B)
C              WHERE:
C                  A = (USL-MEDIAN)/((P(.99865)-P(.00135))/2)
C                  B = (MEDIAN-LSL)/((P(.99865)-P(.00135))/2)
C                  P = THE PERCENTILE FUNCTION
C     NOTE--CNPK IS A MEASURE OF PROCESS ACCURACY--
C           COMBINING BOTH PRECISION AND UNBIASEDNESS.
C           IT IS A NON-PARAMETERIC METHOD FOR THE CPK STATISTIC
C           THAT IS RECOMMENDED WHEN THE DATA ARE NOT NORMAL.
C     NOTE--THE CNPK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CNPK
C           WHICH ARE LARGE (ABOVE 2);
C           VALUES OF CNPK FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CNPK    = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CNPK
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CNPK INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--AFP 800-7. "USAF R&M 2000 PROCESS", DEPARTMENT OF
C                 THE AIR FORCE, HQ USAF, WASHINGTON, DC GPO, 1 OCT
C                 1988. (THIS ISN'T THE RIGHT DOCUMENT, NOT SURE
C                 WHICH DOCUMENT ACTUALLY HAS THE TECHNICAL DETAILS).
C               --PEARN, TAI, HSIAO, AND AO (2014), "APPROXIMATELY
C                 UNBIASED ESTIMATOR FOR NON-NORMAL PROCESS
C                 CAPABILITY INDEX", JOURNAL OF TESTING AND
C                 EVALUATION, VOL. 42, NO. 6, PP. 1-10.
C               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99.3
C     ORIGINAL VERSION--MARCH     1999.
C     UPDATED         --APRIL     2015. SUPPORT ALTERNATIVE DEFINITION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CNPK '
      ISUBN2='    '
      IERROR='NO'
C
      DMEAN=0.0D0
      XCNPK=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CNPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
         WRITE(ICOUT,56)I,X(I)
   56    FORMAT('I,X(I) = ',I8,G15.7)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CNPK  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CNPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ****************************************
C               **  STEP 2--                          **
C               **  COMPUTE THE MEDIAN AND PERCENTILE **
C               **  POINTS                            **
C               ****************************************
C
      IWRITE='OFF'
      CALL MEDIAN(X,N,IWRITE,XTEMP,MAXNXT,XMED,IBUGA3,IERROR)
      IF(ICNPKD.EQ.'PEAR')THEN
        P=99.865
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.135
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ELSE
        P=99.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ENDIF
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CNPK RATIO                      **
C               **************************************************
C
      IF(ICNPKD.EQ.'PEAR')THEN
        DENOM=(P995 - P005)/2.0
        UPPER=(ENGUSL-XMED)/DENOM
        ALOWER=(XMED-ENGLSL)/DENOM
        XCNPK=MIN(UPPER,ALOWER)
      ELSE
        UPPER=(ENGUSL-XMED)/(P995-XMED)
        ALOWER=(XMED-ENGLSL)/(XMED-P005)
        XCNPK=MIN(UPPER,ALOWER)
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCNPK
  811   FORMAT('THE CNPK OF THE ',I8,' OBSERVATIONS = ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CNPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XMED,P005,P995
 9014   FORMAT('XMED,P005,P995 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)UPPER,ALOWER,XCNPK
 9016   FORMAT('UPPER,ALOWER ,XCNPK= ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CNPM(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,TARGET,IWRITE,
     1                ICNPKD,XCNPM,
     1                IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPM PROCESS
C              CAPABILITY INDEX OF THE DATA IN THE INPUT VECTOR X.
C              THIS IS A NON-PARAMETRIC VERSION OF THE CPM STATISTIC.
C
C                 CNPM = (USL - LSL)/
C                        [6*SQRT{((P(0.99865)-P(0.00135)/6)**2 +
C                        (MEDIAN - TARGET)**2}]
C
C           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
C           COMPARABLE TO THE NORMAL-BASED CPM STATISTIC (+/- 3*SIGMA).
C           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND USES
C           P(0.995) AND P(0.005).
C     NOTE--CNPM IS A MEASURE OF PROCESS ACCURACY--
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--XCNPM  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CNPM
C                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
C                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPM INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--MEAN AND SD.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NORMA HUBELE, ARIZONA STATE
C               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.04
C     ORIGINAL VERSION--APRIL     2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL NUM
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CNPM'
      ISUBN2='    '
      IERROR='NO'
C
      XCNPM=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CNPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL,TARGET
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL,TARGET = ',A4,2X,I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CNPM **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CNPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ****************************************
C               **  STEP 2--                          **
C               **  COMPUTE THE MEDIAN AND PERCENTILE **
C               **  POINTS                            **
C               ****************************************
C
      IWRITE='OFF'
      CALL MEDIAN(X,N,IWRITE,XTEMP,MAXNXT,XMED,IBUGA3,IERROR)
      IF(ICNPKD.EQ.'PEAR')THEN
        P=99.865
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.135
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ELSE
        P=99.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P995,IBUGA3,IERROR)
        P=0.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP,MAXNXT,P005,IBUGA3,IERROR)
      ENDIF
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CNPM RATIO                      **
C               **************************************************
C
      NUM=ABS(ENGUSL-ENGLSL)
      TERM1=(P995-P005)/6.0
      TERM2=(XMED-TARGET)**2
      DEN=6.0*SQRT(TERM1**2 + TERM2)
      IF(DEN.GT.0.0)XCNPM=NUM/DEN
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XNCPM
  811   FORMAT('THE CNPM OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CNPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XMED,P005,P995
 9014   FORMAT('XMED,P005,P995 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NUM,DEN,XCNPM
 9016   FORMAT('NUM,DEN,XCNPM= ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CNPMK(X,N,XTEMP1,MAXNXT,ENGLSL,ENGUSL,TARGET,IWRITE,
     1                 ICNPKD,XCNPMK,
     1                 IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPMK PROCESS
C              CAPABILITY INDEX OF THE DATA IN THE INPUT VECTOR X.
C              THIS IS A NON-PARAMETRIC VERSION OF THE CPMK STATISTIC.
C
C                CNPMK = min{(USL - MED,MED-LSL)}/
C                        [3*SQRT{((P(0.99865)-P(0.00135)/6)**2 +
C                        (MEDIAN - TARGET)**2}]
C
C           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
C           COMPARABLE TO THE NORMAL-BASED CPM STATISTIC (+/- 3*SIGMA).
C           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND USES
C           P(0.995) AND P(0.005).
C     NOTE--CNPM IS A MEASURE OF PROCESS ACCURACY--
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--XCNPMK = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CNPMK
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPMK INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.04
C     ORIGINAL VERSION--APRIL     2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL NUM
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP1(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CNPM'
      ISUBN2='K   '
      IERROR='NO'
C
      XCNPMK=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CNPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL,TARGET
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL,TARGET = ',A4,2X,I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CNPM **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CNPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ****************************************
C               **  STEP 2--                          **
C               **  COMPUTE THE MEDIAN AND PERCENTILE **
C               **  POINTS                            **
C               ****************************************
C
      IWRITE='OFF'
      CALL MEDIAN(X,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
      IF(ICNPKD.EQ.'PEAR')THEN
        P=99.865
        CALL PERCEN(P,X,N,IWRITE,XTEMP1,MAXNXT,P995,IBUGA3,IERROR)
        P=0.135
        CALL PERCEN(P,X,N,IWRITE,XTEMP1,MAXNXT,P005,IBUGA3,IERROR)
      ELSE
        P=99.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP1,MAXNXT,P995,IBUGA3,IERROR)
        P=0.5
        CALL PERCEN(P,X,N,IWRITE,XTEMP1,MAXNXT,P005,IBUGA3,IERROR)
      ENDIF
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CNPM RATIO                      **
C               **************************************************
C
      UPPER=ENGUSL-XMED
      ALOWER=XMED-ENGLSL
      NUM=MIN(UPPER,ALOWER)
      TERM1=(P995-P005)/6.0
      TERM2=(XMED-TARGET)**2
      DEN=3.0*SQRT(TERM1**2 + TERM2)
      IF(DEN.GT.0.0)XCNPMK=NUM/DEN
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XNCPMK
  811   FORMAT('THE CNPMK OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CNPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XMED,P005,P995
 9014   FORMAT('XMED,P005,P995 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NUM,DEN,XCNPMK
 9016   FORMAT('NUM,DEN,XCNPMK= ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE COCODE(X,N,XREF,NREF,XPRIME,IBUGA3)
C
C     PURPOSE--THIS SUBROUTINE CO-CODES
C              THE N ELEMENTS OF THE SINGLE PRECISION VECTOR X,
C              AS DICTATED BY HOW X MATCHES XREF.
C     IN PARTICULAR, ALL ELEMENTS IN X THAT MATCH XREF(1)
C                    WILL GET CODED WITH 1.
C                    ALL ELEMENTS IN X THAT MATCH XREF(2)
C                    WILL GET CODED WITH 2.
C                    ETC.
C              THE OUTPUT IS, IN FACT, PLACED IN XPRIME.
C              (X AND XREF REMAIN UNCHANGED)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS TO BE CO-CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X AND XPRIME.
C                     --XREF   = THE SINGLE PRECISION VECTOR OF
C                                REFERENCE OBSERVATIONS.
C                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR XREF.
C     OUTPUT ARGUMENTS--XPRIME = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE RECODED DATA VALUES
C                                WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR XPRIME
C             CONTAINING THE RECODED VALUES.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     ORIGINAL VERSION--JULY      1991.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*),XREF(*),XPRIME(*)
      CHARACTER*4 IBUGA3
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,15)
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'SORTC  SUBROUTINE IS NON-POSITIVE *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
C
      IF(IBUGA3.NE.'ON')GOTO190
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,110)
  110 FORMAT('***** AT THE BEGINNING OF COCODE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)N,NREF
  111 FORMAT('N,NREF = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO112I=1,N
      WRITE(ICOUT,113)I,X(I),XREF(I)
  113 FORMAT('I,X(I),XREF(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  190 CONTINUE
C
      DO1100I=1,N
      XPRIME(I)=-999
 1100 CONTINUE
C
      DO1200I=1,NREF
      XREFI=XREF(I)
      DO1300J=1,N
      IF(X(J).EQ.XREFI)XPRIME(J)=I
 1300 CONTINUE
 1200 CONTINUE
C
 9000 CONTINUE
      IF(IBUGA3.NE.'ON')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COCODE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)N,NREF
 9012 FORMAT('N,NREF = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I),XREF(I)
 9016 FORMAT('I,X(I),XREF(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      DO9020I=1,N
      WRITE(ICOUT,9021)I,XPRIME(I)
 9021 FORMAT('I,XPRIME(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COCOPY(YREF,NREF,X,NX,XREF,Y,NY,IBUGA3)
C
C     PURPOSE--THIS SUBROUTINE CO-COPIES
C              THE NREF ELEMENTS OF THE SINGLE PRECISION
C              VECTOR YREF INTO THE (TYPICALLY) LONGER VECTOR Y.
C              AS DICTATED BY HOW X MATCHES XREF.
C     IN PARTICULAR, FOR ALL ELEMENTS IN X THAT MATCH XREF(1),
C                    Y WILL BECOME YREF(1).
C                    FOR ALL ELEMENTS IN X THAT MATCH XREF(2),
C                    Y WILL BECOME YREF(2).
C                    ETC.
C              THE OUTPUT IS, IN FACT, PLACED IN Y.
C              (X, XREF, AND YREF REMAIN UNCHANGED).
C     INPUT  ARGUMENTS--YREF   = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS TO BE CO-COPIED.
C                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR YREF (AND XREF).
C                     --X      = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS USED FOR MATCHING .
C                     --NX     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X (AND Y).
C                     --XREF   = THE SINGLE PRECISION VECTOR OF
C                                REFERENCE OBSERVATIONS.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE VARIOUS YREF VALUES
C                                WILL BE COPIED.
C                       NY     = THE INTEGER NUMBER OF ELEMENTS
C                                IN Y (= NX)
C     OUTPUT--THE SINGLE PRECISION VECTOR Y
C             CONTAINING THE COPIED VALUES.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     ORIGINAL VERSION--JULY      1991.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION YREF(*),X(*),XREF(*),Y(*)
      CHARACTER*4 IBUGA3
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(NX.LT.1)GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,15)
   15 FORMAT('***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE ',
     1'SORTC  SUBROUTINE IS NON-POSITIVE *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)NX
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      NY=NX
      RETURN
   90 CONTINUE
C
      IF(IBUGA3.NE.'ON')GOTO190
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,110)
  110 FORMAT('***** AT THE BEGINNING OF COCOPY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)NREF,NX
  111 FORMAT('NREF,NX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO112I=1,NX
      WRITE(ICOUT,113)I,X(I),XREF(I),YREF(I)
  113 FORMAT('I,X(I),XREF(I),YREF(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  190 CONTINUE
C
      DO1100I=1,NX
      Y(I)=-999
 1100 CONTINUE
C
      DO1200I=1,NREF
      XREFI=XREF(I)
      DO1300J=1,NX
      IF(X(J).EQ.XREFI)Y(J)=YREF(I)
 1300 CONTINUE
 1200 CONTINUE
      NY=NX
C
 9000 CONTINUE
      IF(IBUGA3.NE.'ON')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COCOPY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NREF,NX,NY
 9012 FORMAT('NREF,NX,NY = ',I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NX
      WRITE(ICOUT,9016)I,X(I),XREF(I),YREF(I)
 9016 FORMAT('I,X(I),XREF(I),YREF(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      DO9020I=1,NY
      WRITE(ICOUT,9021)I,Y(I)
 9021 FORMAT('I,Y(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CODCT2(X1,X2,N,ICCTOF,ICCTG1,IWRITE,
     1                  Y,XIDTEM,XIDTE2,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
C              CROSS TABULATION OF TWO GROUP-ID VARIABLES.  THIS
C              CAN BE USEFUL FOR COMMANDS OF THE FORM
C
C                  <COMMAND>  Y  X
C
C              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
C              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
C              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
C              A BOX PLOT OVER SEVERAL GROUPS.
C
C              THE CODING IS BASED ON THE FOLLOWING FORMULA:
C
C                  ICODE = OFFSET + (ISET1-1)*NGROUP2 + ISET2
C
C              WHERE
C
C                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
C                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
C                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
C                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
C
C              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
C              THE ICCTG1 PARAMETER CAN BE USED TO CONTROL THIS
C              (I.E., WE USE THE MAXIMUM OF NGROUP2 AND ICCTG1).
C     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIRST GROUP VARIABLE
C                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SECOND GROUP VARIABLE
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X1 AND X2.
C                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE OFFSET.
C                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING BETWEEN GROUPS.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
C             X1 AND X2.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/6
C     ORIGINAL VERSION--JUNE      2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODC'
      ISUBN2='T2  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODCT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,ICCTOF,ICCTG1
   52   FORMAT('IBUGA3,ISUBRO,N,ICCTOF,ICCTG1 = ',A4,2X,A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I)
   56     FORMAT('I,X1(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  PERFORM THE CODING--                                 **
C               ***********************************************************
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
C
      IFACT1=MAX(NGRP2,ICCTG1)
C
      DO100I=1,N
C
        DO200J=1,NGRP1
          DO300K=1,NGRP2
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT2')THEN
              WRITE(ICOUT,301)I,J,K
  301         FORMAT('I,J,K = ',3I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),XIDTEM(J),XIDTE2(K)
  302         FORMAT('X1(I),X2(I),XIDTEM(J),XIDTE2(K)=',4G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K))THEN
              IINDX=ICCTOF + (J-1)*IFACT1 + K
              Y(I)=REAL(IINDX)
              GOTO100
            ENDIF
  300     CONTINUE
  200   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('***** INTERNAL ERROR IN CODCT2 SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,310)I
  310   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)X1(I)
  312   FORMAT('      GROUP-ID VARIABLE 1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)X2(I)
  313   FORMAT('      GROUP-ID VARIABLE 2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  100 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2
 9013   FORMAT('NGRP1,NGRP2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),Y(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODCT3(X1,X2,X3,N,ICCTOF,ICCTG1,ICCTG2,IWRITE,
     1                  Y,XIDTEM,XIDTE2,XIDTE3,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
C              CROSS TABULATION OF THREE GROUP-ID VARIABLES.  THIS
C              CAN BE USEFUL FOR COMMANDS OF THE FORM
C
C                  <COMMAND>  Y  X
C
C              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
C              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
C              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
C              A BOX PLOT OVER SEVERAL GROUPS.
C
C              THE CODING IS BASED ON THE FOLLOWING FORMULA:
C
C                  ICODE = OFFSET + (ISET1-1)*NGROUP2*NGROUP3 +
C                                   (ISET2-1)*NGROUP3 + ISET3
C
C              WHERE
C
C                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
C                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
C                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
C                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
C                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
C                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
C
C              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
C              THE ICCTG1 AND ICCTG2 PARAMETERS CAN BE USED TO CONTROL
C              THIS (I.E., WE USE:
C
C                   THE MAXIMUM OF NGROUP2 AND ICCTG1
C                   THE MAXIMUM OF NGROUP3 AND ICCTG2
C
C     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIRST GROUP VARIABLE
C                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SECOND GROUP VARIABLE
C                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE THIRD GROUP VARIABLE
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X1, X2 AND X3.
C                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE OFFSET.
C                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 2.
C                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 3.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
C             X1, X2 AND X3.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/6
C     ORIGINAL VERSION--JUNE      2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODC'
      ISUBN2='T3  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODCT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I)
   56     FORMAT('I,X1(I),X2(I),X3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  PERFORM THE CODING--                                 **
C               ***********************************************************
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
C
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
C
      DO100I=1,N
C
        DO200J=1,NGRP1
          DO300K=1,NGRP2
            DO400L=1,NGRP3
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT3')THEN
              WRITE(ICOUT,301)I,J,K,L
  301         FORMAT('I,J,K,L = ',4I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I)
  302         FORMAT('X1(I),X2(I),X3(I)=',3G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L)=',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.
     1         X3(I).EQ.XIDTE3(L))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2 + (K-1)*IFACT2 + L
              Y(I)=REAL(IINDX)
              GOTO100
            ENDIF
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('***** INTERNAL ERROR IN CODCT3 SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,310)I
  310   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)X1(I)
  312   FORMAT('      GROUP-ID VARIABLE 1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)X2(I)
  313   FORMAT('      GROUP-ID VARIABLE 2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)X3(I)
  314   FORMAT('      GROUP-ID VARIABLE 3 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  100 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3
 9013   FORMAT('NGRP1,NGRP2,NGRP3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),Y(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODCT4(X1,X2,X3,X4,N,
     1                  ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,
     1                  Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
C              CROSS TABULATION OF FOUR GROUP-ID VARIABLES.  THIS
C              CAN BE USEFUL FOR COMMANDS OF THE FORM
C
C                  <COMMAND>  Y  X
C
C              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
C              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
C              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
C              A BOX PLOT OVER SEVERAL GROUPS.
C
C              THE CODING IS BASED ON THE FOLLOWING FORMULA:
C
C                  ICODE = OFFSET + (ISET1-1)*NGROUP2*NGROUP3*NGROUP4 +
C                                   (ISET2-1)*NGROUP3*NGROUP4 +
C                                   (ISET3-1)*NGROUP4 + ISET4
C
C              WHERE
C
C                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
C                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
C                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
C                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
C                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
C                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
C                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
C                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
C
C              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
C              THE ICCTG1, ICCTG2, AND ICCTG3 PARAMETERS CAN BE USED
C              TO CONTROL THIS (I.E., WE USE:
C
C                   THE MAXIMUM OF NGROUP2 AND ICCTG1
C                   THE MAXIMUM OF NGROUP3 AND ICCTG2
C                   THE MAXIMUM OF NGROUP4 AND ICCTG3
C
C     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIRST GROUP VARIABLE
C                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SECOND GROUP VARIABLE
C                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE THIRD GROUP VARIABLE
C                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FOURTH GROUP VARIABLE
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X1, X2, X3 AND X4.
C                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE OFFSET.
C                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 2.
C                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 3.
C                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 4.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
C             X1, X2, X3 AND X4.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/6
C     ORIGINAL VERSION--JUNE      2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODC'
      ISUBN2='T4  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODCT4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  PERFORM THE CODING--                                 **
C               ***********************************************************
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
      CALL DISTIN(X4,N,IWRITE,XIDTE4,NGRP4,IBUGA3,IERROR)
      CALL SORT(XIDTE4,NGRP4,XIDTE4)
C
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
C
      DO100I=1,N
C
        DO200J=1,NGRP1
          DO300K=1,NGRP2
            DO400L=1,NGRP3
            DO500M=1,NGRP4
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT4')THEN
              WRITE(ICOUT,301)I,J,K,L,M
  301         FORMAT('I,J,K,L,M = ',5I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I),X4(I)
  302         FORMAT('X1(I),X2(I),X3(I),X4(I)=',4G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M)=',4G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.
     1         X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3 + 
     1        (K-1)*IFACT2*IFACT3 +
     1        (L-1)*IFACT3 + M
              Y(I)=REAL(IINDX)
              GOTO100
            ENDIF
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('***** INTERNAL ERROR IN CODCT4 SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,310)I
  310   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)X1(I)
  312   FORMAT('      GROUP-ID VARIABLE 1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)X2(I)
  313   FORMAT('      GROUP-ID VARIABLE 2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)X3(I)
  314   FORMAT('      GROUP-ID VARIABLE 3 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,315)X4(I)
  315   FORMAT('      GROUP-ID VARIABLE 4 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  100 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),Y(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODCT5(X1,X2,X3,X4,X5,N,
     1                  ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICTG4,IWRITE,
     1                  Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
C              CROSS TABULATION OF FIVE GROUP-ID VARIABLES.  THIS
C              CAN BE USEFUL FOR COMMANDS OF THE FORM
C
C                  <COMMAND>  Y  X
C
C              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
C              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
C              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
C              A BOX PLOT OVER SEVERAL GROUPS.
C
C              THE CODING IS BASED ON THE FOLLOWING FORMULA:
C
C                  ICODE = OFFSET +
C                          (ISET1-1)*NGROUP2*NGROUP3*NGROUP4*NGROUP5 +
C                          (ISET2-1)*NGROUP3*NGROUP4*NGROUP5 +
C                          (ISET3-1)*NGROUP4*NGROUP5
C                          (ISET4-1)*NGROUP5 + ISET5
C
C              WHERE
C
C                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
C                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
C                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
C                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
C                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
C                  ISET5     = I-TH DISTINCT VALUE OF GROUP 5
C                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
C                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
C                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
C                  NGROUP5   = NUMBER OF DISTINCT VALUES FOR GROUP 5
C
C              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
C              THE ICCTG1, ICCTG2, ICCTG3,AND ICCTG4 PARAMETERS CAN BE
C              USED TO CONTROL THIS (I.E., WE USE:
C
C                   THE MAXIMUM OF NGROUP2 AND ICCTG1
C                   THE MAXIMUM OF NGROUP3 AND ICCTG2
C                   THE MAXIMUM OF NGROUP4 AND ICCTG3
C                   THE MAXIMUM OF NGROUP5 AND ICCTG4
C
C     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIRST GROUP VARIABLE
C                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SECOND GROUP VARIABLE
C                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE THIRD GROUP VARIABLE
C                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FOURTH GROUP VARIABLE
C                     --X5     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIFTH GROUP VARIABLE
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X1, X2, X3, X4 AND X5.
C                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE OFFSET.
C                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 2.
C                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 3.
C                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 4.
C                     --ICCTG4 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 5.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
C             X1, X2, X3, X4 AND X5.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/6
C     ORIGINAL VERSION--JUNE      2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION X5(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION XIDTE5(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODC'
      ISUBN2='T4  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT5')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODCT5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I),X5(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  PERFORM THE CODING--                                 **
C               ***********************************************************
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
      CALL DISTIN(X4,N,IWRITE,XIDTE4,NGRP4,IBUGA3,IERROR)
      CALL SORT(XIDTE4,NGRP4,XIDTE4)
      CALL DISTIN(X5,N,IWRITE,XIDTE5,NGRP5,IBUGA3,IERROR)
      CALL SORT(XIDTE5,NGRP5,XIDTE5)
C
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
      IFACT4=MAX(NGRP5,ICCTG4)
C
      DO100I=1,N
C
        DO200J=1,NGRP1
          DO300K=1,NGRP2
            DO400L=1,NGRP3
            DO500M=1,NGRP4
            DO600JJ=1,NGRP5
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT5')THEN
              WRITE(ICOUT,301)I,J,K,L,M
  301         FORMAT('I,J,K,L,M = ',5I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I),X4(I),X5(I)
  302         FORMAT('X1(I),X2(I),X3(I),X4(I),X5(I)=',5G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L),
     1                        XIDTE4(M),XIDTE5(JJ)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M),',
     1               'XIDTE5(JJ)=',5G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.
     1         X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M) .AND.
     1         X5(I).EQ.XIDTE5(JJ))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3*IFACT4 + 
     1        (K-1)*IFACT2*IFACT3*IFACT4 +
     1        (L-1)*IFACT3*IFACT4 +
     1        (M-1)*IFACT4 + JJ
              Y(I)=REAL(IINDX)
              GOTO100
            ENDIF
  600     CONTINUE
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('***** INTERNAL ERROR IN CODCT5 SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,310)I
  310   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)X1(I)
  312   FORMAT('      GROUP-ID VARIABLE 1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)X2(I)
  313   FORMAT('      GROUP-ID VARIABLE 2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)X3(I)
  314   FORMAT('      GROUP-ID VARIABLE 3 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,315)X4(I)
  315   FORMAT('      GROUP-ID VARIABLE 4 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,316)X5(I)
  316   FORMAT('      GROUP-ID VARIABLE 5 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  100 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4*NGRP5
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4,NGRP5
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4,NGRP5 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),X5(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),Y(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODCT6(X1,X2,X3,X4,X5,X6,N,
     1                  ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICTG4,ICTG5,IWRITE,
     1                  Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
C              CROSS TABULATION OF SIX GROUP-ID VARIABLES.  THIS
C              CAN BE USEFUL FOR COMMANDS OF THE FORM
C
C                  <COMMAND>  Y  X
C
C              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
C              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
C              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
C              A BOX PLOT OVER SEVERAL GROUPS.
C
C              THE CODING IS BASED ON THE FOLLOWING FORMULA:
C
C                  ICODE = OFFSET +
C                          (ISET1-1)*NGROUP2*NGROUP3*NGROUP4*NGROUP5*NGROUP6 +
C                          (ISET2-1)*NGROUP3*NGROUP4*NGROUP5*NGROUP6 +
C                          (ISET3-1)*NGROUP4*NGROUP5*NGROUP6 +
C                          (ISET4-1)*NGROUP5*NGROUP6 +
C                          (ISET5-1)*NGROUP6 + ISET6
C
C              WHERE
C
C                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
C                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
C                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
C                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
C                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
C                  ISET5     = I-TH DISTINCT VALUE OF GROUP 5
C                  ISET6     = I-TH DISTINCT VALUE OF GROUP 6
C                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
C                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
C                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
C                  NGROUP5   = NUMBER OF DISTINCT VALUES FOR GROUP 5
C                  NGROUP6   = NUMBER OF DISTINCT VALUES FOR GROUP 6
C
C              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
C              THE ICCTG1, ICCTG2, ICCTG3, ICCTG4, AND ICCTG5 PARAMETERS
C              CAN BE USED TO CONTROL THIS (I.E., WE USE:
C
C                   THE MAXIMUM OF NGROUP2 AND ICCTG1
C                   THE MAXIMUM OF NGROUP3 AND ICCTG2
C                   THE MAXIMUM OF NGROUP4 AND ICCTG3
C                   THE MAXIMUM OF NGROUP5 AND ICCTG4
C                   THE MAXIMUM OF NGROUP6 AND ICCTG5
C
C     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIRST GROUP VARIABLE
C                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SECOND GROUP VARIABLE
C                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE THIRD GROUP VARIABLE
C                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FOURTH GROUP VARIABLE
C                     --X5     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE FIFTH GROUP VARIABLE
C                     --X6     = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE VALUES OF THE SIXTH GROUP VARIABLE
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X1, X2, X3, X4, X5 AND
C                                X6.
C                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE OFFSET.
C                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 2.
C                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 3.
C                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 4.
C                     --ICCTG4 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 5.
C                     --ICCTG5 = THE INTEGER PARAMETER THAT SPECIFIES
C                                THE SPACING FOR GROUP 6.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
C             X1, X2, X3, X4, X5 AND X6.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/6
C     ORIGINAL VERSION--JUNE      2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION X5(*)
      DIMENSION X6(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION XIDTE5(*)
      DIMENSION XIDTE6(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODC'
      ISUBN2='T6  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT6')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODCT6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I) = ',I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  PERFORM THE CODING--                                 **
C               ***********************************************************
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
      CALL DISTIN(X4,N,IWRITE,XIDTE4,NGRP4,IBUGA3,IERROR)
      CALL SORT(XIDTE4,NGRP4,XIDTE4)
      CALL DISTIN(X5,N,IWRITE,XIDTE5,NGRP5,IBUGA3,IERROR)
      CALL SORT(XIDTE5,NGRP5,XIDTE5)
      CALL DISTIN(X6,N,IWRITE,XIDTE6,NGRP6,IBUGA3,IERROR)
      CALL SORT(XIDTE6,NGRP6,XIDTE6)
C
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
      IFACT4=MAX(NGRP5,ICCTG4)
      IFACT5=MAX(NGRP6,ICCTG5)
C
      DO100I=1,N
C
        DO200J=1,NGRP1
          DO300K=1,NGRP2
            DO400L=1,NGRP3
            DO500M=1,NGRP4
            DO600JJ=1,NGRP5
            DO700KK=1,NGRP6
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT6')THEN
              WRITE(ICOUT,301)I,J,K,L,M
  301         FORMAT('I,J,K,L,M = ',5I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)
  302         FORMAT('X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)=',6G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L),
     1                        XIDTE4(M),XIDTE5(JJ),XIDTE6(KK)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M),',
     1               'XIDTE5(JJ),XIDTE6(KK)=',6G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.
     1         X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M) .AND.
     1         X5(I).EQ.XIDTE5(JJ) .AND. X6(I).EQ.XIDTE6(KK))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3*IFACT4*IFACT5 + 
     1        (K-1)*IFACT2*IFACT3*IFACT4*IFACT5 +
     1        (L-1)*IFACT3*IFACT4*IFACT5 +
     1        (M-1)*IFACT4*IFACT5 +
     1        (JJ-1)*IFACT5 + KK
              Y(I)=REAL(IINDX)
              GOTO100
            ENDIF
  700     CONTINUE
  600     CONTINUE
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('***** INTERNAL ERROR IN CODCT6 SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,310)I
  310   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)X1(I)
  312   FORMAT('      GROUP-ID VARIABLE 1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)X2(I)
  313   FORMAT('      GROUP-ID VARIABLE 2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)X3(I)
  314   FORMAT('      GROUP-ID VARIABLE 3 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,315)X4(I)
  315   FORMAT('      GROUP-ID VARIABLE 4 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,316)X5(I)
  316   FORMAT('      GROUP-ID VARIABLE 5 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,317)X6(I)
  317   FORMAT('      GROUP-ID VARIABLE 6 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  100 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4*NGRP5*NGRP6
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4,NGRP5,NGRP6
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4,NGRP5,NGRP6 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I),Y(I) = ',
     1           I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODE(X,N,IWRITE,Y,DIST,MAXOBV,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
C              OF THE INPUT VECTOR X
C              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
C              THE CODING IS AS FOLLOWS--
C              THE MINIMUM IS CODED AS 1.0.
C              THE NEXT LARGER VALUE AS 2.0,
C              THE NEXT LARGER VALUE AS 3.0,
C              ETC.
C     NOTE--THIS ROUTINE IN JJF8 HAS BEEN MODIFIED FOR DATAPLOT
C           FROM THE SAME-NAME SUBROUTINE IN JJF6 IN 4 IMPORTANT WAYS--
C           1)  THE UPPER LIMIT (IUPPER) HAS BEEN
C               REDUCED FROM 7500 TO 1000
C           2)  THE VECTOR DIST HAS HAD ITS DIMENSION
C               CHANGED FROM 7500 TO 1000.
C           3)  THE VECTOR DIST HAS BEEN TAKEN OUT OF COMMON.
C           4)  THE VECTOR WS HAS BEEN DELETED.
C           5)  THE OUTPUT WRITING HAS BEEN SUPPRESSED.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y
C             WHICH WILL CONTAIN THE CODED VALUES
C             CORRESPONDING TO THE OBSERVATIONS IN
C             THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--ALL OCCURRANCES OF THE MINIMUM ARE CODED AS 1.0;
C              ALL OCCURANCES OF THE NEXT LARGER VALUE
C              ARE CODED AS 2.0;
C              ALL OCCURANCES OF THE NEXT LARGER VALUE
C              ARE CODED AS 3.0, ETC.
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --JUNE      1977.
C     UPDATED         --JULY      1977.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION DIST(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='    '
      IERROR='NO'
      IUPPER=MAXOBV
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,IUPPER
   52   FORMAT('IBUGA3,N,IUPPER = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      IS LESS THAN 1 OR GREATER THAN ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      DO137I=1,N
        Y(I)=1.0
  137 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  PERFORM THE CODING--                                   **
C               **  PULL OUT THE DISTINCT VALUES,                          **
C               **  THEN SORT (AND ESSENTIALLY RANK) THE DISTINCT VALUES,  **
C               **  THEN APPLY THE RANKS TO ALL THE VALUES.                **
C               *************************************************************
C
      NUMDIS=1
      DIST(NUMDIS)=X(1)
      DO200I=2,N
        DO300J=1,NUMDIS
          IF(X(I).EQ.DIST(J))GOTO200
  300   CONTINUE
        NUMDIS=NUMDIS+1
        DIST(NUMDIS)=X(I)
  200 CONTINUE
C
      CALL SORT(DIST,NUMDIS,DIST)
C
      DO600I=1,N
        DO700J=1,NUMDIS
          IF(X(I).EQ.DIST(J))THEN
            Y(I)=J
            GOTO600
          ENDIF
  700   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,705)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,710)I,X(I)
  705   FORMAT('***** INTERNAL ERROR IN CODE SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
  710   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8,' = ',
     1         G15.7)
        GOTO9000
  600 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)NUMDIS
  811   FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        AI=1
        WRITE(ICOUT,812)DIST(1),AI
  812   FORMAT('THE MINIMUM (= ',G15.7,' ) HAS CODE VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        AI=NUMDIS
        WRITE(ICOUT,813)DIST(NUMDIS),AI
  813   FORMAT('THE MAXIMUM (= ',G15.7,' ) HAS CODE VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,NUMDIS
 9013   FORMAT('N,NUMDIS = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I),DIST(I)
 9016     FORMAT('I,X(I),Y(I),DIST(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODECH(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,YTEMP,
     1                  IWRITE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
C              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
C              A NUMERIC VARIABLE.  THAT IS, EACH DISTINCT
C              CHARACTER VARIABLE WILL BE ASSIGNED AN INTEGER
C              CODE (DETERMINED BY ORDER THAT THE FIRST OCCURENCE
C              IS FOUND).
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE CHARACTER VARIABLE.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y
C             WHICH WILL CONTAIN THE CODED VALUES
C             CORRESPONDING TO THE OBSERVATIONS IN
C             THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/1
C     ORIGINAL VERSION--JANUARY   2004.
C     UPDATED         --FEBRUARY  2006. FIX BUG WHERE IT WAS ONLY
C                                       WORKING IF THERE WAS ONE
C                                       CHARACTER VARIABLE IN THE
C                                       DPZCHF.DAT.
C     UPDATED         --APRIL     2017. MODIFY THE FEEDBACK TO SHOW
C                                       THE ACTUAL MAPPING
C     UPDATED         --JUNE      2019. DIMENSION SCRATCH REAL ARRAYS IN
C                                       CALLING ROUTINE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION YTEMP(*)
      DIMENSION PARAM(*)
      DIMENSION W2HOLD(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*4 ITYPEH(*)
      CHARACTER*4 IW21HO(*)
      CHARACTER*4 IW22HO(*)
      CHARACTER*4 IA(*)
      CHARACTER*4 IPARN(*)
      CHARACTER*4 IPARN2(*)
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='CH  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODECH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                     *
C               **  EXAMINE THE LEFT-HAND SIDE--                 *
C               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
C               **  ALREADY IN THE NAME LIST?                    *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
C               **  TABLE OF THE NAME ON THE LEFT.               *
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO2000I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GOTO2100
        ENDIF
 2000 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN CODECH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2203)MAXNAM
 2203   FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      ENTER      STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2205)
 2205   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2206)
 2206   FORMAT('      THEN DELETE SOME OF THE ALREADY-USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2100 CONTINUE
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 2--                              **
C               **  OPEN THE DPZCHF.DAT FILE.             **
C               ********************************************
C
      IHRIGH=IHARG(5)
      IHRIG2=IHARG2(5)
C
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
C
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,
     1            ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODECH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      UNABLE TO OPEN THE FILE CHARACTER DATA FILE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)IFILE
  119   FORMAT('      ',A80)
        CALL DPWRST('XXX','BUG ')
        GOTO8000
      ENDIF
C
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
C
CCCCC FEBRUARY 2006:  BUG FIX FOR THE FOLLOWING LOOP.
C
      IVAR=-1
      DO130I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
CCCCC     GOTO199
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GOTO199
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',
     1       'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  199 CONTINUE
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  PERFORM THE CODING--                       **
C               **  STORE UNIQUE VALUES IN IXTEMP, COMPARE     **
C               **  TO LIST IN IXTEMP.                         **
C               *************************************************
C
      IATEMP=' '
      IFRMT='(A   )'
      WRITE(IFRMT(3:5),'(I3)')25*IVAR
      N=1
      IROW=1
      READ(IOUNIT,IFRMT,END=491,ERR=491)IATEMP
      YTEMP(1)=REAL(N)
      IFRST=(IVAR-1)*25 + 1
      ILAST=IVAR*25 - 1
      IXTEMP(1)=' '
      IXTEMP(1)=IATEMP(IFRST:ILAST)
C
      DO210I=2,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
        DO220J=1,N
          IF(IATEMP(IFRST:ILAST).EQ.IXTEMP(J)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GOTO210
          ENDIF
  220   CONTINUE
        N=N+1
        IXTEMP(N)=' '
        IXTEMP(N)=IATEMP(IFRST:ILAST)
        YTEMP(IROW)=REAL(N)
  210 CONTINUE
      GOTO499
C
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',
     1       'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
  499 CONTINUE
C
C     2017/04: MODIFY THE FEEDBACK TO SHOW THE FULL MAPPING
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)IHRIGH,IHRIG2,N
  811   FORMAT('NUMBER OF DISTINCT CHARACTER VALUES FOR ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(N.LE.9)THEN
          IFRMT="(A  ,' => ',I1)"
        ELSEIF(N.LE.99)THEN
          IFRMT="(A  ,' => ',I2)"
        ELSE
          IFRMT="(A  ,' => ',I3)"
        ENDIF
C
        MAXCHR=1
        DO810I=1,MIN(N,100)
          DO813J=24,1,-1
            IF(IXTEMP(I)(J:J).NE.' ')THEN
              IF(J.GT.MAXCHR)MAXCHR=J
              GOTO815
            ENDIF
  813     CONTINUE
  815     CONTINUE
  810   CONTINUE
        WRITE(IFRMT(3:4),'(I2)')MAXCHR
C
        DO820I=1,N
          WRITE(ICOUT,IFRMT)IXTEMP(I),I
          CALL DPWRST('XXX','BUG ')
  820   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 5--                                       **
C               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
C               **  HOUSEKEEPING ARRAY                             **
C               *****************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 88--                        **
C               **  CLOSE THE DPZCHF.DAT FILE.       **
C               ***************************************
C
 8000 CONTINUE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODECH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,N,IROW
 9012   FORMAT('IBUGA3,IERROR,N,IROW = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO9035I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEC2(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,YTEMP,YTEMP2,IPERM,
     1                  IWRITE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
C              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
C              A NUMERIC VARIABLE.  THAT IS, EACH DISTINCT
C              CHARACTER VARIABLE WILL BE ASSIGNED AN INTEGER
C              CODE.  THIS ROUTINE IS SIMILAR TO CODECH.  THE
C              DISTINCTION IS THAT CODECH CODES BY THE ORDER THE
C              VALUES ARE ENCOUNTERED IN THE FILE WHILE THIS
C              ROUTINE CODES BY (LEXICAL) ALPHABETIC ORDER.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--HPSORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/1
C     ORIGINAL VERSION--JANUARY   2004.
C     UPDATED         --DECEMBER  2009. DON'T USE IXSAVE SO COMMENT
C                                       OUT DECLARATION
C     UPDATED         --DECEMBER  2009. MODIFY DECLARATION OF IXWORK
C                                       FOR INTEL COMPILER
C     UPDATED         --APRIL     2017. MODIFY THE FEEDBACK TO SHOW
C                                       THE ACTUAL MAPPING
C     UPDATED         --JUNE      2019. DIMENSION REAL SCRATCH ARRAYS
C                                       IN CALLING ROUTINE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION W2HOLD(*)
      DIMENSION PARAM(*)
      DIMENSION YTEMP(*)
      DIMENSION YTEMP2(*)
      DIMENSION IPERM(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*4 ITYPEH(*)
      CHARACTER*4 IW21HO(*)
      CHARACTER*4 IW22HO(*)
      CHARACTER*4 IA(*)
      CHARACTER*4 IPARN(*)
      CHARACTER*4 IPARN2(*)
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV/2)
      CHARACTER*24 IXWORK(MAXOBV/2)
CCCCC CHARACTER*24 IXSAVE(MAXOBV/2)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
      EQUIVALENCE (CGARBG(MAXOBV/2 + 1),IXWORK(1))
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='C2  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                     *
C               **  EXAMINE THE LEFT-HAND SIDE--                 *
C               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
C               **  ALREADY IN THE NAME LIST?                    *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
C               **  TABLE OF THE NAME ON THE LEFT.               *
C               **************************************************
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO2000I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GOTO2100
        ENDIF
 2000 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN CODEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2203)MAXNAM
 2203   FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      ENTER      STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2205)
 2205   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2206)
 2206   FORMAT('      THEN DELETE SOME OF THE ALREADY-USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2100 CONTINUE
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 2--                              **
C               **  OPEN THE DPZCHF.DAT FILE.             **
C               ********************************************
C
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
C
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
C
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,
     1            ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      UNABLE TO OPEN THE FILE CHARACTER DATA FILE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)IFILE
  119   FORMAT('      ',A80)
        CALL DPWRST('XXX','BUG ')
        GOTO8000
      ENDIF
C
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
C
      IVAR=-1
      DO130I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
CCCCC     GOTO199
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GOTO199
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',
     1       'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  199 CONTINUE
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  PERFORM THE CODING--                       **
C               **  1) STORE UNIQUE VALUES IN IXTEMP           **
C               **  2) SORT VALUES IN IXTEMP                   **
C               **  3) CODE BASED ON SORTED IXTEMP VALUES      **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IATEMP=' '
      IFRMT=' '
      IFRMT='(A   )'
      WRITE(IFRMT(3:5),'(I3)')25*IVAR
      N=1
      IROW=1
      READ(IOUNIT,IFRMT,END=491,ERR=491)IATEMP
      YTEMP(1)=REAL(N)
      IFRST=(IVAR-1)*25 + 1
      ILAST=IVAR*25 - 1
      IXTEMP(1)=' '
      IXTEMP(1)=IATEMP(IFRST:ILAST)
C
      DO210I=2,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=IROW+1
        DO220J=1,N
          IF(IATEMP(IFRST:ILAST).EQ.IXTEMP(J)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GOTO210
          ENDIF
  220   CONTINUE
        N=N+1
        IF(N.GT.MAXOBV/2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,221)
  221     FORMAT('      NUMBER OF UNIQUE CHARACTER VALUE EXCEEDS ',
     1           I8,' .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,223)
  223     FORMAT('      CODING NOT PERFORMED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IXTEMP(N)=' '
        IXTEMP(N)=IATEMP(IFRST:ILAST)
        YTEMP(IROW)=REAL(N)
  210 CONTINUE
C
  499 CONTINUE
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IBEG=1
      IEND=24
      KFLAG=2
      IER=0
      CALL HPSORT(IXTEMP,N,IBEG,IEND,IPERM,KFLAG,IXWORK(1),IER)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')THEN
        WRITE(ICOUT,292)N,IROW,IER
  292   FORMAT('N,IROW,IER = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO290I=1,N
            WRITE(ICOUT,293)I,IXTEMP(I),IPERM(I)
  293       FORMAT('I,IXTEMP(I),IPERM(I) = ',I8,1X,A24,1X,I8)
            CALL DPWRST('XXX','BUG ')
  290     CONTINUE
        ENDIF
      ENDIF
      IF(IER.GT.0)GOTO9000
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO310I=1,IROW
        ITEMP=INT(YTEMP(I) + 0.5)
        DO320K=1,N
          IF(ITEMP.EQ.IPERM(K))THEN
            INDX=K
            GOTO329
          ENDIF
  320   CONTINUE
  329   CONTINUE
        YTEMP2(I)=REAL(INDX)
  310 CONTINUE
      DO330I=1,IROW
        YTEMP(I)=YTEMP2(I)
  330 CONTINUE
C
      GOTO599
C
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',
     1       'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
  599 CONTINUE
C
C     2017/04: MODIFY THE FEEDBACK TO SHOW THE FULL MAPPING
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)IHRIGH,IHRIG2,N
  811   FORMAT('NUMBER OF DISTINCT CHARACTER VALUES FOR ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(N.LE.9)THEN
          IFRMT="(A  ,' => ',I1)"
        ELSEIF(N.LE.99)THEN
          IFRMT="(A  ,' => ',I2)"
        ELSE
          IFRMT="(A  ,' => ',I3)"
        ENDIF
C
        MAXCHR=1
        DO810I=1,MIN(N,100)
          DO813J=24,1,-1
            IF(IXTEMP(I)(J:J).NE.' ')THEN
              IF(J.GT.MAXCHR)MAXCHR=J
              GOTO815
            ENDIF
  813     CONTINUE
  815     CONTINUE
  810   CONTINUE
        WRITE(IFRMT(3:4),'(I2)')MAXCHR
C
        DO820I=1,N
          WRITE(ICOUT,IFRMT)IXTEMP(I),I
          CALL DPWRST('XXX','BUG ')
  820   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 5--                                       **
C               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
C               **  HOUSEKEEPING ARRAY                             **
C               *****************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 6--                         **
C               **  CLOSE THE DPZCHF.DAT FILE.       **
C               ***************************************
C
 8000 CONTINUE
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,IROW
 9013   FORMAT('N,IROW = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO9035I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEC3(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,YTEMP,
     1                  IWRITE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
C              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
C              A NUMERIC VARIABLE.  THIS IMPLEMENTS THE
C              "REFERENCE CHARACTER CODE" COMMAND.
C
C              FOR THE "CODECH" ROUTINE (WHICH IMPLEMENTS THE
C              "CHARACTER CODE" COMMAND"), EACH DISTINCT CHARACTER
C              VARIABLE WILL BE ASSIGNED AN INTEGER CODE DETERMINED
C              BY ORDER THAT THE FIRST OCCURENCE IS FOUND).
C
C              THIS VARIANT IS SIMILAR.  HOWEVER, INSTEAD OF BASING THE
C              CODE BASED ON THE ORDER OF FIRST APPEARANCE, THE CODE
C              WILL BE BASED ON A PREVIOUSLY DEFINED GROUP LABEL.  THIS
C              IS USEFUL WHEN, FOR EXAMPLE, READING SEVERAL SETS OF DATA
C              THAT USE THE SAME CATEGORICAL VARIABLE AND WE WANT THE
C              CODING TO BE CONSISTENT ACROSS THE DATA FILES.
C
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS IN
C                                THE CHARACTER VARIABLE.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
C                   IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2018/06
C     ORIGINAL VERSION--JUNE      2018.
C     UPDATED         --JUNE      2019. MOVE DIMENSION OF SCRATCH
C                                       REAL ARRAY TO CALLING ROUTINE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION YTEMP(*)
      DIMENSION W2HOLD(*)
      DIMENSION PARAM(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 IHRIG3
      CHARACTER*4 IHRIG4
C
      CHARACTER*4 ITYPEH(*)
      CHARACTER*4 IW21HO(*)
      CHARACTER*4 IW22HO(*)
      CHARACTER*4 IA(*)
      CHARACTER*4 IPARN(*)
      CHARACTER*4 IPARN2(*)
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='CH  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODEC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                     *
C               **  EXAMINE THE LEFT-HAND SIDE--                 *
C               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
C               **  ALREADY IN THE NAME LIST?                    *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
C               **  TABLE OF THE NAME ON THE LEFT.               *
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO100I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GOTO110
        ENDIF
  100 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN REFERENCE CHARACTER CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)MAXNAM
  113   FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)
  114   FORMAT('      ENTER      STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      THEN DELETE SOME OF THE ALREADY-USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  110 CONTINUE
C
C               **************************************************
C               **  STEP 2--                                     *
C               **  EXAMINE THE RIGHT-HAND SIDE--                *
C               **  IS THE SECOND NAME ON THE RIGHT HAND SIDE    *
C               **  A PREVIOUSLY DEFINED GROUP LABEL?            *
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRIG3=IHARG(7)
      IHRIG4=IHARG2(7)
      DO200I=1,MAXGRP
        IF(IHRIG3.EQ.IGRPVN(I)(1:4).AND.IHRIG4.EQ.IGRPVN(I)(5:8))THEN
          IGRP=I
          GOTO210
        ENDIF
  200 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,212)IHRIG3,IHRIG4
  212 FORMAT('      THE SPECIFIED GROUP (',2A4,') WAS NOT FOUND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  210 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
        WRITE(ICOUT,221)IGRP
  221   FORMAT('AT 210: IGRP = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 3--                              **
C               **  OPEN THE DPZCHF.DAT FILE.             **
C               ********************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
C
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
C
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,
     1            ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,311)
  311   FORMAT('      UNABLE TO OPEN THE FILE CHARACTER DATA FILE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,319)IFILE
  319   FORMAT('      ',A80)
        CALL DPWRST('XXX','BUG ')
        GOTO7000
      ENDIF
C
      READ(IOUNIT,'(I8)',END=371,ERR=371)NUMVAR
C
      IVAR=-1
      DO330I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=381,ERR=381)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
        ENDIF
  330 CONTINUE
      IF(IVAR.GT.0)GOTO399
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,331)IHRIGH,IHRIG2
  331 FORMAT('      VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',
     1       'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO7000
C
  371 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,373)
  373 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO7000
C
  381 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,383)
  383 FORMAT('      ERROR READING THE VARIABLE NAMES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO7000
C
  399 CONTINUE
C
C               *************************************************
C               **  STEP 4--                                   **
C               **  PERFORM THE CODING--                       **
C               **  STORE UNIQUE VALUES IN IXTEMP, COMPARE     **
C               **  TO LIST IN IXTEMP.                         **
C               *************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFRMT='(A   )'
      WRITE(IFRMT(3:5),'(I3)')25*IVAR
      IFRST=(IVAR-1)*25 + 1
      ILAST=IVAR*25 - 1
C
      DO410I=1,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
        DO420J=1,MAXGLA
          IF(IATEMP(IFRST:ILAST).EQ.IGRPLA(J,IGRP)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GOTO419
          ENDIF
  420   CONTINUE
        YTEMP(IROW)=-1.0
C
  419   CONTINUE
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
          WRITE(ICOUT,421)I,IFRST,ILAST
  421     FORMAT('AT 419: I,IFRST,ILAST = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,423)IATEMP(IFRST:ILAST)
  423     FORMAT('IATEMP(IFRST:ILAST) = ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  410 CONTINUE
      GOTO499
C
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',
     1       'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO7000
C
C
C               ******************************
C               **  STEP 5--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
  499 CONTINUE
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL MINIM(YTEMP,IROW,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(YTEMP,IROW,IWRITE,YMAX,IBUGA3,IERROR)
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)IHLEFT,IHLEF2,INT(YMIN)
  811   FORMAT('THE MINIMUM VALUE FOR ',2A4,' IS: ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)IHLEFT,IHLEF2,INT(YMAX)
  813   FORMAT('THE MAXIMUM VALUE FOR ',2A4,' IS: ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
C               **  HOUSEKEEPING ARRAY                             **
C               *****************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 7--                         **
C               **  CLOSE THE DPZCHF.DAT FILE.       **
C               ***************************************
C
 7000 CONTINUE
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODEC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,IROW
 9013   FORMAT('IERROR,N,IROW = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEDX(X,N,IWRITE,Y,XDIST,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--FOR CLASSIC 2-LEVEL FACTORIAL DESIGNS, IT IS CONVENIENT
C              FOR EACH OF THE FACTOR VARIABLES TO LABEL THE LOW VALUE AS
C              "-1" AND THE HIGH VALUE AS "+1".  IN ADDITION, THERE MAY BE
C              CENTER POINTS WHICH ARE CODED AS "0".  IF THE FACTOR
C              VARIABLE IS CODED IN THE ORIGINAL UNITS OF THE DATA, THIS
C              ROUTINE CAN BE USED TO CONVERT IT TO THE "-1" AND "+1"
C              CODING.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
C                   IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     VERSION NUMBER--2018/01
C     ORIGINAL VERSION--JANUARY   2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XDIST(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='DX  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEDX')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODEDX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,ISUBRO,N
   53   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEDX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        Y(1)=1.0
        GOTO8000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      DO137I=1,N
        Y(I)=1.0
  137 CONTINUE
      GOTO8000
  139 CONTINUE
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  PERFORM THE CODING--                                   **
C               *************************************************************
C
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL SORT(XDIST,NDIST,XDIST)
C
      IF(NDIST.EQ.1)THEN
        DO210I=1,N
          Y(I)=1.0
  210   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(2)
        DO220I=1,N
          IF(X(I).EQ.AVAL1)THEN
            Y(I)=-1.0
          ELSE
            Y(I)=1.0
          ENDIF
  220   CONTINUE
      ELSEIF(NDIST.EQ.3)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(2)
        AVAL3=XDIST(3)
        DO230I=1,N
          IF(X(I).EQ.AVAL1)THEN
            Y(I)=-1.0
          ELSEIF(X(I).EQ.AVAL3)THEN
            Y(I)=1.0
          ELSE
            Y(I)=0.0
          ENDIF
  230   CONTINUE
      ELSE
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,241)
  241   FORMAT('      THE RESPONSE VARIABLE CONTAINS MORE THAN THREE ',
     1         'DISTINCT VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,243)NDIST
  243   FORMAT('      THE NUMBER OF DISTINCT VALUES DETECTED WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
 8000 CONTINUE
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NDIST.LE.2)THEN
          WRITE(ICOUT,8112)
 8112     FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',
     1         '-1 AND +1 VALUES.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,8114)
 8114     FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',
     1         '-1, 0, AND +1 VALUES.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEDX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEDX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR
 9013   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,G15.7,F7.0)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODED2(X,N,IWRITE,Y,NOUT,XDIST,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--FOR CLASSIC 2-LEVEL FACTORIAL DESIGNS, IT IS CONVENIENT
C              FOR EACH OF THE FACTOR VARIABLES TO LABEL THE LOW VALUE AS
C              "-1" AND THE HIGH VALUE AS "+1".  IN ADDITION, THERE MAY BE
C              CENTER POINTS WHICH ARE CODED AS "0".  IF THE FACTOR
C              VARIABLE IS CODED IN THE ORIGINAL UNITS OF THE DATA, THIS
C              ROUTINE CAN BE USED TO CONVERT IT TO THE "-1" AND "+1"
C              CODING.
C
C              THIS ROUTINE IS SIMILAR TO "CODEDX".  THE DISTINCTION IS
C              THAT THIS ROUTINE ONLY SAVES THE MINIMUM VALUE (AS -1)
C              AND THE MAXIMUM VALUE (AS +1).  ALL OTHER VALUES ARE
C              DISCARDED.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C                     --NOUT   = THE INTEGER NUMBER OF OBSERVATIONS
C                                THAT ARE SAVED IN Y (NOT NECCESSARILY
C                                EQUAL TO N).
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
C                   IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     VERSION NUMBER--2018/10
C     ORIGINAL VERSION--OCTOBER   2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XDIST(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='D2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DED2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODED2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,ISUBRO,N
   53   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODED2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        Y(1)=1.0
        NOUT=1
        GOTO8000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      DO137I=1,N
        Y(I)=1.0
  137 CONTINUE
      NOUT=N
      GOTO8000
  139 CONTINUE
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  PERFORM THE CODING--                                   **
C               *************************************************************
C
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL SORT(XDIST,NDIST,XDIST)
C
      IF(NDIST.EQ.1)THEN
        DO210I=1,N
          Y(I)=1.0
  210   CONTINUE
        NOUT=N
      ELSEIF(NDIST.EQ.2)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(2)
        DO220I=1,N
          IF(X(I).EQ.AVAL1)THEN
            Y(I)=-1.0
          ELSE
            Y(I)=1.0
          ENDIF
  220   CONTINUE
        NOUT=N
      ELSEIF(NDIST.GE.3)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(NDIST)
        NOUT=0
        DO230I=1,N
          IF(X(I).EQ.AVAL1)THEN
            Y(I)=-1.0
            NOUT=NOUT+1
          ELSEIF(X(I).EQ.AVAL2)THEN
            Y(I)=1.0
            NOUT=NOUT+1
          ENDIF
  230   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
 8000 CONTINUE
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8112)
 8112   FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',
     1         '-1 AND +1 VALUES.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DED2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODED2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NOUT,NDIST
 9013   FORMAT('IERROR,NOUT,NDIST = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NOUT
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,F7.0)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEH(X,N,NUMINT,IWRITE,Y,XS,MAXOBV,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
C              OF THE INPUT VECTOR X
C              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
C              THE CODING IS AS FOLLOWS--
C                  THE FIRST NUMINT'TH OF THE DATA IS CODED AS 1.0
C                  THE NEXT  NUMINT'TH OF THE DATA IS CODED AS 2.0
C                  ETC.
C                  THE LAST  NUMINT'TH OF THE DATA IS CODED AS NUMINT
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y
C             WHICH WILL CONTAIN THE CODED VALUES
C             CORRESPONDING TO THE OBSERVATIONS IN
C             THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XS(MAXOBV)
CCCCC FOLLOWING LINES ADDED JUNE, 1990
CCCCC INCLUDE 'DPCOZ2.INC'
CCCCC EQUIVALENCE (G2RBAG(IGAR45),XS(1))
CCCCC END CHANGE
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='N   '
C
      IERROR='NO'
      IUPPER=MAXOBV
C
      X50=0.0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CODEH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N,IUPPER,NUMINT
   53 FORMAT('N,IUPPER,NUMINT = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(1.LE.N.AND.N.LE.IUPPER)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)IUPPER
  111 FORMAT('***** ERROR IN CODEH--',
     1'THE SECOND INPUT ARGUMENT (N) IS SMALLER THAN 1',
     1'OR LARGER THAN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,118)N
  118 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      IF(N.EQ.1)GOTO120
      GOTO129
  120 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,121)
CC121 FORMAT('***** NON-FATAL DIAGNOSTIC IN CODEH--',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1'THE SECOND INPUT ARGUMENT (N) HAS THE VALUE 1')
      Y(1)=1.0
      GOTO9000
  129 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,136)HOLD
CC136 FORMAT('***** NON-FATAL DIAGNOSTIC IN CODEH--',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      DO137I=1,N
      Y(I)=1.0
  137 CONTINUE
      GOTO9000
  139 CONTINUE
C
  190 CONTINUE
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  PERFORM THE CODING--                                   **
C               *************************************************************
C
      CALL SORT(X,N,XS)
C
      AN=N
C
 1400 CONTINUE
      DO1410I=1,N
      Y(I)=4.0
 1410 CONTINUE
C
      N2=(N+1)/2
      IARG1=(N2+1)/2
      IARG2=(N2+1)-IARG1
      IARG1R=N-IARG1+1
      IARG2R=N-IARG2+1
      X75=(XS(IARG1R)+XS(IARG2R))/2.0
      XCUT=X75
      DO1420I=1,N
      IF(X(I).LE.XCUT)Y(I)=3.0
 1420 CONTINUE
C
      N50=N/2
      N50P1=N50+1
      IEVODD=N-2*(N/2)
      IF(IEVODD.EQ.0)X50=(XS(N50)+XS(N50P1))/2.0
      IF(IEVODD.EQ.1)X50=XS(N50P1)
      XCUT=X50
      DO1430I=1,N
      IF(X(I).LE.XCUT)Y(I)=2.0
 1430 CONTINUE
C
      N2=(N+1)/2
      IARG1=(N2+1)/2
      IARG2=(N2+1)-IARG1
      X25=(XS(IARG1)+XS(IARG2))/2.0
      XCUT=X25
      DO1440I=1,N
      IF(X(I).LE.XCUT)Y(I)=1.0
 1440 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO8190
      IF(IWRITE.EQ.'OFF')GOTO8190
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)NUMINT
 8112 FORMAT('NUMBER OF CODE INTERVALS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      AI=1
      WRITE(ICOUT,8114)XS(1),AI
 8114 FORMAT('THE MINIMUM (= ',E15.7,' ) HAS CODE VALUE ',F10.0)
      CALL DPWRST('XXX','BUG ')
      AI=NUMINT
      WRITE(ICOUT,8116)XS(N),AI
 8116 FORMAT('THE MAXIMUM (= ',E15.7,' ) HAS CODE VALUE ',F10.0)
      CALL DPWRST('XXX','BUG ')
 8190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CODEH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N,NUMINT
 9013 FORMAT('N,NUMINT = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CODEN(X,N,NUMINT,IWRITE,Y,XS,MAXOBV,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
C              OF THE INPUT VECTOR X
C              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
C              THE CODING IS AS FOLLOWS--
C                  THE FIRST NUMINT'TH OF THE DATA IS CODED AS 1.0
C                  THE NEXT  NUMINT'TH OF THE DATA IS CODED AS 2.0
C                  ETC.
C                  THE LAST  NUMINT'TH OF THE DATA IS CODED AS NUMINT
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE CODED VALUES
C                                WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y
C             WHICH WILL CONTAIN THE CODED VALUES
C             CORRESPONDING TO THE OBSERVATIONS IN
C             THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XS(MAXOBV)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='N   '
      IERROR='NO'
C
      IUPPER=MAXOBV
      XMED=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,N,IUPPER,NUMINT
   53   FORMAT('IBUGA3,N,IUPPER,NUMINT = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      IS LESS THAN 1 OR GREATER THAN ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      DO137I=1,N
        Y(I)=1.0
  137 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  PERFORM THE CODING--                                   **
C               *************************************************************
C
      CALL SORT(X,N,XS)
C
      AN=N
      IF(NUMINT.EQ.1)THEN
        DO1110I=1,N
          Y(I)=NUMINT
 1110   CONTINUE
      ELSEIF(NUMINT.GE.3)THEN
        DO1310I=1,N
          Y(I)=NUMINT
 1310   CONTINUE
        ANUMIN=NUMINT
        JMAX=NUMINT-1
        DO1320J=1,JMAX
          JREV=JMAX-J+1
          AJREV=JREV
          P=AJREV/ANUMIN
          AK=P*AN
          K1=AK
          K2=AK+1.0
          IF(K1.LE.1)K1=1
          IF(K1.GE.N)K1=N
          IF(K2.LE.1)K2=1
          IF(K2.GE.N)K2=N
          XCUT=(XS(K1)+XS(K2))/2.0
          DO1350I=1,N
            IF(X(I).LE.XCUT)Y(I)=JREV
 1350     CONTINUE
 1320   CONTINUE
      ELSE
        DO1210I=1,N
          Y(I)=NUMINT
 1210   CONTINUE
        N50=N/2
        N50P1=N50+1
        IEVODD=N-2*(N/2)
        IF(IEVODD.EQ.0)XMED=(XS(N50)+XS(N50P1))/2.0
        IF(IEVODD.EQ.1)XMED=XS(N50P1)
        XCUT=XMED
        DO1250I=1,N
          IF(X(I).LE.XCUT)Y(I)=1.0
 1250   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8112)NUMINT
 8112   FORMAT('NUMBER OF CODE INTERVALS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        AI=1
        WRITE(ICOUT,8114)XS(1),AI
 8114   FORMAT('THE MINIMUM (= ',E15.7,' ) HAS CODE VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        AI=NUMINT
        WRITE(ICOUT,8116)XS(N),AI
 8116   FORMAT('THE MAXIMUM (= ',E15.7,' ) HAS CODE VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,NUMINT
 9013   FORMAT('IERROR,N,NUMINT = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEST(ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
C              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
C              A STRING.  THE LHS DEFINES THE BASE NAME FOR THE
C              STRINGS.
C     OUTPUT--THE CHARACTER STRINGS.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/10
C     ORIGINAL VERSION--OCTOBER   2011.
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*8 ISTRIN
      CHARACTER*8 IHLEFT
      CHARACTER*4 IHLEF3
      CHARACTER*4 IHLEF4
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*4 ISTRZ2(24)
C
      CHARACTER*4 ISUBN0
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 IERRFI
C
      CHARACTER*24 IATEMP
      CHARACTER*12 IFRMT
      CHARACTER*24 IXTEMP(9999)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='ST  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  OPEN THE DPZCHF.DAT FILE.             **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
C
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
C
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,
     1            ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CHARACTER CODE STRING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      UNABLE TO OPEN THE FILE CHARACTER DATA FILE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)IFILE
  119   FORMAT('      ',A80)
        CALL DPWRST('XXX','BUG ')
        GOTO8000
      ENDIF
C
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
C
      IVAR=-1
      DO130I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GOTO199
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',
     1       'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',
     1       'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  199 CONTINUE
C
C               **********************************
C               **  STEP 2--                    **
C               **  DETERMINE NUMBER OF STRINGS **
C               **  TO CREATE                   **
C               **********************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NSTR=NUMVAR
      IF(NSTR.GT.9999)NSTR=9999
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  EXTRACT THE BASE NAME ON THE LHS OF THE    **
C               **  EQUAL SIGN AND THEN LOOP THROUGH THE       **
C               **  NUMBER OF STRINGS TO CREATE.               **
C               *************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT(1:4)=IHARG(1)
      IHLEFT(5:8)=IHARG2(1)
      NBASE=1
      DO310I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE=I
          GOTO319
        ENDIF
  310 CONTINUE
  319 CONTINUE
C
      IF(NSTR.LE.9)THEN
        IF(NBASE.GT.7)NBASE=7
      ELSEIF(NSTR.LE.99)THEN
        IF(NBASE.GT.6)NBASE=6
      ELSEIF(NSTR.LE.999)THEN
        IF(NBASE.GT.5)NBASE=5
      ELSE
        IF(NBASE.GT.4)NBASE=4
      ENDIF
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IVAR.EQ.1)THEN
        IFRMT='(A24)'
      ELSE
        IFRMT='(   X,A24)'
        WRITE(IFRMT(2:4),'(I3)')25*(IVAR-1)
      ENDIF
C
      N=0
      IROW=0
C
      DO410I=1,MAXOBV
C
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
C
C       CHECK TO SEE IF TEXT ON CURRENT ROW IS NEW OR
C       HAS BEEN PREVIOUSLY ENTERED.
C
        INEW=1
        IF(N.GE.1)THEN
          DO420J=1,N
            IF(IATEMP(1:24).EQ.IXTEMP(J)(1:24))THEN
              INEW=0
              GOTO429
            ENDIF
  420     CONTINUE
  429     CONTINUE
        ENDIF
C
C       ADD NEW STRING IF REQUIRED
C
        IF(INEW.EQ.0)GOTO410
        N=N+1
        IF(N.GT.9999)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,431)
  431     FORMAT('      ATTEMPT TO CREATE MORE THAN 9,999 STRINGS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,433)
  433     FORMAT('      NO MORE STRINGS WILL BE GENERATED.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IXTEMP(N)=' '
          IXTEMP(N)=IATEMP(1:24)
          ISTRIN=' '
          ISTRIN(1:NBASE)=IHLEFT(1:NBASE)
          IF(N.LE.9)THEN
            WRITE(ISTRIN(NBASE+1:NBASE+1),'(I1)')N
          ELSEIF(N.LE.99)THEN
            WRITE(ISTRIN(NBASE+1:NBASE+2),'(I2)')N
          ELSEIF(N.LE.999)THEN
            WRITE(ISTRIN(NBASE+1:NBASE+3),'(I3)')N
          ELSE
            WRITE(ISTRIN(NBASE+1:NBASE+4),'(I4)')N
          ENDIF
C
          NEWNAM='NO'
          NEWCOL='NO'
          ICASEL='UNKN'
          NIOLD1=0
          ICOLL=0
C
          DO510II=1,NUMNAM
            I2=II
            IF(ISTRIN(1:4).EQ.IHNAME(I2).AND.
     1         ISTRIN(5:8).EQ.IHNAM2(I2))THEN
              IF(IUSE(I2).EQ.'F')THEN
                ICASEL='STRI'
                ILISTL=I2
                GOTO519
              ELSE
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,111)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,513)ISTRIN
  513           FORMAT('      THE NAME ',A8,' ALREADY EXISTS, BUT NOT ',
     1                 'AS A STRING.')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,515)
  515           FORMAT('      THIS STRING WILL NOT BE CREATED.')
                CALL DPWRST('XXX','BUG ')
                GOTO9000
              ENDIF
            ENDIF
  510     CONTINUE
  519     CONTINUE
C
          NEWNAM='YES'
          ICASEL='STRI'
C
          ILISTL=NUMNAM+1
          IF(ILISTL.GT.MAXNAM)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,522)
  522       FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',
     1             'FUNCTION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,524)MAXNAM
  524       FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  ADD THE CURRENT STRING                         **
C               *****************************************************
C
          ISTEPN='6'
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'STGL')
     1       CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          NCHAR=1
          DO605JJ=24,1,-1
            IF(IXTEMP(N)(JJ:JJ).NE.' ')THEN
              NCHAR=JJ
              GOTO609
            ENDIF
  605     CONTINUE
  609     CONTINUE
          IHLEF3=ISTRIN(1:4)
          IHLEF4=ISTRIN(5:8)
          DO611J=1,NCHAR
            ISTRZ2(J)=' '
            ISTRZ2(J)(1:1)=IXTEMP(N)(J:J)
  611     CONTINUE
C
          CALL DPINFU(ISTRZ2,NCHAR,IHNAME,IHNAM2,IUSE,IN,
     1                IVSTAR,IVSTOP,
     1                NUMNAM,IANS,IWIDTH,IHLEF3,IHLEF4,ILISTL,
     1                NEWNAM,MAXNAM,
     1                IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ENDIF
C
  410 CONTINUE
      GOTO499
C
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)I
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',
     1       'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
  499 CONTINUE
      GOTO8000
C
C               ***************************************
C               **  STEP 88--                        **
C               **  CLOSE THE DPZCHF.DAT FILE.       **
C               ***************************************
C
 8000 CONTINUE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8001)N,ISTRIN(1:NBASE)
 8001   FORMAT('      ',I5,' STRINGS CREATED WITH BASE NAME = ',A8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEST')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,IROW
 9013   FORMAT('N,IROW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEX(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN DATA OF THE FORM
C
C
C              1  1  1  0  0  0  1  1
C
C              WE WANT TO CREATE A CODED VARIABLE
C
C              1  1  1  0  0  0  2  2
C
C              THAT IS, FOR EACH NON-ZERO CHUNK, WE WANT TO
C              CREATE A COUNTER FOR EACH NON-ZERO BLOCK.
C
C              THIS IS USED IN THE CONTEXT OF A TAG VARIABLE
C              WHERE THE TAG IS SET TO 1 WHEN SOME CONDITION IS
C              SATISFIED.  HOWEVER, WE WANT TO UNIQUELY IDENTIFY
C              EACH CONTIGUOUS CHUNK OF DATA THAT SATISFIES THE
C              CONDITION.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/07
C     ORIGINAL VERSION--JULY      2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='X   '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEX')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('      THE NUMBER OF OBSERVATIONS, ',I8,' IS LESS ',
     1         'THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************************************************
C               **  STEP 2--                                       **
C               **  PERFORM THE CODING--                           **
C               *****************************************************
C
      ICNT=0
      IFLAG=0
      DO600I=1,N
        IF(X(I).EQ.0.0)THEN
          Y(I)=0.0
          IFLAG=0
        ELSE
          IF(IFLAG.EQ.0)THEN
            ICNT=ICNT+1
            Y(I)=REAL(ICNT)
            IFLAG=1
          ELSEIF(IFLAG.EQ.1)THEN
            Y(I)=REAL(ICNT)
          ENDIF
        ENDIF
  600 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)X(1),Y(1)
  812   FORMAT('THE FIRST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',
     1         'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,814)X(N),Y(N)
  814   FORMAT('THE LAST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',
     1         'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CODEZ(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE IS SIMILAR TO THE CODE ROUTINE.
C              HOWEVER, IT DIFFERS IN ONE KEY RESPECT.  THE CODE
C              ROUTINE CODES BASED ON THE DISTINCT VALUES REGARDLESS
C              OF THE ORDER OF THE DATA.  THIS ROUTINE CREATES THE
C              CODE BASED ON WHEN THE INPUT VECTOR CHANGES VALUE.
C              FOR EXAMPLE, IF X HAS
C
C                  1 1 1 2 2 2 3 3 3 1 1 2 2 2 2 3 3 3
C
C              THEN THE CODED VECTOR WILL BE
C
C                  1 1 1 2 2 2 3 3 3 4 4 5 5 5 5 6 6
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS TO BE CODED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
C                                THE CODED VALUES WILL BE PLACED.
C     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
C             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2016/6
C     ORIGINAL VERSION--JUNE      2016.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CODE'
      ISUBN2='Z   '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEZ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *****************************
C               **  COMPUTE CODED VALUES.  **
C               *****************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('      THE NUMBER OF OBSERVATIONS, ',I8,' IS LESS ',
     1         'THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GOTO9000
      ENDIF
C
C
C               *****************************************************
C               **  STEP 2--                                       **
C               **  PERFORM THE CODING--                           **
C               *****************************************************
C
      HOLD=X(1)
      ACODE=1.0
      Y(1)=ACODE
      DO600I=2,N
        IF(X(I).EQ.HOLD)THEN
          Y(I)=ACODE
        ELSE
          HOLD=X(I)
          ACODE=ACODE+1.0
          Y(I)=ACODE
        ENDIF
  600 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)X(1),Y(1)
  812   FORMAT('THE FIRST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',
     1         'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,814)X(N),Y(N)
  814   FORMAT('THE LAST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',
     1         'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEZ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE COENAM(IV1,IV2,IWORD1,IWORD2,IBUGCN,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CREATES A HOLLERITH COEFFICIENT NAME
C              FROM THE 2 INPUT INTEGER VALUES IV1 AND IV2.
C              IT ALSO AUTOMATICALLY PUTS THE LETTER A AS
C              THE FIRST LETTER OF THE PARAMETER NAME.
C              EXAMPLES--
C                 INPUT--IV1 = 1   AND IV2 = 7    OUTPUT--A17
C                 INPUT--IV1 = 2   AND IV2 = 3    OUTPUT--A23
C                 INPUT--IV1 = 5   AND IV2 = 2    OUTPUT--A52
C     NOTE--IF THE OUTPUT STRING HAPPENS TO CONSIST OF
C           1 TO 4 CHARACTERS, THEN CHARACTERS 1 TO 4
C           WILL BE PLACED INTO THE FIRST HOLLERITH
C           VARIABLE IWORD1.
C           IF THE OUTPUT STRING HAPPENS TO CONSIST OF
C           MORE THAN 4 CHARACTERS, THEN CHARACTERS 5 TO 8
C           WILL BE PLACED INTO THE SECOND HOLLERITH
C           VARIABLE IWORD2.
C           IF THE OUTPUT STRING HAPPENS TO CONSIST OF
C           MORE THAN 8 CHARACTERS, THEN CHARACTERS 9 ON UP
C           WILL BE IGNORED.
C     NOTE--IV1 AND IV2 ARE INTEGER VARIABLES.
C     NOTE--IWORD1 IS A HOLLERITH VARIABLE.
C         --IWORD2 IS A HOLLERITH VARIABLE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1978.
C     UPDATED         --MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IBUGCN
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTRIT
      CHARACTER*4 ISTRIN
      CHARACTER*4 IWORD3
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION ISTRIT(15)
      DIMENSION ISTRIN(30)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      ISUBN1='COEN'
      ISUBN2='AM  '
C
      IF(IBUGCN.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COENAM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IV1,IV2
   52 FORMAT('IV1,IV2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGCN
   53 FORMAT('IBUGCN = ',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  DEFINE THE FIRST CHARACTER  **
C               **  OF THE PARAMETER NAME       **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      K=0
      K=K+1
      ISTRIN(K)='A'
C
C               *******************************************
C               **  STEP 2--                             **
C               **  FORM THE STRING CONTAINING           **
C               **  THE 1 CHARACTER PER WORD             **
C               **  REPRESENTATION OF THE VALUE IN IV1.  **
C               *******************************************
C
      J=0
      ISTEPN='2'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREM=IV1
      DO100IPASS=1,10
      J=J+1
      IDIGIT=IREM-10*(IREM/10)
      IF(IDIGIT.EQ.0)ISTRIT(J)='0'
      IF(IDIGIT.EQ.1)ISTRIT(J)='1'
      IF(IDIGIT.EQ.2)ISTRIT(J)='2'
      IF(IDIGIT.EQ.3)ISTRIT(J)='3'
      IF(IDIGIT.EQ.4)ISTRIT(J)='4'
      IF(IDIGIT.EQ.5)ISTRIT(J)='5'
      IF(IDIGIT.EQ.6)ISTRIT(J)='6'
      IF(IDIGIT.EQ.7)ISTRIT(J)='7'
      IF(IDIGIT.EQ.8)ISTRIT(J)='8'
      IF(IDIGIT.EQ.9)ISTRIT(J)='9'
      IREM=IREM-IDIGIT
      IREM=IREM/10
      IF(IREM.LE.0)GOTO140
  100 CONTINUE
  140 CONTINUE
      N1=J
C
      DO150I=1,N1
      K=K+1
      IREV=N1-I+1
      ISTRIN(K)=ISTRIT(IREV)
  150 CONTINUE
C
C               *******************************************
C               **  STEP 3--                             **
C               **  FORM THE STRING CONTAINING           **
C               **  THE 1 CHARACTER PER WORD             **
C               **  REPRESENTATION OF THE VALUE IN IV2.  **
C               *******************************************
C
      ISTEPN='3'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      IREM=IV2
      DO200IPASS=1,10
      J=J+1
      IDIGIT=IREM-10*(IREM/10)
      IF(IDIGIT.EQ.0)ISTRIT(J)='0'
      IF(IDIGIT.EQ.1)ISTRIT(J)='1'
      IF(IDIGIT.EQ.2)ISTRIT(J)='2'
      IF(IDIGIT.EQ.3)ISTRIT(J)='3'
      IF(IDIGIT.EQ.4)ISTRIT(J)='4'
      IF(IDIGIT.EQ.5)ISTRIT(J)='5'
      IF(IDIGIT.EQ.6)ISTRIT(J)='6'
      IF(IDIGIT.EQ.7)ISTRIT(J)='7'
      IF(IDIGIT.EQ.8)ISTRIT(J)='8'
      IF(IDIGIT.EQ.9)ISTRIT(J)='9'
      IREM=IREM-IDIGIT
      IREM=IREM/10
      IF(IREM.LE.0)GOTO240
  200 CONTINUE
  240 CONTINUE
      N2=J
C
      DO250I=1,N2
      K=K+1
      IREV=N2-I+1
      ISTRIN(K)=ISTRIT(IREV)
  250 CONTINUE
C
C               *******************************************************
C               **  STEP 4--                                         **
C               **  CONVERT THE 1 CHARACTER PER WORD REPRESENTATION  **
C               **  FOR THE PARAMETER NAME                           **
C               **  INTO A 4 CHARACTER PER WORD REPRESENTATION.      **
C               *******************************************************
C
      ISTEPN='4'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=1
      ISTOP=K
      CALL DP1H4H(ISTART,ISTOP,ISTRIN,
     1IWORD1,IWORD2,IWORD3,NUMWD,NUMCH,IBUGCN,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGCN.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COENAM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IERROR
 9012 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N1,N2,ISTART,ISTOP
 9013 FORMAT('N1,N2,ISTART,ISTOP = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)(ISTRIN(I),I=1,K)
 9014 FORMAT('ISTRIN(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NUMBPC,NUMCPW
 9015 FORMAT('NUMBPC,NUMCPW = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)NUMWD,NUMCH
 9016 FORMAT('NUMWD, NUMCH = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IWORD1,IWORD2
 9017 FORMAT('IWORD1,IWORD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)IWORD3
 9018 FORMAT('IWORD3 = ',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COLLAP(NVAR, X, Y, LOCY, NX, NY, DIM, CONFIG)
C
C        ALGORITHM AS 51.1  APPL. STATIST. (1972) VOL.21, P.218
C
C        COMPUTES A MARGINAL TABLE FROM A COMPLETE TABLE.
C        ALL PARAMETERS ARE ASSUMED VALID WITHOUT TEST.
C
C        IF THE VALUE OF NVAR IS TO BE GREATER THAN 7, THE
C        DIMENSIONS IN THE DECLARATIONS OF SIZE AND COORD MUST
C        BE INCREASED TO NVAR+1 AND NVAR RESPECTIVELY.
C
      INTEGER SIZE(8), DIM(NVAR), CONFIG(NVAR), COORD(7)
C
C        THE LARGER TABLE IS X AND THE SMALLER ONE IS Y
C
      REAL X(NX), Y(NY), ZERO
      DATA ZERO /0.0/
C
C        INITIALISE ARRAYS
C
      SIZE(1) = 1
      DO 10 K = 1, NVAR
         L = CONFIG(K)
         IF (L .EQ. 0) GOTO 20
         SIZE(K + 1) = SIZE(K) * DIM(L)
   10 CONTINUE
C
C        FIND NUMBER OF VARIABLES IN CONFIGURATION
C
      K = NVAR + 1
   20 CONTINUE
      N = K - 1
C
C        INITIALISE Y. FIRST CELL OF MARGINAL TABLE IS
C        AT Y(LOCY) AND TABLE HAS SIZE(K) ELEMENTS
C
      LOCU = LOCY + SIZE(K) - 1
      DO 30 J = LOCY, LOCU
         Y(J) = ZERO
   30 CONTINUE
C
C        INITIALISE COORDINATES
C
      DO 50 K = 1, NVAR
         COORD(K) = 0
   50 CONTINUE
C
C        FIND LOCATIONS IN TABLES
C
      I = 1
   60 CONTINUE
      J = LOCY
      DO 70 K = 1, N
         L = CONFIG(K)
         J = J + COORD(L) * SIZE(K)
   70 CONTINUE
      Y(J) = Y(J) + X(I)
C
C        UPDATE COORDINATES
C
      I = I + 1
      DO 80 K = 1, NVAR
         COORD(K) = COORD(K) + 1
         IF (COORD(K) .LT. DIM(K)) GOTO 60
         COORD(K) = 0
   80 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMARI(Y1,Y2,Y3,Y4,N1,IACASE,IWRITE,
     1                  Y5,Y6,N5,SCAL3,ITYP3,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT COMPLEX ARITHMETIC OPERATIONS
C              OF THE COMPLEX DATA IN Y1,Y2 AND Y3,Y4.
C
C     OPERATIONS--ADDITION
C                 SUBTRACTTION
C                 MULTIPLICATION
C                 DIVISION
C                 EXPONENTIATION
C                 SQUARE ROOT
C                 ROOTS OF A POLYNOMIAL (WITH COMPLEX COEFFICIENTS)
C                 CONJUGATE
C
C     INPUT  ARGUMENTS--Y1 (REAL PART)       Y2 (IMAGINARY PART)
C                     --Y3 (REAL PART)       Y4 (IMAGINARY PART)
C     OUTPUT ARGUMENTS--Y5 (REAL PART)       Y6 (IMAGINARY PART)
C
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTORS Y5(.) AND Y6(.)
C           BEING IDENTICAL TO THE INPUT VECTORS Y1(.) AND Y2(.), OR
C           Y3(.) AND Y4(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/5
C     ORIGINAL VERSION--APRIL     1987.
C     UPDATED         --AUGUST    1987.  COMPLEX SQUARE ROOT
C     UPDATED         --AUGUST    1987.  COMPLEX ROOTS OF POLYNOMIAL
C     UPDATED         --SEPTEMBER 1987.  COMPLEX CONJUGATE
C     UPDATED         --MAY       1995.  EQUIVALENCE FOR ARRAYS
C     UPDATED         --AUGUST    1995.  REPLACE NUMERICAL RECIPES
C                                        ROUTINE FOR COMPLEX ROOTS
C                                        WITH CMLIB ROUTINE
C     UPDATED         --JUNE      2019.  DIMENSION COEFS, ROOTS,
C                                        WORK, ERRBND
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IACASE
      CHARACTER*4 IWRITE
      CHARACTER*4 ITYP3
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMPLEX STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      COMPLEX CY1Y2
      COMPLEX CTRANS
      COMPLEX COEFS
      COMPLEX ROOTS
CCCCC FOLLOWING LINES ADDED AUGUST 1995
      COMPLEX WORK
C
C-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DY1
      DOUBLE PRECISION DY2
      DOUBLE PRECISION DY3
      DOUBLE PRECISION DY4
      DOUBLE PRECISION DY5
      DOUBLE PRECISION DY6
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DE
      DOUBLE PRECISION DC
      DOUBLE PRECISION DS
C
C-----LOGICAL STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC LOGICAL POLISH
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION Y5(*)
      DIMENSION Y6(*)
C
      DIMENSION COEFS(MAXOBV)
      DIMENSION ROOTS(MAXOBV)
      DIMENSION WORK(MAXOBV)
      DIMENSION ERRBND(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(JGAR15),COEFS(1))
      EQUIVALENCE (GARBAG(JGAR17),ROOTS(1))
      EQUIVALENCE (GARBAG(JGAR19),WORK(1))
      EQUIVALENCE (GARBAG(IGAR10),ERRBND(1))
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMA'
      ISUBN2='RI  '
C
      IERROR='NO'
C
      SCAL3=(-999.0)
      ITYP3='VECT'
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'MARI')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COMARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO,IACASE,IWRITE
   52 FORMAT('IBUGA3,ISUBRO,IACASE,IWRITE = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N1
   53 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N1
      WRITE(ICOUT,56)I,Y1(I),Y2(I),Y3(I),Y4(I)
   56 FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I) = ',I8,4E13.5)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ***********************************************
C               **  CARRY OUT COMPLEX ARITHMETIC OPERATIONS  **
C               ***********************************************
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
C               ********************************************
C
      IF(N1.LT.1)GOTO1100
      GOTO1190
C
 1100 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN COMARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COAD')WRITE(ICOUT,1154)
 1154 FORMAT('      THE COMPLEX ADDITION IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COAD')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COSU')WRITE(ICOUT,1155)
 1155 FORMAT('      THE COMPLEX SUBTRACTION IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COSU')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COMU')WRITE(ICOUT,1156)
 1156 FORMAT('      THE COMPLEX MULTIPLICATION IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COMU')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'CODI')WRITE(ICOUT,1157)
 1157 FORMAT('      THE COMPLEX DIVISION IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'CODI')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COEX')WRITE(ICOUT,1158)
 1158 FORMAT('      THE COMPLEX EXPONENTIATION IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COEX')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COSR')WRITE(ICOUT,1159)
 1159 FORMAT('      THE COMPLEX SQUARE ROOT IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COSR')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'CORO')WRITE(ICOUT,1160)
 1160 FORMAT('      THE COMPLEX ROOTS ARE TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'CORO')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COR1')WRITE(ICOUT,1161)
 1161 FORMAT('      THE COMPLEX ROOTS ARE TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COR1')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COCO')WRITE(ICOUT,1162)
 1162 FORMAT('      THE COMPLEX CONJUGATE IS TO BE ',
     1'COMPUTED')
      IF(IACASE.EQ.'COCO')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1171)
 1171 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)N1
 1173 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1190 CONTINUE
C
C               *********************************
C               **  STEP 12--                  **
C               **  BRANCH TO THE PROPER CASE  **
C               *********************************
C
      IF(IACASE.EQ.'COAD')GOTO2100
      IF(IACASE.EQ.'COSU')GOTO2200
      IF(IACASE.EQ.'COMU')GOTO2300
      IF(IACASE.EQ.'CODI')GOTO2400
      IF(IACASE.EQ.'COEX')GOTO2500
      IF(IACASE.EQ.'COSR')GOTO2600
      IF(IACASE.EQ.'CORO')GOTO2700
      IF(IACASE.EQ.'COR1')GOTO2700
      IF(IACASE.EQ.'COCO')GOTO2800
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** INTERNAL ERROR IN COMARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      IACASE NOT EQUAL TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      COAD, COSU, COMU, CODI,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      COEX, COSR, CORO, COR1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      OR COCO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1221)
 1221 FORMAT('      IACASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *********************************************
C               **  STEP 21--                              **
C               **  TREAT THE COMPLEX ADDITION       CASE  **
C               *********************************************
C
 2100 CONTINUE
      DO2110I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1+DY3
      DY6=DY2+DY4
      Y5(I)=DY5
      Y6(I)=DY6
 2110 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *********************************************
C               **  STEP 22--                              **
C               **  TREAT THE COMPLEX SUBTRACTION    CASE  **
C               *********************************************
C
 2200 CONTINUE
      DO2210I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1-DY3
      DY6=DY2-DY4
      Y5(I)=DY5
      Y6(I)=DY6
 2210 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *********************************************
C               **  STEP 23--                              **
C               **  TREAT THE COMPLEX MULTIPLICATION CASE  **
C               *********************************************
C
 2300 CONTINUE
      DO2310I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1*DY3-DY2*DY4
      DY6=DY1*DY4+DY2*DY3
      Y5(I)=DY5
      Y6(I)=DY6
 2310 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *********************************************
C               **  STEP 24--                              **
C               **  TREAT THE COMPLEX DIVISION       CASE  **
C               *********************************************
C
 2400 CONTINUE
      DO2410I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DDEN=DY3**2+DY4**2
      IF(DDEN.NE.0.0D0)GOTO2419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2411)
 2411 FORMAT('***** ERROR IN COMARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2412)
 2412 FORMAT('      A ZERO DENOMINATOR WAS ENCOUNTERED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2413)
 2413 FORMAT('      IN ATTEMPTING TO CARRY OUT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2414)
 2414 FORMAT('      A COMPLEX DIVISION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2415)I
 2415 FORMAT('      THE ',I8,'TH ELEMENT OF THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2416)
 2416 FORMAT('      REAL AND IMAGINARY PARTS OF THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2417)
 2417 FORMAT('      COMPLEX DIVISOR ARE BOTH 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2418)I,Y3(I),Y4(I)
 2418 FORMAT('I,Y3(I),Y4(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2419 CONTINUE
      DY5=(DY1*DY3+DY2*DY4)/DDEN
      DY6=(DY2*DY3-DY1*DY4)/DDEN
      Y5(I)=DY5
      Y6(I)=DY6
 2410 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *********************************************
C               **  STEP 25--                              **
C               **  TREAT THE COMPLEX EXPONENTIATION CASE  **
C               *********************************************
C
 2500 CONTINUE
      DO2510I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DE=DEXP(DY1)
      DC=DCOS(DY2)
      DS=DSIN(DY2)
      DY5=DE*DC
      DY6=DE*DS
      Y5(I)=DY5
      Y6(I)=DY6
 2510 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *********************************************
C               **  STEP 26--                              **
C               **  TREAT THE COMPLEX SQUARE ROOT    CASE  **
C               *********************************************
C
 2600 CONTINUE
      DO2610I=1,N1
      CY1Y2=CMPLX(Y1(I),Y2(I))
      CTRANS=CSQRT(CY1Y2)
      Y5(I)=REAL(CTRANS)
      Y6(I)=AIMAG(CTRANS)
 2610 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               ***********************************************
C               **  STEP 27--                                **
C               **  TREAT THE COMPLEX ROOTS OF A POLYNOMIAL  **
C               **  WITH COMPLEX COEFFICIENTS CASE           **
C               ***********************************************
C
 2700 CONTINUE
      NCOEFS=N1
      NROOTS=NCOEFS-1
C
CCCCC AUGUST 1995.  REPLACE NUMERICAL RECIPES ROUTINE WITH
CCCCC SLATEC ROUTINE.
CCCCC CPZERO EXPECTS COEFFICIENTS IN OPPOSIT ORDER OF ZROOTS.
CCCCC DO2710I=1,NCOEFS
CCCCC COEFS(I)=CMPLX(Y1(I),Y2(I))
C2710 CONTINUE
      ICOUNT=0
      DO2710I=NCOEFS,1,-1
      ICOUNT=ICOUNT+1
      COEFS(ICOUNT)=CMPLX(Y1(I),Y2(I))
 2710 CONTINUE
C
      IFLG=0
      CALL CPZERO(NROOTS,COEFS,ROOTS,WORK,IFLG,ERRBND)
      IF(IFLG.EQ.1)THEN
        WRITE(ICOUT,2721)
 2721   FORMAT('***** ERROR IN COMARI--LEADING COEFFICIENT IS ',
     1         'ZERO OR DEGREE IS ZERO')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLG.EQ.2)THEN
        WRITE(ICOUT,2726)
 2726   FORMAT('***** ERROR IN COMARI--ROOTS DID NOT CONVERGE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
CCCCC POLISH=.FALSE.
CCCCC CALL ZROOTS(COEFS,NROOTS,ROOTS,POLISH)
C
CCCCC DO2720I=1,NROOTS
CCCCC ROOTS(I)=ROOTS(I)*(1.0+0.01*I)
C2720 CONTINUE
C
CCCCC POLISH=.TRUE.
CCCCC CALL ZROOTS(COEFS,NROOTS,ROOTS,POLISH)
C
      DO2730I=1,NROOTS
      Y5(I)=REAL(ROOTS(I))
      Y6(I)=AIMAG(ROOTS(I))
 2730 CONTINUE
C
      ITYP3='VECT'
      N5=NROOTS
      GOTO9000
C
C               *********************************************
C               **  STEP 28--                              **
C               **  TREAT THE COMPLEX CONJUGATE      CASE  **
C               *********************************************
C
 2800 CONTINUE
      DO2810I=1,N1
      Y5(I)=Y1(I)
      Y6(I)=(-Y2(I))
 2810 CONTINUE
C
      ITYP3='VECT'
      N5=N1
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'MARI')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COMARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IACASE,IWRITE
 9012 FORMAT('IBUGA3,ISUBRO,IACASE,IWRITE = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IERROR
 9013 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)N1,N5
 9017 FORMAT('N1,N5 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)SCAL3,ITYP3
 9018 FORMAT('SCAL3,ITYP3 = ',E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IF(ITYP3.EQ.'SCAL')GOTO9090
      DO9015I=1,N1
      WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I),Y4(I),Y5(I),Y6(I)
 9016 FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I),Y5(I),Y6(I) = ',I8,6E13.5)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMDIG(X,N,IWRITE,XDIGI,NDIGI,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE COMMON DIGITS FOR A
C              VECTOR OF NUMBERS.  FOR EXAMPLE, GIVEN
C                0.0321, 0.0323, 0.0329, 0.0325
C              THE COMMON DIGITS ARE 0.03.  NOTE THAT ONLY DIGITS
C              TO THE RIGHT OF THE DECIMAL PLACE ARE CONSIDERED.
C              THE FOLLOWING SPECIAL CASES ARE CONSIDERED:
C                  1) IF THE FIRST DECIMAL DOES NOT AGREE, SET
C                     XDIGI=-1.0.
C                  2) IF THE INTEGER PORTION OF THE NUMBER DOES
C                     NOT AGREE, THEN SET XDIGI=-1.0.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--XDIGI  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED COMMON DIGITS
C                     --NDIGI  = THE INTEGER VALUE OF THE
C                                NUMBER OF COMMON DIGITS
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             COMMON DIGITS
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001.8
C     ORIGINAL VERSION--AUGUST    2001.
C
      PARAMETER(MAXDIG=7)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION DIGITS(MAXDIG)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMD'
      ISUBN2='IG  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COMDIG--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.GE.2)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN COMDIG--THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH THE COMMON DIGITS ARE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      TO BE COMPUTED MUST BE 2 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMDIG--',
     1'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XDIGI=ABS(HOLD)-REAL(INT(ABS(HOLD)))
      NDIGI=MAXDIG
      GOTO9000
  139 CONTINUE
C
C  CHECK IF INTEGER PORTION OF NUMBERS MATCHES FOR ALL THE NUMBERS.
C
      IHOLD=INT(X(1))
      DO145I=2,N
        IXTEMP=INT(X(I))
        IF(IXTEMP.NE.IHOLD)THEN
          NDIG=-1
          XDIGI=0.0
          IF(IFEEDB.EQ.'OFF')GOTO149
          IF(IWRITE.EQ.'OFF')GOTO149
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,146)N
  146 FORMAT('THE INTEGER PORTION OF THE  ',I8,' OBSERVATIONS DOES ',
     1       'NOT MATCH.')
          CALL DPWRST('XXX','BUG ')
  149     CONTINUE
          GOTO800
        ENDIF
  145 CONTINUE
C
  190 CONTINUE
C
C               ************************
C               **  STEP 2--          **
C               **  COMPUTE THE DIGITS**
C               ************************
C
      XDIGI=0.0
      NDIGI=0
C
      DO200L=1,MAXDIG
        ATEMP=X(1)*10**(L-1)
        ADIG=ABS(ATEMP) - INT(ABS(ATEMP))
        IDIG=INT(ADIG*10)
        DO300I=2,N
          ATEMP=X(I)*10**(L-1)
          ADIG=ABS(ATEMP) - INT(ABS(ATEMP))
          IDIG2=INT(ADIG*10)
          IF(IDIG.NE.IDIG2)GOTO209
  300   CONTINUE
        NDIGI=NDIGI+1
        DIGITS(NDIGI)=IDIG
  200 CONTINUE
  209 CONTINUE
C
      IF(NDIGI.GT.0)THEN
        XDIGI=REAL(INT(X(1)))*(10**NDIGI)
        DO400I=1,NDIGI
          ATEMP=DIGITS(I)*(10**(NDIGI-I))
          XDIGI=XDIGI + ATEMP
  400   CONTINUE
        XDIGI=XDIGI/(10**NDIGI)
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
  800 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,NDIGI
  811 FORMAT('THE NUMBER OF COMMON DIGITS FOR THE ',I8,
     1       ' OBSERVATIONS = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,813)XDIGI
  813 FORMAT('THE COMMON DIGITS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF SUM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NDIGI,XDIGI
 9015 FORMAT('NDIGI,XDIGI = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMOVE(X,Y,N,IWRITE,XYCOMO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE (LEIGH-PERLMAN) COMOVEMENT COEFFICIENT
C              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
C              THE SAMPLE COMOVEMENT COEFFICIENT WILL BE A SINGLE
C              PRECISION VALUE CALCULATED AS THE
C              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE FIRST SET
C                                OF DATA.
C                     --Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE SECOND SET
C                                OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X, OR EQUIVALENTLY,
C                                THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--XYCOMO = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE COMOVEMENT COEFFICIENT
C                                BETWEEN THE 2 SETS OF DATA
C                                IN THE INPUT VECTORS X AND Y.
C                                THIS SINGLE PRECISION VALUE
C                                WILL BE BETWEEN -1.0 AND 1.0
C                                (INCLUSIVELY).
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE COMOVEMENT COEFFICIENT BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--AN INDEX FOR COMOVEMENT OF TIME SEQUENCES
C                 WITH GEOPHYSICAL APPLICATIONS:  A WORKING PAPER
C                 (PENN STATE INTERFACE CONFERANCE ON ASTRONOMY
C                 AUGUST 11-14, 1991)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--92/8
C     ORIGINAL VERSION--AUGUST    1991.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DXIM1
      DOUBLE PRECISION DYI
      DOUBLE PRECISION DYIM1
      DOUBLE PRECISION DDELX
      DOUBLE PRECISION DDELY
      DOUBLE PRECISION DSUMX
      DOUBLE PRECISION DSUMY
      DOUBLE PRECISION DSUMXY
      DOUBLE PRECISION DSQRTX
      DOUBLE PRECISION DSQRTY
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMO'
      ISUBN2='VE  '
C
      IERROR='NO'
C
      DN=0.0D0
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMXY=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COMOVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I),Y(I)
   56 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *******************************************
C               **  COMPUTE     COMOVEMENT COEFFICIENT  **
C               *******************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.GE.2)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN COMOVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE COMOVEMENT COEFFICIENT IS TO BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      COMPUTED, MUST BE 2 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      IF(N.EQ.2)GOTO120
      GOTO129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',
     1'THE THIRD INPUT ARGUMENT (N) HAS THE VALUE 2')
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
      GOTO9000
  129 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',
     1'THE FIRST  INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
      GOTO9000
  139 CONTINUE
C
      HOLD=Y(1)
      DO145I=2,N
      IF(Y(I).NE.HOLD)GOTO149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,146)HOLD
  146 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',
     1'THE SECOND INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
      GOTO9000
  149 CONTINUE
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE     COMOVEMENT COEFFICIENT.  **
C               ************************************************
C
      DN=N
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMXY=0.0D0
      DO300I=2,N
      IM1=I-1
      DXI=X(I)
      DXIM1=X(IM1)
      DDELX=DXI-DXIM1
      DYI=Y(I)
      DYIM1=Y(IM1)
      DDELY=DYI-DYIM1
      DSUMX=DSUMX+DDELX**2
      DSUMY=DSUMY+DDELY**2
      DSUMXY=DSUMXY+DDELX*DDELY
  300 CONTINUE
      DSQRTX=0.0
      IF(DSUMX.GT.0.0D0)DSQRTX=DSQRT(DSUMX)
      DSQRTY=0.0
      IF(DSUMY.GT.0.0D0)DSQRTY=DSQRT(DSUMY)
      XYCOMO=DSUMXY/(DSQRTX*DSQRTY)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,XYCOMO
  811 FORMAT('THE LEIGH-PERLMAN COMOVEMENT COEF. OF THE ',
     1I8,' OBSERV. = ',E15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DN,DSUMX,DSUMY,DSUMXY
 9014 FORMAT('DN,DSUMX,DSUMY,DSUMXY = ',4D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)XYCOMO
 9015 FORMAT('XYCOMO = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMPIC(IFUNC1,N1,IOLD,IOLD2,INEW,INEW2,NCHANG,
     1IFUNC2,N2,IBUGA3,IERROR)
C
C     PURPOSE--SCAN THE FUNCTIONAL EXPRESSION GIVEN IN IFUNC1
C              AND CHANGE ALL OCCURRANCES OF
C              PARAMETER, VARIABLE, FUNCTION, AND
C              NUMBERS GIVEN IN IOLD BY THE CORRESPONDING
C              STRINGS GIVEN IN INEW.
C     NOTE--IT IS ASSUMED THAT NAMES ARE
C           ALREADY IN THE FORM OF A4--THAT IS
C           INDIVIDUALLY PACKED PER WORD.
C     NOTE--NUMBERS MAY NOT BE CHANGED.
C     NOTE--PARAMETERS MAY BE CHANGED TO NUMBERS
C           BUT ONLY THE FIRST 8 CHARACTERS OF THE NUMBER WILL
C           BE TRANSFERRED.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFUNC1
      CHARACTER*4 IOLD
      CHARACTER*4 IOLD2
      CHARACTER*4 INEW
      CHARACTER*4 INEW2
      CHARACTER*4 IFUNC2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICH11
      CHARACTER*4 ICH12
      CHARACTER*4 ICH1
      CHARACTER*4 ICH21
      CHARACTER*4 ICH22
      CHARACTER*4 ICH2
      CHARACTER*4 ICASEP
      CHARACTER*4 ICASEA
      CHARACTER*4 IHALF1
      CHARACTER*4 IHALF2
C
C---------------------------------------------------------------------
C
      DIMENSION IFUNC1(*)
      DIMENSION IFUNC2(*)
      DIMENSION IOLD(*)
      DIMENSION IOLD2(*)
      DIMENSION INEW(*)
      DIMENSION INEW2(*)
C
      DIMENSION ICH11(10)
      DIMENSION ICH12(10)
      DIMENSION ICH1(20)
      DIMENSION ICH21(10)
      DIMENSION ICH22(10)
      DIMENSION ICH2(20)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMP'
      ISUBN2='IC  '
C
      IERROR='NO'
C
      NUMASC=4
      NUMAS2=2*NUMASC
C
      IEND1=0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COMPIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)N1,IBUGA3
   52 FORMAT('N1,IBUGA3 = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(IFUNC1(I),I=1,N1)
   53 FORMAT('IFUNC1(.)=',30A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NCHANG
   54 FORMAT('NCHANG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NCHANG
      WRITE(ICOUT,56)I,IOLD(I),IOLD2(I),INEW(I),INEW2(I)
   56 FORMAT('I,IOLD(I),IOLD2(I),INEW(I),INEW2(I) = ',
     1I8,2X,A4,A4,2X,A4,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  STEP 1--                              **
C               **  COPY THE INPUT FUNCTION IN IFUNC1(.)  **
C               **  INTO THE OUTPUT VECTOR IFUNC2(.).     **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LE.0)GOTO190
      DO80I=1,N1
      IFUNC2(I)=IFUNC1(I)
   80 CONTINUE
      N2=N1
C
C               *****************************************
C               **  STEP 2--                           **
C               **  LOOP THROUGH THE INPUT FUNCTION--  **
C               **  1 CHARACTER (USUALLY) AT A TIME.   **
C               *****************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I=0
  100 CONTINUE
      I=I+1
      IF(I.GT.N2)GOTO190
      IF(NCHANG.LE.0)GOTO190
C
C               ******************************************
C               **  STEP 3--                            **
C               **  FOR THIS CHARACTER (CHARACTER I),   **
C               **  SCAN THROUGH ALL POTENTIAL CHANGES  **
C               **  TO BE MADE.                         **
C               ******************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO200J=1,NCHANG
      CALL DPXH1H(IOLD(J),ICH11,IEND11,IBUGA3)
      CALL DPXH1H(IOLD2(J),ICH12,IEND12,IBUGA3)
      DO205K=1,NUMAS2
      ICH1(K)=' '
  205 CONTINUE
      L=0
      DO206K=1,NUMASC
      L=L+1
      ICH1(L)=ICH11(K)
  206 CONTINUE
      DO207K=1,NUMASC
      L=L+1
      ICH1(L)=ICH12(K)
  207 CONTINUE
      IEND1=0
      IF(IEND11.GE.1)IEND1=IEND11
      IF(IEND11.GE.NUMASC)IEND1=NUMASC
      IF(IEND12.GE.1)IEND1=NUMASC+IEND12
      IF(IEND12.GE.NUMAS2)IEND1=NUMAS2
C
      IF(IEND1.LE.0)GOTO200
C
C               *********************************************
C               **  STEP 4--                               **
C               **  CHECK FOR A LEFT OR RIGHT PARENTHESIS  **
C               **  IN THE INPUT CHANGE PATTERN.           **
C               *********************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEP='NO'
      DO210K=1,IEND1
      IF(ICH1(K).EQ.'(')GOTO220
      IF(ICH1(K).EQ.')')GOTO220
  210 CONTINUE
      ICASEP='NO'
      GOTO290
  220 CONTINUE
      ICASEP='YES'
  290 CONTINUE
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  STARTING WITH CHARACTER I OF THE INPUT FUNCTION,  **
C               **  COMPARE THE STRING IN THE INPUT FUNCTION          **
C               **  WITH THIS INPUT CHANGE PATTERN.                   **
C               **  DETERMINE IF THERE IS A MATCH.                    **
C               ********************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      L1=I-1
      DO300K=1,IEND1
      L1=L1+1
      IF(IFUNC2(L1).EQ.ICH1(K))GOTO300
      GOTO200
  300 CONTINUE
C
C               **********************************************
C               **  STEP 6--                                **
C               **  IF HAVE A MATCH,                        **
C               **  CHECK TO SEE IF THE STRING              **
C               **  IN THE FUNCTION                         **
C               **  IS PRECEDED BY A +, -, *, /, **, (,     **
C               **  (OR IS THE FIRST STRING ON THE LINE),   **
C               **  AND ALSO                                **
C               **  IS SUCCEDED BY A +, -, *, /, **, ),     **
C               **  (OR IS THE LAST  STRING ON THE LINE).   **
C               **  A FULFILLMENT OF ANY OF THE ABOVE       **
C               **  14 CONDITIONS WILL BE SUFFICIENT        **
C               **  TO ASSURE THAT INDIVIDUAL MIDDLE        **
C               **  CHARACTERS IN LIBRARY FUNCTIONS         **
C               **  (E.G., THE 'X' IN 'EXP')                **
C               **  AND IN MULTI-CHARACTER VARIABLE NAMES   **
C               **  (E.G., THE 'X' IN 'FLUX')               **
C               **  WILL NOT BE INADVERTANTLY CHANGED       **
C               **  (E.G., BY, SAY, 'FOR X = 3').           **
C               **********************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEA='NO'
      IHALF1='NO'
      IHALF2='NO'
C
      IM1=I-1
      IF(IM1.LE.0)GOTO410
      IF(IFUNC2(IM1).EQ.'+')GOTO410
      IF(IFUNC2(IM1).EQ.'-')GOTO410
      IF(IFUNC2(IM1).EQ.'*')GOTO410
      IF(IFUNC2(IM1).EQ.'/')GOTO410
      IF(IFUNC2(IM1).EQ.'**')GOTO410
      IF(IFUNC2(IM1).EQ.'(')GOTO410
      IHALF1='NO'
      GOTO419
  410 CONTINUE
      IHALF1='YES'
  419 CONTINUE
C
      L1P1=L1+1
      IF(L1P1.GT.N2)GOTO420
      IF(IFUNC2(L1P1).EQ.'+')GOTO420
      IF(IFUNC2(L1P1).EQ.'-')GOTO420
      IF(IFUNC2(L1P1).EQ.'*')GOTO420
      IF(IFUNC2(L1P1).EQ.'/')GOTO420
      IF(IFUNC2(L1P1).EQ.'**')GOTO420
      IF(IFUNC2(L1P1).EQ.')')GOTO420
      IHALF2='NO'
      GOTO429
  420 CONTINUE
      IHALF2='YES'
  429 CONTINUE
C
      ICASEA='NO'
      IF(IHALF1.EQ.'YES'.AND.IHALF2.EQ.'YES')ICASEA='YES'
C
  490 CONTINUE
C
C               ***********************************************************
C               **  STEP 7--                                             **
C               **  IF THE INPUT STRING HAD ANY PARENTHESES,             **
C               **     THEN CHANGE ANY MATCHING STRING IN THE FUNCTION.  **
C               **  IF THE INPUT STRING HAD NO PARENTHESES,              **
C               **     THEN CHANGE MATCHING STRINGS IN THE FUNCTION      **
C               **     ONLY WHEN THE MATCHING FUNCTION SUBSTRING         **
C               **     IS PRECEDED BY A +, -, *, /, **, (,               **
C               **     (OR IS THE FIRST STRING ON THE LINE), AND ALSO    **
C               **     IS SUCCEDED BY A +, -, *, /, **, ),               **
C               **     (OR IS THE LAST  STRING ON THE LINE).             **
C               ***********************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEP.EQ.'YES')GOTO590
      IF(ICASEP.EQ.'NO'.AND.ICASEA.EQ.'YES')GOTO590
      GOTO200
  590 CONTINUE
C
C               **************************************************
C               **  STEP 8--                                    **
C               **  IF CHANGES ARE TO BE MADE,                  **
C               **  EXTRACT THE OUTPUT CHANGE PATTERN           **
C               **  CORRESPONDING TO THE INPUT CHANGE PATTERN.  **
C               **************************************************
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPXH1H(INEW(J),ICH21,IEND21,IBUGA3)
      CALL DPXH1H(INEW2(J),ICH22,IEND22,IBUGA3)
      DO605K=1,NUMAS2
      ICH2(K)=' '
  605 CONTINUE
      L=0
      DO606K=1,NUMASC
      L=L+1
      ICH2(L)=ICH21(K)
  606 CONTINUE
      DO607K=1,NUMASC
      L=L+1
      ICH2(L)=ICH22(K)
  607 CONTINUE
      IEND2=0
      IF(IEND21.GE.1)IEND2=IEND21
      IF(IEND21.GE.NUMASC)IEND2=NUMASC
      IF(IEND22.GE.1)IEND2=NUMASC+IEND21
      IF(IEND22.GE.NUMAS2)IEND2=NUMAS2
C
      IF(IEND2.LE.0)GOTO200
C
C               ******************************
C               **  STEP 9--                **
C               **  CARRY OUT THE CHANGES   **
C               **  IN THE INPUT FUNCTION.  **
C               ******************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTAR1=I
      ISTOP1=ISTAR1+IEND1-1
      ISTAR2=1
      ISTOP2=ISTAR2+IEND2-1
      CALL DPSIRS(IFUNC2,N2,ISTAR1,ISTOP1,ICH2,IEND2,ISTAR2,ISTOP2,
     1IBUGA3,IERROR)
      I=ISTOP1+(IEND2-IEND1)
      GOTO100
C
  200 CONTINUE
      GOTO100
C
  190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COMPIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)N1,IBUGA3
 9012 FORMAT('N1,IBUGA3 = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)(IFUNC1(I),I=1,N1)
 9013 FORMAT('IFUNC1(.)=',30A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NCHANG
 9014 FORMAT('NCHANG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NCHANG
      WRITE(ICOUT,9016)I,IOLD(I),IOLD2(I),INEW(I),INEW2(I)
 9016 FORMAT('I,IOLD(I),IOLD2(I),INEW(I),INEW2(I) = ',
     1I8,2X,A4,A4,2X,A4,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9017)N2
 9017 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)(IFUNC2(I),I=1,N2)
 9018 FORMAT('IFUNC2(.)=',30A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)NUMASC,NUMAS2
 9019 FORMAT('NUMASC,NUMAS2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)IEND11,IEND12,IEND1,IEND21,IEND22,IEND2
 9020 FORMAT('IEND11,IEND12,IEND1,IEND21,IEND22,IEND2 = ',6I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)(ICH11(I),I=1,10)
 9021 FORMAT('(ICH11(I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)(ICH12(I),I=1,10)
 9022 FORMAT('(ICH12(I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)(ICH1 (I),I=1,10)
 9023 FORMAT('(ICH1 (I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9024)(ICH21(I),I=1,10)
 9024 FORMAT('(ICH21(I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)(ICH22(I),I=1,10)
 9025 FORMAT('(ICH22(I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9026)(ICH2 (I),I=1,10)
 9026 FORMAT('(ICH2 (I),I=1,10) = ',10A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMPID(IA,NUMCHA,IPASS,PARAM,IPARN1,IPARN2,NUMPAR,
     1IVARN1,IVARN2,NUMVAR,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,ID,NUMCHD,
     1IBUGCO,IBUGEV,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE DETERMINES THE DERIVATIVE OF
C              A FORTRAN MATHEMATICAL FUNCTION EXPRESSION.
C     NOTE--TYPICALLY THIS SUBROUTINE IS CALLED ONLY
C           WITH IPASS=2; IN SUCH CASE,
C           IPARN1(.) AND NUMPAR ARE NEVER DETERMINED,
C           NEEDED, OR OUTPUTTED.
C           (NOTE--THERE EXISTS POSSIBLE DIFFERENCES WITH NUMPAR
C           AS DEFINED FOR THIS SUBROUTINE
C           AS OPPOSED TO THE DEFINITION FOR COMPID).
C     INPUT  ARGUMENTS--IA     = THE HOLLARITH VECTOR WHICH CONTAINS
C                                THE FUNCTION OF INTEREST
C                                FOR WHICH THE ANALYTIC DERIVATIVE
C                                IS TO BE DETERMINED.
C                                IA(.) MAY BE EITHER UNPACKED (1 CHARACTER PER W
C                                OR PACKED (4 CHARACTERS PER WORD)
C                                ALTHOUGH THE USUAL REPRESENTATION IS UNPACKED.
C                     --NUMCHA = THE INTEGER VALUE WHICH
C                                DEFINES THE NUMBER OF CHARACTERS IN IA.
C                                NUMCHA DEFINES THE LENGTH OF THE
C                                HOLLARITH STRING TO BE OPERATED ON.
C                     --IPASS  = AN INTEGER FLAG CODE
C                                WHICH DEFINES WHICH PASS (1 OR 2) INTO THIS
C                                SUBROUTINE THE USER IS IN.
C                                PASS 1 DETERMINE PARAMETER NAMES;
C                                PASS 2 DOES FUNCTION EVALUATIONS.
C                     --PARAM  = THE SINGLE PRECISION VECTOR OF PARAMETER
C                                (AND VARIABLE)
C                                VALUES CORRESPONDING TO THE PARAMETER NAMES
C                                AS GIVEN IN THE VECTOR IPARN1.
C                     --IPARN1 = THE INTEGER VECTOR OF PARAMETER
C                                (AND VARIABLE)
C                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
C     OUTPUT ARGUMENTS--ID     = THE HOLLARITH VECTOR WHICH CONTAINS
C                                THE DESIRED DERIVATIVE FUNCTION.
C                                ID(.) IS UNPACKED (THAT IS,
C                                1 CHARACTER PER WORD).
C                     --NUMCHD = THE INTEGER VALUE WHICH
C                                DEFINES THE NUMBER OF CHARACTERS IN ID.
C                                NUMCHD DEFINES THE LENGTH OF THE
C                                HOLLARITH STRING FOR THE DERIVATIVE FUNCTION.
C     OUTPUT--THE SINGLE PRECISION COMPUTED SCALAR VALUE,
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER           SUBROUTINES NEEDED--EVAL
C     FORTRAN LIBRARY SUBROUTINES NEEDED--(ALL IN EVAL)
C                                         SQRT
C                                         EXP
C                                         LOG
C                                         LOG10
C                                         SIN
C                                         COS
C                                         ATAN
C                                         ATAN2
C                                         TANH
C                                         ABS
C                                         AINT
C                                         ARCSIN
C                                         ARCCOS
C                                         ARCTAN
C                                         OCTAL
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     NOTE--THIS SUBROUTINE ALLOWS ONE TO PERFORM
C           INTERACTIVE FUNCTION EVALUATIONS.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--DECEMBER  1978.
C     UPDATED         --JANUARY   1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --APRIL     1986.
C
      CHARACTER*4 IA
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IR
      CHARACTER*4 IB
      CHARACTER*4 IL
      CHARACTER*4 ICH
      CHARACTER*4 IW21
      CHARACTER*4 IW22
      CHARACTER*4 ITYPE
      CHARACTER*4 IANS1
      CHARACTER*4 IANS2
      CHARACTER*4 IANS3
      CHARACTER*4 IANS4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
CCCCC CHARACTER*4 IBUG0
CCCCC CHARACTER*4 IBUG1
CCCCC CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
CCCCC CHARACTER*4 IBUG4
CCCCC CHARACTER*4 IBUG5
CCCCC CHARACTER*4 IBUG6
CCCCC CHARACTER*4 IBUG7
CCCCC CHARACTER*4 IBUGXH
CCCCC CHARACTER*4 IBUGCD
C
      CHARACTER*4 ID
      CHARACTER*4 ID1
      CHARACTER*4 ID2
      CHARACTER*4 ID3
      CHARACTER*4 ICON
C
C---------------------------------------------------------------------
C
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
C
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
C
      DIMENSION ID(*)
C
C     NOTE--THE DIMENSIONS OF ITYPEH, IW21HO, IW22HO, AND W2HOLD
C           WHICH ARE DEFINED IN THE MAIN PROGRAM
C           SHOULD BE AT LEAST AS LARGE AS THE DIMENSIONS
C           OF IW21 AND IW22 BELOW.
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
CCCCC DIMENSION IB(225)
CCCCC DIMENSION IR(225)
CCCCC DIMENSION IBEGIN(225)
CCCCC DIMENSION IEND(225)
CCCCC DIMENSION ITYPE(225)
CCCCC DIMENSION IW21(225)
CCCCC DIMENSION IW22(225)
CCCCC DIMENSION W2(225)
      DIMENSION IB(1000)
      DIMENSION IR(1000)
      DIMENSION IBEGIN(1000)
      DIMENSION IEND(1000)
      DIMENSION ITYPE(1000)
      DIMENSION IW21(1000)
      DIMENSION IW22(1000)
      DIMENSION W2(1000)
C
      DIMENSION ID1(250)
      DIMENSION ID2(250)
      DIMENSION ID3(250)
C
      DIMENSION ICH(10)
C
      DIMENSION IL(10)
C
      DIMENSION ICON(1000)
      DIMENSION ICON1(50)
      DIMENSION ICON2(50)
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-----------------------------------------------------
C
CCCCC DATA IBUG0/'OFF'/
CCCCC DATA IBUG1/'OFF'/
CCCCC DATA IBUG2/'OFF'/
CCCCC DATA IBUG3/'OFF'/
CCCCC DATA IBUG4/'OFF'/
CCCCC DATA IBUG5/'OFF'/
CCCCC DATA IBUG6/'OFF'/
CCCCC DATA IBUG7/'OFF'/
CCCCC DATA IBUGXH/'OFF'/
CCCCC DATA IBUGCD/'OFF'/
C
C     DEFINE THE UPPER LIMIT OF THE NUMBER OF CHARACTERS
C     THAT MAY BE PROCESSED BY THIS SUBROUTINE
C     (COUNTING BLANKS, LEFT-HAND SIDE, EQUAL SIGN,
C     AND RIGHT HAND SIDE).
C     IF RESTRICT THE EXPRESSION TO 1 LINE IMAGE,
C     THEN A REASONABLE UPPER BOUND IS 80.
C     WHATEVER UPPER BOUND IS SET,
C     THE DIMENSIONS OF MOST OF THE VECTORS
C     MUST BE EQUAL OR LARGER TO THIS NUMBER.
C     (THE VECTOR IL(.) WHICH CONTAINS THE
C     NUMBER OF CHARACTERS TO THE LEFT
C     OF THE EQUAL SIGN (BLANKS IGNORED)
C     MAY BE MUCH SMALLER--LIKE 6.)
C     NOTE--AS OF JANUARY 1979, THE BOUND WAS RESET TO 150.
C
CCCCC DATA MAXCHA/150/
CCCCC DATA MAXCHA/225/
      DATA MAXCHA/1000/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMP'
      ISUBN2='ID  '
C
      IERROR='NO  '
C
C     THE FOLLOWING STATEMENT (N=1) HAS BEEN ADDED
C     IN CONVERTING THE COMPIL SUBROUTINE
C     TO THE COMPID SUBROUTINE.
C
      N=1
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NUMCHA,N,IPASS,IANGLU
   52 FORMAT('NUMCHA,N,IPASS,IANGLU = ',3I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(IA(I),I=1,NUMCHA)
   53 FORMAT('IA--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IBUGCO,IBUGEV
   54 FORMAT('IBUGCO,IBUGEV = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)NUMPAR
   61 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMPAR.LE.0)GOTO64
      DO62I=1,NUMPAR
      WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I),PARAM(I)
   63 FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2X,A4,2X,A4,2X,
     1E15.7)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   64 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)NUMVAR
   65 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMVAR.LE.0)GOTO69
      DO66I=1,NUMVAR
      WRITE(ICOUT,67)I,IVARN1(I),IVARN2(I)
   67 FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   66 CONTINUE
   69 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NWHOLD
   71 FORMAT('NWHOLD = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NWHOLD.LE.0)GOTO79
      DO72I=1,NWHOLD
      WRITE(ICOUT,73)I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I)
   73 FORMAT('I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   79 CONTINUE
   90 CONTINUE
C
C               ************************************************************
C               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.  **
C               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND         **
C               **  REGARDLESS OF THE WORD SIZE.                          **
C               ************************************************************
C
      NUMASC=4
      NUMAS2=2*NUMASC
      NUMAS3=3*NUMASC
      NUMAS4=4*NUMASC
C
C     CHECK THAT THE INPUT NUMBER OF CHARACTERS NUMCHA
C     (INCLUDING LEFT SIDE, RIGHT SIDE, EQUAL SIGN,
C     AND BLANKS) IS AT LEAST 1 AND AT MOST MAXCHA
C     (WHERE MAXCHA IS THE INTERNALLY DEFINED VARIABLE
C     WHICH CONTROLS DIMENSION SIZES AND WHICH
C     TYPICALLY HAS THE VALUE 80).
C
      IF(1.LE.NUMCHA.AND.NUMCHA.LE.MAXCHA)GOTO139
      WRITE(ICOUT,121)
  121 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)
  122 FORMAT('      THE NUMBER OF CHARACTERS NUMCHA ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,123)
  123 FORMAT('      WHICH DEFINES THE LENGTH OF THE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,124)
  124 FORMAT('      INPUT EXPRESSION (INCLUDING LEFT-HAND SIDE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,125)
  125 FORMAT('      RIGHT-HAND SIDE, EQUAL SIGN, AND ALL BLANKS)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,126)
  126 FORMAT('      IS SMALLER THAN 1 OR LARGER THAN MAXCHA')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,127)
  127 FORMAT('      (MAXCHA IS AN INTERNALLY-DEFINED VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,128)MAXCHA
  128 FORMAT('      WHICH HAS THE VALUE = ',I8,'   .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,129)
  129 FORMAT('      THE NUMBER OF CHARACTERS IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,130)NUMCHA
  130 FORMAT('      INPUT EXPRESSION IS ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCHA.GE.1)WRITE(ICOUT,131)(IA(I),I=1,NUMCHA)
  131 FORMAT('      INPUT EXPRESSION--',100A1)
      IF(NUMCHA.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
  139 CONTINUE
C
C     BLANK-OUT AND ZERO-OUT SOME VARIABLES AND VECTORS.
C
CCCCC Y=0.0
      IC2=0
C
      DO160I=1,NUMCHA
      IR(I)='    '
      IB(I)='    '
      IW21(I)='    '
      IW22(I)='    '
      W2(I)=0.0
      ITYPE(I)='    '
      IW21HO(I)='    '
      IW22HO(I)='    '
      W2HOLD(I)=0.0
      ITYPEH(I)='    '
      ID1(I)='    '
      ID2(I)='    '
      ID3(I)='    '
      ID(I)='    '
  160 CONTINUE
C
C
C               ***********************************************
C               **  STEP 1--                                 **
C               **  OPERATE ON THE VECTOR IA(.).             **
C               **  IA(.) MAY BE OPTIONALLY EITHER UNPACKED  **
C               **  (1 CHARACTER PER WORD),                  **
C               **  OR PACKED                                **
C               **  (UP TO 4 CHARACTERS PER WORD).           **
C               **  IN ANY EVENT, IB(.) IS UNPACKED.         **
C               **  NOTE ALSO THAT IB(.) HAS BLANKS OMITTED. **
C               ***********************************************
C
      ISTEPN='1'
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      K=0
      DO200I=1,NUMCHA
      IF(IA(I).EQ.'    ')GOTO200
      CALL DPXH1H(IA(I),ICH,ILASTC,IBUGEV)
      IF(ILASTC.LE.0)GOTO200
      DO250J=1,ILASTC
      K=K+1
      IB(K)=ICH(J)
  250 CONTINUE
  200 CONTINUE
      NCTOT=K
C
      IF(NCTOT.GE.1)GOTO290
      WRITE(ICOUT,205)NCTOT
  205 FORMAT('***** ERROR IN COMPID--TOTAL NUMBER OF CHARACTERS ',
     1'IN MODEL (INCL. BOTH SIDES, BLANKS, AND EQUAL SIGN) ',
     1'IS < 2.  NCTOT = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,271)NUMCHA,N,IPASS
  271 FORMAT('NUMCHA,N,IPASS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,272)(IA(I),I=1,NUMCHA)
  272 FORMAT('IA--',80A1)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,281)NUMPAR
  281 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMPAR.LE.0)GOTO289
      DO282I=1,NUMPAR
      WRITE(ICOUT,283)I,IPARN1(I),IPARN2(I),PARAM(I)
  283 FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2X,A4,2X,A4,2X,
     1E15.7)
      CALL DPWRST('XXX','BUG ')
  282 CONTINUE
  289 CONTINUE
      IERROR='YES '
      GOTO9000
C
  290 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO299
      WRITE(ICOUT,291)NCTOT
  291 FORMAT('NCTOT = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO292I=1,NCTOT
      WRITE(ICOUT,293)I,IB(I)
  293 FORMAT('I,IB(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  292 CONTINUE
  299 CONTINUE
C
C               **************************************************************
C               **  STEP 2--                                                **
C               **  OPERATE ON THE VECTOR IB(.).                            **
C               **  DETERMINE THE NUMBER OF CHARACTERS (IF ANY)             **
C               **  FOR THE LEFT-HAND SIDE.  OUTPUT THEM INTO THE           **
C               **  VECTOR IL(.).                                           **
C               **************************************************************
C
      DO500I=1,NCTOT
      I2=I
      IF(IB(I).EQ.'=   ')GOTO550
  500 CONTINUE
      NCL=0
      ISTARR=1
      GOTO559
  550 CONTINUE
      NCL=I2-1
      ISTARR=I2+1
  559 CONTINUE
C
      IF(NCL.LE.0)GOTO699
      DO600I=1,NCL
      IL(I)=IB(I)
  600 CONTINUE
  690 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO699
      ISTEPN='2'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,691)NCL
  691 FORMAT('NCL = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO692I=1,NCL
      WRITE(ICOUT,693)I,IL(I)
  693 FORMAT('I,IL(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  692 CONTINUE
  699 CONTINUE
C
C               ***************************************************************
C               **  STEP 3--                                                 **
C               **  OPERATE ON THE VECTOR IB(.).                             **
C               **  DETERMINE THE NUMBER OF CHARACTERS FOR RIGHT-HAND SIDE.  **
C               **  OUTPUT THEM INTO THE VECTOR IR(.).                       **
C               ***************************************************************
C
      IF(ISTARR.LE.NCTOT)GOTO719
      WRITE(ICOUT,701)
  701 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,702)
  702 FORMAT('      THE NUMBER OF CHARACTERS ON THE RIGHT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,703)
  703 FORMAT('      (WITH BLANKS IGNORED) IS 0.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,704)
  704 FORMAT('      THE TOTAL NUMBER OF PACKED CHARACTERS   NCTOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,705)
  705 FORMAT('      LEFT (IF ANY), EQUAL SIGN (IF ANY), AND RIGHT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,706)NCTOT
  706 FORMAT('      = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,707)
  707 FORMAT('      THE START POSITION FOR THE PACKED RIGHT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,708)ISTARR
  708 FORMAT('      IS COLUMN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,709)NUMCHA
  709 FORMAT('      THE INPUT NUMBER OF CHARACTERS NUMCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCHA.GE.1)WRITE(ICOUT,710)(IA(I),I=1,NUMCHA)
  710 FORMAT('      INPUT EXPRESSION--',100A1)
      IF(NUMCHA.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
  719 CONTINUE
C
      K=0
      DO700I=ISTARR,NCTOT
      K=K+1
      IR(K)=IB(I)
  700 CONTINUE
      NCR=K
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO799
      ISTEPN='3'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,791)NCR
  791 FORMAT('NCR = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO792I=1,NCR
      WRITE(ICOUT,793)I,IR(I)
  793 FORMAT('I,IR(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  792 CONTINUE
  799 CONTINUE
C
C               ****************************************************************
C               **  STEP 4--
C               **  OPERATE ON THE VECTOR IR(.).
C               **  SIMPLIFY THE RIGHT-HAND SIDE.
C               **  ANALYZE THE RIGHT-HAND SIDE.
C               **  DETERMINE THE NUMBER OF DIFFERENT LOGICAL COMPONENTS.
C               **        1. NUMBER (CONSISTING OF 0,1,2,...,9 OR .)
C               **        2. X VARIABLE
C               **        3. OPERATION (+   -   *   /   **)
C               **        4. PARENTHESES (   (   OR   )    )
C               **        5. LIBRARY FUNCTION (ALOG   EXP   ETC + AUGMENTED LIB.
C               **        6. COMMA (FOR MULTI-ARGUMENT LIBRARY FUNCTIONS)
C               **        7. PARAMETER (ANYTHING NOT ABOVE)
C               **  CHECK FOR SYNTAX ERRORS.
C               **  OUTPUT THE TYPE COMPONENT INTO ITYPE(.).
C               **  OUTPUT THE START LOCATION IN IR(.) OF EACH COMPONENT INTO IB
C               **  OUTPUT THE STOP  LOCATION IN IR(.) OF EACH COMPONENT INTO IE
C               ****************************************************************
C
      CALL DPSIPA(IR,NCR,IBUGEV,IERROR)
      CALL DPSISI(IR,NCR,IBUGEV,IERROR)
      CALL DPSIP1(IR,NCR,IBUGEV,IERROR)
      CALL DPSIP0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIE1(IR,NCR,IBUGEV,IERROR)
      CALL DPSIE0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIA0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIA2(IR,NCR,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(IR,NCR,IBUGEV,IERROR)
C
      NW=0
      I=1
      NCON=0
 1050 CONTINUE
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
C
      IF(IR(I).EQ.'0   ')GOTO1100
      IF(IR(I).EQ.'1   ')GOTO1100
      IF(IR(I).EQ.'2   ')GOTO1100
      IF(IR(I).EQ.'3   ')GOTO1100
      IF(IR(I).EQ.'4   ')GOTO1100
      IF(IR(I).EQ.'5   ')GOTO1100
      IF(IR(I).EQ.'6   ')GOTO1100
      IF(IR(I).EQ.'7   ')GOTO1100
      IF(IR(I).EQ.'8   ')GOTO1100
      IF(IR(I).EQ.'9   ')GOTO1100
      IF(IR(I).EQ.'.   ')GOTO1100
C
C     NOTE--THE FOLLOWING LINE IS BEING COMMENTED OUT
C     SO AS TO GENERALIZE COMPIL INTO COMPID
C     (1 VARIABLE INTO MANY VARIABLES).
C
CCCCC IF(IR(I).EQ.'X   ')GOTO1200
C
      IF(IR(I).EQ.'+   ')GOTO1300
      IF(IR(I).EQ.'-   ')GOTO1300
      IF(IR(I).EQ.'*   ')GOTO1300
      IF(IR(I).EQ.'/   ')GOTO1300
C
      IF(IR(I).EQ.'(   ')GOTO1410
      IF(IR(I).EQ.')   ')GOTO1420
C
      IF(IR(I).EQ.',   ')GOTO1700
C
C     CHECK FOR A LIBRARY FUNCTION.
C
CCCCC CALL CKLIBF(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      CALL CKLIB1(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'NO')CALL CKLIB2(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO1069
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1061)
 1061 FORMAT('***** IN COMPID, AFTER RETURNING FROM CKLIBF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1062)NCR,I
 1062 FORMAT('NCR,I = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO1063I4=1,NCR
      WRITE(ICOUT,1064)I4,IR(I4)
 1064 FORMAT('I4,IR(I4) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1063 CONTINUE
      WRITE(ICOUT,1065)IFOUND,NCLF,IERROR
 1065 FORMAT('IFOUND,NCLF,IERROR = ',A4,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1069 CONTINUE
C
      IF(IERROR.EQ.'YES ')GOTO9000
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.8)GOTO1580
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.7)GOTO1570
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.6)GOTO1560
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.5)GOTO1550
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.4)GOTO1540
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.3)GOTO1530
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.2)GOTO1520
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.1)GOTO1510
C
      GOTO1600
C
 1100 CONTINUE
      NCON=NCON+1
      ICON1(NCON)=IC2+1
      IC=0
      NW=NW+1
      ITYPE(NW)='N   '
      JMIN=I
      J=I
 1150 CONTINUE
      IC=IC+1
      IC2=IC2+1
      ICON(IC2)=IR(J)
      J=J+1
      IF(J.GT.NCR)GOTO1160
      IF(IR(J).EQ.'0   ')GOTO1150
      IF(IR(J).EQ.'1   ')GOTO1150
      IF(IR(J).EQ.'2   ')GOTO1150
      IF(IR(J).EQ.'3   ')GOTO1150
      IF(IR(J).EQ.'4   ')GOTO1150
      IF(IR(J).EQ.'5   ')GOTO1150
      IF(IR(J).EQ.'6   ')GOTO1150
      IF(IR(J).EQ.'7   ')GOTO1150
      IF(IR(J).EQ.'8   ')GOTO1150
      IF(IR(J).EQ.'9   ')GOTO1150
      IF(IR(J).EQ.'.   ')GOTO1150
 1160 CONTINUE
      ICON2(NCON)=IC2
      JMAX=J-1
      GOTO1800
C
C1200 CONTINUE
CCCCC NW=NW+1
CCCCC NLPWP=0
CCCCC NRPWP=0
CCCCC JMIN=I
CCCCC J=I
CCCCC ILOOP=0
 1250 CONTINUE
      J=J+1
      IF(J.GT.NCR)GOTO1260
      IF(IR(J).EQ.'+   ')GOTO1260
      IF(IR(J).EQ.'-   ')GOTO1260
      IF(IR(J).EQ.'*   ')GOTO1260
      IF(IR(J).EQ.'/   ')GOTO1260
      IF(IR(J).EQ.'(   ')NLPWP=NLPWP+1
      IF(IR(J).EQ.')   ')NRPWP=NRPWP+1
      IF(IR(J).EQ.')   '.AND.NRPWP.GT.NLPWP)GOTO1260
      ILOOP=ILOOP+1
      IF(ILOOP.LE.NUMAS2)GOTO1250
      WRITE(ICOUT,1256)NUMAS2
 1256 FORMAT('***** ERROR IN COMPID--PARAMETER NAME EXCEEDS ',I8,
     1'CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      DO1257K=JMIN,J
      WRITE(ICOUT,1258)K,IR(K)
 1258 FORMAT('K,IR(K) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1257 CONTINUE
      IERROR='YES '
      GOTO9000
 1260 CONTINUE
      JMAX=J-1
C     THE FOLLOWING STATEMENT HAS BEEN
C     COMMENTED OUT IN GOING FROM THE
C     COMPIL SUBROUTINE TO THE COMPID
C     SUBROUTINE SO THAT X WILL NOT
C     BE TREATED AS A SPECIAL VARIABLE.
CCCCC IF(JMAX.EQ.JMIN)ITYPE(NW)='X   '
      IF(JMAX.GT.JMIN)ITYPE(NW)='PAR '
      GOTO1800
C
 1300 CONTINUE
      NW=NW+1
      ITYPE(NW)='OP  '
      JMIN=I
      JMAX=I
      IP1=I+1
      IF(IR(I).EQ.'*   '.AND.IR(IP1).EQ.'*   ')JMAX=IP1
      GOTO1800
C
 1410 CONTINUE
      NW=NW+1
      ITYPE(NW)='LP  '
      JMIN=I
      JMAX=I
      GOTO1800
 1420 CONTINUE
      NW=NW+1
      ITYPE(NW)='RP  '
      JMIN=I
      JMAX=I
      GOTO1800
C
 1510 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I
      GOTO1800
C
 1520 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+1
      GOTO1800
C
 1530 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+2
      GOTO1800
C
 1540 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+3
      GOTO1800
C
 1550 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+4
      GOTO1800
C
 1560 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+5
      GOTO1800
C
 1570 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+6
      GOTO1800
C
 1580 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+7
      GOTO1800
C
 1700 CONTINUE
      NW=NW+1
      ITYPE(NW)='COM '
      JMIN=I
      JMAX=I
      GOTO1800
C
 1600 CONTINUE
      NW=NW+1
      ITYPE(NW)='PAR '
      NLPWP=0
      NRPWP=0
      JMIN=I
      J=I
      ILOOP=0
C
 1650 CONTINUE
      J=J+1
      IF(J.GT.NCR)GOTO1660
      IF(IR(J).EQ.'+   ')GOTO1660
      IF(IR(J).EQ.'-   ')GOTO1660
      IF(IR(J).EQ.'*   ')GOTO1660
      IF(IR(J).EQ.'/   ')GOTO1660
      IF(IR(J).EQ.'(   ')NLPWP=NLPWP+1
      IF(IR(J).EQ.')   ')NRPWP=NRPWP+1
      IF(IR(J).EQ.')   '.AND.NRPWP.GT.NLPWP)GOTO1660
      IF(IR(J).EQ.',   ')GOTO1660
      ILOOP=ILOOP+1
      IF(ILOOP.LE.NUMAS2)GOTO1650
      WRITE(ICOUT,1656)NUMAS2
 1656 FORMAT('***** ERROR IN COMPID--PARAMETER NAME EXCEEDS ',I8,
     1'CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      DO1657K=JMIN,J
      WRITE(ICOUT,1658)K,IR(K)
 1658 FORMAT('K,IR(K) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1657 CONTINUE
      IERROR='YES '
      GOTO9000
 1660 CONTINUE
      JMAX=J-1
      GOTO1800
C
 1800 CONTINUE
C
C     CHECK THAT NW HAS NOT EXCEEDED MAXCHA (USUALLY 80)
C
      IF(NW.LE.MAXCHA)GOTO1900
      WRITE(ICOUT,1901)
 1901 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1902)
 1902 FORMAT('      THE VARIABLE NW HAS JUST EXCEEDED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1903)
 1903 FORMAT('      THE MAX ALLOWABLE LIMIT DEFINED ',
     1'BY THE INTERNAL VARIABLE MAXCHA.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1904)MAXCHA
 1904 FORMAT('      THIS LIMIT IS MAXCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1905)NUMCHA
 1905 FORMAT('      THE INPUT NUMBER OF CHARACTERS NUMCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCHA.GE.1)WRITE(ICOUT,1906)(IA(I),I=1,NUMCHA)
 1906 FORMAT('      INPUT EXPRESSION--',100A1)
      IF(NUMCHA.GE.1)CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1907)
 1907 FORMAT('      THE NUMBER OF (PACKED) CHARACTERS ON ',
     1'RIGHT-HAND SIDE NCR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCR.GE.1)WRITE(ICOUT,1908)(IR(I),I=1,NCR)
 1908 FORMAT('      (PACKED) RIGHT-HAND SIDE--',95A1)
      IF(NCR.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
 1900 CONTINUE
C
      IBEGIN(NW)=JMIN
      IEND(NW)=JMAX
      I=JMAX
C
      I=I+1
      IF(I.LE.NCR)GOTO1050
 1950 CONTINUE
C
C     TEST THAT NW IS POSITIVE.
C
      IF(NW.GE.1)GOTO1959
      WRITE(ICOUT,1951)NW
 1951 FORMAT('***** ERROR IN COMPID--NW IS NON-POSITIVE. ',
     1'NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
 1959 CONTINUE
C
      IF(NW.EQ.1)GOTO1969
      DO1960I=1,NW
      IP1=I+1
      IF(ITYPE(I).EQ.'LF  '.AND.ITYPE(IP1).NE.'LP  ')GOTO1961
      GOTO1960
 1961 CONTINUE
      WRITE(ICOUT,1962)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1963)NW
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1964)I
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1965)ITYPE(I)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1966)ITYPE(IP1)
 1962 FORMAT('***** ERROR IN COMPID--LIBRARY FUNCTION ',
     1'NOT FOLLOWED BY A LEFT PARENTHESES')
      CALL DPWRST('XXX','BUG ')
 1963 FORMAT('             NW = ',I8)
 1964 FORMAT('             I  = ',I8)
 1965 FORMAT('             ITYPE(I) = ',A4)
 1966 FORMAT('             ITYPE(I+1) = ',A4)
      IERROR='YES '
      GOTO9000
 1960 CONTINUE
 1969 CONTINUE
C
      IF(ITYPE(NW).EQ.'OP  ')GOTO1970
      IF(ITYPE(NW).EQ.'LF  ')GOTO1972
      GOTO1979
C
 1970 CONTINUE
      WRITE(ICOUT,1971)ITYPE(NW)
 1971 FORMAT('***** ERROR IN COMPID--LAST TERM IN TOTAL ',
     1' EXPRESSION IS AN OPERATION = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
 1972 CONTINUE
      WRITE(ICOUT,1973)ITYPE(NW)
 1973 FORMAT('***** ERROR IN COMPID--LAST TERM IN TOTAL ',
     1' EXPRESSION = A LIBRARY FUNCTION = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
 1979 CONTINUE
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO1999
      ISTEPN='4'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,1991)NW,ICMIN
 1991 FORMAT('NW,ICMIN = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO1992I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,1993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),
     1IBEGIN(I),IEND(I)
 1993 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',
     1'IBEGIN(I),IEND(I) = ',I8,2X,3A4,A4,2X,I8,2X,I8)
      CALL DPWRST('XXX','BUG ')
 1992 CONTINUE
 1999 CONTINUE
C
C               ****************************************************************
C               **  STEP 5--
C               **  OPERATE ON EACH COMPONENT OF THE VECTOR IR(.).
C               **  CONVERT THE NUMBERS TO FLOATING POINT VALUES.
C               **  CONVERT THE PARAMETERS TO FLOATING POINT VALUES.
C      *  SET THE X TO A DUMMY VALUE OF 0.0 FOR THE TIME BEING.       **
C               **  CONVERT THE OPERATIONS INTO A 1-WORD REPRESENTATION.
C               **  'CONVERT' THE PARENTHESES INTO A 1-WORD REPRESENTATION.
C               **  CONVERT THE COEFFICIENTS TO COEFFICIENT VALUES.
C               **  CONVERT THE LIBRARY FUNCTIONS INTO A 1-WORD REPRESENTATION.
C               **  SAVE THE CONTENTS OF ITYPE, IW2, AND W2 IN
C               **  ITYPEH, IW21HO, AND WHOLD FOR LATER USE
C               **  IN REDEFINING ITYPE, IW2, AND W2 FOR EACH NEW X VALUE.
C               **  OUTPUT THE VECTORS IW2 AND W2.
C               **  OUTPUT THE VECTORS IW21HO, W2HOLD, AND ITYPEH.
C               ****************************************************************
C
CCCCC IC=0 APRIL 29, 1986
      IC3=0
      DO3000I=1,NW
      ICMIN=IBEGIN(I)
      ICMAX=IEND(I)
      IF(ITYPE(I).EQ.'N   ')GOTO3100
      IF(ITYPE(I).EQ.'X   ')GOTO3200
      IF(ITYPE(I).EQ.'OP  ')GOTO3300
      IF(ITYPE(I).EQ.'LP  '.OR.ITYPE(I).EQ.'RP  ')GOTO3400
      IF(ITYPE(I).EQ.'PAR ')GOTO3500
      IF(ITYPE(I).EQ.'LF  ')GOTO3600
      IF(ITYPE(I).EQ.'COM ')GOTO3700
      WRITE(ICOUT,3005)
 3005 FORMAT('***** ERROR IN COMPID--ITYPE(I) NOT X, OP, LP, PAR, ',
     1'OR LF')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3006)I,ITYPE(I),IW21(I),W2(I)
 3006 FORMAT('I,ITYPE(I),IW21(I),W2(I) = ',
     1I8,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
C
 3100 CONTINUE
CCCCC IC=IC+1 APRIL 29, 1986
      IC3=IC3+1
CCCCC IW21(I)=IC
CCCCC CALL DPC4IH(IC,IW21(I),IBUGEV,IERROR) APRIL 29, 1986
      CALL DPC4IH(IC3,IW21(I),IBUGEV,IERROR)
      IW22(I)='    '
      W2(I)=0.0
      IANS1='    '
      IANS2='    '
      IANS3='    '
      IANS4='    '
      J=0
      DO3150IC=ICMIN,ICMAX
      J=J+1
      JM1=J-1
      L=J-(NUMASC*(JM1/NUMASC))
      K=NUMBPC*(L-1)
      K=IABS(K)
CCCCC WRITE(ICOUT,3333)J,JM1,L,K,IR(IC)
 3333 FORMAT('J,JM1,L,K,IR(IC) = ',4I8,2X,A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IF(J.LE.NUMASC)GOTO3151
      IF(J.LE.NUMAS2)GOTO3152
      IF(J.LE.NUMAS3)GOTO3153
      IF(J.LE.NUMAS4)GOTO3154
      GOTO3155
 3151 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS1)
      GOTO3155
 3152 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS2)
      GOTO3155
 3153 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS3)
      GOTO3155
 3154 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS4)
      GOTO3155
 3155 CONTINUE
CCCCC WRITE(ICOUT,4444)IANS1,IANS2,IANS3,IANS4
 4444 FORMAT(4A4)
CCCCC CALL DPWRST('XXX','BUG ')
 3150 CONTINUE
      CALL ERRORF(IANS1,IANS2,IANS3,IANS4,-1000000000.0,1000000000.0,
     11000000000.0,ANS2,IERROR)
      IF(IERROR.EQ.'YES ')GOTO9000
      W2(I)=ANS2
      GOTO3000
C
 3200 CONTINUE
      W2(I)=0.0
      GOTO3000
C
 3300 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      ICMINP=ICMIN+1
      IF(IR(ICMIN).EQ.'*   '.AND.IR(ICMINP).EQ.'*   ')IW21(I)='**  '
      IF(IR(ICMIN).EQ.'*   '.AND.IR(ICMINP).EQ.'*   ')IW22(I)='    '
      GOTO3000
C
 3400 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      GOTO3000
C
 3500 CONTINUE
      IW21(I)='    '
      IW22(I)='    '
      ICMAX2=ICMIN+NUMAS2-1
      IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
      J=0
      DO3530IC=ICMIN,ICMAX2
      J=J+1
      J2=J
      IF(J2.GT.NUMASC)J2=J-NUMASC
      ISTAR3=NUMBPC*(J2-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
 3530 CONTINUE
C
      IF(IPASS.EQ.1)GOTO3000
C
      IF(NUMPAR.LE.0)GOTO3559
      DO3550J=1,NUMPAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GOTO3555
 3550 CONTINUE
      GOTO3559
 3555 CONTINUE
      W2(I)=PARAM(J)
      GOTO3000
 3559 CONTINUE
C
      IF(NUMVAR.LE.0)GOTO3569
      DO3560J=1,NUMVAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GOTO3565
 3560 CONTINUE
 3565 CONTINUE
      W2(I)=0.0
      ITYPE(I)='VAR '
      GOTO3000
 3569 CONTINUE
C
      WRITE(ICOUT,3571)
 3571 FORMAT('***** ERROR IN COMPID--NO MATCH FOR PARAM./VAR. NAME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3572)IW21(I),IW22(I)
 3572 FORMAT('                       GIVEN PARAM./VAR. NAME = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3573)NUMPAR
 3573 FORMAT('                       NUMBER OF PARAM./VAR. =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3574)
 3574 FORMAT('                       ADMISSIBLE PARAM./VAR. ',
     1'NAMES = ')
      CALL DPWRST('XXX','BUG ')
      DO3575J=1,NUMPAR
      WRITE(ICOUT,3576)J,IPARN1(J),IPARN2(J)
 3576 FORMAT('                       PARAM./VAR. NAME ',I4,'--  ',
     12A4)
      CALL DPWRST('XXX','BUG ')
 3575 CONTINUE
      WRITE(ICOUT,3577)(IA(J),J=1,NUMCHA)
 3577 FORMAT('      FUNCTION EXPRESSION--',100A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GOTO9000
C
 3600 CONTINUE
      IW21(I)='    '
      IW22(I)='    '
      ICMAX2=ICMIN+NUMAS2-1
      IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
      J=0
      DO3650IC=ICMIN,ICMAX2
      J=J+1
      J2=J
      IF(J2.GT.NUMASC)J2=J-NUMASC
      ISTAR3=NUMBPC*(J2-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
 3650 CONTINUE
      GOTO3000
C
 3700 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      GOTO3000
C
 3000 CONTINUE
      NWHOLD=NW
      DO3900I=1,NW
      ITYPEH(I)=ITYPE(I)
      IW21HO(I)=IW21(I)
      IW22HO(I)=IW22(I)
      W2HOLD(I)=W2(I)
 3900 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO3999
      ISTEPN='5'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      DO3992I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,3993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),
     1IW21(I),IW22(I),W2(I)
 3993 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',
     1'IW21(I),IW22HO(I),W2(I) = ',I8,2X,3A4,2X,A4,2X,A4,2X,A4,2X,E15.6)
      CALL DPWRST('XXX','BUG ')
 3992 CONTINUE
 3999 CONTINUE
C
C               ****************************************************
C               **  STEP 6--                                      **
C               **  THIS STEP IS TO BE EXECUTED IF IPASS=1;       **
C               **  OTHERWISE IT IS SKIPPED.                      **
C               **  IF THIS STEP IS EXECUTED, STEP 7 IS NOT;      **
C               **  IF THIS STEP IS NOT EXECUTED, STEP 7 IS.      **
C               **  OPERATE ON IW2 AND ITYPE VECTORS.             **
C               **  DETERMINE THE NUMBER OF DISTINCT PARAMETERS.  **
C               **  FORM THE OUTPUT VECTOR IPARN1.                 **
C               **  CHECK TO SEE IF SOME OF THE PREVIOSULY-       **
C               **  DEFINED PARAMETERS ARE IN FACT VARIABLES.     **
C               ****************************************************
C
      IF(IPASS.EQ.1)GOTO4050
      GOTO4999
 4050 CONTINUE
C
      NUMPAR=0
      DO4100I=1,NW
      IF(ITYPE(I).EQ.'PAR ')GOTO4190
      GOTO4100
 4190 CONTINUE
C
      IF(NUMVAR.LE.0)GOTO4290
      DO4250J=1,NUMVAR
      IF(IW21(I).EQ.IVARN1(J).AND.IW22(I).EQ.IVARN2(J))GOTO4260
 4250 CONTINUE
      GOTO4290
 4260 CONTINUE
      ITYPE(I)='VAR '
      GOTO4100
 4290 CONTINUE
C
      IF(NUMPAR.EQ.0)GOTO4300
      DO4400J=1,NUMPAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GOTO4100
 4400 CONTINUE
 4300 CONTINUE
      NUMPAR=NUMPAR+1
      IPARN1(NUMPAR)=IW21(I)
      IPARN2(NUMPAR)=IW22(I)
 4100 CONTINUE
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO4599
      ISTEPN='6'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,4591)
 4591 FORMAT('AT END OF STEP 6 FOR PASS 1 (RIGHT BEFORE ',
     1'RETURNING TO MAIN ROUTINE FROM COMPID)--')
      CALL DPWRST('XXX','BUG ')
      DO4592I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,4593)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),
     1IW21(I),IW22(I),W2(I)
 4593 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',
     1'IW21(I),IW22(I),W2(I) = ',I8,2X,3A4,A4,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 4592 CONTINUE
 4599 CONTINUE
C
      GOTO9000
 4999 CONTINUE
C
C               ****************************************************************
C               **  STEP 7--
C               **  OPERATE ON THE W2(.) AND IW21(.) VECTORS.
C               **  THIS STEP IS NOT EXECUTED IF STEP 6 IS;
C               **  THIS STEP IS EXECUTED IF STEP 6 IS NOT.
C               **  FIRST MAKE SURE THAT THE NUMBER OF LEFT
C               **  AND RIGHT PARENTHESES ARE THE SAME.
C               **  (STEP 6 THEN SETS UP A LARGE DO LOOP
C               **  WHICH GOES THROUGH ALL OF THE VALUES OF THE X VECTOR
C               **  AND GENERATES CORRESPONDING VALUES OF THE Y VECTOR.)
C               **  FOR A GIVEN X VALUE, IT EVALUATES THE FUNCTION
C               **  BY FIRST SEEKING THE INNERMOST PARENTHESES
C               **  (BY SEARCHING FOR THE FIRST REMAINING RIGHT PARENTHESS).
C               **  AND THEN EVALUATING ALL SUCH PARENTHETICAL EXPRESSIONS--
C               **  WORKING FROM THE INNERMOST OUT.
C               **  AFTER EVALUATING A PARENTHESES PAIR,
C               **  THE ENTIRE PARENTHESES GROUP (PARENTHESES INCLUDED)
C               **  IS REPLACED BY THE SCALAR ANSWER.
C               **  THE IW2, W2, AND ITYPE VECTORS ARE SQUEEZED ACCORDINGLY
C               **  (IN THE SUBROUTINE EVAL).
C               **  SINCE THE VECTORS IW2, W2, AND ITYPE ARE ALTERED (SQUEEZED)
C               **  FOR EACH X VALUE, THEY MUST BE REDEFINED FROM THE SAVED
C               **  VALUES IN IW2, W2, AND ITYPE FOR EACH NEW X VALUE.
C               **  THE ABOVE SQUEEZING OPERATION IS REPEATED
C               **  FOR EACH PARENTHESES PAIR UNTIL ALL PARENTHESES
C               **  ARE GONE AND WE REMAIN ONLY WITH THE FINAL ANSWER.
C               **  FOR EACH VALUE X(.) OF THE INPUT X VECTOR,
C               **  OUTPUT THE CORRESPONDING VALUE Y(.) OF
C               **  THE DESIRED OUTPUT VECTOR.
C               **  FOR A GIVEN VALUE X(.), THE CORRESPONDING
C               **  COMPUTED Y(.) WILL BE THE EVALUATED VALUE OF
C               **  THE RIGHT-HAND SIDE OF THE SPECIFIED EQUATION Y = F(X).
C               ****************************************************************
C
 5000 CONTINUE
C
      NLP=0
      NRP=0
      DO5100I=1,NW
      IF(ITYPE(I).EQ.'LP  ')NLP=NLP+1
      IF(ITYPE(I).EQ.'RP  ')NRP=NRP+1
 5100 CONTINUE
      IF(NLP.EQ.NRP)GOTO5190
      WRITE(ICOUT,5155)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5156)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5157)NLP
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5158)NRP
 5155 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
 5156 FORMAT('NUMBER OF LEFT PARENTHESES NOT EQUAL TO ',
     1'NUMBER OF RIGHT PARENTHESES')
 5157 FORMAT('NUMBER OF LEFT  PARENTHESES = ',I8)
 5158 FORMAT('NUMBER OF RIGHT PARENTHESES = ',I8)
      IERROR='YES '
      GOTO9000
 5190 CONTINUE
C
CCCCC DO8000II=1,N
      NW=NWHOLD
      DO5200I=1,NW
      ITYPE(I)=ITYPEH(I)
      IW21(I)=IW21HO(I)
      IW22(I)=IW22HO(I)
      W2(I)=W2HOLD(I)
C     THE FOLLOWING STATEMENT HAS BEEN COMMENTED OUT
C     IN GOING FROM COMPIL TO COMPID.
CCCCC IF(ITYPE(I).EQ.'X   ')W2(I)=X(II)
 5200 CONTINUE
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1GOTO5249
      GOTO5299
 5249 CONTINUE
      ISTEPN='7'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      DO5250I=1,NW
      WRITE(ICOUT,5251)I,IW21HO(I),IW21(I),ITYPE(I)
 5251 FORMAT('I,IW21HO(I),IW21(I),ITYPE(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5250 CONTINUE
 5299 CONTINUE
C
C               *********************************
C               **  STEP 7--                   **
C               **  DETERMINE THE DERIVATIVE.  **
C               *********************************
C
      CALL DERIV0(IW21,IW22,ITYPE,NW,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICON,ICON1,ICON2,NCON,ID1,ID2,NUMCD2,
     1IBUGEV,ISUBRO,IFOUND,IERROR)
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO5319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5311)
 5311 FORMAT('***** IN COMPID, AFTER RETURNING FROM DERIV0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5312)NUMCD2
 5312 FORMAT('      NUMCD2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO5315I=1,NUMCD2
      WRITE(ICOUT,5316)I,ID1(I),ID2(I)
 5316 FORMAT('      I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5315 CONTINUE
 5319 CONTINUE
C
C               ***********************************************************
C               **  STEP 7.2--                                           **
C               **  FORM THE OUTPUT VECTOR ID(.).                        **
C               **  NOTE THAT ID1(.) AND ID2(.) ARE PARALLEL             **
C               **  REPRESENTATIONS OF THE DESIRED DERIVATIVE FUNCTION   **
C               **  (ID1(.) HAS THE FIRST 4 CHARACTERS;                  **
C               **  ID2(.) HAS THE NEXT 4 CHARACTERS).                   **
C               **  MOST COMPONENTS (E.G., +, -, *, /, (, ), ETC.)       **
C               **  USE ONLY 1 CHARACTER OUT OF THE 8.                   **
C               **  SOME COMPONENTS (NAMELY, **)                         **
C               **  USE 2 CHARACTERS OUT OF THE 8.                       **
C               **  SOME COMPONTENTS (NAMELY, LIBRARY FUNCTIONS)         **
C               **  USE MANY (3 TO 7) CHARACTERS OUT OF THE 8.           **
C               **  IN ANY EVENT, THE OUTPUT VECTOR ID(.) WILL BE        **
C               **  AN UNPACKED (1 CHARACTER PER WORD) SYNTHESIS         **
C               **  OF THE 2 PACKED \VYYEYC\TYORS Y\I\D1(.) AND ID2(.).  **
C               ***********************************************************
C
      ISTEPN='7.2'
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      IF(NUMCD2.LE.0)GOTO5639
      DO5600I=1,NUMCD2
      IF(ID1(I).EQ.'    ')GOTO5619
      J=J+1
      ID3(J)=ID1(I)
 5619 CONTINUE
      IF(ID2(I).EQ.'    ')GOTO5629
      J=J+1
      ID3(J)=ID2(I)
 5629 CONTINUE
 5600 CONTINUE
 5639 CONTINUE
      NUMCH3=J
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO5649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5641)NUMCD2,NUMCH3
 5641 FORMAT('NUMCD2,NUMCH3 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO5645I=1,NUMCH3
      WRITE(ICOUT,5646)I,ID3(I)
 5646 FORMAT('I,ID3(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5645 CONTINUE
 5649 CONTINUE
C
      K=0
      DO5700I=1,NUMCH3
      IF(ID3(I).EQ.'    ')GOTO5700
      CALL DPXH1H(ID3(I),ICH,ILASTC,IBUGEV)
      IF(ILASTC.LE.0)GOTO5700
      DO5750J=1,ILASTC
      K=K+1
      ID(K)=ICH(J)
 5750 CONTINUE
 5700 CONTINUE
      NCTOTD=K
C
      IF(NCTOTD.GE.1)GOTO5789
      WRITE(ICOUT,5705)NCTOTD
 5705 FORMAT('***** ERROR IN COMPID--TOTAL NUMBER OF CHARACTERS ',
     1'IN DERIVATIVE. (INCL. BLANKS, AND EQUAL SIGN) ',
     1'IS < 2.  NCTOTD = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5771)NUMCHD,N,IPASS
 5771 FORMAT('NUMCHD,N,IPASS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5772)(ID(I),I=1,NUMCHD)
 5772 FORMAT('ID--',80A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5789 CONTINUE
C
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO5799
      WRITE(ICOUT,5791)NCTOTD
 5791 FORMAT('NCTOTD = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO5792I=1,NCTOTD
      WRITE(ICOUT,5793)I,ID(I)
 5793 FORMAT('I,ID(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5792 CONTINUE
 5799 CONTINUE
      NUMCHD=NCTOTD
C
C               *******************************************
C               **  STEP 7.3--                           **
C               **  SIMPLIFY THE FUNCTIONAL EXPRESSION.  **
C               *******************************************
C
      CALL DPSIPA(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSISI(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA2(ID,NUMCHD,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(ID,NUMCHD,IBUGEV,IERROR)
C
      CALL DPSIPA(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSISI(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA2(ID,NUMCHD,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(ID,NUMCHD,IBUGEV,IERROR)
C
 8000 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NUMCHA,NUMCHD
 9012 FORMAT('NUMCHA,NUMCHD = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)
 9013 FORMAT('INPUT FUNCTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)(IA(J),J=1,NUMCHA)
 9016 FORMAT(130A1)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,9022)
 9022 FORMAT('OUTPUT DERIVATIVE--')
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMCHD,12
      JMIN=I
      JMAX=JMIN+11
      IF(JMAX.GT.NUMCHD)JMAX=NUMCHD
      WRITE(ICOUT,9026)(ID(J),J=JMIN,JMAX)
 9026 FORMAT(12A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
C
      WRITE(ICOUT,9031)IBUGEV,IERROR
 9031 FORMAT('IBUGEV,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN1,IPARN2,NUMPAR,
     1                  IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y,
     1                  IBUGCO,IBUGEV,IERROR)
C
C     PURPOSE--THIS SUBROUTINE INTERPRETS AND EVALUATES A FORTRAN
C              MATHEMATICAL FUNCTION EXPRESSION OF THE FORM
C               Y=F(.,.,.,.,...).  IT IS A GENERALIZATION OF JJF7.COMPIL
C              WHICH COULD HANDLE ONLY 1 ARGUMENT (X).  THIS SUBROUTINE
C              IS TYPICALLY ENTERED WITH TWO PASSES--
C              THE FIRST PASS ANALYZES THE STRING AND HAS AS ITS OUTPUT
C              THE HOLLERITH NAMES OF THE VARIOUS PARAMETERS. A
C              'PARAMETER' IN THIS SUBROUTINE (COMPIM) MEANS ANY USUAL
C              PARAMETER IN AN EXPRESSION AS WELL AS ANY VARIABLE NAME
C              (E.G., X1, X2, X3, TEMP, RES, ETC.) THIS IS A FUNDAMENTAL
C              WAY THAT COMPIM DIFFERS FROM COMPIL.  ALSO, COMPIM OUTPUTS
C              ONLY A COMPUTED SCALAR VALUE (AS OPPOSED TO COMPIL WHICH
C              OUTPUTS AN ENTIRE COMPUTED VECTOR).  THESE NAMES ARE
C              OUTPUTTED IN THIS FIRST PASS AS ELEMENTS IN THE VECTORS
C              IPARN1 AND IPARN2.  THE SECOND PASS USES INPUT PARAMETER
C              VALUES (INPUTTED IN THE VECTOR PARAM) TO ACTUALLY EVALUATE
C              THE FUNCTION (OUTPUTTED IN THE SCALAR Y).  NOTE THAT IF
C              SOME OF THE 'PARAMETERS' ARE IN FACT ELEMENTS OF A VECTOR
C              VARIABLE, THE ITERATING THROUGH THE ENTIRE VECTOR IS DONE
C              IN THE CALLING SUBROUTINE AND NOT WITHIN COMPIM
C              (THIS IS ANOTHER WAY THAT COMPIM DIFFERS FROM COMPIL).
C     INPUT  ARGUMENTS--IA     = THE INTEGER VECTOR WHICH CONTAINS
C                                THE HOLLERITH CHARACTERS WHICH
C                                MAKE UP THE LINE OF FORTRAN CODE.
C                                THIS VECTOR CONTAINS THE STRING
C                                TO BE OPERATED ON, INTERPRETED,
C                                AND EVALUATED.
C                     --NUMCHA = THE INTEGER VALUE WHICH
C                                DEFINES THE NUMBER OF CHARACTERS IN IA.
C                                NUMCHA DEFINES THE LENGTH OF THE
C                                HOLLERITH STRING TO BE OPERATED ON,
C                                INTERPRETED, AND EVALUATED.
C                     --IPASS  = AN INTEGER FLAG CODE
C                                WHICH DEFINES WHICH PASS (1 OR 2) INTO THIS
C                                SUBROUTINE THE USER IS IN.
C                                PASS 1 DETERMINE PARAMETER NAMES;
C                                PASS 2 DOES FUNCTION EVALUATIONS.
C                     --PARAM  = THE SINGLE PRECISION VECTOR OF PARAMETER
C                                (AND VARIABLE)
C                                VALUES CORRESPONDING TO THE PARAMETER NAMES
C                                AS GIVEN IN THE VECTOR IPARN.
C                     --IPARN1 = THE INTEGER VECTOR
C                                CONTAINING CHARACTERS 1 THROUGH 4
C                                OF PARAMETER (AND VARIABLE)
C                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
C                     --IPARN2 = THE INTEGER VECTOR
C                                CONTAINING CHARACTERS 5 THROUGH 8
C                                OF PARAMETER (AND VARIABLE)
C                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
C      OUTPUT ARGUMENTS--Y     = THE SINGLE PRECISION COMPUTED SCALAR VALUE OF
C                                THE FUNCTION AS DETERMINED BY PASS 2
C                                AND WHICH CONSTITUTE THE ULTIMATE
C                                OUTPUT FROM THIS SUBROUTINE.
C                                THAT IS, SYMBOLICALLY,
C                                Y = F(X1,X2,X3,TEMP,RES,ETC.,PAR1,PAR2,PAR3,ETC
C     OUTPUT--THE SINGLE PRECISION COMPUTED SCALAR VALUE,
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER           SUBROUTINES NEEDED--EVAL
C     FORTRAN LIBRARY SUBROUTINES NEEDED--(ALL IN EVAL)
C                                         SQRT
C                                         EXP
C                                         LOG
C                                         LOG10
C                                         SIN
C                                         COS
C                                         ATAN
C                                         ATAN2
C                                         TANH
C                                         ABS
C                                         AINT
C                                         ARCSIN
C                                         ARCCOS
C                                         ARCTAN
C                                         OCTAL
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     NOTE--THIS SUBROUTINE ALLOWS ONE TO PERFORM
C           INTERACTIVE FUNCTION EVALUATIONS.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1976.
C     UPDATED         --FEBRUARY  1977.
C     UPDATED         --DECEMBER  1977.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --DECEMBER  1978.
C     UPDATED         --JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1986.
C     UPDATED         --DECEMBER  1988.  BLANK OUT IR(.) FOR AT LEAST 10 CHAR
C     UPDATED         --SEPTEMBER 1994.  ADD SAVE4 ARGUMENT TO EVALM.
C     UPDATED         --APRIL     1995.  BUG:
C                                        LET A = TPDF(X,2) - TPDF(X,3)
C                                        SETS SAVE1 TO 2 IN BOTH CASES
C     UPDATED         --MAY       1998.  ADD FIFTH PARAMETER
C     UPDATED         --JUNE      2003.  ADD SAVE6, SAVE7, SAVE8
C                                        ARGUMENTS TO EVALM.
C     UPDATED         --FEBRUARY  2005.  CONVERT STRING TO UPPER CASE
C     UPDATED         --DECEMBER  2010.  INITIALIZATION OF SAVE1 ...
C                                        SAVE8
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IA
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IR
      CHARACTER*4 IB
      CHARACTER*4 IL
      CHARACTER*4 ICH
      CHARACTER*4 IW21
      CHARACTER*4 IW22
      CHARACTER*4 ITYPE
      CHARACTER*4 IANS1
      CHARACTER*4 IANS2
      CHARACTER*4 IANS3
      CHARACTER*4 IANS4
      CHARACTER*4 IFOUND
C
C---------------------------------------------------------------------
C
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
C
C     NOTE--THE DIMENSIONS OF ITYPEH, IW21HO, IW22HO, AND W2HOLD
C           WHICH ARE DEFINED IN THE MAIN PROGRAM
C           SHOULD BE AT LEAST AS LARGE AS THE DIMENSIONS
C           OF IW2 AND IW22 BELOW.
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
C     NOTE--THE DIMENSION OF IB SHOULD BE THE SAME AS
C           THE DIMENSION OF SUBROUTINE IA IN DPLET.
C
      PARAMETER (MAXCHA=1000)
C
      DIMENSION IB(MAXCHA)
      DIMENSION IR(MAXCHA)
      DIMENSION IBEGIN(MAXCHA)
      DIMENSION IEND(MAXCHA)
      DIMENSION ITYPE(MAXCHA)
      DIMENSION IW21(MAXCHA)
      DIMENSION IW22(MAXCHA)
      DIMENSION W2(MAXCHA)
C
      DIMENSION ICH(10)
      DIMENSION IL(10)
C
CCCCC ADD FOLLOWING SECTION APRIL 1995.
C
      PARAMETER(MAXNST=25)
      DIMENSION SAVE1(MAXNST)
      DIMENSION SAVE2(MAXNST)
      DIMENSION SAVE3(MAXNST)
      DIMENSION SAVE4(MAXNST)
      DIMENSION SAVE5(MAXNST)
      DIMENSION SAVE6(MAXNST)
      DIMENSION SAVE7(MAXNST)
      DIMENSION SAVE8(MAXNST)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE THE UPPER LIMIT OF THE NUMBER OF CHARACTERS THAT MAY BE
C     PROCESSED BY THIS SUBROUTINE (COUNTING BLANKS, LEFT-HAND SIDE,
C     EQUAL SIGN, AND RIGHT HAND SIDE).  IF RESTRICT THE EXPRESSION TO 1
C     LINE IMAGE, THEN A REASONABLE UPPER BOUND IS 80.  WHATEVER UPPER
C     BOUND IS SET, THE DIMENSIONS OF MOST OF THE VECTORS MUST BE EQUAL
C     OR LARGER TO THIS NUMBER.  (THE VECTOR IL(.) WHICH CONTAINS THE
C     NUMBER OF CHARACTERS TO THE LEFT OF THE EQUAL SIGN (BLANKS IGNORED)
C     MAY BE MUCH SMALLER--LIKE 6.)
C     NOTE--AS OF JANUARY 1979, THE BOUND WAS RESET TO 150.
C
CCCCC DATA MAXCHA/150/
CCCCC DATA MAXCHA/225/
CCCCC DATA MAXCHA/1000/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COMP'
      ISUBN2='IM  '
C
      IERROR='NO'
C
C     THE FOLLOWING STATEMENT (N=1) HAS BEEN ADDED IN CONVERTING
C     THE COMPIL SUBROUTINE TO THE COMPIM SUBROUTINE.
C
      N=1
C
      IF(IBUGCO.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF COMPIM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NUMCHA,N,IPASS,IANGLU,IBUGCO,IBUGEV
   52   FORMAT('NUMCHA,N,IPASS,IANGLU,IBUGCO,IBUGEV = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)(IA(I),I=1,MIN(80,NUMCHA))
   53   FORMAT('IA--',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)NUMPAR
   61   FORMAT('NUMPAR = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMPAR.GE.1)THEN
          DO62I=1,NUMPAR
            WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I),PARAM(I)
   63       FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2(2X,A4),2X,
     1             F15.7)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)NWHOLD
   71   FORMAT('NWHOLD = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NWHOLD.GE.1)THEN
          DO72I=1,NWHOLD
            WRITE(ICOUT,73)I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I)
   73       FORMAT('I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I) = ',
     1             I8,3(2X,A4),2X,F15.7)
            CALL DPWRST('XXX','BUG ')
   72     CONTINUE
        ENDIF
        WRITE(ICOUT,81)IPASS,NW
   81   FORMAT('IPASS,NW = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NW.GE.1)THEN
          WRITE(ICOUT,82)ITYPE(NW)
   82     FORMAT('ITYPE(NW) = ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               **********************************************************
C               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.**
C               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND       **
C               **  REGARDLESS OF THE WORD SIZE.                        **
C               **********************************************************
C
      NUMASC=4
      NUMAS2=2*NUMASC
      NUMAS3=3*NUMASC
      NUMAS4=4*NUMASC
C
C     IF IPASS = 2, SKIP ALL OF THE PRELIMINARY CODE
C     AND JUMP TO CALCULATIVE PART OF CODE.
C
      IF(IPASS.EQ.2)GOTO5000
C
C     CHECK THAT THE INPUT NUMBER OF CHARACTERS NUMCHA
C     (INCLUDING LEFT SIDE, RIGHT SIDE, EQUAL SIGN,
C     AND BLANKS) IS AT LEAST 1 AND AT MOST MAXCHA
C     (WHERE MAXCHA IS THE INTERNALLY DEFINED VARIABLE
C     WHICH CONTROLS DIMENSION SIZES AND WHICH
C     TYPICALLY HAS THE VALUE 80).
C
      IF(NUMCHA.LT.1 .OR. NUMCHA.GT.MAXCHA)THEN
        WRITE(ICOUT,21)
   21   FORMAT('***** ERROR IN COMPIM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
   22   FORMAT('      THE NUMBER OF CHARACTERS NUMCHA WHICH DEFINES ',
     1         'THE LENGTH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,24)
   24   FORMAT('      OF THE INPUT EXPRESSION (INCLUDING LEFT-HAND ',
     1         'SIDE,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,25)
   25   FORMAT('      RIGHT-HAND SIDE, EQUAL SIGN, AND ALL BLANKS) IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)
   26   FORMAT('      LESS THAN 1 OR LARGER THAN MAXCHA (MAXCHA IS AN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,28)MAXCHA
   28   FORMAT('      INTERNALLY DEFINED VARIABLE WHICH HAS THE ',
     1         'VALUE = ',I8,'   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,30)NUMCHA
   30   FORMAT('      THE NUMBER OF CHARACTERS IN THE INPUT ',
     1         'EXPRESSION IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,31)(IA(I),I=1,MIN(100,NUMCHA))
   31     FORMAT('      INPUT EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
CCCCC FEBRUARY 2005.  CONVERT INPUT FUNCTION TO ALL UPPER CASE.
CCCCC                 THIS IS TO ADDRESS ISSUE WHERE IF FUNCTION
CCCCC                 WAS DEFINED AS "LET STRING" RATHER THAN
CCCCC                 "LET FUNCTION", CASE IS PRESERVED.  HOWEVER,
CCCCC                 WHEN EVALUATING FUNCTION, WE NEED THE STRING
CCCCC                 TO BE EVALUATED IN UPPER CASE.
C
      DO91I=1,NUMCHA
        ITEMP=ICHAR(IA(I)(1:1))
        IF(ITEMP.GE.97 .AND. ITEMP.LE.122)THEN
          ITEMP=ITEMP-32
          IA(I)(1:1)=CHAR(ITEMP)
        ENDIF
   91 CONTINUE
C
C     BLANK-OUT AND ZERO-OUT SOME VARIABLES AND VECTORS.
C
      Y=0.0
      DO160I=1,NUMCHA
        IR(I)='    '
        IB(I)='    '
        IW21(I)='    '
        IW22(I)='    '
        W2(I)=0.0
        ITYPE(I)='    '
        IW21HO(I)='    '
        IW22HO(I)='    '
        W2HOLD(I)=0.0
        ITYPEH(I)='    '
  160 CONTINUE
C
C     THE FOLLOWING LOOP WAS PUT IN TO AVOID A PROBLEM
C     ESSENTAILLY CAUSED IN DPLIB1 AND WHICH
C     SHOWED UP IN  LET A = 1 1 3   LET A = ABS(A)   LET B = A
C     MARY BETH    12/88
C
      DO161I=1,10
        IR(I)='    '
  161 CONTINUE
C
C               ************************************
C               **  STEP 1--                      **
C               **  OPERATE ON THE VECTOR IA(.).  **
C               **  SQUEEZE OUT ALL BLANKS.       **
C               **  OUTPUT THE VECTOR IB(.).      **
C               ************************************
C
      K=0
      DO100I=1,NUMCHA
        IF(IA(I).EQ.' ')GOTO100
        CALL DPXH1H(IA(I),ICH,ILASTC,IBUGCO)
        IF(ILASTC.LE.0)GOTO100
        DO150J=1,ILASTC
          K=K+1
          IB(K)=ICH(J)
  150   CONTINUE
  100 CONTINUE
      NCTOT=K
      IF(NCTOT.LT.1)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NCTOT
  105   FORMAT('      TOTAL NUMBER OF CHARACTERS IN MODEL (INCLUDING ',
     1         'BOTH SIDES, BLANKS, AND EQUAL SIGN) IS < 1.  NCTOT = ',
     1         I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,171)NUMCHA,N,IPASS
  171   FORMAT('NUMCHA,N,IPASS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,172)(IA(I),I=1,MIN(80,NUMCHA))
  172   FORMAT('IA--',80A1)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)NUMPAR
  181   FORMAT('NUMPAR = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMPAR.GT.0)THEN
          DO182I=1,NUMPAR
            WRITE(ICOUT,183)I,IPARN1(I),IPARN2(I),PARAM(I)
  183       FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2(2X,A4),2X,
     1             F15.7)
            CALL DPWRST('XXX','BUG ')
  182     CONTINUE
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,191)NCTOT
  191   FORMAT('NCTOT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO192I=1,NCTOT
          WRITE(ICOUT,193)I,IB(I)
  193     FORMAT('I,IB(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  192   CONTINUE
      ENDIF
C
C               *********************************************************
C               **  STEP 2--                                           **
C               **  OPERATE ON THE VECTOR IB(.).                       **
C               **  DETERMINE THE NUMBER OF CHARACTERS (IF ANY)        **
C               **  FOR THE LEFT-HAND SIDE.  OUTPUT THEM INTO THE      **
C               **  VECTOR IL(.).                                      **
C               *********************************************************
C
      DO500I=1,NCTOT
        I2=I
        IF(IB(I).EQ.'=')THEN
          NCL=I2-1
          ISTARR=I2+1
          GOTO559
        ENDIF
  500 CONTINUE
      NCL=0
      ISTARR=1
  559 CONTINUE
C
      IF(NCL.GT.0)THEN
        DO600I=1,NCL
          IL(I)=IB(I)
  600   CONTINUE
      ENDIF
C
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='2'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,691)NCL
  691   FORMAT('NCL = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO692I=1,NCL
          WRITE(ICOUT,693)I,IL(I)
  693     FORMAT('I,IL(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  692   CONTINUE
      ENDIF
C
C               *********************************************************
C               **  STEP 3--                                           **
C               **  OPERATE ON THE VECTOR IB(.).  DETERMINE THE        **
C               **  NUMBER OF CHARACTERS FOR RIGHT-HAND SIDE.          **
C               **  OUTPUT THEM INTO THE VECTOR IR(.).                 **
C               *********************************************************
C
      IF(ISTARR.GT.NCTOT)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,702)
  702   FORMAT('      THE NUMBER OF CHARACTERS ON THE RIGHT (WITH ',
     1         'BLANKS IGNORED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,703)
  703   FORMAT('      IS 0.  THE TOTAL NUMBER OF PACKED CHARACTERS ',
     1         'LEFT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,704)NCTOT
  704   FORMAT('      (IF ANY), EQUAL SIGN (IF ANY), AND RIGHT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,707)ISTARR
  707   FORMAT('      THE START POSITION FOR THE PACKED RIGHT IS ',
     1         'COLUMN ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,709)NUMCHA
  709   FORMAT('      THE INPUT NUMBER OF CHARACTERS NUMCHA = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,31)(IA(I),I=1,MIN(100,NUMCHA))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      K=0
      DO700I=ISTARR,NCTOT
        K=K+1
        IR(K)=IB(I)
  700 CONTINUE
      NCR=K
C
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='3'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,791)NCR
  791   FORMAT('NCR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO792I=1,NCR
          WRITE(ICOUT,793)I,IR(I)
  793     FORMAT('I,IR(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  792   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  OPERATE ON THE VECTOR IR(.).  ANALYZE THE         **
C               **  RIGHT-HAND SIDE.  DETERMINE THE NUMBER OF         **
C               **  DIFFERENT LOGICAL COMPONENTS.                     **
C               **     1. NUMBER (CONSISTING OF 0,1,2,...,9 OR .)     **
C               **     2. X VARIABLE                                  **
C               **     3. OPERATION (+   -   *   /   **)              **
C               **     4. PARENTHESES (   (   OR   )    )             **
C               **     5. LIBRARY FUNCTION (ALOG   EXP   ETC +        **
C               **        AUGMENTED LIB.                              **
C               **     6. COMMA (FOR MULTI-ARGUMENT LIBRARY FUNCTIONS)**
C               **     7. PARAMETER (ANYTHING NOT ABOVE)              **
C               **  CHECK FOR SYNTAX ERRORS.                          **
C               **  OUTPUT THE TYPE COMPONENT INTO ITYPE(.).          **
C               **  OUTPUT THE START LOCATION IN IR(.) OF EACH        **
C               **  OUTPUT THE STOP  LOCATION IN IR(.) OF EACH        **
C               **  COMPONENT INTO IE                                 **
C               ********************************************************
C
      NW=0
      I=1
 1050 CONTINUE
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
C
      IF(IR(I).EQ.'0' .OR. IR(I).EQ.'1' .OR. IR(I).EQ.'2' .OR.
     1   IR(I).EQ.'3' .OR. IR(I).EQ.'4' .OR. IR(I).EQ.'5' .OR.
     1   IR(I).EQ.'6' .OR. IR(I).EQ.'7' .OR. IR(I).EQ.'8' .OR.
     1   IR(I).EQ.'9' .OR. IR(I).EQ.'.')THEN
        NW=NW+1
        ITYPE(NW)='N'
        JMIN=I
        J=I
 1150   CONTINUE
        J=J+1
        IF(J.LE.NCR)THEN
          IF(IR(J).EQ.'0' .OR. IR(J).EQ.'1' .OR. IR(J).EQ.'2' .OR.
     1       IR(J).EQ.'3' .OR. IR(J).EQ.'4' .OR. IR(J).EQ.'5' .OR.
     1       IR(J).EQ.'6' .OR. IR(J).EQ.'7' .OR. IR(J).EQ.'8' .OR.
     1       IR(J).EQ.'9' .OR. IR(J).EQ.'.')THEN
               GOTO1150
          ENDIF
        ENDIF
        JMAX=J-1
        GOTO1800
      ELSEIF(IR(I).EQ.'+' .OR. IR(I).EQ.'-' .OR. IR(I).EQ.'*' .OR.
     1   IR(I).EQ.'/')THEN
        NW=NW+1
        ITYPE(NW)='OP'
        JMIN=I
        JMAX=I
        IP1=I+1
        IF(IR(I).EQ.'*'.AND.IR(IP1).EQ.'*')JMAX=IP1
        GOTO1800
      ELSEIF(IR(I).EQ.'(')THEN
        NW=NW+1
        ITYPE(NW)='LP'
        JMIN=I
        JMAX=I
        GOTO1800
      ELSEIF(IR(I).EQ.')')THEN
        NW=NW+1
        ITYPE(NW)='RP'
        JMIN=I
        JMAX=I
        GOTO1800
      ELSEIF(IR(I).EQ.',')THEN
        NW=NW+1
        ITYPE(NW)='COM'
        JMIN=I
        JMAX=I
        GOTO1800
      ENDIF
C
C     CHECK FOR A LIBRARY FUNCTION.
C
      CALL CKLIB1(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'NO')CALL CKLIB2(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IFOUND.EQ.'YES')THEN
        IF(NCLF.GE.1 .AND. NCLF.LE.8)THEN
          NW=NW+1
          ITYPE(NW)='LF'
          JMIN=I
          JMAX=I+NCLF-1
          GOTO1800
        ENDIF
      ENDIF
C
      NW=NW+1
      ITYPE(NW)='PAR'
      NLPWP=0
      NRPWP=0
      JMIN=I
      J=I
      ILOOP=0
 1650 CONTINUE
      J=J+1
      IF(J.LE.NCR)THEN
        IF(IR(J).EQ.'+' .OR. IR(J).EQ.'-' .OR.
     1     IR(J).EQ.'*' .OR. IR(J).EQ.'/' .OR.
     1     IR(J).EQ.',')GOTO1660
        IF(IR(J).EQ.'(')NLPWP=NLPWP+1
        IF(IR(J).EQ.')')NRPWP=NRPWP+1
        IF(IR(J).EQ.')'.AND.NRPWP.GT.NLPWP)GOTO1660
        ILOOP=ILOOP+1
        IF(ILOOP.LE.NUMAS2)GOTO1650
C
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1656)NUMAS2
 1656   FORMAT('      PARAMETER NAME EXCEEDS ',I8,' CHARACTERS')
        CALL DPWRST('XXX','BUG ')
        DO1657K=JMIN,J
          WRITE(ICOUT,1658)K,IR(K)
 1658     FORMAT('K, IR(K) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 1657   CONTINUE
        IERROR='YES'
        GOTO9000
      ENDIF
C
 1660 CONTINUE
      JMAX=J-1
C
 1800 CONTINUE
C
C     CHECK THAT NW HAS NOT EXCEEDED MAXCHA (USUALLY 80)
C
      IF(NW.GT.MAXCHA)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1902)
 1902   FORMAT('      THE VARIABLE NW HAS JUST EXCEEDED THE MAXIMUM ',
     1         'ALLOWABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1903)
 1903   FORMAT('      LIMIT DEFINED BY THE INTERNAL VARIABLE MAXCHA.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1904)MAXCHA
 1904   FORMAT('      THIS LIMIT IS MAXCHA = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1905)NUMCHA
 1905   FORMAT('      THE INPUT NUMBER OF CHARACTERS NUMCHA = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,31)(IA(I),I=1,MIN(100,NUMCHA))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,1907)
 1907   FORMAT('      THE NUMBER OF (PACKED) CHARACTERS ON ',
     1         'RIGHT-HAND SIDE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCR.GE.1)THEN
          WRITE(ICOUT,1908)(IR(I),I=1,MIN(95,NCR))
 1908     FORMAT('      (PACKED) RIGHT-HAND SIDE--',95A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IBEGIN(NW)=JMIN
      IEND(NW)=JMAX
      I=JMAX
C
      I=I+1
      IF(I.LE.NCR)GOTO1050
 1950 CONTINUE
C
C     TEST THAT NW IS POSITIVE.
C
      IF(NW.LT.1)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1951)NW
 1951   FORMAT('      NW IS NON-POSITIVE.  NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NW.EQ.1)THEN
        DO1960I=1,NW
          IP1=I+1
          IF(ITYPE(I).EQ.'LF'.AND.ITYPE(IP1).NE.'LP')THEN
            WRITE(ICOUT,21)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1962)
 1962       FORMAT('      LIBRARY FUNCTION NOT FOLLOWED BY A LEFT ',
     1             'PARENTHESES')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1963)NW
 1963       FORMAT('             NW = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1964)I
 1964       FORMAT('             I  = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1965)ITYPE(I)
 1965       FORMAT('             ITYPE(I) = ',A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1966)ITYPE(IP1)
 1966       FORMAT('             ITYPE(I+1) = ',A4)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
 1960   CONTINUE
      ENDIF
C
      IF(ITYPE(NW).EQ.'OP')THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1971)ITYPE(NW)
 1971   FORMAT('      LAST TERM IN TOTAL EXPRESSION IS AN OPERATION = ',
     1         A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(ITYPE(NW).EQ.'LF')THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1973)ITYPE(NW)
 1973   FORMAT('      LAST TERM IN TOTAL EXPRESSION = A LIBRARY ',
     1         'FUNCTION = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1975)IPASS,NW
 1975   FORMAT('IPASS,NW = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NW.GE.1)THEN
          WRITE(ICOUT,1976)ITYPE(NW)
 1976     FORMAT('ITYPE(NW) = ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='4'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1991)NW
 1991   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO1992I=1,NW
          ICMIN=IBEGIN(I)
          ICMINP=ICMIN+1
          ICMINQ=ICMIN+2
          WRITE(ICOUT,1993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),
     1                     IBEGIN(I),IEND(I)
 1993     FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',
     1           'IBEGIN(I),IEND(I) = ',I8,2X,4A4,2(2X,I8))
          CALL DPWRST('XXX','BUG ')
 1992   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  OPERATE ON EACH COMPONENT OF THE VECTOR IR(.).    **
C               **  CONVERT THE NUMBERS TO FLOATING POINT VALUES.     **
C               **  CONVERT THE PARAMATERS TO FLOATING POINT VALUES.  **
C               **  SET THE X TO AN DUMMY VALUE OF 0.0 FOR THE TIME BEING.
C               **  CONVERT THE OPERATIONS INTO A 1-WORD REPRESENTATION.
C               **  'CONVERT' THE PARENTHESES INTO A 1-WORD REPRESENTATION.
C               **  CONVERT THE COEFFICIENTS TO COEFFICIENT VALUES.   **
C               **  CONVERT THE LIBRARY FUNCTIONS INTO A 1-WORD REPRESENTATION.
C               **  SAVE THE CONTENTS OF ITYPE, IW21, IW22, AND W2 IN **
C               **  ITYPEH, IW21HO, IW22HO, AND WHOLD FOR LATER USE   **
C               **  IN REDEFINING ITYPE, IW21, IW22, AND W2 FOR EACH NEW X VALUE
C               **  OUTPUT THE VECTORS IW21, IW22 AND W2.             **
C               **  OUTPUT THE VECTORS IW21HO, IW22HO, W2HOLD, AND ITYPEH.
C               ********************************************************
C
      DO3000I=1,NW
        ICMIN=IBEGIN(I)
        ICMAX=IEND(I)
        IF(ITYPE(I).EQ.'N')THEN
          W2(I)=0.0
          IANS1='    '
          IANS2='    '
          IANS3='    '
          IANS4='    '
          J=0
          DO3150IC=ICMIN,ICMAX
            J=J+1
            JM1=J-1
            L=J-(NUMASC*(JM1/NUMASC))
            K=NUMBPC*(L-1)
            K=IABS(K)
            IF(J.LE.NUMASC)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS1)
            ELSEIF(J.LE.NUMAS2)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS2)
            ELSEIF(J.LE.NUMAS3)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS3)
            ELSEIF(J.LE.NUMAS4)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS4)
            ENDIF
 3150     CONTINUE
          ERRMAX=10.0**9
          ERRMIN=-ERRMAX
          CALL ERRORF(IANS1,IANS2,IANS3,IANS4,ERRMIN,ERRMAX,
     1                ERRMAX,ANS2,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          W2(I)=ANS2
          GOTO3000
        ELSEIF(ITYPE(I).EQ.'X')THEN
          W2(I)=0.0
          GOTO3000
        ELSEIF(ITYPE(I).EQ.'OP')THEN
          IW21(I)=IR(ICMIN)
          ICMINP=ICMIN+1
          IF(IR(ICMIN).EQ.'*'.AND.IR(ICMINP).EQ.'*')IW21(I)='**'
          GOTO3000
        ELSEIF(ITYPE(I).EQ.'LP'.OR.ITYPE(I).EQ.'RP')THEN
          IW21(I)=IR(ICMIN)
          GOTO3000
        ELSEIF(ITYPE(I).EQ.'PAR')THEN
          IW21(I)='    '
          IW22(I)='    '
          ICMAX2=ICMIN+NUMAS2-1
          IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
          J=0
          DO3550IC=ICMIN,ICMAX2
            J=J+1
            J2=J
            IF(J2.GT.NUMASC)J2=J-NUMASC
            ISTAR3=NUMBPC*(J2-1)
            ISTAR3=IABS(ISTAR3)
            IF(J.LE.NUMASC)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
            ELSE
              CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
            ENDIF
 3550     CONTINUE
C
          IF(IPASS.EQ.1)GOTO3000
C
          DO3570J=1,NUMPAR
            IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))THEN
              W2(I)=PARAM(J)
              GOTO3000
            ENDIF
 3570     CONTINUE
          WRITE(ICOUT,21)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3571)
 3571     FORMAT('      NO MATCH FOR PARAMETER/VARIABLE NAME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3572)IW21(I),IW22(I)
 3572     FORMAT('               GIVEN PARAMETER/VARIABLE NAME = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3573)NUMPAR
 3573     FORMAT('               NUMBER OF PARAMETER/VARIABLE =',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3574)
 3574     FORMAT('               ADMISSIBLE PARAMETER/VARIABLE ',
     1           'NAMES = ')
          CALL DPWRST('XXX','BUG ')
          DO3575J=1,NUMPAR
            WRITE(ICOUT,3576)J,IPARN1(J),IPARN2(J)
 3576       FORMAT('               PARAMETER/VARIABLE NAME ',I4,'--',
     1             2A4)
            CALL DPWRST('XXX','BUG ')
 3575     CONTINUE
          WRITE(ICOUT,3577)(IA(J),J=1,MIN(100,NUMCHA))
 3577     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(ITYPE(I).EQ.'LF')THEN
          IW21(I)='    '
          IW22(I)='    '
          ICMAX2=ICMIN+NUMAS2-1
          IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
          J=0
          DO3650IC=ICMIN,ICMAX2
            J=J+1
            J2=J
            IF(J2.GT.NUMASC)J2=J-NUMASC
            ISTAR3=NUMBPC*(J2-1)
            ISTAR3=IABS(ISTAR3)
            IF(J.LE.NUMASC)THEN
              CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
            ELSE
              CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
            ENDIF
 3650     CONTINUE
          GOTO3000
        ELSEIF(ITYPE(I).EQ.'COM')THEN
          IW21(I)=IR(ICMIN)
          GOTO3000
        ENDIF
C
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3005)
 3005   FORMAT('      ITYPE(I) NOT X, OP, LP, PAR, OR LF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3006)I,ITYPE(I),IW21(I),W2(I)
 3006   FORMAT('I,ITYPE(I),IW21(I),W2(I) = ',I8,2(2X,A4),2X,F15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
 3000 CONTINUE
C
      NWHOLD=NW
      DO3900I=1,NW
        ITYPEH(I)=ITYPE(I)
        IW21HO(I)=IW21(I)
        IW22HO(I)=IW22(I)
        W2HOLD(I)=W2(I)
 3900 CONTINUE
C
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='5'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        DO3992I=1,NW
          ICMIN=IBEGIN(I)
          ICMINP=ICMIN+1
          ICMINQ=ICMIN+2
          WRITE(ICOUT,3993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),
     1                     IW21(I),IW22(I),W2(I)
 3993     FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),W21(I),',
     1           'IW22HO(I),W2(I) = ',I8,2X,3A4,3(2X,A4),2X,F15.6)
          CALL DPWRST('XXX','BUG ')
 3992   CONTINUE
      ENDIF
C
C               ****************************************************
C               **  STEP 6--                                      **
C               **  THIS STEP IS TO BE EXECUTED IF IPASS=1;       **
C               **  OTHERWISE IT IS SKIPPED.                      **
C               **  IF THIS STEP IS EXECUTED, STEP 7 IS NOT;      **
C               **  IF THIS STEP IS NOT EXECUTED, STEP 7 IS.      **
C               **  OPERATE ON IW21, IW22, AND ITYPE VECTORS.      **
C               **  DETERMINE THE NUMBER OF DISTINCT PARAMETERS.  **
C               **  FORM THE OUTPUT VECTOR IPARN.                 **
C               ****************************************************
C
      IF(IPASS.EQ.1)THEN
C
        NUMPAR=0
        DO4100I=1,NW
          IF(ITYPE(I).NE.'PAR')GOTO4100
          IF(NUMPAR.GT.0)THEN
            DO4400J=1,NUMPAR
              IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GOTO4100
 4400       CONTINUE
          ENDIF
          NUMPAR=NUMPAR+1
          IPARN1(NUMPAR)=IW21(I)
          IPARN2(NUMPAR)=IW22(I)
 4100   CONTINUE
        GOTO9000
      ENDIF
C
C               *********************************************************
C               **  STEP 7-- C                                         **
C               **  OPERATE ON THE W2(.), IW21(.), AND IW22(.) VECTORS.**
C               **  THIS STEP IS NOT EXECUTED IF STEP 6 IS; THIS STEP  **
C               **  IS EXECUTED IF STEP 6 IS NOT.  FIRST MAKE SURE THAT**
C               **  THE NUMBER OF LEFT AND RIGHT PARENTHESES ARE THE   **
C               **  SAME.  (STEP 6 THEN SETS UP A LARGE DO LOOP WHICH  **
C               **  GOES THROUGH ALL OF THE VALUES OF THE X VECTOR AND **
C               **  GENERATES CORRESPONDING VALUES OF THE Y VECTOR.)   **
C               **  FOR A GIVEN X VALUE, IT EVALUATES THE FUNCTION BY  **
C               **  FIRST SEEKING THE INNERMOST PARENTHESES (BY        **
C               **  SEARCHING FOR THE FIRST REMAINING RIGHT PARENTHESS).*
C               **  AND THEN EVALUATING ALL SUCH PARENTHETICAL         **
C               **  EXPRESSIONS--WORKING FROM THE INNERMOST OUT. AFTER **
C               **  EVALUATING A PARENTHESES PAIR, THE ENTIRE          **
C               **  PARENTHESES GROUP (PARENTHESES INCLUDED) IS        **
C               **  REPLACED BY THE SCALAR ANSWER.  THE IW21, IW22,    **
C               **  W2, AND ITYPE VECTORS ARE SQUEEZED ACCORDING (IN   **
C               **  THE SUBROUTINE EVAL).  SINCE THE VECTORS IW21,     **
C               **  IW22, W2, AND ITYPE ARE ALTERED FOR EACH X VALUE,  **
C               **  THEY MUST BE REDEFINED FROM THE SAVED VALUES IN    **
C               **  IW21HO, IW22HO, W2HOLD, AND ITYPEH FOR EACH NEW X  **
C               **  THE ABOVE SQUEEZING OPERATION IS REPEATED FOR EACH **
C               **  PARENTHESES PAIR UNTIL ALL PARENTHESES ARE GONE    **
C               **  AND WE REMAIN ONLY WITH THE FINAL ANSWER.  FOR     **
C               **  EACH VALUE X(.) OF THE INPUT X VECTOR, OUTPUT THE  **
C               **  CORRESPONDING VALUE Y(.) OF THE DESIRED OUTPUT     **
C               **  VECTOR.  FOR A GIVEN VALUE X(.), THE CORRESPONDING **
C               **  COMPUTED Y(.) WILL BE THE EVALUATED VALUE OF THE   **
C               **  RIGHT-HAND SIDE OF THE SPECIFIED EQUATION Y = F(X).**
C               *********************************************************
C
 5000 CONTINUE
C 
      NW=NWHOLD
      DO5050I=1,NW
        ITYPE(I)=ITYPEH(I)
        IW21(I)=IW21HO(I)
        IW22(I)=IW22HO(I)
        W2(I)=W2HOLD(I)
 5050 CONTINUE
C
      DO5060I=1,NW
        IF(ITYPE(I).EQ.'PAR')THEN
          IF(NUMPAR.GT.0)THEN
            DO5070J=1,NUMPAR
              J2=J
              IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))THEN
                W2(I)=PARAM(J2)
                GOTO5060
              ENDIF
 5070       CONTINUE
          ENDIF
C
          WRITE(ICOUT,21)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5071)
 5071     FORMAT('      NO MATCH FOR PARAMETER/VARIABLE NAME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5072)IW21(I),IW22(I)
 5072     FORMAT('      GIVEN PARAMETER/VARIABLE NAME = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5073)NUMPAR
 5073     FORMAT('      NUMBER OF PARAMETERS/VARIABLES =',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5074)
 5074     FORMAT('      ADMISSIBLE PARAMETER/VARIABLE NAMES = ')
          CALL DPWRST('XXX','BUG ')
          DO5075J=1,NUMPAR
            WRITE(ICOUT,5076)J,IPARN1(J),IPARN2(J)
 5076       FORMAT('      PARAMETER/VARIABLE NAME ',I3,'--',2A4)
            CALL DPWRST('XXX','BUG ')
 5075     CONTINUE
          WRITE(ICOUT,5077)(IA(J),J=1,MIN(100,NUMCHA))
 5077     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(ITYPE(I).EQ.'N'  .OR. ITYPE(I).EQ.'X'  .OR.
     1         ITYPE(I).EQ.'OP' .OR. ITYPE(I).EQ.'LP' .OR.
     1         ITYPE(I).EQ.'RP' .OR. ITYPE(I).EQ.'LF' .OR.
     1         ITYPE(I).EQ.'COM')THEN
          GOTO5060
        ELSE
          WRITE(ICOUT,21)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5061)
 5061     FORMAT('      ITYPE(I) NOT X, OP, LP, PAR, OR LF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5062)I,ITYPE(I),IW21(I),IW22(I),W2(I)
 5062     FORMAT('I,ITYPE(I),IW21(I),IW22(I),W2(I) = ',
     1           I8,3(2X,A4),2X,F15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 5060 CONTINUE
C
      NLP=0
      NRP=0
      DO5100I=1,NW
        IF(ITYPE(I).EQ.'LP')NLP=NLP+1
        IF(ITYPE(I).EQ.'RP')NRP=NRP+1
 5100 CONTINUE
C
      IF(NLP.NE.NRP)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5156)
 5156   FORMAT('      NUMBER OF LEFT PARENTHESES NOT EQUAL TO ',
     1         'NUMBER OF RIGHT PARENTHESES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5157)NLP
 5157   FORMAT('      NUMBER OF LEFT  PARENTHESES = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5158)NRP
 5158   FORMAT('      NUMBER OF RIGHT PARENTHESES = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
CCCCC ADD FOLLOWING LINES APRIL 1995.
CCCCC 2010/12: INITIALIZE TO CPUMIN RATHER THAN -99.9.
CCCCC          NEED TO MODIFY DPLIB1, DPLIB2, DPLIB3 TO
CCCCC          CHECK FOR CPUMIN RATHER THAN -99.9.
C
      ILIBC1=0
      ILIBC2=0
      DO5195IJ=1,MAXNST
        SAVE1(IJ)=CPUMIN
        SAVE2(IJ)=CPUMIN
        SAVE3(IJ)=CPUMIN
        SAVE4(IJ)=CPUMIN
        SAVE5(IJ)=CPUMIN
        SAVE6(IJ)=CPUMIN
        SAVE7(IJ)=CPUMIN
        SAVE8(IJ)=CPUMIN
 5195 CONTINUE
C
      DO10000II=1,N
C
        IF(II.GT.1)THEN
          NW=NWHOLD
          DO5200I=1,NW
            ITYPE(I)=ITYPEH(I)
            IW21(I)=IW21HO(I)
            IW22(I)=IW22HO(I)
            W2(I)=W2HOLD(I)
 5200     CONTINUE
        ENDIF
C
        IF(IBUGCO.EQ.'ON')THEN
          ISTEPN='7'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          DO5250I=1,NW
            WRITE(ICOUT,5251)I,IW21HO(I),IW22HO(I),IW21(I),IW22(I)
 5251       FORMAT('I,IW21HO(I),IW22HO(I),IW21(I),IW22(I) = ',
     1             I8,4(2X,A4))
            CALL DPWRST('XXX','BUG ')
 5250     CONTINUE
        ENDIF
C
        ILOOP=1
 5350   CONTINUE
        DO5400I=1,NW
          I2=I
          IF(ITYPE(I).EQ.'RP')THEN
            ISTOP=I2
            DO5600JJ=1,ISTOP
              IREV=ISTOP-JJ+1
              IF(ITYPE(IREV).EQ.'LP')THEN
                ISTART=IREV
                GOTO5690
              ENDIF
 5600       CONTINUE
            WRITE(ICOUT,21)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5605)
 5605       FORMAT('      ITYPE(IREV) NOT LP')
            CALL DPWRST('XXX','BUG ')
            ISTART=IREV
            GOTO5690
          ENDIF
 5400   CONTINUE
        ISTOP=NW+1
        ISTART=0
 5690   CONTINUE
C
        ISTAP1=ISTART+1
        ISTOM1=ISTOP-1
        IJUNK=ISTART-1
        IF(IJUNK.GE.1)THEN
          IF(ITYPE(IJUNK).EQ.'LF')ILIBC1=ILIBC1+1
        ENDIF
        CALL EVALM(IW21,IW22,W2,ITYPE,ISTAP1,ISTOM1,IANGLU,Y,
     1             SAVE1,SAVE2,SAVE3,SAVE4,SAVE5,SAVE6,SAVE7,SAVE8,
     1             ILIBC1,ILIBC2,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(ISTART.GT.0)THEN
          W2(ISTART)=Y
          ITYPE(ISTART)='V'
          IF(NW.GT.1)THEN
            ISTOPP=ISTOP+1
            J=ISTART
            IF(ISTOP.NE.NW)THEN
              DO5700I=ISTOPP,NW
                J=J+1
                IW21(J)=IW21(I)
                IW22(J)=IW22(I)
                W2(J)=W2(I)
                ITYPE(J)=ITYPE(I)
 5700         CONTINUE
            ENDIF
            NW=J
            GOTO5350
          ENDIF
        ENDIF
10000 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGCO.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COMPIM--')
        CALL DPWRST('XXX','BUG ')
        DO9113I=1,MAXNST
          WRITE(ICOUT,9013)I,SAVE1(I),SAVE2(I),SAVE3(I),SAVE4(I),Y
 9013     FORMAT('I,SAVE1,SAVE2,SAVE3,SAVE4,Y = ',I3,5E15.7)
          CALL DPWRST('XXX','BUG ')
 9113   CONTINUE
        WRITE(ICOUT,9014)NUMCHA,N,IPASS,NW,IANGLU
 9014   FORMAT('NUMCHA,N,IPASS,NW,IANGLU = ',4I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NW.GE.1)THEN
          WRITE(ICOUT,9022)ITYPE(NW)
 9022     FORMAT('ITYPE(NW) = ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION CONDIT( N, SYMIN )
*
*     Computes condition number of symmetric matix in situ
*
      INTEGER NL, N
      PARAMETER ( NL = 100 )
      DOUBLE PRECISION DET, SYMIN(*), SUM, ROWMX, ROWMXI,
     & SYM(NL*(NL+1)/2)
      INTEGER II, IJ, I, J, IM
      ROWMX = 0
      IJ = 0
      DO 100 I = 1,N
         SUM = 0
         IM = (I-2)*(I-1)/2
         DO 200 J = 1,I-1
            IM = IM + 1
            SUM = SUM + ABS(SYMIN(IM))
            IJ = IJ + 1
            SYM(IJ) = SYMIN(IM)
  200    CONTINUE
         SUM = SUM + 1
         IJ = IJ + 1
         SYM(IJ) = 1
         IM = IM + I
         DO 300 J = I,N-1
            SUM = SUM + ABS(SYMIN(IM))
            IM = IM + J
  300    CONTINUE
         ROWMX = MAX( SUM, ROWMX )
  100 CONTINUE
      CALL SYMINV(N, SYM, DET)
      ROWMXI = 0
      II = 0
      DO 400 I = 1,N
         SUM = 0
         IJ = II
         DO 500 J = 1,I
            IJ = IJ + 1
            SUM = SUM + ABS(SYM(IJ))
 500     CONTINUE
         DO 600 J = I,N-1
            IJ = IJ + J
            SUM = SUM + ABS(SYM(IJ))
 600     CONTINUE
         ROWMXI = MAX( SUM, ROWMXI )
         II = II + I
 400  CONTINUE
      CONDIT = ROWMX*ROWMXI
C
      RETURN
      END
      SUBROUTINE CONINS(X,Y,NPT,XX,YY,NPTC)
C
C     PURPOSE--INCORPORATE AN INTERIOR CLOSED CONTOUR SEGMENT
C              INTO ANOTHER SEGMENT
C
C     RECOMMENDED DIMENSIONS--
C        X(NPT+NPTC+1)
C        Y(NPT+NPTC+1)
C        XX(NPTC)
C        YY(NPTC)
C        LC(4)
C
C     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
C                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
C     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
C
C---------------------------------------------------------------------
C
CCCCC DIMENSION X(NPT+NPTC+1),Y(NPT+NPTC+1),XX(NPTC),YY(NPTC),LC(4)
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XX(*)
      DIMENSION YY(*)
C
      DIMENSION LC(4)
C
C-----START POINT-----------------------------------------------------
C
C   FIRST FIND UP, DOWN, LEFT & RIGHT EXTREMES OF AN INTERIOR SEGMENT
      DO 10 I=1,4
        LC(I)=1
 10   CONTINUE
      DO 20 L=1,NPTC
        IF (XX(L).LT.XX(LC(1))) LC(1)=L
        IF (YY(L).GT.YY(LC(2))) LC(2)=L
        IF (XX(L).GT.XX(LC(3))) LC(3)=L
        IF (YY(L).LT.YY(LC(4))) LC(4)=L
 20   CONTINUE
C   FIND A REASONABLY CLOSE APPROACH OF INTERIOR SEGMENT TO THE CONTINUOUS
C                        STRING
      L1=LC(1)
      L0=1
      DMN=SQRT((XX(L1)-X(L0))**2+(YY(L1)-Y(L0))**2)
      DO 100 L=1,NPT
        DO 200 I=1,4
          LL=LC(I)
          DTST=SQRT((XX(LL)-X(L))**2+(YY(LL)-Y(L))**2)
          IF (DTST.LT.DMN) THEN
            DMN=DTST
            L0=L
            L1=LL
          END IF
 200    CONTINUE
 100  CONTINUE
C   REORDER THE INTERIOR SEGMENT
      DO 300 L=1,L1-1
        HX=XX(1)
        HY=YY(1)
        DO 400 LL=2,NPTC-1
          XX(LL-1)=XX(LL)
          YY(LL-1)=YY(LL)
 400    CONTINUE
        XX(NPTC-1)=HX
        YY(NPTC-1)=HY
 300  CONTINUE
      XX(NPTC)=XX(1)
      YY(NPTC)=YY(1)
C   INSERT THE INTERIOR SEGMENT INTO THE CONTINUOUS STRING
      DO 500 L=NPT,L0,-1
        X(L+1)=X(L)
        Y(L+1)=Y(L)
 500  CONTINUE
      NPT=NPT+1
      L0=L0+1
      L2=NPT+1
      L3=NPTC+L2
      NPT=L3-1
      DO 600 L=L2,NPT
        LL=L-L2+1
        X(L)=XX(LL)
        Y(L)=YY(LL)
 600  CONTINUE
      CALL STRSWP(X,L0,L2,L3)
      CALL STRSWP(Y,L0,L2,L3)
      RETURN
      END
      SUBROUTINE CONCDF(DX,DSHAPE,DM,ICONDF,DCDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
C              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
C              DEFINED FOR ALL INTEGER X >= 1.
C
C              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
C              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
C              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
C              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
C              SIMILAR FORM TO THE GEETA DISTRIBUTION.
C
C              THE PROBABILITY MASS FUNCTION IS:
C              p(X;THETA,M)=
C                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  0 < THETA < 1; 1 <= M < 1/THETA
C
C              A RECURRENCE RELATION FOR THE CDF FUNCTION IS
C
C                  P(X;THETA,M) = {(M-1)*(X-1)+1}/(X-1)}*
C                                 THETA*(1-TYHETA)**(M-1)*
C                                 PROD[i=1 to X-2][(1 + M/(M*X-M-i)]*
C                                 P(X-1;THETA,M)
C
C              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
C              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
C              THE PROBABILITY MASS FUNCTION:
C              p(X;MU,M)=
C                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
C                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  MU >= 1; M > 1
C              NOTE THAT THE RELATION IS:
C
C                  THETA=(MU-1)/(M*MU)
C             
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE A NON-NEGATIVE INTEGER.
C                     --DSHAPE = THE FIRST SHAPE PARAMETER
C                                (EITHER THETA OR MU)
C                     --DM     = THE SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION FUNCTION
C             VALUE CDF FOR THE CONSUL DISTRIBUTION WITH SHAPE
C             PARAMETERS THETA (OR MU) AND M
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
C                 --0 < THETA < 1; 1 < M < 1/THETA
C                 --MU >= 1; M > 1
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/8
C     ORIGINAL VERSION--AUGUST    2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPDFSV
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DSUM
C
      CHARACTER*4 ICONDF
      CHARACTER*4 ICOND2
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
        DTHETA=(DMU-1.0D0)/(DM*DMU)
      ENDIF
C
      IX=INT(DX+0.5D0)
      IF(IX.LT.1)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO CONCDF IS LESS ',
     1'THAN 1')
C
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GOTO9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONCDF IS NOT ',
     1         'IN THE INTERVAL (0,1)')
C
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GOTO9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONCDF IS NOT ',
     1         'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GOTO9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONCDF IS ',
     1         'LESS THAN 1')
C
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GOTO9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONCDF IS ',
     1         'LESS THAN 1')
      ENDIF
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DCDF=(1.0D0 - DTHETA)**DM
      IF(IX.EQ.1)THEN
        GOTO9000
      ELSE
        DX=2.0D0
        ICOND2='THET'
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(IX.EQ.2)GOTO9000
        DX=3.0D0
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(IX.EQ.3)GOTO9000
        DPDFSV=DPDF
      ENDIF
C
      DO100I=4,IX
        DX=DBLE(I)
        DTERM1=DLOG(DTHETA) + (DM-1.0D0)*DLOG(1.0D0 - DTHETA)
        DTERM2=DLOG((DM-1.0D0)*(DX-1.0D0) + 1.0D0) - DLOG(DX-1.0D0)
        DTERM3=DTERM1 + DTERM2
        DSUM=0.0D0
        DO200J=1,I-2
          DSUM=DSUM + DLOG(1.0D0 + DM/(DM*DX - DM - DBLE(J)))
  200   CONTINUE
        IF(DPDFSV.GT.0.0D0)THEN
          DPDF=DEXP(DTERM3 + DSUM + DLOG(DPDFSV))
        ELSE
          GOTO9000
        ENDIF
        DCDF=DCDF + DPDF
        DPDFSV=DPDF
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE CONFOU(ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--CREATE STRINGS FOR CONFOUNDING FOR CERTAIN
C              TWO-LEVEL DESIGNS.
C     EXAMPLE--LET CON COP = CONFOUND  N K
C
C              BASED ON VALUES OF N AND K, A NUMBER OF STRINGS
C              STARTING WITH "CON" AND "COP" WILL BE CREATED.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/01
C     ORIGINAL VERSION--JANUARY   2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*8 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*8 ISTR1
      CHARACTER*8 ISTR2
      CHARACTER*8   ISTRZ1
      CHARACTER*16  ISTRZ2
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CONF'
      ISUBN2='OU  '
      IERROR='NO'
C
      N=-1
      K=-1
      ILOC3=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CONFOU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,NUMNAM
   52   FORMAT('IBUGA3,ISUBRO,NUMNAM = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),
     1                   IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',
     1           'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)NUMCHF,MAXCHF
   57   FORMAT('NUMCHF,MAXCHF = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(120,MAXCHF))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  ERROR CHECKING--EXACTLY 6   **
C               **  AGUMENTS REQUIRED.          **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.NE.6)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN CONFOUND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      EXACTLY SIX ARGUMENTS EXPECTED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NUMARG
  105   FORMAT('      ',I3,' ARGUMENTS GIVEN.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **********************************
C               **  STEP 2--                    **
C               **  EXTRACT VALUES FOR N AND K  **
C               **  FROM RIGHT HAND SIDE.       **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     N AND K CAN EITHER BE PREVIOUSLY DEFINED PARAMETERS OR
C     SIMPLY NUMBERS.  ANY OTHER TYPE IS AN ERROR.
C
      IHRIGH=IHARG(5)
      IHRIG2=IHARG2(5)
C
      DO210I=1,NUMNAM
        IF(IHRIGH(1:4).EQ.IHNAME(I)(1:4) .AND.
     1     IHRIG2(1:4).EQ.IHNAM2(I)(1:4))THEN
            AK=VALUE(I)
            GOTO219
        ENDIF
  210 CONTINUE
C
C     NAME NOT FOUND.  CHECK IF ARGUMENT IS A NUMBER.
C
      IF(IARGT(5).EQ.'NUMB')THEN
        AK=ARG(5)
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)
  203   FORMAT('      THE NUMBER OF FACTORS ARGUMENT WAS NOT FOUND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)
  205   FORMAT('      AS EITHER A PARAMETER OR A NUMBER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,207)IHARG(5),IHARG2(5)
  207   FORMAT('      THE ARGUMENT IS: ',2A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
  219 CONTINUE
C
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
C
      DO260I=1,NUMNAM
        IF(IHRIGH(1:4).EQ.IHNAME(I)(1:4) .AND.
     1     IHRIG2(1:4).EQ.IHNAM2(I)(1:4))THEN
            AN=VALUE(I)
            GOTO269
        ENDIF
  260 CONTINUE
C
C     NAME NOT FOUND.  CHECK IF ARGUMENT IS A NUMBER.
C
      IF(IARGT(6).EQ.'NUMB')THEN
        AN=ARG(6)
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,263)
  263   FORMAT('      THE SAMPLE SIZE ARGUMENT WAS NOT FOUND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,265)
  265   FORMAT('      AS EITHER A PARAMETER OR A NUMBER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,267)IHARG(6),IHARG2(6)
  267   FORMAT('      THE ARGUMENT IS: ',2A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  269 CONTINUE
C
      IF(AK.GT.AN)THEN
        AKSAV=AK
        AK=AN
        AN=AKSAV
      ENDIF
C
      K=INT(AK+0.1)
      NTEMP=INT(AN+0.1)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,291)K,NTEMP
  291   FORMAT('K,NTEMP = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  EXTRACT THE BASE NAMES ON THE LHS OF THE   **
C               **  EQUAL SIGN AND THEN LOOP THROUGH THE       **
C               **  NUMBER OF STRINGS TO CREATE.               **
C               *************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT(1:4)=IHARG(1)
      IHLEFT(5:8)=IHARG2(1)
      NBASE1=1
      DO310I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE1=I
          GOTO319
        ENDIF
  310 CONTINUE
  319 CONTINUE
C
      ISTR1=' '
      IF(NBASE1.LE.4)THEN
        ISTR1(1:NBASE1)=IHLEFT(1:NBASE1)
      ELSE
        ISTR1(1:4)=IHLEFT(1:4)
        NCHR=NBASE1-5+1
        ISTR1(5:NBASE1)=IHLEF2(1:NCHR)
      ENDIF
C
      IHLEFT(1:4)=IHARG(2)
      IHLEFT(5:8)=IHARG2(2)
      NBASE2=1
      DO360I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE2=I
          GOTO369
        ENDIF
  360 CONTINUE
  369 CONTINUE
C
      ISTR2=' '
      IF(NBASE2.LE.4)THEN
        ISTR2(1:NBASE2)=IHLEFT(1:NBASE2)
      ELSE
        ISTR2(1:4)=IHLEFT(1:4)
        NCHR=NBASE2-5+1
        ISTR2(5:NBASE2)=IHLEF2(1:NCHR)
      ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,391)NBASE1,NBASE2,ISTR1,ISTR2
  391   FORMAT('NBASE1,NBASE2,ISTR1,ISTR2 = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 4--                    **
C               **  STEP THROUGH THE SUPPORTED  **
C               **  K/N COMBINATIONS AND CREATE **
C               **  THE STRINGS.                **
C               **********************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NTEMP.EQ.4)THEN
        IF(K.EQ.2)THEN
C
C         K = 2, N = 4 (2**2)
C
C           CON1   = 1
C           CON2   = 2
C           CON12  = 12
C
C           COP1   = 1
C           COP2   = 2
C           COP12  = 12
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.6)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.6)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO8020
C
        ELSEIF(K.EQ.3)THEN
C
C         K = 3, N = 4 (2**(3-1)
C
C           CON1   = 1
C           CON2   = 2
C           CON12  = 3
C
C           COP1   = 1+23
C           COP2   = 2+13
C           COP12  = 3+12
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.6)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.6)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:4)='1+23'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:4)='2+13'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:4)='3+12'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSE
          IERROR='YES'
          GOTO8030
        ENDIF
      ELSEIF(NTEMP.EQ.8)THEN
        IF(K.EQ.3)THEN
C
C         K = 3, N = 8 (2**3)
C
C           CON1   = 1
C           CON2   = 2
C           CON3   = 3
C           CON12  = 12
C           CON13  = 13
C           CON23  = 23
C           CON123 = 123
C
C           COP1   = 1
C           COP2   = 2
C           COP3   = 3
C           COP12  = 12
C           COP13  = 13
C           COP23  = 23
C           COP123 = 123
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:2)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.4)THEN
C
C         K = 4, N = 8 (2**(4-1))
C
C           CON1   = 1
C           CON2   = 2
C           CON3   = 3
C           CON12  = 12
C           CON13  = 13
C           CON23  = 14
C           CON123 = 4
C
C           COP1   = 1
C           COP2   = 2
C           COP3   = 3
C           COP12  = 12+34
C           COP13  = 13+24
C           COP23  = 14+23
C           COP123 = 4
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:2)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+34'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+24'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='14+23'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.5)THEN
C
C         K = 5, N = 8 (2**(5-2))
C
C           CON1   = 1
C           CON2   = 2
C           CON3   = 3
C           CON12  = 4
C           CON13  = 5
C           CON23  = 23
C           CON123 = 25
C
C           COP1   = 1+24+35
C           COP2   = 2+14
C           COP3   = 3+15
C           COP12  = 4+12
C           COP13  = 5+13
C           COP23  = 23+45
C           COP123 = 25+34
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:7)='1+24+35'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:4)='2+14'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:4)='3+15'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:4)='4+12'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:4)='5+13'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='23+45'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:5)='25+34'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.6)THEN
C
C         K = 6, N = 8 (2**(6-3))
C
C           CON1   = 1
C           CON2   = 2
C           CON3   = 3
C           CON12  = 4
C           CON13  = 5
C           CON23  = 6
C           CON123 = 16
C
C           COP1   = 1+24+35
C           COP2   = 2+14+36
C           COP3   = 3+15+26
C           COP12  = 4+12+56
C           COP13  = 5+13+46
C           COP23  = 6+23+45
C           COP123 = 16+25+34
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:7)='1+24+35'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:7)='2+14+36'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:7)='3+15+26'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:7)='4+12+56'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:7)='5+13+46'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:7)='6+23+45'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:8)='16+25+34'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.7)THEN
C
C         K = 7, N = 8 (2**(7-4))
C
C           CON1   = 1
C           CON2   = 2
C           CON3   = 3
C           CON12  = 4
C           CON13  = 5
C           CON23  = 6
C           CON123 = 7
C
C           COP1   = 1+24+35+67
C           COP2   = 2+14+36+57
C           COP3   = 3+15+26+47
C           COP12  = 4+12+56+37
C           COP13  = 5+13+46+17
C           COP23  = 6+23+45+17
C           COP123 = 7+16+25+34
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:10)='1+24+35+67'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:10)='2+14+36+57'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:10)='3+15+26+47'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:10)='4+12+56+37'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:10)='5+13+46+17'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:10)='6+23+45+17'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:10)='7+16+25+34'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSE
          IERROR='YES'
          GOTO8030
        ENDIF
      ELSEIF(NTEMP.EQ.16)THEN
        IF(K.EQ.4)THEN
C
C         K = 4, N = 16 (2**4)
C
C           CON1    = 1
C           CON2    = 2
C           CON3    = 3
C           CON4    = 4
C           CON12   = 12
C           CON13   = 13
C           CON14   = 14
C           CON23   = 23
C           CON24   = 24
C           CON34   = 34
C           CON123  = 123
C           CON124  = 124
C           CON134  = 134
C           CON234  = 234
C           CON1234 = 1234
C
C           COP1    = 1
C           COP2    = 2
C           COP3    = 3
C           COP4    = 4
C           COP12   = 12
C           COP13   = 13
C           COP14   = 14
C           COP23   = 23
C           COP24   = 24
C           COP34   = 34
C           COP123  = 123
C           COP124  = 124
C           COP134  = 134
C           COP234  = 234
C           COP1234 = 1234
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.5)THEN
C
C         K = 5, N = 16 (2**(5-1))
C
C           CON1    = 1
C           CON2    = 2
C           CON3    = 3
C           CON4    = 4
C           CON12   = 12
C           CON13   = 13
C           CON14   = 14
C           CON23   = 23
C           CON24   = 24
C           CON34   = 34
C           CON123  = 45
C           CON124  = 35
C           CON134  = 25
C           CON234  = 15
C           CON1234 = 5
C
C           COP1    = 1
C           COP2    = 2
C           COP3    = 3
C           COP4    = 4
C           COP12   = 12
C           COP13   = 13
C           COP14   = 14
C           COP23   = 23
C           COP24   = 24
C           COP34   = 34
C           COP123  = 45
C           COP124  = 35
C           COP134  = 25
C           COP234  = 15
C           COP1234 = 5
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.6)THEN
C
C         K = 6, N = 16 (2**(6-2))
C
C           CON1    = 1
C           CON2    = 2
C           CON3    = 3
C           CON4    = 4
C           CON12   = 12
C           CON13   = 13
C           CON14   = 14
C           CON23   = 23
C           CON24   = 24
C           CON34   = 34
C           CON123  = 5
C           CON124  = 124
C           CON134  = 134
C           CON234  = 6
C           CON1234 = 16
C
C           COP1    = 1
C           COP2    = 2
C           COP3    = 3
C           COP4    = 4
C           COP12   = 12+35
C           COP13   = 13+25
C           COP14   = 14+56
C           COP23   = 23+15+46
C           COP24   = 24+36
C           COP34   = 34+26
C           COP123  = 5
C           COP124  = 124
C           COP134  = 134
C           COP234  = 6
C           COP1234 = 45
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+35'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+25'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:5)='14+56'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:8)='23+15+46'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:5)='24+36'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:5)='34+26'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.7)THEN
C
C         K = 7, N = 16 (2**(7-3))
C
C           CON1    = 1
C           CON2    = 2
C           CON3    = 3
C           CON4    = 4
C           CON12   = 12
C           CON13   = 13
C           CON14   = 14
C           CON23   = 23
C           CON24   = 24
C           CON34   = 34
C           CON123  = 7
C           CON124  = 124
C           CON134  = 6
C           CON234  = 5
C           CON1234 = 15
C
C           COP1    = 1
C           COP2    = 2
C           COP3    = 3
C           COP4    = 4
C           COP12   = 12+37+56
C           COP13   = 13+27+46
C           COP14   = 14+36+57
C           COP23   = 15+26+47
C           COP24   = 16+25+34
C           COP34   = 17+23+45
C           COP123  = 7
C           COP124  = 124
C           COP134  = 6
C           COP234  = 5
C           COP1234 = 15+26+47
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:8)='12+37+56'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:8)='13+27+46'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:8)='14+36+57'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:8)='15+26+47'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:8)='16+25+34'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:8)='17+23+45'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:8)='15+26+47'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.8)THEN
C
C         K = 8, N = 16 (2**(8-4))
C
C           CON1    = 1
C           CON2    = 2
C           CON3    = 3
C           CON4    = 4
C           CON12   = 12
C           CON13   = 13
C           CON14   = 14
C           CON23   = 23
C           CON24   = 24
C           CON34   = 34
C           CON123  = 7
C           CON124  = 8
C           CON134  = 6
C           CON234  = 5
C           CON1234 = 15
C
C           COP1    = 1
C           COP2    = 2
C           COP3    = 3
C           COP4    = 4
C           COP12   = 12+37+48+56
C           COP13   = 13+27+46+58
C           COP14   = 14+28+36+57
C           COP23   = 23+17+45+68
C           COP24   = 24+18+35+67
C           COP34   = 34+16+25+78
C           COP123  = 7
C           COP124  = 8
C           COP134  = 6
C           COP234  = 5
C           COP1234 = 15+26+38+47
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:11)='12+37+48+56'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:11)='13+27+46+58'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:11)='14+28+36+57'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:11)='23+17+45+68'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:11)='24+18+35+67'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:11)='34+16+25+78'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:11)='15+26+38+47'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSE
          IERROR='YES'
          GOTO8030
        ENDIF
      ELSEIF(NTEMP.EQ.32)THEN
        IF(K.EQ.5)THEN
C
C         K = 5, N = 32 (2**5)
C
C           CON1     = 1
C           CON2     = 2
C           CON3     = 3
C           CON4     = 4
C           CON5     = 5
C           CON12    = 12
C           CON13    = 13
C           CON14    = 14
C           CON15    = 15
C           CON23    = 23
C           CON24    = 24
C           CON25    = 25
C           CON34    = 34
C           CON35    = 35
C           CON45    = 45
C           CON123   = 123
C           CON124   = 124
C           CON125   = 125
C           CON134   = 134
C           CON135   = 135
C           CON145   = 145
C           CON234   = 234
C           CON235   = 235
C           CON245   = 245
C           CON345   = 345
C           CON1234  = 1234
C           CON1235  = 1235
C           CON1245  = 1245
C           CON1345  = 1345
C           CON2345  = 2345
C           CON12345 = 12345
C
C           COP1     = 1
C           COP2     = 2
C           COP3     = 3
C           COP4     = 4
C           COP5     = 5
C           COP12    = 12
C           COP13    = 13
C           COP14    = 14
C           COP15    = 15
C           COP23    = 23
C           COP24    = 24
C           COP25    = 25
C           COP34    = 34
C           COP35    = 35
C           COP45    = 45
C           COP123   = 123
C           COP124   = 124
C           COP125   = 125
C           COP134   = 134
C           COP135   = 135
C           COP145   = 145
C           COP234   = 234
C           COP235   = 235
C           COP245   = 245
C           COP345   = 345
C           COP1234  = 1234
C           COP1235  = 1235
C           COP1245  = 1245
C           COP1345  = 1345
C           COP2345  = 2345
C           COP12345 = 12345
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:4)='1235'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:4)='1245'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:4)='1345'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:4)='2345'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:5)='12345'
          NCHAR=5
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:4)='1235'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:4)='1245'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:4)='1345'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:4)='2345'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:5)='12345'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.6)THEN
C
C         K = 6, N = 32 (2**(6-1))
C
C           CON1     = 1
C           CON2     = 2
C           CON3     = 3
C           CON4     = 4
C           CON5     = 5
C           CON12    = 12
C           CON13    = 13
C           CON14    = 14
C           CON15    = 15
C           CON23    = 23
C           CON24    = 24
C           CON25    = 25
C           CON34    = 34
C           CON35    = 35
C           CON45    = 45
C           CON123   = 123
C           CON124   = 124
C           CON125   = 125
C           CON134   = 134
C           CON135   = 135
C           CON145   = 145
C           CON234   = 234
C           CON235   = 235
C           CON245   = 245
C           CON345   = 345
C           CON1234  = 56
C           CON1235  = 46
C           CON1245  = 36
C           CON1345  = 26
C           CON2345  = 16
C           CON12345 = 6
C
C           COP1     = 1
C           COP2     = 2
C           COP3     = 3
C           COP4     = 4
C           COP5     = 5
C           COP12    = 12
C           COP13    = 13
C           COP14    = 14
C           COP15    = 15
C           COP23    = 23
C           COP24    = 24
C           COP25    = 25
C           COP34    = 34
C           COP35    = 35
C           COP45    = 45
C           COP123   = 123
C           COP124   = 124
C           COP125   = 125
C           COP134   = 134
C           COP135   = 135
C           COP145   = 145
C           COP234   = 234
C           COP235   = 235
C           COP245   = 245
C           COP345   = 345
C           COP1234  = 56
C           COP1235  = 46
C           COP1245  = 36
C           COP1345  = 26
C           COP2345  = 16
C           COP12345 = 6
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.7)THEN
C
C         K = 7, N = 32 (2**(7-2))
C
C           CON1     = 1
C           CON2     = 2
C           CON3     = 3
C           CON4     = 4
C           CON5     = 5
C           CON12    = 12
C           CON13    = 13
C           CON14    = 14
C           CON15    = 15
C           CON23    = 23
C           CON24    = 24
C           CON25    = 25
C           CON34    = 34
C           CON35    = 35
C           CON45    = 45
C           CON123   = 46
C           CON124   = 36
C           CON125   = 47
C           CON134   = 26
C           CON135   = 135
C           CON145   = 27
C           CON234   = 16
C           CON235   = 235
C           CON245   = 17
C           CON345   = 345
C           CON1234  = 6
C           CON1235  = 456
C           CON1245  = 7
C           CON1345  = 256
C           CON2345  = 156
C           CON12345 = 56
C
C           COP1     = 1
C           COP2     = 2
C           COP3     = 3
C           COP4     = 4
C           COP5     = 5
C           COP12    = 12
C           COP13    = 13
C           COP14    = 14
C           COP15    = 15
C           COP23    = 23
C           COP24    = 24
C           COP25    = 25
C           COP34    = 34
C           COP35    = 35+67
C           COP45    = 45
C           COP123   = 46
C           COP124   = 36+57
C           COP125   = 47
C           COP134   = 26
C           COP135   = 135
C           COP145   = 27
C           COP234   = 16
C           COP235   = 235
C           COP245   = 17
C           COP345   = 345
C           COP1234  = 6
C           COP1235  = 456
C           COP1245  = 7
C           COP1345  = 256
C           COP2345  = 156
C           COP12345 = 56+37
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:2)='47'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:3)='456'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:3)='256'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:3)='156'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:5)='35+67'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:5)='36+57'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:2)='47'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:3)='456'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:3)='256'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:3)='156'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:5)='56+37'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(K.EQ.10)THEN
C
C         K = 10, N = 32 (2**(10-5))
C
C           CON1     = 1
C           CON2     = 2
C           CON3     = 3
C           CON4     = 4
C           CON5     = 5
C           CON12    = 12
C           CON13    = 13
C           CON14    = 14
C           CON15    = 15
C           CON23    = 23
C           CON24    = 24
C           CON25    = 25
C           CON34    = 34
C           CON35    = 35
C           CON45    = 45
C           CON123   = 46
C           CON124   = 36
C           CON125   = 37
C           CON134   = 26
C           CON135   = 27
C           CON145   = 28
C           CON234   = 16
C           CON235   = 17
C           CON245   = 18
C           CON345   = 19
C           CON1234  = 6
C           CON1235  = 7
C           CON1245  = 8
C           CON1345  = 9
C           CON2345  = 0
C           CON12345 = 10
C
C           COP1     = 1
C           COP2     = 2
C           COP3     = 3
C           COP4     = 4
C           COP5     = 5
C           COP12    = 12+90
C           COP13    = 13+80
C           COP14    = 14+70
C           COP15    = 15+60
C           COP23    = 23+89
C           COP24    = 24+79
C           COP25    = 25+69
C           COP34    = 34+78
C           COP35    = 35+68
C           COP45    = 45+67
C           COP123   = 46+57
C           COP124   = 36+58
C           COP125   = 37+48
C           COP134   = 26+59
C           COP135   = 27+49
C           COP145   = 28+39
C           COP234   = 16+50
C           COP235   = 17+40
C           COP245   = 18+30
C           COP345   = 19+20
C           COP1234  = 6
C           COP1235  = 7
C           COP1245  = 8
C           COP1345  = 9
C           COP2345  = 0
C           COP12345 = 56+47+38+29+10
C
C         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
C
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GOTO8010
          ENDIF
C
C         NOW CREATE THE STRINGS
C
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:2)='37'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:2)='28'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:2)='18'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:2)='19'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:1)='9'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:1)='0'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:2)='10'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+90'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+80'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:5)='14+70'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:5)='15+60'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='23+89'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:5)='24+79'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:5)='25+69'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:5)='34+78'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:5)='35+68'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:5)='45+67'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:5)='46+57'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:5)='36+58'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:5)='37+48'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:5)='26+59'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:5)='27+49'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:5)='28+39'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:5)='16+50'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:5)='17+40'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:5)='18+30'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:5)='19+20'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:1)='9'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:1)='0'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:14)='56+47+38+29+10'
          NCHAR=14
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
        ELSE
          IERROR='YES'
          GOTO8030
        ENDIF
      ELSE
        IERROR='YES'
        GOTO8030
      ENDIF
C
C               *****************************************************
C               **  STEP 7--                                       **
C               **  PRINT FEEDBACK MESSAGE                         **
C               *****************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'ON' .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,710)
  710   FORMAT(I5,' THE CONFOUNDING STRINGS HAVE BEEN CREATED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               *****************************************************
C               **  STEP 8--                                       **
C               **  PRINT ERROR MESSAGES                           **
C               *****************************************************
C
 8010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8011)
 8011 FORMAT('       STRING BASE TOO LONG FOR SPECIFIED N AND K.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8013)ISTR1
 8013 FORMAT('       BASE FOR FIRST SET OF STRINGS IS ',A8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8015)ISTR2
 8015 FORMAT('       BASE FOR SECOND SET OF STRINGS IS ',A8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8033)K
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8035)NTEMP
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 8020 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8021)
 8021 FORMAT('       ERROR IN CREATING THE STRINGS.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 8030 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8031)
 8031 FORMAT('       CONFOUND NOT SPECIFIED FOR GIVEN K AND N.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8033)K
 8033 FORMAT('       THE VALUE OF K IS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8035)NTEMP
 8035 FORMAT('       THE VALUE OF N IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CONFOU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMNAM
 9013   FORMAT('NUMNAM,IVALUE = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',
     1           'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CONFO2(ISTRIN,ISTRZZ,NCHAR,ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--UTILITY ROUTINE FOR "CONFOU".  THIS ROUTINE
C              UPDATES A SINGLE STRING IN THE INTERNAL STRING
C              TABLE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/01
C     ORIGINAL VERSION--JANUARY   2015.
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*8 ISTRIN
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEL
      CHARACTER*8 IHLEFT
      CHARACTER*4 IHLEF2
C
      CHARACTER*(*) ISTRZZ
      CHARACTER*4  ISTRZ2(40)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CONF'
      ISUBN2='O2  '
      IERROR='NO'
C
      N=-1
      K=-1
      ILOC3=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CONFO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,NUMNAM
   52   FORMAT('IBUGA3,ISUBRO,NUMNAM = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),
     1                   IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',
     1           'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)NUMCHF,MAXCHF
   57   FORMAT('NUMCHF,MAXCHF = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(120,MAXCHF))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 5--                                         *
C               **  EXAMINE THE CURRENT STRING--                     *
C               **  IF THIS IS A PREVIOUSLY DEFINED NAME, IT SHOULD  *
C               **  BE A STRING    (IF NOT, REPORT AN ERROR).        *
C               ******************************************************
C
        ISTEPN='5'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFO2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DO510II=1,NUMNAM
          I2=II
          IF(ISTRIN(1:4).EQ.IHNAME(I2).AND.
     1       ISTRIN(5:8).EQ.IHNAM2(I2))THEN
            IF(IUSE(I2).EQ.'F')THEN
              ICASEL='STRI'
              ILISTL=I2
              GOTO519
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,511)
  511         FORMAT('****** ERROR IN CONFO2--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,513)ISTRIN
  513         FORMAT('      THE NAME ',A8,' ALREADY EXISTS, BUT NOT ',
     1               'AS A STRING.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,515)
  515         FORMAT('      THIS STRING WILL NOT BE CREATED.')
              CALL DPWRST('XXX','BUG ')
              GOTO9000
            ENDIF
          ENDIF
  510   CONTINUE
C
        NEWNAM='YES'
        ICASEL='STRI'
C
        ILISTL=NUMNAM+1
        IF(ILISTL.GT.MAXNAM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,522)
  522     FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',
     1           'FUNCTION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,524)MAXNAM
  524     FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
  519   CONTINUE
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  ADD THE CURRENT STRING                         **
C               *****************************************************
C
        ISTEPN='6'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFO2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IHLEFT=ISTRIN(1:4)
        IHLEF2=ISTRIN(5:8)
        DO411J=1,NCHAR
          ISTRZ2(J)=' '
          ISTRZ2(J)(1:1)=ISTRZZ(J:J)
  411   CONTINUE
C
        CALL DPINFU(ISTRZ2,NCHAR,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,
     1              NEWNAM,MAXNME,
     1              IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
  410 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CONFO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMNAM
 9013   FORMAT('NUMNAM,IVALUE = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',
     1           'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION CONFUN(DM)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE FUNCTIONS FOR THE
C              CONSUL MEAN AND ONES FREQUENCY EQUATION.
C
C              THE MEAN AND ONES FREQUENCY ESTIMATE OF MU IS:
C
C                  MUHAT = XBAR
C
C              THE ESTIMATE OF M IS THEN THE SOLUTION OF THE
C              EQUATION
C
C                 M*LOG(1 - (XBAR-1)/(M*XBAR)) - LOG(N1/N) = 0
C
C              CALLED BY DFZERO ROUTINE FOR SOLVING A NONLINEAR
C              UNIVARIATE EQUATION.
C     EXAMPLE--CONSUL MAXIMUM LIKELIHOOD Y
C     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/8
C     ORIGINAL VERSION--AUGUST    2006.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DM
C
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION S2
      DOUBLE PRECISION F1FREQ
      COMMON/CONCOM/XBAR,S2,F1FREQ,MAXROW,N
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CONFUN=DM*DLOG(1.0D0 - (XBAR-1.0D0)/(DM*XBAR)) - DLOG(F1FREQ)
C
      RETURN
      END
      SUBROUTINE CONFU2(N,XPAR,FVEC,IFLAG,Y,K)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE FUNCTIONS FOR THE
C              CONSUL MAXIMUM LIKELIHOOD EQUATION.
C
C              THE MAXIMUM LIKELIHOOD FREQUENCY ESTIMATE OF MU IS:
C
C                  MUHAT = XBAR
C
C              THE ESTIMATE OF M IS THEN THE SOLUTION OF THE
C              EQUATION
C
C                 LOG(1 - (XBAR-1)/(M*XBAR)) + (1/(N*XBAR))*
C                 SUM[X=2 to k][SUM[i=0 to X-2][X*N(x)/(M*X-i)]] = 0
C
C              THIS ROUTINE ASSUMES THE DATA IS IN THE FORM
C
C                   X(I)  FREQ(I)
C
C              CALLED BY DNSQE ROUTINE FOR SOLVING SIMULTANEOUS
C              NONLINEAR EQUATIONS.  NOTE THAT THE CALLING SEQUENCE
C              DID NOT ACCOMODATE A DATA ARRAY (AND ASSCIATED NUMBER OF
C              OBSERVATIONS), SO THESE WERE ADDED TO THE CALL LIST.
C              SINCE DNSQE ONLY PASSES ONE ARRAY, WE SPLIT INTO
C              TWO PARTS: 1 - MAXNXT/2 ARE THE FREQUENCIES WHILE
C              (MAXNXT/2 + 1) - MAXNXT ARE THE CLASS VALUES (I.E.,
C              THE X).
C     EXAMPLE--CONSUL MAXIMUM LIKELIHOOD Y
C     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/8
C     ORIGINAL VERSION--AUGUST    2006.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION XPAR(*)
      DOUBLE PRECISION FVEC(*)
      REAL Y(*)
C
      DOUBLE PRECISION DM
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DFREQ
C
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION S2
      DOUBLE PRECISION F1FREQ
      COMMON/CONCOM/XBAR,S2,F1FREQ,MAXROW,NTOT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      DM=XPAR(1)
      DN=DBLE(NTOT)
      IINDX=MAXROW/2
C
      DTERM1=(DM*XBAR - XBAR + 1.0D0)/(DM*XBAR)
      DTERM2=1.0D0/(DN*XBAR)
C
      DSUM1=0.0D0
      DO100I=2,K
        DX=DBLE(Y(IINDX+I))
        DFREQ=Y(I)
        DO200J=0,I-2
          DSUM1=DSUM1 + DX*DFREQ/(DM*DX - DBLE(J))
  200   CONTINUE
  100 CONTINUE
C
      DTERM3=DTERM2*DSUM1
      FVEC(1)=DTERM1 - DEXP(-DTERM3)
CCCCC FVEC(1)=DTERM1 + DTERM2*DSUM1
C
      RETURN
      END
      SUBROUTINE CONPDF(DX,DSHAPE,DM,ICONDF,DPDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY MASS
C              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
C              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
C              DEFINED FOR ALL INTEGER X >= 1.
C
C              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
C              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
C              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
C              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
C              SIMILAR FORM TO THE GEETA DISTRIBUTION.
C
C              THE PROBABILITY MASS FUNCTION IS:
C              p(X;THETA,M)=
C                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  0 < THETA < 1; 1 <= M < 1/THETA
C
C              THE MEAN AND VARIANCE ARE:
C
C                  MEAN     = 1/(1-THETA*M)
C                  VARIANCE = M*THETA*(1-THETA)/
C                             (1-THETA*M)**3
C
C              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
C              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
C              THE PROBABILITY MASS FUNCTION:
C              p(X;MU,M)=
C                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
C                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  MU >= 1; M > 1
C              NOTE THAT THE RELATION IS:
C
C                  THETA=(MU-1)/(M*MU)
C             
C              THE MEAN AND VARIANCE BECOME:
C
C                  MEAN     = MU
C                  VARIANCE = MU*(MU-1)*(M*MU-MU+1)/M
C
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY MASS
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE A NON-NEGATIVE INTEGER.
C                     --DSHAPE = THE FIRST SHAPE PARAMETER
C                                (EITHER THETA OR MU)
C                     --DM     = THE SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--DPDF   = THE DOUBLE PRECISION PROBABILITY MASS
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY MASS FUNCTION VALUE
C             PDF FOR THE CONSUL DISTRIBUTION WITH SHAPE PARAMETERS
C             THETA (OR MU) AND M
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
C                 --0 < THETA < 1; 1 < M < 1/THETA
C                 --MU >= 1; M > 1
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/8
C     ORIGINAL VERSION--AUGUST    2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DPDF
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DLNGAM
C
      CHARACTER*4 ICONDF
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
      ENDIF
C
      IX=INT(DX+0.5D0)
      IF(IX.LT.1)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        DPDF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO CONPDF IS LESS ',
     1'THAN 1')
C
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GOTO9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPDF IS NOT ',
     1         'IN THE INTERVAL (0,1)')
C
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GOTO9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPDF IS NOT ',
     1         'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GOTO9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPDF IS ',
     1         'LESS THAN 1')
C
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GOTO9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPDF IS ',
     1         'LESS THAN 1')
      ENDIF
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DX=DBLE(IX)
C
      IF(ICONDF.EQ.'THET')THEN
        DTERM1=DLNGAM(DM*DX+1.0D0) + (DX-1.0D0)*DLOG(DTHETA) +
     1         (DM*DX-DX+1.0D0)*DLOG(1.0D0 - DTHETA)
        DTERM2=DLNGAM(DX) + DLNGAM(DM*DX-DX+2.0D0)
        DTERM3=DLOG(DX)
        DTERM4=DTERM1 - DTERM2 - DTERM3
        DPDF=DEXP(DTERM4)
      ELSE
        DTERM1=-DLOG(DX)
        DTERM2=DLNGAM(DM*DX+1.0D0)
        DTERM3=-DLNGAM(DX) - DLNGAM(DM*DX-DX+2.0D0)
        DTERM4=(DX-1.0D0)*(DLOG(DMU-1.0D0) - DLOG(DM) - DLOG(DMU))
        DTERM5=(DM*DX-DX+1.0D0)*DLOG(1.0D0 - (DMU-1.0D0)/(DM*DMU))
        DTERM6=DTERM1 + DTERM2 + DTERM3 + DTERM4 + DTERM5
        DPDF=DEXP(DTERM6)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE CONPPF(DP,DSHAPE,DM,ICONDF,DPPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
C              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
C              DEFINED FOR ALL INTEGER X >= 1.
C
C              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
C              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
C              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
C              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
C              SIMILAR FORM TO THE GEETA DISTRIBUTION.
C
C              THE PROBABILITY MASS FUNCTION IS:
C              p(X;THETA,M)=
C                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  0 < THETA < 1; 1 <= M < 1/THETA
C
C              A RECURRENCE RELATION FOR THE CDF FUNCTION IS
C
C                  P(X;THETA,M) = {(M-1)*(X-1)+1}/(X-1)}*
C                                 THETA*(1-TYHETA)**(M-1)*
C                                 PROD[i=1 to X-2][(1 + M/(M*X-M-i)]*
C                                 P(X-1;THETA,M)
C
C              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
C              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
C              THE PROBABILITY MASS FUNCTION:
C              p(X;MU,M)=
C                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
C                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  MU >= 1; M > 1
C              NOTE THAT THE RELATION IS:
C
C                  THETA=(MU-1)/(M*MU)
C             
C              THE PERCENT POINT FUNCTION IS COMPUTED BY SUMMING
C              THE CUMULATIVE DISTRIBUTION UNTIL THE APPROPRIATE
C              PROBABILITY IS REACHED.
C
C     INPUT  ARGUMENTS--DP     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --DSHAPE = THE FIRST SHAPE PARAMETER
C                                (EITHER THETA OR MU)
C                     --DM     = THE SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--DPPF   = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
C             VALUE PPF FOR THE CONSUL DISTRIBUTION WITH SHAPE
C             PARAMETERS THETA (OR MU) AND M
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
C                 --0 < THETA < 1; 1 < M < 1/THETA
C                 --MU >= 1; M > 1
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/8
C     ORIGINAL VERSION--AUGUST    2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DP
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPDFSV
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEPS
C
      CHARACTER*4 ICONDF
      CHARACTER*4 ICOND2
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
        DTHETA=(DMU-1.0D0)/(DM*DMU)
      ENDIF
C
      IF(DP.LT.0.0D0 .OR. DP.GE.1.0D0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DP
        CALL DPWRST('XXX','BUG ')
        DPPF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO GETPPF IS OUTSIDE ',
     1'THE (0,1] INTERVAL')
C
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GOTO9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPPF IS NOT ',
     1         'IN THE INTERVAL (0,1)')
C
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GOTO9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPPF IS NOT ',
     1         'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GOTO9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPPF IS ',
     1         'LESS THAN 1')
C
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GOTO9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPPF IS ',
     1         'LESS THAN 1')
      ENDIF
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DEPS=1.0D-7
      DCDF=(1.0D0 - DTHETA)**DM
      IF(DCDF.GE.DP-DEPS)THEN
        DPPF=1.0D0
        GOTO9000
      ELSE
        DX=2.0D0
        ICOND2='THET'
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=2.0D0
          GOTO9000
        ENDIF
        DX=3.0D0
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=3.0D0
          GOTO9000
        ENDIF
        DPDFSV=DPDF
      ENDIF
C
      I=3
  100 CONTINUE
        I=I+1
        DX=DBLE(I)
        DTERM1=DLOG(DTHETA) + (DM-1.0D0)*DLOG(1.0D0 - DTHETA)
        DTERM2=DLOG((DM-1.0D0)*(DX-1.0D0) + 1.0D0) - DLOG(DX-1.0D0)
        DTERM3=DTERM1 + DTERM2
        DSUM=0.0D0
        DO200J=1,I-2
          DSUM=DSUM + DLOG(1.0D0 + DM/(DM*DX - DM - DBLE(J)))
  200   CONTINUE
        IF(DPDFSV.GT.0.0D0)THEN
          DPDF=DEXP(DTERM3 + DSUM + DLOG(DPDFSV))
        ELSE
          DPPF=DBLE(I)
          GOTO9000
        ENDIF
        DCDF=DCDF + DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=DBLE(I)
          GOTO9000
        ENDIF
        DPDFSV=DPDF
      GOTO100
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE CONRAN(N,SHAPE,AM,ICONDF,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE CONSUL DISTRIBUTION WITH SHAPE PARAMETERS
C              THETA OR MU AND AM.
C
C              THE PROBABILITY MASS FUNCTION IS:
C              p(X;THETA,M)=
C                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  0 < THETA < 1; 1 <= M < 1/THETA
C
C              THE MEAN AND VARIANCE ARE:
C
C                  MEAN     = 1/(1-THETA*M)
C                  VARIANCE = M*THETA*(1-THETA)/
C                             (1-THETA*M)**3
C
C              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
C              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
C              THE PROBABILITY MASS FUNCTION:
C              p(X;MU,M)=
C                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
C                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
C                  X = 1, 2, 3, ,...
C                  MU >= 1; M > 1
C              NOTE THAT THE RELATION IS:
C
C                  THETA=(MU-1)/(M*MU)
C             
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --SHAPE  = THE SINGLE PRECISION VALUE
C                                OF THE FIRST SHAPE PARAMETER.
C                     --AM     = THE SINGLE PRECISION VALUE
C                                OF THE SECOND SHAPE PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE CONSUL
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE CONSUL DISTRIBUTION
C             WITH SHAPE PARAMETERS THETA (OR MU) AND AM.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --0 < THETA < 1, 1 < M < 1/THETA
C                   MU >= 1; M > 1
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, CONPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CONSUL (1990), "CONSUL DISTRIBUTION AND ITS
C                 PROPERTIES", COMMUNICATIONS IN STATISTICS--
C                 THEORY AND METHODS, 19, PP. 3051-3068.
C               --CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
C                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/7
C     ORIGINAL VERSION--JULY      2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
      CHARACTER*4 ICONDF
C
      DOUBLE PRECISION DPPF
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF CONSUL RANDOM ',
     1       'NUMBERS IS NON-POSITIVE')
C
      IF(ICONDF.EQ.'THET')THEN
        THETA=SHAPE
        IF(THETA.LE.0.0 .OR. THETA.GE.1.0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)THETA
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
   15   FORMAT('***** ERROR--THE THETA PARAMETER FOR THE CONSUL')
   16   FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (0,1) INTERVAL')
C
        IF(AM.LT.1.0 .OR. AM.GE.1.0/THETA)THEN
          WRITE(ICOUT,25)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,26)1.0/THETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AM
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
   25   FORMAT('***** ERROR--THE M PARAMETER FOR THE CONSUL')
   26   FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (1,',G15.7,') ',
     1         'INTERVAL')
      ELSE
        AMU=SHAPE
        IF(AMU.LT.1.0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,36)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AMU
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
   35   FORMAT('***** ERROR--THE MU PARAMETER FOR THE CONSUL')
   36   FORMAT('      RANDOM NUMBERS IS LESS THAN 1')
C
        IF(AM.LE.1.0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,39)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AM
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
   38   FORMAT('***** ERROR--THE M PARAMETER FOR THE CONSUL')
   39   FORMAT('      RANDOM NUMBERS IS LESS THAN OR EQUAL TO 1')
      ENDIF
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C     GENERATE N CONSUL DISTRIBUTION RANDOM NUMBERS USING THE
C     INVERSION METHOD.
C
      CALL UNIRAN(N,ISEED,X)
      DO100I=1,N
        XTEMP=X(I)
        CALL CONPPF(DBLE(XTEMP),DBLE(SHAPE),DBLE(AM),ICONDF,DPPF)
        X(I)=REAL(DPPF)
  100 CONTINUE
C
 9000 CONTINUE
C
      RETURN
      END
      SUBROUTINE CONV14(ISTRIN,NSTRIN,IA,IB,IWIDTH,IBUGXX,IERROR)
C     PURPOSE--CONVERT THE FIRST NSTRIN CHARACTERS IF ISTRIN
C              TO THE FIRST CHARACTERS OF THE CHARACTER*4 ARRAYS
C              IA AND IB.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93.3
C     ORIGINAL VERSION--FEBRUARY 1993
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*80 ISTRIN
      CHARACTER*4 IA
      CHARACTER*4 IB
      CHARACTER*4 IBUGXX
      CHARACTER*4 IERROR
C
      CHARACTER*4 IC4
C
C---------------------------------------------------------------------
C
      DIMENSION IA(80)
      DIMENSION IB(80)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGXX.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CONV14--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGXX,IERROR
   52 FORMAT('IBUGXX,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISTRIN(1:80)
   53 FORMAT('ISTRIN(1:80) = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NSTRIN
   54 FORMAT('NSTRIN = ',I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IWIDTH=NSTRIN
      IF(1.LE.NSTRIN.AND.NSTRIN.LE.80)THEN
         DO1000I=1,NSTRIN
         IC4='    '
         IC4(1:1)=ISTRIN(I:I)
         IA(I)=IC4
         IB(I)=IC4
 1000    CONTINUE
         IERROR='NO'
      ELSE
         IERROR='YES'
      ENDIF
C
      IF(IBUGXX.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CONV14--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGXX,IERROR
 9012 FORMAT('IBUGXX,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ISTRIN(1:80)
 9013 FORMAT('ISTRIN(1:80) = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NSTRIN,IWIDTH
 9014 FORMAT('NSTRIN,IWIDTH = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
         DO9020I=1,IWIDTH
         WRITE(ICOUT,9021)I,IA(I),IB(I)
 9021    FORMAT('I,IA(I),IB(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9020    CONTINUE
      ENDIF
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CONVOL(Y1,N1,Y2,N2,NUMVAR,IWRITE,MAXN,
     1Y3,N3,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CONVOLUTION OF 2 VARIABLES.
C     NOTE--IF  THE FIRST  VARIABLE IS Y1(.)
C           AND THE SECOND VARIABLE IS Y2(.),
C           THEN THE OUTPUT VARIABLE CONTAINING THE
C           CONVOLUTION
C           WILL BE COMPUTED AS FOLLOWS--
C              Y3(1) = Y1(1)*Y2(1)
C              Y3(2) = Y1(1)*Y2(2) + Y1(2)*Y2(1)
C              Y3(3) = Y1(1)*Y2(3) + Y1(2)*Y2(2) + Y1(3)*Y2(1)
C              ETC.
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y3(.)
C           BEING IDENTICAL (OVERLAYED) ON THE INPUT VECTORS Y1(.) OR Y2(.)
C     NOTE--Y1 AND Y2 NEED NOT BE THE SAME LENGTH.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CONV'
      ISUBN2='OL  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CONVOL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N1,N2,NUMVAR,MAXN
   53 FORMAT('N1,N2,NUMVAR,MAXN = ',4I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N1
      WRITE(ICOUT,56)I,Y1(I)
   56 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      DO57I=1,N2
      WRITE(ICOUT,58)I,Y2(I)
   58 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   57 CONTINUE
   90 CONTINUE
C
C               *******************************
C               **  COMPUTE THE CONVOLUTION  **
C               *******************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LE.0)GOTO150
      IF(N2.LE.0)GOTO150
      I3MIN=2
      I3MAX=N1+N2
      N3=I3MAX-I3MIN+1
      IF(N3.GT.MAXN)GOTO170
C
      DO100I3=1,N3
      Y3(I3)=0.0
  100 CONTINUE
C
      DO500I1=1,N1
      DO600I2=1,N2
      Y1P=Y1(I1)
      Y2P=Y2(I2)
      Y3P=Y1P*Y2P
      IARG=I1+I2-1
      Y3(IARG)=Y3(IARG)+Y3P
  600 CONTINUE
  500 CONTINUE
      GOTO190
C
  150 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,151)
  151 FORMAT('***** ERROR IN CONVOL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)
  152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,153)
  153 FORMAT('      IN THE VARIABLES FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,154)
  154 FORMAT('      THE CONVOLUTION IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,155)
  155 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,156)
  156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,157)N1,N2
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',2I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO190
C
  170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,171)
  171 FORMAT('***** ERROR IN CONVOL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,172)
  172 FORMAT('      THE NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      IN THE RESULTING CONVOLUTION VARIABLE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,175)MAXN
  175 FORMAT('      MUST BE LESS THAN OR EQUAL TO ',I8,' .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,176)
  176 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,177)N3
  177 FORMAT('      THE OUTPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO190
C
  190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CONVOL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N1,N2,NUMVAR,MAXN,N3
 9013 FORMAT('N1,N2,NUMVAR,MAXN,N3 = ',5I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N3
      WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I)
 9016 FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CORMAT(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
C              CORRECT MATCHES BETWEEN TWO VARIABLES.  THE
C              NUMBER OF CORRECT MATCHES IS THE SUM OF THE
C              TRUE POSITIVES AND TRUE NEGATIVES.
C
C              THIS IS SPECIFICALLY FOR THE 2X2 CASE.  THAT IS,
C              EACH VARIABLE HAS TWO MUTUALLY EXCLUSIVE
C              CHOICES CODED AS 1 (FOR SUCCESS) OR 0 (FOR
C              FAILURE).  A TRUE POSITIVE IS DEFINED AS THE
C              CASE WHERE THE SECOND VARIABLE IS 1 AND THE FIRST
C              VARIABLE IS A 1.
C
C              A TYPICAL EXAMPLE WOULD BE WHERE VARIABLE ONE
C              DENOTES THE GROUND TRUTH AND A VALUE OF 1
C              INDICATES "PRESENT" AND A VALUE OF 0 INDICATES
C              "NOT PRESENT".  VARIABLE TWO REPRESENTS SOME TYPE
C              OF DETECTION DEVICE WHERE A VALUE OF 1 INDICATES
C              THE DEVICE DETECTED THE SPECIFIED OBJECT WHILE A
C              VALUE OF 0 INDICATES THAT THE OBJECT WAS NOT
C              DETECTED.  A TRUE POSITIVE THEN IS THE CASE WHERE
C              THE DEVICE DETECTED THE OBJECT WHEN IT WAS
C              ACTUALY THERE.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE FIRST SET
C                                OF DATA.
C                     --Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE SECOND SET
C                                OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X, OR EQUIVALENTLY,
C                                THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED TRUE POSITIVE PROPORTION
C                                BETWEEN THE 2 SETS OF DATA
C                                IN THE INPUT VECTORS X AND Y.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TRUE POSITIVE PROPORTION BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/5
C     ORIGINAL VERSION--MAY       2007.
C     UPDATED         --AUGUST    2007. IF 2X2 CASE, CHECK IF SUM
C                                       OF ENTRIES IS <= 4.  IN THIS
C                                       CASE, ASSUME WE HAVE RAW DATA
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='TRUP'
      ISUBN2='OS  '
C
      IERROR='NO'
C
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CORMAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N
   53   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR IN THE CORRECT MATCH PROPORTION')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLES IS LESS THAN TWO')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1205)N
 1205   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 22--                             **
C               **  CHECK THAT THE VARIABLES HAVE AT MOST **
C               **  TWO DISTINCT VALUES (1 INDICATES A    **
C               **  SUCCESS, 0 INDICATES A FAILURE).      **
C               ********************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOTE: CHECK FOR SPECIAL CASE N = 2.  IN THIS CASE,
C           ASSUME WE HAVE A 2X2 TABLE OF COUNTS INSTEAD
C           OF RAW DATA.
C
      IF(N.EQ.2)THEN
        N11=INT(X(1)+0.5)
        N21=INT(X(2)+0.5)
        N12=INT(Y(1)+0.5)
        N22=INT(Y(2)+0.5)
C
C       CHECK IF ALL ENTRIES 0 OR 1. IF SO, ASSUME
C       RAW DATA CASE.
C
        IF((N11.EQ.0 .OR. N11.EQ.1) .AND.
     1     (N12.EQ.0 .OR. N12.EQ.1) .AND.
     1     (N21.EQ.0 .OR. N21.EQ.1) .AND.
     1     (N22.EQ.0 .OR. N22.EQ.1)) GOTO1349
C
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1311)
 1311     FORMAT('      ROW 1 COLUMN 1 OF THE COUNTS TABLE IS ',
     1           'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1321)
 1321     FORMAT('      ROW 2 COLUMN 1 OF THE COUNTS TABLE IS ',
     1           'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1331)
 1331     FORMAT('      ROW 1 COLUMN 2 OF THE COUNTS TABLE IS ',
     1           'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
 1341     FORMAT('      ROW 2 COLUMN 2 OF THE COUNTS TABLE IS ',
     1           'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        NTEMP=N11 + N12 + N21 + N22
        STAT=REAL(N11 + N22)/REAL(NTEMP)
        GOTO3000
      ENDIF
C
 1349 CONTINUE
C
      CALL DISTIN(X,N,IWRITE,XIDTEM,NDIST,IBUGA3,IERROR)
      IF(NDIST.EQ.1)THEN
        AVAL=XIDTEM(1)
        IF(ABS(AVAL).LE.0.5)THEN
          AVAL=0.0
        ELSE
          AVAL=1.0
        ENDIF
        DO2202I=1,N
          X(I)=1.0
 2202   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        IF(XIDTEM(1).EQ.1.0 .OR. XIDTEM(2).EQ.1.0)THEN
          DO2203I=1,N
            IF(X(I).NE.1.0)X(I)=0.0
 2203     CONTINUE
        ELSE
          ATEMP1=MIN(XIDTEM(1),XIDTEM(2))
          ATEMP2=MAX(XIDTEM(1),XIDTEM(2))
          DO2208I=1,N1
            IF(X(I).EQ.ATEMP1)X(I)=0.0
            IF(X(I).EQ.ATEMP2)X(I)=1.0
 2208     CONTINUE
        ENDIF
      ELSEIF(NDIST.GT.2)THEN
        N11=0
        N12=0
        N21=0
        DO2510I=1,N
          IF(Y(I).EQ.X(I))THEN
            N11=N11+1
          ELSEIF(Y(I).LT.X(I))THEN
            N12=N12+1
          ELSEIF(Y(I).GT.X(I))THEN
            N21=N21+1
          ENDIF
 2510   CONTINUE
        STAT=REAL(N11)/REAL(N)
        GOTO9000
      ELSE
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1201)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2211)
C2211   FORMAT('      RESPONSE VARIABLE ONE SHOULD CONTAIN AT MOST')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2213)
C2213   FORMAT('      TWO DISTINCT VALUES.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2215)NDIST
C2215   FORMAT('      ',I8,' DISTINCT VALUES FOUND.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
      ENDIF
C
      CALL DISTIN(Y,N,IWRITE,XIDTEM,NDIST,IBUGA3,IERROR)
      IF(NDIST.EQ.1)THEN
        AVAL=XIDTEM(1)
        IF(ABS(AVAL).LE.0.5)THEN
          AVAL=0.0
        ELSE
          AVAL=1.0
        ENDIF
        DO2302I=1,N
          Y(I)=1.0
 2302   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        IF(XIDTEM(1).EQ.1.0 .OR. XIDTEM(2).EQ.1.0)THEN
          DO2303I=1,N
            IF(Y(I).NE.1.0)Y(I)=0.0
 2303     CONTINUE
        ELSE
          ATEMP1=MIN(XIDTEM(1),XIDTEM(2))
          ATEMP2=MAX(XIDTEM(1),XIDTEM(2))
          DO2308I=1,N
            IF(Y(I).EQ.ATEMP1)Y(I)=0.0
            IF(Y(I).EQ.ATEMP2)Y(I)=1.0
 2308     CONTINUE
        ENDIF
      ELSEIF(NDIST.GT.2)THEN
        N11=0
        N12=0
        N21=0
        DO2520I=1,N
          IF(Y(I).EQ.X(I))THEN
            N11=N11+1
          ELSEIF(Y(I).LT.X(I))THEN
            N12=N12+1
          ELSEIF(Y(I).GT.X(I))THEN
            N21=N21+1
          ENDIF
 2520   CONTINUE
        STAT=REAL(N11)/REAL(N)
        GOTO9000
      ELSE
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1201)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2311)
C2311   FORMAT('      RESPONSE VARIABLE TWO SHOULD CONTAIN AT MOST')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2313)
C2313   FORMAT('      TWO DISTINCT VALUES.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2315)NDIST
C2315   FORMAT('      ',I8,' DISTINCT VALUES FOUND.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
      ENDIF
C
      N11=0
      N12=0
      N21=0
      N22=0
      DO2410I=1,N
        IF(X(I).EQ.1.0 .AND. Y(I).EQ.1.0)THEN
          N11=N11+1
        ELSEIF(X(I).EQ.0.0 .AND. Y(I).EQ.0.0)THEN
          N22=N22+1
        ELSEIF(X(I).EQ.1.0 .AND. Y(I).EQ.0.0)THEN
          N12=N12+1
        ELSEIF(X(I).EQ.0.0 .AND. Y(I).EQ.1.0)THEN
          N21=N21+1
        ENDIF
 2410 CONTINUE
C
      STAT=REAL(N11 + N22)/REAL(N)
C
 3000 CONTINUE
C
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF' .OR. IWRITE.EQ.'NO')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CORRECT MATCH PROPORTION = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CORMAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,N11,N12,N21,N22
 9013   FORMAT('N,N11,N12,N21,N22 = ',5I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STAT
 9015   FORMAT('STAT = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CORR(X,Y,N,IWRITE,XYCORR,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CORRELATION COEFFICIENT
C              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
C              THE SAMPLE CORRELATION COEFFICIENT WILL BE A SINGLE
C              PRECISION VALUE CALCULATED AS THE SUM OF CROSS PRODUCTS
C              DIVIDED BY (N-1).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS WHICH
C                                CONSTITUTE THE FIRST SET OF DATA.
C                     --Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS WHICH
C                                CONSTITUTE THE SECOND SET OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X, OR EQUIVALENTLY,
C                                THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--XYCORR = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CORRELATION COEFFICIENT
C                                BETWEEN THE 2 SETS OF DATA IN THE
C                                INPUT VECTORS X AND Y.  THIS SINGLE
C                                PRECISION VALUE WILL BE BETWEEN -1.0
C                                AND 1.0 (INCLUSIVELY).
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CORRELATION COEFFICIENT BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 235-236.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 2, EDITION 1, 1961, PAGES 292-293.
C               --SNEDECOR AND COCHRAN, STATISTICAL METHODS,
C                 EDITION 6, 1967, PAGES 172-198.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1979.
C     UPDATED         --JUNE      1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM12
      DOUBLE PRECISION DMEAN1
      DOUBLE PRECISION DMEAN2
      DOUBLE PRECISION DSQRT1
      DOUBLE PRECISION DSQRT2
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CORR'
      ISUBN2='    '
      IERROR='NO'
C
      DN=0.0D0
      DMEAN1=0.0D0
      DMEAN2=0.0D0
      DSUM12=0.0D0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CORR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
         WRITE(ICOUT,56)I,X(I),Y(I)
   56    FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************************
C               **  COMPUTE     CORRELATION COEFFICIENT  **
C               *******************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CORRELATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(N.EQ.1)THEN
        XYCORR=1.0
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      IF(IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** WARNING IN CORRELATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,136)HOLD
  136   FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ',
     1         'ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      XYCORR=1.0
      GOTO9000
  139 CONTINUE
C
      HOLD=Y(1)
      DO145I=2,N
       IF(Y(I).NE.HOLD)GOTO149
  145 CONTINUE
      IF(IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,146)HOLD
  146   FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ',
     1         'ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      XYCORR=1.0
      GOTO9000
  149 CONTINUE
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE     CORRELATION COEFFICIENT.  **
C               ************************************************
C
      DN=N
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO200I=1,N
        DX1=X(I)
        DX2=Y(I)
        DSUM1=DSUM1+DX1
        DSUM2=DSUM2+DX2
  200 CONTINUE
      DMEAN1=DSUM1/DN
      DMEAN2=DSUM2/DN
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM12=0.0D0
      DO300I=1,N
        DX1=X(I)
        DX2=Y(I)
        DSUM1=DSUM1+(DX1-DMEAN1)*(DX1-DMEAN1)
        DSUM2=DSUM2+(DX2-DMEAN2)*(DX2-DMEAN2)
        DSUM12=DSUM12+(DX1-DMEAN1)*(DX2-DMEAN2)
  300 CONTINUE
      DSQRT1=0.0
      IF(DSUM1.GT.0.0D0)DSQRT1=DSQRT(DSUM1)
      DSQRT2=0.0
      IF(DSUM2.GT.0.0D0)DSQRT2=DSQRT(DSUM2)
      XYCORR=DSUM12/(DSQRT1*DSQRT2)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XYCORR
  811   FORMAT('THE CORRELATION COEFFICIENT OF THE ',I8,
     1         ' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CORR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,XYCORR
 9012   FORMAT('IERROR,XYCORR = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DN,DMEAN1,DMEAN2,DSUM12
 9014   FORMAT('DN,DMEAN1,DMEAN2,DSUM12 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CORRAT(Y,X,N,ICASE,IWRITE,XDIST,ETA,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CORRELATION RATIO
C              FOR THE RESPONSE VARIABLE Y AND GROUP-ID VARIABLE X.
C              THE FORMULA IS:
C
C                 ETA**2 = SUM[i=1 to p][N(i)*(YBAR(i) - YBAR)**2/
C                          SUM[i=1 to p][SUM[j=1 to N(i)][Y(ij) - YBAR)**2
C
C              WHERE
C
C                 P       = NUMBER OF GROUPS
C                 N(i)    = NUMBER OF OBERVATIONS IN GROUP i
C                 YBAR(i) = MEAN OF GROUP i
C                 YBAR    = GRAND MEAN
C
C              THE INTRACLASS CORRELATION COEFFICIENT IS THE THE
C              SQUARE ROOT OF THE CORRELATION RATIO.
C
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS FOR THE
C                                RESPONSE VARIABLE.
C                     --X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS FOR THE
C                                GROUP-ID VARIABLE.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--CORR   = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CORRELATION RATIO.
C                                THIS SINGLE PRECISION VALUE WILL BE
C                                BETWEEN -1.0 AND 1.0 (INCLUSIVELY).
C                     --ETA    = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE INTRACLASS CORRELATION.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE
C             CORRELATION RATIO FOR THE 2 SETS OF DATA IN THE INPUT
C             VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--Pearson E.S. (1926) "Review of Statistical Methods
C                 for Research Workers (R. A. Fisher)", Science
C                 Progress, 20, 733-734.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2019/08
C     ORIGINAL VERSION--AUGUST    2019.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASE
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XDIST(*)
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DSUM4
      DOUBLE PRECISION DMEAN
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CORR'
      ISUBN2='AT  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RRAT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CORR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************************
C               **  COMPUTE     CORRELATION RATIO        **
C               *******************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CORRELATION RATIO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE     CORRELATION RATIO.        **
C               ************************************************
C
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL MEAN(Y,N,IWRITE,YBAR,IBUGA3,IERROR)
C
      DSUM2=0.0D0
      DSUM4=0.0D0
      DO1000IGRP=1,NDIST
        NTEMP=0
        AHOLD=XDIST(IGRP)
        DSUM1=0.0D0
        DSUM3=0.0D0
        DO1010I=1,N
          IF(X(I).EQ.AHOLD)THEN
            NTEMP=NTEMP+1
            DSUM1=DSUM1 + DBLE(Y(I))
            DSUM3=DSUM3 + (DBLE(Y(I)) - DBLE(YBAR))**2
          ENDIF
 1010   CONTINUE
        DMEAN=DSUM1/DBLE(NTEMP)
        DN=DBLE(NTEMP)
        DSUM2=DSUM2 + DN*(DMEAN-DBLE(YBAR))**2
        DSUM4=DSUM4 + DSUM3
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RRAT')THEN
          WRITE(ICOUT,1019)IGRP,NTEMP,DSUM1,DSUM3
 1019     FORMAT('IGRP,NTEMP,DSUM1,DSUM3 = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 1000 CONTINUE
      ETASQ=REAL(DSUM2/DSUM4)
      ETA=DSQRT(DSUM2/DSUM4)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE.EQ.'CRAT')THEN
          WRITE(ICOUT,8011)N,ETA
 8011     FORMAT('THE CORRELATION RATIO OF THE ',I8,
     1         ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,8013)N,ETASQ
 8013     FORMAT('THE INTRACLASS CORRELATION OF THE ',I8,
     1         ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RRAT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CORRAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NDIST,CORR,ETA
 9012   FORMAT('IERROR,NDIST,CORR,ETA = ',A4,2X,I8,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DN,DMEAN1,DMEAN2,DSUM12
 9014   FORMAT('DN,DMEAN1,DMEAN2,DSUM12 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE COSCDF(X,CDF)
C
C     NOTE--COSINE CDF IS:
C              COSCDF(X) = (PI + X + SIN(X))/(2*PI),  -PI<=X<=PI
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA PI/3.1415926535898E0/
C
C-----START POINT-----------------------------------------------------
C
      CDF=0.0
      IF(X.LT.-PI)THEN
        CDF=0.0
      ELSEIF(X.GT.PI)THEN
        CDF=1.0
      ELSE
        CDF=(PI + X + SIN(X))/(2*PI)
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE COSDIS(X,Y,N,IWRITE,ICASE,STATVA,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE COSINE DISTANCE BETWEEN THE
C              TWO SETS OF DATA IN THE INPUT VECTORS X AND Y.  THE
C              SAMPLE COSINE DISTANCE WILL BE A SINGLE PRECISION VALUE
C              CALCULATED AS:
C
C                 SIMLARITY = SUM[i=1 to n][X(i)*Y(i)]/
C                             {SQRT(SUM{i=1 to n][X(i)**2])*
C                             SQRT(SUM{i=1 to n][Y(i)**2])}
C
C                 DISTANCE = 1 - SIMILARITY
C
C              THE ABOVE DISTANCE IS FOR POSITIVE VECTORS.  NOTE
C              THAT THIS DISTANCE IS NOT A PROPER DISTANCE IN THAT
C              THE SCHWARTZ INEQUALITY DOES NOT HOLD.  HOWEVER, THE
C              ANGULAR VERSIONS (FOR POSITIVE VECTORS) ARE PROPER
C              DISTANCES:
C
C                 ANGULAR DISTANCE = (1/COSINE SIMILARITY)/PI
C                 ANGULAR SIMILARITY = 1 - DISTANCE
C
C              2018/08: UPDATED FORMULAS
C
C                 ANGULAR DISTANCE = COS^(-1)(COSINE SIMILARITY)/PI
C                 ANGULAR SIMILARITY = 1 - DISTANCE
C
C              IF NEGATIVE DATA IS ENCOUNTERED IN THE INPUT
C              VECTORS, ONLY THE COSINE SIMILARITY IS COMPUTED.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS WHICH
C                                CONSTITUTE THE FIRST SET OF DATA.
C                     --Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS WHICH
C                                CONSTITUTE THE SECOND SET OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTORS X AND Y.
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE COSINE DISTANCE
C                                BETWEEN THE TWO SETS OF DATA IN THE
C                                INPUT VECTORS X AND Y.  THIS SINGLE
C                                PRECISION VALUE WILL BE BETWEEN 0.0
C                                AND 1.0 (INCLUSIVELY).
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE COSINE DISTANCE BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHN FOREMAN (2014), "DATA SMART", WILEY.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/03
C     ORIGINAL VERSION--MARCH     2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DTERM1
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA PI/3.14159265358979/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COSD'
      ISUBN2='IS  '
      IERROR='NO'
      COSSIM=CPUMIN
      COSDST=CPUMIN
      ANGSIM=CPUMIN
      ANGDIS=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDIS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF COSDIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN COSINE DISTANCE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLES IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.EQ.1)THEN
        STATVA=1.0
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE COSINE DISTANCE.              **
C               ************************************************
C
      IFLAG=1
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO200I=1,N
        IF(X(I).LT.0.0)IFLAG=0
        IF(Y(I).LT.0.0)IFLAG=0
        DX1=X(I)
        DX2=Y(I)
        DSUM1=DSUM1+DX1*DX2
        DSUM2=DSUM2+DX1**2
        DSUM3=DSUM3+DX2**2
  200 CONTINUE
      IF(DSUM2.GT.0.0D0 .AND. DSUM3.GT.0.0D0)THEN
        DTERM1=DSUM1/(DSQRT(DSUM2)*DSQRT(DSUM3))
      ELSE
        GOTO9000
      ENDIF
      COSSIM=REAL(DTERM1)
      IF(IFLAG.EQ.1)THEN
        COSDST=1.0 - COSSIM
        AFACT=2.0
      ELSE
        AFACT=1.0
      ENDIF
      ANGDIS=AFACT*ACOS(COSSIM)/PI
      ANGSIM=1.0 - ANGDIS
C
      IF(ICASE.EQ.'COSS')THEN
        STATVA=COSSIM
      ELSEIF(ICASE.EQ.'COSD')THEN
        STATVA=COSDST
      ELSEIF(ICASE.EQ.'ACOS')THEN
        STATVA=ANGSIM
      ELSEIF(ICASE.EQ.'ACOD')THEN
        STATVA=ANGDIS
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE.EQ.'COSD')THEN
          WRITE(ICOUT,811)N,STATVA
  811     FORMAT('THE COSINE DISTANCE OF THE ',I8,
     1           ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'COSS')THEN
          WRITE(ICOUT,813)N,STATVA
  813     FORMAT('THE COSINE SIMILARITY OF THE ',I8,
     1           ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'ANGS')THEN
          WRITE(ICOUT,815)N,STATVA
  815     FORMAT('THE ANGULAR COSINE SIMILARITY OF THE ',I8,
     1           ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'ANGD')THEN
          WRITE(ICOUT,817)N,STATVA
  817     FORMAT('THE ANGULAR COSINE DISTANCE OF THE ',I8,
     1           ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDIS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CORR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,STATVA,IFLAG1
 9012   FORMAT('IERROR,STATVA,IFLAG1 = ',A4,2X,G15.7,I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DSUM1,DSUM2,DSUM3,DTERM1
 9014   FORMAT('DSUM1,DSUM2,DSUM3,DTERM1 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)COSSIM,COSDST,ANGSIM,ANGDIS
 9016   FORMAT('COSSIM,COSDST,ANGSIM,ANGDIS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE COSPDF(X,PDF)
C
C     NOTE--COSINE PDF IS:
C              COSPDF(X) = (1 + COS(X))/(2*PI),  -PI<=X<=PI
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA PI/3.1415926535898E0/
C
C-----START POINT-----------------------------------------------------
C
      PDF=0.0
      IF(X.LT.-PI .OR. X.GT.PI)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('***** ERROR--THE INPUT ARGUMENT TO COSPDF IS NOT IN THE ',
     1       'INTERVAL (-PI,PI).')
  302 FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      PDF=(1.0 + COS(X))/(2*PI)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE COSPPF(P,PPF)
C
C     NOTE--ALGORITHM ADDED APRIL 1995 (ALAN)
C           USE A BISECTION METHOD
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA PI/3.1415926535898E0/
      DATA EPS /1.0E-10/
      DATA SIG /1.0E-10/
      DATA ZERO /0./
      DATA MAXIT /500/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO COSPPF IS OUTSIDE',
     1         ' THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        RETURN
      ENDIF
C
      IERR=0
      IC = 0
      IF(P.LE.0.0)THEN
        PPF=-PI
        GOTO9999
      ELSEIF(P.GE.1.0)THEN
        PPF=PI
        GOTO9999
      ENDIF
C
      XL = -PI
      XR = PI
      FXL = -P
      FXR = 1.0 - P
CCCCC INVALID P EXPLICITLY CHECKED FOR EARLIER.
CCCCC IF(FXL*FXR .GT. ZERO)GOTO50
C
C  BISECTION METHOD
C
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL COSCDF(X,CDF)
      P1=CDF
      PPF=X
      FCS = P1 - P
      IF(FCS*FXL.GT.ZERO)GOTO110
      XR = X
      FXR = FCS 
      GOTO115
  110 CONTINUE
      XL = X
      FXL = FCS
  115 CONTINUE
      XRML = XR - XL
      IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GOTO9999
      IC = IC + 1
      IF(IC.LE.MAXIT)GOTO105
      WRITE(ICOUT,130)
      CALL DPWRST('XXX','BUG ')
  130 FORMAT('***** ERROR--COSPPF ROUTINE DID NOT CONVERGE. ***')
      GOTO9999
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE COSRAN(N,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE COSINE DISTRIBUTION
C              F(X) = 0.5*EXP(-ABS(X)).
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE COSINE DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2001/10
C     ORIGINAL VERSION--OCTOBER   2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO COSRAN IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N COSINE RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
        CALL COSPPF(X(I),XTEMP)
        X(I)=XTEMP
  100 CONTINUE
C
      RETURN
      END
      SUBROUTINE COSTRA(Y1,N1,IWRITE,Y2,N2,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE COSINE TRANSFORM OF A VARIABLE--
C            = THE COEFFICIENTS OF THE COSINE TERM
C              IN THE FINITE FOURIER RESPRESENTATION OF THE DATA IN Y1.
C              Y2(1) = A0 = MEAN
C              Y2(2) = A1
C              Y2(3) = A2
C              ETC.
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y2(.)
C           BEING IDENTICAL TO THE INPUT VECTOR Y1(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85/1
C     ORIGINAL VERSION--DECEMBER  1984.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DN1
      DOUBLE PRECISION DDEL
      DOUBLE PRECISION DI
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DK
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DY1K
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COST'
      ISUBN2='RA  '
C
      IERROR='NO'
C
      N1HALF=(-999)
      IMAX=(-999)
      IEVODD=(-999)
      DDEL=(-999.0D0)
      DN1=(-999.0D0)
C
      DN1=N1
C
      DPI=3.14159265358979D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COSTRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N1
   53 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N1
      WRITE(ICOUT,56)I,Y1(I)
   56 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ***********************************
C               **  COMPUTE COSINE TRANSFORM.    **
C               ***********************************
C
      IF(N1.LT.1)GOTO1100
      GOTO1190
C
 1100 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN COSTRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)
 1154 FORMAT('      THE COSINE TRANSFORM IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)
 1156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1157)N1
 1157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1190 CONTINUE
C
      N1HALF=N1/2
      N1HALP=N1HALF+1
      IMAX=N1HALP
      IEVODD=N1-2*(N1/2)
      DDEL=(DN1+1.0D0)/2.0D0
      IF(IEVODD.EQ.0)DDEL=(DN1+2.0D0)/2.0D0
C
      J=0
      J=J+1
      DSUM=0.0
      DO1205K=1,N1
      DY1K=Y1(K)
      DSUM=DSUM+DY1K
 1205 CONTINUE
      COEF=DSUM/DN1
      Y2(J)=COEF
C
      DO1210IP1=2,IMAX
      J=J+1
      I=IP1-1
      DI=I
CCCCC FREQI=DI/DN1
      DSUM=0.0D0
C
      DO1220K=1,N1
      DK=K
      DOMEGA=2.0*DPI*(DI/DN1)
      DY1K=Y1(K)
      DSUM=DSUM+DY1K*DCOS(DOMEGA*(DK-DDEL))
 1220 CONTINUE
      COEF=DSUM/DN1
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,1221)J,I,DN1,DI,COEF
 1221 FORMAT('J,I,DN1,DI,COEF = ',I8,I8,2D15.7,E15.7)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      Y2(J)=COEF
C
 1210 CONTINUE
C
      N2=J
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COSTRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N1,N2,N1HALF,IMAX,IEVODD,DDEL
 9013 FORMAT('N1,N2,N1HALF,IMAX,IEVODD,DDEL = ',5I8,D15.7)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N1
      WRITE(ICOUT,9016)I,Y1(I),Y2(I)
 9016 FORMAT('I,Y1(I),Y2(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COV(X,Y,N,IWRITE,XYCOV,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE COVARIANCE COEFFICIENT
C              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
C              THE SAMPLE COVARIANCE COEFFICIENT WILL BE A SINGLE
C              PRECISION VALUE CALCULATED AS THE
C              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE FIRST SET
C                                OF DATA.
C                     --Y      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE SECOND SET
C                                OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X, OR EQUIVALENTLY,
C                                THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--XYCOV  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE COVARIANCE COEFFICIENT
C                                BETWEEN THE 2 SETS OF DATA
C                                IN THE INPUT VECTORS X AND Y.
C                                THIS SINGLE PRECISION VALUE
C                                WILL BE BETWEEN -1.0 AND 1.0
C                                (INCLUSIVELY).
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE COVARIANCE COEFFICIENT BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 235-236.
C               --KENDALL AND STUART, THE ADVANCED THEORY OF
C                 STATISTICS, VOLUME 2, EDITION 1, 1961, PAGES 292-293.
C               --SNEDECOR AND COCHRAN, STATISTICAL METHODS,
C                 EDITION 6, 1967, PAGES 172-198.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1979.
C     UPDATED         --JUNE      1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM12
      DOUBLE PRECISION DMEAN1
      DOUBLE PRECISION DMEAN2
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='COV '
      ISUBN2='    '
C
      IERROR='NO'
C
      DN=0.0D0
      DMEAN1=0.0D0
      DMEAN2=0.0D0
      DSUM12=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF COV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I),Y(I)
   56 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *******************************************
C               **  COMPUTE     COVARIANCE  COEFFICIENT  **
C               *******************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.GE.1)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN COV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE COVARIANCE COEFFICIENT IS TO BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      COMPUTED, MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      IF(N.EQ.1)GOTO120
      GOTO129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',
     1'THE THIRD INPUT ARGUMENT (N) HAS THE VALUE 1')
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
      GOTO9000
  129 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',
     1'THE FIRST  INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
      GOTO9000
  139 CONTINUE
C
      HOLD=Y(1)
      DO145I=2,N
      IF(Y(I).NE.HOLD)GOTO149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,146)HOLD
  146 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',
     1'THE SECOND INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
      GOTO9000
  149 CONTINUE
C
  190 CONTINUE
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE     COVARIANCE  COEFFICIENT.  **
C               ************************************************
C
      DN=N
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO200I=1,N
      DX1=X(I)
      DX2=Y(I)
      DSUM1=DSUM1+DX1
      DSUM2=DSUM2+DX2
  200 CONTINUE
      DMEAN1=DSUM1/DN
      DMEAN2=DSUM2/DN
C
      DSUM12=0.0D0
      DO300I=1,N
      DX1=X(I)
      DX2=Y(I)
      DSUM12=DSUM12+(DX1-DMEAN1)*(DX2-DMEAN2)
  300 CONTINUE
      XYCOV=DSUM12/(DN-1.0D0)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,XYCOV
  811 FORMAT('THE COVARIANCE COEFFICIENT OF THE ',I8,
     1' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF COV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DN,DMEAN1,DMEAN2,DSUM12
 9014 FORMAT('DN,DMEAN1,DMEAN2,DSUM12 = ',4D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)XYCOV
 9015 FORMAT('XYCOV = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE COVMAT(YM1,YM9,DMEAN,MAXROM,NR,NC,MAXVAR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE VARIANCE-COVARIANCE
C              MATRIX.  THIS IS A UTILITY ROUTINE, ERROR CHECKING
C              PERFORMED BY CALLING ROUTINES.
C     INPUT  ARGUMENTS--YM1    = THE SINGLE PRECISION MATRIX OF
C                                OBSERVATIONS
C                     --NR     = THE INTEGER NUMBER OF ROWS
C                     --NC     = THE INTEGER NUMBER OF COLUMNS
C                     --MAXROM = LEADING DIMENSION OF XMAT, COVMAT
C     OUTPUT ARGUMENTS--YM9    = THE SINGLE PRECISION MATRIX WHICH
C                                WILL CONTAIN THE COVARIANCE MATRIX
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE VARIANCE-COVARIANCE MATRIX.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2003/2
C     ORIGINAL VERSION--FEBRUARY  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DNR
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DCOV
      DOUBLE PRECISION DMEAN(*)
C
      DIMENSION YM1(MAXROM,NC)
      DIMENSION YM9(MAXVAR,MAXVAR)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      DNR=DBLE(NR)
C
      DO5111J=1,NC
        DSUM1=0.0D0
        DO5112I=1,NR
          DYM1=YM1(I,J)
          DSUM1=DSUM1+DYM1
 5112   CONTINUE
        DMEAN(J)=-9999.0D0
        DDENOM=DNR
        IF(DDENOM.NE.0.0D0)DMEAN(J)=REAL(DSUM1/DDENOM)
 5111 CONTINUE
C
      DO5121J=1,NC
        DO5122K=J,NC
          DSUM1=0.0D0
          DO5123I=1,NR
            DYM1=YM1(I,J)
            DYM2=YM1(I,K)
            DDEL1=DYM1-DMEAN(J)
            DDEL2=DYM2-DMEAN(K)
            DSUM1=DSUM1+DDEL1*DDEL2
 5123     CONTINUE
          DCOV=-9999.0D0
          DDENOM=DNR-1.0D0
          IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
          YM9(J,K)=DCOV
          YM9(K,J)=DCOV
 5122   CONTINUE
 5121 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      RETURN
      END
      SUBROUTINE CP(X,N,ENGLSL,ENGUSL,IWRITE,XCP,XLCL,XUCL,
     1             IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CP (PROCESS
C              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
C              CP = (ENGUSL - ENGLSL) / 6*S
C     NOTE--IF THE TARGET VALUE IS MIDWAY BETWEEN ENGUSL AND ENGLSL,
C           THEN AN ALTERNATIVE EQUIVALENT DEFINITION FOR CP IS
C              CP = (ENGUSL-TARGET) / 3*S
C     NOTE--CP IS A MEASURE OF PROCESS PRECISION--
C           IT CONTAINS NO BIAS INFORMATION.
C     NOTE--THE CP INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CP WHICH ARE
C           LARGE (ABOVE 2); VALUES OF CP FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CP     = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CP
C                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
C                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CP INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--R&M 2000 AIRFORCE MANUAL
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89.5
C     ORIGINAL VERSION--MAY       1989.
C     UPDATED         --SEPTEMBER 1990. REVERSE INPUT ARGS
C     UPDATED         --APRIL     2001. ADD LOWER AND UPPER 95%
C                                       CONFIDENCE INTERVAL.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCP
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CP  '
      ISUBN2='    '
      IERROR='NO'
C
      XCP=0.0
      DMEAN=0.0D0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CP  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CP RATIO                        **
C               **************************************************
C
      DUSL=ENGUSL
      DLSL=ENGLSL
C
      DNUM=DUSL-DLSL
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
C
      DDEN=6.0*DSD
C
      DCP=0.0
      IF(DDEN.GT.0.0D0)DCP=DNUM/DDEN
      XCP=DCP
C
      XLCL=0.0
      XUCL=0.0
      AN=REAL(N)
      NV=N-1
      AV=REAL(NV)
      P=0.975
      CALL CHSPPF(P,NV,PPF)
      IF((PPF/AV).GT.0.0)XUCL=XCP*SQRT(PPF/AV)
      P=0.025
      CALL CHSPPF(P,NV,PPF)
      IF((PPF/AV).GT.0.0)XLCL=XCP*SQRT(PPF/AV)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCP
  811   FORMAT('THE CP OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD,DUSL,DLSL
 9014   FORMAT('DMEAN,DSD,DUSL,DLSL = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)DNUM,DDEN,DCP,XCP
 9017   FORMAT('DNUM,DDEN,DCP,XCP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CPEVL(N,M,A,Z,C,B,KBD)
C***BEGIN PROLOGUE  CPEVL
C***REFER TO  CPZERO
C
C        Evaluate a complex polynomial and its derivatives.
C        Optionally compute error bounds for these values.
C
C   INPUT...
C        N = Degree of the polynomial
C        M = Number of derivatives to be calculated,
C            M=0 evaluates only the function
C            M=1 evaluates the function and first derivative, etc.
C             if M .GT. N+1 function and all N derivatives will be
C                calculated.
C       A = Complex vector containing the N+1 coefficients of polynomial
C               A(I)= coefficient of Z**(N+1-I)
C        Z = Complex point at which the evaluation is to take place.
C        C = Array of 2(M+1) words into which values are placed.
C        B = Array of 2(M+1) words only needed if bounds are to be
C              calculated.  It is not used otherwise.
C        KBD = A logical variable, e.g. .TRUE. or .FALSE. which is
C              to be set .TRUE. if bounds are to be computed.
C
C  OUTPUT...
C        C =  C(I+1) contains the complex value of the I-th
C              derivative at Z, I=0,...,M
C        B =  B(I) contains the bounds on the real and imaginary parts
C              of C(I) if they were requested.
C***ROUTINES CALLED  I1MACH
C***END PROLOGUE  CPEVL
C
      COMPLEX A(1),C(1),Z,CI,CIM1,B(1),BI,BIM1,T,ZA,Q
      LOGICAL KBD
C
      INCLUDE 'DPCOMC.INC'
C
      DATA NBITS /0/
      ZA(Q)=CMPLX(ABS(REAL(Q)),ABS(AIMAG(Q)))
C***FIRST EXECUTABLE STATEMENT  CPEVL
      IF ( NBITS .EQ. 0 ) NBITS = I1MACH (11)
      D1=2.**(1-NBITS)
      NP1=N+1
      DO 1 J=1,NP1
         CI=0.0
         CIM1=A(J)
         BI=0.0
         BIM1=0.0
         MINI=MIN0(M+1,N+2-J)
            DO 1 I=1,MINI
               IF(J .NE. 1) CI=C(I)
               IF(I .NE. 1) CIM1=C(I-1)
               C(I)=CIM1+Z*CI
               IF(.NOT. KBD) GO TO 1
               IF(J .NE. 1) BI=B(I)
               IF(I .NE. 1) BIM1=B(I-1)
               T=BI+(3.*D1+4.*D1*D1)*ZA(CI)
               R=REAL(ZA(Z)*CMPLX(REAL(T),-AIMAG(T)))
               S=AIMAG(ZA(Z)*T)
               B(I)=(1.+8.*D1)*(BIM1+D1*ZA(CIM1)+CMPLX(R,S))
               IF(J .EQ. 1) B(I)=0.0
    1 CONTINUE
      RETURN
      END
      SUBROUTINE CPK(X,N,ENGLSL,ENGUSL,IWRITE,XCPK,XLCL,XUCL,
     1               IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPK (PROCESS
C              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
C
C                 CPK = MIN(USL-MEAN,MEAN-LSL)/(3*S)
C
C     NOTE--CPK IS A MEASURE OF PROCESS ACCURACY--
C           COMBINING BOTH PRECISION AND UNBIASEDNESS.
C     NOTE--THE CPK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPK WHICH ARE
C           LARGE (ABOVE 2); VALUES OF CPK FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CPK    = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPK
C                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
C                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CPK INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--R&M 2000 AIR FORCE MANUAL
C               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89.5
C     ORIGINAL VERSION--MAY       1989.
C     UPDATED         --SEPTEMBER 1990. REVERSE INPUT ARGS
C     UPDATED         --APRIL     2001. 95% CONFIDENCE LIMITS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPK
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPK '
      ISUBN2='    '
      IERROR='NO'
C
      XCPK=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPK  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPK RATIO                       **
C               **************************************************
C
      DUSL=ENGUSL
      DLSL=ENGLSL
C
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
C
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
C
      DDEN=3.0*DSD
C
      DCPK=0.0
      IF(DDEN.GT.0.0D0)DCPK=DNUM/DDEN
      XCPK=DCPK
C
      AN=REAL(N)
      P=0.975
      TERM1=1.0/(9.0*AN)
      TERM2=XCPK*XCPK/(2.0*(AN-1.0))
      CALL NORPPF(P,PPF)
      XLCL=XCPK - PPF*SQRT(TERM1 + TERM2)
      XUCL=XCPK + PPF*SQRT(TERM1 + TERM2)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCPK
  811   FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD
 9014   FORMAT('DMEAN,DSD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)DUSL,DLSL,DUPPER,DLOWER
 9016   FORMAT('DUSL,DLSL,DUPPER,DLOWER = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)DNUM,DDEN,DCPK,XCPK
 9017   FORMAT('DNUM,DDEN,DCPK,XCPK = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CPKM(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPKM,XLCL,XUCL,
     1                IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPKM (PROCESS
C              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
C
C                 CPKM = MIN(USL-MEAN,MEAN-LSL)/
C                        {3*SQRT(S**2 +(MEAN-TARGET)**2)}
C
C     NOTE--CPKM IS A MEASURE OF PROCESS ACCURACY--
C           COMBINING BOTH PRECISION AND UNBIASEDNESS.
C     NOTE--THE CPKM INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPKM WHICH ARE
C           LARGE (ABOVE 2); VALUES OF CPKM FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C                     --TARGET = TARGET VALUE (ENGINEERING)
C     OUTPUT ARGUMENTS--CPKM   = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPKM
C                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
C                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CPKM INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.4
C     ORIGINAL VERSION--APRIL     2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPKM
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPKM '
      ISUBN2='    '
      IERROR='NO'
C
      XCPKM=0.0
      XCL=CPUMIN
      XUL=CPUMIN
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPKM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPKM  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CPKM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPKM RATIO                      **
C               **************************************************
C
      DUSL=DBLE(ENGUSL)
      DLSL=DBLE(ENGLSL)
      DTARG=DBLE(TARGET)
C
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
C
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
C
      DDEN=3.0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
C
      DCPKM=0.0
      IF(DDEN.GT.0.0D0)DCPKM=DNUM/DDEN
      XCPKM=DCPK
C
C     FOLLOWING CONFIDENCE INTERVALS ARE FOR CPK.  HAVEN'T FOUND
C     A SOURCE FOR CPKM CONFIDENCE INTERVALS.
C
CCCCC AN=REAL(N)
CCCCC P=0.975
CCCCC TERM1=1.0/(9.0*AN)
CCCCC TERM2=XCPKM*XCPK/(2.0*(AN-1.0))
CCCCC CALL NORPPF(P,PPF)
CCCCC XLCL=XCPKM - PPF*SQRT(TERM1 + TERM2)
CCCCC XUCL=XCPKM + PPF*SQRT(TERM1 + TERM2)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCPKM
  811   FORMAT('THE CPKM OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CPKM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD
 9014   FORMAT('DMEAN,DSD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)DUSL,DLSL,DUPPER,DLOWER
 9016   FORMAT('DUSL,DLSL,DUPPER,DLOWER = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)DNUM,DDEN,DCPKM,XCPKM
 9017   FORMAT('DNUM,DDEN,DCPKM,XCPK = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CPL(X,N,ENGLSL,ENGUSL,IWRITE,XCPL,XLCL,XUCL,
     1               IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE CPL (PROCESS CAPABILITY INDEX)
C              OF THE DATA IN THE INPUT VECTOR X.
C              CPL = NUMERATOR/DENOMINATOR
C              WHERE NUMERATOR = XBAR - LOWER SPEC LIMIT
C              AND DENOMINATOR = 3 * SIGMA
C     NOTE--CPL IS A VARIATION OF CPL WHEN YOU ARE ONLY
C           INTERESTED IN THE LOWER SPEC LIMIT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CPL    = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPL
C                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
C                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CPL INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--R&M 2000 AIR FORCE MANUAL
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001.4
C     ORIGINAL VERSION--APRIL     2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPL
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPL '
      ISUBN2='    '
C
      IERROR='NO'
C
      DMEAN=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ENGUSL,ENGLSL
   54 FORMAT('ENGUSL,ENGLSL = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPL  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.GE.1)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN CPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE CPL STATISTIC IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      IF(N.EQ.1)GOTO120
      GOTO129
  120 CONTINUE
      XSD=0.0
      GOTO9000
  129 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      XSD=0.0
      GOTO9000
  139 CONTINUE
C
  190 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
      DX=X(I)
      DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
      DX=X(I)
      DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPL RATIO                       **
C               **************************************************
C
      DUSL=ENGUSL
      DLSL=ENGLSL
C
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
C
      DNUM=DLOWER
C
      DDEN=3.0*DSD
C
      DCPL=0.0D0
      IF(DDEN.GT.0.0D0)DCPL=DNUM/DDEN
      XCPL=DCPL
C
      AN=REAL(N)
      P=0.975
      CALL NORPPF(P,PPF)
      XLCL=0.0
      XUCL=0.0
      IF(N.GT.1)THEN
        XLCL=XCPL - PPF*SQRT((1.0/(9.0*AN)) + XCPL/(2.0*(AN-1.0)))
        XUCL=XCPL + PPF*SQRT((1.0/(9.0*AN)) + XCPL/(2.0*(AN-1.0)))
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,XCPL
  811 FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DMEAN
 9014 FORMAT('DMEAN = ',D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)DSD
 9015 FORMAT('DSD = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)DUSL,DLSL,DUPPER,DLOWER
 9016 FORMAT('DUSL,DLSL,DUPPER,DLOWER = ',4D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)DNUM,DDEN,DCPL,XCPL
 9017 FORMAT('DNUM,DDEN,DCPL,XCPL = ',3D15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CPM(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPM,XLCL,XUCL,
     1               IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPM (PROCESS
C              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
C
C                 CPM = (USL - LSL)/(6*SQRT(S**2+(XBAR-TARGET)**2))
C
C     NOTE--CPM IS A MEASURE OF PROCESS ACCURACY--
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--XCPM   = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPM
C                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
C                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPM INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--MEAN AND SD.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NORMA HUBELE, ARIZONA STATE
C               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98.11
C     ORIGINAL VERSION--NOVEMBER  1998.
C     UPDATED         --APRIL     2001. ADD 95% CONFIDENCE LIMITS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPM
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPM '
      ISUBN2='    '
      IERROR='NO'
C
      XCPM=0.0
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL,TARGET
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL,TARGET = ',A4,2X,I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPM  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XMEAN=DMEAN
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPM RATIO                       **
C               **************************************************
C
      DUSL=ENGUSL
      DLSL=ENGLSL
      DTARG=TARGET
C
      DNUM=DUSL-DLSL
      DDEN=6.0D0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
C
      DCPM=0.0
      IF(DDEN.GT.0.0D0)DCPM=DNUM/DDEN
      XCPM=DCPM
C
      XLCL=0.0
      XUCL=0.0
      AN=REAL(N)
      NV=N-1
      AV=REAL(NV)
      P=0.975
      CALL CHSPPF(P,NV,PPF)
      IF((PPF/AV).GT.0.0)XUCL=XCPM*SQRT(PPF/AV)
      P=0.025
      CALL CHSPPF(P,NV,PPF)
      IF((PPF/AV).GT.0.0)XLCL=XCPM*SQRT(PPF/AV)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCPM
  811   FORMAT('THE CPM OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD,DUSL,DLSL
 9014   FORMAT('DMEAN,DSD,DUSL,DLSL = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)DNUM,DDEN,DCPM,XCPM
 9017   FORMAT('DNUM,DDEN,DCPM,XCPM = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CPMK(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPMK,XLCL,XUCL,
     1                IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPMK (PROCESS
C              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
C
C                 CPMK = MIN(USL-MEAN,MEAN-LSL)/
C                        {3*SQRT(S**2 +(MEAN-TARGET)**2)}
C
C     NOTE--CPMK IS A MEASURE OF PROCESS ACCURACY--
C           COMBINING BOTH PRECISION AND UNBIASEDNESS.
C     NOTE--THE CPMK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
C           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPMK WHICH ARE
C           LARGE (ABOVE 2); VALUES OF CPMK FROM 0.5 TO 1.0 ARE TYPICAL.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C                     --TARGET = TARGET VALUE (ENGINEERING)
C     OUTPUT ARGUMENTS--CPMK   = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPMK
C                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
C                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CPMK INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
C                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
C                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
C                 VOL. 18, NO. 7, PP. 762-770.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.4
C     ORIGINAL VERSION--APRIL     2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPMK
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPMK '
      ISUBN2='    '
      IERROR='NO'
C
      XCPMK=0.0
      XCL=CPUMIN
      XUL=CPUMIN
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPMK  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
        DX=X(I)
        DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
        DX=X(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPMK RATIO                      **
C               **************************************************
C
      DUSL=DBLE(ENGUSL)
      DLSL=DBLE(ENGLSL)
      DTARG=DBLE(TARGET)
C
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
C
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
C
      DDEN=3.0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
C
      DCPMK=0.0
      IF(DDEN.GT.0.0D0)DCPMK=DNUM/DDEN
      XCPMK=DCPMK
C
C     FOLLOWING CONFIDENCE INTERVALS ARE FOR CPK.  HAVEN'T FOUND
C     A SOURCE FOR CPMK CONFIDENCE INTERVALS.
C
CCCCC AN=REAL(N)
CCCCC P=0.975
CCCCC TERM1=1.0/(9.0*AN)
CCCCC TERM2=XCPMK*XCPK/(2.0*(AN-1.0))
CCCCC CALL NORPPF(P,PPF)
CCCCC XLCL=XCPMK - PPF*SQRT(TERM1 + TERM2)
CCCCC XUCL=XCPMK + PPF*SQRT(TERM1 + TERM2)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,XCPMK
  811   FORMAT('THE CPMK OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD
 9014   FORMAT('DMEAN,DSD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)DUSL,DLSL,DUPPER,DLOWER
 9016   FORMAT('DUSL,DLSL,DUPPER,DLOWER = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)DNUM,DDEN,DCPMK,XCPMK
 9017   FORMAT('DNUM,DDEN,DCPMK,XCPMK = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      COMPLEX FUNCTION CPSI(ZIN)
C***BEGIN PROLOGUE  CPSI
C***DATE WRITTEN   780501   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C7C
C***KEYWORDS  COMPLEX,DIGAMMA FUNCTION,PSI FUNCTION,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the Psi function of complex argument.
C***DESCRIPTION
C
C PSI(X) calculates the psi (or digamma) function of X.  PSI(X)
C is the logarithmic derivative of the gamma function of X.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CCOT,R1MACH,XERROR
C***END PROLOGUE  CPSI
      COMPLEX ZIN, Z, Z2INV, CORR,  CCOT, CLOG
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DIMENSION BERN(13)
      DATA BERN( 1) /   .8333333333 3333333 E-1 /
      DATA BERN( 2) /  -.8333333333 3333333 E-2 /
      DATA BERN( 3) /   .3968253968 2539683 E-2 /
      DATA BERN( 4) /  -.4166666666 6666667 E-2 /
      DATA BERN( 5) /   .7575757575 7575758 E-2 /
      DATA BERN( 6) /  -.2109279609 2796093 E-1 /
      DATA BERN( 7) /   .8333333333 3333333 E-1 /
      DATA BERN( 8) /  -.4432598039 2156863 E0 /
      DATA BERN( 9) /   .3053954330 2701197 E1 /
      DATA BERN(10) /  -.2645621212 1212121 E2 /
      DATA BERN(11) /   .2814601449 2753623 E3 /
      DATA BERN(12) /  -.3454885393 7728938 E4 /
      DATA BERN(13) /   .5482758333 3333333 E5 /
      DATA PI / 3.141592653 589793 E0 /
      DATA NTERM, BOUND, DXREL, RMIN, RBIG / 0, 4*0.0 /
C***FIRST EXECUTABLE STATEMENT  CPSI
      IF (NTERM.NE.0) GO TO 10
      NTERM = -0.30*LOG(R1MACH(3))
C MAYBE BOUND = N*(0.1*EPS)**(-1/(2*N-1)) / (PI*EXP(1))
      BOUND = 0.1171*FLOAT(NTERM) *
     1  (0.1*R1MACH(3))**(-1.0/(2.0*FLOAT(NTERM)-1.0))
      DXREL = SQRT(R1MACH(4))
      RMIN = EXP (AMAX1 (LOG(R1MACH(1)), -LOG(R1MACH(2))) + 0.011 )
      RBIG = 1.0/R1MACH(3)
C
 10   Z = ZIN
      X = REAL(Z)
      Y = AIMAG(Z)
      IF (Y.LT.0.0) Z = CONJG(Z)
C
      CORR = (0.0, 0.0)
      CABSZ = CABS(Z)
      IF (X.GE.0.0 .AND. CABSZ.GT.BOUND) GO TO 50
      IF (X.LT.0.0 .AND. ABS(Y).GT.BOUND) GO TO 50
C
      IF (CABSZ.LT.BOUND) GO TO 20
C
C USE THE REFLECTION FORMULA FOR REAL(Z) NEGATIVE, CABS(Z) LARGE, AND
C ABS(AIMAG(Y)) SMALL.
C
      CORR = -PI*CCOT(PI*Z)
      Z = 1.0 - Z
      GO TO 50
C
C USE THE RECURSION RELATION FOR CABS(Z) SMALL.
C
 20   IF (CABSZ.LT.RMIN) THEN
CCCCC   CALL XERROR ( 'CPSI    CPSI CALLED WITH Z SO NE
CCCCC1AR 0 THAT CPSI OVERFLOWS',      56, 2, 2)
        WRITE(ICOUT,102)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 102  FORMAT('***** INTERNAL ERROR FROM CPSI: ARGUMENT SO CLOSE',
     1' TO ZERO THAT CPSI OVERFLOWS')
C
      IF (X.GE.(-0.5) .OR. ABS(Y).GT.DXREL) GO TO 30
      IF (CABS((Z-AINT(X-0.5))/X).LT.DXREL) THEN
CCCCC   CALL XERROR ( 'CPSI    ANSWE
CCCCC1R LT HALF PRECISION BECAUSE Z TOO NEAR NEGATIVE INTEGER', 68, 1, 1
CCCCC2)
        WRITE(ICOUT,202)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 202  FORMAT('***** INTERNAL ERROR FROM CPSI: ANSWER LESS THAN HALF',
     1' PRECISION BECAUSE ARGUMENT TOO NEAR A NEGATIVE INTEGER')
      IF (Y.EQ.0.0 .AND. X.EQ.AINT(X)) THEN
CCCCC   CALL XERROR ( 'CPSI    Z IS A NEG
CCCCC1ATIVE INTEGER', 31, 3, 2)
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 302  FORMAT('***** INTERNAL ERROR FROM CPSI: ARGUMENT IS A ',
     1' NEGATIVE INTEGER')
C
 30   N = SQRT(BOUND**2-Y**2) - X + 1.0
      DO 40 I=1,N
        CORR = CORR - 1.0/Z
        Z = Z + 1.0
 40   CONTINUE
C
C NOW EVALUATE THE ASYMPTOTIC SERIES FOR SUITABLY LARGE Z.
C
 50   IF (CABSZ.GT.RBIG) CPSI = CLOG(Z) + CORR
      IF (CABSZ.GT.RBIG) GO TO 70
C
      CPSI = (0.0, 0.0)
      Z2INV = 1.0/Z**2
      DO 60 I=1,NTERM
        NDX = NTERM + 1 - I
        CPSI = BERN(NDX) + Z2INV*CPSI
 60   CONTINUE
      CPSI = CLOG(Z) - 0.5/Z - CPSI*Z2INV + CORR
C
 70   IF (Y.LT.0.0) CPSI = CONJG(CPSI)
C
      RETURN
      END
      SUBROUTINE CPU(X,N,ENGLSL,ENGUSL,IWRITE,XCPU,XLCL,XUCL,
     1               IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE CPU (PROCESS CAPABILITY INDEX)
C              OF THE DATA IN THE INPUT VECTOR X.
C              CPU = NUMERATOR/DENOMINATOR
C              WHERE NUMERATOR = XBAR + UPPER SPEC LIMIT
C              AND DENOMINATOR = 3 * SIGMA
C     NOTE--CPU IS A VARIATION OF CPK WHEN YOU ARE ONLY
C           INTERESTED IN THE UPPER SPEC LIMIT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
C                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
C     OUTPUT ARGUMENTS--CPU    = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE CPU
C                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
C                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CPU INDEX
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--R&M 2000 AIR FORCE MANUAL
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001.4
C     ORIGINAL VERSION--APRIL     2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPU
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CPU '
      ISUBN2='    '
C
      IERROR='NO'
C
      DMEAN=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CPU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ENGUSL,ENGLSL
   54 FORMAT('ENGUSL,ENGLSL = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  COMPUTE PROCESS CAPABILITY INDEX CPU  **
C               ********************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.GE.1)GOTO119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN CPU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE CPU STATISTIC IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)
  115 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,116)
  116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,117)N
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  119 CONTINUE
C
      IF(N.EQ.1)GOTO120
      GOTO129
  120 CONTINUE
      XSD=0.0
      GOTO9000
  129 CONTINUE
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      XSD=0.0
      GOTO9000
  139 CONTINUE
C
  190 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  COMPUTE THE STANDARD DEVIATION.  **
C               ***************************************
C
      DN=N
      DSUM=0.0D0
      DO200I=1,N
      DX=X(I)
      DSUM=DSUM+DX
  200 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO300I=1,N
      DX=X(I)
      DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  COMPUTE THE CPU RATIO                       **
C               **************************************************
C
      DUSL=ENGUSL
      DLSL=ENGLSL
C
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
C
      DNUM=DUPPER
C
      DDEN=3.0*DSD
C
      DCPU=0.0D0
      IF(DDEN.GT.0.0D0)DCPU=DNUM/DDEN
      XCPU=DCPU
C
      AN=REAL(N)
      P=0.975
      CALL NORPPF(P,PPF)
      XLCL=0.0
      XUCL=0.0
      IF(N.GT.1)THEN
        XLCL=XCPU - PPF*SQRT((1.0/(9.0*AN)) + XCPU/(2.0*(AN-1.0)))
        XUCL=XCPU + PPF*SQRT((1.0/(9.0*AN)) + XCPU/(2.0*(AN-1.0)))
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,XCPU
  811 FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CPU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DMEAN
 9014 FORMAT('DMEAN = ',D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)DSD
 9015 FORMAT('DSD = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)DUSL,DLSL,DUPPER,DLOWER
 9016 FORMAT('DUSL,DLSL,DUPPER,DLOWER = ',4D15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)DNUM,DDEN,DCPU,XCPU
 9017 FORMAT('DNUM,DDEN,DCPU,XCPU = ',3D15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CPZERO(IN,A,R,T,IFLG,S)
C***BEGIN PROLOGUE  CPZERO
C***DATE WRITTEN   810223   (YYMMDD)
C***REVISION DATE  860227   (YYMMDD)
C***CATEGORY NO.  F1A1B
C***KEYWORDS  COMPLEX,POLYNOMIAL ROOTS,ROOTS,ZEROES,ZEROS
C***AUTHOR  KAHANER, D. K., (NBS)
C***PURPOSE  Find the zeros of a polynomial with complex coefficients.
C***DESCRIPTION
C
C      Find the zeros of the complex polynomial
C         P(Z)= A(1)*Z**N + A(2)*Z**(N-1) +...+ A(N+1)
C
C    Input...
C       IN = degree of P(Z)
C       A = complex vector containing coefficients of P(Z),
C            A(I) = coefficient of Z**(N+1-i)
C       R = N word complex vector containing initial estimates for zeros
C            if these are known.
C       T = 4(N+1) word array used for temporary storage
C       IFLG = flag to indicate if initial estimates of
C              zeros are input.
C            If IFLG .EQ. 0, no estimates are input.
C            If IFLG .NE. 0, the vector R contains estimates of
C               the zeros
C       ** WARNING ****** If estimates are input, they must
C                         be separated, that is, distinct or
C                         not repeated.
C       S = an N word array
C
C    Output...
C       R(I) = Ith zero,
C       S(I) = bound for R(I) .
C       IFLG = error diagnostic
C    Error Diagnostics...
C       If IFLG .EQ. 0 on return, all is well
C       If IFLG .EQ. 1 on return, A(1)=0.0 or N=0 on input
C       If IFLG .EQ. 2 on return, the program failed to coverge
C                after 25*N iterations.  Best current estimates of the
C                zeros are in R(I).  Error bounds are not calculated.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CPEVL
C***END PROLOGUE  CPZERO
C
CCCCC APRIL 1996.  MAKE DUMMY DIMENSION "*"
CCCCC REAL  S(1)
CCCCC COMPLEX R(1),T(1),A(1),PN,TEMP
      REAL  S(*)
      COMPLEX R(*),T(*),A(*),PN,TEMP,PNTEMP(1),TEMP2(1)
C***FIRST EXECUTABLE STATEMENT  CPZERO
      IF( IN .LE. 0 .OR. CABS(A(1)) .EQ. 0.0 ) GO TO 30
C
C       CHECK FOR EASILY OBTAINED ZEROS
C
      N=IN
      N1=N+1
      IF(IFLG .NE. 0) GO TO 14
    1 N1=N+1
      IF(N .GT. 1) GO TO 2
         R(1)=-A(2)/A(1)
         S(1)=0.0
         RETURN
    2 IF( CABS(A(N1)) .NE. 0.0 ) GO TO 3
         R(N)=0.0
         S(N)=0.0
         N=N-1
         GO TO 1
C
C          IF INITIAL ESTIMATES FOR ZEROS NOT GIVEN, FIND SOME
C
    3 TEMP=-A(2)/(A(1)*FLOAT(N))
      CALL CPEVL(N,N,A,TEMP,T,T,.FALSE.)
      IMAX=N+2
      T(N1)=CABS(T(N1))
      DO 6 I=2,N1
         T(N+I)=-CABS(T(N+2-I))
         IF(REAL(T(N+I)) .LT. REAL(T(IMAX))) IMAX=N+I
    6 CONTINUE
      X=(-REAL(T(IMAX))/REAL(T(N1)))**(1./FLOAT(IMAX-N1))
    7 X=2.*X
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PNTEMP,PNTEMP,.FALSE.)
         PN=PNTEMP(1)
      IF (REAL(PN).LT.0.) GO TO 7
      U=.5*X
      V=X
   10 X=.5*(U+V)
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PNTEMP,PNTEMP,.FALSE.)
         PN=PNTEMP(1)
         IF (REAL(PN).GT.0.) V=X
         IF (REAL(PN).LE.0.) U=X
         IF((V-U) .GT. .001*(1.+V)) GO TO 10
      DO 13 I=1,N
         U=(3.14159265/FLOAT(N))*(.5+2.*FLOAT(I-1))
   13    R(I)=AMAX1(X,.001*CABS(TEMP))*CMPLX(COS(U),SIN(U))+TEMP
C
C          MAIN ITERATION LOOP STARTS HERE
C
   14 NR=0
      NMAX=25*N
      DO 19 NIT=1,NMAX
         DO 18 I=1,N
            IF(NIT .NE. 1 .AND. CABS(T(I)) .EQ. 0.) GO TO 18
               CALL CPEVL(N,0,A,R(I),PNTEMP,TEMP2,.TRUE.)
               PN=PNTEMP(1)
               TEMP=TEMP2(1)
               IF(ABS(REAL(PN))+ABS(AIMAG(PN)) .GT. REAL(TEMP)+
     1              AIMAG(TEMP)) GO TO 16
                  T(I)=0.0
                  NR=NR+1
                  GO TO 18
   16          TEMP=A(1)
               DO 17 J=1,N
   17             IF(J .NE. I) TEMP=TEMP*(R(I)-R(J))
               T(I)=PN/TEMP
   18    CONTINUE
         DO 15 I=1,N
   15       R(I)=R(I)-T(I)
         IF(NR .EQ. N) GO TO 21
   19 CONTINUE
      GO TO 26
C
C          CALCULATE ERROR BOUNDS FOR ZEROS
C
   21 DO 25 NR=1,N
         CALL CPEVL(N,N,A,R(NR),T,T(N+2),.TRUE.)
         X=CABS(CMPLX(ABS(REAL(T(1))),ABS(AIMAG(T(1))))+T(N+2))
         S(NR)=0.0
         DO 23 I=1,N
            X=X*FLOAT(N1-I)/FLOAT(I)
            TEMP=CMPLX(AMAX1(ABS(REAL(T(I+1)))-REAL(T(N1+I)),0.0),
     1           AMAX1(ABS(AIMAG(T(I+1)))-AIMAG(T(N1+I)),0.0))
   23       S(NR)=AMAX1(S(NR),(CABS(TEMP)/X)**(1./FLOAT(I)))
   25    S(NR)=1./S(NR)
         IFLG=0
      RETURN
C        ERROR EXITS
   26 IFLG=2
      RETURN
   30 IFLG=1
      RETURN
      END
      SUBROUTINE CRAMER(Y1,Y2,N,IWRITE,XIDTEM,XIDTE2,TEMP1,STAT,
     1           IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES CRAMER'S COEFFICIENT
C              FOR RXC CONTINGENCY TABLES.  THIS IS
C
C                  SQRT(T/(N*(Q-1)))
C
C              WHERE
C
C                  T = CHI-SQUARE STATISTIC
C                    = SUM[i=1 to r][SUM[j=1 to c]
C                      [(O(ij)-E(ij))**2/E(ij)]]
C
C                      O = OBSERVED COUNT
C                      E = EXPECTED COUNT
C                        = ROW TOTAL*COL TOTAL/GRAND TOTAL
C
C                  N = TOTAL NUMBER OF OBSERVATIONS
C                  Q = MIN(R,C)
C
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, WILEY, PP. 229-230.
C     NOTE--THIS SUBROUTINE HANDLES THE RAW DATA CASE.  USE
C           THE COMMAND
C
C               LET A = MATRIX CRAMER CONTINGENCY COEFFICENT M
C
C           IF YOUR DATA CONSISTS OF AN RXC TABLE.
C     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE FIRST SET
C                                OF DATA.
C                     --Y2     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED) OBSERVATIONS
C                                WHICH CONSTITUTE THE SECOND SET
C                                OF DATA.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X, OR EQUIVALENTLY,
C                                THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
C                                CRAMER'S CONTINGENCY COEFFICIENT
C                                BETWEEN THE 2 SETS OF DATA
C                                IN THE INPUT VECTORS X AND Y.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CRAMER'S CONTINGENCY COEFFICENT BETWEEN THE
C             2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      PARAMETER(MAXLEV=20000)
      PARAMETER(IWORK1=0)
      PARAMETER(IWORK2=20000)
      PARAMETER(IWORK3=40000)
      PARAMETER(IWORK4=60000)
      PARAMETER(IWORK5=80000)
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CRAM'
      ISUBN2='ER  '
C
      IERROR='NO'
C
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CRAMER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N
   53   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
 1201   FORMAT('****** ERROR IN CRAMER CONTINGENCY COEFFICIENT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N
 2103   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
C               ******************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)MAXLEV
 2202   FORMAT('      NUMBER OF SETS FOR VARIABLE ONE IS OUTSIDE ',
     1         'THE INTERVAL (1,',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)NUMSE1
 2204   FORMAT('      THE NUMBER OF SET = ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NUMSE2.LT.1 .OR. NUMSE2.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)MAXLEV
 2212   FORMAT('      NUMBER OF SETS FOR VARIABLE TWO IS OUTSIDE ',
     1         'THE INTERVAL (1,',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)NUMSE2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ***********************************************
C               **  STEP 2.3--                               **
C               **  COMPUTE THE CHI-SQUARE STATISTIC         **
C               ***********************************************
C
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE COUNTS FOR EACH CELL
C
      J=0
      DO2310ISET1=1,NUMSE1
        DO2320ISET2=1,NUMSE2
C
          K=0
          DO2330I=1,N
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
              K=K+1
            ENDIF
 2330     CONTINUE
          NTEMP=K
          J=J+1
          TEMP1(IWORK1+J)=REAL(K)
          TEMP1(IWORK2+J)=XIDTEM(ISET1)
          TEMP1(IWORK3+J)=XIDTE2(ISET2)
C
 2320   CONTINUE
 2310 CONTINUE
      NTEMP2=J
C
C     COMPUTE ROW AND COLUMN TOTALS AND GRAND TOTAL.
C
      J=0
      GTOTAL=0.0
C
      DO2340ISET1=1,NUMSE1
        TEMP1(IWORK4+ISET1)=0.0
        DO2350ISET2=1,NUMSE2
          J=J+1
          TEMP1(IWORK4+ISET1)=TEMP1(IWORK4+ISET1) + TEMP1(IWORK1+J)
          GTOTAL=GTOTAL + TEMP1(IWORK1+J)
 2350   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2352)ISET1,TEMP1(IWORK4+ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
C
      DO2360ISET2=1,NUMSE2
        TEMP1(IWORK5+ISET2)=0.0
        DO2370J=1,NTEMP2
          IF(TEMP1(IWORK3+J).EQ.XIDTE2(ISET2))THEN
            TEMP1(IWORK5+ISET2)=TEMP1(IWORK5+ISET2) + TEMP1(IWORK1+J)
          ENDIF
 2370   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2372)ISET2,TEMP1(IWORK5+ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2360 CONTINUE
C
C     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
C
      STAT=0.0
      J=0
C
      DO2380ISET1=1,NUMSE1
        DO2390ISET2=1,NUMSE2
          J=J+1
          EXP=TEMP1(IWORK4+ISET1)*TEMP1(IWORK5+ISET2)/GTOTAL
          STAT=STAT + (TEMP1(IWORK1+J) - EXP)**2/EXP
 2390   CONTINUE
 2380 CONTINUE
      T=STAT
      Q=REAL(MIN(NUMSE1,NUMSE2))
      STAT=STAT/(GTOTAL*(Q-1.0))
      STAT=SQRT(STAT)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CRAMER CONTINGENCY COEFFICIENT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CRAMER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)T,GTOTAL,Q,STAT
 9015   FORMAT('T,GTOTAL,Q,STAT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CRAME2(XMAT,MAXOBV,NR1,NC1,IWRITE,
     1           TEMP1,STAT,
     1           IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES CRAMER'S COEFFICIENT
C              FOR RXC CONTINGENCY TABLES.  THIS IS
C
C                  SQRT(T/(N*(Q-1)))
C
C              WHERE
C
C                  T = CHI-SQUARE STATISTIC
C                    = SUM[i=1 to r][SUM[j=1 to c]
C                      [(O(ij)-E(ij))**2/E(ij)]]
C
C                      O = OBSERVED COUNT
C                      E = EXPECTED COUNT
C                        = ROW TOTAL*COL TOTAL/GRAND TOTAL
C
C                  N = TOTAL NUMBER OF OBSERVATIONS
C                  Q = MIN(R,C)
C
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, WILEY, PP. 229-230.
C     NOTE--THIS SUBROUTINE HANDLES THE SUMMARY DATA CASE (I.E..
C           THE DATA IS GIVEN AS AN RXC TABLE).   THE "CRAMER"
C           SUBROUTINE IS USED FOR THE RAW DATA CASE.
C     INPUT  ARGUMENTS--XMAT   = THE SINGLE PRECISION MATRIX OF
C                                OBSERVATIONS (RXC TABLE)
C                     --MAXOBV = THE INTEGER NUMBER THAT SPECIFIES
C                                THE MAXIMUM NUMBER OF ROWS IN THE
C                                MATRIX.
C                     --NR1    = THE INTEGER NUMBER OF ROWS
C                                IN THE MATRIX XMAT.
C                     --NC1    = THE INTEGER NUMBER OF COLUMNS
C                                IN THE MATRIX XMAT.
C     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
C                                CRAMER'S CONTINGENCY COEFFICIENT
C                                OF THE DATA IN THE MATRIX XMAT.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE CRAMER'S CONTINGENCY COEFFICENT OF THE DATA
C             IN THE MATRIX XMAT.
C     RESTRICTIONS--THE MAXIMUM NUMBER OF LEVELS IS 50,000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      PARAMETER(MAXLEV=50000)
      PARAMETER(IWORK1=0)
      PARAMETER(IWORK2=50000)
C
      DIMENSION XMAT(MAXOBV,NC1)
      DIMENSION TEMP1(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CRAM'
      ISUBN2='ER  '
C
      IERROR='NO'
C
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CRAME2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXOBV,NR1,NC1
   53   FORMAT('MAXOBV,NR1,NC1 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NR1
          DO60J=1,NC1
            WRITE(ICOUT,56)I,J,XMAT(I,J)
   56       FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   60     CONTINUE
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NR1.LT.2 .OR. NR1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
 1201   FORMAT('****** ERROR IN MATRIX CRAMER CONTINGENCY ',
     1         'COEFFICIENT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX IS LESS ',
     1         'THAN 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2102)MAXLEV
 2102   FORMAT('      OR GREATER THAN ',I10,'.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)NR1
 2103   FORMAT('NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NC1.LT.2 .OR. NC1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2111)
 2111   FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX IS LESS ',
     1         'THAN 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2102)MAXLEV
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2113)NC1
 2113   FORMAT('NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      GTOTAL=0.0
      DO2120J=1,NC1
        DO2130I=1,NR1
          ITEMP=INT(XMAT(I,J)+0.5)
          IF(ITEMP.LT.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2131)
 2131       FORMAT('      A NEGATIVE COUNT WAS ENCOUNTERED IN THE ',
     1             'INPUT MATRIX.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2133)I,J,ITEMP
 2133       FORMAT('      COUNT FOR ROW ',I8,' COLUMN ',I8,' = ',I8)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GOTO9000
          ENDIF
          XMAT(I,J)=REAL(ITEMP)
          GTOTAL=GTOTAL + XMAT(I,J)
 2130   CONTINUE
 2120 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2344)GTOTAL
 2344   FORMAT('GTOTAL = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  COMPUTE THE ROW AND COLUMN TOTALS.              **
C               ******************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2340ISET1=1,NR1
        TEMP1(IWORK1+ISET1)=0.0
        DO2350ISET2=1,NC1
          TEMP1(IWORK1+ISET1)=TEMP1(IWORK1+ISET1) + XMAT(ISET1,ISET2)
          IF(IBUGA3.EQ.'ON')THEN
            WRITE(ICOUT,2342)ISET1,ISET2,XMAT(ISET1,ISET2)
 2342       FORMAT('ISET1,ISET2,XMAT(I,J) =',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
 2350   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2352)ISET1,TEMP1(IWORK1+ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
C
      DO2360ISET2=1,NC1
        TEMP1(IWORK2+ISET2)=0.0
        DO2370ISET1=1,NR1
          TEMP1(IWORK2+ISET2)=TEMP1(IWORK2+ISET2) + XMAT(ISET1,ISET2)
 2370   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2372)ISET2,TEMP1(IWORK2+ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2360 CONTINUE
C
C               ******************************************************
C               **  STEP 2.3--                                      **
C               **  COMPUTE THE CHI-SQUARE STATISTIC.               **
C               ******************************************************
C
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
C
      STAT=0.0
C
      DO2380ISET1=1,NR1
        DO2390ISET2=1,NC1
          EXP=TEMP1(IWORK1+ISET1)*TEMP1(IWORK2+ISET2)/GTOTAL
          STAT=STAT + (XMAT(ISET1,ISET2) - EXP)**2/EXP
 2390   CONTINUE
 2380 CONTINUE
      T=STAT
      Q=REAL(MIN(NR1,NC1))
      STAT=STAT/(GTOTAL*(Q-1.0))
      STAT=SQRT(STAT)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'OFF')GOTO890
      IF(IWRITE.EQ.'OFF')GOTO890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CRAMER CONTINGENCY COEFFICIENT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CRAME2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)T,GTOTAL,Q,STAT
 9015   FORMAT('T,GTOTAL,Q,STAT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      FUNCTION CSEVL (X, CS, N)
C***BEGIN PROLOGUE  CSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      SINGLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  CSEVL
      REAL B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  CSEVL
      IF (FIRST) ONEPL = 1.0E0 + R1MACH(4)
      FIRST = .FALSE.
C
      IF (N .LT. 1) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        CSEVL = 0.0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM CSEVL.  THE NUMBER OF TERMS IS ')
   12 FORMAT('      LESS THAN OR EQUAL TO ZERO.                *****')
      IF (N .GT. 1000) THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        CSEVL = 0.0
        RETURN
      ENDIF
   21 FORMAT('***** ERROR FROM CSEVL.  THE NUMBER OF TERMS IS ')
   22 FORMAT('      GREATER THAN 1000.                         *****')
      IF (ABS(X) .GT. ONEPL) THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   31 FORMAT('***** WARNING FROM CSEVL.  X IS OUTSIDE THE ')
   32 FORMAT('      INTERVAL (-1,+1).                          *****')
C
      B1 = 0.0E0
      B0 = 0.0E0
      TWOX = 2.0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      CSEVL = 0.5E0*(B0-B2)
C
      RETURN
      END
      SUBROUTINE CUMAVE(X,NX,IWRITE,Y,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE AVERAGE (MEAN) OF AN ARRAY
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DOUBLE PRECISION DSUM
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMA'
      ISUBN2='VE  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CUMAVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NX
   53 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NX
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  COMPUTE CUMULATIVE AVERAGE      **
C               **************************************
C
      Y(1)=X(1)
      IF(NX.LT.2)GOTO9000
      DSUM=DBLE(Y(1))
      DO100I=2,NX
        DSUM=DSUM + DBLE(X(I))
        Y(I)=REAL(DSUM/DBLE(I))
  100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CUMAVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX
 9013 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CUMHAZ(X,TAG,NX,IWRITE,Y,XTEMP,MAXOBV,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE HAZARD OF AN ARRAY
C              THE TAG VARIABLE IDENTIFIES CENSORED DATA
C              (1 = FAILURE TIME, 0 = CENSORED)
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --JANUARY   2007. ARGUMENT LIST TO RANK
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DOUBLE PRECISION DSUM
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMH'
      ISUBN2='AZ  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CUMHAZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NX
   53 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NX
      WRITE(ICOUT,56)I,X(I),TAG(I)
   56 FORMAT('I,X(I), TAG(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  COMPUTE CUMULATIVE HAZARD       **
C               **************************************
C
      CALL SORTC(X,TAG,NX,Y,TAG)
      CALL RANK(Y,NX,IWRITE,Y,XTEMP,MAXOBV,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      AFACT=REAL(NX+1)
      DO100J=1,NX
        IF(ABS(TAG(J)).GE.0.5)THEN
          Y(J)=100./(AFACT - Y(J))
        ELSE
          Y(J)=0.0
        ENDIF
  100 CONTINUE
C
      DSUM=0.0D0
      DO200I=1,NX
        DSUM=DSUM+DBLE(Y(I))
        Y(I)=REAL(DSUM)
  200 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CUMHAZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX
 9013 FORMAT('NX = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CUMINT(Y,X,N,NUMVAR,IWRITE,Z,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE INTEGRAL OF A VARIABLE.
C     NOTE--IF THE VERTICAL AXIS VARIABLE IS Y(.)
C           AND THE HORIZONTAL AXIS VARIABLE IS X(.),
C           THEN THE OUTPUT VARIABLE CONTAINING THE
C           CUMULATIVE INTEGRAL
C           WILL BE COMPUTED AS FOLLOWS--
C              Z(1) = 0
C              Z(2) = Z(1) + (Y(2)-Y(1))*(X(2)-X(1))/2
C              Z(3) = Z(2) + Y(2)*(X(3)-X(2)) + (Y(3)-Y(2))*(X(3)-X(2))/2
C              ETC.
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Z(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.)
C           OR THE INPUT VECTORS X(.) AND Y(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1979.
C     UPDATED         --APRIL     1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Z(*)
C
      DOUBLE PRECISION DINT
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DYI
      DOUBLE PRECISION DXIM1
      DOUBLE PRECISION DYIM1
      DOUBLE PRECISION DDELX
      DOUBLE PRECISION DDELY
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMI'
      ISUBN2='NT  '
C
      IERROR='NO'
C
      DXI=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CUMINT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N,NUMVAR
   53 FORMAT('N,NUMVAR = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I),Y(I)
   56 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ****************************************************
C               **  CUMPUTE THE CUMULATIVE (NUMERICAL) INTEGRAL.  **
C               ****************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DINT=0.0D0
      IF(N.LT.1)GOTO150
      IF(N.EQ.1)GOTO190
      I=1
      IF(NUMVAR.EQ.1)DXI=I
      IF(NUMVAR.EQ.2)DXI=X(I)
      DYI=Y(1)
      Z(1)=0.0
      DO100I=2,N
      DXIM1=DXI
      DYIM1=DYI
      IF(NUMVAR.EQ.1)DXI=I
      IF(NUMVAR.EQ.2)DXI=X(I)
      DYI=Y(I)
      DDELX=DXI-DXIM1
      DDELY=DYI-DYIM1
      DTERM1=DYIM1*DDELX
      DTERM2=DDELY*DDELX/2.0D0
      DINT=DINT+DTERM1+DTERM2
      Z(I)=DINT
  100 CONTINUE
      GOTO190
C
  150 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,151)
  151 FORMAT('***** ERROR IN CUMINT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)
  152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,153)
  153 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,154)
  154 FORMAT('      THE CUMULATIVE INTEGRAL IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,155)
  155 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,156)
  156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,157)N
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
  190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CUMINT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N,NUMVAR
 9013 FORMAT('N,NUMVAR = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I),Y(I),Z(I)
 9016 FORMAT('I,X(I),Y(I),Z(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE CUMMAX(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE MAXIMUM OF A VARIABLE
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/12
C     ORIGINAL VERSION--DECEMBER  2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMM'
      ISUBN2='IN  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMAX')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMMAX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************
C               **  COMPUTE CUMULATIVE MAXIMUM.  **
C               ***********************************
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN CUMULATIVE MAXIMUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)
  153   FORMAT('      RESPONSE VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
C
      ELSE
C
        Y(1)=X(1)
        YMAX=Y(1)
        DO100I=1,N
          IF(X(I).GT.YMAX)THEN
            Y(I)=X(I)
            YMAX=Y(I)
          ELSE
            Y(I)=YMAX
          ENDIF
  100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMAX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMMAX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CUMMIN(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE MINIMUM OF A VARIABLE
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/12
C     ORIGINAL VERSION--DECEMBER  2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMM'
      ISUBN2='IN  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMIN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMMIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************
C               **  COMPUTE CUMULATIVE MINIMUM.  **
C               ***********************************
C
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN CUMULATIVE MINIMUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)
  153   FORMAT('      RESPONSE VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
C
      ELSE
C
        Y(1)=X(1)
        YMIN=Y(1)
        DO100I=1,N
          IF(X(I).LT.YMIN)THEN
            Y(I)=X(I)
            YMIN=Y(I)
          ELSE
            Y(I)=YMIN
          ENDIF
  100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMIN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMMIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CUMPRO(X,N,IWRITE,Y,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE PRODUCT OF A VARIABLE--
C              Y(1) = X(1)
C              Y(2) = X(1) * X(2)
C              Y(3) = X(1) * X(2) * X(3)
C              ETC.
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DX
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMP'
      ISUBN2='RO  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMPRO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ***********************************
C               **  COMPUTE CUMULATIVE PRODUCT.  **
C               ***********************************
C
      DPROD=1.0D0
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN CUMULATIVE PRODUCT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)
  153   FORMAT('      RESPONSE VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
      ELSE
        DO100I=1,N
          DX=X(I)
          DPROD=DPROD*DX
          Y(I)=DPROD
  100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMPRO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE CUMSTA(Y1,Y2,Y3,N,NUMV,ICASS7,MAXNXT,
     1                  ISEED,IQUAME,IQUASE,PSTAMV,ICSTSV,
     1                  TEMP1,TEMP2,TEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  YOUT,NOUT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE A "CUMULATIVE" STATISTIC.  ALTHOUGH THIS IS TYPICALLY
C              USED FOR A LOCATION STATISTIC, IN CAN BE USED FOR ANY
C              SUPPORTED STATISTIC.  NOTE THAT A FEW SPECIFIC STATISTICS
C              ARE GENERATED SEPARATELY FROM THIS SUBROUTINE.  THESE
C              ARE TYPICALLY GENERATED MORE EFFICIENTLY THAN THIS ROUTINE
C              WHICH SIMPLY LOOPS THROUGH THE ARRAY AND CALLS CMPSTA TO
C              COMPUTE THE STATISTIC.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2013/01
C     ORIGINAL VERSION--JANUARY     2013.
C     UPDATED         --MARCH       2013. CUMULATIVE STATISTIC START
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASS7
      CHARACTER*4 IQUAME
      CHARACTER*4 IQUASE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION YOUT(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMS'
      ISUBN2='TA  '
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MSTA')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF CUMSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASS7,N,ICSTSV
   71   FORMAT('ICASS7,N,ICSTSV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO75I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),Y3(I)
   73     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN CUMULATIVE <STAT> COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 1--LOOP THROUGH AND COMPUTE THE STATISTIC  **
C               ******************************************************
C
C     MARCH 2013: SOME STATISTICS REQUIRE A MINIMUM NUMBER OF VALUES
C                 IN ORDER TO COMPUTE.  USER CAN ENTER THE COMMAND
C
C                    SET CUMULATIVE STATISTIC START <IVAL>
C
C                 TO SPECIFY A MINIMUM NUMBER OF VALUES BEFORE START
C                 COMPUTING THE STATISTIC.
C
      NOUT=0
      ISTRT=ICSTSV
      IF(ISTRT.LT.1 .OR. ISTRT.GT.N)ISTRT=1
      DO1010I=ISTRT,N
        NTEMP=I
        CALL CMPSTA(Y1,Y2,Y3,TEMP1,TEMP2,TEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,NUMV,ICASS7,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NOUT=NOUT+1
        YOUT(NOUT)=STAT
 1010 CONTINUE
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MSTA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NOUT
 9013   FORMAT('NOUT = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NOUT.GE.1)THEN
          DO9021I=1,NOUT
            WRITE(ICOUT,9023)I,YOUT(I)
 9023       FORMAT('I,YOUT(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE CUMSUM(X,N,IWRITE,Y,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE SUM OF A VARIABLE--
C              Y(1) = X(1)
C              Y(2) = X(1) + X(2)
C              Y(3) = X(1) + X(2) + X(3)
C              ETC.
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1979.
C     UPDATED         --APRIL     1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUMS'
      ISUBN2='UM  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMSUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N
   52   FORMAT('IBUGA3,N = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************
C               **  COMPUTE CUMULATIVE SUM.  **
C               *******************************
C
      DSUM=0.0D0
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN CUMULATIVE SUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)
  153   FORMAT('      RESPONSE VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
      ELSE
        DO100I=1,N
          DX=X(I)
          DSUM=DSUM+DX
          Y(I)=DSUM
  100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMSUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE cumtnc(t,df,pnonc,cum,ccum)
C
C     2017/01: THIS ROUTINE IS FROM THE DCDFLIB LIBRARY OF BARRY BROWN,
C              JAMES LAVATO, AND KATHY RUSSELL.
C
C              THE MAIN MODIFICATION OF THIS ROUTINE IS TO REPLACE
C              GAMLN, CUMT, CUMNOR, AND BRATIO WITH ROUTINES THAT ARE
C              ALREADY IN DATAPLOT.
C
C**********************************************************************
C
C     SUBROUTINE CUMTNC(T,DF,PNONC,CUM,CCUM)
C
C                 CUMulative Non-Central T-distribution
C
C
C                              Function
C
C
C     Computes the integral from -infinity to T of the non-central
C     t-density.
C
C
C                              Arguments
C
C
C     T --> Upper limit of integration of the non-central t-density.
C                                                  T is DOUBLE PRECISION
C
C     DF --> Degrees of freedom of the non-central t-distribution.
C                                                  DF is DOUBLE PRECISIO
C
C     PNONC --> Non-centrality parameter of the non-central t distibutio
C                                                  PNONC is DOUBLE PRECI
C
C     CUM <-- Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C     CCUM <-- Compliment of Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C     Upper tail    of  the  cumulative  noncentral t   using
C     formulae from page 532  of Johnson, Kotz,  Balakrishnan, Coninuous
C     Univariate Distributions, Vol 2, 2nd Edition.  Wiley (1995)
C
C     This implementation starts the calculation at i = lambda,
C     which is near the largest Di.  It then sums forward and backward.
C***********************************************************************
C     .. Parameters ..

      DOUBLE PRECISION one,zero,half,two,onep5
      PARAMETER (one=1.0d0,zero=0.0d0,half=0.5d0,two=2.0d0,onep5=1.5d0)
      DOUBLE PRECISION conv
      PARAMETER (conv=1.0d-7)
      DOUBLE PRECISION tiny
      PARAMETER (tiny=1.0d-10)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION ccum,cum,df,pnonc,t
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION alghdf,b,bb,bbcent,bcent,cent,d,dcent,dpnonc,
     +                 dum1,dum2,e,ecent,halfdf,lambda,lnomx,lnx,omx,
     +                 pnonc2,s,scent,ss,sscent,t2,term,tt,twoi,x,
     +                 xi,xlnd,xlne
      INTEGER ierr
      LOGICAL qrevs
C     ..
C     .. External Functions ..
CCCCC DOUBLE PRECISION gamln
CCCCC EXTERNAL gamln
      DOUBLE PRECISION DLNGAM
      EXTERNAL DLNGAM
C     ..
C     .. External Subroutines ..
CCCCC EXTERNAL bratio,cumnor,cumt
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,exp,int,log,max,min
C     ..

C     Case pnonc essentially zero

      IF (abs(pnonc).LE.tiny) THEN
ccccc     CALL cumt(t,df,cum,ccum)
          CALL tdcdf(t,df,cum)
          ccum=1.0d0 - cum
          RETURN

      END IF

      qrevs = t .LT. zero
      IF (qrevs) THEN
          tt = -t
          dpnonc = -pnonc

      ELSE
          tt = t
          dpnonc = pnonc
      END IF

      pnonc2 = dpnonc*dpnonc
      t2 = tt*tt

      IF (abs(tt).LE.tiny) THEN
ccccc     CALL cumnor(-pnonc,cum,ccum)
          CALL nodcdf(-pnonc,cum)
          ccum=1.0d0 - cum
          RETURN

      END IF

      lambda = half*pnonc2
      x = df/ (df+t2)
      omx = one - x

      lnx = log(x)
      lnomx = log(omx)

      halfdf = half*df
ccccc alghdf = gamln(halfdf)
      alghdf = DLNGAM(halfdf)

C     ******************** Case i = lambda

      cent = int(lambda)

      IF (cent.LT.one) cent = one

C     Compute d=T(2i) in log space and offset by exp(-lambda)

ccccc xlnd = cent*log(lambda) - gamln(cent+one) - lambda
      xlnd = cent*log(lambda) - dlngam(cent+one) - lambda

      dcent = exp(xlnd)

C     Compute e=t(2i+1) in log space offset by exp(-lambda)

ccccc xlne = (cent+half)*log(lambda) - gamln(cent+onep5) - lambda
      xlne = (cent+half)*log(lambda) - dlngam(cent+onep5) - lambda
      ecent = exp(xlne)

      IF (dpnonc.LT.zero) ecent = -ecent

C     Compute bcent=B(2*cent)

ccccc CALL bratio(halfdf,cent+half,x,omx,bcent,dum1,ierr)
      bcent=dbetai(x,halfdf,cent+half)

C     compute bbcent=B(2*cent+1)

ccccc CALL bratio(halfdf,cent+one,x,omx,bbcent,dum2,ierr)
      bbcent=dbetai(x,halfdf,cent+one)

C     Case bcent and bbcent are essentially zero
C     Thus t is effectively infinite

      IF ((bcent+bbcent).LT.tiny) THEN
          IF (qrevs) THEN
              cum = zero
              ccum = one

          ELSE
              cum = one
              ccum = zero
          END IF

          RETURN

      END IF

C     Case bcent and bbcent are essentially one
C     Thus t is effectively zero

      IF ((dum1+dum2).LT.tiny) THEN
ccccc     CALL cumnor(-pnonc,cum,ccum)
          CALL nodcdf(-pnonc,cum)
          ccum=1.0d0 - cum
          RETURN

      END IF

C     First term in ccum is D*B + E*BB

      ccum = dcent*bcent + ecent*bbcent

C     compute s(cent) = B(2*(cent+1)) - B(2*cent))

ccccc scent = gamln(halfdf+cent+half) - gamln(cent+onep5) - alghdf +
      scent = dlngam(halfdf+cent+half) - dlngam(cent+onep5) - alghdf +
     +        halfdf*lnx + (cent+half)*lnomx
      scent = exp(scent)

C     compute ss(cent) = B(2*cent+3) - B(2*cent+1)

ccccc sscent = gamln(halfdf+cent+one) - gamln(cent+two) - alghdf +
      sscent = dlngam(halfdf+cent+one) - dlngam(cent+two) - alghdf +
     +         halfdf*lnx + (cent+one)*lnomx
      sscent = exp(sscent)

C     ******************** Sum Forward

      xi = cent + one
      twoi = two*xi

      d = dcent

      e = ecent

      b = bcent

      bb = bbcent

      s = scent

      ss = sscent

   10 b = b + s
      bb = bb + ss

      d = (lambda/xi)*d
      e = (lambda/ (xi+half))*e

      term = d*b + e*bb

      ccum = ccum + term

      s = s*omx* (df+twoi-one)/ (twoi+one)

      ss = ss*omx* (df+twoi)/ (twoi+two)

      xi = xi + one
      twoi = two*xi

      IF (abs(term).GT.conv*ccum) GO TO 10

C     ******************** Sum Backward

      xi = cent
      twoi = two*xi

      d = dcent

      e = ecent

      b = bcent

      bb = bbcent

      s = scent* (one+twoi)/ ((df+twoi-one)*omx)

      ss = sscent* (two+twoi)/ ((df+twoi)*omx)

   20 b = b - s
      bb = bb - ss

      d = d* (xi/lambda)

      e = e* ((xi+half)/lambda)

      term = d*b + e*bb

      ccum = ccum + term

      xi = xi - one

      IF (xi.LT.half) GO TO 30

      twoi = two*xi

      s = s* (one+twoi)/ ((df+twoi-one)*omx)

      ss = ss* (two+twoi)/ ((df+twoi)*omx)

      IF (abs(term).GT.conv*ccum) GO TO 20

   30 CONTINUE

      IF (qrevs) THEN
          cum = half*ccum
          ccum = one - cum

      ELSE
          ccum = half*ccum
          cum = one - ccum
      END IF

C     Due to roundoff error the answer may not lie between zero and one
C     Force it to do so

      cum = max(min(cum,one),zero)
      ccum = max(min(ccum,one),zero)

      RETURN

      END
      SUBROUTINE CURVE (P, X, N0, N, EPS, MAXITR, MU, SIGMA, ITER,
     1   SEMU, SESIG, COVAR, E0, EX, CHISQ, 
     1   F, F1, XN,
     1   FUNC,
     1   IFAULT)
C 
C       ALGORITHM AS 95 APPL. STATIST. (1976) VOL.25, NO.1
C 
C       ESTIMATES MU AND SIGMA OF DISTRIBUTION FUNCTION
C       F( (X-MU)/SIGMA ) FROM A GROUPED SAMPLE OF X VALUES.
C       NOTE ON ARRAY SIZES
C       THE ARRAYS IN THE SECOND DIMENSION STATEMENT MUST HAVE
C       MINIMUM SIZE P.  IF P IS TO EXCEED 20, A SUITABLE SIZE
C       MUST BE SET FOR THEM, AND THE IF STATEMENT WHICH CHECKS
C       THE VALUE OF P MUST BE AMENDED.
C
C     Auxiliary routines required: FUNC & DEVIAT (both user-supplied)
C
      PARAMETER (MAXCLA=1000)
C
      INTEGER P
      REAL NN, NI, NP, MU, ONE, ZERO
      DIMENSION X(*), N(*), EX(*)
      DIMENSION F(*), F1(*), XN(*)
C
      EXTERNAL FUNC
C
      DATA RR/1.0E-10/
      DATA ONE/1.0/
      DATA ZERO/0.0/
C 
C       ERROR EXIT IF P TOO SMALL OR TOO LARGE
C 
      IF (P.LT.2 .OR. P.GT.MAXCLA) THEN
         IFAULT = 1
         GOTO9000
      END IF
C
      IFAULT = 0
C 
C       SET FREQUENCIES IN FLOATING POINT
C 
      XN0 = N0
      NSUM = N0
      DO 10 I = 1, P
        XN(I) = N(I)
        NSUM = NSUM + N(I)
 10   CONTINUE
      K = P - 1
      XNSUM = REAL(NSUM)
      NP = XN(P)
C 
C       ITERATIVE APPROXIMATION
C 
      DO 40 ITER = 1, MAXITR
C 
C       COMPUTE VALUES OF DISTRIBUTION AND DENSITY FUNCTIONS,
C       USING CURRENT VALUES OF MU, SIGMA
C 
        DO 20 I = 1, P
          CALL FUNC ((X(I) - MU)/SIGMA, F(I), F1(I))
 20     CONTINUE
        DM = ONE - F(P)
C
C
C       TEST FOR SMALL DIVISOR TO AVOID OVERFLOW
C
        IF (ABS(DM).LT.RR) THEN
           IFAULT=2
           GO TO 9000
        ENDIF
C
        F1P = F1(P)
        IF (ABS(F(1)).LT.RR) THEN
           IFAULT=2
           GO TO 9000
        ENDIF
C
        XI1 = X(1) - MU
        XP = X(P) - MU
        R = F1(1)/F(1)
        S = F1P/DM
        T = -XN0*R
        U = NP*S
        A = T + U
        B = XI1*T + XP*U
        R = F1(1)*R
        S = F1P*S
        C = R + S
        R = XI1*S
        S = XP*S
        D = R + S
        E = XI1*R + XP*S
        DO 30 I = 1, K
          FI = F(I)
          FI1 = F(I + 1)
          F1I1 = F1(I + 1)
          F1I = F1(I)
          XI = XI1
          XI1 = X(I + 1) - MU
          NI = XN(I)
          R = FI1 - FI
C
          IF (ABS(R).LT.RR) THEN
             IFAULT=2
             GO TO 9000
          ENDIF
C
          S = F1I1 - F1I
          U = XI1*F1I1 - XI*F1I
          SR = S/R
          UR = U/R
          A = A - NI*SR
          B = B - NI*UR
          C = C + S*SR
          D = D + S*UR
          E = E + U*UR
 30     CONTINUE
        DENOM = (C*E - D*D)*XNSUM
C 
C       COMPUTE ADJUSTMENTS TO MU, SIGMA
C 
        SIGDEN = SIGMA/DENOM
        DMU = (E*A - D*B)*SIGDEN
        DSIGMA = (C*B - D*A)*SIGMA*SIGDEN
        MU = MU + DMU
        SIGMA = SIGMA + DSIGMA
        ERR = ABS(DMU) + ABS(DSIGMA)
C
C       TEST FOR CONVERGENCE
C
        IF (ERR.LT.EPS) GOTO50
 40   CONTINUE
C
C     SET FAULT IF LIMIT FOR NUMBER OF ITERATIONS IS
C     REACHED, THEN PROCEED
C
      IFAULT = 4
      ITER = MAXITR
C
 50   CONTINUE
      DO 60 I = 1, P
        CALL FUNC ((X(I) - MU)/SIGMA, F(I), DUM)
 60   CONTINUE
C 
C     COMPUTE VARIANCES AND COVARIANCE OF ESTIMATES
C 
      SIGDEN = SIGMA*SIGMA/DENOM
      VARMU = E*SIGDEN
      SIGDEN = SIGMA*SIGDEN
      COVAR = -D*SIGDEN
      VARSIG = C*SIGMA*SIGDEN
      IF (VARMU.LT.ZERO .OR. VARSIG.LT.ZERO) THEN
         IFAULT=3
         GO TO 9000
      ENDIF
C
      SEMU = SQRT(VARMU)
      SESIG = SQRT(VARSIG)
C 
C       COMPUTE EXPECTED FREQUENCIES AND CHI SQUARE
C 
      E0 = XNSUM*F(1)
      EP = XNSUM*(ONE - F(P))
      EX(P) = EP
      CHISQ = ((XN0 - E0)**2)/E0 + ((NP - EP)**2)/EP
      DO 70 I = 1, K
        NN = XNSUM*(F(I+1) - F(I))
        CHISQ = CHISQ + ((NN - XN(I))**2)/NN
        EX(I) = NN
 70   CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE CUSARL(X,NX,IWRITE,Y,ICASE,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE CUMULATIVE SUM ARL.
C              USE APPLIED STATISTICS ALGORITHM AS 258.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/3
C     ORIGINAL VERSION--MARCH    1999.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CUSA'
      ISUBN2='RL  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF CUSARL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICASE,IBUGA3
   52 FORMAT('ICAE,IBUGA3 = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NX
   53 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NX
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *********************************************
C               **  CHECK FOR PARAMERERS: DELTA, S0, K, H  **
C               *********************************************
C
      IHP='S0  '
      IHP2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        S0=0.0
      ELSE
        S0=VALUE(ILOCP)
      ENDIF
C
      IHP='K   '
      IHP2='    '
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AK=VALUE(ILOCP)
C
      IF(AK.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16211)
16211 FORMAT('***** ERROR IN CUSARL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16212)
16212 FORMAT('      THE SPECIFIED PARAMETER K')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16213)
16213 FORMAT('      FOR THE CUMULATIVE SUM AVERAGE RUN LENGTH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16214)
16214 FORMAT('      MUST BE GREATER THAN OR EQUAL TO 0;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16215)
16215 FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16216)AK
16216 FORMAT('      THE SPECIFIED VALUE OF K = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IHP='H   '
      IHP2='    '
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AH=VALUE(ILOCP)
C
      IF(AH.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16311)
16311 FORMAT('***** ERROR IN CUSARL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16312)
16312 FORMAT('      THE SPECIFIED PARAMETER H')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16313)
16313 FORMAT('      FOR THE CUMULATIVE SUM AVERAGE RUN LENGTH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16314)
16314 FORMAT('      MUST BE GREATER THAN OR EQUAL TO 0;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16315)
16315 FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16316)AH
16316 FORMAT('      THE SPECIFIED VALUE OF K = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO100I=1,NX
        DELTA=X(I)
        IF(ICASE.EQ.'TWOS')THEN
          CALL ARL2(DELTA,AK,AH,S0,ARL,ARLFIR,IFAULT)
        ELSE
          CALL ARL1(DELTA,AK,AH,S0,ARL,ARLFIR,IFAULT)
        ENDIF
        IF(IFAULT.EQ.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,141)
  141 FORMAT('***** ERROR IN CUSARL--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,143)
  143 FORMAT('      ERROR IN INPUT ARGUMENTS TO ARL ROUTINE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(IFAULT.EQ.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,151)
  151 FORMAT('***** ERROR IN CUSARL--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,153)DELTA
  153 FORMAT('      FOR X = ',G15.7,', EQUATIONS ARE SINGULAR.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(IFAULT.EQ.3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,161)
  161 FORMAT('***** ERROR IN CUSARL--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,163)DELTA
  163 FORMAT('      FOR X = ',G15.7,', VALUE OF S0 IS TOO LARGE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(S0.GT.0.0)THEN
          Y(I)=ARLFIR
        ELSE
          Y(I)=ARL
        ENDIF
  100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF CUSARL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX
 9013 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      double precision function cvflow(cv)
      implicit double precision (a-h,o-z)
      common /cvc/ estcv,sqrtn,df,ratio,alphad2,omad2
c
      xncp = sqrtn/cv
ccccc call cdftnc(1,p,q,ratio,df,xncp,ier,bound)
      call nctcd2(ratio,df,xncp,p)
ccccc if (ier .ne. 0) then
ccccc    write(6,10)
ccccc    write(7,10)
c10      format(/,1x,'The ier value from a call',
cccccx   ' to cdftnc was nonzero.  Please contact',/,
cccccx   1x,'Steve Verrill at sverrill@fs.fed.us.',/)
ccccc    stop
ccccc endif
      cvflow = alphad2 - p
      return
      end
      double precision function cvfup(cv)
      implicit double precision (a-h,o-z)
      common /cvc/ estcv,sqrtn,df,ratio,alphad2,omad2
      xncp = sqrtn/cv
      call nctcd2(ratio,df,xncp,p)
ccccc if (ier .ne. 0) then
ccccc    write(6,10)
ccccc    write(7,10)
c10      format(/,1x,'The ier value from a call',
cccccx   ' to cdftnc was nonzero.  Please contact',/,
cccccx   1x,'Steve Verrill at sverrill@fs.fed.us.',/)
ccccc    stop
ccccc endif
      cvfup = omad2 - p
      return
      end

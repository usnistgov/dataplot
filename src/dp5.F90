      SUBROUTINE CNP(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,IWRITE,ICNPKD,   &
                     XCNP,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE NON-PARAMETRIC CP
!              (PROCESS CAPABILITY INDEX) OF THE DATA IN THE INPUT
!              VECTOR X.
!                 CNP = (ENGUSL - ENGLSL)/(P(.99865) - P(0.00135))
!           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
!           COMPARABLE TO THE NORMAL-BASED CP STATISTIC (+/- 3*SIGMA).
!           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND HAS THE
!           FORMULA
!                 CNP = (ENGUSL - ENGLSL)/(P(.995) - P(0.005))
!     NOTE--CP IS A MEASURE OF PROCESS PRECISION--
!           IT CONTAINS NO BIAS INFORMATION.
!     NOTE--THE CP INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CP WHICH ARE
!           LARGE (ABOVE 2); VALUES OF CP FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CP     = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CP
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CP INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-28999
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.4
!     ORIGINAL VERSION--APRIL     2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      REAL NUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CNP '
      ISUBN2='    '
      IERROR='NO'
!
      XCNP=0.0
      DMEAN=0.0D0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CP  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CNP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ****************************************
!               **  STEP 2--                          **
!               **  COMPUTE THE MEDIAN AND PERCENTILE **
!               **  POINTS                            **
!               ****************************************
!
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
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CNP RATIO                       **
!               **************************************************
!
      NUM=ENGUSL-ENGLSL
      IF(NUM.LE.0.0)NUM=0.0D0
      DEN=P995-P005
      IF(DEN.GT.0.0)XCNP=NUM/DEN
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
        WRITE(ICOUT,811)N,XCNP
  811   FORMAT('THE CNP OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
 9011   FORMAT('***** AT THE END       OF CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)NUM,DEN,XCNP
 9017   FORMAT('NUM,DEN,XCNP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CNP
      SUBROUTINE CNPK(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,IWRITE,ICNPKD,   &
                      XCNPK,   &
                      IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPK
!              (PROCESS CAPABILITY INDEX) OF THE DATA IN THE INPUT
!              VECTOR X.
!              CNPK = MIN(A,B)
!              WHERE:
!                  A = (USL-MEDIAN)/(P(.995)-MEDIAN)
!                  B = (MEDIAN-LSL)/(MEDIAN-P(.005))
!                  P = THE PERCENTILE FUNCTION
!     NOTE--SUPPORT OPTIONAL FORMULA THAT SEEMS TO BE MORE
!           PREVALENT IN THE LITERATURE:
!
!              CNPK = MIN(A,B)
!              WHERE:
!                  A = (USL-MEDIAN)/((P(.99865)-P(.00135))/2)
!                  B = (MEDIAN-LSL)/((P(.99865)-P(.00135))/2)
!                  P = THE PERCENTILE FUNCTION
!     NOTE--CNPK IS A MEASURE OF PROCESS ACCURACY--
!           COMBINING BOTH PRECISION AND UNBIASEDNESS.
!           IT IS A NON-PARAMETERIC METHOD FOR THE CPK STATISTIC
!           THAT IS RECOMMENDED WHEN THE DATA ARE NOT NORMAL.
!     NOTE--THE CNPK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CNPK
!           WHICH ARE LARGE (ABOVE 2);
!           VALUES OF CNPK FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CNPK    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CNPK
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CNPK INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--AFP 800-7. "USAF R&M 2000 PROCESS", DEPARTMENT OF
!                 THE AIR FORCE, HQ USAF, WASHINGTON, DC GPO, 1 OCT
!                 1988. (THIS ISN'T THE RIGHT DOCUMENT, NOT SURE
!                 WHICH DOCUMENT ACTUALLY HAS THE TECHNICAL DETAILS).
!               --PEARN, TAI, HSIAO, AND AO (2014), "APPROXIMATELY
!                 UNBIASED ESTIMATOR FOR NON-NORMAL PROCESS
!                 CAPABILITY INDEX", JOURNAL OF TESTING AND
!                 EVALUATION, VOL. 42, NO. 6, PP. 1-10.
!               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99.3
!     ORIGINAL VERSION--MARCH     1999.
!     UPDATED         --APRIL     2015. SUPPORT ALTERNATIVE DEFINITION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICNPKD
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CNPK'
      ISUBN2='    '
      IERROR='NO'
!
      DMEAN=0.0D0
      XCNPK=0.0
!
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
        DO 55 I=1,N
         WRITE(ICOUT,56)I,X(I)
   56    FORMAT('I,X(I) = ',I8,G15.7)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CNPK  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CNPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ****************************************
!               **  STEP 2--                          **
!               **  COMPUTE THE MEDIAN AND PERCENTILE **
!               **  POINTS                            **
!               ****************************************
!
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
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CNPK RATIO                      **
!               **************************************************
!
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
        WRITE(ICOUT,811)N,XCNPK
  811   FORMAT('THE CNPK OF THE ',I8,' OBSERVATIONS = ',   &
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
!
      RETURN
      END SUBROUTINE CNPK
      SUBROUTINE CNPM(X,N,XTEMP,MAXNXT,ENGLSL,ENGUSL,TARGET,IWRITE,   &
                      ICNPKD,XCNPM,   &
                      IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPM PROCESS
!              CAPABILITY INDEX OF THE DATA IN THE INPUT VECTOR X.
!              THIS IS A NON-PARAMETRIC VERSION OF THE CPM STATISTIC.
!
!                 CNPM = (USL - LSL)/
!                        [6*SQRT{((P(0.99865)-P(0.00135)/6)**2 +
!                        (MEDIAN - TARGET)**2}]
!
!           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
!           COMPARABLE TO THE NORMAL-BASED CPM STATISTIC (+/- 3*SIGMA).
!           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND USES
!           P(0.995) AND P(0.005).
!     NOTE--CNPM IS A MEASURE OF PROCESS ACCURACY--
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--XCNPM  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CNPM
!                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
!                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPM INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--MEAN AND SD.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NORMA HUBELE, ARIZONA STATE
!               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.04
!     ORIGINAL VERSION--APRIL     2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      REAL NUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CNPM'
      ISUBN2='    '
      IERROR='NO'
!
      XCNPM=0.0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CNPM **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CNPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ****************************************
!               **  STEP 2--                          **
!               **  COMPUTE THE MEDIAN AND PERCENTILE **
!               **  POINTS                            **
!               ****************************************
!
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
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CNPM RATIO                      **
!               **************************************************
!
      NUM=ABS(ENGUSL-ENGLSL)
      TERM1=(P995-P005)/6.0
      TERM2=(XMED-TARGET)**2
      DEN=6.0*SQRT(TERM1**2 + TERM2)
      IF(DEN.GT.0.0)XCNPM=NUM/DEN
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
        WRITE(ICOUT,811)N,XNCPM
  811   FORMAT('THE CNPM OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CNPM
      SUBROUTINE CNPMK(X,N,XTEMP1,MAXNXT,ENGLSL,ENGUSL,TARGET,IWRITE,   &
                       ICNPKD,XCNPMK,   &
                       IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CNPMK PROCESS
!              CAPABILITY INDEX OF THE DATA IN THE INPUT VECTOR X.
!              THIS IS A NON-PARAMETRIC VERSION OF THE CPMK STATISTIC.
!
!                CNPMK = min{(USL - MED,MED-LSL)}/
!                        [3*SQRT{((P(0.99865)-P(0.00135)/6)**2 +
!                        (MEDIAN - TARGET)**2}]
!
!           WHERE P(x) IS THE PERCENTILE FUNCTION.  THIS HAS COVERAGE
!           COMPARABLE TO THE NORMAL-BASED CPM STATISTIC (+/- 3*SIGMA).
!           AN ALTERNATIVE DEFINITION HAS 99% COVERAGE AND USES
!           P(0.995) AND P(0.005).
!     NOTE--CNPM IS A MEASURE OF PROCESS ACCURACY--
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--XCNPMK = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CNPMK
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPMK INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.04
!     ORIGINAL VERSION--APRIL     2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICNPKD
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      REAL NUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP1(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CNPM'
      ISUBN2='K   '
      IERROR='NO'
!
      XCNPMK=0.0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CNPM **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CNPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ****************************************
!               **  STEP 2--                          **
!               **  COMPUTE THE MEDIAN AND PERCENTILE **
!               **  POINTS                            **
!               ****************************************
!
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
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CNPM RATIO                      **
!               **************************************************
!
      UPPER=ENGUSL-XMED
      ALOWER=XMED-ENGLSL
      NUM=MIN(UPPER,ALOWER)
      TERM1=(P995-P005)/6.0
      TERM2=(XMED-TARGET)**2
      DEN=3.0*SQRT(TERM1**2 + TERM2)
      IF(DEN.GT.0.0)XCNPMK=NUM/DEN
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
        WRITE(ICOUT,811)N,XNCPMK
  811   FORMAT('THE CNPMK OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CNPMK
      SUBROUTINE COCODE(X,N,XREF,NREF,XPRIME,IBUGA3)
!
!     PURPOSE--THIS SUBROUTINE CO-CODES
!              THE N ELEMENTS OF THE SINGLE PRECISION VECTOR X,
!              AS DICTATED BY HOW X MATCHES XREF.
!     IN PARTICULAR, ALL ELEMENTS IN X THAT MATCH XREF(1)
!                    WILL GET CODED WITH 1.
!                    ALL ELEMENTS IN X THAT MATCH XREF(2)
!                    WILL GET CODED WITH 2.
!                    ETC.
!              THE OUTPUT IS, IN FACT, PLACED IN XPRIME.
!              (X AND XREF REMAIN UNCHANGED)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS TO BE CO-CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X AND XPRIME.
!                     --XREF   = THE SINGLE PRECISION VECTOR OF
!                                REFERENCE OBSERVATIONS.
!                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR XREF.
!     OUTPUT ARGUMENTS--XPRIME = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE RECODED DATA VALUES
!                                WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR XPRIME
!             CONTAINING THE RECODED VALUES.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     ORIGINAL VERSION--JULY      1991.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*),XREF(*),XPRIME(*)
      CHARACTER*4 IBUGA3
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)GO TO 50
      GO TO 90
   50 CONTINUE
      WRITE(ICOUT,15)
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',   &
      'SORTC  SUBROUTINE IS NON-POSITIVE *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
!
      IF(IBUGA3.NE.'ON')GO TO 190
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,110)
  110 FORMAT('***** AT THE BEGINNING OF COCODE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)N,NREF
  111 FORMAT('N,NREF = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO 112 I=1,N
      WRITE(ICOUT,113)I,X(I),XREF(I)
  113 FORMAT('I,X(I),XREF(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  190 CONTINUE
!
      DO 1100 I=1,N
      XPRIME(I)=-999
 1100 CONTINUE
!
      DO 1200 I=1,NREF
      XREFI=XREF(I)
      DO 1300 J=1,N
      IF(X(J).EQ.XREFI)XPRIME(J)=I
 1300 CONTINUE
 1200 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COCODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,NREF
 9012   FORMAT('N,NREF = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),XREF(I),XPRIME(I)
 9016     FORMAT('I,X(I),XREF(I),XPRIME(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE COCODE
      SUBROUTINE COCOPY(YREF,NREF,X,NX,XREF,Y,NY,IBUGA3)
!
!     PURPOSE--THIS SUBROUTINE CO-COPIES
!              THE NREF ELEMENTS OF THE SINGLE PRECISION
!              VECTOR YREF INTO THE (TYPICALLY) LONGER VECTOR Y.
!              AS DICTATED BY HOW X MATCHES XREF.
!     IN PARTICULAR, FOR ALL ELEMENTS IN X THAT MATCH XREF(1),
!                    Y WILL BECOME YREF(1).
!                    FOR ALL ELEMENTS IN X THAT MATCH XREF(2),
!                    Y WILL BECOME YREF(2).
!                    ETC.
!              THE OUTPUT IS, IN FACT, PLACED IN Y.
!              (X, XREF, AND YREF REMAIN UNCHANGED).
!     INPUT  ARGUMENTS--YREF   = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS TO BE CO-COPIED.
!                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR YREF (AND XREF).
!                     --X      = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS USED FOR MATCHING .
!                     --NX     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X (AND Y).
!                     --XREF   = THE SINGLE PRECISION VECTOR OF
!                                REFERENCE OBSERVATIONS.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE VARIOUS YREF VALUES
!                                WILL BE COPIED.
!                       NY     = THE INTEGER NUMBER OF ELEMENTS
!                                IN Y (= NX)
!     OUTPUT--THE SINGLE PRECISION VECTOR Y
!             CONTAINING THE COPIED VALUES.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     ORIGINAL VERSION--JULY      1991.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION YREF(*),X(*),XREF(*),Y(*)
      CHARACTER*4 IBUGA3
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(NX.LT.1)GO TO 50
      GO TO 90
   50 CONTINUE
      WRITE(ICOUT,15)
   15 FORMAT('***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
      'SORTC  SUBROUTINE IS NON-POSITIVE *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)NX
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      NY=NX
      RETURN
   90 CONTINUE
!
      IF(IBUGA3.NE.'ON')GO TO 190
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,110)
  110 FORMAT('***** AT THE BEGINNING OF COCOPY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)NREF,NX
  111 FORMAT('NREF,NX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      DO 112 I=1,NX
      WRITE(ICOUT,113)I,X(I),XREF(I),YREF(I)
  113 FORMAT('I,X(I),XREF(I),YREF(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  190 CONTINUE
!
      DO 1100 I=1,NX
      Y(I)=-999
 1100 CONTINUE
!
      DO 1200 I=1,NREF
      XREFI=XREF(I)
      DO 1300 J=1,NX
      IF(X(J).EQ.XREFI)Y(J)=YREF(I)
 1300 CONTINUE
 1200 CONTINUE
      NY=NX
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COCOPY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NREF,NX,NY
 9012   FORMAT('NREF,NX,NY = ',I8,I8,I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NX
          WRITE(ICOUT,9016)I,X(I),XREF(I),YREF(I)
 9016     FORMAT('I,X(I),XREF(I),YREF(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9020 I=1,NY
          WRITE(ICOUT,9021)I,Y(I)
 9021     FORMAT('I,Y(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE COCOPY
      SUBROUTINE CODCT2(X1,X2,N,ICCTOF,ICCTG1,IWRITE,   &
                        Y,XIDTEM,XIDTE2,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
!              CROSS TABULATION OF TWO GROUP-ID VARIABLES.  THIS
!              CAN BE USEFUL FOR COMMANDS OF THE FORM
!
!                  <COMMAND>  Y  X
!
!              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
!              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
!              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
!              A BOX PLOT OVER SEVERAL GROUPS.
!
!              THE CODING IS BASED ON THE FOLLOWING FORMULA:
!
!                  ICODE = OFFSET + (ISET1-1)*NGROUP2 + ISET2
!
!              WHERE
!
!                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
!                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
!                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
!                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
!
!              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
!              THE ICCTG1 PARAMETER CAN BE USED TO CONTROL THIS
!              (I.E., WE USE THE MAXIMUM OF NGROUP2 AND ICCTG1).
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIRST GROUP VARIABLE
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SECOND GROUP VARIABLE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X1 AND X2.
!                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE OFFSET.
!                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING BETWEEN GROUPS.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
!             X1 AND X2.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/6
!     ORIGINAL VERSION--JUNE      2009.
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
!CCCC INCLUDE 'DPCOPA.INC'
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODC'
      ISUBN2='T2  '
!
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I)
   56     FORMAT('I,X1(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  PERFORM THE CODING--                                 **
!               ***********************************************************
!
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
!
      IFACT1=MAX(NGRP2,ICCTG1)
!
      DO 100 I=1,N
!
        DO 200 J=1,NGRP1
          DO 300 K=1,NGRP2
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT2')THEN
              WRITE(ICOUT,301)I,J,K
  301         FORMAT('I,J,K = ',3I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),XIDTEM(J),XIDTE2(K)
  302         FORMAT('X1(I),X2(I),XIDTEM(J),XIDTE2(K)=',4G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K))THEN
              IINDX=ICCTOF + (J-1)*IFACT1 + K
              Y(I)=REAL(IINDX)
              GO TO 100
            ENDIF
  300     CONTINUE
  200   CONTINUE
!
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
        GO TO 9000
!
  100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2
 9013   FORMAT('NGRP1,NGRP2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),Y(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODCT2
      SUBROUTINE CODCT3(X1,X2,X3,N,ICCTOF,ICCTG1,ICCTG2,IWRITE,   &
                        Y,XIDTEM,XIDTE2,XIDTE3,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
!              CROSS TABULATION OF THREE GROUP-ID VARIABLES.  THIS
!              CAN BE USEFUL FOR COMMANDS OF THE FORM
!
!                  <COMMAND>  Y  X
!
!              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
!              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
!              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
!              A BOX PLOT OVER SEVERAL GROUPS.
!
!              THE CODING IS BASED ON THE FOLLOWING FORMULA:
!
!                  ICODE = OFFSET + (ISET1-1)*NGROUP2*NGROUP3 +
!                                   (ISET2-1)*NGROUP3 + ISET3
!
!              WHERE
!
!                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
!                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
!                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
!                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
!                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
!                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
!
!              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
!              THE ICCTG1 AND ICCTG2 PARAMETERS CAN BE USED TO CONTROL
!              THIS (I.E., WE USE:
!
!                   THE MAXIMUM OF NGROUP2 AND ICCTG1
!                   THE MAXIMUM OF NGROUP3 AND ICCTG2
!
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIRST GROUP VARIABLE
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SECOND GROUP VARIABLE
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE THIRD GROUP VARIABLE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X1, X2 AND X3.
!                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE OFFSET.
!                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 2.
!                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 3.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
!             X1, X2 AND X3.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/6
!     ORIGINAL VERSION--JUNE      2009.
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
!CCCC INCLUDE 'DPCOPA.INC'
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODC'
      ISUBN2='T3  '
!
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I)
   56     FORMAT('I,X1(I),X2(I),X3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  PERFORM THE CODING--                                 **
!               ***********************************************************
!
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
!
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
!
      DO 100 I=1,N
!
        DO 200 J=1,NGRP1
          DO 300 K=1,NGRP2
            DO 400 L=1,NGRP3
!
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
!
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.   &
               X3(I).EQ.XIDTE3(L))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2 + (K-1)*IFACT2 + L
              Y(I)=REAL(IINDX)
              GO TO 100
            ENDIF
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
!
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
        GO TO 9000
!
  100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3
 9013   FORMAT('NGRP1,NGRP2,NGRP3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),Y(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODCT3
      SUBROUTINE CODCT4(X1,X2,X3,X4,N,   &
                        ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,   &
                        Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
!              CROSS TABULATION OF FOUR GROUP-ID VARIABLES.  THIS
!              CAN BE USEFUL FOR COMMANDS OF THE FORM
!
!                  <COMMAND>  Y  X
!
!              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
!              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
!              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
!              A BOX PLOT OVER SEVERAL GROUPS.
!
!              THE CODING IS BASED ON THE FOLLOWING FORMULA:
!
!                  ICODE = OFFSET + (ISET1-1)*NGROUP2*NGROUP3*NGROUP4 +
!                                   (ISET2-1)*NGROUP3*NGROUP4 +
!                                   (ISET3-1)*NGROUP4 + ISET4
!
!              WHERE
!
!                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
!                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
!                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
!                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
!                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
!                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
!                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
!                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
!
!              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
!              THE ICCTG1, ICCTG2, AND ICCTG3 PARAMETERS CAN BE USED
!              TO CONTROL THIS (I.E., WE USE:
!
!                   THE MAXIMUM OF NGROUP2 AND ICCTG1
!                   THE MAXIMUM OF NGROUP3 AND ICCTG2
!                   THE MAXIMUM OF NGROUP4 AND ICCTG3
!
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIRST GROUP VARIABLE
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SECOND GROUP VARIABLE
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE THIRD GROUP VARIABLE
!                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FOURTH GROUP VARIABLE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X1, X2, X3 AND X4.
!                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE OFFSET.
!                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 2.
!                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 3.
!                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 4.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
!             X1, X2, X3 AND X4.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/6
!     ORIGINAL VERSION--JUNE      2009.
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
!CCCC INCLUDE 'DPCOPA.INC'
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODC'
      ISUBN2='T4  '
!
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  PERFORM THE CODING--                                 **
!               ***********************************************************
!
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NGRP1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NGRP1,XIDTEM)
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NGRP2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NGRP2,XIDTE2)
      CALL DISTIN(X3,N,IWRITE,XIDTE3,NGRP3,IBUGA3,IERROR)
      CALL SORT(XIDTE3,NGRP3,XIDTE3)
      CALL DISTIN(X4,N,IWRITE,XIDTE4,NGRP4,IBUGA3,IERROR)
      CALL SORT(XIDTE4,NGRP4,XIDTE4)
!
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
!
      DO 100 I=1,N
!
        DO 200 J=1,NGRP1
          DO 300 K=1,NGRP2
            DO 400 L=1,NGRP3
            DO 500 M=1,NGRP4
!
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
!
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.   &
               X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3 +   &
              (K-1)*IFACT2*IFACT3 +   &
              (L-1)*IFACT3 + M
              Y(I)=REAL(IINDX)
              GO TO 100
            ENDIF
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
!
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
        GO TO 9000
!
  100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),Y(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODCT4
      SUBROUTINE CODCT5(X1,X2,X3,X4,X5,N,   &
                        ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,   &
                        Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
!              CROSS TABULATION OF FIVE GROUP-ID VARIABLES.  THIS
!              CAN BE USEFUL FOR COMMANDS OF THE FORM
!
!                  <COMMAND>  Y  X
!
!              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
!              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
!              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
!              A BOX PLOT OVER SEVERAL GROUPS.
!
!              THE CODING IS BASED ON THE FOLLOWING FORMULA:
!
!                  ICODE = OFFSET +
!                          (ISET1-1)*NGROUP2*NGROUP3*NGROUP4*NGROUP5 +
!                          (ISET2-1)*NGROUP3*NGROUP4*NGROUP5 +
!                          (ISET3-1)*NGROUP4*NGROUP5
!                          (ISET4-1)*NGROUP5 + ISET5
!
!              WHERE
!
!                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
!                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
!                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
!                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
!                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
!                  ISET5     = I-TH DISTINCT VALUE OF GROUP 5
!                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
!                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
!                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
!                  NGROUP5   = NUMBER OF DISTINCT VALUES FOR GROUP 5
!
!              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
!              THE ICCTG1, ICCTG2, ICCTG3,AND ICCTG4 PARAMETERS CAN BE
!              USED TO CONTROL THIS (I.E., WE USE:
!
!                   THE MAXIMUM OF NGROUP2 AND ICCTG1
!                   THE MAXIMUM OF NGROUP3 AND ICCTG2
!                   THE MAXIMUM OF NGROUP4 AND ICCTG3
!                   THE MAXIMUM OF NGROUP5 AND ICCTG4
!
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIRST GROUP VARIABLE
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SECOND GROUP VARIABLE
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE THIRD GROUP VARIABLE
!                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FOURTH GROUP VARIABLE
!                     --X5     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIFTH GROUP VARIABLE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X1, X2, X3, X4 AND X5.
!                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE OFFSET.
!                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 2.
!                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 3.
!                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 4.
!                     --ICCTG4 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 5.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
!             X1, X2, X3, X4 AND X5.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/6
!     ORIGINAL VERSION--JUNE      2009.
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
!CCCC INCLUDE 'DPCOPA.INC'
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODC'
      ISUBN2='T4  '
!
      IERROR='NO'
!
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
        WRITE(ICOUT,53)ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4
   53   FORMAT('ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4 = ',5I5)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I),X5(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  PERFORM THE CODING--                                 **
!               ***********************************************************
!
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
!
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
      IFACT4=MAX(NGRP5,ICCTG4)
!
      DO 100 I=1,N
!
        DO 200 J=1,NGRP1
          DO 300 K=1,NGRP2
            DO 400 L=1,NGRP3
            DO 500 M=1,NGRP4
            DO 600 JJ=1,NGRP5
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT5')THEN
              WRITE(ICOUT,301)I,J,K,L,M
  301         FORMAT('I,J,K,L,M = ',5I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I),X4(I),X5(I)
  302         FORMAT('X1(I),X2(I),X3(I),X4(I),X5(I)=',5G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L),   &
                              XIDTE4(M),XIDTE5(JJ)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M),',   &
                     'XIDTE5(JJ)=',5G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.   &
               X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M) .AND.   &
               X5(I).EQ.XIDTE5(JJ))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3*IFACT4 +   &
              (K-1)*IFACT2*IFACT3*IFACT4 +   &
              (L-1)*IFACT3*IFACT4 +   &
              (M-1)*IFACT4 + JJ
              Y(I)=REAL(IINDX)
              GO TO 100
            ENDIF
  600     CONTINUE
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
!
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
        GO TO 9000
!
  100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4*NGRP5
  811   FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
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
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4,NGRP5
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4,NGRP5 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),X5(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),Y(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODCT5
      SUBROUTINE CODCT6(X1,X2,X3,X4,X5,X6,N,   &
                        ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,   &
                        IWRITE,   &
                        Y,XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A CODED VARIABLE FROM THE
!              CROSS TABULATION OF SIX GROUP-ID VARIABLES.  THIS
!              CAN BE USEFUL FOR COMMANDS OF THE FORM
!
!                  <COMMAND>  Y  X
!
!              WHERE X IS A GROUP-ID VARIABLE.  THIS ALLOWS US TO
!              USE THESE COMMANDS FOR THE CASE WHERE WE ACTUALLY
!              HAVE MULTIPLE GROUPS.  FOR EXAMPLE, WE CAN CREATE
!              A BOX PLOT OVER SEVERAL GROUPS.
!
!              THE CODING IS BASED ON THE FOLLOWING FORMULA:
!
!                  ICODE = OFFSET +
!                          (ISET1-1)*NGROUP2*NGROUP3*NGROUP4*NGROUP5*NGROUP6 +
!                          (ISET2-1)*NGROUP3*NGROUP4*NGROUP5*NGROUP6 +
!                          (ISET3-1)*NGROUP4*NGROUP5*NGROUP6 +
!                          (ISET4-1)*NGROUP5*NGROUP6 +
!                          (ISET5-1)*NGROUP6 + ISET6
!
!              WHERE
!
!                  OFFSET    = AN INITIAL OFFSET (DEFAULTS TO 0)
!                  ISET1     = I-TH DISTINCT VALUE OF GROUP 1
!                  ISET2     = I-TH DISTINCT VALUE OF GROUP 2
!                  ISET3     = I-TH DISTINCT VALUE OF GROUP 3
!                  ISET4     = I-TH DISTINCT VALUE OF GROUP 4
!                  ISET5     = I-TH DISTINCT VALUE OF GROUP 5
!                  ISET6     = I-TH DISTINCT VALUE OF GROUP 6
!                  NGROUP2   = NUMBER OF DISTINCT VALUES FOR GROUP 2
!                  NGROUP3   = NUMBER OF DISTINCT VALUES FOR GROUP 3
!                  NGROUP4   = NUMBER OF DISTINCT VALUES FOR GROUP 4
!                  NGROUP5   = NUMBER OF DISTINCT VALUES FOR GROUP 5
!                  NGROUP6   = NUMBER OF DISTINCT VALUES FOR GROUP 6
!
!              FOR PLOTS, WE MAY WANT TO SPACE GROUPS FURTHER APART.
!              THE ICCTG1, ICCTG2, ICCTG3, ICCTG4, AND ICCTG5 PARAMETERS
!              CAN BE USED TO CONTROL THIS (I.E., WE USE:
!
!                   THE MAXIMUM OF NGROUP2 AND ICCTG1
!                   THE MAXIMUM OF NGROUP3 AND ICCTG2
!                   THE MAXIMUM OF NGROUP4 AND ICCTG3
!                   THE MAXIMUM OF NGROUP5 AND ICCTG4
!                   THE MAXIMUM OF NGROUP6 AND ICCTG5
!
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIRST GROUP VARIABLE
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SECOND GROUP VARIABLE
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE THIRD GROUP VARIABLE
!                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FOURTH GROUP VARIABLE
!                     --X5     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE FIFTH GROUP VARIABLE
!                     --X6     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE VALUES OF THE SIXTH GROUP VARIABLE
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X1, X2, X3, X4, X5 AND
!                                X6.
!                     --ICCTOF = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE OFFSET.
!                     --ICCTG1 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 2.
!                     --ICCTG2 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 3.
!                     --ICCTG3 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 4.
!                     --ICCTG4 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 5.
!                     --ICCTG5 = THE INTEGER PARAMETER THAT SPECIFIES
!                                THE SPACING FOR GROUP 6.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTORS
!             X1, X2, X3, X4, X5 AND X6.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/6
!     ORIGINAL VERSION--JUNE      2009.
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
!CCCC INCLUDE 'DPCOPA.INC'
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODC'
      ISUBN2='T6  '
      IERROR='NO'
!
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
        WRITE(ICOUT,53)ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5
   53   FORMAT('ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5 = ',6I5)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I) = ',I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  PERFORM THE CODING--                                 **
!               ***********************************************************
!
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
!
      IFACT1=MAX(NGRP2,ICCTG1)
      IFACT2=MAX(NGRP3,ICCTG2)
      IFACT3=MAX(NGRP4,ICCTG3)
      IFACT4=MAX(NGRP5,ICCTG4)
      IFACT5=MAX(NGRP6,ICCTG5)
!
      DO 100 I=1,N
!
        DO 200 J=1,NGRP1
          DO 300 K=1,NGRP2
            DO 400 L=1,NGRP3
            DO 500 M=1,NGRP4
            DO 600 JJ=1,NGRP5
            DO 700 KK=1,NGRP6
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT6')THEN
              WRITE(ICOUT,301)I,J,K,L,M
  301         FORMAT('I,J,K,L,M = ',5I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,302)X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)
  302         FORMAT('X1(I),X2(I),X3(I),X4(I),X5(I),X6(I)=',6G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,303)XIDTEM(J),XIDTE2(K),XIDTE3(L),   &
                              XIDTE4(M),XIDTE5(JJ),XIDTE6(KK)
  303         FORMAT('XIDTEM(J),XIDTE2(K),XIDTE3(L),XIDTE4(M),',   &
                     'XIDTE5(JJ),XIDTE6(KK)=',6G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            IF(X1(I).EQ.XIDTEM(J) .AND. X2(I).EQ.XIDTE2(K) .AND.   &
               X3(I).EQ.XIDTE3(L) .AND. X4(I).EQ.XIDTE4(M) .AND.   &
               X5(I).EQ.XIDTE5(JJ) .AND. X6(I).EQ.XIDTE6(KK))THEN
              IINDX=ICCTOF + (J-1)*IFACT1*IFACT2*IFACT3*IFACT4*IFACT5 +   &
              (K-1)*IFACT2*IFACT3*IFACT4*IFACT5 +   &
              (L-1)*IFACT3*IFACT4*IFACT5 +   &
              (M-1)*IFACT4*IFACT5 +   &
              (JJ-1)*IFACT5 + KK
              Y(I)=REAL(IINDX)
              GO TO 100
            ENDIF
  700     CONTINUE
  600     CONTINUE
  500     CONTINUE
  400     CONTINUE
  300     CONTINUE
  200   CONTINUE
!
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
        GO TO 9000
!
  100 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)NGRP1*NGRP2*NGRP3*NGRP4*NGRP5*NGRP6
  811 FORMAT('NUMBER OF DISTINCT CODE VALUES = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DCT6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODCT6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NGRP1,NGRP2,NGRP3,NGRP4,NGRP5,NGRP6
 9013   FORMAT('NGRP1,NGRP2,NGRP3,NGRP4,NGRP5,NGRP6 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I),Y(I)
 9016     FORMAT('I,X1(I),X2(I),X3(I),X4(I),X5(I),X6(I),Y(I) = ',   &
                 I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODCT6
      SUBROUTINE CODE(X,N,IWRITE,Y,DIST,MAXOBV,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
!              OF THE INPUT VECTOR X
!              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
!              THE CODING IS AS FOLLOWS--
!              THE MINIMUM IS CODED AS 1.0.
!              THE NEXT LARGER VALUE AS 2.0,
!              THE NEXT LARGER VALUE AS 3.0,
!              ETC.
!     NOTE--THIS ROUTINE IN JJF8 HAS BEEN MODIFIED FOR DATAPLOT
!           FROM THE SAME-NAME SUBROUTINE IN JJF6 IN 4 IMPORTANT WAYS--
!           1)  THE UPPER LIMIT (IUPPER) HAS BEEN
!               REDUCED FROM 7500 TO 1000
!           2)  THE VECTOR DIST HAS HAD ITS DIMENSION
!               CHANGED FROM 7500 TO 1000.
!           3)  THE VECTOR DIST HAS BEEN TAKEN OUT OF COMMON.
!           4)  THE VECTOR WS HAS BEEN DELETED.
!           5)  THE OUTPUT WRITING HAS BEEN SUPPRESSED.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y
!             WHICH WILL CONTAIN THE CODED VALUES
!             CORRESPONDING TO THE OBSERVATIONS IN
!             THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 15000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--ALL OCCURRANCES OF THE MINIMUM ARE CODED AS 1.0;
!              ALL OCCURANCES OF THE NEXT LARGER VALUE
!              ARE CODED AS 2.0;
!              ALL OCCURANCES OF THE NEXT LARGER VALUE
!              ARE CODED AS 3.0, ETC.
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --JUNE      1977.
!     UPDATED         --JULY      1977.
!     UPDATED         --JULY      1979.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
      DIMENSION DIST(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='    '
      IERROR='NO'
      IUPPER=MAXOBV
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      IS LESS THAN 1 OR GREATER THAN ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      DO 137 I=1,N
        Y(I)=1.0
  137 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               *************************************************************
!               **  STEP 2--                                               **
!               **  PERFORM THE CODING--                                   **
!               **  PULL OUT THE DISTINCT VALUES,                          **
!               **  THEN SORT (AND ESSENTIALLY RANK) THE DISTINCT VALUES,  **
!               **  THEN APPLY THE RANKS TO ALL THE VALUES.                **
!               *************************************************************
!
      NUMDIS=1
      DIST(NUMDIS)=X(1)
      DO 200 I=2,N
        DO 300 J=1,NUMDIS
          IF(X(I).EQ.DIST(J))GO TO 200
  300   CONTINUE
        NUMDIS=NUMDIS+1
        DIST(NUMDIS)=X(I)
  200 CONTINUE
!
      CALL SORT(DIST,NUMDIS,DIST)
!
      DO 600 I=1,N
        DO 700 J=1,NUMDIS
          IF(X(I).EQ.DIST(J))THEN
            Y(I)=J
            GO TO 600
          ENDIF
  700   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,705)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,710)I,X(I)
  705   FORMAT('***** INTERNAL ERROR IN CODE SUBROUTINE--')
        CALL DPWRST('XXX','BUG ')
  710   FORMAT('      NO CODE FOUND FOR ELEMENT NUMBER ',I8,' = ',   &
               G15.7)
        GO TO 9000
  600 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
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
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I),DIST(I)
 9016     FORMAT('I,X(I),Y(I),DIST(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODE
      SUBROUTINE CODECH(YTEMP,IWRITE,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
!              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
!              A NUMERIC VARIABLE.  THAT IS, EACH DISTINCT
!              CHARACTER VARIABLE WILL BE ASSIGNED AN INTEGER
!              CODE (DETERMINED BY ORDER THAT THE FIRST OCCURENCE
!              IS FOUND).
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE CHARACTER VARIABLE.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y
!             WHICH WILL CONTAIN THE CODED VALUES
!             CORRESPONDING TO THE OBSERVATIONS IN
!             THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS MAXOBV.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/1
!     ORIGINAL VERSION--JANUARY   2004.
!     UPDATED         --FEBRUARY  2006. FIX BUG WHERE IT WAS ONLY
!                                       WORKING IF THERE WAS ONE
!                                       CHARACTER VARIABLE IN THE
!                                       DPZCHF.DAT.
!     UPDATED         --APRIL     2017. MODIFY THE FEEDBACK TO SHOW
!                                       THE ACTUAL MAPPING
!     UPDATED         --JUNE      2019. DIMENSION SCRATCH REAL ARRAYS IN
!                                       CALLING ROUTINE
!     UPDATED         --SEPTEMBER 2021. REPORT AN ERROR IF LHS
!                                       PREVIOUSLY DEFINED, BUT NOT AS
!                                       VARIABLE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION YTEMP(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
!
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='CH  '
      IERROR='NO'
!
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
!
!               **************************************************
!               **  STEP 1--                                     *
!               **  EXAMINE THE LEFT-HAND SIDE--                 *
!               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
!               **  ALREADY IN THE NAME LIST?                    *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
!               **  TABLE OF THE NAME ON THE LEFT.               *
!               **************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V   ')THEN
            ILISTL=I2
            GO TO 2100
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2201)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2010)IHLEFT,IHLEF2
 2010       FORMAT('      AN ATTEMPT WAS MADE TO USE ',2A4,' AS A ',   &
                   'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            IF(IUSE(I).EQ.'P')THEN
              WRITE(ICOUT,2011)
 2011         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',   &
                     'PARAMETER.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'M')THEN
              WRITE(ICOUT,2012)
 2012         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A MATRIX.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'F')THEN
              WRITE(ICOUT,2013)
 2013         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A STRING.')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,2014)IUSE(I)
 2014         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',A4,'.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
 2000 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN CHARACTER CODE (CODECH)--')
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
        GO TO 9000
      ENDIF
!
 2100 CONTINUE
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 2--                              **
!               **  OPEN THE DPZCHF.DAT FILE.             **
!               ********************************************
!
      IHRIGH=IHARG(5)
      IHRIG2=IHARG2(5)
!
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
!
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,   &
                  ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
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
        GO TO 8000
      ENDIF
!
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
!
!CCCC FEBRUARY 2006:  BUG FIX FOR THE FOLLOWING LOOP.
!
      IVAR=-1
      DO 130 I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
!CCCC     GO TO 199
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GO TO 199
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',   &
             'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  199 CONTINUE
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  PERFORM THE CODING--                       **
!               **  STORE UNIQUE VALUES IN IXTEMP, COMPARE     **
!               **  TO LIST IN IXTEMP.                         **
!               *************************************************
!
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
!
      DO 210 I=2,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
        DO 220 J=1,N
          IF(IATEMP(IFRST:ILAST).EQ.IXTEMP(J)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GO TO 210
          ENDIF
  220   CONTINUE
        N=N+1
        IXTEMP(N)=' '
        IXTEMP(N)=IATEMP(IFRST:ILAST)
        YTEMP(IROW)=REAL(N)
  210 CONTINUE
      GO TO 499
!
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',   &
             'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
  499 CONTINUE
!
!     2017/04: MODIFY THE FEEDBACK TO SHOW THE FULL MAPPING
!
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
!
        MAXCHR=1
        DO 810 I=1,MIN(N,100)
          DO 813 J=24,1,-1
            IF(IXTEMP(I)(J:J).NE.' ')THEN
              IF(J.GT.MAXCHR)MAXCHR=J
              GO TO 815
            ENDIF
  813     CONTINUE
  815     CONTINUE
  810   CONTINUE
        WRITE(IFRMT(3:4),'(I2)')MAXCHR
!
        DO 820 I=1,N
          WRITE(ICOUT,IFRMT)IXTEMP(I),I
          CALL DPWRST('XXX','BUG ')
  820   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  STEP 5--                                       **
!               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
!               **  HOUSEKEEPING ARRAY                             **
!               *****************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,   &
      ISUBN1,ISUBN2,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 88--                        **
!               **  CLOSE THE DPZCHF.DAT FILE.       **
!               ***************************************
!
 8000 CONTINUE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DECH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODECH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,N,IROW
 9012   FORMAT('IBUGA3,IERROR,N,IROW = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9035 I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODECH
      SUBROUTINE CODEC2(YTEMP,YTEMP2,IPERM,IWRITE,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
!              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
!              A NUMERIC VARIABLE.  THAT IS, EACH DISTINCT
!              CHARACTER VARIABLE WILL BE ASSIGNED AN INTEGER
!              CODE.  THIS ROUTINE IS SIMILAR TO CODECH.  THE
!              DISTINCTION IS THAT CODECH CODES BY THE ORDER THE
!              VALUES ARE ENCOUNTERED IN THE FILE WHILE THIS
!              ROUTINE CODES BY (LEXICAL) ALPHABETIC ORDER.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS MAXOBV.
!     OTHER DATAPAC   SUBROUTINES NEEDED--HPSORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/1
!     ORIGINAL VERSION--JANUARY   2004.
!     UPDATED         --DECEMBER  2009. DON'T USE IXSAVE SO COMMENT
!                                       OUT DECLARATION
!     UPDATED         --DECEMBER  2009. MODIFY DECLARATION OF IXWORK
!                                       FOR INTEL COMPILER
!     UPDATED         --APRIL     2017. MODIFY THE FEEDBACK TO SHOW
!                                       THE ACTUAL MAPPING
!     UPDATED         --JUNE      2019. DIMENSION REAL SCRATCH ARRAYS
!                                       IN CALLING ROUTINE
!     UPDATED         --SEPTEMBER 2021. REPORT AN ERROR IF LHS
!                                       PREVIOUSLY DEFINED, BUT NOT AS
!                                       VARIABLE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION YTEMP(*)
      DIMENSION YTEMP2(*)
      DIMENSION IPERM(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
!
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV/2)
      CHARACTER*24 IXWORK(MAXOBV/2)
!CCCC CHARACTER*24 IXSAVE(MAXOBV/2)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
      EQUIVALENCE (CGARBG(MAXOBV/2 + 1),IXWORK(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='C2  '
      IERROR='NO'
!
      INDX=0
!
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
!
!               **************************************************
!               **  STEP 1--                                     *
!               **  EXAMINE THE LEFT-HAND SIDE--                 *
!               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
!               **  ALREADY IN THE NAME LIST?                    *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
!               **  TABLE OF THE NAME ON THE LEFT.               *
!               **************************************************
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V   ')THEN
            ILISTL=I2
            GO TO 2100
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2201)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2010)IHLEFT,IHLEF2
 2010       FORMAT('      AN ATTEMPT WAS MADE TO USE ',2A4,' AS A ',   &
                   'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            IF(IUSE(I).EQ.'P')THEN
              WRITE(ICOUT,2011)
 2011         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',   &
                     'PARAMETER.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'M')THEN
              WRITE(ICOUT,2012)
 2012         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A MATRIX.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'F')THEN
              WRITE(ICOUT,2013)
 2013         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A STRING.')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,2014)IUSE(I)
 2014         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',A4,'.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
 2000 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN ALPHABETIC CHARACTER CODE (CODEC2)--')
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
        GO TO 9000
      ENDIF
!
 2100 CONTINUE
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 2--                              **
!               **  OPEN THE DPZCHF.DAT FILE.             **
!               ********************************************
!
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
!
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
!
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,   &
                  ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
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
        GO TO 8000
      ENDIF
!
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
!
      IVAR=-1
      DO 130 I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
!CCCC     GO TO 199
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GO TO 199
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',   &
             'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  199 CONTINUE
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  PERFORM THE CODING--                       **
!               **  1) STORE UNIQUE VALUES IN IXTEMP           **
!               **  2) SORT VALUES IN IXTEMP                   **
!               **  3) CODE BASED ON SORTED IXTEMP VALUES      **
!               *************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      DO 210 I=2,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=IROW+1
        DO 220 J=1,N
          IF(IATEMP(IFRST:ILAST).EQ.IXTEMP(J)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GO TO 210
          ENDIF
  220   CONTINUE
        N=N+1
        IF(N.GT.MAXOBV/2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,221)
  221     FORMAT('      NUMBER OF UNIQUE CHARACTER VALUE EXCEEDS ',   &
                 I8,' .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,223)
  223     FORMAT('      CODING NOT PERFORMED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        IXTEMP(N)=' '
        IXTEMP(N)=IATEMP(IFRST:ILAST)
        YTEMP(IROW)=REAL(N)
  210 CONTINUE
!
  499 CONTINUE
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IBEG=1
      IEND=24
      KFLAG=2
      IER=0
      CALL HPSORT(IXTEMP,N,IBEG,IEND,IPERM,KFLAG,IXWORK(1),IER)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')THEN
        WRITE(ICOUT,292)N,IROW,IER
  292   FORMAT('N,IROW,IER = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO 290 I=1,N
            WRITE(ICOUT,293)I,IXTEMP(I),IPERM(I)
  293       FORMAT('I,IXTEMP(I),IPERM(I) = ',I8,1X,A24,1X,I8)
            CALL DPWRST('XXX','BUG ')
  290     CONTINUE
        ENDIF
      ENDIF
      IF(IER.GT.0)GO TO 9000
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 310 I=1,IROW
        ITEMP=INT(YTEMP(I) + 0.5)
        DO 320 K=1,N
          IF(ITEMP.EQ.IPERM(K))THEN
            INDX=K
            GO TO 329
          ENDIF
  320   CONTINUE
  329   CONTINUE
        YTEMP2(I)=REAL(INDX)
  310 CONTINUE
      DO 330 I=1,IROW
        YTEMP(I)=YTEMP2(I)
  330 CONTINUE
!
      GO TO 599
!
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',   &
             'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
  599 CONTINUE
!
!     2017/04: MODIFY THE FEEDBACK TO SHOW THE FULL MAPPING
!
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
!
        MAXCHR=1
        DO 810 I=1,MIN(N,100)
          DO 813 J=24,1,-1
            IF(IXTEMP(I)(J:J).NE.' ')THEN
              IF(J.GT.MAXCHR)MAXCHR=J
              GO TO 815
            ENDIF
  813     CONTINUE
  815     CONTINUE
  810   CONTINUE
        WRITE(IFRMT(3:4),'(I2)')MAXCHR
!
        DO 820 I=1,N
          WRITE(ICOUT,IFRMT)IXTEMP(I),I
          CALL DPWRST('XXX','BUG ')
  820   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  STEP 5--                                       **
!               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
!               **  HOUSEKEEPING ARRAY                             **
!               *****************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,   &
      ISUBN1,ISUBN2,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 6--                         **
!               **  CLOSE THE DPZCHF.DAT FILE.       **
!               ***************************************
!
 8000 CONTINUE
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
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
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9035 I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEC2
      SUBROUTINE CODEC3(YTEMP,IWRITE,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
!              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
!              A NUMERIC VARIABLE.  THIS IMPLEMENTS THE
!              "REFERENCE CHARACTER CODE" COMMAND.
!
!              FOR THE "CODECH" ROUTINE (WHICH IMPLEMENTS THE
!              "CHARACTER CODE" COMMAND"), EACH DISTINCT CHARACTER
!              VARIABLE WILL BE ASSIGNED AN INTEGER CODE DETERMINED
!              BY ORDER THAT THE FIRST OCCURENCE IS FOUND).
!
!              THIS VARIANT IS SIMILAR.  HOWEVER, INSTEAD OF BASING THE
!              CODE BASED ON THE ORDER OF FIRST APPEARANCE, THE CODE
!              WILL BE BASED ON A PREVIOUSLY DEFINED GROUP LABEL.  THIS
!              IS USEFUL WHEN, FOR EXAMPLE, READING SEVERAL SETS OF DATA
!              THAT USE THE SAME CATEGORICAL VARIABLE AND WE WANT THE
!              CODING TO BE CONSISTENT ACROSS THE DATA FILES.
!
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS IN
!                                THE CHARACTER VARIABLE.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
!                   IS MAXOBV.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2018/06
!     ORIGINAL VERSION--JUNE      2018.
!     UPDATED         --JUNE      2019. MOVE DIMENSION OF SCRATCH
!                                       REAL ARRAY TO CALLING ROUTINE
!     UPDATED         --SEPTEMBER 2021. REPORT AN ERROR IF LHS
!                                       PREVIOUSLY DEFINED, BUT NOT AS
!                                       VARIABLE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION YTEMP(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 IHRIG3
      CHARACTER*4 IHRIG4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
!
      CHARACTER*500 IATEMP
      CHARACTER*20 IFRMT
      CHARACTER*24 IXTEMP(MAXOBV)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='CH  '
      IERROR='NO'
!
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
!
!               **************************************************
!               **  STEP 1--                                     *
!               **  EXAMINE THE LEFT-HAND SIDE--                 *
!               **  IS THE NAME     NAME TO LEFT OF = SIGN       *
!               **  ALREADY IN THE NAME LIST?                    *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE   *
!               **  TABLE OF THE NAME ON THE LEFT.               *
!               **************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 100 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V   ')THEN
            ILISTL=I2
            GO TO 110
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2010)IHLEFT,IHLEF2
 2010       FORMAT('      AN ATTEMPT WAS MADE TO USE ',2A4,' AS A ',   &
                   'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            IF(IUSE(I).EQ.'P')THEN
              WRITE(ICOUT,2011)
 2011         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',   &
                     'PARAMETER.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'M')THEN
              WRITE(ICOUT,2012)
 2012         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A MATRIX.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'F')THEN
              WRITE(ICOUT,2013)
 2013         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A STRING.')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,2014)IUSE(I)
 2014         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',A4,'.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
  100 CONTINUE
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN REFERENCE CHARACTER CODE (CODEC3)--')
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
        GO TO 9000
      ENDIF
!
  110 CONTINUE
!
!               **************************************************
!               **  STEP 2--                                     *
!               **  EXAMINE THE RIGHT-HAND SIDE--                *
!               **  IS THE SECOND NAME ON THE RIGHT HAND SIDE    *
!               **  A PREVIOUSLY DEFINED GROUP LABEL?            *
!               **************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRIG3=IHARG(7)
      IHRIG4=IHARG2(7)
      DO 200 I=1,MAXGRP
        IF(IHRIG3.EQ.IGRPVN(I)(1:4).AND.IHRIG4.EQ.IGRPVN(I)(5:8))THEN
          IGRP=I
          GO TO 210
        ENDIF
  200 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,212)IHRIG3,IHRIG4
  212 FORMAT('      THE SPECIFIED GROUP (',2A4,') WAS NOT FOUND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  210 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
        WRITE(ICOUT,221)IGRP
  221   FORMAT('AT 210: IGRP = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 3--                              **
!               **  OPEN THE DPZCHF.DAT FILE.             **
!               ********************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
!
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
!
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,   &
                  ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
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
        GO TO 7000
      ENDIF
!
      READ(IOUNIT,'(I8)',END=371,ERR=371)NUMVAR
!
      IVAR=-1
      DO 330 I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=381,ERR=381)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
        ENDIF
  330 CONTINUE
      IF(IVAR.GT.0)GO TO 399
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,331)IHRIGH,IHRIG2
  331 FORMAT('      VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',   &
             'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 7000
!
  371 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,373)
  373 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 7000
!
  381 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,383)
  383 FORMAT('      ERROR READING THE VARIABLE NAMES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 7000
!
  399 CONTINUE
!
!               *************************************************
!               **  STEP 4--                                   **
!               **  PERFORM THE CODING--                       **
!               **  STORE UNIQUE VALUES IN IXTEMP, COMPARE     **
!               **  TO LIST IN IXTEMP.                         **
!               *************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFRMT='(A   )'
      WRITE(IFRMT(3:5),'(I3)')25*IVAR
      IFRST=(IVAR-1)*25 + 1
      ILAST=IVAR*25 - 1
!
      DO 410 I=1,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
        DO 420 J=1,MAXGLA
          IF(IATEMP(IFRST:ILAST).EQ.IGRPLA(J,IGRP)(1:24))THEN
            YTEMP(IROW)=REAL(J)
            GO TO 419
          ENDIF
  420   CONTINUE
        YTEMP(IROW)=-1.0
!
  419   CONTINUE
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
          WRITE(ICOUT,421)I,IFRST,ILAST
  421     FORMAT('AT 419: I,IFRST,ILAST = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,423)IATEMP(IFRST:ILAST)
  423     FORMAT('IATEMP(IFRST:ILAST) = ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
  410 CONTINUE
      GO TO 499
!
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',   &
             'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 7000
!
!
!               ******************************
!               **  STEP 5--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
  499 CONTINUE
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MINIM(YTEMP,IROW,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(YTEMP,IROW,IWRITE,YMAX,IBUGA3,IERROR)
!
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
!
!               *****************************************************
!               **  STEP 6--                                       **
!               **  ENTER THE CODED      VALUES INTO THE DATAPLOT  **
!               **  HOUSEKEEPING ARRAY                             **
!               *****************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEL='V'
      XINT=0.0
      IXINT=0
      CALL DPINVP(IHLEFT,IHLEF2,ICASEL,YTEMP,IROW,XINT,IXINT,   &
      ISUBN1,ISUBN2,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 7--                         **
!               **  CLOSE THE DPZCHF.DAT FILE.       **
!               ***************************************
!
 7000 CONTINUE
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF CODEC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,IROW
 9013   FORMAT('IERROR,N,IROW = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9035 I=1,IROW
          WRITE(ICOUT,9036)I,YTEMP(I)
 9036     FORMAT('I,YTEMP(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEC3
      SUBROUTINE CODEDX(X,N,IWRITE,Y,XDIST,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--FOR CLASSIC 2-LEVEL FACTORIAL DESIGNS, IT IS CONVENIENT
!              FOR EACH OF THE FACTOR VARIABLES TO LABEL THE LOW VALUE AS
!              "-1" AND THE HIGH VALUE AS "+1".  IN ADDITION, THERE MAY BE
!              CENTER POINTS WHICH ARE CODED AS "0".  IF THE FACTOR
!              VARIABLE IS CODED IN THE ORIGINAL UNITS OF THE DATA, THIS
!              ROUTINE CAN BE USED TO CONVERT IT TO THE "-1" AND "+1"
!              CODING.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
!                   IS 15000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     VERSION NUMBER--2018/01
!     ORIGINAL VERSION--JANUARY   2018.
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
      DIMENSION Y(*)
      DIMENSION XDIST(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='DX  '
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEDX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        Y(1)=1.0
        GO TO 8000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      DO 137 I=1,N
        Y(I)=1.0
  137 CONTINUE
      GO TO 8000
  139 CONTINUE
!
!               *************************************************************
!               **  STEP 2--                                               **
!               **  PERFORM THE CODING--                                   **
!               *************************************************************
!
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL SORT(XDIST,NDIST,XDIST)
!
      IF(NDIST.EQ.1)THEN
        DO 210 I=1,N
          Y(I)=1.0
  210   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(2)
        DO 220 I=1,N
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
        DO 230 I=1,N
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
  241   FORMAT('      THE RESPONSE VARIABLE CONTAINS MORE THAN THREE ',   &
               'DISTINCT VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,243)NDIST
  243   FORMAT('      THE NUMBER OF DISTINCT VALUES DETECTED WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
 8000 CONTINUE
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NDIST.LE.2)THEN
          WRITE(ICOUT,8112)
 8112     FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',   &
               '-1 AND +1 VALUES.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,8114)
 8114     FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',   &
               '-1, 0, AND +1 VALUES.')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEDX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEDX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR
 9013   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,G15.7,F7.0)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEDX
      SUBROUTINE CODED2(X,N,IWRITE,Y,NOUT,XDIST,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--FOR CLASSIC 2-LEVEL FACTORIAL DESIGNS, IT IS CONVENIENT
!              FOR EACH OF THE FACTOR VARIABLES TO LABEL THE LOW VALUE AS
!              "-1" AND THE HIGH VALUE AS "+1".  IN ADDITION, THERE MAY BE
!              CENTER POINTS WHICH ARE CODED AS "0".  IF THE FACTOR
!              VARIABLE IS CODED IN THE ORIGINAL UNITS OF THE DATA, THIS
!              ROUTINE CAN BE USED TO CONVERT IT TO THE "-1" AND "+1"
!              CODING.
!
!              THIS ROUTINE IS SIMILAR TO "CODEDX".  THE DISTINCTION IS
!              THAT THIS ROUTINE ONLY SAVES THE MINIMUM VALUE (AS -1)
!              AND THE MAXIMUM VALUE (AS +1).  ALL OTHER VALUES ARE
!              DISCARDED.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!                     --NOUT   = THE INTEGER NUMBER OF OBSERVATIONS
!                                THAT ARE SAVED IN Y (NOT NECCESSARILY
!                                EQUAL TO N).
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N FOR THIS SUBROUTINE
!                   IS 15000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     VERSION NUMBER--2018/10
!     ORIGINAL VERSION--OCTOBER   2018.
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
      DIMENSION Y(*)
      DIMENSION XDIST(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='D2  '
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODED2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        Y(1)=1.0
        NOUT=1
        GO TO 8000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      DO 137 I=1,N
        Y(I)=1.0
  137 CONTINUE
      NOUT=N
      GO TO 8000
  139 CONTINUE
!
!               *************************************************************
!               **  STEP 2--                                               **
!               **  PERFORM THE CODING--                                   **
!               *************************************************************
!
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL SORT(XDIST,NDIST,XDIST)
!
      IF(NDIST.EQ.1)THEN
        DO 210 I=1,N
          Y(I)=1.0
  210   CONTINUE
        NOUT=N
      ELSEIF(NDIST.EQ.2)THEN
        AVAL1=XDIST(1)
        AVAL2=XDIST(2)
        DO 220 I=1,N
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
        DO 230 I=1,N
          IF(X(I).EQ.AVAL1)THEN
            Y(I)=-1.0
            NOUT=NOUT+1
          ELSEIF(X(I).EQ.AVAL2)THEN
            Y(I)=1.0
            NOUT=NOUT+1
          ENDIF
  230   CONTINUE
      ENDIF
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
 8000 CONTINUE
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8112)
 8112   FORMAT('THE RESPONSE VARIABLE HAS BEEN CODED AS ',   &
               '-1 AND +1 VALUES.')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DED2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODED2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NOUT,NDIST
 9013   FORMAT('IERROR,NOUT,NDIST = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,F7.0)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODED2
      SUBROUTINE CODEH(X,N,NUMINT,IWRITE,Y,XS,MAXOBV,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
!              OF THE INPUT VECTOR X
!              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
!              THE CODING IS AS FOLLOWS--
!                  THE FIRST NUMINT'TH OF THE DATA IS CODED AS 1.0
!                  THE NEXT  NUMINT'TH OF THE DATA IS CODED AS 2.0
!                  ETC.
!                  THE LAST  NUMINT'TH OF THE DATA IS CODED AS NUMINT
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y
!             WHICH WILL CONTAIN THE CODED VALUES
!             CORRESPONDING TO THE OBSERVATIONS IN
!             THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 15000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
!CCCC INCLUDE 'DPCOPA.INC'
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XS(MAXOBV)
!CCCC FOLLOWING LINES ADDED JUNE, 1990
!CCCC INCLUDE 'DPCOZ2.INC'
!CCCC EQUIVALENCE (G2RBAG(IGAR45),XS(1))
!CCCC END CHANGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='N   '
!
      IERROR='NO'
      IUPPER=MAXOBV
!
      X50=0.0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(1.LE.N.AND.N.LE.IUPPER)GO TO 119
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)IUPPER
  111 FORMAT('***** ERROR IN CODEH--',   &
      'THE SECOND INPUT ARGUMENT (N) IS SMALLER THAN 1',   &
      'OR LARGER THAN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,118)N
  118 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,121)
!C121 FORMAT('***** NON-FATAL DIAGNOSTIC IN CODEH--',
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC1'THE SECOND INPUT ARGUMENT (N) HAS THE VALUE 1')
      Y(1)=1.0
      GO TO 9000
  129 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,136)HOLD
!C136 FORMAT('***** NON-FATAL DIAGNOSTIC IN CODEH--',
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC1'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      DO 137 I=1,N
      Y(I)=1.0
  137 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  PERFORM THE CODING--                             **
!               *******************************************************
!
      CALL SORT(X,N,XS)
!
      AN=N
!
      DO 1410 I=1,N
      Y(I)=4.0
 1410 CONTINUE
!
      N2=(N+1)/2
      IARG1=(N2+1)/2
      IARG2=(N2+1)-IARG1
      IARG1R=N-IARG1+1
      IARG2R=N-IARG2+1
      X75=(XS(IARG1R)+XS(IARG2R))/2.0
      XCUT=X75
      DO 1420 I=1,N
      IF(X(I).LE.XCUT)Y(I)=3.0
 1420 CONTINUE
!
      N50=N/2
      N50P1=N50+1
      IEVODD=N-2*(N/2)
      IF(IEVODD.EQ.0)X50=(XS(N50)+XS(N50P1))/2.0
      IF(IEVODD.EQ.1)X50=XS(N50P1)
      XCUT=X50
      DO 1430 I=1,N
      IF(X(I).LE.XCUT)Y(I)=2.0
 1430 CONTINUE
!
      N2=(N+1)/2
      IARG1=(N2+1)/2
      IARG2=(N2+1)-IARG1
      X25=(XS(IARG1)+XS(IARG2))/2.0
      XCUT=X25
      DO 1440 I=1,N
      IF(X(I).LE.XCUT)Y(I)=1.0
 1440 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 8190
      IF(IWRITE.EQ.'OFF')GO TO 8190
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
 9011 FORMAT('***** AT THE END       OF CODEH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N,NUMINT
 9013 FORMAT('N,NUMINT = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,N
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE CODEH
      SUBROUTINE CODEN(X,N,NUMINT,IWRITE,Y,XS,MAXOBV,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CODES THE ELEMENTS
!              OF THE INPUT VECTOR X
!              AND PUTS THE CODED VALUES INTO THE OUTPUT VECTOR Y.
!              THE CODING IS AS FOLLOWS--
!                  THE FIRST NUMINT'TH OF THE DATA IS CODED AS 1.0
!                  THE NEXT  NUMINT'TH OF THE DATA IS CODED AS 2.0
!                  ETC.
!                  THE LAST  NUMINT'TH OF THE DATA IS CODED AS NUMINT
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE CODED VALUES
!                                WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y
!             WHICH WILL CONTAIN THE CODED VALUES
!             CORRESPONDING TO THE OBSERVATIONS IN
!             THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 15000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
      DIMENSION XS(MAXOBV)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='N   '
      IERROR='NO'
!
      IUPPER=MAXOBV
      XMED=0.0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      IS LESS THAN 1 OR GREATER THAN ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I10)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      DO 137 I=1,N
        Y(I)=1.0
  137 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               *************************************************************
!               **  STEP 2--                                               **
!               **  PERFORM THE CODING--                                   **
!               *************************************************************
!
      CALL SORT(X,N,XS)
!
      AN=N
      IF(NUMINT.EQ.1)THEN
        DO 1110 I=1,N
          Y(I)=NUMINT
 1110   CONTINUE
      ELSEIF(NUMINT.GE.3)THEN
        DO 1310 I=1,N
          Y(I)=NUMINT
 1310   CONTINUE
        ANUMIN=NUMINT
        JMAX=NUMINT-1
        DO 1320 J=1,JMAX
          JREV=JMAX-J+1
          AJREV=JREV
          P=AJREV/ANUMIN
          AK=P*AN
          K1=INT(AK)
          K2=INT(AK+1.0)
          IF(K1.LE.1)K1=1
          IF(K1.GE.N)K1=N
          IF(K2.LE.1)K2=1
          IF(K2.GE.N)K2=N
          XCUT=(XS(K1)+XS(K2))/2.0
          DO 1350 I=1,N
            IF(X(I).LE.XCUT)Y(I)=JREV
 1350     CONTINUE
 1320   CONTINUE
      ELSE
        DO 1210 I=1,N
          Y(I)=NUMINT
 1210   CONTINUE
        N50=N/2
        N50P1=N50+1
        IEVODD=N-2*(N/2)
        IF(IEVODD.EQ.0)XMED=(XS(N50)+XS(N50P1))/2.0
        IF(IEVODD.EQ.1)XMED=XS(N50P1)
        XCUT=XMED
        DO 1250 I=1,N
          IF(X(I).LE.XCUT)Y(I)=1.0
 1250   CONTINUE
      ENDIF
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,NUMINT
 9013   FORMAT('IERROR,N,NUMINT = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEN
      SUBROUTINE CODEST(ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE READS THE CHARCTER DATA STORED IN
!              FILE "DPZCHF.DAT" AND CODES A SELECTED FIELD INTO
!              A STRING.  THE LHS DEFINES THE BASE NAME FOR THE
!              STRINGS.
!     OUTPUT--THE CHARACTER STRINGS.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS MAXOBV.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/10
!     ORIGINAL VERSION--OCTOBER   2011.
!     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEL
!
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
!
      CHARACTER*4 ISTRZ2(24)
!
      CHARACTER*4 ISUBN0
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZC.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 IERRFI
!
      CHARACTER*24 IATEMP
      CHARACTER*12 IFRMT
      CHARACTER*24 IXTEMP(9999)
      EQUIVALENCE (CGARBG(1),IXTEMP(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='ST  '
      IERROR='NO'
!
      NBASE=0
!
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
!
!               ********************************************
!               **  STEP 1--                              **
!               **  OPEN THE DPZCHF.DAT FILE.             **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
!
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
!
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,   &
                  ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
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
        GO TO 8000
      ENDIF
!
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
!
      IVAR=-1
      DO 130 I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
        ENDIF
  130 CONTINUE
      IF(IVAR.GT.0)GO TO 199
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)IHRIGH,IHRIG2
  131 FORMAT('***** VARIABLE ',2A4,' NOT FOUND IN THE CHARACTER ',   &
             'DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  171 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  181 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,183)
  183 FORMAT('      ERROR READING THE VARIABLE NAMES ',   &
             'IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  199 CONTINUE
!
!               **********************************
!               **  STEP 2--                    **
!               **  DETERMINE NUMBER OF STRINGS **
!               **  TO CREATE                   **
!               **********************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NSTR=NUMVAR
      IF(NSTR.GT.9999)NSTR=9999
!
!               *************************************************
!               **  STEP 3--                                   **
!               **  EXTRACT THE BASE NAME ON THE LHS OF THE    **
!               **  EQUAL SIGN AND THEN LOOP THROUGH THE       **
!               **  NUMBER OF STRINGS TO CREATE.               **
!               *************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT(1:4)=IHARG(1)
      IHLEFT(5:8)=IHARG2(1)
      NBASE=1
      DO 310 I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE=I
          GO TO 319
        ENDIF
  310 CONTINUE
  319 CONTINUE
!
      IF(NSTR.LE.9)THEN
        IF(NBASE.GT.7)NBASE=7
      ELSEIF(NSTR.LE.99)THEN
        IF(NBASE.GT.6)NBASE=6
      ELSEIF(NSTR.LE.999)THEN
        IF(NBASE.GT.5)NBASE=5
      ELSE
        IF(NBASE.GT.4)NBASE=4
      ENDIF
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IVAR.EQ.1)THEN
        IFRMT='(A24)'
      ELSE
        IFRMT='(   X,A24)'
        WRITE(IFRMT(2:4),'(I3)')25*(IVAR-1)
      ENDIF
!
      N=0
      IROW=0
!
      DO 410 I=1,MAXOBV
!
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROW=I
!
!       CHECK TO SEE IF TEXT ON CURRENT ROW IS NEW OR
!       HAS BEEN PREVIOUSLY ENTERED.
!
        INEW=1
        IF(N.GE.1)THEN
          DO 420 J=1,N
            IF(IATEMP(1:24).EQ.IXTEMP(J)(1:24))THEN
              INEW=0
              GO TO 429
            ENDIF
  420     CONTINUE
  429     CONTINUE
        ENDIF
!
!       ADD NEW STRING IF REQUIRED
!
        IF(INEW.EQ.0)GO TO 410
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
!
          NEWNAM='NO'
          NEWCOL='NO'
          ICASEL='UNKN'
          NIOLD1=0
          ICOLL=0
!
          DO 510 II=1,NUMNAM
            I2=II
            IF(ISTRIN(1:4).EQ.IHNAME(I2).AND.   &
               ISTRIN(5:8).EQ.IHNAM2(I2))THEN
              IF(IUSE(I2).EQ.'F')THEN
                ICASEL='STRI'
                ILISTL=I2
                GO TO 519
              ELSE
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,111)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,513)ISTRIN
  513           FORMAT('      THE NAME ',A8,' ALREADY EXISTS, BUT NOT ',   &
                       'AS A STRING.')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,515)
  515           FORMAT('      THIS STRING WILL NOT BE CREATED.')
                CALL DPWRST('XXX','BUG ')
                GO TO 9000
              ENDIF
            ENDIF
  510     CONTINUE
  519     CONTINUE
!
          NEWNAM='YES'
          ICASEL='STRI'
!
          ILISTL=NUMNAM+1
          IF(ILISTL.GT.MAXNAM)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,522)
  522       FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',   &
                   'FUNCTION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,524)MAXNAM
  524       FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
!               *****************************************************
!               **  STEP 6--                                       **
!               **  ADD THE CURRENT STRING                         **
!               *****************************************************
!
          ISTEPN='6'
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'STGL')   &
             CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          NCHAR=1
          DO 605 JJ=24,1,-1
            IF(IXTEMP(N)(JJ:JJ).NE.' ')THEN
              NCHAR=JJ
              GO TO 609
            ENDIF
  605     CONTINUE
  609     CONTINUE
          IHLEF3=ISTRIN(1:4)
          IHLEF4=ISTRIN(5:8)
          DO 611 J=1,NCHAR
            ISTRZ2(J)=' '
            ISTRZ2(J)(1:1)=IXTEMP(N)(J:J)
  611     CONTINUE
!
          CALL DPINFU(ISTRZ2,NCHAR,IHNAME,IHNAM2,IUSE,IN,   &
                      IVSTAR,IVSTOP,   &
                      NUMNAM,IANS,IWIDTH,IHLEF3,IHLEF4,ILISTL,   &
                      NEWNAM,MAXNAM,   &
                      IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ENDIF
!
  410 CONTINUE
      GO TO 499
!
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)I
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',   &
             'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8000
!
  499 CONTINUE
      GO TO 8000
!
!               ***************************************
!               **  STEP 88--                        **
!               **  CLOSE THE DPZCHF.DAT FILE.       **
!               ***************************************
!
 8000 CONTINUE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8001)N,ISTRIN(1:NBASE)
 8001   FORMAT('      ',I5,' STRINGS CREATED WITH BASE NAME = ',A8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
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
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,IXTEMP(I)
 9016     FORMAT('I,IXTEMP(I) = ',I8,A24)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEST
      SUBROUTINE CODEX(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--GIVEN DATA OF THE FORM
!
!
!              1  1  1  0  0  0  1  1
!
!              WE WANT TO CREATE A CODED VARIABLE
!
!              1  1  1  0  0  0  2  2
!
!              THAT IS, FOR EACH NON-ZERO CHUNK, WE WANT TO
!              CREATE A COUNTER FOR EACH NON-ZERO BLOCK.
!
!              THIS IS USED IN THE CONTEXT OF A TAG VARIABLE
!              WHERE THE TAG IS SET TO 1 WHEN SOME CONDITION IS
!              SATISFIED.  HOWEVER, WE WANT TO UNIQUELY IDENTIFY
!              EACH CONTIGUOUS CHUNK OF DATA THAT SATISFIES THE
!              CONDITION.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/07
!     ORIGINAL VERSION--JULY      2017.
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
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='X   '
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('      THE NUMBER OF OBSERVATIONS, ',I8,' IS LESS ',   &
               'THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************************************
!               **  STEP 2--                                       **
!               **  PERFORM THE CODING--                           **
!               *****************************************************
!
      ICNT=0
      IFLAG=0
      DO 600 I=1,N
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
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)X(1),Y(1)
  812   FORMAT('THE FIRST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',   &
               'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,814)X(N),Y(N)
  814   FORMAT('THE LAST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',   &
               'VALUE ',F10.0)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEX
      SUBROUTINE CODEZ(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE IS SIMILAR TO THE CODE ROUTINE.
!              HOWEVER, IT DIFFERS IN ONE KEY RESPECT.  THE CODE
!              ROUTINE CODES BASED ON THE DISTINCT VALUES REGARDLESS
!              OF THE ORDER OF THE DATA.  THIS ROUTINE CREATES THE
!              CODE BASED ON WHEN THE INPUT VECTOR CHANGES VALUE.
!              FOR EXAMPLE, IF X HAS
!
!                  1 1 1 2 2 2 3 3 3 1 1 2 2 2 2 3 3 3
!
!              THEN THE CODED VECTOR WILL BE
!
!                  1 1 1 2 2 2 3 3 3 4 4 5 5 5 5 6 6
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS TO BE CODED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE CODED VALUES WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR Y WHICH WILL CONTAIN THE CODED
!             VALUES CORRESPONDING TO THE OBSERVATIONS IN THE VECTOR X.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
!     REFERENCES--NONE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2016/6
!     ORIGINAL VERSION--JUNE      2016.
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
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CODE'
      ISUBN2='Z   '
!
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  COMPUTE CODED VALUES.  **
!               *****************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CODEZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('      THE NUMBER OF OBSERVATIONS, ',I8,' IS LESS ',   &
               'THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        Y(1)=1.0
        GO TO 9000
      ENDIF
!
!
!               *****************************************************
!               **  STEP 2--                                       **
!               **  PERFORM THE CODING--                           **
!               *****************************************************
!
      HOLD=X(1)
      ACODE=1.0
      Y(1)=ACODE
      DO 600 I=2,N
        IF(X(I).EQ.HOLD)THEN
          Y(I)=ACODE
        ELSE
          HOLD=X(I)
          ACODE=ACODE+1.0
          Y(I)=ACODE
        ENDIF
  600 CONTINUE
!
!               ******************************
!               **  STEP 3--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)X(1),Y(1)
  812   FORMAT('THE FIRST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',   &
               'VALUE ',F10.0)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,814)X(N),Y(N)
  814   FORMAT('THE LAST OUTPUT VALUE (= ',G15.7,' ) HAS CODE ',   &
               'VALUE ',F10.0)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ODEZ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CODEZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CODEZ
      SUBROUTINE COENAM(IV1,IV2,IWORD1,IWORD2,IBUGCN,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A HOLLERITH COEFFICIENT NAME
!              FROM THE 2 INPUT INTEGER VALUES IV1 AND IV2.
!              IT ALSO AUTOMATICALLY PUTS THE LETTER A AS
!              THE FIRST LETTER OF THE PARAMETER NAME.
!              EXAMPLES--
!                 INPUT--IV1 = 1   AND IV2 = 7    OUTPUT--A17
!                 INPUT--IV1 = 2   AND IV2 = 3    OUTPUT--A23
!                 INPUT--IV1 = 5   AND IV2 = 2    OUTPUT--A52
!     NOTE--IF THE OUTPUT STRING HAPPENS TO CONSIST OF
!           1 TO 4 CHARACTERS, THEN CHARACTERS 1 TO 4
!           WILL BE PLACED INTO THE FIRST HOLLERITH
!           VARIABLE IWORD1.
!           IF THE OUTPUT STRING HAPPENS TO CONSIST OF
!           MORE THAN 4 CHARACTERS, THEN CHARACTERS 5 TO 8
!           WILL BE PLACED INTO THE SECOND HOLLERITH
!           VARIABLE IWORD2.
!           IF THE OUTPUT STRING HAPPENS TO CONSIST OF
!           MORE THAN 8 CHARACTERS, THEN CHARACTERS 9 ON UP
!           WILL BE IGNORED.
!     NOTE--IV1 AND IV2 ARE INTEGER VARIABLES.
!     NOTE--IWORD1 IS A HOLLERITH VARIABLE.
!         --IWORD2 IS A HOLLERITH VARIABLE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1978.
!     UPDATED         --MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IBUGCN
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTRIT
      CHARACTER*4 ISTRIN
      CHARACTER*4 IWORD3
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION ISTRIT(15)
      DIMENSION ISTRIN(30)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      ISUBN1='COEN'
      ISUBN2='AM  '
!
      IF(IBUGCN.EQ.'OFF')GO TO 90
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
!
!               **********************************
!               **  STEP 1--                    **
!               **  DEFINE THE FIRST CHARACTER  **
!               **  OF THE PARAMETER NAME       **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      K=0
      K=K+1
      ISTRIN(K)='A'
!
!               *******************************************
!               **  STEP 2--                             **
!               **  FORM THE STRING CONTAINING           **
!               **  THE 1 CHARACTER PER WORD             **
!               **  REPRESENTATION OF THE VALUE IN IV1.  **
!               *******************************************
!
      J=0
      ISTEPN='2'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREM=IV1
      DO 100 IPASS=1,10
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
      IF(IREM.LE.0)GO TO 140
  100 CONTINUE
  140 CONTINUE
      N1=J
!
      DO 150 I=1,N1
      K=K+1
      IREV=N1-I+1
      ISTRIN(K)=ISTRIT(IREV)
  150 CONTINUE
!
!               *******************************************
!               **  STEP 3--                             **
!               **  FORM THE STRING CONTAINING           **
!               **  THE 1 CHARACTER PER WORD             **
!               **  REPRESENTATION OF THE VALUE IN IV2.  **
!               *******************************************
!
      ISTEPN='3'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      IREM=IV2
      DO 200 IPASS=1,10
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
      IF(IREM.LE.0)GO TO 240
  200 CONTINUE
  240 CONTINUE
      N2=J
!
      DO 250 I=1,N2
      K=K+1
      IREV=N2-I+1
      ISTRIN(K)=ISTRIT(IREV)
  250 CONTINUE
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  CONVERT THE 1 CHARACTER PER WORD REPRESENTATION  **
!               **  FOR THE PARAMETER NAME                           **
!               **  INTO A 4 CHARACTER PER WORD REPRESENTATION.      **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGCN.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=1
      ISTOP=K
      CALL DP1H4H(ISTART,ISTOP,ISTRIN,   &
      IWORD1,IWORD2,IWORD3,NUMWD,NUMCH,IBUGCN,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGCN.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COENAM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N1,N2,ISTART,ISTOP
 9013   FORMAT('N1,N2,ISTART,ISTOP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ISTRIN(I),I=1,K)
 9014   FORMAT('ISTRIN(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMBPC,NUMCPW,NUMWD,NUMCH
 9015   FORMAT('NUMBPC,NUMCPW,NUMWD,NUMCH = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IWORD1,IWORD2,IWORD3
 9017   FORMAT('IWORD1,IWORD2,IWORD3 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE COENAM
      SUBROUTINE COLLAP(NVAR, X, Y, LOCY, NX, NY, DIM, CONFIG)
!
!        ALGORITHM AS 51.1  APPL. STATIST. (1972) VOL.21, P.218
!
!        COMPUTES A MARGINAL TABLE FROM A COMPLETE TABLE.
!        ALL PARAMETERS ARE ASSUMED VALID WITHOUT TEST.
!
!        IF THE VALUE OF NVAR IS TO BE GREATER THAN 7, THE
!        DIMENSIONS IN THE DECLARATIONS OF SIZE AND COORD MUST
!        BE INCREASED TO NVAR+1 AND NVAR RESPECTIVELY.
!
      INTEGER SIZE(8), DIM(NVAR), CONFIG(NVAR), COORD(7)
!
!        THE LARGER TABLE IS X AND THE SMALLER ONE IS Y
!
      REAL X(NX), Y(NY), ZERO
      DATA ZERO /0.0/
!
!        INITIALISE ARRAYS
!
      SIZE(1) = 1
      DO 10 K = 1, NVAR
         L = CONFIG(K)
         IF (L .EQ. 0) GO TO  20
         SIZE(K + 1) = SIZE(K) * DIM(L)
   10 CONTINUE
!
!        FIND NUMBER OF VARIABLES IN CONFIGURATION
!
      K = NVAR + 1
   20 CONTINUE
      N = K - 1
!
!        INITIALISE Y. FIRST CELL OF MARGINAL TABLE IS
!        AT Y(LOCY) AND TABLE HAS SIZE(K) ELEMENTS
!
      LOCU = LOCY + SIZE(K) - 1
      DO 30 J = LOCY, LOCU
         Y(J) = ZERO
   30 CONTINUE
!
!        INITIALISE COORDINATES
!
      DO 50 K = 1, NVAR
         COORD(K) = 0
   50 CONTINUE
!
!        FIND LOCATIONS IN TABLES
!
      I = 1
   60 CONTINUE
      J = LOCY
      DO 70 K = 1, N
         L = CONFIG(K)
         J = J + COORD(L) * SIZE(K)
   70 CONTINUE
      Y(J) = Y(J) + X(I)
!
!        UPDATE COORDINATES
!
      I = I + 1
      DO 80 K = 1, NVAR
         COORD(K) = COORD(K) + 1
         IF (COORD(K) .LT. DIM(K)) GO TO  60
         COORD(K) = 0
   80 CONTINUE
!
      RETURN
      END SUBROUTINE COLLAP
      SUBROUTINE COMARI(Y1,Y2,Y3,Y4,N1,IACASE,IWRITE,   &
                        Y5,Y6,N5,SCAL3,ITYP3,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT COMPLEX ARITHMETIC OPERATIONS
!              OF THE COMPLEX DATA IN Y1,Y2 AND Y3,Y4.
!
!     OPERATIONS--ADDITION
!                 SUBTRACTTION
!                 MULTIPLICATION
!                 DIVISION
!                 EXPONENTIATION
!                 SQUARE ROOT
!                 ROOTS OF A POLYNOMIAL (WITH COMPLEX COEFFICIENTS)
!                 CONJUGATE
!
!     INPUT  ARGUMENTS--Y1 (REAL PART)       Y2 (IMAGINARY PART)
!                     --Y3 (REAL PART)       Y4 (IMAGINARY PART)
!     OUTPUT ARGUMENTS--Y5 (REAL PART)       Y6 (IMAGINARY PART)
!
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTORS Y5(.) AND Y6(.)
!           BEING IDENTICAL TO THE INPUT VECTORS Y1(.) AND Y2(.), OR
!           Y3(.) AND Y4(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/5
!     ORIGINAL VERSION--APRIL     1987.
!     UPDATED         --AUGUST    1987.  COMPLEX SQUARE ROOT
!     UPDATED         --AUGUST    1987.  COMPLEX ROOTS OF POLYNOMIAL
!     UPDATED         --SEPTEMBER 1987.  COMPLEX CONJUGATE
!     UPDATED         --MAY       1995.  EQUIVALENCE FOR ARRAYS
!     UPDATED         --AUGUST    1995.  REPLACE NUMERICAL RECIPES
!                                        ROUTINE FOR COMPLEX ROOTS
!                                        WITH CMLIB ROUTINE
!     UPDATED         --JUNE      2019.  DIMENSION COEFS, ROOTS,
!                                        WORK, ERRBND
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
!-----COMPLEX STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      COMPLEX CY1Y2
      COMPLEX CTRANS
      COMPLEX COEFS
      COMPLEX ROOTS
!CCCC FOLLOWING LINES ADDED AUGUST 1995
      COMPLEX WORK
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
!-----LOGICAL STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC LOGICAL POLISH
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION Y5(*)
      DIMENSION Y6(*)
!
      DIMENSION COEFS(MAXOBV)
      DIMENSION ROOTS(MAXOBV)
      DIMENSION WORK(MAXOBV)
      DIMENSION ERRBND(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(JGAR15),COEFS(1))
      EQUIVALENCE (GARBAG(JGAR17),ROOTS(1))
      EQUIVALENCE (GARBAG(JGAR19),WORK(1))
      EQUIVALENCE (GARBAG(IGAR10),ERRBND(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMA'
      ISUBN2='RI  '
!
      IERROR='NO'
!
      SCAL3=(-999.0)
      ITYP3='VECT'
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'MARI')GO TO 90
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
      DO 55 I=1,N1
      WRITE(ICOUT,56)I,Y1(I),Y2(I),Y3(I),Y4(I)
   56 FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I) = ',I8,4E13.5)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ***********************************************
!               **  CARRY OUT COMPLEX ARITHMETIC OPERATIONS  **
!               ***********************************************
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
!               ********************************************
!
      IF(N1.LT.1)GO TO 1100
      GO TO 1190
!
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
 1154 FORMAT('      THE COMPLEX ADDITION IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COAD')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COSU')WRITE(ICOUT,1155)
 1155 FORMAT('      THE COMPLEX SUBTRACTION IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COSU')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COMU')WRITE(ICOUT,1156)
 1156 FORMAT('      THE COMPLEX MULTIPLICATION IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COMU')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'CODI')WRITE(ICOUT,1157)
 1157 FORMAT('      THE COMPLEX DIVISION IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'CODI')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COEX')WRITE(ICOUT,1158)
 1158 FORMAT('      THE COMPLEX EXPONENTIATION IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COEX')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COSR')WRITE(ICOUT,1159)
 1159 FORMAT('      THE COMPLEX SQUARE ROOT IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COSR')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'CORO')WRITE(ICOUT,1160)
 1160 FORMAT('      THE COMPLEX ROOTS ARE TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'CORO')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COR1')WRITE(ICOUT,1161)
 1161 FORMAT('      THE COMPLEX ROOTS ARE TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COR1')CALL DPWRST('XXX','BUG ')
      IF(IACASE.EQ.'COCO')WRITE(ICOUT,1162)
 1162 FORMAT('      THE COMPLEX CONJUGATE IS TO BE ',   &
      'COMPUTED')
      IF(IACASE.EQ.'COCO')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1171)
 1171 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)N1
 1173 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1190 CONTINUE
!
!               *********************************
!               **  STEP 12--                  **
!               **  BRANCH TO THE PROPER CASE  **
!               *********************************
!
      IF(IACASE.EQ.'COAD')GO TO 2100
      IF(IACASE.EQ.'COSU')GO TO 2200
      IF(IACASE.EQ.'COMU')GO TO 2300
      IF(IACASE.EQ.'CODI')GO TO 2400
      IF(IACASE.EQ.'COEX')GO TO 2500
      IF(IACASE.EQ.'COSR')GO TO 2600
      IF(IACASE.EQ.'CORO')GO TO 2700
      IF(IACASE.EQ.'COR1')GO TO 2700
      IF(IACASE.EQ.'COCO')GO TO 2800
!
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
      GO TO 9000
!
!               *********************************************
!               **  STEP 21--                              **
!               **  TREAT THE COMPLEX ADDITION       CASE  **
!               *********************************************
!
 2100 CONTINUE
      DO 2110 I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1+DY3
      DY6=DY2+DY4
      Y5(I)=DY5
      Y6(I)=DY6
 2110 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *********************************************
!               **  STEP 22--                              **
!               **  TREAT THE COMPLEX SUBTRACTION    CASE  **
!               *********************************************
!
 2200 CONTINUE
      DO 2210 I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1-DY3
      DY6=DY2-DY4
      Y5(I)=DY5
      Y6(I)=DY6
 2210 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *********************************************
!               **  STEP 23--                              **
!               **  TREAT THE COMPLEX MULTIPLICATION CASE  **
!               *********************************************
!
 2300 CONTINUE
      DO 2310 I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DY5=DY1*DY3-DY2*DY4
      DY6=DY1*DY4+DY2*DY3
      Y5(I)=DY5
      Y6(I)=DY6
 2310 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *********************************************
!               **  STEP 24--                              **
!               **  TREAT THE COMPLEX DIVISION       CASE  **
!               *********************************************
!
 2400 CONTINUE
      DO 2410 I=1,N1
      DY1=Y1(I)
      DY2=Y2(I)
      DY3=Y3(I)
      DY4=Y4(I)
      DDEN=DY3**2+DY4**2
      IF(DDEN.NE.0.0D0)GO TO 2419
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
      GO TO 9000
 2419 CONTINUE
      DY5=(DY1*DY3+DY2*DY4)/DDEN
      DY6=(DY2*DY3-DY1*DY4)/DDEN
      Y5(I)=DY5
      Y6(I)=DY6
 2410 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *********************************************
!               **  STEP 25--                              **
!               **  TREAT THE COMPLEX EXPONENTIATION CASE  **
!               *********************************************
!
 2500 CONTINUE
      DO 2510 I=1,N1
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
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *********************************************
!               **  STEP 26--                              **
!               **  TREAT THE COMPLEX SQUARE ROOT    CASE  **
!               *********************************************
!
 2600 CONTINUE
      DO 2610 I=1,N1
      CY1Y2=CMPLX(Y1(I),Y2(I))
      CTRANS=CSQRT(CY1Y2)
      Y5(I)=REAL(CTRANS)
      Y6(I)=AIMAG(CTRANS)
 2610 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               ***********************************************
!               **  STEP 27--                                **
!               **  TREAT THE COMPLEX ROOTS OF A POLYNOMIAL  **
!               **  WITH COMPLEX COEFFICIENTS CASE           **
!               ***********************************************
!
 2700 CONTINUE
      NCOEFS=N1
      NROOTS=NCOEFS-1
!
!CCCC AUGUST 1995.  REPLACE NUMERICAL RECIPES ROUTINE WITH
!CCCC SLATEC ROUTINE.
!CCCC CPZERO EXPECTS COEFFICIENTS IN OPPOSIT ORDER OF ZROOTS.
!CCCC DO2710I=1,NCOEFS
!CCCC COEFS(I)=CMPLX(Y1(I),Y2(I))
!2710 CONTINUE
      ICOUNT=0
      DO 2710 I=NCOEFS,1,-1
      ICOUNT=ICOUNT+1
      COEFS(ICOUNT)=CMPLX(Y1(I),Y2(I))
 2710 CONTINUE
!
      IFLG=0
      CALL CPZERO(NROOTS,COEFS,ROOTS,WORK,IFLG,ERRBND)
      IF(IFLG.EQ.1)THEN
        WRITE(ICOUT,2721)
 2721   FORMAT('***** ERROR IN COMARI--LEADING COEFFICIENT IS ',   &
               'ZERO OR DEGREE IS ZERO')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLG.EQ.2)THEN
        WRITE(ICOUT,2726)
 2726   FORMAT('***** ERROR IN COMARI--ROOTS DID NOT CONVERGE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!CCCC POLISH=.FALSE.
!CCCC CALL ZROOTS(COEFS,NROOTS,ROOTS,POLISH)
!
!CCCC DO2720I=1,NROOTS
!CCCC ROOTS(I)=ROOTS(I)*(1.0+0.01*I)
!2720 CONTINUE
!
!CCCC POLISH=.TRUE.
!CCCC CALL ZROOTS(COEFS,NROOTS,ROOTS,POLISH)
!
      DO 2730 I=1,NROOTS
      Y5(I)=REAL(ROOTS(I))
      Y6(I)=AIMAG(ROOTS(I))
 2730 CONTINUE
!
      ITYP3='VECT'
      N5=NROOTS
      GO TO 9000
!
!               *********************************************
!               **  STEP 28--                              **
!               **  TREAT THE COMPLEX CONJUGATE      CASE  **
!               *********************************************
!
 2800 CONTINUE
      DO 2810 I=1,N1
      Y5(I)=Y1(I)
      Y6(I)=(-Y2(I))
 2810 CONTINUE
!
      ITYP3='VECT'
      N5=N1
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'MARI')GO TO 9090
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
      IF(ITYP3.EQ.'SCAL')GO TO 9090
      DO 9015 I=1,N1
      WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I),Y4(I),Y5(I),Y6(I)
 9016 FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I),Y5(I),Y6(I) = ',I8,6E13.5)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE COMARI
      SUBROUTINE COMDIG(X,N,IWRITE,XDIGI,NDIGI,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE COMMON DIGITS FOR A
!              VECTOR OF NUMBERS.  FOR EXAMPLE, GIVEN
!                0.0321, 0.0323, 0.0329, 0.0325
!              THE COMMON DIGITS ARE 0.03.  NOTE THAT ONLY DIGITS
!              TO THE RIGHT OF THE DECIMAL PLACE ARE CONSIDERED.
!              THE FOLLOWING SPECIAL CASES ARE CONSIDERED:
!                  1) IF THE FIRST DECIMAL DOES NOT AGREE, SET
!                     XDIGI=-1.0.
!                  2) IF THE INTEGER PORTION OF THE NUMBER DOES
!                     NOT AGREE, THEN SET XDIGI=-1.0.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XDIGI  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED COMMON DIGITS
!                     --NDIGI  = THE INTEGER VALUE OF THE
!                                NUMBER OF COMMON DIGITS
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             COMMON DIGITS
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001.8
!     ORIGINAL VERSION--AUGUST    2001.
!
      PARAMETER(MAXDIG=7)
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
      DIMENSION DIGITS(MAXDIG)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMD'
      ISUBN2='IG  '
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.GE.2)GO TO 119
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
      GO TO 9000
  119 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMDIG--',   &
      'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XDIGI=ABS(HOLD)-REAL(INT(ABS(HOLD)))
      NDIGI=MAXDIG
      GO TO 9000
  139 CONTINUE
!
!  CHECK IF INTEGER PORTION OF NUMBERS MATCHES FOR ALL THE NUMBERS.
!
      IHOLD=INT(X(1))
      DO 145 I=2,N
        IXTEMP=INT(X(I))
        IF(IXTEMP.NE.IHOLD)THEN
          NDIG=-1
          XDIGI=0.0
          IF(IFEEDB.EQ.'OFF')GO TO 149
          IF(IWRITE.EQ.'OFF')GO TO 149
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,146)N
  146 FORMAT('THE INTEGER PORTION OF THE  ',I8,' OBSERVATIONS DOES ',   &
             'NOT MATCH.')
          CALL DPWRST('XXX','BUG ')
  149     CONTINUE
          GO TO 800
        ENDIF
  145 CONTINUE
!
!               ************************
!               **  STEP 2--          **
!               **  COMPUTE THE DIGITS**
!               ************************
!
      XDIGI=0.0
      NDIGI=0
!
      DO 200 L=1,MAXDIG
        ATEMP=X(1)*10**(L-1)
        ADIG=ABS(ATEMP) - INT(ABS(ATEMP))
        IDIG=INT(ADIG*10)
        DO 300 I=2,N
          ATEMP=X(I)*10**(L-1)
          ADIG=ABS(ATEMP) - INT(ABS(ATEMP))
          IDIG2=INT(ADIG*10)
          IF(IDIG.NE.IDIG2)GO TO 209
  300   CONTINUE
        NDIGI=NDIGI+1
        DIGITS(NDIGI)=IDIG
  200 CONTINUE
  209 CONTINUE
!
      IF(NDIGI.GT.0)THEN
        XDIGI=REAL(INT(X(1)))*(10**NDIGI)
        DO 400 I=1,NDIGI
          ATEMP=DIGITS(I)*(10**(NDIGI-I))
          XDIGI=XDIGI + ATEMP
  400   CONTINUE
        XDIGI=XDIGI/(10**NDIGI)
      ENDIF
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
  800 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)N,NDIGI
  811 FORMAT('THE NUMBER OF COMMON DIGITS FOR THE ',I8,   &
             ' OBSERVATIONS = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,813)XDIGI
  813 FORMAT('THE COMMON DIGITS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE COMDIG
      SUBROUTINE COMOVE(X,Y,N,IWRITE,XYCOMO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE (LEIGH-PERLMAN) COMOVEMENT COEFFICIENT
!              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
!              THE SAMPLE COMOVEMENT COEFFICIENT WILL BE A SINGLE
!              PRECISION VALUE CALCULATED AS THE
!              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE SECOND SET
!                                OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--XYCOMO = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE COMOVEMENT COEFFICIENT
!                                BETWEEN THE 2 SETS OF DATA
!                                IN THE INPUT VECTORS X AND Y.
!                                THIS SINGLE PRECISION VALUE
!                                WILL BE BETWEEN -1.0 AND 1.0
!                                (INCLUSIVELY).
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COMOVEMENT COEFFICIENT BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--AN INDEX FOR COMOVEMENT OF TIME SEQUENCES
!                 WITH GEOPHYSICAL APPLICATIONS:  A WORKING PAPER
!                 (PENN STATE INTERFACE CONFERANCE ON ASTRONOMY
!                 AUGUST 11-14, 1991)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--92/8
!     ORIGINAL VERSION--AUGUST    1991.
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
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMO'
      ISUBN2='VE  '
!
      IERROR='NO'
!
      DN=0.0D0
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMXY=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I),Y(I)
   56 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *******************************************
!               **  COMPUTE     COMOVEMENT COEFFICIENT  **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.GE.2)GO TO 119
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
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.2)GO TO 120
      GO TO 129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',   &
      'THE THIRD INPUT ARGUMENT (N) HAS THE VALUE 2')
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
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
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',   &
      'THE FIRST  INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
      GO TO 9000
  139 CONTINUE
!
      HOLD=Y(1)
      DO 145 I=2,N
      IF(Y(I).NE.HOLD)GO TO 149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,146)HOLD
  146 FORMAT('***** NON-FATAL DIAGNOSTIC IN COMOVE--',   &
      'THE SECOND INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOMO=1.0
      GO TO 9000
  149 CONTINUE
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE     COMOVEMENT COEFFICIENT.  **
!               ************************************************
!
      DN=N
      DSUMX=0.0D0
      DSUMY=0.0D0
      DSUMXY=0.0D0
      DO 300 I=2,N
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
      WRITE(ICOUT,811)N,XYCOMO
  811 FORMAT('THE LEIGH-PERLMAN COMOVEMENT COEF. OF THE ',   &
      I8,' OBSERV. = ',E15.7)
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
!
      RETURN
      END SUBROUTINE COMOVE
      SUBROUTINE COMPIC(IFUNC1,N1,IOLD,IOLD2,INEW,INEW2,NCHANG,   &
      IFUNC2,N2,IBUGA3,IERROR)
!
!     PURPOSE--SCAN THE FUNCTIONAL EXPRESSION GIVEN IN IFUNC1
!              AND CHANGE ALL OCCURRANCES OF
!              PARAMETER, VARIABLE, FUNCTION, AND
!              NUMBERS GIVEN IN IOLD BY THE CORRESPONDING
!              STRINGS GIVEN IN INEW.
!     NOTE--IT IS ASSUMED THAT NAMES ARE
!           ALREADY IN THE FORM OF A4--THAT IS
!           INDIVIDUALLY PACKED PER WORD.
!     NOTE--NUMBERS MAY NOT BE CHANGED.
!     NOTE--PARAMETERS MAY BE CHANGED TO NUMBERS
!           BUT ONLY THE FIRST 8 CHARACTERS OF THE NUMBER WILL
!           BE TRANSFERRED.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JULY      1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IFUNC1
      CHARACTER*4 IOLD
      CHARACTER*4 IOLD2
      CHARACTER*4 INEW
      CHARACTER*4 INEW2
      CHARACTER*4 IFUNC2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION IFUNC1(*)
      DIMENSION IFUNC2(*)
      DIMENSION IOLD(*)
      DIMENSION IOLD2(*)
      DIMENSION INEW(*)
      DIMENSION INEW2(*)
!
      DIMENSION ICH11(10)
      DIMENSION ICH12(10)
      DIMENSION ICH1(20)
      DIMENSION ICH21(10)
      DIMENSION ICH22(10)
      DIMENSION ICH2(20)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMP'
      ISUBN2='IC  '
!
      IERROR='NO'
!
      NUMASC=4
      NUMAS2=2*NUMASC
!
      IEND1=0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,NCHANG
      WRITE(ICOUT,56)I,IOLD(I),IOLD2(I),INEW(I),INEW2(I)
   56 FORMAT('I,IOLD(I),IOLD2(I),INEW(I),INEW2(I) = ',   &
      I8,2X,A4,A4,2X,A4,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  STEP 1--                              **
!               **  COPY THE INPUT FUNCTION IN IFUNC1(.)  **
!               **  INTO THE OUTPUT VECTOR IFUNC2(.).     **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.0)GO TO 190
      DO 80 I=1,N1
      IFUNC2(I)=IFUNC1(I)
   80 CONTINUE
      N2=N1
!
!               *****************************************
!               **  STEP 2--                           **
!               **  LOOP THROUGH THE INPUT FUNCTION--  **
!               **  1 CHARACTER (USUALLY) AT A TIME.   **
!               *****************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I=0
  100 CONTINUE
      I=I+1
      IF(I.GT.N2)GO TO 190
      IF(NCHANG.LE.0)GO TO 190
!
!               ******************************************
!               **  STEP 3--                            **
!               **  FOR THIS CHARACTER (CHARACTER I),   **
!               **  SCAN THROUGH ALL POTENTIAL CHANGES  **
!               **  TO BE MADE.                         **
!               ******************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 200 J=1,NCHANG
      CALL DPXH1H(IOLD(J),ICH11,IEND11,IBUGA3)
      CALL DPXH1H(IOLD2(J),ICH12,IEND12,IBUGA3)
      DO 205 K=1,NUMAS2
      ICH1(K)=' '
  205 CONTINUE
      L=0
      DO 206 K=1,NUMASC
      L=L+1
      ICH1(L)=ICH11(K)
  206 CONTINUE
      DO 207 K=1,NUMASC
      L=L+1
      ICH1(L)=ICH12(K)
  207 CONTINUE
      IEND1=0
      IF(IEND11.GE.1)IEND1=IEND11
      IF(IEND11.GE.NUMASC)IEND1=NUMASC
      IF(IEND12.GE.1)IEND1=NUMASC+IEND12
      IF(IEND12.GE.NUMAS2)IEND1=NUMAS2
!
      IF(IEND1.LE.0)GO TO 200
!
!               *********************************************
!               **  STEP 4--                               **
!               **  CHECK FOR A LEFT OR RIGHT PARENTHESIS  **
!               **  IN THE INPUT CHANGE PATTERN.           **
!               *********************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEP='NO'
      DO 210 K=1,IEND1
      IF(ICH1(K).EQ.'(')GO TO 220
      IF(ICH1(K).EQ.')')GO TO 220
  210 CONTINUE
      ICASEP='NO'
      GO TO 290
  220 CONTINUE
      ICASEP='YES'
  290 CONTINUE
!
!               ********************************************************
!               **  STEP 5--                                          **
!               **  STARTING WITH CHARACTER I OF THE INPUT FUNCTION,  **
!               **  COMPARE THE STRING IN THE INPUT FUNCTION          **
!               **  WITH THIS INPUT CHANGE PATTERN.                   **
!               **  DETERMINE IF THERE IS A MATCH.                    **
!               ********************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      L1=I-1
      DO 300 K=1,IEND1
      L1=L1+1
      IF(IFUNC2(L1).EQ.ICH1(K))GO TO 300
      GO TO 200
  300 CONTINUE
!
!               **********************************************
!               **  STEP 6--                                **
!               **  IF HAVE A MATCH,                        **
!               **  CHECK TO SEE IF THE STRING              **
!               **  IN THE FUNCTION                         **
!               **  IS PRECEDED BY A +, -, *, /, **, (,     **
!               **  (OR IS THE FIRST STRING ON THE LINE),   **
!               **  AND ALSO                                **
!               **  IS SUCCEDED BY A +, -, *, /, **, ),     **
!               **  (OR IS THE LAST  STRING ON THE LINE).   **
!               **  A FULFILLMENT OF ANY OF THE ABOVE       **
!               **  14 CONDITIONS WILL BE SUFFICIENT        **
!               **  TO ASSURE THAT INDIVIDUAL MIDDLE        **
!               **  CHARACTERS IN LIBRARY FUNCTIONS         **
!               **  (E.G., THE 'X' IN 'EXP')                **
!               **  AND IN MULTI-CHARACTER VARIABLE NAMES   **
!               **  (E.G., THE 'X' IN 'FLUX')               **
!               **  WILL NOT BE INADVERTANTLY CHANGED       **
!               **  (E.G., BY, SAY, 'FOR X = 3').           **
!               **********************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEA='NO'
      IHALF1='NO'
      IHALF2='NO'
!
      IM1=I-1
      IF(IM1.LE.0)GO TO 410
      IF(IFUNC2(IM1).EQ.'+')GO TO 410
      IF(IFUNC2(IM1).EQ.'-')GO TO 410
      IF(IFUNC2(IM1).EQ.'*')GO TO 410
      IF(IFUNC2(IM1).EQ.'/')GO TO 410
      IF(IFUNC2(IM1).EQ.'**')GO TO 410
      IF(IFUNC2(IM1).EQ.'(')GO TO 410
      IHALF1='NO'
      GO TO 419
  410 CONTINUE
      IHALF1='YES'
  419 CONTINUE
!
      L1P1=L1+1
      IF(L1P1.GT.N2)GO TO 420
      IF(IFUNC2(L1P1).EQ.'+')GO TO 420
      IF(IFUNC2(L1P1).EQ.'-')GO TO 420
      IF(IFUNC2(L1P1).EQ.'*')GO TO 420
      IF(IFUNC2(L1P1).EQ.'/')GO TO 420
      IF(IFUNC2(L1P1).EQ.'**')GO TO 420
      IF(IFUNC2(L1P1).EQ.')')GO TO 420
      IHALF2='NO'
      GO TO 429
  420 CONTINUE
      IHALF2='YES'
  429 CONTINUE
!
      ICASEA='NO'
      IF(IHALF1.EQ.'YES'.AND.IHALF2.EQ.'YES')ICASEA='YES'
!
!               *********************************************************
!               **  STEP 7--                                           **
!               **  IF THE INPUT STRING HAD ANY PARENTHESES,           **
!               **     THEN CHANGE ANY MATCHING STRING IN THE FUNCTION.**
!               **  IF THE INPUT STRING HAD NO PARENTHESES,            **
!               **     THEN CHANGE MATCHING STRINGS IN THE FUNCTION    **
!               **     ONLY WHEN THE MATCHING FUNCTION SUBSTRING       **
!               **     IS PRECEDED BY A +, -, *, /, **, (,             **
!               **     (OR IS THE FIRST STRING ON THE LINE), AND ALSO  **
!               **     IS SUCCEDED BY A +, -, *, /, **, ),             **
!               **     (OR IS THE LAST  STRING ON THE LINE).           **
!               *********************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEP.EQ.'YES')GO TO 590
      IF(ICASEP.EQ.'NO'.AND.ICASEA.EQ.'YES')GO TO 590
      GO TO 200
  590 CONTINUE
!
!               **************************************************
!               **  STEP 8--                                    **
!               **  IF CHANGES ARE TO BE MADE,                  **
!               **  EXTRACT THE OUTPUT CHANGE PATTERN           **
!               **  CORRESPONDING TO THE INPUT CHANGE PATTERN.  **
!               **************************************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPXH1H(INEW(J),ICH21,IEND21,IBUGA3)
      CALL DPXH1H(INEW2(J),ICH22,IEND22,IBUGA3)
      DO 605 K=1,NUMAS2
      ICH2(K)=' '
  605 CONTINUE
      L=0
      DO 606 K=1,NUMASC
      L=L+1
      ICH2(L)=ICH21(K)
  606 CONTINUE
      DO 607 K=1,NUMASC
      L=L+1
      ICH2(L)=ICH22(K)
  607 CONTINUE
      IEND2=0
      IF(IEND21.GE.1)IEND2=IEND21
      IF(IEND21.GE.NUMASC)IEND2=NUMASC
      IF(IEND22.GE.1)IEND2=NUMASC+IEND21
      IF(IEND22.GE.NUMAS2)IEND2=NUMAS2
!
      IF(IEND2.LE.0)GO TO 200
!
!               ******************************
!               **  STEP 9--                **
!               **  CARRY OUT THE CHANGES   **
!               **  IN THE INPUT FUNCTION.  **
!               ******************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=I
      ISTOP1=ISTAR1+IEND1-1
      ISTAR2=1
      ISTOP2=ISTAR2+IEND2-1
      CALL DPSIRS(IFUNC2,N2,ISTAR1,ISTOP1,ICH2,IEND2,ISTAR2,ISTOP2,   &
      IBUGA3,IERROR)
      I=ISTOP1+(IEND2-IEND1)
      GO TO 100
!
  200 CONTINUE
      GO TO 100
!
  190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COMPIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,N1,NCHANG,NUMASC,NUMAS2
 9012   FORMAT('IBUGA3,N1,N2,NCHANG,NUMASC,NUMAS2 = ',A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)(IFUNC1(I),I=1,N1)
 9013   FORMAT('IFUNC1(.)=',30A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NCHANG
          WRITE(ICOUT,9016)I,IOLD(I),IOLD2(I),INEW(I),INEW2(I)
 9016     FORMAT('I,IOLD(I),IOLD2(I),INEW(I),INEW2(I) = ',   &
                 I8,2X,2A4,2X,2A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9018)(IFUNC2(I),I=1,N2)
 9018   FORMAT('IFUNC2(.)=',30A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IEND11,IEND12,IEND1,IEND21,IEND22,IEND2
 9020   FORMAT('IEND11,IEND12,IEND1,IEND21,IEND22,IEND2 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)(ICH11(I),I=1,10)
 9021   FORMAT('(ICH11(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)(ICH12(I),I=1,10)
 9022   FORMAT('(ICH12(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)(ICH1 (I),I=1,10)
 9023   FORMAT('(ICH1 (I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)(ICH21(I),I=1,10)
 9024   FORMAT('(ICH21(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)(ICH22(I),I=1,10)
 9025   FORMAT('(ICH22(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)(ICH2 (I),I=1,10)
 9026   FORMAT('(ICH2 (I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE COMPIC
      SUBROUTINE COMPID(IA,NUMCHA,IPASS,PARAM,IPARN1,IPARN2,NUMPAR,   &
      IVARN1,IVARN2,NUMVAR,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,ID,NUMCHD,   &
      IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE DETERMINES THE DERIVATIVE OF
!              A FORTRAN MATHEMATICAL FUNCTION EXPRESSION.
!     NOTE--TYPICALLY THIS SUBROUTINE IS CALLED ONLY
!           WITH IPASS=2; IN SUCH CASE,
!           IPARN1(.) AND NUMPAR ARE NEVER DETERMINED,
!           NEEDED, OR OUTPUTTED.
!           (NOTE--THERE EXISTS POSSIBLE DIFFERENCES WITH NUMPAR
!           AS DEFINED FOR THIS SUBROUTINE
!           AS OPPOSED TO THE DEFINITION FOR COMPID).
!     INPUT  ARGUMENTS--IA     = THE HOLLARITH VECTOR WHICH CONTAINS
!                                THE FUNCTION OF INTEREST
!                                FOR WHICH THE ANALYTIC DERIVATIVE
!                                IS TO BE DETERMINED.
!                                IA(.) MAY BE EITHER UNPACKED (1 CHARACTER PER W
!                                OR PACKED (4 CHARACTERS PER WORD)
!                                ALTHOUGH THE USUAL REPRESENTATION IS UNPACKED.
!                     --NUMCHA = THE INTEGER VALUE WHICH
!                                DEFINES THE NUMBER OF CHARACTERS IN IA.
!                                NUMCHA DEFINES THE LENGTH OF THE
!                                HOLLARITH STRING TO BE OPERATED ON.
!                     --IPASS  = AN INTEGER FLAG CODE
!                                WHICH DEFINES WHICH PASS (1 OR 2) INTO THIS
!                                SUBROUTINE THE USER IS IN.
!                                PASS 1 DETERMINE PARAMETER NAMES;
!                                PASS 2 DOES FUNCTION EVALUATIONS.
!                     --PARAM  = THE SINGLE PRECISION VECTOR OF PARAMETER
!                                (AND VARIABLE)
!                                VALUES CORRESPONDING TO THE PARAMETER NAMES
!                                AS GIVEN IN THE VECTOR IPARN1.
!                     --IPARN1 = THE INTEGER VECTOR OF PARAMETER
!                                (AND VARIABLE)
!                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
!     OUTPUT ARGUMENTS--ID     = THE HOLLARITH VECTOR WHICH CONTAINS
!                                THE DESIRED DERIVATIVE FUNCTION.
!                                ID(.) IS UNPACKED (THAT IS,
!                                1 CHARACTER PER WORD).
!                     --NUMCHD = THE INTEGER VALUE WHICH
!                                DEFINES THE NUMBER OF CHARACTERS IN ID.
!                                NUMCHD DEFINES THE LENGTH OF THE
!                                HOLLARITH STRING FOR THE DERIVATIVE FUNCTION.
!     OUTPUT--THE SINGLE PRECISION COMPUTED SCALAR VALUE,
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER           SUBROUTINES NEEDED--EVAL
!     FORTRAN LIBRARY SUBROUTINES NEEDED--(ALL IN EVAL)
!                                         SQRT
!                                         EXP
!                                         LOG
!                                         LOG10
!                                         SIN
!                                         COS
!                                         ATAN
!                                         ATAN2
!                                         TANH
!                                         ABS
!                                         AINT
!                                         ARCSIN
!                                         ARCCOS
!                                         ARCTAN
!                                         OCTAL
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     NOTE--THIS SUBROUTINE ALLOWS ONE TO PERFORM
!           INTERACTIVE FUNCTION EVALUATIONS.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--DECEMBER  1978.
!     UPDATED         --JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --APRIL     1986.
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!CCCC CHARACTER*4 IBUG0
!CCCC CHARACTER*4 IBUG1
!CCCC CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!CCCC CHARACTER*4 IBUG4
!CCCC CHARACTER*4 IBUG5
!CCCC CHARACTER*4 IBUG6
!CCCC CHARACTER*4 IBUG7
!CCCC CHARACTER*4 IBUGXH
!CCCC CHARACTER*4 IBUGCD
!
      CHARACTER*4 ID
      CHARACTER*4 ID1
      CHARACTER*4 ID2
      CHARACTER*4 ID3
      CHARACTER*4 ICON
!
!---------------------------------------------------------------------
!
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
!
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
!
      DIMENSION ID(*)
!
!     NOTE--THE DIMENSIONS OF ITYPEH, IW21HO, IW22HO, AND W2HOLD
!           WHICH ARE DEFINED IN THE MAIN PROGRAM
!           SHOULD BE AT LEAST AS LARGE AS THE DIMENSIONS
!           OF IW21 AND IW22 BELOW.
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
!CCCC DIMENSION IB(225)
!CCCC DIMENSION IR(225)
!CCCC DIMENSION IBEGIN(225)
!CCCC DIMENSION IEND(225)
!CCCC DIMENSION ITYPE(225)
!CCCC DIMENSION IW21(225)
!CCCC DIMENSION IW22(225)
!CCCC DIMENSION W2(225)
      DIMENSION IB(1000)
      DIMENSION IR(1000)
      DIMENSION IBEGIN(1000)
      DIMENSION IEND(1000)
      DIMENSION ITYPE(1000)
      DIMENSION IW21(1000)
      DIMENSION IW22(1000)
      DIMENSION W2(1000)
!
      DIMENSION ID1(250)
      DIMENSION ID2(250)
      DIMENSION ID3(250)
!
      DIMENSION ICH(10)
!
      DIMENSION IL(10)
!
      DIMENSION ICON(1000)
      DIMENSION ICON1(50)
      DIMENSION ICON2(50)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!
!CCCC DATA IBUG0/'OFF'/
!CCCC DATA IBUG1/'OFF'/
!CCCC DATA IBUG2/'OFF'/
!CCCC DATA IBUG3/'OFF'/
!CCCC DATA IBUG4/'OFF'/
!CCCC DATA IBUG5/'OFF'/
!CCCC DATA IBUG6/'OFF'/
!CCCC DATA IBUG7/'OFF'/
!CCCC DATA IBUGXH/'OFF'/
!CCCC DATA IBUGCD/'OFF'/
!
!     DEFINE THE UPPER LIMIT OF THE NUMBER OF CHARACTERS
!     THAT MAY BE PROCESSED BY THIS SUBROUTINE
!     (COUNTING BLANKS, LEFT-HAND SIDE, EQUAL SIGN,
!     AND RIGHT HAND SIDE).
!     IF RESTRICT THE EXPRESSION TO 1 LINE IMAGE,
!     THEN A REASONABLE UPPER BOUND IS 80.
!     WHATEVER UPPER BOUND IS SET,
!     THE DIMENSIONS OF MOST OF THE VECTORS
!     MUST BE EQUAL OR LARGER TO THIS NUMBER.
!     (THE VECTOR IL(.) WHICH CONTAINS THE
!     NUMBER OF CHARACTERS TO THE LEFT
!     OF THE EQUAL SIGN (BLANKS IGNORED)
!     MAY BE MUCH SMALLER--LIKE 6.)
!     NOTE--AS OF JANUARY 1979, THE BOUND WAS RESET TO 150.
!
!CCCC DATA MAXCHA/150/
!CCCC DATA MAXCHA/225/
      DATA MAXCHA/1000/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMP'
      ISUBN2='ID  '
!
      IERROR='NO  '
!
!     THE FOLLOWING STATEMENT (N=1) HAS BEEN ADDED
!     IN CONVERTING THE COMPIL SUBROUTINE
!     TO THE COMPID SUBROUTINE.
!
      N=1
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 90
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
      IF(NUMPAR.LE.0)GO TO 64
      DO 62 I=1,NUMPAR
      WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I),PARAM(I)
   63 FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2X,A4,2X,A4,2X,   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   64 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)NUMVAR
   65 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMVAR.LE.0)GO TO 69
      DO 66 I=1,NUMVAR
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
      IF(NWHOLD.LE.0)GO TO 79
      DO 72 I=1,NWHOLD
      WRITE(ICOUT,73)I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I)
   73 FORMAT('I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I) = ',   &
      I8,2X,A4,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   79 CONTINUE
   90 CONTINUE
!
!               ************************************************************
!               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.  **
!               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND         **
!               **  REGARDLESS OF THE WORD SIZE.                          **
!               ************************************************************
!
      NUMASC=4
      NUMAS2=2*NUMASC
      NUMAS3=3*NUMASC
      NUMAS4=4*NUMASC
!
!     CHECK THAT THE INPUT NUMBER OF CHARACTERS NUMCHA
!     (INCLUDING LEFT SIDE, RIGHT SIDE, EQUAL SIGN,
!     AND BLANKS) IS AT LEAST 1 AND AT MOST MAXCHA
!     (WHERE MAXCHA IS THE INTERNALLY DEFINED VARIABLE
!     WHICH CONTROLS DIMENSION SIZES AND WHICH
!     TYPICALLY HAS THE VALUE 80).
!
      IF(1.LE.NUMCHA.AND.NUMCHA.LE.MAXCHA)GO TO 139
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
      GO TO 9000
  139 CONTINUE
!
!     BLANK-OUT AND ZERO-OUT SOME VARIABLES AND VECTORS.
!
!CCCC Y=0.0
      IC2=0
!
      DO 160 I=1,NUMCHA
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
!
!
!               ***********************************************
!               **  STEP 1--                                 **
!               **  OPERATE ON THE VECTOR IA(.).             **
!               **  IA(.) MAY BE OPTIONALLY EITHER UNPACKED  **
!               **  (1 CHARACTER PER WORD),                  **
!               **  OR PACKED                                **
!               **  (UP TO 4 CHARACTERS PER WORD).           **
!               **  IN ANY EVENT, IB(.) IS UNPACKED.         **
!               **  NOTE ALSO THAT IB(.) HAS BLANKS OMITTED. **
!               ***********************************************
!
      ISTEPN='1'
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      K=0
      DO 200 I=1,NUMCHA
      IF(IA(I).EQ.'    ')GO TO 200
      CALL DPXH1H(IA(I),ICH,ILASTC,IBUGEV)
      IF(ILASTC.LE.0)GO TO 200
      DO 250 J=1,ILASTC
      K=K+1
      IB(K)=ICH(J)
  250 CONTINUE
  200 CONTINUE
      NCTOT=K
!
      IF(NCTOT.GE.1)GO TO 290
      WRITE(ICOUT,205)NCTOT
  205 FORMAT('***** ERROR IN COMPID--TOTAL NUMBER OF CHARACTERS ',   &
      'IN MODEL (INCL. BOTH SIDES, BLANKS, AND EQUAL SIGN) ',   &
      'IS < 2.  NCTOT = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,271)NUMCHA,N,IPASS
  271 FORMAT('NUMCHA,N,IPASS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,272)(IA(I),I=1,NUMCHA)
  272 FORMAT('IA--',80A1)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,281)NUMPAR
  281 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMPAR.LE.0)GO TO 289
      DO 282 I=1,NUMPAR
      WRITE(ICOUT,283)I,IPARN1(I),IPARN2(I),PARAM(I)
  283 FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2X,A4,2X,A4,2X,   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
  282 CONTINUE
  289 CONTINUE
      IERROR='YES '
      GO TO 9000
!
  290 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 299
      WRITE(ICOUT,291)NCTOT
  291 FORMAT('NCTOT = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 292 I=1,NCTOT
      WRITE(ICOUT,293)I,IB(I)
  293 FORMAT('I,IB(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  292 CONTINUE
  299 CONTINUE
!
!               **************************************************************
!               **  STEP 2--                                                **
!               **  OPERATE ON THE VECTOR IB(.).                            **
!               **  DETERMINE THE NUMBER OF CHARACTERS (IF ANY)             **
!               **  FOR THE LEFT-HAND SIDE.  OUTPUT THEM INTO THE           **
!               **  VECTOR IL(.).                                           **
!               **************************************************************
!
      DO 500 I=1,NCTOT
      I2=I
      IF(IB(I).EQ.'=   ')GO TO 550
  500 CONTINUE
      NCL=0
      ISTARR=1
      GO TO 559
  550 CONTINUE
      NCL=I2-1
      ISTARR=I2+1
  559 CONTINUE
!
      IF(NCL.LE.0)GO TO 699
      DO 600 I=1,NCL
      IL(I)=IB(I)
  600 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 699
      ISTEPN='2'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,691)NCL
  691 FORMAT('NCL = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 692 I=1,NCL
      WRITE(ICOUT,693)I,IL(I)
  693 FORMAT('I,IL(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  692 CONTINUE
  699 CONTINUE
!
!               ***************************************************************
!               **  STEP 3--                                                 **
!               **  OPERATE ON THE VECTOR IB(.).                             **
!               **  DETERMINE THE NUMBER OF CHARACTERS FOR RIGHT-HAND SIDE.  **
!               **  OUTPUT THEM INTO THE VECTOR IR(.).                       **
!               ***************************************************************
!
      IF(ISTARR.LE.NCTOT)GO TO 719
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
      GO TO 9000
  719 CONTINUE
!
      K=0
      DO 700 I=ISTARR,NCTOT
      K=K+1
      IR(K)=IB(I)
  700 CONTINUE
      NCR=K
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 799
      ISTEPN='3'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,791)NCR
  791 FORMAT('NCR = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 792 I=1,NCR
      WRITE(ICOUT,793)I,IR(I)
  793 FORMAT('I,IR(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  792 CONTINUE
  799 CONTINUE
!
!               ****************************************************************
!               **  STEP 4--
!               **  OPERATE ON THE VECTOR IR(.).
!               **  SIMPLIFY THE RIGHT-HAND SIDE.
!               **  ANALYZE THE RIGHT-HAND SIDE.
!               **  DETERMINE THE NUMBER OF DIFFERENT LOGICAL COMPONENTS.
!               **        1. NUMBER (CONSISTING OF 0,1,2,...,9 OR .)
!               **        2. X VARIABLE
!               **        3. OPERATION (+   -   *   /   **)
!               **        4. PARENTHESES (   (   OR   )    )
!               **        5. LIBRARY FUNCTION (ALOG   EXP   ETC + AUGMENTED LIB.
!               **        6. COMMA (FOR MULTI-ARGUMENT LIBRARY FUNCTIONS)
!               **        7. PARAMETER (ANYTHING NOT ABOVE)
!               **  CHECK FOR SYNTAX ERRORS.
!               **  OUTPUT THE TYPE COMPONENT INTO ITYPE(.).
!               **  OUTPUT THE START LOCATION IN IR(.) OF EACH COMPONENT INTO IB
!               **  OUTPUT THE STOP  LOCATION IN IR(.) OF EACH COMPONENT INTO IE
!               ****************************************************************
!
      CALL DPSIPA(IR,NCR,IBUGEV,IERROR)
      CALL DPSISI(IR,NCR,IBUGEV,IERROR)
      CALL DPSIP1(IR,NCR,IBUGEV,IERROR)
      CALL DPSIP0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIE1(IR,NCR,IBUGEV,IERROR)
      CALL DPSIE0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIA0(IR,NCR,IBUGEV,IERROR)
      CALL DPSIA2(IR,NCR,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(IR,NCR,IBUGEV,IERROR)
!
      NW=0
      I=1
      NCON=0
 1050 CONTINUE
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
!
      IF(IR(I).EQ.'0   ')GO TO 1100
      IF(IR(I).EQ.'1   ')GO TO 1100
      IF(IR(I).EQ.'2   ')GO TO 1100
      IF(IR(I).EQ.'3   ')GO TO 1100
      IF(IR(I).EQ.'4   ')GO TO 1100
      IF(IR(I).EQ.'5   ')GO TO 1100
      IF(IR(I).EQ.'6   ')GO TO 1100
      IF(IR(I).EQ.'7   ')GO TO 1100
      IF(IR(I).EQ.'8   ')GO TO 1100
      IF(IR(I).EQ.'9   ')GO TO 1100
      IF(IR(I).EQ.'.   ')GO TO 1100
!
!     NOTE--THE FOLLOWING LINE IS BEING COMMENTED OUT
!     SO AS TO GENERALIZE COMPIL INTO COMPID
!     (1 VARIABLE INTO MANY VARIABLES).
!
!CCCC IF(IR(I).EQ.'X   ')GO TO 1200
!
      IF(IR(I).EQ.'+   ')GO TO 1300
      IF(IR(I).EQ.'-   ')GO TO 1300
      IF(IR(I).EQ.'*   ')GO TO 1300
      IF(IR(I).EQ.'/   ')GO TO 1300
!
      IF(IR(I).EQ.'(   ')GO TO 1410
      IF(IR(I).EQ.')   ')GO TO 1420
!
      IF(IR(I).EQ.',   ')GO TO 1700
!
!     CHECK FOR A LIBRARY FUNCTION.
!
!CCCC CALL CKLIBF(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      CALL CKLIB1(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'NO')CALL CKLIB2(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 1069
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1061)
 1061 FORMAT('***** IN COMPID, AFTER RETURNING FROM CKLIBF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1062)NCR,I
 1062 FORMAT('NCR,I = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 1063 I4=1,NCR
      WRITE(ICOUT,1064)I4,IR(I4)
 1064 FORMAT('I4,IR(I4) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1063 CONTINUE
      WRITE(ICOUT,1065)IFOUND,NCLF,IERROR
 1065 FORMAT('IFOUND,NCLF,IERROR = ',A4,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1069 CONTINUE
!
      IF(IERROR.EQ.'YES ')GO TO 9000
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.8)GO TO 1580
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.7)GO TO 1570
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.6)GO TO 1560
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.5)GO TO 1550
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.4)GO TO 1540
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.3)GO TO 1530
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.2)GO TO 1520
      IF(IFOUND.EQ.'YES '.AND.NCLF.EQ.1)GO TO 1510
!
      GO TO 1600
!
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
      IF(J.GT.NCR)GO TO 1160
      IF(IR(J).EQ.'0   ')GO TO 1150
      IF(IR(J).EQ.'1   ')GO TO 1150
      IF(IR(J).EQ.'2   ')GO TO 1150
      IF(IR(J).EQ.'3   ')GO TO 1150
      IF(IR(J).EQ.'4   ')GO TO 1150
      IF(IR(J).EQ.'5   ')GO TO 1150
      IF(IR(J).EQ.'6   ')GO TO 1150
      IF(IR(J).EQ.'7   ')GO TO 1150
      IF(IR(J).EQ.'8   ')GO TO 1150
      IF(IR(J).EQ.'9   ')GO TO 1150
      IF(IR(J).EQ.'.   ')GO TO 1150
 1160 CONTINUE
      ICON2(NCON)=IC2
      JMAX=J-1
      GO TO 1800
!
!1200 CONTINUE
!CCCC NW=NW+1
!CCCC NLPWP=0
!CCCC NRPWP=0
!CCCC JMIN=I
!CCCC J=I
!CCCC ILOOP=0
 1250 CONTINUE
      J=J+1
      IF(J.GT.NCR)GO TO 1260
      IF(IR(J).EQ.'+   ')GO TO 1260
      IF(IR(J).EQ.'-   ')GO TO 1260
      IF(IR(J).EQ.'*   ')GO TO 1260
      IF(IR(J).EQ.'/   ')GO TO 1260
      IF(IR(J).EQ.'(   ')NLPWP=NLPWP+1
      IF(IR(J).EQ.')   ')NRPWP=NRPWP+1
      IF(IR(J).EQ.')   '.AND.NRPWP.GT.NLPWP)GO TO 1260
      ILOOP=ILOOP+1
      IF(ILOOP.LE.NUMAS2)GO TO 1250
      WRITE(ICOUT,1256)NUMAS2
 1256 FORMAT('***** ERROR IN COMPID--PARAMETER NAME EXCEEDS ',I8,   &
      'CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      DO 1257 K=JMIN,J
      WRITE(ICOUT,1258)K,IR(K)
 1258 FORMAT('K,IR(K) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1257 CONTINUE
      IERROR='YES '
      GO TO 9000
 1260 CONTINUE
      JMAX=J-1
!     THE FOLLOWING STATEMENT HAS BEEN
!     COMMENTED OUT IN GOING FROM THE
!     COMPIL SUBROUTINE TO THE COMPID
!     SUBROUTINE SO THAT X WILL NOT
!     BE TREATED AS A SPECIAL VARIABLE.
!CCCC IF(JMAX.EQ.JMIN)ITYPE(NW)='X   '
      IF(JMAX.GT.JMIN)ITYPE(NW)='PAR '
      GO TO 1800
!
 1300 CONTINUE
      NW=NW+1
      ITYPE(NW)='OP  '
      JMIN=I
      JMAX=I
      IP1=I+1
      IF(IR(I).EQ.'*   '.AND.IR(IP1).EQ.'*   ')JMAX=IP1
      GO TO 1800
!
 1410 CONTINUE
      NW=NW+1
      ITYPE(NW)='LP  '
      JMIN=I
      JMAX=I
      GO TO 1800
 1420 CONTINUE
      NW=NW+1
      ITYPE(NW)='RP  '
      JMIN=I
      JMAX=I
      GO TO 1800
!
 1510 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I
      GO TO 1800
!
 1520 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+1
      GO TO 1800
!
 1530 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+2
      GO TO 1800
!
 1540 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+3
      GO TO 1800
!
 1550 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+4
      GO TO 1800
!
 1560 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+5
      GO TO 1800
!
 1570 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+6
      GO TO 1800
!
 1580 CONTINUE
      NW=NW+1
      ITYPE(NW)='LF  '
      JMIN=I
      JMAX=I+7
      GO TO 1800
!
 1700 CONTINUE
      NW=NW+1
      ITYPE(NW)='COM '
      JMIN=I
      JMAX=I
      GO TO 1800
!
 1600 CONTINUE
      NW=NW+1
      ITYPE(NW)='PAR '
      NLPWP=0
      NRPWP=0
      JMIN=I
      J=I
      ILOOP=0
!
 1650 CONTINUE
      J=J+1
      IF(J.GT.NCR)GO TO 1660
      IF(IR(J).EQ.'+   ')GO TO 1660
      IF(IR(J).EQ.'-   ')GO TO 1660
      IF(IR(J).EQ.'*   ')GO TO 1660
      IF(IR(J).EQ.'/   ')GO TO 1660
      IF(IR(J).EQ.'(   ')NLPWP=NLPWP+1
      IF(IR(J).EQ.')   ')NRPWP=NRPWP+1
      IF(IR(J).EQ.')   '.AND.NRPWP.GT.NLPWP)GO TO 1660
      IF(IR(J).EQ.',   ')GO TO 1660
      ILOOP=ILOOP+1
      IF(ILOOP.LE.NUMAS2)GO TO 1650
      WRITE(ICOUT,1656)NUMAS2
 1656 FORMAT('***** ERROR IN COMPID--PARAMETER NAME EXCEEDS ',I8,   &
      'CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      DO 1657 K=JMIN,J
      WRITE(ICOUT,1658)K,IR(K)
 1658 FORMAT('K,IR(K) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1657 CONTINUE
      IERROR='YES '
      GO TO 9000
 1660 CONTINUE
      JMAX=J-1
      GO TO 1800
!
 1800 CONTINUE
!
!     CHECK THAT NW HAS NOT EXCEEDED MAXCHA (USUALLY 80)
!
      IF(NW.LE.MAXCHA)GO TO 1900
      WRITE(ICOUT,1901)
 1901 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1902)
 1902 FORMAT('      THE VARIABLE NW HAS JUST EXCEEDED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1903)
 1903 FORMAT('      THE MAX ALLOWABLE LIMIT DEFINED ',   &
      'BY THE INTERNAL VARIABLE MAXCHA.')
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
 1907 FORMAT('      THE NUMBER OF (PACKED) CHARACTERS ON ',   &
      'RIGHT-HAND SIDE NCR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCR.GE.1)WRITE(ICOUT,1908)(IR(I),I=1,NCR)
 1908 FORMAT('      (PACKED) RIGHT-HAND SIDE--',95A1)
      IF(NCR.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
 1900 CONTINUE
!
      IBEGIN(NW)=JMIN
      IEND(NW)=JMAX
      I=JMAX
!
      I=I+1
      IF(I.LE.NCR)GO TO 1050
!
!     TEST THAT NW IS POSITIVE.
!
      IF(NW.GE.1)GO TO 1959
      WRITE(ICOUT,1951)NW
 1951 FORMAT('***** ERROR IN COMPID--NW IS NON-POSITIVE. ',   &
      'NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
 1959 CONTINUE
!
      IF(NW.EQ.1)GO TO 1969
      DO 1960 I=1,NW
      IP1=I+1
      IF(ITYPE(I).EQ.'LF  '.AND.ITYPE(IP1).NE.'LP  ')GO TO 1961
      GO TO 1960
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
 1962 FORMAT('***** ERROR IN COMPID--LIBRARY FUNCTION ',   &
      'NOT FOLLOWED BY A LEFT PARENTHESES')
      CALL DPWRST('XXX','BUG ')
 1963 FORMAT('             NW = ',I8)
 1964 FORMAT('             I  = ',I8)
 1965 FORMAT('             ITYPE(I) = ',A4)
 1966 FORMAT('             ITYPE(I+1) = ',A4)
      IERROR='YES '
      GO TO 9000
 1960 CONTINUE
 1969 CONTINUE
!
      IF(ITYPE(NW).EQ.'OP  ')GO TO 1970
      IF(ITYPE(NW).EQ.'LF  ')GO TO 1972
      GO TO 1979
!
 1970 CONTINUE
      WRITE(ICOUT,1971)ITYPE(NW)
 1971 FORMAT('***** ERROR IN COMPID--LAST TERM IN TOTAL ',   &
      ' EXPRESSION IS AN OPERATION = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
 1972 CONTINUE
      WRITE(ICOUT,1973)ITYPE(NW)
 1973 FORMAT('***** ERROR IN COMPID--LAST TERM IN TOTAL ',   &
      ' EXPRESSION = A LIBRARY FUNCTION = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
 1979 CONTINUE
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 1999
      ISTEPN='4'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,1991)NW,ICMIN
 1991 FORMAT('NW,ICMIN = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 1992 I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,1993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),   &
      IBEGIN(I),IEND(I)
 1993 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',   &
      'IBEGIN(I),IEND(I) = ',I8,2X,3A4,A4,2X,I8,2X,I8)
      CALL DPWRST('XXX','BUG ')
 1992 CONTINUE
 1999 CONTINUE
!
!               ****************************************************************
!               **  STEP 5--
!               **  OPERATE ON EACH COMPONENT OF THE VECTOR IR(.).
!               **  CONVERT THE NUMBERS TO FLOATING POINT VALUES.
!               **  CONVERT THE PARAMETERS TO FLOATING POINT VALUES.
!      *  SET THE X TO A DUMMY VALUE OF 0.0 FOR THE TIME BEING.       **
!               **  CONVERT THE OPERATIONS INTO A 1-WORD REPRESENTATION.
!               **  'CONVERT' THE PARENTHESES INTO A 1-WORD REPRESENTATION.
!               **  CONVERT THE COEFFICIENTS TO COEFFICIENT VALUES.
!               **  CONVERT THE LIBRARY FUNCTIONS INTO A 1-WORD REPRESENTATION.
!               **  SAVE THE CONTENTS OF ITYPE, IW2, AND W2 IN
!               **  ITYPEH, IW21HO, AND WHOLD FOR LATER USE
!               **  IN REDEFINING ITYPE, IW2, AND W2 FOR EACH NEW X VALUE.
!               **  OUTPUT THE VECTORS IW2 AND W2.
!               **  OUTPUT THE VECTORS IW21HO, W2HOLD, AND ITYPEH.
!               ****************************************************************
!
!CCCC IC=0 APRIL 29, 1986
      IC3=0
      DO 3000 I=1,NW
      ICMIN=IBEGIN(I)
      ICMAX=IEND(I)
      IF(ITYPE(I).EQ.'N   ')GO TO 3100
      IF(ITYPE(I).EQ.'X   ')GO TO 3200
      IF(ITYPE(I).EQ.'OP  ')GO TO 3300
      IF(ITYPE(I).EQ.'LP  '.OR.ITYPE(I).EQ.'RP  ')GO TO 3400
      IF(ITYPE(I).EQ.'PAR ')GO TO 3500
      IF(ITYPE(I).EQ.'LF  ')GO TO 3600
      IF(ITYPE(I).EQ.'COM ')GO TO 3700
      WRITE(ICOUT,3005)
 3005 FORMAT('***** ERROR IN COMPID--ITYPE(I) NOT X, OP, LP, PAR, ',   &
      'OR LF')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3006)I,ITYPE(I),IW21(I),W2(I)
 3006 FORMAT('I,ITYPE(I),IW21(I),W2(I) = ',   &
      I8,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
!
 3100 CONTINUE
!CCCC IC=IC+1 APRIL 29, 1986
      IC3=IC3+1
!CCCC IW21(I)=IC
!CCCC CALL DPC4IH(IC,IW21(I),IBUGEV,IERROR) APRIL 29, 1986
      CALL DPC4IH(IC3,IW21(I),IBUGEV,IERROR)
      IW22(I)='    '
      W2(I)=0.0
      IANS1='    '
      IANS2='    '
      IANS3='    '
      IANS4='    '
      J=0
      DO 3150 IC=ICMIN,ICMAX
      J=J+1
      JM1=J-1
      L=J-(NUMASC*(JM1/NUMASC))
      K=NUMBPC*(L-1)
      K=IABS(K)
!CCCC WRITE(ICOUT,3333)J,JM1,L,K,IR(IC)
!3333 FORMAT('J,JM1,L,K,IR(IC) = ',4I8,2X,A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IF(J.LE.NUMASC)GO TO 3151
      IF(J.LE.NUMAS2)GO TO 3152
      IF(J.LE.NUMAS3)GO TO 3153
      IF(J.LE.NUMAS4)GO TO 3154
      GO TO 3155
 3151 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS1)
      GO TO 3155
 3152 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS2)
      GO TO 3155
 3153 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS3)
      GO TO 3155
 3154 CONTINUE
      CALL DPCHEX(0,NUMBPC,IR(IC),K,NUMBPC,IANS4)
      GO TO 3155
 3155 CONTINUE
!CCCC WRITE(ICOUT,4444)IANS1,IANS2,IANS3,IANS4
!4444 FORMAT(4A4)
!CCCC CALL DPWRST('XXX','BUG ')
 3150 CONTINUE
      CALL ERRORF(IANS1,IANS2,IANS3,IANS4,-1000000000.0,1000000000.0,   &
      1000000000.0,ANS2,IERROR)
      IF(IERROR.EQ.'YES ')GO TO 9000
      W2(I)=ANS2
      GO TO 3000
!
 3200 CONTINUE
      W2(I)=0.0
      GO TO 3000
!
 3300 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      ICMINP=ICMIN+1
      IF(IR(ICMIN).EQ.'*   '.AND.IR(ICMINP).EQ.'*   ')IW21(I)='**  '
      IF(IR(ICMIN).EQ.'*   '.AND.IR(ICMINP).EQ.'*   ')IW22(I)='    '
      GO TO 3000
!
 3400 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      GO TO 3000
!
 3500 CONTINUE
      IW21(I)='    '
      IW22(I)='    '
      ICMAX2=ICMIN+NUMAS2-1
      IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
      J=0
      DO 3530 IC=ICMIN,ICMAX2
      J=J+1
      J2=J
      IF(J2.GT.NUMASC)J2=J-NUMASC
      ISTAR3=NUMBPC*(J2-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
 3530 CONTINUE
!
      IF(IPASS.EQ.1)GO TO 3000
!
      IF(NUMPAR.LE.0)GO TO 3559
      DO 3550 J=1,NUMPAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GO TO 3555
 3550 CONTINUE
      GO TO 3559
 3555 CONTINUE
      W2(I)=PARAM(J)
      GO TO 3000
 3559 CONTINUE
!
      IF(NUMVAR.LE.0)GO TO 3569
      DO 3560 J=1,NUMVAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GO TO 3565
 3560 CONTINUE
 3565 CONTINUE
      W2(I)=0.0
      ITYPE(I)='VAR '
      GO TO 3000
 3569 CONTINUE
!
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
 3574 FORMAT('                       ADMISSIBLE PARAM./VAR. ',   &
      'NAMES = ')
      CALL DPWRST('XXX','BUG ')
      DO 3575 J=1,NUMPAR
      WRITE(ICOUT,3576)J,IPARN1(J),IPARN2(J)
 3576 FORMAT('                       PARAM./VAR. NAME ',I4,'--  ',   &
      2A4)
      CALL DPWRST('XXX','BUG ')
 3575 CONTINUE
      WRITE(ICOUT,3577)(IA(J),J=1,NUMCHA)
 3577 FORMAT('      FUNCTION EXPRESSION--',100A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES '
      GO TO 9000
!
 3600 CONTINUE
      IW21(I)='    '
      IW22(I)='    '
      ICMAX2=ICMIN+NUMAS2-1
      IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
      J=0
      DO 3650 IC=ICMIN,ICMAX2
      J=J+1
      J2=J
      IF(J2.GT.NUMASC)J2=J-NUMASC
      ISTAR3=NUMBPC*(J2-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW21(I))
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IR(IC),ISTAR3,NUMBPC,IW22(I))
 3650 CONTINUE
      GO TO 3000
!
 3700 CONTINUE
      IW21(I)=IR(ICMIN)
      IW22(I)='    '
      GO TO 3000
!
 3000 CONTINUE
      NWHOLD=NW
      DO 3900 I=1,NW
      ITYPEH(I)=ITYPE(I)
      IW21HO(I)=IW21(I)
      IW22HO(I)=IW22(I)
      W2HOLD(I)=W2(I)
 3900 CONTINUE
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 3999
      ISTEPN='5'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      DO 3992 I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,3993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),   &
      IW21(I),IW22(I),W2(I)
 3993 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',   &
      'IW21(I),IW22HO(I),W2(I) = ',I8,2X,3A4,2X,A4,2X,A4,2X,A4,2X,E15.6)
      CALL DPWRST('XXX','BUG ')
 3992 CONTINUE
 3999 CONTINUE
!
!               ****************************************************
!               **  STEP 6--                                      **
!               **  THIS STEP IS TO BE EXECUTED IF IPASS=1;       **
!               **  OTHERWISE IT IS SKIPPED.                      **
!               **  IF THIS STEP IS EXECUTED, STEP 7 IS NOT;      **
!               **  IF THIS STEP IS NOT EXECUTED, STEP 7 IS.      **
!               **  OPERATE ON IW2 AND ITYPE VECTORS.             **
!               **  DETERMINE THE NUMBER OF DISTINCT PARAMETERS.  **
!               **  FORM THE OUTPUT VECTOR IPARN1.                 **
!               **  CHECK TO SEE IF SOME OF THE PREVIOSULY-       **
!               **  DEFINED PARAMETERS ARE IN FACT VARIABLES.     **
!               ****************************************************
!
      IF(IPASS.EQ.1)GO TO 4050
      GO TO 4999
 4050 CONTINUE
!
      NUMPAR=0
      DO 4100 I=1,NW
      IF(ITYPE(I).EQ.'PAR ')GO TO 4190
      GO TO 4100
 4190 CONTINUE
!
      IF(NUMVAR.LE.0)GO TO 4290
      DO 4250 J=1,NUMVAR
      IF(IW21(I).EQ.IVARN1(J).AND.IW22(I).EQ.IVARN2(J))GO TO 4260
 4250 CONTINUE
      GO TO 4290
 4260 CONTINUE
      ITYPE(I)='VAR '
      GO TO 4100
 4290 CONTINUE
!
      IF(NUMPAR.EQ.0)GO TO 4300
      DO 4400 J=1,NUMPAR
      IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GO TO 4100
 4400 CONTINUE
 4300 CONTINUE
      NUMPAR=NUMPAR+1
      IPARN1(NUMPAR)=IW21(I)
      IPARN2(NUMPAR)=IW22(I)
 4100 CONTINUE
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 4599
      ISTEPN='6'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,4591)
 4591 FORMAT('AT END OF STEP 6 FOR PASS 1 (RIGHT BEFORE ',   &
      'RETURNING TO MAIN ROUTINE FROM COMPID)--')
      CALL DPWRST('XXX','BUG ')
      DO 4592 I=1,NW
      ICMIN=IBEGIN(I)
      ICMINP=ICMIN+1
      ICMINQ=ICMIN+2
      WRITE(ICOUT,4593)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),   &
      IW21(I),IW22(I),W2(I)
 4593 FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',   &
      'IW21(I),IW22(I),W2(I) = ',I8,2X,3A4,A4,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 4592 CONTINUE
 4599 CONTINUE
!
      GO TO 9000
 4999 CONTINUE
!
!               ****************************************************************
!               **  STEP 7--
!               **  OPERATE ON THE W2(.) AND IW21(.) VECTORS.
!               **  THIS STEP IS NOT EXECUTED IF STEP 6 IS;
!               **  THIS STEP IS EXECUTED IF STEP 6 IS NOT.
!               **  FIRST MAKE SURE THAT THE NUMBER OF LEFT
!               **  AND RIGHT PARENTHESES ARE THE SAME.
!               **  (STEP 6 THEN SETS UP A LARGE DO LOOP
!               **  WHICH GOES THROUGH ALL OF THE VALUES OF THE X VECTOR
!               **  AND GENERATES CORRESPONDING VALUES OF THE Y VECTOR.)
!               **  FOR A GIVEN X VALUE, IT EVALUATES THE FUNCTION
!               **  BY FIRST SEEKING THE INNERMOST PARENTHESES
!               **  (BY SEARCHING FOR THE FIRST REMAINING RIGHT PARENTHESS).
!               **  AND THEN EVALUATING ALL SUCH PARENTHETICAL EXPRESSIONS--
!               **  WORKING FROM THE INNERMOST OUT.
!               **  AFTER EVALUATING A PARENTHESES PAIR,
!               **  THE ENTIRE PARENTHESES GROUP (PARENTHESES INCLUDED)
!               **  IS REPLACED BY THE SCALAR ANSWER.
!               **  THE IW2, W2, AND ITYPE VECTORS ARE SQUEEZED ACCORDINGLY
!               **  (IN THE SUBROUTINE EVAL).
!               **  SINCE THE VECTORS IW2, W2, AND ITYPE ARE ALTERED (SQUEEZED)
!               **  FOR EACH X VALUE, THEY MUST BE REDEFINED FROM THE SAVED
!               **  VALUES IN IW2, W2, AND ITYPE FOR EACH NEW X VALUE.
!               **  THE ABOVE SQUEEZING OPERATION IS REPEATED
!               **  FOR EACH PARENTHESES PAIR UNTIL ALL PARENTHESES
!               **  ARE GONE AND WE REMAIN ONLY WITH THE FINAL ANSWER.
!               **  FOR EACH VALUE X(.) OF THE INPUT X VECTOR,
!               **  OUTPUT THE CORRESPONDING VALUE Y(.) OF
!               **  THE DESIRED OUTPUT VECTOR.
!               **  FOR A GIVEN VALUE X(.), THE CORRESPONDING
!               **  COMPUTED Y(.) WILL BE THE EVALUATED VALUE OF
!               **  THE RIGHT-HAND SIDE OF THE SPECIFIED EQUATION Y = F(X).
!               ****************************************************************
!
      NLP=0
      NRP=0
      DO 5100 I=1,NW
      IF(ITYPE(I).EQ.'LP  ')NLP=NLP+1
      IF(ITYPE(I).EQ.'RP  ')NRP=NRP+1
 5100 CONTINUE
      IF(NLP.EQ.NRP)GO TO 5190
      WRITE(ICOUT,5155)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5156)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5157)NLP
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5158)NRP
 5155 FORMAT('***** ERROR IN COMPID--')
      CALL DPWRST('XXX','BUG ')
 5156 FORMAT('NUMBER OF LEFT PARENTHESES NOT EQUAL TO ',   &
      'NUMBER OF RIGHT PARENTHESES')
 5157 FORMAT('NUMBER OF LEFT  PARENTHESES = ',I8)
 5158 FORMAT('NUMBER OF RIGHT PARENTHESES = ',I8)
      IERROR='YES '
      GO TO 9000
 5190 CONTINUE
!
!CCCC DO8000II=1,N
      NW=NWHOLD
      DO 5200 I=1,NW
      ITYPE(I)=ITYPEH(I)
      IW21(I)=IW21HO(I)
      IW22(I)=IW22HO(I)
      W2(I)=W2HOLD(I)
!     THE FOLLOWING STATEMENT HAS BEEN COMMENTED OUT
!     IN GOING FROM COMPIL TO COMPID.
!CCCC IF(ITYPE(I).EQ.'X   ')W2(I)=X(II)
 5200 CONTINUE
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      GO TO 5249
      GO TO 5299
 5249 CONTINUE
      ISTEPN='7'
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      DO 5250 I=1,NW
      WRITE(ICOUT,5251)I,IW21HO(I),IW21(I),ITYPE(I)
 5251 FORMAT('I,IW21HO(I),IW21(I),ITYPE(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5250 CONTINUE
 5299 CONTINUE
!
!               *********************************
!               **  STEP 7--                   **
!               **  DETERMINE THE DERIVATIVE.  **
!               *********************************
!
      CALL DERIV0(IW21,IW22,ITYPE,NW,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICON,ICON1,ICON2,NCON,ID1,ID2,NUMCD2,   &
      IBUGEV,ISUBRO,IFOUND,IERROR)
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 5319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5311)
 5311 FORMAT('***** IN COMPID, AFTER RETURNING FROM DERIV0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5312)NUMCD2
 5312 FORMAT('      NUMCD2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 5315 I=1,NUMCD2
      WRITE(ICOUT,5316)I,ID1(I),ID2(I)
 5316 FORMAT('      I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5315 CONTINUE
 5319 CONTINUE
!
!               ***********************************************************
!               **  STEP 7.2--                                           **
!               **  FORM THE OUTPUT VECTOR ID(.).                        **
!               **  NOTE THAT ID1(.) AND ID2(.) ARE PARALLEL             **
!               **  REPRESENTATIONS OF THE DESIRED DERIVATIVE FUNCTION   **
!               **  (ID1(.) HAS THE FIRST 4 CHARACTERS;                  **
!               **  ID2(.) HAS THE NEXT 4 CHARACTERS).                   **
!               **  MOST COMPONENTS (E.G., +, -, *, /, (, ), ETC.)       **
!               **  USE ONLY 1 CHARACTER OUT OF THE 8.                   **
!               **  SOME COMPONENTS (NAMELY, **)                         **
!               **  USE 2 CHARACTERS OUT OF THE 8.                       **
!               **  SOME COMPONTENTS (NAMELY, LIBRARY FUNCTIONS)         **
!               **  USE MANY (3 TO 7) CHARACTERS OUT OF THE 8.           **
!               **  IN ANY EVENT, THE OUTPUT VECTOR ID(.) WILL BE        **
!               **  AN UNPACKED (1 CHARACTER PER WORD) SYNTHESIS         **
!               **  OF THE 2 PACKED \VYYEYC\TYORS Y\I\D1(.) AND ID2(.).  **
!               ***********************************************************
!
      ISTEPN='7.2'
      IF(IBUGCO.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      IF(NUMCD2.LE.0)GO TO 5639
      DO 5600 I=1,NUMCD2
      IF(ID1(I).EQ.'    ')GO TO 5619
      J=J+1
      ID3(J)=ID1(I)
 5619 CONTINUE
      IF(ID2(I).EQ.'    ')GO TO 5629
      J=J+1
      ID3(J)=ID2(I)
 5629 CONTINUE
 5600 CONTINUE
 5639 CONTINUE
      NUMCH3=J
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 5649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5641)NUMCD2,NUMCH3
 5641 FORMAT('NUMCD2,NUMCH3 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 5645 I=1,NUMCH3
      WRITE(ICOUT,5646)I,ID3(I)
 5646 FORMAT('I,ID3(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5645 CONTINUE
 5649 CONTINUE
!
      K=0
      DO 5700 I=1,NUMCH3
      IF(ID3(I).EQ.'    ')GO TO 5700
      CALL DPXH1H(ID3(I),ICH,ILASTC,IBUGEV)
      IF(ILASTC.LE.0)GO TO 5700
      DO 5750 J=1,ILASTC
      K=K+1
      ID(K)=ICH(J)
 5750 CONTINUE
 5700 CONTINUE
      NCTOTD=K
!
      IF(NCTOTD.GE.1)GO TO 5789
      WRITE(ICOUT,5705)NCTOTD
 5705 FORMAT('***** ERROR IN COMPID--TOTAL NUMBER OF CHARACTERS ',   &
      'IN DERIVATIVE. (INCL. BLANKS, AND EQUAL SIGN) ',   &
      'IS < 2.  NCTOTD = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5771)NUMCHD,N,IPASS
 5771 FORMAT('NUMCHD,N,IPASS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5772)(ID(I),I=1,NUMCHD)
 5772 FORMAT('ID--',80A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 5789 CONTINUE
!
      IF(IBUGCO.EQ.'OFF'.AND.ISUBRO.NE.'MPID')GO TO 5799
      WRITE(ICOUT,5791)NCTOTD
 5791 FORMAT('NCTOTD = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 5792 I=1,NCTOTD
      WRITE(ICOUT,5793)I,ID(I)
 5793 FORMAT('I,ID(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5792 CONTINUE
 5799 CONTINUE
      NUMCHD=NCTOTD
!
!               *******************************************
!               **  STEP 7.3--                           **
!               **  SIMPLIFY THE FUNCTIONAL EXPRESSION.  **
!               *******************************************
!
      CALL DPSIPA(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSISI(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA2(ID,NUMCHD,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(ID,NUMCHD,IBUGEV,IERROR)
!
      CALL DPSIPA(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSISI(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIP0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE1(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIE0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA0(ID,NUMCHD,IBUGEV,IERROR)
      CALL DPSIA2(ID,NUMCHD,IBUGEV,ISUBRO,IERROR)
      CALL DPSIFL(ID,NUMCHD,IBUGEV,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGCO.EQ.'ON' .OR. ISUBRO.EQ.'MPID')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COMPID--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NUMCHA,NUMCHD
 9012   FORMAT('IERROR,NUMCHA,NUMCHD = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)
 9013   FORMAT('INPUT FUNCTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)(IA(J),J=1,NUMCHA)
 9016   FORMAT(130A1)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,9022)
 9022   FORMAT('OUTPUT DERIVATIVE--')
        CALL DPWRST('XXX','BUG ')
        DO 9025 I=1,NUMCHD,12
          JMIN=I
          JMAX=JMIN+11
          IF(JMAX.GT.NUMCHD)JMAX=NUMCHD
          WRITE(ICOUT,9026)(ID(J),J=JMIN,JMAX)
 9026     FORMAT(12A4)
          CALL DPWRST('XXX','BUG ')
 9025   CONTINUE
!
      ENDIF
!
      RETURN
      END SUBROUTINE COMPID
      SUBROUTINE COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN1,IPARN2,NUMPAR,   &
                        IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y,   &
                        IBUGCO,IBUGEV,IERROR)
!
!     PURPOSE--THIS SUBROUTINE INTERPRETS AND EVALUATES A FORTRAN
!              MATHEMATICAL FUNCTION EXPRESSION OF THE FORM
!               Y=F(.,.,.,.,...).  IT IS A GENERALIZATION OF JJF7.COMPIL
!              WHICH COULD HANDLE ONLY 1 ARGUMENT (X).  THIS SUBROUTINE
!              IS TYPICALLY ENTERED WITH TWO PASSES--
!              THE FIRST PASS ANALYZES THE STRING AND HAS AS ITS OUTPUT
!              THE HOLLERITH NAMES OF THE VARIOUS PARAMETERS. A
!              'PARAMETER' IN THIS SUBROUTINE (COMPIM) MEANS ANY USUAL
!              PARAMETER IN AN EXPRESSION AS WELL AS ANY VARIABLE NAME
!              (E.G., X1, X2, X3, TEMP, RES, ETC.) THIS IS A FUNDAMENTAL
!              WAY THAT COMPIM DIFFERS FROM COMPIL.  ALSO, COMPIM OUTPUTS
!              ONLY A COMPUTED SCALAR VALUE (AS OPPOSED TO COMPIL WHICH
!              OUTPUTS AN ENTIRE COMPUTED VECTOR).  THESE NAMES ARE
!              OUTPUTTED IN THIS FIRST PASS AS ELEMENTS IN THE VECTORS
!              IPARN1 AND IPARN2.  THE SECOND PASS USES INPUT PARAMETER
!              VALUES (INPUTTED IN THE VECTOR PARAM) TO ACTUALLY EVALUATE
!              THE FUNCTION (OUTPUTTED IN THE SCALAR Y).  NOTE THAT IF
!              SOME OF THE 'PARAMETERS' ARE IN FACT ELEMENTS OF A VECTOR
!              VARIABLE, THE ITERATING THROUGH THE ENTIRE VECTOR IS DONE
!              IN THE CALLING SUBROUTINE AND NOT WITHIN COMPIM
!              (THIS IS ANOTHER WAY THAT COMPIM DIFFERS FROM COMPIL).
!     INPUT  ARGUMENTS--IA     = THE INTEGER VECTOR WHICH CONTAINS
!                                THE HOLLERITH CHARACTERS WHICH
!                                MAKE UP THE LINE OF FORTRAN CODE.
!                                THIS VECTOR CONTAINS THE STRING
!                                TO BE OPERATED ON, INTERPRETED,
!                                AND EVALUATED.
!                     --NUMCHA = THE INTEGER VALUE WHICH
!                                DEFINES THE NUMBER OF CHARACTERS IN IA.
!                                NUMCHA DEFINES THE LENGTH OF THE
!                                HOLLERITH STRING TO BE OPERATED ON,
!                                INTERPRETED, AND EVALUATED.
!                     --IPASS  = AN INTEGER FLAG CODE
!                                WHICH DEFINES WHICH PASS (1 OR 2) INTO THIS
!                                SUBROUTINE THE USER IS IN.
!                                PASS 1 DETERMINE PARAMETER NAMES;
!                                PASS 2 DOES FUNCTION EVALUATIONS.
!                     --PARAM  = THE SINGLE PRECISION VECTOR OF PARAMETER
!                                (AND VARIABLE)
!                                VALUES CORRESPONDING TO THE PARAMETER NAMES
!                                AS GIVEN IN THE VECTOR IPARN.
!                     --IPARN1 = THE INTEGER VECTOR
!                                CONTAINING CHARACTERS 1 THROUGH 4
!                                OF PARAMETER (AND VARIABLE)
!                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
!                     --IPARN2 = THE INTEGER VECTOR
!                                CONTAINING CHARACTERS 5 THROUGH 8
!                                OF PARAMETER (AND VARIABLE)
!                                NAMES AS TYPICALLY DETERMINED BY PASS 1.
!      OUTPUT ARGUMENTS--Y     = THE SINGLE PRECISION COMPUTED SCALAR VALUE OF
!                                THE FUNCTION AS DETERMINED BY PASS 2
!                                AND WHICH CONSTITUTE THE ULTIMATE
!                                OUTPUT FROM THIS SUBROUTINE.
!                                THAT IS, SYMBOLICALLY,
!                                Y = F(X1,X2,X3,TEMP,RES,ETC.,PAR1,PAR2,PAR3,ETC
!     OUTPUT--THE SINGLE PRECISION COMPUTED SCALAR VALUE,
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER           SUBROUTINES NEEDED--EVAL
!     FORTRAN LIBRARY SUBROUTINES NEEDED--(ALL IN EVAL)
!                                         SQRT
!                                         EXP
!                                         LOG
!                                         LOG10
!                                         SIN
!                                         COS
!                                         ATAN
!                                         ATAN2
!                                         TANH
!                                         ABS
!                                         AINT
!                                         ARCSIN
!                                         ARCCOS
!                                         ARCTAN
!                                         OCTAL
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     NOTE--THIS SUBROUTINE ALLOWS ONE TO PERFORM
!           INTERACTIVE FUNCTION EVALUATIONS.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1976.
!     UPDATED         --FEBRUARY  1977.
!     UPDATED         --DECEMBER  1977.
!     UPDATED         --JANUARY   1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --DECEMBER  1978.
!     UPDATED         --JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JULY      1979.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --JUNE      1981.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1986.
!     UPDATED         --DECEMBER  1988.  BLANK OUT IR(.) FOR AT LEAST 10 CHAR
!     UPDATED         --SEPTEMBER 1994.  ADD SAVE4 ARGUMENT TO EVALM.
!     UPDATED         --APRIL     1995.  BUG:
!                                        LET A = TPDF(X,2) - TPDF(X,3)
!                                        SETS SAVE1 TO 2 IN BOTH CASES
!     UPDATED         --MAY       1998.  ADD FIFTH PARAMETER
!     UPDATED         --JUNE      2003.  ADD SAVE6, SAVE7, SAVE8
!                                        ARGUMENTS TO EVALM.
!     UPDATED         --FEBRUARY  2005.  CONVERT STRING TO UPPER CASE
!     UPDATED         --DECEMBER  2010.  INITIALIZATION OF SAVE1 ...
!                                        SAVE8
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
!
!     NOTE--THE DIMENSIONS OF ITYPEH, IW21HO, IW22HO, AND W2HOLD
!           WHICH ARE DEFINED IN THE MAIN PROGRAM
!           SHOULD BE AT LEAST AS LARGE AS THE DIMENSIONS
!           OF IW2 AND IW22 BELOW.
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
!     NOTE--THE DIMENSION OF IB SHOULD BE THE SAME AS
!           THE DIMENSION OF SUBROUTINE IA IN DPLET.
!
      PARAMETER (MAXCHA=1000)
!
      DIMENSION IB(MAXCHA)
      DIMENSION IR(MAXCHA)
      DIMENSION IBEGIN(MAXCHA)
      DIMENSION IEND(MAXCHA)
      DIMENSION ITYPE(MAXCHA)
      DIMENSION IW21(MAXCHA)
      DIMENSION IW22(MAXCHA)
      DIMENSION W2(MAXCHA)
!
      DIMENSION ICH(10)
      DIMENSION IL(10)
!
!CCCC ADD FOLLOWING SECTION APRIL 1995.
!
      PARAMETER(MAXNST=25)
      DIMENSION SAVE1(MAXNST)
      DIMENSION SAVE2(MAXNST)
      DIMENSION SAVE3(MAXNST)
      DIMENSION SAVE4(MAXNST)
      DIMENSION SAVE5(MAXNST)
      DIMENSION SAVE6(MAXNST)
      DIMENSION SAVE7(MAXNST)
      DIMENSION SAVE8(MAXNST)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE THE UPPER LIMIT OF THE NUMBER OF CHARACTERS THAT MAY BE
!     PROCESSED BY THIS SUBROUTINE (COUNTING BLANKS, LEFT-HAND SIDE,
!     EQUAL SIGN, AND RIGHT HAND SIDE).  IF RESTRICT THE EXPRESSION TO 1
!     LINE IMAGE, THEN A REASONABLE UPPER BOUND IS 80.  WHATEVER UPPER
!     BOUND IS SET, THE DIMENSIONS OF MOST OF THE VECTORS MUST BE EQUAL
!     OR LARGER TO THIS NUMBER.  (THE VECTOR IL(.) WHICH CONTAINS THE
!     NUMBER OF CHARACTERS TO THE LEFT OF THE EQUAL SIGN (BLANKS IGNORED)
!     MAY BE MUCH SMALLER--LIKE 6.)
!     NOTE--AS OF JANUARY 1979, THE BOUND WAS RESET TO 150.
!
!CCCC DATA MAXCHA/150/
!CCCC DATA MAXCHA/225/
!CCCC DATA MAXCHA/1000/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COMP'
      ISUBN2='IM  '
!
      IERROR='NO'
!
!     THE FOLLOWING STATEMENT (N=1) HAS BEEN ADDED IN CONVERTING
!     THE COMPIL SUBROUTINE TO THE COMPIM SUBROUTINE.
!
      N=1
!
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
          DO 62 I=1,NUMPAR
            WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I),PARAM(I)
   63       FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2(2X,A4),2X,   &
                   F15.7)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)NWHOLD
   71   FORMAT('NWHOLD = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NWHOLD.GE.1)THEN
          DO 72 I=1,NWHOLD
            WRITE(ICOUT,73)I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I)
   73       FORMAT('I,ITYPEH(I),IW21HO(I),IW22HO(I),W2HOLD(I) = ',   &
                   I8,3(2X,A4),2X,F15.7)
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
!
!               **********************************************************
!               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.**
!               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND       **
!               **  REGARDLESS OF THE WORD SIZE.                        **
!               **********************************************************
!
      NUMASC=4
      NUMAS2=2*NUMASC
      NUMAS3=3*NUMASC
      NUMAS4=4*NUMASC
!
!     IF IPASS = 2, SKIP ALL OF THE PRELIMINARY CODE
!     AND JUMP TO CALCULATIVE PART OF CODE.
!
      IF(IPASS.EQ.2)GO TO 5000
!
!     CHECK THAT THE INPUT NUMBER OF CHARACTERS NUMCHA
!     (INCLUDING LEFT SIDE, RIGHT SIDE, EQUAL SIGN,
!     AND BLANKS) IS AT LEAST 1 AND AT MOST MAXCHA
!     (WHERE MAXCHA IS THE INTERNALLY DEFINED VARIABLE
!     WHICH CONTROLS DIMENSION SIZES AND WHICH
!     TYPICALLY HAS THE VALUE 80).
!
      IF(NUMCHA.LT.1 .OR. NUMCHA.GT.MAXCHA)THEN
        WRITE(ICOUT,21)
   21   FORMAT('***** ERROR IN COMPIM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
   22   FORMAT('      THE NUMBER OF CHARACTERS NUMCHA WHICH DEFINES ',   &
               'THE LENGTH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,24)
   24   FORMAT('      OF THE INPUT EXPRESSION (INCLUDING LEFT-HAND ',   &
               'SIDE,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,25)
   25   FORMAT('      RIGHT-HAND SIDE, EQUAL SIGN, AND ALL BLANKS) IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)
   26   FORMAT('      LESS THAN 1 OR LARGER THAN MAXCHA (MAXCHA IS AN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,28)MAXCHA
   28   FORMAT('      INTERNALLY DEFINED VARIABLE WHICH HAS THE ',   &
               'VALUE = ',I8,'   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,30)NUMCHA
   30   FORMAT('      THE NUMBER OF CHARACTERS IN THE INPUT ',   &
               'EXPRESSION IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,31)(IA(I),I=1,MIN(100,NUMCHA))
   31     FORMAT('      INPUT EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!CCCC FEBRUARY 2005.  CONVERT INPUT FUNCTION TO ALL UPPER CASE.
!CCCC                 THIS IS TO ADDRESS ISSUE WHERE IF FUNCTION
!CCCC                 WAS DEFINED AS "LET STRING" RATHER THAN
!CCCC                 "LET FUNCTION", CASE IS PRESERVED.  HOWEVER,
!CCCC                 WHEN EVALUATING FUNCTION, WE NEED THE STRING
!CCCC                 TO BE EVALUATED IN UPPER CASE.
!
      DO 91 I=1,NUMCHA
        ITEMP=ICHAR(IA(I)(1:1))
        IF(ITEMP.GE.97 .AND. ITEMP.LE.122)THEN
          ITEMP=ITEMP-32
          IA(I)(1:1)=CHAR(ITEMP)
        ENDIF
   91 CONTINUE
!
!     BLANK-OUT AND ZERO-OUT SOME VARIABLES AND VECTORS.
!
      Y=0.0
      DO 160 I=1,NUMCHA
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
!
!     THE FOLLOWING LOOP WAS PUT IN TO AVOID A PROBLEM
!     ESSENTAILLY CAUSED IN DPLIB1 AND WHICH
!     SHOWED UP IN  LET A = 1 1 3   LET A = ABS(A)   LET B = A
!     MARY BETH    12/88
!
      DO 161 I=1,10
        IR(I)='    '
  161 CONTINUE
!
!               ************************************
!               **  STEP 1--                      **
!               **  OPERATE ON THE VECTOR IA(.).  **
!               **  SQUEEZE OUT ALL BLANKS.       **
!               **  OUTPUT THE VECTOR IB(.).      **
!               ************************************
!
      K=0
      DO 100 I=1,NUMCHA
        IF(IA(I).EQ.' ')GO TO 100
        CALL DPXH1H(IA(I),ICH,ILASTC,IBUGCO)
        IF(ILASTC.LE.0)GO TO 100
        DO 150 J=1,ILASTC
          K=K+1
          IB(K)=ICH(J)
  150   CONTINUE
  100 CONTINUE
      NCTOT=K
      IF(NCTOT.LT.1)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NCTOT
  105   FORMAT('      TOTAL NUMBER OF CHARACTERS IN MODEL (INCLUDING ',   &
               'BOTH SIDES, BLANKS, AND EQUAL SIGN) IS < 1.  NCTOT = ',   &
               I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,171)NUMCHA,N,IPASS
  171   FORMAT('NUMCHA,N,IPASS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,172)(IA(I),I=1,MIN(80,NUMCHA))
  172   FORMAT('IA--',80A1)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)NUMPAR
  181   FORMAT('NUMPAR = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMPAR.GT.0)THEN
          DO 182 I=1,NUMPAR
            WRITE(ICOUT,183)I,IPARN1(I),IPARN2(I),PARAM(I)
  183       FORMAT('I,IPARN1(I),IPARN2(I),PARAM(I) = ',I8,2(2X,A4),2X,   &
                   F15.7)
            CALL DPWRST('XXX','BUG ')
  182     CONTINUE
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,191)NCTOT
  191   FORMAT('NCTOT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 192 I=1,NCTOT
          WRITE(ICOUT,193)I,IB(I)
  193     FORMAT('I,IB(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  192   CONTINUE
      ENDIF
!
!               *********************************************************
!               **  STEP 2--                                           **
!               **  OPERATE ON THE VECTOR IB(.).                       **
!               **  DETERMINE THE NUMBER OF CHARACTERS (IF ANY)        **
!               **  FOR THE LEFT-HAND SIDE.  OUTPUT THEM INTO THE      **
!               **  VECTOR IL(.).                                      **
!               *********************************************************
!
      DO 500 I=1,NCTOT
        I2=I
        IF(IB(I).EQ.'=')THEN
          NCL=I2-1
          ISTARR=I2+1
          GO TO 559
        ENDIF
  500 CONTINUE
      NCL=0
      ISTARR=1
  559 CONTINUE
!
      IF(NCL.GT.0)THEN
        DO 600 I=1,NCL
          IL(I)=IB(I)
  600   CONTINUE
      ENDIF
!
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='2'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,691)NCL
  691   FORMAT('NCL = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 692 I=1,NCL
          WRITE(ICOUT,693)I,IL(I)
  693     FORMAT('I,IL(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  692   CONTINUE
      ENDIF
!
!               *********************************************************
!               **  STEP 3--                                           **
!               **  OPERATE ON THE VECTOR IB(.).  DETERMINE THE        **
!               **  NUMBER OF CHARACTERS FOR RIGHT-HAND SIDE.          **
!               **  OUTPUT THEM INTO THE VECTOR IR(.).                 **
!               *********************************************************
!
      IF(ISTARR.GT.NCTOT)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,702)
  702   FORMAT('      THE NUMBER OF CHARACTERS ON THE RIGHT (WITH ',   &
               'BLANKS IGNORED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,703)
  703   FORMAT('      IS 0.  THE TOTAL NUMBER OF PACKED CHARACTERS ',   &
               'LEFT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,704)NCTOT
  704   FORMAT('      (IF ANY), EQUAL SIGN (IF ANY), AND RIGHT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,707)ISTARR
  707   FORMAT('      THE START POSITION FOR THE PACKED RIGHT IS ',   &
               'COLUMN ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,709)NUMCHA
  709   FORMAT('      THE INPUT NUMBER OF CHARACTERS NUMCHA = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,31)(IA(I),I=1,MIN(100,NUMCHA))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      K=0
      DO 700 I=ISTARR,NCTOT
        K=K+1
        IR(K)=IB(I)
  700 CONTINUE
      NCR=K
!
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='3'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,791)NCR
  791   FORMAT('NCR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 792 I=1,NCR
          WRITE(ICOUT,793)I,IR(I)
  793     FORMAT('I,IR(I) = ',I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
  792   CONTINUE
      ENDIF
!
!               ********************************************************
!               **  STEP 4--                                          **
!               **  OPERATE ON THE VECTOR IR(.).  ANALYZE THE         **
!               **  RIGHT-HAND SIDE.  DETERMINE THE NUMBER OF         **
!               **  DIFFERENT LOGICAL COMPONENTS.                     **
!               **     1. NUMBER (CONSISTING OF 0,1,2,...,9 OR .)     **
!               **     2. X VARIABLE                                  **
!               **     3. OPERATION (+   -   *   /   **)              **
!               **     4. PARENTHESES (   (   OR   )    )             **
!               **     5. LIBRARY FUNCTION (ALOG   EXP   ETC +        **
!               **        AUGMENTED LIB.                              **
!               **     6. COMMA (FOR MULTI-ARGUMENT LIBRARY FUNCTIONS)**
!               **     7. PARAMETER (ANYTHING NOT ABOVE)              **
!               **  CHECK FOR SYNTAX ERRORS.                          **
!               **  OUTPUT THE TYPE COMPONENT INTO ITYPE(.).          **
!               **  OUTPUT THE START LOCATION IN IR(.) OF EACH        **
!               **  OUTPUT THE STOP  LOCATION IN IR(.) OF EACH        **
!               **  COMPONENT INTO IE                                 **
!               ********************************************************
!
      NW=0
      I=1
 1050 CONTINUE
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
!
      IF(IR(I).EQ.'0' .OR. IR(I).EQ.'1' .OR. IR(I).EQ.'2' .OR.   &
         IR(I).EQ.'3' .OR. IR(I).EQ.'4' .OR. IR(I).EQ.'5' .OR.   &
         IR(I).EQ.'6' .OR. IR(I).EQ.'7' .OR. IR(I).EQ.'8' .OR.   &
         IR(I).EQ.'9' .OR. IR(I).EQ.'.')THEN
        NW=NW+1
        ITYPE(NW)='N'
        JMIN=I
        J=I
 1150   CONTINUE
        J=J+1
        IF(J.LE.NCR)THEN
          IF(IR(J).EQ.'0' .OR. IR(J).EQ.'1' .OR. IR(J).EQ.'2' .OR.   &
             IR(J).EQ.'3' .OR. IR(J).EQ.'4' .OR. IR(J).EQ.'5' .OR.   &
             IR(J).EQ.'6' .OR. IR(J).EQ.'7' .OR. IR(J).EQ.'8' .OR.   &
             IR(J).EQ.'9' .OR. IR(J).EQ.'.')THEN
               GO TO 1150
          ENDIF
        ENDIF
        JMAX=J-1
        GO TO 1800
      ELSEIF(IR(I).EQ.'+' .OR. IR(I).EQ.'-' .OR. IR(I).EQ.'*' .OR.   &
         IR(I).EQ.'/')THEN
        NW=NW+1
        ITYPE(NW)='OP'
        JMIN=I
        JMAX=I
        IP1=I+1
        IF(IR(I).EQ.'*'.AND.IR(IP1).EQ.'*')JMAX=IP1
        GO TO 1800
      ELSEIF(IR(I).EQ.'(')THEN
        NW=NW+1
        ITYPE(NW)='LP'
        JMIN=I
        JMAX=I
        GO TO 1800
      ELSEIF(IR(I).EQ.')')THEN
        NW=NW+1
        ITYPE(NW)='RP'
        JMIN=I
        JMAX=I
        GO TO 1800
      ELSEIF(IR(I).EQ.',')THEN
        NW=NW+1
        ITYPE(NW)='COM'
        JMIN=I
        JMAX=I
        GO TO 1800
      ENDIF
!
!     CHECK FOR A LIBRARY FUNCTION.
!
      CALL CKLIB1(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'NO')CALL CKLIB2(IR,NCR,I,IFOUND,NCLF,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IFOUND.EQ.'YES')THEN
        IF(NCLF.GE.1 .AND. NCLF.LE.8)THEN
          NW=NW+1
          ITYPE(NW)='LF'
          JMIN=I
          JMAX=I+NCLF-1
          GO TO 1800
        ENDIF
      ENDIF
!
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
        IF(IR(J).EQ.'+' .OR. IR(J).EQ.'-' .OR.   &
           IR(J).EQ.'*' .OR. IR(J).EQ.'/' .OR.   &
           IR(J).EQ.',')GO TO 1660
        IF(IR(J).EQ.'(')NLPWP=NLPWP+1
        IF(IR(J).EQ.')')NRPWP=NRPWP+1
        IF(IR(J).EQ.')'.AND.NRPWP.GT.NLPWP)GO TO 1660
        ILOOP=ILOOP+1
        IF(ILOOP.LE.NUMAS2)GO TO 1650
!
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1656)NUMAS2
 1656   FORMAT('      PARAMETER NAME EXCEEDS ',I8,' CHARACTERS')
        CALL DPWRST('XXX','BUG ')
        DO 1657 K=JMIN,J
          WRITE(ICOUT,1658)K,IR(K)
 1658     FORMAT('K, IR(K) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 1657   CONTINUE
        IERROR='YES'
        GO TO 9000
      ENDIF
!
 1660 CONTINUE
      JMAX=J-1
!
 1800 CONTINUE
!
!     CHECK THAT NW HAS NOT EXCEEDED MAXCHA (USUALLY 80)
!
      IF(NW.GT.MAXCHA)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1902)
 1902   FORMAT('      THE VARIABLE NW HAS JUST EXCEEDED THE MAXIMUM ',   &
               'ALLOWABLE')
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
 1907   FORMAT('      THE NUMBER OF (PACKED) CHARACTERS ON ',   &
               'RIGHT-HAND SIDE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCR.GE.1)THEN
          WRITE(ICOUT,1908)(IR(I),I=1,MIN(95,NCR))
 1908     FORMAT('      (PACKED) RIGHT-HAND SIDE--',95A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IBEGIN(NW)=JMIN
      IEND(NW)=JMAX
      I=JMAX
!
      I=I+1
      IF(I.LE.NCR)GO TO 1050
!
!     TEST THAT NW IS POSITIVE.
!
      IF(NW.LT.1)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1951)NW
 1951   FORMAT('      NW IS NON-POSITIVE.  NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NW.EQ.1)THEN
        DO 1960 I=1,NW
          IP1=I+1
          IF(ITYPE(I).EQ.'LF'.AND.ITYPE(IP1).NE.'LP')THEN
            WRITE(ICOUT,21)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1962)
 1962       FORMAT('      LIBRARY FUNCTION NOT FOLLOWED BY A LEFT ',   &
                   'PARENTHESES')
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
            GO TO 9000
          ENDIF
 1960   CONTINUE
      ENDIF
!
      IF(ITYPE(NW).EQ.'OP')THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1971)ITYPE(NW)
 1971   FORMAT('      LAST TERM IN TOTAL EXPRESSION IS AN OPERATION = ',   &
               A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(ITYPE(NW).EQ.'LF')THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1973)ITYPE(NW)
 1973   FORMAT('      LAST TERM IN TOTAL EXPRESSION = A LIBRARY ',   &
               'FUNCTION = ',A4)
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
        GO TO 9000
      ENDIF
!
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='4'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1991)NW
 1991   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 1992 I=1,NW
          ICMIN=IBEGIN(I)
          ICMINP=ICMIN+1
          ICMINQ=ICMIN+2
          WRITE(ICOUT,1993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),   &
                           IBEGIN(I),IEND(I)
 1993     FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),',   &
                 'IBEGIN(I),IEND(I) = ',I8,2X,4A4,2(2X,I8))
          CALL DPWRST('XXX','BUG ')
 1992   CONTINUE
      ENDIF
!
!               ********************************************************
!               **  STEP 5--                                          **
!               **  OPERATE ON EACH COMPONENT OF THE VECTOR IR(.).    **
!               **  CONVERT THE NUMBERS TO FLOATING POINT VALUES.     **
!               **  CONVERT THE PARAMATERS TO FLOATING POINT VALUES.  **
!               **  SET THE X TO AN DUMMY VALUE OF 0.0 FOR THE TIME BEING.
!               **  CONVERT THE OPERATIONS INTO A 1-WORD REPRESENTATION.
!               **  'CONVERT' THE PARENTHESES INTO A 1-WORD REPRESENTATION.
!               **  CONVERT THE COEFFICIENTS TO COEFFICIENT VALUES.   **
!               **  CONVERT THE LIBRARY FUNCTIONS INTO A 1-WORD REPRESENTATION.
!               **  SAVE THE CONTENTS OF ITYPE, IW21, IW22, AND W2 IN **
!               **  ITYPEH, IW21HO, IW22HO, AND WHOLD FOR LATER USE   **
!               **  IN REDEFINING ITYPE, IW21, IW22, AND W2 FOR EACH NEW X VALUE
!               **  OUTPUT THE VECTORS IW21, IW22 AND W2.             **
!               **  OUTPUT THE VECTORS IW21HO, IW22HO, W2HOLD, AND ITYPEH.
!               ********************************************************
!
      DO 3000 I=1,NW
        ICMIN=IBEGIN(I)
        ICMAX=IEND(I)
        IF(ITYPE(I).EQ.'N')THEN
          W2(I)=0.0
          IANS1='    '
          IANS2='    '
          IANS3='    '
          IANS4='    '
          J=0
          DO 3150 IC=ICMIN,ICMAX
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
          CALL ERRORF(IANS1,IANS2,IANS3,IANS4,ERRMIN,ERRMAX,   &
                      ERRMAX,ANS2,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          W2(I)=ANS2
          GO TO 3000
        ELSEIF(ITYPE(I).EQ.'X')THEN
          W2(I)=0.0
          GO TO 3000
        ELSEIF(ITYPE(I).EQ.'OP')THEN
          IW21(I)=IR(ICMIN)
          ICMINP=ICMIN+1
          IF(IR(ICMIN).EQ.'*'.AND.IR(ICMINP).EQ.'*')IW21(I)='**'
          GO TO 3000
        ELSEIF(ITYPE(I).EQ.'LP'.OR.ITYPE(I).EQ.'RP')THEN
          IW21(I)=IR(ICMIN)
          GO TO 3000
        ELSEIF(ITYPE(I).EQ.'PAR')THEN
          IW21(I)='    '
          IW22(I)='    '
          ICMAX2=ICMIN+NUMAS2-1
          IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
          J=0
          DO 3550 IC=ICMIN,ICMAX2
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
!
          IF(IPASS.EQ.1)GO TO 3000
!
          DO 3570 J=1,NUMPAR
            IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))THEN
              W2(I)=PARAM(J)
              GO TO 3000
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
 3574     FORMAT('               ADMISSIBLE PARAMETER/VARIABLE ',   &
                 'NAMES = ')
          CALL DPWRST('XXX','BUG ')
          DO 3575 J=1,NUMPAR
            WRITE(ICOUT,3576)J,IPARN1(J),IPARN2(J)
 3576       FORMAT('               PARAMETER/VARIABLE NAME ',I4,'--',   &
                   2A4)
            CALL DPWRST('XXX','BUG ')
 3575     CONTINUE
          WRITE(ICOUT,3577)(IA(J),J=1,MIN(100,NUMCHA))
 3577     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ITYPE(I).EQ.'LF')THEN
          IW21(I)='    '
          IW22(I)='    '
          ICMAX2=ICMIN+NUMAS2-1
          IF(ICMAX.LE.ICMAX2)ICMAX2=ICMAX
          J=0
          DO 3650 IC=ICMIN,ICMAX2
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
          GO TO 3000
        ELSEIF(ITYPE(I).EQ.'COM')THEN
          IW21(I)=IR(ICMIN)
          GO TO 3000
        ENDIF
!
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3005)
 3005   FORMAT('      ITYPE(I) NOT X, OP, LP, PAR, OR LF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3006)I,ITYPE(I),IW21(I),W2(I)
 3006   FORMAT('I,ITYPE(I),IW21(I),W2(I) = ',I8,2(2X,A4),2X,F15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
!
 3000 CONTINUE
!
      NWHOLD=NW
      DO 3900 I=1,NW
        ITYPEH(I)=ITYPE(I)
        IW21HO(I)=IW21(I)
        IW22HO(I)=IW22(I)
        W2HOLD(I)=W2(I)
 3900 CONTINUE
!
      IF(IBUGCO.EQ.'ON')THEN
        ISTEPN='5'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        DO 3992 I=1,NW
          ICMIN=IBEGIN(I)
          ICMINP=ICMIN+1
          ICMINQ=ICMIN+2
          WRITE(ICOUT,3993)I,IR(ICMIN),IR(ICMINP),IR(ICMINQ),ITYPE(I),   &
                           IW21(I),IW22(I),W2(I)
 3993     FORMAT('I,IR(ICMIN),IR(ICMIN+1),IR(ICMIN+2),ITYPE(I),W21(I),',   &
                 'IW22HO(I),W2(I) = ',I8,2X,3A4,3(2X,A4),2X,F15.6)
          CALL DPWRST('XXX','BUG ')
 3992   CONTINUE
      ENDIF
!
!               ****************************************************
!               **  STEP 6--                                      **
!               **  THIS STEP IS TO BE EXECUTED IF IPASS=1;       **
!               **  OTHERWISE IT IS SKIPPED.                      **
!               **  IF THIS STEP IS EXECUTED, STEP 7 IS NOT;      **
!               **  IF THIS STEP IS NOT EXECUTED, STEP 7 IS.      **
!               **  OPERATE ON IW21, IW22, AND ITYPE VECTORS.      **
!               **  DETERMINE THE NUMBER OF DISTINCT PARAMETERS.  **
!               **  FORM THE OUTPUT VECTOR IPARN.                 **
!               ****************************************************
!
      IF(IPASS.EQ.1)THEN
!
        NUMPAR=0
        DO 4100 I=1,NW
          IF(ITYPE(I).NE.'PAR')GO TO 4100
          IF(NUMPAR.GT.0)THEN
            DO 4400 J=1,NUMPAR
              IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))GO TO 4100
 4400       CONTINUE
          ENDIF
          NUMPAR=NUMPAR+1
          IPARN1(NUMPAR)=IW21(I)
          IPARN2(NUMPAR)=IW22(I)
 4100   CONTINUE
        GO TO 9000
      ENDIF
!
!               *********************************************************
!               **  STEP 7-- C                                         **
!               **  OPERATE ON THE W2(.), IW21(.), AND IW22(.) VECTORS.**
!               **  THIS STEP IS NOT EXECUTED IF STEP 6 IS; THIS STEP  **
!               **  IS EXECUTED IF STEP 6 IS NOT.  FIRST MAKE SURE THAT**
!               **  THE NUMBER OF LEFT AND RIGHT PARENTHESES ARE THE   **
!               **  SAME.  (STEP 6 THEN SETS UP A LARGE DO LOOP WHICH  **
!               **  GOES THROUGH ALL OF THE VALUES OF THE X VECTOR AND **
!               **  GENERATES CORRESPONDING VALUES OF THE Y VECTOR.)   **
!               **  FOR A GIVEN X VALUE, IT EVALUATES THE FUNCTION BY  **
!               **  FIRST SEEKING THE INNERMOST PARENTHESES (BY        **
!               **  SEARCHING FOR THE FIRST REMAINING RIGHT PARENTHESS).*
!               **  AND THEN EVALUATING ALL SUCH PARENTHETICAL         **
!               **  EXPRESSIONS--WORKING FROM THE INNERMOST OUT. AFTER **
!               **  EVALUATING A PARENTHESES PAIR, THE ENTIRE          **
!               **  PARENTHESES GROUP (PARENTHESES INCLUDED) IS        **
!               **  REPLACED BY THE SCALAR ANSWER.  THE IW21, IW22,    **
!               **  W2, AND ITYPE VECTORS ARE SQUEEZED ACCORDING (IN   **
!               **  THE SUBROUTINE EVAL).  SINCE THE VECTORS IW21,     **
!               **  IW22, W2, AND ITYPE ARE ALTERED FOR EACH X VALUE,  **
!               **  THEY MUST BE REDEFINED FROM THE SAVED VALUES IN    **
!               **  IW21HO, IW22HO, W2HOLD, AND ITYPEH FOR EACH NEW X  **
!               **  THE ABOVE SQUEEZING OPERATION IS REPEATED FOR EACH **
!               **  PARENTHESES PAIR UNTIL ALL PARENTHESES ARE GONE    **
!               **  AND WE REMAIN ONLY WITH THE FINAL ANSWER.  FOR     **
!               **  EACH VALUE X(.) OF THE INPUT X VECTOR, OUTPUT THE  **
!               **  CORRESPONDING VALUE Y(.) OF THE DESIRED OUTPUT     **
!               **  VECTOR.  FOR A GIVEN VALUE X(.), THE CORRESPONDING **
!               **  COMPUTED Y(.) WILL BE THE EVALUATED VALUE OF THE   **
!               **  RIGHT-HAND SIDE OF THE SPECIFIED EQUATION Y = F(X).**
!               *********************************************************
!
 5000 CONTINUE
!
      NW=NWHOLD
      DO 5050 I=1,NW
        ITYPE(I)=ITYPEH(I)
        IW21(I)=IW21HO(I)
        IW22(I)=IW22HO(I)
        W2(I)=W2HOLD(I)
 5050 CONTINUE
!
      DO 5060 I=1,NW
        IF(ITYPE(I).EQ.'PAR')THEN
          IF(NUMPAR.GT.0)THEN
            DO 5070 J=1,NUMPAR
              J2=J
              IF(IW21(I).EQ.IPARN1(J).AND.IW22(I).EQ.IPARN2(J))THEN
                W2(I)=PARAM(J2)
                GO TO 5060
              ENDIF
 5070       CONTINUE
          ENDIF
!
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
          DO 5075 J=1,NUMPAR
            WRITE(ICOUT,5076)J,IPARN1(J),IPARN2(J)
 5076       FORMAT('      PARAMETER/VARIABLE NAME ',I3,'--',2A4)
            CALL DPWRST('XXX','BUG ')
 5075     CONTINUE
          WRITE(ICOUT,5077)(IA(J),J=1,MIN(100,NUMCHA))
 5077     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(ITYPE(I).EQ.'N'  .OR. ITYPE(I).EQ.'X'  .OR.   &
               ITYPE(I).EQ.'OP' .OR. ITYPE(I).EQ.'LP' .OR.   &
               ITYPE(I).EQ.'RP' .OR. ITYPE(I).EQ.'LF' .OR.   &
               ITYPE(I).EQ.'COM')THEN
          GO TO 5060
        ELSE
          WRITE(ICOUT,21)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5061)
 5061     FORMAT('      ITYPE(I) NOT X, OP, LP, PAR, OR LF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5062)I,ITYPE(I),IW21(I),IW22(I),W2(I)
 5062     FORMAT('I,ITYPE(I),IW21(I),IW22(I),W2(I) = ',   &
                 I8,3(2X,A4),2X,F15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
 5060 CONTINUE
!
      NLP=0
      NRP=0
      DO 5100 I=1,NW
        IF(ITYPE(I).EQ.'LP')NLP=NLP+1
        IF(ITYPE(I).EQ.'RP')NRP=NRP+1
 5100 CONTINUE
!
      IF(NLP.NE.NRP)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5156)
 5156   FORMAT('      NUMBER OF LEFT PARENTHESES NOT EQUAL TO ',   &
               'NUMBER OF RIGHT PARENTHESES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5157)NLP
 5157   FORMAT('      NUMBER OF LEFT  PARENTHESES = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5158)NRP
 5158   FORMAT('      NUMBER OF RIGHT PARENTHESES = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!CCCC ADD FOLLOWING LINES APRIL 1995.
!CCCC 2010/12: INITIALIZE TO CPUMIN RATHER THAN -99.9.
!CCCC          NEED TO MODIFY DPLIB1, DPLIB2, DPLIB3 TO
!CCCC          CHECK FOR CPUMIN RATHER THAN -99.9.
!
      ILIBC1=0
      ILIBC2=0
      DO 5195 IJ=1,MAXNST
        SAVE1(IJ)=CPUMIN
        SAVE2(IJ)=CPUMIN
        SAVE3(IJ)=CPUMIN
        SAVE4(IJ)=CPUMIN
        SAVE5(IJ)=CPUMIN
        SAVE6(IJ)=CPUMIN
        SAVE7(IJ)=CPUMIN
        SAVE8(IJ)=CPUMIN
 5195 CONTINUE
!
      DO 10000 II=1,N
!
        IF(II.GT.1)THEN
          NW=NWHOLD
          DO 5200 I=1,NW
            ITYPE(I)=ITYPEH(I)
            IW21(I)=IW21HO(I)
            IW22(I)=IW22HO(I)
            W2(I)=W2HOLD(I)
 5200     CONTINUE
        ENDIF
!
        IF(IBUGCO.EQ.'ON')THEN
          ISTEPN='7'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          DO 5250 I=1,NW
            WRITE(ICOUT,5251)I,IW21HO(I),IW22HO(I),IW21(I),IW22(I)
 5251       FORMAT('I,IW21HO(I),IW22HO(I),IW21(I),IW22(I) = ',   &
                   I8,4(2X,A4))
            CALL DPWRST('XXX','BUG ')
 5250     CONTINUE
        ENDIF
!
        ILOOP=1
 5350   CONTINUE
        DO 5400 I=1,NW
          I2=I
          IF(ITYPE(I).EQ.'RP')THEN
            ISTOP=I2
            DO 5600 JJ=1,ISTOP
              IREV=ISTOP-JJ+1
              IF(ITYPE(IREV).EQ.'LP')THEN
                ISTART=IREV
                GO TO 5690
              ENDIF
 5600       CONTINUE
            WRITE(ICOUT,21)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5605)
 5605       FORMAT('      ITYPE(IREV) NOT LP')
            CALL DPWRST('XXX','BUG ')
            ISTART=IREV
            GO TO 5690
          ENDIF
 5400   CONTINUE
        ISTOP=NW+1
        ISTART=0
 5690   CONTINUE
!
        ISTAP1=ISTART+1
        ISTOM1=ISTOP-1
        IJUNK=ISTART-1
        IF(IJUNK.GE.1)THEN
          IF(ITYPE(IJUNK).EQ.'LF')ILIBC1=ILIBC1+1
        ENDIF
        CALL EVALM(IW21,IW22,W2,ITYPE,ISTAP1,ISTOM1,IANGLU,Y,   &
                   SAVE1,SAVE2,SAVE3,SAVE4,SAVE5,SAVE6,SAVE7,SAVE8,   &
                   ILIBC1,ILIBC2,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(ISTART.GT.0)THEN
          W2(ISTART)=Y
          ITYPE(ISTART)='V'
          IF(NW.GT.1)THEN
            ISTOPP=ISTOP+1
            J=ISTART
            IF(ISTOP.NE.NW)THEN
              DO 5700 I=ISTOPP,NW
                J=J+1
                IW21(J)=IW21(I)
                IW22(J)=IW22(I)
                W2(J)=W2(I)
                ITYPE(J)=ITYPE(I)
 5700         CONTINUE
            ENDIF
            NW=J
            GO TO 5350
          ENDIF
        ENDIF
10000 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGCO.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF COMPIM--')
        CALL DPWRST('XXX','BUG ')
        DO 9113 I=1,MAXNST
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
!
      RETURN
      END SUBROUTINE COMPIM
      DOUBLE PRECISION FUNCTION CONDIT( N, SYMIN )
!
!     Computes condition number of symmetric matix in situ
!
      INTEGER NL, N
      PARAMETER ( NL = 100 )
      DOUBLE PRECISION DET, SYMIN(*), SUM, ROWMX, ROWMXI,   &
       SYM(NL*(NL+1)/2)
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
!
      RETURN
      END FUNCTION CONDIT
      SUBROUTINE CONINS(X,Y,NPT,XX,YY,NPTC)
!
!     PURPOSE--INCORPORATE AN INTERIOR CLOSED CONTOUR SEGMENT
!              INTO ANOTHER SEGMENT
!
!     RECOMMENDED DIMENSIONS--
!        X(NPT+NPTC+1)
!        Y(NPT+NPTC+1)
!        XX(NPTC)
!        YY(NPTC)
!        LC(4)
!
!     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
!                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
!     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
!
!---------------------------------------------------------------------
!
!CCCC DIMENSION X(NPT+NPTC+1),Y(NPT+NPTC+1),XX(NPTC),YY(NPTC),LC(4)
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XX(*)
      DIMENSION YY(*)
!
      DIMENSION LC(4)
!
!-----START POINT-----------------------------------------------------
!
!   FIRST FIND UP, DOWN, LEFT & RIGHT EXTREMES OF AN INTERIOR SEGMENT
      DO 10 I=1,4
        LC(I)=1
 10   CONTINUE
      DO 20 L=1,NPTC
        IF (XX(L).LT.XX(LC(1))) LC(1)=L
        IF (YY(L).GT.YY(LC(2))) LC(2)=L
        IF (XX(L).GT.XX(LC(3))) LC(3)=L
        IF (YY(L).LT.YY(LC(4))) LC(4)=L
 20   CONTINUE
!   FIND A REASONABLY CLOSE APPROACH OF INTERIOR SEGMENT TO THE CONTINUOUS
!                        STRING
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
!   REORDER THE INTERIOR SEGMENT
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
!   INSERT THE INTERIOR SEGMENT INTO THE CONTINUOUS STRING
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
      END SUBROUTINE CONINS
      SUBROUTINE CONCDF(DX,DSHAPE,DM,ICONDF,DCDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
!              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
!              DEFINED FOR ALL INTEGER X >= 1.
!
!              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
!              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
!              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
!              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
!              SIMILAR FORM TO THE GEETA DISTRIBUTION.
!
!              THE PROBABILITY MASS FUNCTION IS:
!              p(X;THETA,M)=
!                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  0 < THETA < 1; 1 <= M < 1/THETA
!
!              A RECURRENCE RELATION FOR THE CDF FUNCTION IS
!
!                  P(X;THETA,M) = {(M-1)*(X-1)+1}/(X-1)}*
!                                 THETA*(1-TYHETA)**(M-1)*
!                                 PROD[i=1 to X-2][(1 + M/(M*X-M-i)]*
!                                 P(X-1;THETA,M)
!
!              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
!              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
!              THE PROBABILITY MASS FUNCTION:
!              p(X;MU,M)=
!                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
!                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  MU >= 1; M > 1
!              NOTE THAT THE RELATION IS:
!
!                  THETA=(MU-1)/(M*MU)
!
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE A NON-NEGATIVE INTEGER.
!                     --DSHAPE = THE FIRST SHAPE PARAMETER
!                                (EITHER THETA OR MU)
!                     --DM     = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION FUNCTION
!             VALUE CDF FOR THE CONSUL DISTRIBUTION WITH SHAPE
!             PARAMETERS THETA (OR MU) AND M
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --0 < THETA < 1; 1 < M < 1/THETA
!                 --MU >= 1; M > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/8
!     ORIGINAL VERSION--AUGUST    2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPDFSV
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DSUM
!
      CHARACTER*4 ICONDF
      CHARACTER*4 ICOND2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
        DTHETA=(DMU-1.0D0)/(DM*DMU)
      ENDIF
!
      IX=INT(DX+0.5D0)
      IF(IX.LT.1)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO CONCDF IS LESS ',   &
      'THAN 1')
!
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GO TO 9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONCDF IS NOT ',   &
               'IN THE INTERVAL (0,1)')
!
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GO TO 9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONCDF IS NOT ',   &
               'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GO TO 9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONCDF IS ',   &
               'LESS THAN 1')
!
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DCDF=0.0
          GO TO 9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONCDF IS ',   &
               'LESS THAN 1')
      ENDIF
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DCDF=(1.0D0 - DTHETA)**DM
      IF(IX.EQ.1)THEN
        GO TO 9000
      ELSE
        DX=2.0D0
        ICOND2='THET'
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(IX.EQ.2)GO TO 9000
        DX=3.0D0
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(IX.EQ.3)GO TO 9000
        DPDFSV=DPDF
      ENDIF
!
      DO 100 I=4,IX
        DX=DBLE(I)
        DTERM1=DLOG(DTHETA) + (DM-1.0D0)*DLOG(1.0D0 - DTHETA)
        DTERM2=DLOG((DM-1.0D0)*(DX-1.0D0) + 1.0D0) - DLOG(DX-1.0D0)
        DTERM3=DTERM1 + DTERM2
        DSUM=0.0D0
        DO 200 J=1,I-2
          DSUM=DSUM + DLOG(1.0D0 + DM/(DM*DX - DM - DBLE(J)))
  200   CONTINUE
        IF(DPDFSV.GT.0.0D0)THEN
          DPDF=DEXP(DTERM3 + DSUM + DLOG(DPDFSV))
        ELSE
          GO TO 9000
        ENDIF
        DCDF=DCDF + DPDF
        DPDFSV=DPDF
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE CONCDF
      SUBROUTINE CONFOU(ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--CREATE STRINGS FOR CONFOUNDING FOR CERTAIN
!              TWO-LEVEL DESIGNS.
!     EXAMPLE--LET CON COP = CONFOUND  N K
!
!              BASED ON VALUES OF N AND K, A NUMBER OF STRINGS
!              STARTING WITH "CON" AND "COP" WILL BE CREATED.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/01
!     ORIGINAL VERSION--JANUARY   2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*8 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
!
      CHARACTER*8 ISTR1
      CHARACTER*8 ISTR2
      CHARACTER*8   ISTRZ1
      CHARACTER*16  ISTRZ2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CONF'
      ISUBN2='OU  '
      IERROR='NO'
!
      N=-1
      K=-1
      ILOC3=0
!
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
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),   &
                         IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',   &
                 'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)NUMCHF,MAXCHF
   57   FORMAT('NUMCHF,MAXCHF = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(120,MAXCHF))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  ERROR CHECKING--EXACTLY 6   **
!               **  AGUMENTS REQUIRED.          **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               **********************************
!               **  STEP 2--                    **
!               **  EXTRACT VALUES FOR N AND K  **
!               **  FROM RIGHT HAND SIDE.       **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     N AND K CAN EITHER BE PREVIOUSLY DEFINED PARAMETERS OR
!     SIMPLY NUMBERS.  ANY OTHER TYPE IS AN ERROR.
!
      IHRIGH=IHARG(5)
      IHRIG2=IHARG2(5)
!
      DO 210 I=1,NUMNAM
        IF(IHRIGH(1:4).EQ.IHNAME(I)(1:4) .AND.   &
           IHRIG2(1:4).EQ.IHNAM2(I)(1:4))THEN
            AK=VALUE(I)
            GO TO 219
        ENDIF
  210 CONTINUE
!
!     NAME NOT FOUND.  CHECK IF ARGUMENT IS A NUMBER.
!
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
        GO TO 9000
      ENDIF
  219 CONTINUE
!
      IHRIGH=IHARG(6)
      IHRIG2=IHARG2(6)
!
      DO 260 I=1,NUMNAM
        IF(IHRIGH(1:4).EQ.IHNAME(I)(1:4) .AND.   &
           IHRIG2(1:4).EQ.IHNAM2(I)(1:4))THEN
            AN=VALUE(I)
            GO TO 269
        ENDIF
  260 CONTINUE
!
!     NAME NOT FOUND.  CHECK IF ARGUMENT IS A NUMBER.
!
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
        GO TO 9000
      ENDIF
!
  269 CONTINUE
!
      IF(AK.GT.AN)THEN
        AKSAV=AK
        AK=AN
        AN=AKSAV
      ENDIF
!
      K=INT(AK+0.1)
      NTEMP=INT(AN+0.1)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,291)K,NTEMP
  291   FORMAT('K,NTEMP = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *************************************************
!               **  STEP 3--                                   **
!               **  EXTRACT THE BASE NAMES ON THE LHS OF THE   **
!               **  EQUAL SIGN AND THEN LOOP THROUGH THE       **
!               **  NUMBER OF STRINGS TO CREATE.               **
!               *************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT(1:4)=IHARG(1)
      IHLEFT(5:8)=IHARG2(1)
      NBASE1=1
      DO 310 I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE1=I
          GO TO 319
        ENDIF
  310 CONTINUE
  319 CONTINUE
!
      ISTR1=' '
      IF(NBASE1.LE.4)THEN
        ISTR1(1:NBASE1)=IHLEFT(1:NBASE1)
      ELSE
        ISTR1(1:4)=IHLEFT(1:4)
        NCHR=NBASE1-5+1
        ISTR1(5:NBASE1)=IHLEF2(1:NCHR)
      ENDIF
!
      IHLEFT(1:4)=IHARG(2)
      IHLEFT(5:8)=IHARG2(2)
      NBASE2=1
      DO 360 I=8,1,-1
        IF(IHLEFT(I:I).NE.' ')THEN
          NBASE2=I
          GO TO 369
        ENDIF
  360 CONTINUE
  369 CONTINUE
!
      ISTR2=' '
      IF(NBASE2.LE.4)THEN
        ISTR2(1:NBASE2)=IHLEFT(1:NBASE2)
      ELSE
        ISTR2(1:4)=IHLEFT(1:4)
        NCHR=NBASE2-5+1
        ISTR2(5:NBASE2)=IHLEF2(1:NCHR)
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NFOU')THEN
        WRITE(ICOUT,391)NBASE1,NBASE2,ISTR1,ISTR2
  391   FORMAT('NBASE1,NBASE2,ISTR1,ISTR2 = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 4--                    **
!               **  STEP THROUGH THE SUPPORTED  **
!               **  K/N COMBINATIONS AND CREATE **
!               **  THE STRINGS.                **
!               **********************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NTEMP.EQ.4)THEN
        IF(K.EQ.2)THEN
!
!         K = 2, N = 4 (2**2)
!
!           CON1   = 1
!           CON2   = 2
!           CON12  = 12
!
!           COP1   = 1
!           COP2   = 2
!           COP12  = 12
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.6)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.6)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8020
!
        ELSEIF(K.EQ.3)THEN
!
!         K = 3, N = 4 (2**(3-1)
!
!           CON1   = 1
!           CON2   = 2
!           CON12  = 3
!
!           COP1   = 1+23
!           COP2   = 2+13
!           COP12  = 3+12
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.6)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.6)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:4)='1+23'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:4)='2+13'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:4)='3+12'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          IERROR='YES'
          GO TO 8030
        ENDIF
      ELSEIF(NTEMP.EQ.8)THEN
        IF(K.EQ.3)THEN
!
!         K = 3, N = 8 (2**3)
!
!           CON1   = 1
!           CON2   = 2
!           CON3   = 3
!           CON12  = 12
!           CON13  = 13
!           CON23  = 23
!           CON123 = 123
!
!           COP1   = 1
!           COP2   = 2
!           COP3   = 3
!           COP12  = 12
!           COP13  = 13
!           COP23  = 23
!           COP123 = 123
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:2)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.4)THEN
!
!         K = 4, N = 8 (2**(4-1))
!
!           CON1   = 1
!           CON2   = 2
!           CON3   = 3
!           CON12  = 12
!           CON13  = 13
!           CON23  = 14
!           CON123 = 4
!
!           COP1   = 1
!           COP2   = 2
!           COP3   = 3
!           COP12  = 12+34
!           COP13  = 13+24
!           COP23  = 14+23
!           COP123 = 4
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:2)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+34'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+24'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='14+23'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.5)THEN
!
!         K = 5, N = 8 (2**(5-2))
!
!           CON1   = 1
!           CON2   = 2
!           CON3   = 3
!           CON12  = 4
!           CON13  = 5
!           CON23  = 23
!           CON123 = 25
!
!           COP1   = 1+24+35
!           COP2   = 2+14
!           COP3   = 3+15
!           COP12  = 4+12
!           COP13  = 5+13
!           COP23  = 23+45
!           COP123 = 25+34
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:7)='1+24+35'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:4)='2+14'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:4)='3+15'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:4)='4+12'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:4)='5+13'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='23+45'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:5)='25+34'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.6)THEN
!
!         K = 6, N = 8 (2**(6-3))
!
!           CON1   = 1
!           CON2   = 2
!           CON3   = 3
!           CON12  = 4
!           CON13  = 5
!           CON23  = 6
!           CON123 = 16
!
!           COP1   = 1+24+35
!           COP2   = 2+14+36
!           COP3   = 3+15+26
!           COP12  = 4+12+56
!           COP13  = 5+13+46
!           COP23  = 6+23+45
!           COP123 = 16+25+34
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:7)='1+24+35'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:7)='2+14+36'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:7)='3+15+26'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:7)='4+12+56'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:7)='5+13+46'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:7)='6+23+45'
          NCHAR=7
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:8)='16+25+34'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.7)THEN
!
!         K = 7, N = 8 (2**(7-4))
!
!           CON1   = 1
!           CON2   = 2
!           CON3   = 3
!           CON12  = 4
!           CON13  = 5
!           CON23  = 6
!           CON123 = 7
!
!           COP1   = 1+24+35+67
!           COP2   = 2+14+36+57
!           COP3   = 3+15+26+47
!           COP12  = 4+12+56+37
!           COP13  = 5+13+46+17
!           COP23  = 6+23+45+17
!           COP123 = 7+16+25+34
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.5)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:10)='1+24+35+67'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:10)='2+14+36+57'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:10)='3+15+26+47'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:10)='4+12+56+37'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:10)='5+13+46+17'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:10)='6+23+45+17'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:10)='7+16+25+34'
          NCHAR=10
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          IERROR='YES'
          GO TO 8030
        ENDIF
      ELSEIF(NTEMP.EQ.16)THEN
        IF(K.EQ.4)THEN
!
!         K = 4, N = 16 (2**4)
!
!           CON1    = 1
!           CON2    = 2
!           CON3    = 3
!           CON4    = 4
!           CON12   = 12
!           CON13   = 13
!           CON14   = 14
!           CON23   = 23
!           CON24   = 24
!           CON34   = 34
!           CON123  = 123
!           CON124  = 124
!           CON134  = 134
!           CON234  = 234
!           CON1234 = 1234
!
!           COP1    = 1
!           COP2    = 2
!           COP3    = 3
!           COP4    = 4
!           COP12   = 12
!           COP13   = 13
!           COP14   = 14
!           COP23   = 23
!           COP24   = 24
!           COP34   = 34
!           COP123  = 123
!           COP124  = 124
!           COP134  = 134
!           COP234  = 234
!           COP1234 = 1234
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.5)THEN
!
!         K = 5, N = 16 (2**(5-1))
!
!           CON1    = 1
!           CON2    = 2
!           CON3    = 3
!           CON4    = 4
!           CON12   = 12
!           CON13   = 13
!           CON14   = 14
!           CON23   = 23
!           CON24   = 24
!           CON34   = 34
!           CON123  = 45
!           CON124  = 35
!           CON134  = 25
!           CON234  = 15
!           CON1234 = 5
!
!           COP1    = 1
!           COP2    = 2
!           COP3    = 3
!           COP4    = 4
!           COP12   = 12
!           COP13   = 13
!           COP14   = 14
!           COP23   = 23
!           COP24   = 24
!           COP34   = 34
!           COP123  = 45
!           COP124  = 35
!           COP134  = 25
!           COP234  = 15
!           COP1234 = 5
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.6)THEN
!
!         K = 6, N = 16 (2**(6-2))
!
!           CON1    = 1
!           CON2    = 2
!           CON3    = 3
!           CON4    = 4
!           CON12   = 12
!           CON13   = 13
!           CON14   = 14
!           CON23   = 23
!           CON24   = 24
!           CON34   = 34
!           CON123  = 5
!           CON124  = 124
!           CON134  = 134
!           CON234  = 6
!           CON1234 = 16
!
!           COP1    = 1
!           COP2    = 2
!           COP3    = 3
!           COP4    = 4
!           COP12   = 12+35
!           COP13   = 13+25
!           COP14   = 14+56
!           COP23   = 23+15+46
!           COP24   = 24+36
!           COP34   = 34+26
!           COP123  = 5
!           COP124  = 124
!           COP134  = 134
!           COP234  = 6
!           COP1234 = 45
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+35'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+25'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:5)='14+56'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:8)='23+15+46'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:5)='24+36'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:5)='34+26'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.7)THEN
!
!         K = 7, N = 16 (2**(7-3))
!
!           CON1    = 1
!           CON2    = 2
!           CON3    = 3
!           CON4    = 4
!           CON12   = 12
!           CON13   = 13
!           CON14   = 14
!           CON23   = 23
!           CON24   = 24
!           CON34   = 34
!           CON123  = 7
!           CON124  = 124
!           CON134  = 6
!           CON234  = 5
!           CON1234 = 15
!
!           COP1    = 1
!           COP2    = 2
!           COP3    = 3
!           COP4    = 4
!           COP12   = 12+37+56
!           COP13   = 13+27+46
!           COP14   = 14+36+57
!           COP23   = 15+26+47
!           COP24   = 16+25+34
!           COP34   = 17+23+45
!           COP123  = 7
!           COP124  = 124
!           COP134  = 6
!           COP234  = 5
!           COP1234 = 15+26+47
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:8)='12+37+56'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:8)='13+27+46'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:8)='14+36+57'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:8)='15+26+47'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:8)='16+25+34'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:8)='17+23+45'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:8)='15+26+47'
          NCHAR=8
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.8)THEN
!
!         K = 8, N = 16 (2**(8-4))
!
!           CON1    = 1
!           CON2    = 2
!           CON3    = 3
!           CON4    = 4
!           CON12   = 12
!           CON13   = 13
!           CON14   = 14
!           CON23   = 23
!           CON24   = 24
!           CON34   = 34
!           CON123  = 7
!           CON124  = 8
!           CON134  = 6
!           CON234  = 5
!           CON1234 = 15
!
!           COP1    = 1
!           COP2    = 2
!           COP3    = 3
!           COP4    = 4
!           COP12   = 12+37+48+56
!           COP13   = 13+27+46+58
!           COP14   = 14+28+36+57
!           COP23   = 23+17+45+68
!           COP24   = 24+18+35+67
!           COP34   = 34+16+25+78
!           COP123  = 7
!           COP124  = 8
!           COP134  = 6
!           COP234  = 5
!           COP1234 = 15+26+38+47
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.4)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:11)='12+37+48+56'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:11)='13+27+46+58'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:11)='14+28+36+57'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:11)='23+17+45+68'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:11)='24+18+35+67'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:11)='34+16+25+78'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:11)='15+26+38+47'
          NCHAR=11
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          IERROR='YES'
          GO TO 8030
        ENDIF
      ELSEIF(NTEMP.EQ.32)THEN
        IF(K.EQ.5)THEN
!
!         K = 5, N = 32 (2**5)
!
!           CON1     = 1
!           CON2     = 2
!           CON3     = 3
!           CON4     = 4
!           CON5     = 5
!           CON12    = 12
!           CON13    = 13
!           CON14    = 14
!           CON15    = 15
!           CON23    = 23
!           CON24    = 24
!           CON25    = 25
!           CON34    = 34
!           CON35    = 35
!           CON45    = 45
!           CON123   = 123
!           CON124   = 124
!           CON125   = 125
!           CON134   = 134
!           CON135   = 135
!           CON145   = 145
!           CON234   = 234
!           CON235   = 235
!           CON245   = 245
!           CON345   = 345
!           CON1234  = 1234
!           CON1235  = 1235
!           CON1245  = 1245
!           CON1345  = 1345
!           CON2345  = 2345
!           CON12345 = 12345
!
!           COP1     = 1
!           COP2     = 2
!           COP3     = 3
!           COP4     = 4
!           COP5     = 5
!           COP12    = 12
!           COP13    = 13
!           COP14    = 14
!           COP15    = 15
!           COP23    = 23
!           COP24    = 24
!           COP25    = 25
!           COP34    = 34
!           COP35    = 35
!           COP45    = 45
!           COP123   = 123
!           COP124   = 124
!           COP125   = 125
!           COP134   = 134
!           COP135   = 135
!           COP145   = 145
!           COP234   = 234
!           COP235   = 235
!           COP245   = 245
!           COP345   = 345
!           COP1234  = 1234
!           COP1235  = 1235
!           COP1245  = 1245
!           COP1345  = 1345
!           COP2345  = 2345
!           COP12345 = 12345
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:4)='1235'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:4)='1245'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:4)='1345'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:4)='2345'
          NCHAR=4
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:5)='12345'
          NCHAR=5
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:4)='1234'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:4)='1235'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:4)='1245'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:4)='1345'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:4)='2345'
          NCHAR=4
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:5)='12345'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.6)THEN
!
!         K = 6, N = 32 (2**(6-1))
!
!           CON1     = 1
!           CON2     = 2
!           CON3     = 3
!           CON4     = 4
!           CON5     = 5
!           CON12    = 12
!           CON13    = 13
!           CON14    = 14
!           CON15    = 15
!           CON23    = 23
!           CON24    = 24
!           CON25    = 25
!           CON34    = 34
!           CON35    = 35
!           CON45    = 45
!           CON123   = 123
!           CON124   = 124
!           CON125   = 125
!           CON134   = 134
!           CON135   = 135
!           CON145   = 145
!           CON234   = 234
!           CON235   = 235
!           CON245   = 245
!           CON345   = 345
!           CON1234  = 56
!           CON1235  = 46
!           CON1245  = 36
!           CON1345  = 26
!           CON2345  = 16
!           CON12345 = 6
!
!           COP1     = 1
!           COP2     = 2
!           COP3     = 3
!           COP4     = 4
!           COP5     = 5
!           COP12    = 12
!           COP13    = 13
!           COP14    = 14
!           COP15    = 15
!           COP23    = 23
!           COP24    = 24
!           COP25    = 25
!           COP34    = 34
!           COP35    = 35
!           COP45    = 45
!           COP123   = 123
!           COP124   = 124
!           COP125   = 125
!           COP134   = 134
!           COP135   = 135
!           COP145   = 145
!           COP234   = 234
!           COP235   = 235
!           COP245   = 245
!           COP345   = 345
!           COP1234  = 56
!           COP1235  = 46
!           COP1245  = 36
!           COP1345  = 26
!           COP2345  = 16
!           COP12345 = 6
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:3)='123'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:3)='124'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:3)='125'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:3)='134'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:3)='145'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:3)='234'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:3)='245'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.7)THEN
!
!         K = 7, N = 32 (2**(7-2))
!
!           CON1     = 1
!           CON2     = 2
!           CON3     = 3
!           CON4     = 4
!           CON5     = 5
!           CON12    = 12
!           CON13    = 13
!           CON14    = 14
!           CON15    = 15
!           CON23    = 23
!           CON24    = 24
!           CON25    = 25
!           CON34    = 34
!           CON35    = 35
!           CON45    = 45
!           CON123   = 46
!           CON124   = 36
!           CON125   = 47
!           CON134   = 26
!           CON135   = 135
!           CON145   = 27
!           CON234   = 16
!           CON235   = 235
!           CON245   = 17
!           CON345   = 345
!           CON1234  = 6
!           CON1235  = 456
!           CON1245  = 7
!           CON1345  = 256
!           CON2345  = 156
!           CON12345 = 56
!
!           COP1     = 1
!           COP2     = 2
!           COP3     = 3
!           COP4     = 4
!           COP5     = 5
!           COP12    = 12
!           COP13    = 13
!           COP14    = 14
!           COP15    = 15
!           COP23    = 23
!           COP24    = 24
!           COP25    = 25
!           COP34    = 34
!           COP35    = 35+67
!           COP45    = 45
!           COP123   = 46
!           COP124   = 36+57
!           COP125   = 47
!           COP134   = 26
!           COP135   = 135
!           COP145   = 27
!           COP234   = 16
!           COP235   = 235
!           COP245   = 17
!           COP345   = 345
!           COP1234  = 6
!           COP1235  = 456
!           COP1245  = 7
!           COP1345  = 256
!           COP2345  = 156
!           COP12345 = 56+37
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:2)='47'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:3)='456'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:3)='256'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:3)='156'
          NCHAR=3
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:2)='56'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:5)='35+67'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:5)='36+57'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:2)='47'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:3)='135'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:3)='235'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:3)='345'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:3)='456'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:3)='256'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:3)='156'
          NCHAR=3
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:5)='56+37'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(K.EQ.10)THEN
!
!         K = 10, N = 32 (2**(10-5))
!
!           CON1     = 1
!           CON2     = 2
!           CON3     = 3
!           CON4     = 4
!           CON5     = 5
!           CON12    = 12
!           CON13    = 13
!           CON14    = 14
!           CON15    = 15
!           CON23    = 23
!           CON24    = 24
!           CON25    = 25
!           CON34    = 34
!           CON35    = 35
!           CON45    = 45
!           CON123   = 46
!           CON124   = 36
!           CON125   = 37
!           CON134   = 26
!           CON135   = 27
!           CON145   = 28
!           CON234   = 16
!           CON235   = 17
!           CON245   = 18
!           CON345   = 19
!           CON1234  = 6
!           CON1235  = 7
!           CON1245  = 8
!           CON1345  = 9
!           CON2345  = 0
!           CON12345 = 10
!
!           COP1     = 1
!           COP2     = 2
!           COP3     = 3
!           COP4     = 4
!           COP5     = 5
!           COP12    = 12+90
!           COP13    = 13+80
!           COP14    = 14+70
!           COP15    = 15+60
!           COP23    = 23+89
!           COP24    = 24+79
!           COP25    = 25+69
!           COP34    = 34+78
!           COP35    = 35+68
!           COP45    = 45+67
!           COP123   = 46+57
!           COP124   = 36+58
!           COP125   = 37+48
!           COP134   = 26+59
!           COP135   = 27+49
!           COP145   = 28+39
!           COP234   = 16+50
!           COP235   = 17+40
!           COP245   = 18+30
!           COP345   = 19+20
!           COP1234  = 6
!           COP1235  = 7
!           COP1245  = 8
!           COP1345  = 9
!           COP2345  = 0
!           COP12345 = 56+47+38+29+10
!
!         MAKE SURE STRING NAME WILL HAVE <= 8 CHARACTERS
!
          IF(NBASE1.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ELSEIF(NBASE2.GT.3)THEN
            IERROR='YES'
            GO TO 8010
          ENDIF
!
!         NOW CREATE THE STRINGS
!
          ISTR1(NBASE1+1:NBASE1+1)='1'
          ISTRZ1(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+1)='2'
          ISTRZ1(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='3'
          ISTRZ1(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='4'
          ISTRZ1(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='5'
          ISTRZ1(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='12'
          ISTRZ1(1:2)='12'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='13'
          ISTRZ1(1:2)='13'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='14'
          ISTRZ1(1:2)='14'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='15'
          ISTRZ1(1:2)='15'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='23'
          ISTRZ1(1:2)='23'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='24'
          ISTRZ1(1:2)='24'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='25'
          ISTRZ1(1:2)='25'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='34'
          ISTRZ1(1:2)='34'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='35'
          ISTRZ1(1:2)='35'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+2)='45'
          ISTRZ1(1:2)='45'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='123'
          ISTRZ1(1:2)='46'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='124'
          ISTRZ1(1:2)='36'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='125'
          ISTRZ1(1:2)='37'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='134'
          ISTRZ1(1:2)='26'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='135'
          ISTRZ1(1:2)='27'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='145'
          ISTRZ1(1:2)='28'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='234'
          ISTRZ1(1:2)='16'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='235'
          ISTRZ1(1:2)='17'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='245'
          ISTRZ1(1:2)='18'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+3)='345'
          ISTRZ1(1:2)='19'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1234'
          ISTRZ1(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1235'
          ISTRZ1(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1245'
          ISTRZ1(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='1345'
          ISTRZ1(1:1)='9'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+4)='2345'
          ISTRZ1(1:1)='0'
          NCHAR=1
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR1(NBASE1+1:NBASE1+5)='12345'
          ISTRZ1(1:2)='10'
          NCHAR=2
          CALL CONFO2(ISTR1,ISTRZ1,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='1'
          ISTRZ2(1:1)='1'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+1)='2'
          ISTRZ2(1:1)='2'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='3'
          ISTRZ2(1:1)='3'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='4'
          ISTRZ2(1:1)='4'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='5'
          ISTRZ2(1:1)='5'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='12'
          ISTRZ2(1:5)='12+90'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='13'
          ISTRZ2(1:5)='13+80'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='14'
          ISTRZ2(1:5)='14+70'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='15'
          ISTRZ2(1:5)='15+60'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='23'
          ISTRZ2(1:5)='23+89'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='24'
          ISTRZ2(1:5)='24+79'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='25'
          ISTRZ2(1:5)='25+69'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='34'
          ISTRZ2(1:5)='34+78'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='35'
          ISTRZ2(1:5)='35+68'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+2)='45'
          ISTRZ2(1:5)='45+67'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='123'
          ISTRZ2(1:5)='46+57'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='124'
          ISTRZ2(1:5)='36+58'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='125'
          ISTRZ2(1:5)='37+48'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='134'
          ISTRZ2(1:5)='26+59'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='135'
          ISTRZ2(1:5)='27+49'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='145'
          ISTRZ2(1:5)='28+39'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='234'
          ISTRZ2(1:5)='16+50'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='235'
          ISTRZ2(1:5)='17+40'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='245'
          ISTRZ2(1:5)='18+30'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+3)='345'
          ISTRZ2(1:5)='19+20'
          NCHAR=5
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1234'
          ISTRZ2(1:1)='6'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1235'
          ISTRZ2(1:1)='7'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1245'
          ISTRZ2(1:1)='8'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='1345'
          ISTRZ2(1:1)='9'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+4)='2345'
          ISTRZ2(1:1)='0'
          NCHAR=1
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ISTR2(NBASE2+1:NBASE2+5)='12345'
          ISTRZ2(1:14)='56+47+38+29+10'
          NCHAR=14
          CALL CONFO2(ISTR2,ISTRZ2,NCHAR,ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          IERROR='YES'
          GO TO 8030
        ENDIF
      ELSE
        IERROR='YES'
        GO TO 8030
      ENDIF
!
!               *****************************************************
!               **  STEP 7--                                       **
!               **  PRINT FEEDBACK MESSAGE                         **
!               *****************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFOU')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFEEDB.EQ.'ON' .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,710)
  710   FORMAT(I5,' THE CONFOUNDING STRINGS HAVE BEEN CREATED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               *****************************************************
!               **  STEP 8--                                       **
!               **  PRINT ERROR MESSAGES                           **
!               *****************************************************
!
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
      GO TO 9000
!
 8020 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8021)
 8021 FORMAT('       ERROR IN CREATING THE STRINGS.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
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
      GO TO 9000
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
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
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',   &
                 'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CONFOU
      SUBROUTINE CONFO2(ISTRIN,ISTRZZ,NCHAR,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--UTILITY ROUTINE FOR "CONFOU".  THIS ROUTINE
!              UPDATES A SINGLE STRING IN THE INTERNAL STRING
!              TABLE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/01
!     ORIGINAL VERSION--JANUARY   2015.
!     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4  ISTRZ2(40)
!
      CHARACTER*8 ISTRIN
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 NEWNAM
      CHARACTER*4 ICASEL
      CHARACTER*8 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*(*) ISTRZZ
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CONF'
      ISUBN2='O2  '
      IERROR='NO'
!
      N=-1
      K=-1
      ILOC3=0
!
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
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),   &
                         IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',   &
                 'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)NUMCHF,MAXCHF
   57   FORMAT('NUMCHF,MAXCHF = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(120,MAXCHF))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 5--                                         *
!               **  EXAMINE THE CURRENT STRING--                     *
!               **  IF THIS IS A PREVIOUSLY DEFINED NAME, IT SHOULD  *
!               **  BE A STRING    (IF NOT, REPORT AN ERROR).        *
!               ******************************************************
!
        ISTEPN='5'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFO2')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        DO 510 II=1,NUMNAM
          I2=II
          IF(ISTRIN(1:4).EQ.IHNAME(I2).AND.   &
             ISTRIN(5:8).EQ.IHNAM2(I2))THEN
            IF(IUSE(I2).EQ.'F')THEN
              ICASEL='STRI'
              ILISTL=I2
              GO TO 519
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,511)
  511         FORMAT('****** ERROR IN CONFO2--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,513)ISTRIN
  513         FORMAT('      THE NAME ',A8,' ALREADY EXISTS, BUT NOT ',   &
                     'AS A STRING.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,515)
  515         FORMAT('      THIS STRING WILL NOT BE CREATED.')
              CALL DPWRST('XXX','BUG ')
              GO TO 9000
            ENDIF
          ENDIF
  510   CONTINUE
!
        NEWNAM='YES'
        ICASEL='STRI'
!
        ILISTL=NUMNAM+1
        IF(ILISTL.GT.MAXNAM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,522)
  522     FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',   &
                 'FUNCTION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,524)MAXNAM
  524     FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
  519   CONTINUE
!
!               *****************************************************
!               **  STEP 6--                                       **
!               **  ADD THE CURRENT STRING                         **
!               *****************************************************
!
        ISTEPN='6'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'NFO2')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IHLEFT=ISTRIN(1:4)
        IHLEF2=ISTRIN(5:8)
        DO 411 J=1,NCHAR
          ISTRZ2(J)=' '
          ISTRZ2(J)(1:1)=ISTRZZ(J:J)
  411   CONTINUE
!
        CALL DPINFU(ISTRZ2,NCHAR,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                    NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,   &
                    NEWNAM,MAXNME,   &
                    IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
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
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',   &
                 'IVSTOP(I)=',I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CONFO2
      DOUBLE PRECISION FUNCTION CONFUN(DM)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE FUNCTIONS FOR THE
!              CONSUL MEAN AND ONES FREQUENCY EQUATION.
!
!              THE MEAN AND ONES FREQUENCY ESTIMATE OF MU IS:
!
!                  MUHAT = XBAR
!
!              THE ESTIMATE OF M IS THEN THE SOLUTION OF THE
!              EQUATION
!
!                 M*LOG(1 - (XBAR-1)/(M*XBAR)) - LOG(N1/N) = 0
!
!              CALLED BY DFZERO ROUTINE FOR SOLVING A NONLINEAR
!              UNIVARIATE EQUATION.
!     EXAMPLE--CONSUL MAXIMUM LIKELIHOOD Y
!     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/8
!     ORIGINAL VERSION--AUGUST    2006.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DM
!
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION S2
      DOUBLE PRECISION F1FREQ
      COMMON/CONCOM/XBAR,S2,F1FREQ,MAXROW,N
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CONFUN=DM*DLOG(1.0D0 - (XBAR-1.0D0)/(DM*XBAR)) - DLOG(F1FREQ)
!
      RETURN
      END FUNCTION CONFUN
      SUBROUTINE CONFU2(N,XPAR,FVEC,IFLAG,Y,K)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE FUNCTIONS FOR THE
!              CONSUL MAXIMUM LIKELIHOOD EQUATION.
!
!              THE MAXIMUM LIKELIHOOD FREQUENCY ESTIMATE OF MU IS:
!
!                  MUHAT = XBAR
!
!              THE ESTIMATE OF M IS THEN THE SOLUTION OF THE
!              EQUATION
!
!                 LOG(1 - (XBAR-1)/(M*XBAR)) + (1/(N*XBAR))*
!                 SUM[X=2 to k][SUM[i=0 to X-2][X*N(x)/(M*X-i)]] = 0
!
!              THIS ROUTINE ASSUMES THE DATA IS IN THE FORM
!
!                   X(I)  FREQ(I)
!
!              CALLED BY DNSQE ROUTINE FOR SOLVING SIMULTANEOUS
!              NONLINEAR EQUATIONS.  NOTE THAT THE CALLING SEQUENCE
!              DID NOT ACCOMODATE A DATA ARRAY (AND ASSCIATED NUMBER OF
!              OBSERVATIONS), SO THESE WERE ADDED TO THE CALL LIST.
!              SINCE DNSQE ONLY PASSES ONE ARRAY, WE SPLIT INTO
!              TWO PARTS: 1 - MAXNXT/2 ARE THE FREQUENCIES WHILE
!              (MAXNXT/2 + 1) - MAXNXT ARE THE CLASS VALUES (I.E.,
!              THE X).
!     EXAMPLE--CONSUL MAXIMUM LIKELIHOOD Y
!     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/8
!     ORIGINAL VERSION--AUGUST    2006.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION XPAR(*)
      DOUBLE PRECISION FVEC(*)
      REAL Y(*)
!
      DOUBLE PRECISION DM
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DFREQ
!
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION S2
      DOUBLE PRECISION F1FREQ
      COMMON/CONCOM/XBAR,S2,F1FREQ,MAXROW,NTOT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      N=1
      IFLAG=0
!
      DM=XPAR(1)
      DN=DBLE(NTOT)
      IINDX=MAXROW/2
!
      DTERM1=(DM*XBAR - XBAR + 1.0D0)/(DM*XBAR)
      DTERM2=1.0D0/(DN*XBAR)
!
      DSUM1=0.0D0
      DO 100 I=2,K
        DX=DBLE(Y(IINDX+I))
        DFREQ=Y(I)
        DO 200 J=0,I-2
          DSUM1=DSUM1 + DX*DFREQ/(DM*DX - DBLE(J))
  200   CONTINUE
  100 CONTINUE
!
      DTERM3=DTERM2*DSUM1
      FVEC(1)=DTERM1 - DEXP(-DTERM3)
!CCCC FVEC(1)=DTERM1 + DTERM2*DSUM1
!
      RETURN
      END SUBROUTINE CONFU2
      SUBROUTINE CONPDF(DX,DSHAPE,DM,ICONDF,DPDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY MASS
!              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
!              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
!              DEFINED FOR ALL INTEGER X >= 1.
!
!              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
!              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
!              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
!              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
!              SIMILAR FORM TO THE GEETA DISTRIBUTION.
!
!              THE PROBABILITY MASS FUNCTION IS:
!              p(X;THETA,M)=
!                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  0 < THETA < 1; 1 <= M < 1/THETA
!
!              THE MEAN AND VARIANCE ARE:
!
!                  MEAN     = 1/(1-THETA*M)
!                  VARIANCE = M*THETA*(1-THETA)/
!                             (1-THETA*M)**3
!
!              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
!              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
!              THE PROBABILITY MASS FUNCTION:
!              p(X;MU,M)=
!                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
!                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  MU >= 1; M > 1
!              NOTE THAT THE RELATION IS:
!
!                  THETA=(MU-1)/(M*MU)
!
!              THE MEAN AND VARIANCE BECOME:
!
!                  MEAN     = MU
!                  VARIANCE = MU*(MU-1)*(M*MU-MU+1)/M
!
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY MASS
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE A NON-NEGATIVE INTEGER.
!                     --DSHAPE = THE FIRST SHAPE PARAMETER
!                                (EITHER THETA OR MU)
!                     --DM     = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--DPDF   = THE DOUBLE PRECISION PROBABILITY MASS
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY MASS FUNCTION VALUE
!             PDF FOR THE CONSUL DISTRIBUTION WITH SHAPE PARAMETERS
!             THETA (OR MU) AND M
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --0 < THETA < 1; 1 < M < 1/THETA
!                 --MU >= 1; M > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/8
!     ORIGINAL VERSION--AUGUST    2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DPDF
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
      DOUBLE PRECISION DTERM6
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DLNGAM
!
      CHARACTER*4 ICONDF
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
      ENDIF
!
      IX=INT(DX+0.5D0)
      IF(IX.LT.1)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        DPDF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO CONPDF IS LESS ',   &
      'THAN 1')
!
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GO TO 9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPDF IS NOT ',   &
               'IN THE INTERVAL (0,1)')
!
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GO TO 9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPDF IS NOT ',   &
               'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GO TO 9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPDF IS ',   &
               'LESS THAN 1')
!
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPDF=0.0
          GO TO 9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPDF IS ',   &
               'LESS THAN 1')
      ENDIF
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DX=DBLE(IX)
!
      IF(ICONDF.EQ.'THET')THEN
        DTERM1=DLNGAM(DM*DX+1.0D0) + (DX-1.0D0)*DLOG(DTHETA) +   &
               (DM*DX-DX+1.0D0)*DLOG(1.0D0 - DTHETA)
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE CONPDF
      SUBROUTINE CONPPF(DP,DSHAPE,DM,ICONDF,DPPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE CONSUL DISTRIBUTION WITH SHAPE
!              PARAMETERS THETA AND M.  THIS DISTRIBUTION IS
!              DEFINED FOR ALL INTEGER X >= 1.
!
!              THIS DISTRIBUTION REDUCES TO THE GEOMETRIC
!              DISTRIBUTION WHEN M = 1.  FOR THIS REASON, IT
!              SOMETIMES REFERRED TO AS THE GENERALIZED GEOMETRIC
!              DISTRIBUTION.  NOTE THAT THIS DISTRIBUTION HAS A
!              SIMILAR FORM TO THE GEETA DISTRIBUTION.
!
!              THE PROBABILITY MASS FUNCTION IS:
!              p(X;THETA,M)=
!                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  0 < THETA < 1; 1 <= M < 1/THETA
!
!              A RECURRENCE RELATION FOR THE CDF FUNCTION IS
!
!                  P(X;THETA,M) = {(M-1)*(X-1)+1}/(X-1)}*
!                                 THETA*(1-TYHETA)**(M-1)*
!                                 PROD[i=1 to X-2][(1 + M/(M*X-M-i)]*
!                                 P(X-1;THETA,M)
!
!              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
!              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
!              THE PROBABILITY MASS FUNCTION:
!              p(X;MU,M)=
!                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
!                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  MU >= 1; M > 1
!              NOTE THAT THE RELATION IS:
!
!                  THETA=(MU-1)/(M*MU)
!
!              THE PERCENT POINT FUNCTION IS COMPUTED BY SUMMING
!              THE CUMULATIVE DISTRIBUTION UNTIL THE APPROPRIATE
!              PROBABILITY IS REACHED.
!
!     INPUT  ARGUMENTS--DP     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --DSHAPE = THE FIRST SHAPE PARAMETER
!                                (EITHER THETA OR MU)
!                     --DM     = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--DPPF   = THE DOUBLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
!             VALUE PPF FOR THE CONSUL DISTRIBUTION WITH SHAPE
!             PARAMETERS THETA (OR MU) AND M
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --0 < THETA < 1; 1 < M < 1/THETA
!                 --MU >= 1; M > 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/8
!     ORIGINAL VERSION--AUGUST    2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DP
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSHAPE
      DOUBLE PRECISION DM
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DPDFSV
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DMU
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DEPS
!
      CHARACTER*4 ICONDF
      CHARACTER*4 ICOND2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(ICONDF.EQ.'THET')THEN
        DTHETA=DSHAPE
      ELSE
        DMU=DSHAPE
        DTHETA=(DMU-1.0D0)/(DM*DMU)
      ENDIF
!
      IF(DP.LT.0.0D0 .OR. DP.GE.1.0D0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DP
        CALL DPWRST('XXX','BUG ')
        DPPF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO GETPPF IS OUTSIDE ',   &
      'THE (0,1] INTERVAL')
!
      IF(ICONDF.EQ.'THET')THEN
        IF(DTHETA.LE.0.0D0 .OR. DTHETA.GE.1.0D0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DTHETA
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GO TO 9000
        ENDIF
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPPF IS NOT ',   &
               'IN THE INTERVAL (0,1)')
!
        IF(DM.LT.1.0D0 .OR. DM.GE.1.0D0/DTHETA)THEN
          WRITE(ICOUT,25)1.0D0/DTHETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GO TO 9000
        ENDIF
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPPF IS NOT ',   &
               'IN THE INTERVAL (1,',G15.7,')')
      ELSE
        IF(DMU.LT.1.0D0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DMU
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GO TO 9000
        ENDIF
   35   FORMAT('***** ERROR--THE SECOND ARGUMENT TO CONPPF IS ',   &
               'LESS THAN 1')
!
        IF(DM.LT.1.0D0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)DM
          CALL DPWRST('XXX','BUG ')
          DPPF=0.0
          GO TO 9000
        ENDIF
   38   FORMAT('***** ERROR--THE THIRD ARGUMENT TO CONPPF IS ',   &
               'LESS THAN 1')
      ENDIF
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DEPS=1.0D-7
      DCDF=(1.0D0 - DTHETA)**DM
      IF(DCDF.GE.DP-DEPS)THEN
        DPPF=1.0D0
        GO TO 9000
      ELSE
        DX=2.0D0
        ICOND2='THET'
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=2.0D0
          GO TO 9000
        ENDIF
        DX=3.0D0
        CALL CONPDF(DX,DTHETA,DM,ICOND2,DPDF)
        DCDF=DCDF+DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=3.0D0
          GO TO 9000
        ENDIF
        DPDFSV=DPDF
      ENDIF
!
      I=3
  100 CONTINUE
        I=I+1
        DX=DBLE(I)
        DTERM1=DLOG(DTHETA) + (DM-1.0D0)*DLOG(1.0D0 - DTHETA)
        DTERM2=DLOG((DM-1.0D0)*(DX-1.0D0) + 1.0D0) - DLOG(DX-1.0D0)
        DTERM3=DTERM1 + DTERM2
        DSUM=0.0D0
        DO 200 J=1,I-2
          DSUM=DSUM + DLOG(1.0D0 + DM/(DM*DX - DM - DBLE(J)))
  200   CONTINUE
        IF(DPDFSV.GT.0.0D0)THEN
          DPDF=DEXP(DTERM3 + DSUM + DLOG(DPDFSV))
        ELSE
          DPPF=DBLE(I)
          GO TO 9000
        ENDIF
        DCDF=DCDF + DPDF
        IF(DCDF.GE.DP-DEPS)THEN
          DPPF=DBLE(I)
          GO TO 9000
        ENDIF
        DPDFSV=DPDF
      GO TO 100
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE CONPPF
      SUBROUTINE CONRAN(N,SHAPE,AM,ICONDF,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE CONSUL DISTRIBUTION WITH SHAPE PARAMETERS
!              THETA OR MU AND AM.
!
!              THE PROBABILITY MASS FUNCTION IS:
!              p(X;THETA,M)=
!                  (M*X  X-1)*THETA**(X-1)*(1-THETA)**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  0 < THETA < 1; 1 <= M < 1/THETA
!
!              THE MEAN AND VARIANCE ARE:
!
!                  MEAN     = 1/(1-THETA*M)
!                  VARIANCE = M*THETA*(1-THETA)/
!                             (1-THETA*M)**3
!
!              THIS DISTRIBUTION IS SOMETIMES PARAMETERIZED USING
!              THE MEAN (MU) INSTEAD OF THETA.  THIS RESULTS IN
!              THE PROBABILITY MASS FUNCTION:
!              p(X;MU,M)=
!                  (M*X  X-1)*((MU-1)/(M*MU))**(X-1)*
!                  (1 - (M-1)/(M*MU))**(M*X-X+1)/X
!                  X = 1, 2, 3, ,...
!                  MU >= 1; M > 1
!              NOTE THAT THE RELATION IS:
!
!                  THETA=(MU-1)/(M*MU)
!
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --SHAPE  = THE SINGLE PRECISION VALUE
!                                OF THE FIRST SHAPE PARAMETER.
!                     --AM     = THE SINGLE PRECISION VALUE
!                                OF THE SECOND SHAPE PARAMETER.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE CONSUL
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE CONSUL DISTRIBUTION
!             WITH SHAPE PARAMETERS THETA (OR MU) AND AM.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --0 < THETA < 1, 1 < M < 1/THETA
!                   MU >= 1; M > 1
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, CONPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CONSUL (1990), "CONSUL DISTRIBUTION AND ITS
!                 PROPERTIES", COMMUNICATIONS IN STATISTICS--
!                 THEORY AND METHODS, 19, PP. 3051-3068.
!               --CONSUL AND FAMOYE (2006), "LAGRANGIAN PROBABILITY
!                 DISTRIBUTIONS", BIRKHAUSER, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/7
!     ORIGINAL VERSION--JULY      2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
      CHARACTER*4 ICONDF
!
      DOUBLE PRECISION DPPF
!
!---------------------------------------------------------------------
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
        GO TO 9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF CONSUL RANDOM ',   &
             'NUMBERS IS NON-POSITIVE')
!
      IF(ICONDF.EQ.'THET')THEN
        THETA=SHAPE
        IF(THETA.LE.0.0 .OR. THETA.GE.1.0)THEN
          WRITE(ICOUT,15)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)THETA
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
   15   FORMAT('***** ERROR--THE THETA PARAMETER FOR THE CONSUL')
   16   FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (0,1) INTERVAL')
!
        IF(AM.LT.1.0 .OR. AM.GE.1.0/THETA)THEN
          WRITE(ICOUT,25)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,26)1.0/THETA
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AM
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
   25   FORMAT('***** ERROR--THE M PARAMETER FOR THE CONSUL')
   26   FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (1,',G15.7,') ',   &
               'INTERVAL')
      ELSE
        AMU=SHAPE
        IF(AMU.LT.1.0)THEN
          WRITE(ICOUT,35)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,36)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AMU
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
   35   FORMAT('***** ERROR--THE MU PARAMETER FOR THE CONSUL')
   36   FORMAT('      RANDOM NUMBERS IS LESS THAN 1')
!
        IF(AM.LE.1.0)THEN
          WRITE(ICOUT,38)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,39)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46)AM
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
   38   FORMAT('***** ERROR--THE M PARAMETER FOR THE CONSUL')
   39   FORMAT('      RANDOM NUMBERS IS LESS THAN OR EQUAL TO 1')
      ENDIF
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N CONSUL DISTRIBUTION RANDOM NUMBERS USING THE
!     INVERSION METHOD.
!
      CALL UNIRAN(N,ISEED,X)
      DO 100 I=1,N
        XTEMP=X(I)
        CALL CONPPF(DBLE(XTEMP),DBLE(SHAPE),DBLE(AM),ICONDF,DPPF)
        X(I)=REAL(DPPF)
  100 CONTINUE
!
 9000 CONTINUE
!
      RETURN
      END SUBROUTINE CONRAN
      SUBROUTINE CONV14(ISTRIN,NSTRIN,IA,IB,IWIDTH,IBUGXX,IERROR)
!
!     PURPOSE--CONVERT THE FIRST NSTRIN CHARACTERS IF ISTRIN
!              TO THE FIRST CHARACTERS OF THE CHARACTER*4 ARRAYS
!              IA AND IB.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--93.3
!     ORIGINAL VERSION--FEBRUARY 1993
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*80 ISTRIN
      CHARACTER (LEN=*) :: ISTRIN
      CHARACTER (LEN=4) :: IA(*)
      CHARACTER (LEN=4) :: IB(*)
      CHARACTER*4 IBUGXX
      CHARACTER*4 IERROR
!
      CHARACTER*4 IC4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGXX.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CONV14--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGXX,IERROR,NSTRIN
   52   FORMAT('IBUGXX,IERROR,NSTRIN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ISTRIN(1:80)
   53   FORMAT('ISTRIN(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IWIDTH=NSTRIN
      IF(1.LE.NSTRIN.AND.NSTRIN.LE.80)THEN
         DO 1000 I=1,NSTRIN
           IC4='    '
           IC4(1:1)=ISTRIN(I:I)
           IA(I)=IC4
           IB(I)=IC4
 1000    CONTINUE
         IERROR='NO'
      ELSE
         IERROR='YES'
      ENDIF
!
      IF(IBUGXX.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CONV14--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,NSTRIN,IWIDTH
 9014   FORMAT('IERROR,NSTRIN,IWIDTH = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          DO 9020 I=1,IWIDTH
            WRITE(ICOUT,9021)I,IA(I),IB(I)
 9021       FORMAT('I,IA(I),IB(I) = ',I8,2X,A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE CONV14
      SUBROUTINE CONVOL(Y1,N1,Y2,N2,NUMVAR,IWRITE,MAXN,   &
                        Y3,N3,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CONVOLUTION OF 2 VARIABLES.
!     NOTE--IF  THE FIRST  VARIABLE IS Y1(.)
!           AND THE SECOND VARIABLE IS Y2(.),
!           THEN THE OUTPUT VARIABLE CONTAINING THE
!           CONVOLUTION
!           WILL BE COMPUTED AS FOLLOWS--
!              Y3(1) = Y1(1)*Y2(1)
!              Y3(2) = Y1(1)*Y2(2) + Y1(2)*Y2(1)
!              Y3(3) = Y1(1)*Y2(3) + Y1(2)*Y2(2) + Y1(3)*Y2(1)
!              ETC.
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y3(.)
!           BEING IDENTICAL (OVERLAYED) ON THE INPUT VECTORS Y1(.) OR Y2(.)
!     NOTE--Y1 AND Y2 NEED NOT BE THE SAME LENGTH.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1981.
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
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CONV'
      ISUBN2='OL  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CONVOL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,IWRITE,N1,N2,NUMVAR,MAXN
   53   FORMAT('IBUGA3,IWRITE,N1,N2,NUMVAR,MAXN = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 57 I=1,N2
          WRITE(ICOUT,58)I,Y2(I)
   58     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
      ENDIF
!
!               *******************************
!               **  COMPUTE THE CONVOLUTION  **
!               *******************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.0)GO TO 150
      IF(N2.LE.0)GO TO 150
      I3MIN=2
      I3MAX=N1+N2
      N3=I3MAX-I3MIN+1
      IF(N3.GT.MAXN)GO TO 170
!
      DO 100 I3=1,N3
      Y3(I3)=0.0
  100 CONTINUE
!
      DO 500 I1=1,N1
      DO 600 I2=1,N2
      Y1P=Y1(I1)
      Y2P=Y2(I2)
      Y3P=Y1P*Y2P
      IARG=I1+I2-1
      Y3(IARG)=Y3(IARG)+Y3P
  600 CONTINUE
  500 CONTINUE
      GO TO 190
!
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
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',2I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 190
!
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
  177 FORMAT('      THE OUTPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 190
!
  190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CONVOL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N1,N2,NUMVAR,MAXN,N3
 9013   FORMAT('IERROR,N1,N2,NUMVAR,MAXN,N3 = ',A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N3
          WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I)
 9016     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CONVOL
      SUBROUTINE CORMAT(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
!              CORRECT MATCHES BETWEEN TWO VARIABLES.  THE
!              NUMBER OF CORRECT MATCHES IS THE SUM OF THE
!              TRUE POSITIVES AND TRUE NEGATIVES.
!
!              THIS IS SPECIFICALLY FOR THE 2X2 CASE.  THAT IS,
!              EACH VARIABLE HAS TWO MUTUALLY EXCLUSIVE
!              CHOICES CODED AS 1 (FOR SUCCESS) OR 0 (FOR
!              FAILURE).  A TRUE POSITIVE IS DEFINED AS THE
!              CASE WHERE THE SECOND VARIABLE IS 1 AND THE FIRST
!              VARIABLE IS A 1.
!
!              A TYPICAL EXAMPLE WOULD BE WHERE VARIABLE ONE
!              DENOTES THE GROUND TRUTH AND A VALUE OF 1
!              INDICATES "PRESENT" AND A VALUE OF 0 INDICATES
!              "NOT PRESENT".  VARIABLE TWO REPRESENTS SOME TYPE
!              OF DETECTION DEVICE WHERE A VALUE OF 1 INDICATES
!              THE DEVICE DETECTED THE SPECIFIED OBJECT WHILE A
!              VALUE OF 0 INDICATES THAT THE OBJECT WAS NOT
!              DETECTED.  A TRUE POSITIVE THEN IS THE CASE WHERE
!              THE DEVICE DETECTED THE OBJECT WHEN IT WAS
!              ACTUALY THERE.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE SECOND SET
!                                OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED TRUE POSITIVE PROPORTION
!                                BETWEEN THE 2 SETS OF DATA
!                                IN THE INPUT VECTORS X AND Y.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TRUE POSITIVE PROPORTION BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/5
!     ORIGINAL VERSION--MAY       2007.
!     UPDATED         --AUGUST    2007. IF 2X2 CASE, CHECK IF SUM
!                                       OF ENTRIES IS <= 4.  IN THIS
!                                       CASE, ASSUME WE HAVE RAW DATA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XIDTEM(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TRUP'
      ISUBN2='OS  '
!
      IERROR='NO'
!
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR IN THE CORRECT MATCH PROPORTION')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLES IS LESS THAN TWO')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1205)N
 1205   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 22--                             **
!               **  CHECK THAT THE VARIABLES HAVE AT MOST **
!               **  TWO DISTINCT VALUES (1 INDICATES A    **
!               **  SUCCESS, 0 INDICATES A FAILURE).      **
!               ********************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOTE: CHECK FOR SPECIAL CASE N = 2.  IN THIS CASE,
!           ASSUME WE HAVE A 2X2 TABLE OF COUNTS INSTEAD
!           OF RAW DATA.
!
      IF(N.EQ.2)THEN
        N11=INT(X(1)+0.5)
        N21=INT(X(2)+0.5)
        N12=INT(Y(1)+0.5)
        N22=INT(Y(2)+0.5)
!
!       CHECK IF ALL ENTRIES 0 OR 1. IF SO, ASSUME
!       RAW DATA CASE.
!
        IF((N11.EQ.0 .OR. N11.EQ.1) .AND.   &
           (N12.EQ.0 .OR. N12.EQ.1) .AND.   &
           (N21.EQ.0 .OR. N21.EQ.1) .AND.   &
           (N22.EQ.0 .OR. N22.EQ.1)) GO TO 1349
!
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1311)
 1311     FORMAT('      ROW 1 COLUMN 1 OF THE COUNTS TABLE IS ',   &
                 'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1321)
 1321     FORMAT('      ROW 2 COLUMN 1 OF THE COUNTS TABLE IS ',   &
                 'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1331)
 1331     FORMAT('      ROW 1 COLUMN 2 OF THE COUNTS TABLE IS ',   &
                 'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
 1341     FORMAT('      ROW 2 COLUMN 2 OF THE COUNTS TABLE IS ',   &
                 'NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        NTEMP=N11 + N12 + N21 + N22
        STAT=REAL(N11 + N22)/REAL(NTEMP)
        GO TO 3000
      ENDIF
!
 1349 CONTINUE
!
      CALL DISTIN(X,N,IWRITE,XIDTEM,NDIST,IBUGA3,IERROR)
      IF(NDIST.EQ.1)THEN
        AVAL=XIDTEM(1)
        IF(ABS(AVAL).LE.0.5)THEN
          AVAL=0.0
        ELSE
          AVAL=1.0
        ENDIF
        DO 2202 I=1,N
          X(I)=1.0
 2202   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        IF(XIDTEM(1).EQ.1.0 .OR. XIDTEM(2).EQ.1.0)THEN
          DO 2203 I=1,N
            IF(X(I).NE.1.0)X(I)=0.0
 2203     CONTINUE
        ELSE
          ATEMP1=MIN(XIDTEM(1),XIDTEM(2))
          ATEMP2=MAX(XIDTEM(1),XIDTEM(2))
          DO 2208 I=1,N
            IF(X(I).EQ.ATEMP1)X(I)=0.0
            IF(X(I).EQ.ATEMP2)X(I)=1.0
 2208     CONTINUE
        ENDIF
      ELSEIF(NDIST.GT.2)THEN
        N11=0
        N12=0
        N21=0
        DO 2510 I=1,N
          IF(Y(I).EQ.X(I))THEN
            N11=N11+1
          ELSEIF(Y(I).LT.X(I))THEN
            N12=N12+1
          ELSEIF(Y(I).GT.X(I))THEN
            N21=N21+1
          ENDIF
 2510   CONTINUE
        STAT=REAL(N11)/REAL(N)
        GO TO 9000
      ELSE
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1201)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2211)
!2211   FORMAT('      RESPONSE VARIABLE ONE SHOULD CONTAIN AT MOST')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2213)
!2213   FORMAT('      TWO DISTINCT VALUES.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2215)NDIST
!2215   FORMAT('      ',I8,' DISTINCT VALUES FOUND.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
      ENDIF
!
      CALL DISTIN(Y,N,IWRITE,XIDTEM,NDIST,IBUGA3,IERROR)
      IF(NDIST.EQ.1)THEN
        AVAL=XIDTEM(1)
        IF(ABS(AVAL).LE.0.5)THEN
          AVAL=0.0
        ELSE
          AVAL=1.0
        ENDIF
        DO 2302 I=1,N
          Y(I)=1.0
 2302   CONTINUE
      ELSEIF(NDIST.EQ.2)THEN
        IF(XIDTEM(1).EQ.1.0 .OR. XIDTEM(2).EQ.1.0)THEN
          DO 2303 I=1,N
            IF(Y(I).NE.1.0)Y(I)=0.0
 2303     CONTINUE
        ELSE
          ATEMP1=MIN(XIDTEM(1),XIDTEM(2))
          ATEMP2=MAX(XIDTEM(1),XIDTEM(2))
          DO 2308 I=1,N
            IF(Y(I).EQ.ATEMP1)Y(I)=0.0
            IF(Y(I).EQ.ATEMP2)Y(I)=1.0
 2308     CONTINUE
        ENDIF
      ELSEIF(NDIST.GT.2)THEN
        N11=0
        N12=0
        N21=0
        DO 2520 I=1,N
          IF(Y(I).EQ.X(I))THEN
            N11=N11+1
          ELSEIF(Y(I).LT.X(I))THEN
            N12=N12+1
          ELSEIF(Y(I).GT.X(I))THEN
            N21=N21+1
          ENDIF
 2520   CONTINUE
        STAT=REAL(N11)/REAL(N)
        GO TO 9000
      ELSE
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1201)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2311)
!2311   FORMAT('      RESPONSE VARIABLE TWO SHOULD CONTAIN AT MOST')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2313)
!2313   FORMAT('      TWO DISTINCT VALUES.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2315)NDIST
!2315   FORMAT('      ',I8,' DISTINCT VALUES FOUND.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
      ENDIF
!
      N11=0
      N12=0
      N21=0
      N22=0
      DO 2410 I=1,N
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
!
      STAT=REAL(N11 + N22)/REAL(N)
!
 3000 CONTINUE
!
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 890
      IF(IWRITE.EQ.'OFF' .OR. IWRITE.EQ.'NO')GO TO 890
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CORRECT MATCH PROPORTION = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
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
!
      RETURN
      END SUBROUTINE CORMAT
      SUBROUTINE CORR(X,Y,N,IWRITE,XYCORR,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CORRELATION COEFFICIENT
!              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
!              THE SAMPLE CORRELATION COEFFICIENT WILL BE A SINGLE
!              PRECISION VALUE CALCULATED AS THE SUM OF CROSS PRODUCTS
!              DIVIDED BY (N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE SECOND SET OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--XYCORR = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CORRELATION COEFFICIENT
!                                BETWEEN THE 2 SETS OF DATA IN THE
!                                INPUT VECTORS X AND Y.  THIS SINGLE
!                                PRECISION VALUE WILL BE BETWEEN -1.0
!                                AND 1.0 (INCLUSIVELY).
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CORRELATION COEFFICIENT BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KENDALL AND STUART, THE ADVANCED THEORY OF
!                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 235-236.
!               --KENDALL AND STUART, THE ADVANCED THEORY OF
!                 STATISTICS, VOLUME 2, EDITION 1, 1961, PAGES 292-293.
!               --SNEDECOR AND COCHRAN, STATISTICAL METHODS,
!                 EDITION 6, 1967, PAGES 172-198.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1979.
!     UPDATED         --JUNE      1979.
!     UPDATED         --JULY      1979.
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
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM12
      DOUBLE PRECISION DMEAN1
      DOUBLE PRECISION DMEAN2
      DOUBLE PRECISION DSQRT1
      DOUBLE PRECISION DSQRT2
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CORR'
      ISUBN2='    '
      IERROR='NO'
!
      DN=0.0D0
      DMEAN1=0.0D0
      DMEAN2=0.0D0
      DSUM12=0.0D0
!
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
        DO 55 I=1,N
         WRITE(ICOUT,56)I,X(I),Y(I)
   56    FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE     CORRELATION COEFFICIENT  **
!               *******************************************
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
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        XYCORR=1.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      IF(IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** WARNING IN CORRELATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,136)HOLD
  136   FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ',   &
               'ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      XYCORR=0.0
      GO TO 9000
  139 CONTINUE
!
      HOLD=Y(1)
      DO 145 I=2,N
       IF(Y(I).NE.HOLD)GO TO 149
  145 CONTINUE
      IF(IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,146)HOLD
  146   FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ',   &
               'ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      XYCORR=0.0
      GO TO 9000
  149 CONTINUE
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE     CORRELATION COEFFICIENT.  **
!               ************************************************
!
      DN=N
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 200 I=1,N
        DX1=X(I)
        DX2=Y(I)
        DSUM1=DSUM1+DX1
        DSUM2=DSUM2+DX2
  200 CONTINUE
      DMEAN1=DSUM1/DN
      DMEAN2=DSUM2/DN
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM12=0.0D0
      DO 300 I=1,N
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
        WRITE(ICOUT,811)N,XYCORR
  811   FORMAT('THE CORRELATION COEFFICIENT OF THE ',I8,   &
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
 9011   FORMAT('***** AT THE END       OF CORR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,XYCORR
 9012   FORMAT('IERROR,XYCORR = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DN,DMEAN1,DMEAN2,DSUM12
 9014   FORMAT('DN,DMEAN1,DMEAN2,DSUM12 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CORR
      SUBROUTINE CORRAT(Y,X,N,ICASE,IWRITE,XDIST,ETA,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CORRELATION RATIO
!              FOR THE RESPONSE VARIABLE Y AND GROUP-ID VARIABLE X.
!              THE FORMULA IS:
!
!                 ETA**2 = SUM[i=1 to p][N(i)*(YBAR(i) - YBAR)**2/
!                          SUM[i=1 to p][SUM[j=1 to N(i)][Y(ij) - YBAR)**2
!
!              WHERE
!
!                 P       = NUMBER OF GROUPS
!                 N(i)    = NUMBER OF OBERVATIONS IN GROUP i
!                 YBAR(i) = MEAN OF GROUP i
!                 YBAR    = GRAND MEAN
!
!              THE INTRACLASS CORRELATION COEFFICIENT IS THE THE
!              SQUARE ROOT OF THE CORRELATION RATIO.
!
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS FOR THE
!                                RESPONSE VARIABLE.
!                     --X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS FOR THE
!                                GROUP-ID VARIABLE.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--CORR   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CORRELATION RATIO.
!                                THIS SINGLE PRECISION VALUE WILL BE
!                                BETWEEN -1.0 AND 1.0 (INCLUSIVELY).
!                     --ETA    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE INTRACLASS CORRELATION.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE
!             CORRELATION RATIO FOR THE 2 SETS OF DATA IN THE INPUT
!             VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--Pearson E.S. (1926) "Review of Statistical Methods
!                 for Research Workers (R. A. Fisher)", Science
!                 Progress, 20, 733-734.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019/08
!     ORIGINAL VERSION--AUGUST    2019.
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
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION XDIST(*)
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DSUM4
      DOUBLE PRECISION DMEAN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CORR'
      ISUBN2='AT  '
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE     CORRELATION RATIO        **
!               *******************************************
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
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE     CORRELATION RATIO.        **
!               ************************************************
!
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      CALL MEAN(Y,N,IWRITE,YBAR,IBUGA3,IERROR)
!
      DSUM2=0.0D0
      DSUM4=0.0D0
      DO 1000 IGRP=1,NDIST
        NTEMP=0
        AHOLD=XDIST(IGRP)
        DSUM1=0.0D0
        DSUM3=0.0D0
        DO 1010 I=1,N
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
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RRAT')THEN
          WRITE(ICOUT,1019)IGRP,NTEMP,DSUM1,DSUM3
 1019     FORMAT('IGRP,NTEMP,DSUM1,DSUM3 = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 1000 CONTINUE
      ETASQ=REAL(DSUM2/DSUM4)
      ETA=DSQRT(DSUM2/DSUM4)
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
        IF(ICASE.EQ.'CRAT')THEN
          WRITE(ICOUT,8011)N,ETA
 8011     FORMAT('THE CORRELATION RATIO OF THE ',I8,   &
               ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,8013)N,ETASQ
 8013     FORMAT('THE INTRACLASS CORRELATION OF THE ',I8,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RRAT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CORRAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NDIST,ETA
 9012   FORMAT('IERROR,NDIST,ETA = ',A4,2X,I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DN,DMEAN1,DMEAN2,DSUM12
 9014   FORMAT('DN,DMEAN1,DMEAN2,DSUM12 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CORRAT
      real function correc(i,n)
!
!     calculates correction for tail area of the i-th largest of n
!     order statistics.
!
      dimension c1(7),c2(7),c3(7)
      real mic
      data c1/9.5, 28.7, 1.9, 0., -7.0, -6.2, -1.6/,   &
       c2/-6195., -9569., -6728., -17614., -8278., -3570., 1075./,   &
       c3/9.338e4, 1.7516e5, 4.1040e5, 2.1576e6, 2.376e6, 2.065e6,   &
       2.065e6/, mic/1.e-6/, c14/1.9e-5/
!
      correc = c14
      if(i*n.eq.4) return
      correc = 0.0
      if(i.lt.1.or.i.gt.7) return
      if(i.ne.4.and.n.gt.20) return
      if(i.eq.4.and.n.gt.40) return
      an = n
      an = 1.0/(an*an)
      correc = (c1(i) + an*(c2(i) + an*c3(i)))*mic
      return
      end
      SUBROUTINE COSCDF(X,CDF)
!
!     NOTE--COSINE CDF IS:
!              COSCDF(X) = (PI + X + SIN(X))/(2*PI),  -PI<=X<=PI
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI/3.1415926535898E0/
!
!-----START POINT-----------------------------------------------------
!
      CDF=0.0
      IF(X.LT.-PI)THEN
        CDF=0.0
      ELSEIF(X.GT.PI)THEN
        CDF=1.0
      ELSE
        CDF=(PI + X + SIN(X))/(2*PI)
      ENDIF
!
      RETURN
      END SUBROUTINE COSCDF
      SUBROUTINE COSDIS(X,Y,N,IWRITE,ICASE,STATVA,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE COSINE DISTANCE BETWEEN THE
!              TWO SETS OF DATA IN THE INPUT VECTORS X AND Y.  THE
!              SAMPLE COSINE DISTANCE WILL BE A SINGLE PRECISION VALUE
!              CALCULATED AS:
!
!                 SIMLARITY = SUM[i=1 to n][X(i)*Y(i)]/
!                             {SQRT(SUM{i=1 to n][X(i)**2])*
!                             SQRT(SUM{i=1 to n][Y(i)**2])}
!
!                 DISTANCE = 1 - SIMILARITY
!
!              THE ABOVE DISTANCE IS FOR POSITIVE VECTORS.  NOTE
!              THAT THIS DISTANCE IS NOT A PROPER DISTANCE IN THAT
!              THE SCHWARTZ INEQUALITY DOES NOT HOLD.  HOWEVER, THE
!              ANGULAR VERSIONS (FOR POSITIVE VECTORS) ARE PROPER
!              DISTANCES:
!
!                 ANGULAR DISTANCE = (1/COSINE SIMILARITY)/PI
!                 ANGULAR SIMILARITY = 1 - DISTANCE
!
!              2018/08: UPDATED FORMULAS
!
!                 ANGULAR DISTANCE = COS^(-1)(COSINE SIMILARITY)/PI
!                 ANGULAR SIMILARITY = 1 - DISTANCE
!
!              IF NEGATIVE DATA IS ENCOUNTERED IN THE INPUT
!              VECTORS, ONLY THE COSINE SIMILARITY IS COMPUTED.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE FIRST SET OF DATA.
!                     --Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS WHICH
!                                CONSTITUTE THE SECOND SET OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS X AND Y.
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE COSINE DISTANCE
!                                BETWEEN THE TWO SETS OF DATA IN THE
!                                INPUT VECTORS X AND Y.  THIS SINGLE
!                                PRECISION VALUE WILL BE BETWEEN 0.0
!                                AND 1.0 (INCLUSIVELY).
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COSINE DISTANCE BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHN FOREMAN (2014), "DATA SMART", WILEY.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/03
!     ORIGINAL VERSION--MARCH     2017.
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
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DTERM1
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI/3.14159265358979/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COSD'
      ISUBN2='IS  '
      IERROR='NO'
      COSSIM=CPUMIN
      COSDST=CPUMIN
      ANGSIM=CPUMIN
      ANGDIS=CPUMIN
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
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
        GO TO 9000
      ENDIF
!
      IF(N.EQ.1)THEN
        STATVA=1.0
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE COSINE DISTANCE.              **
!               ************************************************
!
      IFLAG=1
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO 200 I=1,N
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
        GO TO 9000
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
!
      IF(ICASE.EQ.'COSS')THEN
        STATVA=COSSIM
      ELSEIF(ICASE.EQ.'COSD')THEN
        STATVA=COSDST
      ELSEIF(ICASE.EQ.'ACOS')THEN
        STATVA=ANGSIM
      ELSEIF(ICASE.EQ.'ACOD')THEN
        STATVA=ANGDIS
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
        IF(ICASE.EQ.'COSD')THEN
          WRITE(ICOUT,811)N,STATVA
  811     FORMAT('THE COSINE DISTANCE OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'COSS')THEN
          WRITE(ICOUT,813)N,STATVA
  813     FORMAT('THE COSINE SIMILARITY OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'ANGS')THEN
          WRITE(ICOUT,815)N,STATVA
  815     FORMAT('THE ANGULAR COSINE SIMILARITY OF THE ',I8,   &
                 ' OBSERVATIONS = ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASE.EQ.'ANGD')THEN
          WRITE(ICOUT,817)N,STATVA
  817     FORMAT('THE ANGULAR COSINE DISTANCE OF THE ',I8,   &
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
!
      RETURN
      END SUBROUTINE COSDIS
      SUBROUTINE COSPDF(X,PDF)
!
!     NOTE--COSINE PDF IS:
!              COSPDF(X) = (1 + COS(X))/(2*PI),  -PI<=X<=PI
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI/3.1415926535898E0/
!
!-----START POINT-----------------------------------------------------
!
      PDF=0.0
      IF(X.LT.-PI .OR. X.GT.PI)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  301 FORMAT('***** ERROR--THE INPUT ARGUMENT TO COSPDF IS NOT IN THE ',   &
             'INTERVAL (-PI,PI).')
  302 FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      PDF=(1.0 + COS(X))/(2*PI)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE COSPDF
      SUBROUTINE COSPPF(P,PPF)
!
!     NOTE--ALGORITHM ADDED APRIL 1995 (ALAN)
!           USE A BISECTION METHOD
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI/3.1415926535898E0/
      DATA EPS /1.0E-10/
      DATA SIG /1.0E-10/
      DATA ZERO /0./
      DATA MAXIT /500/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO COSPPF IS OUTSIDE',   &
               ' THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        RETURN
      ENDIF
!
      IERR=0
      IC = 0
      IF(P.LE.0.0)THEN
        PPF=-PI
        GO TO 9999
      ELSEIF(P.GE.1.0)THEN
        PPF=PI
        GO TO 9999
      ENDIF
!
      XL = -PI
      XR = PI
      FXL = -P
      FXR = 1.0 - P
!CCCC INVALID P EXPLICITLY CHECKED FOR EARLIER.
!CCCC IF(FXL*FXR .GT. ZERO)GO TO 50
!
!  BISECTION METHOD
!
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL COSCDF(X,CDF)
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
  130 FORMAT('***** ERROR--COSPPF ROUTINE DID NOT CONVERGE. ***')
      GO TO 9999
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE COSPPF
      SUBROUTINE COSRAN(N,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE COSINE DISTRIBUTION
!              F(X) = 0.5*EXP(-ABS(X)).
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE COSINE DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/10
!     ORIGINAL VERSION--OCTOBER   2001.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO COSRAN IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N COSINE RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
        CALL COSPPF(X(I),XTEMP)
        X(I)=XTEMP
  100 CONTINUE
!
      RETURN
      END SUBROUTINE COSRAN
      SUBROUTINE COSTRA(Y1,N1,IWRITE,Y2,N2,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE COSINE TRANSFORM OF A VARIABLE--
!            = THE COEFFICIENTS OF THE COSINE TERM
!              IN THE FINITE FOURIER RESPRESENTATION OF THE DATA IN Y1.
!              Y2(1) = A0 = MEAN
!              Y2(2) = A1
!              Y2(3) = A2
!              ETC.
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y2(.)
!           BEING IDENTICAL TO THE INPUT VECTOR Y1(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--85/1
!     ORIGINAL VERSION--DECEMBER  1984.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------
!
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DN1
      DOUBLE PRECISION DDEL
      DOUBLE PRECISION DI
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DK
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DY1K
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COST'
      IERROR='NO'
!
      N1HALF=(-999)
      IMAX=(-999)
      IEVODD=(-999)
      DDEL=(-999.0D0)
      DN1=(-999.0D0)
!
      DN1=N1
!
      DPI=3.14159265358979D0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF COSTRA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,IWRITE,N1
   53   FORMAT('IBUGA3,IWRITE,N1 = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  COMPUTE COSINE TRANSFORM.    **
!               ***********************************
!
      IF(N1.LT.1)GO TO 1100
      GO TO 1190
!
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
 1157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1190 CONTINUE
!
      N1HALF=N1/2
      N1HALP=N1HALF+1
      IMAX=N1HALP
      IEVODD=N1-2*(N1/2)
      DDEL=(DN1+1.0D0)/2.0D0
      IF(IEVODD.EQ.0)DDEL=(DN1+2.0D0)/2.0D0
!
      J=0
      J=J+1
      DSUM=0.0
      DO 1205 K=1,N1
      DY1K=Y1(K)
      DSUM=DSUM+DY1K
 1205 CONTINUE
      COEF=DSUM/DN1
      Y2(J)=COEF
!
      DO 1210 IP1=2,IMAX
      J=J+1
      I=IP1-1
      DI=I
!CCCC FREQI=DI/DN1
      DSUM=0.0D0
!
      DO 1220 K=1,N1
      DK=K
      DOMEGA=2.0*DPI*(DI/DN1)
      DY1K=Y1(K)
      DSUM=DSUM+DY1K*DCOS(DOMEGA*(DK-DDEL))
 1220 CONTINUE
      COEF=DSUM/DN1
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,1221)J,I,DN1,DI,COEF
 1221 FORMAT('J,I,DN1,DI,COEF = ',I8,I8,2D15.7,E15.7)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
!
      Y2(J)=COEF
!
 1210 CONTINUE
!
      N2=J
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
 9011 FORMAT('***** AT THE END       OF COSTRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N1,N2,N1HALF,IMAX,IEVODD,DDEL
 9013 FORMAT('N1,N2,N1HALF,IMAX,IEVODD,DDEL = ',5I8,D15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,N1
      WRITE(ICOUT,9016)I,Y1(I),Y2(I)
 9016 FORMAT('I,Y1(I),Y2(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE COSTRA
      SUBROUTINE COV(X,Y,N,IWRITE,XYCOV,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE COVARIANCE COEFFICIENT
!              BETWEEN THE 2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
!              THE SAMPLE COVARIANCE COEFFICIENT WILL BE A SINGLE
!              PRECISION VALUE CALCULATED AS THE
!              SUM OF CROSS PRODUCTS DIVIDED BY (N-1).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --Y      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE SECOND SET
!                                OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--XYCOV  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE COVARIANCE COEFFICIENT
!                                BETWEEN THE 2 SETS OF DATA
!                                IN THE INPUT VECTORS X AND Y.
!                                THIS SINGLE PRECISION VALUE
!                                WILL BE BETWEEN -1.0 AND 1.0
!                                (INCLUSIVELY).
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE COVARIANCE COEFFICIENT BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KENDALL AND STUART, THE ADVANCED THEORY OF
!                 STATISTICS, VOLUME 1, EDITION 2, 1963, PAGES 235-236.
!               --KENDALL AND STUART, THE ADVANCED THEORY OF
!                 STATISTICS, VOLUME 2, EDITION 1, 1961, PAGES 292-293.
!               --SNEDECOR AND COCHRAN, STATISTICAL METHODS,
!                 EDITION 6, 1967, PAGES 172-198.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1979.
!     UPDATED         --JUNE      1979.
!     UPDATED         --JULY      1979.
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
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM12
      DOUBLE PRECISION DMEAN1
      DOUBLE PRECISION DMEAN2
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='COV '
      ISUBN2='    '
!
      IERROR='NO'
!
      DN=0.0D0
      DMEAN1=0.0D0
      DMEAN2=0.0D0
      DSUM12=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I),Y(I)
   56 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *******************************************
!               **  COMPUTE     COVARIANCE  COEFFICIENT  **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.GE.1)GO TO 119
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
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',   &
      'THE THIRD INPUT ARGUMENT (N) HAS THE VALUE 1')
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
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
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',   &
      'THE FIRST  INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
      GO TO 9000
  139 CONTINUE
!
      HOLD=Y(1)
      DO 145 I=2,N
      IF(Y(I).NE.HOLD)GO TO 149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,146)HOLD
  146 FORMAT('***** NON-FATAL DIAGNOSTIC IN COV--',   &
      'THE SECOND INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XYCOV=0.0
      GO TO 9000
  149 CONTINUE
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE     COVARIANCE  COEFFICIENT.  **
!               ************************************************
!
      DN=N
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 200 I=1,N
      DX1=X(I)
      DX2=Y(I)
      DSUM1=DSUM1+DX1
      DSUM2=DSUM2+DX2
  200 CONTINUE
      DMEAN1=DSUM1/DN
      DMEAN2=DSUM2/DN
!
      DSUM12=0.0D0
      DO 300 I=1,N
      DX1=X(I)
      DX2=Y(I)
      DSUM12=DSUM12+(DX1-DMEAN1)*(DX2-DMEAN2)
  300 CONTINUE
      XYCOV=DSUM12/(DN-1.0D0)
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
      WRITE(ICOUT,811)N,XYCOV
  811 FORMAT('THE COVARIANCE COEFFICIENT OF THE ',I8,   &
      ' OBSERVATIONS = ',E15.7)
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
!
      RETURN
      END SUBROUTINE COV
      SUBROUTINE COVMAT(YM1,YM9,DMEAN,MAXROM,NR,NC,MAXVAR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE VARIANCE-COVARIANCE
!              MATRIX.  THIS IS A UTILITY ROUTINE, ERROR CHECKING
!              PERFORMED BY CALLING ROUTINES.
!     INPUT  ARGUMENTS--YM1    = THE SINGLE PRECISION MATRIX OF
!                                OBSERVATIONS
!                     --NR     = THE INTEGER NUMBER OF ROWS
!                     --NC     = THE INTEGER NUMBER OF COLUMNS
!                     --MAXROM = LEADING DIMENSION OF XMAT, COVMAT
!     OUTPUT ARGUMENTS--YM9    = THE SINGLE PRECISION MATRIX WHICH
!                                WILL CONTAIN THE COVARIANCE MATRIX
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE VARIANCE-COVARIANCE MATRIX.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DNR
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DCOV
      DOUBLE PRECISION DMEAN(*)
!
      DIMENSION YM1(MAXROM,NC)
      DIMENSION YM9(MAXVAR,MAXVAR)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DNR=DBLE(NR)
!
      DO 5111 J=1,NC
        DSUM1=0.0D0
        DO 5112 I=1,NR
          DYM1=YM1(I,J)
          DSUM1=DSUM1+DYM1
 5112   CONTINUE
        DMEAN(J)=-9999.0D0
        DDENOM=DNR
        IF(DDENOM.NE.0.0D0)DMEAN(J)=REAL(DSUM1/DDENOM)
 5111 CONTINUE
!
      DO 5121 J=1,NC
        DO 5122 K=J,NC
          DSUM1=0.0D0
          DO 5123 I=1,NR
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      RETURN
      END SUBROUTINE COVMAT
      SUBROUTINE CP(X,N,ENGLSL,ENGUSL,IWRITE,XCP,XLCL,XUCL,   &
                   IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CP (PROCESS
!              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
!              CP = (ENGUSL - ENGLSL) / 6*S
!     NOTE--IF THE TARGET VALUE IS MIDWAY BETWEEN ENGUSL AND ENGLSL,
!           THEN AN ALTERNATIVE EQUIVALENT DEFINITION FOR CP IS
!              CP = (ENGUSL-TARGET) / 3*S
!     NOTE--CP IS A MEASURE OF PROCESS PRECISION--
!           IT CONTAINS NO BIAS INFORMATION.
!     NOTE--THE CP INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CP WHICH ARE
!           LARGE (ABOVE 2); VALUES OF CP FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CP     = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CP
!                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
!                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CP INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--R&M 2000 AIRFORCE MANUAL
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89.5
!     ORIGINAL VERSION--MAY       1989.
!     UPDATED         --SEPTEMBER 1990. REVERSE INPUT ARGS
!     UPDATED         --APRIL     2001. ADD LOWER AND UPPER 95%
!                                       CONFIDENCE INTERVAL.
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCP
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CP  '
      ISUBN2='    '
      IERROR='NO'
!
      XCP=0.0
      DMEAN=0.0D0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CP  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CP RATIO                        **
!               **************************************************
!
      DUSL=ENGUSL
      DLSL=ENGLSL
!
      DNUM=DUSL-DLSL
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
!
      DDEN=6.0*DSD
!
      DCP=0.0
      IF(DDEN.GT.0.0D0)DCP=DNUM/DDEN
      XCP=DCP
!
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
        WRITE(ICOUT,811)N,XCP
  811   FORMAT('THE CP OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CP
      SUBROUTINE CPEVL(N,M,A,Z,C,B,KBD)
!***BEGIN PROLOGUE  CPEVL
!***REFER TO  CPZERO
!
!        Evaluate a complex polynomial and its derivatives.
!        Optionally compute error bounds for these values.
!
!   INPUT...
!        N = Degree of the polynomial
!        M = Number of derivatives to be calculated,
!            M=0 evaluates only the function
!            M=1 evaluates the function and first derivative, etc.
!             if M .GT. N+1 function and all N derivatives will be
!                calculated.
!       A = Complex vector containing the N+1 coefficients of polynomial
!               A(I)= coefficient of Z**(N+1-I)
!        Z = Complex point at which the evaluation is to take place.
!        C = Array of 2(M+1) words into which values are placed.
!        B = Array of 2(M+1) words only needed if bounds are to be
!              calculated.  It is not used otherwise.
!        KBD = A logical variable, e.g. .TRUE. or .FALSE. which is
!              to be set .TRUE. if bounds are to be computed.
!
!  OUTPUT...
!        C =  C(I+1) contains the complex value of the I-th
!              derivative at Z, I=0,...,M
!        B =  B(I) contains the bounds on the real and imaginary parts
!              of C(I) if they were requested.
!***ROUTINES CALLED  I1MACH
!***END PROLOGUE  CPEVL
!
      COMPLEX A(1),C(1),Z,CI,CIM1,B(1),BI,BIM1,T,ZA,Q
      LOGICAL KBD
!
      INCLUDE 'DPCOMC.INC'
!
      DATA NBITS /0/
      ZA(Q)=CMPLX(ABS(REAL(Q)),ABS(AIMAG(Q)))
!***FIRST EXECUTABLE STATEMENT  CPEVL
      IF ( NBITS .EQ. 0 ) NBITS = I1MACH (11)
      D1=2.**(1-NBITS)
      NP1=N+1
      DO 1 J=1,NP1
         CI=0.0
         CIM1=A(J)
         BI=0.0
         BIM1=0.0
         MINI=MIN0(M+1,N+2-J)
            DO 11 I=1,MINI
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
   11       CONTINUE
    1 CONTINUE
      RETURN
      END SUBROUTINE CPEVL
      SUBROUTINE CPK(X,N,ENGLSL,ENGUSL,IWRITE,XCPK,XLCL,XUCL,   &
                     IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPK (PROCESS
!              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
!
!                 CPK = MIN(USL-MEAN,MEAN-LSL)/(3*S)
!
!     NOTE--CPK IS A MEASURE OF PROCESS ACCURACY--
!           COMBINING BOTH PRECISION AND UNBIASEDNESS.
!     NOTE--THE CPK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPK WHICH ARE
!           LARGE (ABOVE 2); VALUES OF CPK FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CPK    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPK
!                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
!                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CPK INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--R&M 2000 AIR FORCE MANUAL
!               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89.5
!     ORIGINAL VERSION--MAY       1989.
!     UPDATED         --SEPTEMBER 1990. REVERSE INPUT ARGS
!     UPDATED         --APRIL     2001. 95% CONFIDENCE LIMITS
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPK
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPK '
      ISUBN2='    '
      IERROR='NO'
!
      XCPK=0.0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPK  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CPK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPK RATIO                       **
!               **************************************************
!
      DUSL=ENGUSL
      DLSL=ENGLSL
!
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
!
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
!
      DDEN=3.0*DSD
!
      DCPK=0.0
      IF(DDEN.GT.0.0D0)DCPK=DNUM/DDEN
      XCPK=DCPK
!
      AN=REAL(N)
      P=0.975
      TERM1=1.0/(9.0*AN)
      TERM2=XCPK*XCPK/(2.0*(AN-1.0))
      CALL NORPPF(P,PPF)
      XLCL=XCPK - PPF*SQRT(TERM1 + TERM2)
      XUCL=XCPK + PPF*SQRT(TERM1 + TERM2)
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
        WRITE(ICOUT,811)N,XCPK
  811   FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CPK
      SUBROUTINE CPKM(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPKM,XLCL,XUCL,   &
                      IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPKM (PROCESS
!              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
!
!                 CPKM = MIN(USL-MEAN,MEAN-LSL)/
!                        {3*SQRT(S**2 +(MEAN-TARGET)**2)}
!
!     NOTE--CPKM IS A MEASURE OF PROCESS ACCURACY--
!           COMBINING BOTH PRECISION AND UNBIASEDNESS.
!     NOTE--THE CPKM INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPKM WHICH ARE
!           LARGE (ABOVE 2); VALUES OF CPKM FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!                     --TARGET = TARGET VALUE (ENGINEERING)
!     OUTPUT ARGUMENTS--CPKM   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPKM
!                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
!                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CPKM INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.4
!     ORIGINAL VERSION--APRIL     2015.
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPKM
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPKM'
      ISUBN2='    '
      IERROR='NO'
!
      XCPKM=0.0
      XCL=CPUMIN
      XUL=CPUMIN
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPKM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL,XLCL,XUCL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL,XLCL,XUCL = ',A4,2X,I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPKM  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CPKM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPKM RATIO                      **
!               **************************************************
!
      DUSL=DBLE(ENGUSL)
      DLSL=DBLE(ENGLSL)
      DTARG=DBLE(TARGET)
!
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
!
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
!
      DDEN=3.0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
!
      DCPKM=0.0
      IF(DDEN.GT.0.0D0)DCPKM=DNUM/DDEN
      XCPKM=DCPKM
!
!     FOLLOWING CONFIDENCE INTERVALS ARE FOR CPK.  HAVEN'T FOUND
!     A SOURCE FOR CPKM CONFIDENCE INTERVALS.
!
!CCCC AN=REAL(N)
!CCCC P=0.975
!CCCC TERM1=1.0/(9.0*AN)
!CCCC TERM2=XCPKM*XCPK/(2.0*(AN-1.0))
!CCCC CALL NORPPF(P,PPF)
!CCCC XLCL=XCPKM - PPF*SQRT(TERM1 + TERM2)
!CCCC XUCL=XCPKM + PPF*SQRT(TERM1 + TERM2)
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
        WRITE(ICOUT,811)N,XCPKM
  811   FORMAT('THE CPKM OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CPKM
      SUBROUTINE CPL(X,N,ENGLSL,ENGUSL,IWRITE,XCPL,XLCL,XUCL,   &
                     IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE CPL (PROCESS CAPABILITY INDEX)
!              OF THE DATA IN THE INPUT VECTOR X.
!              CPL = NUMERATOR/DENOMINATOR
!              WHERE NUMERATOR = XBAR - LOWER SPEC LIMIT
!              AND DENOMINATOR = 3 * SIGMA
!     NOTE--CPL IS A VARIATION OF CPL WHEN YOU ARE ONLY
!           INTERESTED IN THE LOWER SPEC LIMIT.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CPL    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPL
!                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
!                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CPL INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--R&M 2000 AIR FORCE MANUAL
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001.4
!     ORIGINAL VERSION--APRIL     2001.
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPL
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPL '
      ISUBN2='    '
!
      IERROR='NO'
!
      DMEAN=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPL  **
!               ********************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.GE.1)GO TO 119
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
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
      XSD=0.0
      GO TO 9000
  129 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      XSD=0.0
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPL RATIO                       **
!               **************************************************
!
      DUSL=ENGUSL
      DLSL=ENGLSL
!
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
!
      DNUM=DLOWER
!
      DDEN=3.0*DSD
!
      DCPL=0.0D0
      IF(DDEN.GT.0.0D0)DCPL=DNUM/DDEN
      XCPL=DCPL
!
      AN=REAL(N)
      P=0.975
      CALL NORPPF(P,PPF)
      XLCL=0.0
      XUCL=0.0
      IF(N.GT.1)THEN
        XLCL=XCPL - PPF*SQRT((1.0/(9.0*AN)) + XCPL/(2.0*(AN-1.0)))
        XUCL=XCPL + PPF*SQRT((1.0/(9.0*AN)) + XCPL/(2.0*(AN-1.0)))
      ENDIF
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
      WRITE(ICOUT,811)N,XCPL
  811 FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',   &
      E15.7)
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
!
      RETURN
      END SUBROUTINE CPL
      SUBROUTINE CPM(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPM,XLCL,XUCL,   &
                     IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPM (PROCESS
!              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
!
!                 CPM = (USL - LSL)/(6*SQRT(S**2+(XBAR-TARGET)**2))
!
!     NOTE--CPM IS A MEASURE OF PROCESS ACCURACY--
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!                     --TARGET = TARGET (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--XCPM   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPM
!                     --XLCL   = LOWER 95% CONFIDENCE INTERVAL
!                     --XUCL   = UPPER 95% CONFIDENCE INTERVAL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE SAMPLE CPM INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--MEAN AND SD.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NORMA HUBELE, ARIZONA STATE
!               --CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98.11
!     ORIGINAL VERSION--NOVEMBER  1998.
!     UPDATED         --APRIL     2001. ADD 95% CONFIDENCE LIMITS
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPM
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPM '
      ISUBN2='    '
      IERROR='NO'
!
      XCPM=0.0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPM  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CPM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XMEAN=DMEAN
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPM RATIO                       **
!               **************************************************
!
      DUSL=ENGUSL
      DLSL=ENGLSL
      DTARG=TARGET
!
      DNUM=DUSL-DLSL
      DDEN=6.0D0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
!
      DCPM=0.0
      IF(DDEN.GT.0.0D0)DCPM=DNUM/DDEN
      XCPM=DCPM
!
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
        WRITE(ICOUT,811)N,XCPM
  811   FORMAT('THE CPM OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CPM
      SUBROUTINE CPMK(X,N,ENGLSL,ENGUSL,TARGET,IWRITE,XCPMK,XLCL,XUCL,   &
                      IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE CPMK (PROCESS
!              CAPABILITY INDEX) OF THE DATA IN THE INPUT VECTOR X.
!
!                 CPMK = MIN(USL-MEAN,MEAN-LSL)/
!                        {3*SQRT(S**2 +(MEAN-TARGET)**2)}
!
!     NOTE--CPMK IS A MEASURE OF PROCESS ACCURACY--
!           COMBINING BOTH PRECISION AND UNBIASEDNESS.
!     NOTE--THE CPMK INDEX IS A MEASURE WHICH TAKES ON THE VALUES 0 TO
!           INFINITY.  A GOOD PROCESS YIELDS VALUES OF CPMK WHICH ARE
!           LARGE (ABOVE 2); VALUES OF CPMK FROM 0.5 TO 1.0 ARE TYPICAL.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!                     --TARGET = TARGET VALUE (ENGINEERING)
!     OUTPUT ARGUMENTS--CPMK   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPMK
!                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
!                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CPMK INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CHEN AND DING (2001), "A NEW PROCESS CAPABILITY
!                 INDEX FOR NON-NORMAL DISTRIBUTIONS", INTERNATIONAL
!                 JOURNAL OF QUALITY & RELIABILITY MANAGEMENT,
!                 VOL. 18, NO. 7, PP. 762-770.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.4
!     ORIGINAL VERSION--APRIL     2015.
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DTARG
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPMK
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPMK'
      ISUBN2='    '
      IERROR='NO'
!
      XCPMK=0.0
      XCL=CPUMIN
      XUL=CPUMIN
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,ENGUSL,ENGLSL,XLCL,XUCL
   52   FORMAT('IBUGA3,N,ENGUSL,ENGLSL,XLCL,XUCL = ',A4,2X,I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPMK  **
!               ********************************************
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
  111   FORMAT('***** ERROR IN CPMK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPMK RATIO                      **
!               **************************************************
!
      DUSL=DBLE(ENGUSL)
      DLSL=DBLE(ENGLSL)
      DTARG=DBLE(TARGET)
!
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
!
      DNUM=DUPPER
      IF(DLOWER.LT.DUPPER)DNUM=DLOWER
      IF(DNUM.LE.0.0D0)DNUM=0.0D0
!
      DDEN=3.0*DSQRT(DSD**2 + (DMEAN-DTARG)**2)
!
      DCPMK=0.0
      IF(DDEN.GT.0.0D0)DCPMK=DNUM/DDEN
      XCPMK=DCPMK
!
!     FOLLOWING CONFIDENCE INTERVALS ARE FOR CPK.  HAVEN'T FOUND
!     A SOURCE FOR CPMK CONFIDENCE INTERVALS.
!
!CCCC AN=REAL(N)
!CCCC P=0.975
!CCCC TERM1=1.0/(9.0*AN)
!CCCC TERM2=XCPMK*XCPK/(2.0*(AN-1.0))
!CCCC CALL NORPPF(P,PPF)
!CCCC XLCL=XCPMK - PPF*SQRT(TERM1 + TERM2)
!CCCC XUCL=XCPMK + PPF*SQRT(TERM1 + TERM2)
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
        WRITE(ICOUT,811)N,XCPMK
  811   FORMAT('THE CPMK OF THE ',I8,' OBSERVATIONS = ',G15.7)
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
!
      RETURN
      END SUBROUTINE CPMK
      COMPLEX FUNCTION CPSI(ZIN)
!***BEGIN PROLOGUE  CPSI
!***DATE WRITTEN   780501   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  C7C
!***KEYWORDS  COMPLEX,DIGAMMA FUNCTION,PSI FUNCTION,SPECIAL FUNCTION
!***AUTHOR  FULLERTON, W., (LANL)
!***PURPOSE  Computes the Psi function of complex argument.
!***DESCRIPTION
!
! PSI(X) calculates the psi (or digamma) function of X.  PSI(X)
! is the logarithmic derivative of the gamma function of X.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  CCOT,R1MACH,XERROR
!***END PROLOGUE  CPSI
      COMPLEX ZIN, Z, Z2INV, CORR,  CCOT, CLOG
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DIMENSION BERN(13)
      DATA BERN( 1) /   .83333333333333333E-1 /
      DATA BERN( 2) /  -.83333333333333333E-2 /
      DATA BERN( 3) /   .39682539682539683E-2 /
      DATA BERN( 4) /  -.41666666666666667E-2 /
      DATA BERN( 5) /   .75757575757575758E-2 /
      DATA BERN( 6) /  -.21092796092796093E-1 /
      DATA BERN( 7) /   .83333333333333333E-1 /
      DATA BERN( 8) /  -.44325980392156863E0 /
      DATA BERN( 9) /   .30539543302701197E1 /
      DATA BERN(10) /  -.26456212121212121E2 /
      DATA BERN(11) /   .28146014492753623E3 /
      DATA BERN(12) /  -.34548853937728938E4 /
      DATA BERN(13) /   .54827583333333333E5 /
      DATA PI / 3.141592653589793E0 /
      DATA NTERM, BOUND, DXREL, RMIN, RBIG / 0, 4*0.0 /
!***FIRST EXECUTABLE STATEMENT  CPSI
!
      CPSI = (0.0, 0.0)
!
      IF (NTERM.NE.0) GO TO 10
      NTERM = INT(-0.30*LOG(R1MACH(3)))
! MAYBE BOUND = N*(0.1*EPS)**(-1/(2*N-1)) / (PI*EXP(1))
      BOUND = 0.1171*FLOAT(NTERM) *   &
        (0.1*R1MACH(3))**(-1.0/(2.0*FLOAT(NTERM)-1.0))
      DXREL = SQRT(R1MACH(4))
      RMIN = EXP (AMAX1 (LOG(R1MACH(1)), -LOG(R1MACH(2))) + 0.011 )
      RBIG = 1.0/R1MACH(3)
!
 10   Z = ZIN
      X = REAL(Z)
      Y = AIMAG(Z)
      IF (Y.LT.0.0) Z = CONJG(Z)
!
      CORR = (0.0, 0.0)
      CABSZ = CABS(Z)
      IF (X.GE.0.0 .AND. CABSZ.GT.BOUND) GO TO 50
      IF (X.LT.0.0 .AND. ABS(Y).GT.BOUND) GO TO 50
!
      IF (CABSZ.LT.BOUND) GO TO 20
!
! USE THE REFLECTION FORMULA FOR REAL(Z) NEGATIVE, CABS(Z) LARGE, AND
! ABS(AIMAG(Y)) SMALL.
!
      CORR = -PI*CCOT(PI*Z)
      Z = 1.0 - Z
      GO TO 50
!
! USE THE RECURSION RELATION FOR CABS(Z) SMALL.
!
 20   IF (CABSZ.LT.RMIN) THEN
!CCCC   CALL XERROR ( 'CPSI    CPSI CALLED WITH Z SO NE
!CCCC1AR 0 THAT CPSI OVERFLOWS',      56, 2, 2)
        WRITE(ICOUT,102)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 102  FORMAT('***** INTERNAL ERROR FROM CPSI: ARGUMENT SO CLOSE',   &
      ' TO ZERO THAT CPSI OVERFLOWS')
!
      IF (X.GE.(-0.5) .OR. ABS(Y).GT.DXREL) GO TO 30
      IF (CABS((Z-AINT(X-0.5))/X).LT.DXREL) THEN
!CCCC   CALL XERROR ( 'CPSI    ANSWE
!CCCC1R LT HALF PRECISION BECAUSE Z TOO NEAR NEGATIVE INTEGER', 68, 1, 1
!CCCC2)
        WRITE(ICOUT,202)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 202  FORMAT('***** INTERNAL ERROR FROM CPSI: ANSWER LESS THAN HALF',   &
      ' PRECISION BECAUSE ARGUMENT TOO NEAR A NEGATIVE INTEGER')
      IF (Y.EQ.0.0 .AND. X.EQ.AINT(X)) THEN
!CCCC   CALL XERROR ( 'CPSI    Z IS A NEG
!CCCC1ATIVE INTEGER', 31, 3, 2)
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
 302  FORMAT('***** INTERNAL ERROR FROM CPSI: ARGUMENT IS A ',   &
      ' NEGATIVE INTEGER')
!
 30   N = INT(SQRT(BOUND**2-Y**2) - X + 1.0)
      DO 40 I=1,N
        CORR = CORR - 1.0/Z
        Z = Z + 1.0
 40   CONTINUE
!
! NOW EVALUATE THE ASYMPTOTIC SERIES FOR SUITABLY LARGE Z.
!
 50   IF (CABSZ.GT.RBIG) CPSI = CLOG(Z) + CORR
      IF (CABSZ.GT.RBIG) GO TO 70
!
      CPSI = (0.0, 0.0)
      Z2INV = 1.0/Z**2
      DO 60 I=1,NTERM
        NDX = NTERM + 1 - I
        CPSI = BERN(NDX) + Z2INV*CPSI
 60   CONTINUE
      CPSI = CLOG(Z) - 0.5/Z - CPSI*Z2INV + CORR
!
 70   IF (Y.LT.0.0) CPSI = CONJG(CPSI)
!
      RETURN
      END FUNCTION CPSI
      SUBROUTINE CPU(X,N,ENGLSL,ENGUSL,IWRITE,XCPU,XLCL,XUCL,   &
                     IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE CPU (PROCESS CAPABILITY INDEX)
!              OF THE DATA IN THE INPUT VECTOR X.
!              CPU = NUMERATOR/DENOMINATOR
!              WHERE NUMERATOR = XBAR + UPPER SPEC LIMIT
!              AND DENOMINATOR = 3 * SIGMA
!     NOTE--CPU IS A VARIATION OF CPK WHEN YOU ARE ONLY
!           INTERESTED IN THE UPPER SPEC LIMIT.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --ENGLSL = LOWER (ENGINEERING) SPEC LIMIT
!                     --ENGUSL = UPPER (ENGINEERING) SPEC LIMIT
!     OUTPUT ARGUMENTS--CPU    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE CPU
!                     --XLCL   = LOWER 95% CONFIDENCE LEVEL
!                     --XUCL   = UPPER 95% CONFIDENCE LEVEL
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CPU INDEX
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--R&M 2000 AIR FORCE MANUAL
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001.4
!     ORIGINAL VERSION--APRIL     2001.
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
      DOUBLE PRECISION DSD
!
      DOUBLE PRECISION DUSL
      DOUBLE PRECISION DLSL
      DOUBLE PRECISION DUPPER
      DOUBLE PRECISION DLOWER
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDEN
      DOUBLE PRECISION DCPU
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CPU '
      ISUBN2='    '
      IERROR='NO'
!
      DMEAN=0.0D0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
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
      DO 55 I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  COMPUTE PROCESS CAPABILITY INDEX CPU  **
!               ********************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.GE.1)GO TO 119
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
  117 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
      XSD=0.0
      GO TO 9000
  129 CONTINUE
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      XSD=0.0
      GO TO 9000
  139 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  COMPUTE THE STANDARD DEVIATION.  **
!               ***************************************
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
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XSD=DSD
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE THE CPU RATIO                       **
!               **************************************************
!
      DUSL=ENGUSL
      DLSL=ENGLSL
!
      DUPPER=DUSL-DMEAN
      DLOWER=DMEAN-DLSL
!
      DNUM=DUPPER
!
      DDEN=3.0*DSD
!
      DCPU=0.0D0
      IF(DDEN.GT.0.0D0)DCPU=DNUM/DDEN
      XCPU=DCPU
!
      AN=REAL(N)
      P=0.975
      CALL NORPPF(P,PPF)
      XLCL=0.0
      XUCL=0.0
      IF(N.GT.1)THEN
        XLCL=XCPU - PPF*SQRT((1.0/(9.0*AN)) + XCPU/(2.0*(AN-1.0)))
        XUCL=XCPU + PPF*SQRT((1.0/(9.0*AN)) + XCPU/(2.0*(AN-1.0)))
      ENDIF
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
      WRITE(ICOUT,811)N,XCPU
  811 FORMAT('THE CPK OF THE ',I8,' OBSERVATIONS = ',   &
      E15.7)
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
!
      RETURN
      END SUBROUTINE CPU
      SUBROUTINE CPZERO(IN,A,R,T,IFLG,S)
!***BEGIN PROLOGUE  CPZERO
!***DATE WRITTEN   810223   (YYMMDD)
!***REVISION DATE  860227   (YYMMDD)
!***CATEGORY NO.  F1A1B
!***KEYWORDS  COMPLEX,POLYNOMIAL ROOTS,ROOTS,ZEROES,ZEROS
!***AUTHOR  KAHANER, D. K., (NBS)
!***PURPOSE  Find the zeros of a polynomial with complex coefficients.
!***DESCRIPTION
!
!      Find the zeros of the complex polynomial
!         P(Z)= A(1)*Z**N + A(2)*Z**(N-1) +...+ A(N+1)
!
!    Input...
!       IN = degree of P(Z)
!       A = complex vector containing coefficients of P(Z),
!            A(I) = coefficient of Z**(N+1-i)
!       R = N word complex vector containing initial estimates for zeros
!            if these are known.
!       T = 4(N+1) word array used for temporary storage
!       IFLG = flag to indicate if initial estimates of
!              zeros are input.
!            If IFLG .EQ. 0, no estimates are input.
!            If IFLG .NE. 0, the vector R contains estimates of
!               the zeros
!       ** WARNING ****** If estimates are input, they must
!                         be separated, that is, distinct or
!                         not repeated.
!       S = an N word array
!
!    Output...
!       R(I) = Ith zero,
!       S(I) = bound for R(I) .
!       IFLG = error diagnostic
!    Error Diagnostics...
!       If IFLG .EQ. 0 on return, all is well
!       If IFLG .EQ. 1 on return, A(1)=0.0 or N=0 on input
!       If IFLG .EQ. 2 on return, the program failed to coverge
!                after 25*N iterations.  Best current estimates of the
!                zeros are in R(I).  Error bounds are not calculated.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  CPEVL
!***END PROLOGUE  CPZERO
!
!CCCC APRIL 1996.  MAKE DUMMY DIMENSION "*"
!CCCC REAL  S(1)
!CCCC COMPLEX R(1),T(1),A(1),PN,TEMP
      REAL  S(*)
      COMPLEX R(*),T(*),A(*),PN,TEMP,PNTEMP(1),TEMP2(1)
!***FIRST EXECUTABLE STATEMENT  CPZERO
      IF( IN .LE. 0 .OR. CABS(A(1)) .EQ. 0.0 ) GO TO 30
!
!       CHECK FOR EASILY OBTAINED ZEROS
!
      N=IN
      N1=N+1
      IF(IFLG .NE. 0) GO TO 14
    1 CONTINUE
      N1=N+1
      IF(N .GT. 1) GO TO 2
         R(1)=-A(2)/A(1)
         S(1)=0.0
         RETURN
    2 CONTINUE
         IF( CABS(A(N1)) .NE. 0.0 ) GO TO 3
         R(N)=0.0
         S(N)=0.0
         N=N-1
         GO TO 1
!
!          IF INITIAL ESTIMATES FOR ZEROS NOT GIVEN, FIND SOME
!
    3 CONTINUE
      TEMP=-A(2)/(A(1)*FLOAT(N))
      CALL CPEVL(N,N,A,TEMP,T,T,.FALSE.)
      IMAX=N+2
      T(N1)=CABS(T(N1))
      DO 6 I=2,N1
         T(N+I)=-CABS(T(N+2-I))
         IF(REAL(T(N+I)) .LT. REAL(T(IMAX))) IMAX=N+I
    6 CONTINUE
      X=(-REAL(T(IMAX))/REAL(T(N1)))**(1./FLOAT(IMAX-N1))
    7 CONTINUE
         X=2.*X
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PNTEMP,PNTEMP,.FALSE.)
         PN=PNTEMP(1)
      IF (REAL(PN).LT.0.) GO TO 7
      U=.5*X
      V=X
   10 CONTINUE
         X=.5*(U+V)
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PNTEMP,PNTEMP,.FALSE.)
         PN=PNTEMP(1)
         IF (REAL(PN).GT.0.) V=X
         IF (REAL(PN).LE.0.) U=X
         IF((V-U) .GT. .001*(1.+V)) GO TO 10
      DO 13 I=1,N
         U=(3.14159265/FLOAT(N))*(.5+2.*FLOAT(I-1))
         R(I)=AMAX1(X,.001*CABS(TEMP))*CMPLX(COS(U),SIN(U))+TEMP
   13 CONTINUE
!
!          MAIN ITERATION LOOP STARTS HERE
!
   14 CONTINUE
      NR=0
      NMAX=25*N
      DO 19 NIT=1,NMAX
         DO 18 I=1,N
            IF(NIT .NE. 1 .AND. CABS(T(I)) .EQ. 0.) GO TO 18
               CALL CPEVL(N,0,A,R(I),PNTEMP,TEMP2,.TRUE.)
               PN=PNTEMP(1)
               TEMP=TEMP2(1)
               IF(ABS(REAL(PN))+ABS(AIMAG(PN)) .GT. REAL(TEMP)+   &
                    AIMAG(TEMP)) GO TO 16
                  T(I)=0.0
                  NR=NR+1
                  GO TO 18
   16          TEMP=A(1)
               DO 17 J=1,N
                  IF(J .NE. I) TEMP=TEMP*(R(I)-R(J))
   17          CONTINUE
               T(I)=PN/TEMP
   18    CONTINUE
         DO 15 I=1,N
            R(I)=R(I)-T(I)
   15    CONTINUE
         IF(NR .EQ. N) GO TO 21
   19 CONTINUE
      GO TO 26
!
!          CALCULATE ERROR BOUNDS FOR ZEROS
!
   21 DO 25 NR=1,N
         CALL CPEVL(N,N,A,R(NR),T,T(N+2),.TRUE.)
         X=CABS(CMPLX(ABS(REAL(T(1))),ABS(AIMAG(T(1))))+T(N+2))
         S(NR)=0.0
         DO 23 I=1,N
            X=X*FLOAT(N1-I)/FLOAT(I)
            TEMP=CMPLX(AMAX1(ABS(REAL(T(I+1)))-REAL(T(N1+I)),0.0),   &
                 AMAX1(ABS(AIMAG(T(I+1)))-AIMAG(T(N1+I)),0.0))
            S(NR)=AMAX1(S(NR),(CABS(TEMP)/X)**(1./FLOAT(I)))
   23    CONTINUE
         S(NR)=1./S(NR)
   25 CONTINUE
      IFLG=0
      RETURN
!        ERROR EXITS
   26 CONTINUE
      IFLG=2
      RETURN
   30 CONTINUE
      IFLG=1
      RETURN
      END SUBROUTINE CPZERO
      SUBROUTINE CRAMER(Y1,Y2,N,IWRITE,XIDTEM,XIDTE2,TEMP1,STAT,   &
                 IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES CRAMER'S COEFFICIENT
!              FOR RXC CONTINGENCY TABLES.  THIS IS
!
!                  SQRT(T/(N*(Q-1)))
!
!              WHERE
!
!                  T = CHI-SQUARE STATISTIC
!                    = SUM[i=1 to r][SUM[j=1 to c]
!                      [(O(ij)-E(ij))**2/E(ij)]]
!
!                      O = OBSERVED COUNT
!                      E = EXPECTED COUNT
!                        = ROW TOTAL*COL TOTAL/GRAND TOTAL
!
!                  N = TOTAL NUMBER OF OBSERVATIONS
!                  Q = MIN(R,C)
!
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
!                STATISTICS", THIRD EDITION, WILEY, PP. 229-230.
!     NOTE--THIS SUBROUTINE HANDLES THE RAW DATA CASE.  USE
!           THE COMMAND
!
!               LET A = MATRIX CRAMER CONTINGENCY COEFFICENT M
!
!           IF YOUR DATA CONSISTS OF AN RXC TABLE.
!     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE FIRST SET
!                                OF DATA.
!                     --Y2     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED) OBSERVATIONS
!                                WHICH CONSTITUTE THE SECOND SET
!                                OF DATA.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X, OR EQUIVALENTLY,
!                                THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
!                                CRAMER'S CONTINGENCY COEFFICIENT
!                                BETWEEN THE 2 SETS OF DATA
!                                IN THE INPUT VECTORS X AND Y.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CRAMER'S CONTINGENCY COEFFICENT BETWEEN THE
!             2 SETS OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      PARAMETER(MAXLEV=20000)
      PARAMETER(IWORK1=0)
      PARAMETER(IWORK2=20000)
      PARAMETER(IWORK3=40000)
      PARAMETER(IWORK4=60000)
      PARAMETER(IWORK5=80000)
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CRAM'
      ISUBN2='ER  '
!
      IERROR='NO'
!
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 2.2--                                      **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
!               ******************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)MAXLEV
 2202   FORMAT('      NUMBER OF SETS FOR VARIABLE ONE IS OUTSIDE ',   &
               'THE INTERVAL (1,',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)NUMSE1
 2204   FORMAT('      THE NUMBER OF SET = ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NUMSE2.LT.1 .OR. NUMSE2.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)MAXLEV
 2212   FORMAT('      NUMBER OF SETS FOR VARIABLE TWO IS OUTSIDE ',   &
               'THE INTERVAL (1,',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)NUMSE2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  STEP 2.3--                               **
!               **  COMPUTE THE CHI-SQUARE STATISTIC         **
!               ***********************************************
!
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE COUNTS FOR EACH CELL
!
      J=0
      DO 2310 ISET1=1,NUMSE1
        DO 2320 ISET2=1,NUMSE2
!
          K=0
          DO 2330 I=1,N
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
              K=K+1
            ENDIF
 2330     CONTINUE
          NTEMP=K
          J=J+1
          TEMP1(IWORK1+J)=REAL(K)
          TEMP1(IWORK2+J)=XIDTEM(ISET1)
          TEMP1(IWORK3+J)=XIDTE2(ISET2)
!
 2320   CONTINUE
 2310 CONTINUE
      NTEMP2=J
!
!     COMPUTE ROW AND COLUMN TOTALS AND GRAND TOTAL.
!
      J=0
      GTOTAL=0.0
!
      DO 2340 ISET1=1,NUMSE1
        TEMP1(IWORK4+ISET1)=0.0
        DO 2350 ISET2=1,NUMSE2
          J=J+1
          TEMP1(IWORK4+ISET1)=TEMP1(IWORK4+ISET1) + TEMP1(IWORK1+J)
          GTOTAL=GTOTAL + TEMP1(IWORK1+J)
 2350   CONTINUE
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2352)ISET1,TEMP1(IWORK4+ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
!
      DO 2360 ISET2=1,NUMSE2
        TEMP1(IWORK5+ISET2)=0.0
        DO 2370 J=1,NTEMP2
          IF(TEMP1(IWORK3+J).EQ.XIDTE2(ISET2))THEN
            TEMP1(IWORK5+ISET2)=TEMP1(IWORK5+ISET2) + TEMP1(IWORK1+J)
          ENDIF
 2370   CONTINUE
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2372)ISET2,TEMP1(IWORK5+ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 2360 CONTINUE
!
!     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
!
      STAT=0.0
      J=0
!
      DO 2380 ISET1=1,NUMSE1
        DO 2390 ISET2=1,NUMSE2
          J=J+1
          EXP=TEMP1(IWORK4+ISET1)*TEMP1(IWORK5+ISET2)/GTOTAL
          STAT=STAT + (TEMP1(IWORK1+J) - EXP)**2/EXP
 2390   CONTINUE
 2380 CONTINUE
      T=STAT
      Q=REAL(MIN(NUMSE1,NUMSE2))
      STAT=STAT/(GTOTAL*(Q-1.0))
      STAT=SQRT(STAT)
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
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CRAMER CONTINGENCY COEFFICIENT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
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
 9011   FORMAT('***** AT THE END OF CRAMER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)T,GTOTAL,Q,STAT
 9015   FORMAT('T,GTOTAL,Q,STAT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CRAMER
      SUBROUTINE CRAME2(XMAT,MAXOBV,NR1,NC1,IWRITE,   &
                 TEMP1,STAT,   &
                 IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES CRAMER'S COEFFICIENT
!              FOR RXC CONTINGENCY TABLES.  THIS IS
!
!                  SQRT(T/(N*(Q-1)))
!
!              WHERE
!
!                  T = CHI-SQUARE STATISTIC
!                    = SUM[i=1 to r][SUM[j=1 to c]
!                      [(O(ij)-E(ij))**2/E(ij)]]
!
!                      O = OBSERVED COUNT
!                      E = EXPECTED COUNT
!                        = ROW TOTAL*COL TOTAL/GRAND TOTAL
!
!                  N = TOTAL NUMBER OF OBSERVATIONS
!                  Q = MIN(R,C)
!
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
!                STATISTICS", THIRD EDITION, WILEY, PP. 229-230.
!     NOTE--THIS SUBROUTINE HANDLES THE SUMMARY DATA CASE (I.E..
!           THE DATA IS GIVEN AS AN RXC TABLE).   THE "CRAMER"
!           SUBROUTINE IS USED FOR THE RAW DATA CASE.
!     INPUT  ARGUMENTS--XMAT   = THE SINGLE PRECISION MATRIX OF
!                                OBSERVATIONS (RXC TABLE)
!                     --MAXOBV = THE INTEGER NUMBER THAT SPECIFIES
!                                THE MAXIMUM NUMBER OF ROWS IN THE
!                                MATRIX.
!                     --NR1    = THE INTEGER NUMBER OF ROWS
!                                IN THE MATRIX XMAT.
!                     --NC1    = THE INTEGER NUMBER OF COLUMNS
!                                IN THE MATRIX XMAT.
!     OUTPUT ARGUMENTS--STAT   = THE SINGLE PRECISION VALUE OF THE
!                                CRAMER'S CONTINGENCY COEFFICIENT
!                                OF THE DATA IN THE MATRIX XMAT.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE CRAMER'S CONTINGENCY COEFFICENT OF THE DATA
!             IN THE MATRIX XMAT.
!     RESTRICTIONS--THE MAXIMUM NUMBER OF LEVELS IS 50,000.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      PARAMETER(MAXLEV=50000)
      PARAMETER(IWORK1=0)
      PARAMETER(IWORK2=50000)
!
      DIMENSION XMAT(MAXOBV,NC1)
      DIMENSION TEMP1(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CRAM'
      ISUBN2='ER  '
      IERROR='NO'
!
!
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
        DO 55 I=1,NR1
          DO 60 J=1,NC1
            WRITE(ICOUT,56)I,J,XMAT(I,J)
   56       FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   60     CONTINUE
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NR1.LT.2 .OR. NR1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
 1201   FORMAT('****** ERROR IN MATRIX CRAMER CONTINGENCY ',   &
               'COEFFICIENT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX IS LESS ',   &
               'THAN 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2102)MAXLEV
 2102   FORMAT('      OR GREATER THAN ',I10,'.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)NR1
 2103   FORMAT('NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NC1.LT.2 .OR. NC1.GT.MAXLEV)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2111)
 2111   FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX IS LESS ',   &
               'THAN 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2102)MAXLEV
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2113)NC1
 2113   FORMAT('NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      GTOTAL=0.0
      DO 2120 J=1,NC1
        DO 2130 I=1,NR1
          ITEMP=INT(XMAT(I,J)+0.5)
          IF(ITEMP.LT.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2131)
 2131       FORMAT('      A NEGATIVE COUNT WAS ENCOUNTERED IN THE ',   &
                   'INPUT MATRIX.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2133)I,J,ITEMP
 2133       FORMAT('      COUNT FOR ROW ',I8,' COLUMN ',I8,' = ',I8)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          XMAT(I,J)=REAL(ITEMP)
          GTOTAL=GTOTAL + XMAT(I,J)
 2130   CONTINUE
 2120 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2344)GTOTAL
 2344   FORMAT('GTOTAL = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 2.2--                                      **
!               **  COMPUTE THE ROW AND COLUMN TOTALS.              **
!               ******************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2340 ISET1=1,NR1
        TEMP1(IWORK1+ISET1)=0.0
        DO 2350 ISET2=1,NC1
          TEMP1(IWORK1+ISET1)=TEMP1(IWORK1+ISET1) + XMAT(ISET1,ISET2)
          IF(IBUGA3.EQ.'ON')THEN
            WRITE(ICOUT,2342)ISET1,ISET2,XMAT(ISET1,ISET2)
 2342       FORMAT('ISET1,ISET2,XMAT(I,J) =',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
 2350   CONTINUE
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2352)ISET1,TEMP1(IWORK1+ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
!
      DO 2360 ISET2=1,NC1
        TEMP1(IWORK2+ISET2)=0.0
        DO 2370 ISET1=1,NR1
          TEMP1(IWORK2+ISET2)=TEMP1(IWORK2+ISET2) + XMAT(ISET1,ISET2)
 2370   CONTINUE
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2372)ISET2,TEMP1(IWORK2+ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 2360 CONTINUE
!
!               ******************************************************
!               **  STEP 2.3--                                      **
!               **  COMPUTE THE CHI-SQUARE STATISTIC.               **
!               ******************************************************
!
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
!
      STAT=0.0
!
      DO 2380 ISET1=1,NR1
        DO 2390 ISET2=1,NC1
          EXP=TEMP1(IWORK1+ISET1)*TEMP1(IWORK2+ISET2)/GTOTAL
          STAT=STAT + (XMAT(ISET1,ISET2) - EXP)**2/EXP
 2390   CONTINUE
 2380 CONTINUE
      T=STAT
      Q=REAL(MIN(NR1,NC1))
      STAT=STAT/(GTOTAL*(Q-1.0))
      STAT=SQRT(STAT)
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
      WRITE(ICOUT,811)STAT
  811 FORMAT('THE CRAMER CONTINGENCY COEFFICIENT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
  890 CONTINUE
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
 9011   FORMAT('***** AT THE END OF CRAME2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)T,GTOTAL,Q,STAT
 9015   FORMAT('T,GTOTAL,Q,STAT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CRAME2
      SUBROUTINE CSCORE(Y,X,N,IWRITE,XSCORE,XIDTEM,TEMP1,TEMP2,MAXNXT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CONOVER SCORES OF THE N
!              ELEMENTS OF THE SINGLE PRECISION VECTOR X, AND PUTS THE
!              RESULTING N SCORES INTO THE SINGLE PRECISION VECTOR XSCORE.
!
!              THE CONOVER SCORES ARE DEFINED AS:
!
!                  C(i) = (Rank(U(j))**2
!
!              WHERE
!
!                  U(j) = ABS(Y(j(i) - XMEAN(i))
!
!                  XMEAN(i) = MEAN OF THE i-th GROUP
!
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF THE
!                                RESPONSE OBSERVATIONS TO BE SCORED.
!                     --X      = THE SINGLE PRECISION VECTOR OF THE
!                                GROUP-ID VALUES
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XSCORE = THE SINGLE PRECISION VECTOR INTO WHICH
!                                THE SCORES FROM X WILL BE PLACED.
!     OUTPUT--THE SINGLE PRECISION VECTOR XSCORE CONTAINING THE KLOTZ
!             SCORES.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                 THIRD EDITION, WILEY, PP. 401.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023.07
!     ORIGINAL VERSION--JULY      2022.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XSCORE(*)
      DIMENSION XIDTEM(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='KSCO'
      ISUBN2='RE  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CORE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CSCORE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,MAXNXT,IBUGA3,ISUBRO
   53   FORMAT('N,MAXNXT,IBUGA3,ISUBRO = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y(I),X(I)
   56     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1 .OR. N.GT.MAXNXT)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CONOVER SCORES (CSCORE)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)MAXNXT
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
!               **  GENERATE THE SQUARED RANKS            **
!               ********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SCOR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     DETERMINE DISTINCT VALUES OF GROUP-ID VARIABLE.  SUBTRACT
!     GROUP MEANS FROM VARIABLE.
!
      CALL DISTIN(X,N,IWRITE,XIDTEM,NDIST,IBUGA3,IERROR)
      DO 1010 K=1,NDIST
        HOLD=XIDTEM(K)
        NTEMP=0
        DO 1020 I=1,N
          IF(X(I).EQ.HOLD)THEN
            NTEMP=NTEMP+1
            TEMP2(NTEMP)=Y(I)
          ENDIF
 1020   CONTINUE
        CALL MEAN(TEMP2,NTEMP,IWRITE,YMEAN,IBUGA3,IERROR)
        XSCORE(K)=YMEAN
 1010 CONTINUE
!
      DO 1030 I=1,N
        DO 1040 K=1,NDIST
          IF(XIDTEM(K).EQ.X(I))THEN
            TEMP1(I)=ABS(Y(I) - REAL(XSCORE(K)))
            GO TO 1049
          ENDIF
 1040   CONTINUE
 1049   CONTINUE
 1030 CONTINUE
!
!     COMPUTE RANKS AND THEN SQUARE THE RANKS
!
      CALL RANK(TEMP1,N,IWRITE,XSCORE,TEMP2,MAXNXT,IBUGA3,IERROR)
      DO 1110 I=1,N
        XSCORE(I)=XSCORE(I)**2
 1110 CONTINUE
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
        WRITE(ICOUT,912)IINDX,XSCORE(IINDX)
  912   FORMAT('THE CONOVER SCORE OF ROW ',I10,' = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IINDX=N
        WRITE(ICOUT,912)IINDX,XSCORE(IINDX)
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
 9011   FORMAT('***** AT THE END       OF CSCORE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),XSCORE(I)
 9016     FORMAT('I,X(I),XSCORE(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CSCORE
      FUNCTION CSEVL (X, CS, N)
!***BEGIN PROLOGUE  CSEVL
!***PURPOSE  Evaluate a Chebyshev series.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C3A2
!***TYPE      SINGLE PRECISION (CSEVL-S, DCSEVL-D)
!***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
!  Evaluate the N-term Chebyshev series CS at X.  Adapted from
!  a method presented in the paper by Broucke referenced below.
!
!       Input Arguments --
!  X    value at which the series is to be evaluated.
!  CS   array of N terms of a Chebyshev series.  In evaluating
!       CS, only half the first coefficient is summed.
!  N    number of terms in array CS.
!
!***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
!                 Chebyshev series, Algorithm 446, Communications of
!                 the A.C.M. 16, (1973) pp. 254-256.
!               L. Fox and I. B. Parker, Chebyshev Polynomials in
!                 Numerical Analysis, Oxford University Press, 1968,
!                 page 56.
!***ROUTINES CALLED  R1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770401  DATE WRITTEN
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900329  Prologued revised extensively and code rewritten to allow
!           X to be slightly outside interval (-1,+1).  (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  CSEVL
      REAL B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  CSEVL
!
      B0=0.0
      B2=0.0
!
      IF (FIRST) ONEPL = 1.0E0 + R1MACH(4)
      FIRST = .FALSE.
!
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
!
      B1 = 0.0E0
      B0 = 0.0E0
      TWOX = 2.0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
!
      CSEVL = 0.5E0*(B0-B2)
!
      RETURN
      END FUNCTION CSEVL 
      SUBROUTINE CUMAVE(X,NX,IWRITE,Y,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE AVERAGE (MEAN) OF AN ARRAY
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
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
      DOUBLE PRECISION DSUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMA'
      ISUBN2='VE  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMAVE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,NX
   52   FORMAT('IBUGA3,IWRITE,NX = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **************************************
!               **  COMPUTE CUMULATIVE AVERAGE      **
!               **************************************
!
      Y(1)=X(1)
      IF(NX.LT.2)GO TO 9000
      DSUM=DBLE(Y(1))
      DO 100 I=2,NX
        DSUM=DSUM + DBLE(X(I))
        Y(I)=REAL(DSUM/DBLE(I))
  100 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF CUMAVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX
 9013 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE CUMAVE
      SUBROUTINE CUMHAZ(X,TAG,NX,IWRITE,Y,XTEMP,MAXOBV,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE HAZARD OF AN ARRAY
!              THE TAG VARIABLE IDENTIFIES CENSORED DATA
!              (1 = FAILURE TIME, 0 = CENSORED)
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
!     UPDATED         --JANUARY   2007. ARGUMENT LIST TO RANK
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
      DOUBLE PRECISION DSUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION XTEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMH'
      ISUBN2='AZ  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMHAZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,NX
   52   FORMAT('IBUGA3,NX = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I),TAG(I)
   56     FORMAT('I,X(I), TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **************************************
!               **  COMPUTE CUMULATIVE HAZARD       **
!               **************************************
!
      CALL SORTC(X,TAG,NX,Y,TAG)
      CALL RANK(Y,NX,IWRITE,Y,XTEMP,MAXOBV,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      AFACT=REAL(NX+1)
      DO 100 J=1,NX
        IF(ABS(TAG(J)).GE.0.5)THEN
          Y(J)=100./(AFACT - Y(J))
        ELSE
          Y(J)=0.0
        ENDIF
  100 CONTINUE
!
      DSUM=0.0D0
      DO 200 I=1,NX
        DSUM=DSUM+DBLE(Y(I))
        Y(I)=REAL(DSUM)
  200 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMHAZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NX
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMHAZ
      SUBROUTINE CUMINT(Y,X,N,NUMVAR,IWRITE,Z,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE INTEGRAL OF A VARIABLE.
!     NOTE--IF THE VERTICAL AXIS VARIABLE IS Y(.)
!           AND THE HORIZONTAL AXIS VARIABLE IS X(.),
!           THEN THE OUTPUT VARIABLE CONTAINING THE
!           CUMULATIVE INTEGRAL
!           WILL BE COMPUTED AS FOLLOWS--
!              Z(1) = 0
!              Z(2) = Z(1) + (Y(2)-Y(1))*(X(2)-X(1))/2
!              Z(3) = Z(2) + Y(2)*(X(3)-X(2)) + (Y(3)-Y(2))*(X(3)-X(2))/2
!              ETC.
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Z(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.)
!           OR THE INPUT VECTORS X(.) AND Y(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--FEBRUARY  1979.
!     UPDATED         --APRIL     1979.
!     UPDATED         --JULY      1979.
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
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Z(*)
!
      DOUBLE PRECISION DINT
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DYI
      DOUBLE PRECISION DXIM1
      DOUBLE PRECISION DYIM1
      DOUBLE PRECISION DDELX
      DOUBLE PRECISION DDELY
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMI'
      ISUBN2='NT  '
      IERROR='NO'
!
      DXI=0.0D0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMINT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N,NUMVAR
   52   FORMAT('IBUGA3,IWRITE,N,NUMVAR = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I),Y(I)
   56     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************************
!               **  CUMPUTE THE CUMULATIVE (NUMERICAL) INTEGRAL.  **
!               ****************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DINT=0.0D0
      IF(N.LT.1)GO TO 150
      IF(N.EQ.1)GO TO 190
      I=1
      IF(NUMVAR.EQ.1)DXI=I
      IF(NUMVAR.EQ.2)DXI=X(I)
      DYI=Y(1)
      Z(1)=0.0
      DO 100 I=2,N
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
      GO TO 190
!
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
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
  190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMINT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,N,NUMVAR
 9012   FORMAT('IBUGA3,IERROR,N,NUMVAR = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I),Z(I)
 9016     FORMAT('I,X(I),Y(I),Z(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMINT
      SUBROUTINE CUMMAX(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE MAXIMUM OF A VARIABLE
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/12
!     ORIGINAL VERSION--DECEMBER  2012.
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
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMM'
      ISUBN2='IN  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMAX')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMMAX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  COMPUTE CUMULATIVE MAXIMUM.  **
!               ***********************************
!
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
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
!
      ELSE
!
        Y(1)=X(1)
        YMAX=Y(1)
        DO 100 I=1,N
          IF(X(I).GT.YMAX)THEN
            Y(I)=X(I)
            YMAX=Y(I)
          ELSE
            Y(I)=YMAX
          ENDIF
  100   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMAX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMMAX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMMAX
      SUBROUTINE CUMMIN(X,N,IWRITE,Y,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE MINIMUM OF A VARIABLE
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/12
!     ORIGINAL VERSION--DECEMBER  2012.
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
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMM'
      ISUBN2='IN  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMIN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMMIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  COMPUTE CUMULATIVE MINIMUM.  **
!               ***********************************
!
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
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
!
      ELSE
!
        Y(1)=X(1)
        YMIN=Y(1)
        DO 100 I=1,N
          IF(X(I).LT.YMIN)THEN
            Y(I)=X(I)
            YMIN=Y(I)
          ELSE
            Y(I)=YMIN
          ENDIF
  100   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MMIN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMMIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMMIN
      SUBROUTINE CUMPRO(X,N,IWRITE,Y,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE PRODUCT OF A VARIABLE--
!              Y(1) = X(1)
!              Y(2) = X(1) * X(2)
!              Y(3) = X(1) * X(2) * X(3)
!              ETC.
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1979.
!     UPDATED         --JULY      1979.
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
      DIMENSION Y(*)
!
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DX
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMP'
      ISUBN2='RO  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMPRO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***********************************
!               **  COMPUTE CUMULATIVE PRODUCT.  **
!               ***********************************
!
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
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
      ELSE
        DO 100 I=1,N
          DX=X(I)
          DPROD=DPROD*DX
          Y(I)=DPROD
  100   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMPRO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMPRO
      SUBROUTINE CUMSTA(Y1,Y2,Y3,N,NUMV,ICASS7,ISTARA,MAXNXT,   &
                        ISEED,ICSTSV,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        YOUT,NOUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE A "CUMULATIVE" STATISTIC.  ALTHOUGH THIS IS TYPICALLY
!              USED FOR A LOCATION STATISTIC, IN CAN BE USED FOR ANY
!              SUPPORTED STATISTIC.  NOTE THAT A FEW SPECIFIC STATISTICS
!              ARE GENERATED SEPARATELY FROM THIS SUBROUTINE.  THESE
!              ARE TYPICALLY GENERATED MORE EFFICIENTLY THAN THIS ROUTINE
!              WHICH SIMPLY LOOPS THROUGH THE ARRAY AND CALLS CMPSTA TO
!              COMPUTE THE STATISTIC.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/01
!     ORIGINAL VERSION--JANUARY     2013.
!     UPDATED         --MARCH       2013. CUMULATIVE STATISTIC START
!     UPDATED         --AUGUST      2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASS7
      CHARACTER*4 ISTARA
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMS'
      ISUBN2='TA  '
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'MSTA')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF CUMSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASS7,N,ICSTSV
   71   FORMAT('ICASS7,N,ICSTSV = ',A4,2X,2I8)
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 1--LOOP THROUGH AND COMPUTE THE STATISTIC  **
!               ******************************************************
!
!     MARCH 2013: SOME STATISTICS REQUIRE A MINIMUM NUMBER OF VALUES
!                 IN ORDER TO COMPUTE.  USER CAN ENTER THE COMMAND
!
!                    SET CUMULATIVE STATISTIC START <IVAL>
!
!                 TO SPECIFY A MINIMUM NUMBER OF VALUES BEFORE START
!                 COMPUTING THE STATISTIC.
!
      NOUT=0
      ISTRT=ICSTSV
      IF(ISTRT.LT.1 .OR. ISTRT.GT.N)ISTRT=1
      DO 1010 I=ISTRT,N
        NTEMP=I
        CALL CMPSTA(Y1,Y2,Y3,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                    MAXNXT,NTEMP,NTEMP,NTEMP,NUMV,ICASS7,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STAT,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NOUT=NOUT+1
        YOUT(NOUT)=STAT
 1010 CONTINUE
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
          DO 9021 I=1,NOUT
            WRITE(ICOUT,9023)I,YOUT(I)
 9023       FORMAT('I,YOUT(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE CUMSTA
      SUBROUTINE CUMSUM(X,N,IWRITE,Y,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE SUM OF A VARIABLE--
!              Y(1) = X(1)
!              Y(2) = X(1) + X(2)
!              Y(3) = X(1) + X(2) + X(3)
!              ETC.
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--FEBRUARY  1979.
!     UPDATED         --APRIL     1979.
!     UPDATED         --JULY      1979.
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
      DIMENSION Y(*)
!
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUMS'
      ISUBN2='UM  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUMSUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************
!               **  COMPUTE CUMULATIVE SUM.  **
!               *******************************
!
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
  157   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
      ELSE
        DO 100 I=1,N
          DX=X(I)
          DSUM=DSUM+DX
          Y(I)=DSUM
  100   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMSUM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE CUMSUM
      SUBROUTINE cumtnc(t,df,pnonc,cum,ccum)
!
!     2017/01: THIS ROUTINE IS FROM THE DCDFLIB LIBRARY OF BARRY BROWN,
!              JAMES LAVATO, AND KATHY RUSSELL.
!
!              THE MAIN MODIFICATION OF THIS ROUTINE IS TO REPLACE
!              GAMLN, CUMT, CUMNOR, AND BRATIO WITH ROUTINES THAT ARE
!              ALREADY IN DATAPLOT.
!
!**********************************************************************
!
!     SUBROUTINE CUMTNC(T,DF,PNONC,CUM,CCUM)
!
!                 CUMulative Non-Central T-distribution
!
!
!                              Function
!
!
!     Computes the integral from -infinity to T of the non-central
!     t-density.
!
!
!                              Arguments
!
!
!     T --> Upper limit of integration of the non-central t-density.
!                                                  T is DOUBLE PRECISION
!
!     DF --> Degrees of freedom of the non-central t-distribution.
!                                                  DF is DOUBLE PRECISIO
!
!     PNONC --> Non-centrality parameter of the non-central t distibutio
!                                                  PNONC is DOUBLE PRECI
!
!     CUM <-- Cumulative t-distribution.
!                                                  CCUM is DOUBLE PRECIS
!
!     CCUM <-- Compliment of Cumulative t-distribution.
!                                                  CCUM is DOUBLE PRECIS
!
!
!                              Method
!
!     Upper tail    of  the  cumulative  noncentral t   using
!     formulae from page 532  of Johnson, Kotz,  Balakrishnan, Coninuous
!     Univariate Distributions, Vol 2, 2nd Edition.  Wiley (1995)
!
!     This implementation starts the calculation at i = lambda,
!     which is near the largest Di.  It then sums forward and backward.
!***********************************************************************
!     .. Parameters ..
                                                                                                                                  
      DOUBLE PRECISION one,zero,half,two,onep5
      PARAMETER (one=1.0d0,zero=0.0d0,half=0.5d0,two=2.0d0,onep5=1.5d0)
      DOUBLE PRECISION conv
      PARAMETER (conv=1.0d-7)
      DOUBLE PRECISION tiny
      PARAMETER (tiny=1.0d-10)
!     ..
!     .. Scalar Arguments ..
      DOUBLE PRECISION ccum,cum,df,pnonc,t
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION alghdf,b,bb,bbcent,bcent,cent,d,dcent,dpnonc,   &
                       dum1,dum2,e,ecent,halfdf,lambda,lnomx,lnx,omx,   &
                       pnonc2,s,scent,ss,sscent,t2,term,tt,twoi,x,   &
                       xi,xlnd,xlne
      INTEGER ierr
      LOGICAL qrevs
!     ..
!     .. External Functions ..
!CCCC DOUBLE PRECISION gamln
!CCCC EXTERNAL gamln
      DOUBLE PRECISION DLNGAM
      EXTERNAL DLNGAM
!     ..
!     .. External Subroutines ..
!CCCC EXTERNAL bratio,cumnor,cumt
      EXTERNAL bratio
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC abs,exp,int,log,max,min
!     ..
                                                                                                                                  
!
      dum1=0.0
      dum2=0.0
!
!     Case pnonc essentially zero
                                                                                                                                  
      IF (abs(pnonc).LE.tiny) THEN
!cccc     CALL cumt(t,df,cum,ccum)
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
!cccc     CALL cumnor(-pnonc,cum,ccum)
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
!cccc alghdf = gamln(halfdf)
      alghdf = DLNGAM(halfdf)
                                                                                                                                  
!     ******************** Case i = lambda
                                                                                                                                  
      cent = int(lambda)
                                                                                                                                  
      IF (cent.LT.one) cent = one
                                                                                                                                  
!     Compute d=T(2i) in log space and offset by exp(-lambda)
                                                                                                                                  
!cccc xlnd = cent*log(lambda) - gamln(cent+one) - lambda
      xlnd = cent*log(lambda) - dlngam(cent+one) - lambda
                                                                                                                                  
      dcent = exp(xlnd)
                                                                                                                                  
!     Compute e=t(2i+1) in log space offset by exp(-lambda)
                                                                                                                                  
!cccc xlne = (cent+half)*log(lambda) - gamln(cent+onep5) - lambda
      xlne = (cent+half)*log(lambda) - dlngam(cent+onep5) - lambda
      ecent = exp(xlne)
                                                                                                                                  
      IF (dpnonc.LT.zero) ecent = -ecent
                                                                                                                                  
!     Compute bcent=B(2*cent)
                                                                                                                                  
      CALL bratio(halfdf,cent+half,x,omx,bcent,dum1,ierr)
!CCCC bcent=dbetai(x,halfdf,cent+half)
                                                                                                                                  
!     compute bbcent=B(2*cent+1)
                                                                                                                                  
      CALL bratio(halfdf,cent+one,x,omx,bbcent,dum2,ierr)
!CCCC bbcent=dbetai(x,halfdf,cent+one)
                                                                                                                                  
!     Case bcent and bbcent are essentially zero
!     Thus t is effectively infinite
                                                                                                                                  
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
                                                                                                                                  
!     Case bcent and bbcent are essentially one
!     Thus t is effectively zero
                                                                                                                                  
      IF ((dum1+dum2).LT.tiny) THEN
!cccc     CALL cumnor(-pnonc,cum,ccum)
          CALL nodcdf(-pnonc,cum)
          ccum=1.0d0 - cum
          RETURN
                                                                                                                                  
      END IF
                                                                                                                                  
!     First term in ccum is D*B + E*BB
                                                                                                                                  
      ccum = dcent*bcent + ecent*bbcent
                                                                                                                                  
!     compute s(cent) = B(2*(cent+1)) - B(2*cent))
                                                                                                                                  
!cccc scent = gamln(halfdf+cent+half) - gamln(cent+onep5) - alghdf +
      scent = dlngam(halfdf+cent+half) - dlngam(cent+onep5) - alghdf +   &
              halfdf*lnx + (cent+half)*lnomx
      scent = exp(scent)
                                                                                                                                  
!     compute ss(cent) = B(2*cent+3) - B(2*cent+1)
                                                                                                                                  
!cccc sscent = gamln(halfdf+cent+one) - gamln(cent+two) - alghdf +
      sscent = dlngam(halfdf+cent+one) - dlngam(cent+two) - alghdf +   &
               halfdf*lnx + (cent+one)*lnomx
      sscent = exp(sscent)
                                                                                                                                  
!     ******************** Sum Forward
                                                                                                                                  
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
                                                                                                                                  
!     ******************** Sum Backward
                                                                                                                                  
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
                                                                                                                                  
!     Due to roundoff error the answer may not lie between zero and one
!     Force it to do so
                                                                                                                                  
      cum = max(min(cum,one),zero)
      ccum = max(min(ccum,one),zero)
                                                                                                                                  
      RETURN
                                                                                                                                  
      END SUBROUTINE cumtnc
      SUBROUTINE CURVE (P, X, N0, N, EPS, MAXITR, MU, SIGMA, ITER,   &
         SEMU, SESIG, COVAR, E0, EX, CHISQ,   &
         F, F1, XN,   &
         FUNC,   &
         IFAULT)
!
!       ALGORITHM AS 95 APPL. STATIST. (1976) VOL.25, NO.1
!
!       ESTIMATES MU AND SIGMA OF DISTRIBUTION FUNCTION
!       F( (X-MU)/SIGMA ) FROM A GROUPED SAMPLE OF X VALUES.
!       NOTE ON ARRAY SIZES
!       THE ARRAYS IN THE SECOND DIMENSION STATEMENT MUST HAVE
!       MINIMUM SIZE P.  IF P IS TO EXCEED 20, A SUITABLE SIZE
!       MUST BE SET FOR THEM, AND THE IF STATEMENT WHICH CHECKS
!       THE VALUE OF P MUST BE AMENDED.
!
!     Auxiliary routines required: FUNC & DEVIAT (both user-supplied)
!
      PARAMETER (MAXCLA=1000)
!
      INTEGER P
      REAL NN, NI, NP, MU, ONE, ZERO
      DIMENSION X(*), N(*), EX(*)
      DIMENSION F(*), F1(*), XN(*)
!
      EXTERNAL FUNC
!
      DATA RR/1.0E-10/
      DATA ONE/1.0/
      DATA ZERO/0.0/
!
      E=0.0
      D=0.0
      C=0.0
      DENOM=0.0
!
!       ERROR EXIT IF P TOO SMALL OR TOO LARGE
!
      IF (P.LT.2 .OR. P.GT.MAXCLA) THEN
         IFAULT = 1
         GO TO 9000
      END IF
!
      IFAULT = 0
!
!       SET FREQUENCIES IN FLOATING POINT
!
      XN0 = N0
      NSUM = N0
      DO 10 I = 1, P
        XN(I) = N(I)
        NSUM = NSUM + N(I)
 10   CONTINUE
      K = P - 1
      XNSUM = REAL(NSUM)
      NP = XN(P)
!
!       ITERATIVE APPROXIMATION
!
      DO 40 ITER = 1, MAXITR
!
!       COMPUTE VALUES OF DISTRIBUTION AND DENSITY FUNCTIONS,
!       USING CURRENT VALUES OF MU, SIGMA
!
        DO 20 I = 1, P
          CALL FUNC ((X(I) - MU)/SIGMA, F(I), F1(I))
 20     CONTINUE
        DM = ONE - F(P)
!
!
!       TEST FOR SMALL DIVISOR TO AVOID OVERFLOW
!
        IF (ABS(DM).LT.RR) THEN
           IFAULT=2
           GO TO 9000
        ENDIF
!
        F1P = F1(P)
        IF (ABS(F(1)).LT.RR) THEN
           IFAULT=2
           GO TO 9000
        ENDIF
!
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
!
          IF (ABS(R).LT.RR) THEN
             IFAULT=2
             GO TO 9000
          ENDIF
!
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
!
!       COMPUTE ADJUSTMENTS TO MU, SIGMA
!
        SIGDEN = SIGMA/DENOM
        DMU = (E*A - D*B)*SIGDEN
        DSIGMA = (C*B - D*A)*SIGMA*SIGDEN
        MU = MU + DMU
        SIGMA = SIGMA + DSIGMA
        ERR = ABS(DMU) + ABS(DSIGMA)
!
!       TEST FOR CONVERGENCE
!
        IF (ERR.LT.EPS) GO TO 50
 40   CONTINUE
!
!     SET FAULT IF LIMIT FOR NUMBER OF ITERATIONS IS
!     REACHED, THEN PROCEED
!
      IFAULT = 4
      ITER = MAXITR
!
 50   CONTINUE
      DO 60 I = 1, P
        CALL FUNC ((X(I) - MU)/SIGMA, F(I), DUM)
 60   CONTINUE
!
!     COMPUTE VARIANCES AND COVARIANCE OF ESTIMATES
!
      SIGDEN = SIGMA*SIGMA/DENOM
      VARMU = E*SIGDEN
      SIGDEN = SIGMA*SIGDEN
      COVAR = -D*SIGDEN
      VARSIG = C*SIGMA*SIGDEN
      IF (VARMU.LT.ZERO .OR. VARSIG.LT.ZERO) THEN
         IFAULT=3
         GO TO 9000
      ENDIF
!
      SEMU = SQRT(VARMU)
      SESIG = SQRT(VARSIG)
!
!       COMPUTE EXPECTED FREQUENCIES AND CHI SQUARE
!
      E0 = XNSUM*F(1)
      EP = XNSUM*(ONE - F(P))
      EX(P) = EP
      CHISQ = ((XN0 - E0)**2)/E0 + ((NP - EP)**2)/EP
      DO 70 I = 1, K
        NN = XNSUM*(F(I+1) - F(I))
        CHISQ = CHISQ + ((NN - XN(I))**2)/NN
        EX(I) = NN
 70   CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE CURVE 
      SUBROUTINE CUSARL(X,NX,IWRITE,Y,ICASE,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE CUMULATIVE SUM ARL.
!              USE APPLIED STATISTICS ALGORITHM AS 258.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/3
!     ORIGINAL VERSION--MARCH    1999.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CUSA'
      ISUBN2='RL  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CUSARL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASE,IBUGA3,IWRITE,NX
   52   FORMAT('ICAE,IBUGA3,IWRITE,NX = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *********************************************
!               **  CHECK FOR PARAMERERS: DELTA, S0, K, H  **
!               *********************************************
!
      IHP='S0  '
      IHP2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        S0=0.0
      ELSE
        S0=VALUE(ILOCP)
      ENDIF
!
      IHP='K   '
      IHP2='    '
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AK=VALUE(ILOCP)
!
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
        GO TO 9000
      ENDIF
!
      IHP='H   '
      IHP2='    '
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AH=VALUE(ILOCP)
!
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
        GO TO 9000
      ENDIF
!
      DO 100 I=1,NX
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
          GO TO 9000
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
          GO TO 9000
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
          GO TO 9000
        ENDIF
        IF(S0.GT.0.0)THEN
          Y(I)=ARLFIR
        ELSE
          Y(I)=ARL
        ENDIF
  100 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF CUSARL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX
 9013 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE CUSARL
      double precision function cvflow(cv)
      implicit double precision (a-h,o-z)
      common /cvc/ estcv,sqrtn,df,ratio,alphad2,omad2
!
      xncp = sqrtn/cv
!cccc call cdftnc(1,p,q,ratio,df,xncp,ier,bound)
      call nctcd2(ratio,df,xncp,p)
!cccc if (ier .ne. 0) then
!cccc    write(6,10)
!cccc    write(7,10)
!10      format(/,1x,'The ier value from a call',
!ccccx   ' to cdftnc was nonzero.  Please contact',/,
!ccccx   1x,'Steve Verrill at sverrill@fs.fed.us.',/)
!cccc    stop
!cccc endif
      cvflow = alphad2 - p
      return
      end
      double precision function cvfup(cv)
      implicit double precision (a-h,o-z)
      common /cvc/ estcv,sqrtn,df,ratio,alphad2,omad2
      xncp = sqrtn/cv
      call nctcd2(ratio,df,xncp,p)
!cccc if (ier .ne. 0) then
!cccc    write(6,10)
!cccc    write(7,10)
!10      format(/,1x,'The ier value from a call',
!ccccx   ' to cdftnc was nonzero.  Please contact',/,
!ccccx   1x,'Steve Verrill at sverrill@fs.fed.us.',/)
!cccc    stop
!cccc endif
      cvfup = omad2 - p
      return
      end

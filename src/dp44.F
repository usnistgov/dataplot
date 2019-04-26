      SUBROUTINE TAGUCH(X,N,ICASPL,IWRITE,XTAGUC,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
C              OF THE DATA IN THE INPUT VECTOR X.
C              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
C              (FOR THE "TARGET IS BEST WITH
C              VARIANCE DEPENDENT ON MEAN" CASE) =
C              10 * LOG10 ( YBAR**2 / S**2)
C              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
C              (FOR THE "LARGE IS BEST" CASE) =
C              -10 * LOG10 (AVERAGE SUM OF SQUARED INVERSES)
C              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
C              (FOR THE "SMALL IS BEST" CASE) =
C              -10 * LOG10 (AVERAGE SUM OF SQUARED OBSERVATIONS)
C              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
C              (FOR THE "TARGET IS BEST WITH
C              VARIANCE NOT DEPENDENT ON MEAN" CASE) =
C              -10 * LOG10 (VARIANCE)
C              THE DENOMINATOR N-1 IS USED IN COMPUTING THE
C              SAMPLE STANDARD DEVIATION.
C              THE SAMPLE STANDARD DEVIATION = SQRT((THE SUM OF THE
C              SQUARED DEVIATIONS ABOUT THE SAMPLE MEAN)/(N-1)).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--XTAGUC = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DABS, DLOG10.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--ELLIOT, JACK G.
C                 STATISTICAL METHODS AND APPLICATIONS
C                 ALLIED SIGNAL, 1987, PAGES 4-3 AND 4-4.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88.8
C     ORIGINAL VERSION--AUGUST    1988.
C     UPDATED         --MAY       1989.  SN0, SN+, SN-, SN00
C     UPDATED         --APRIL     1992.  DELETE DRATIO
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
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
      DOUBLE PRECISION DTERM
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
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
      ISUBN1='TAGU'
      ISUBN2='CH  '
C
      IERROR='NO'
C
      DMEAN=0.0D0
      DSD=0.0D0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TAGUCH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICASPL,IWRITE,IBUGA3
   52 FORMAT('ICASPL,IWRITE,IBUGA3 = ',A4,2X,A4,2X,A4)
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
C               **********************************************
C               **  COMPUTE  TAGUCHI SIGNAL-TO-NOISE RATIO  **
C               **********************************************
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
  111 FORMAT('***** ERROR IN TAGUCH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)
  114 FORMAT('      THE TAGUCHI SIGNAL-TO-NOISE RATIO IS TO BE ',
     1'COMPUTED')
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
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NON-FATAL DIAGNOSTIC IN TAGUCH--',
     1'THE 2ND INPUT ARGUMENT (N) HAS THE VALUE 1')
      CALL DPWRST('XXX','BUG ')
      XTAGUC=0.0
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
  136 FORMAT('***** NON-FATAL DIAGNOSTIC IN TAGUCH--',
     1'THE FIRST INPUT ARGUMENT (A VECTOR) HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XTAGUC=0.0
      GOTO9000
  139 CONTINUE
C
  190 CONTINUE
C
C               **************************************************
C               **  STEP 10--                                   **
C               **  BRANCH TO THE APPROPRIATE SUBCASE           **
C               **************************************************
C
CCCCC THE FOLLOWING 4 LINES WERE FIXED MAY 1989
      IF(ICASPL.EQ.'SN0')GOTO1100
      IF(ICASPL.EQ.'SN+')GOTO1200
      IF(ICASPL.EQ.'SN-')GOTO1300
      IF(ICASPL.EQ.'SN00')GOTO1400
      GOTO1100
C
C               ******************************************************
C               **  STEP 11--                                       **
C               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO       **
C               **  FOR THE "TARGET IS BEST" CASE                   **
C               **  (AND WITH THE VARIANCE CHANGING WITH THE MEAN)  **
C               ******************************************************
C
 1100 CONTINUE
      DN=N
      DSUM=0.0D0
      DO1110I=1,N
      DX=X(I)
      DSUM=DSUM+DX
 1110 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO1120I=1,N
      DX=X(I)
      DSUM=DSUM+(DX-DMEAN)**2
 1120 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
C
      IF(DSD.EQ.0.0D0)DTERM=(-999.99D0)
      IF(DSD.NE.0.0D0)DTERM=DMEAN/DSD
      IF(DSD.EQ.0.0D0)DTERM2=(-999.99D0)
      IF(DSD.NE.0.0D0)DTERM2=DABS(DTERM)
      IF(DSD.EQ.0.0D0)DTERM3=(-999.99D0)
      IF(DSD.NE.0.0D0)DTERM3=DLOG10(DTERM2)
      XTAGUC=20.0D0*DTERM3
C
      IF(IFEEDB.EQ.'OFF')GOTO1190
      IF(IWRITE.EQ.'OFF')GOTO1190
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('(FOR THE "TARGET IS BEST WITH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('VARIANCE DEPENDENT ON MEAN" CASE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)N,XTAGUC
 1184 FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1190 CONTINUE
      GOTO9000
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO   **
C               **  FOR THE "LARGE IS BEST" CASE                **
C               **************************************************
C
 1200 CONTINUE
      DN=N
      DSUM=0.0D0
      DO1210I=1,N
      DX=X(I)
      DARG=1.0D0/DX
      DSUM=DSUM+DARG*DARG
 1210 CONTINUE
      DTERM=DSUM/DN
C
      DTERM2=DABS(DTERM)
      DTERM3=DLOG10(DTERM2)
      XTAGUC=(-10.0D0*DTERM3)
C
      IF(IFEEDB.EQ.'OFF')GOTO1290
      IF(IWRITE.EQ.'OFF')GOTO1290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)
 1282 FORMAT('(FOR THE "LARGE IS BEST" CASE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1283)N,XTAGUC
 1283 FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1290 CONTINUE
      GOTO9000
C
C               **************************************************
C               **  STEP 13--                                   **
C               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO   **
C               **  FOR THE "SMALL IS BEST" CASE                **
C               **************************************************
C
 1300 CONTINUE
      DN=N
      DSUM=0.0D0
      DO1310I=1,N
      DX=X(I)
      DSUM=DSUM+DX*DX
 1310 CONTINUE
      DTERM=DSUM/DN
C
      DTERM2=DABS(DTERM)
      DTERM3=DLOG10(DTERM2)
      XTAGUC=(-10.0D0*DTERM3)
C
      IF(IFEEDB.EQ.'OFF')GOTO1390
      IF(IWRITE.EQ.'OFF')GOTO1390
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)
 1382 FORMAT('(FOR THE "SMALL IS BEST" CASE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1383)N,XTAGUC
 1383 FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1390 CONTINUE
      GOTO9000
C
C               **********************************************************
C               **  STEP 14--                                           **
C               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO           **
C               **  FOR THE "TARGET IS BEST" CASE                       **
C               **  (AND WITH THE VARIANCE NOT CHANGING WITH THE MEAN)  **
C               **********************************************************
C
 1400 CONTINUE
      DN=N
      DSUM=0.0D0
      DO1410I=1,N
      DX=X(I)
      DSUM=DSUM+DX
 1410 CONTINUE
      DMEAN=DSUM/DN
C
      DSUM=0.0D0
      DO1420I=1,N
      DX=X(I)
      DSUM=DSUM+(DX-DMEAN)**2
 1420 CONTINUE
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
C
      IF(DSD.LE.0.0D0)DTERM=(-999.99D0)
      IF(DSD.GT.0.0D0)DTERM=DSD
      IF(DSD.LE.0.0D0)DTERM2=(-999.99D0)
      IF(DSD.GT.0.0D0)DTERM2=DABS(DTERM)
      IF(DSD.LE.0.0D0)DTERM3=(-999.99D0)
      IF(DSD.GT.0.0D0)DTERM3=DLOG10(DTERM2)
      XTAGUC=(-20.0D0*DTERM3)
C
      IF(IFEEDB.EQ.'OFF')GOTO1490
      IF(IWRITE.EQ.'OFF')GOTO1490
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)
 1482 FORMAT('(FOR THE "TARGET IS BEST WITH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1483)
 1483 FORMAT('VARIANCE NOT DEPENDENT ON MEAN" CASE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1484)N,XTAGUC
 1484 FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1490 CONTINUE
      GOTO9000
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
 9011 FORMAT('***** AT THE END       OF TAGUCH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ICASPL,IWRITE,IBUGA3,IERROR
 9012 FORMAT('ICASPL,IWRITE,IBUGA3,IERROR = ',
     1A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DMEAN,DSD
 9014 FORMAT('DMEAN,DSD = ',2D15.7)
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING 2 LINES WERE FIXED   APRIL 1992
CCCCC WRITE(ICOUT,9015)DRATIO,DTERM,DTERM2,DTERM3
C9015 FORMAT('DRATIO,DTERM,DTERM2,DTERM3 = ',4E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)DTERM,DTERM2,DTERM3
 9015 FORMAT('DTERM,DTERM2,DTERM3 = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)XTAGUC
 9016 FORMAT('XTAGUC = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE TCDF(X,ANU,CDF)
CCCCC SUBROUTINE TCDF(X,NU,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
C               --FEDERIGHI, EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S
C                 T-DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1959, PAGES 683-688.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 27-30.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 132-134.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --MAY       1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --OCTOBER   1976.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
C                                       DEGREES OF FREEDOM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,DNU,PI,C,CSQ,S,SUM,TERM,AI
      DOUBLE PRECISION DSQRT,DATAN
      DOUBLE PRECISION DCONST
      DOUBLE PRECISION TERM1,TERM2,TERM3
      DOUBLE PRECISION DCDFN
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION B11
      DOUBLE PRECISION B21,B22,B23,B24,B25
      DOUBLE PRECISION B31,B32,B33,B34,B35,B36,B37
      DOUBLE PRECISION D1,D3,D5,D7,D9,D11
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DBETAI
C
      EXTERNAL DBETAI
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
      DATA NUCUT/1000/
      DATA PI/3.14159265358979D0/
      DATA DCONST/0.3989422804D0/
      DATA B11/0.25D0/
      DATA B21/0.01041666666667D0/
      DATA B22,B23,B24,B25/3.0D0,-7.0D0,-5.0D0,-3.0D0/
      DATA B31/0.00260416666667D0/
      DATA B32,B33,B34,B35,B36,B37/1.0D0,-11.0D0,14.0D0,6.0D0,
     1                            -3.0D0,-15.0D0/
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GOTO8000
C
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',
     1         'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
C               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
C               **  SET CDF = 0.0 AND RETURN.                     **
C               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
C               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
C               **  SET CDF = 0.0 AND RETURN.                     **
C               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
C               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
C               **  SET CDF = 1.0 AND RETURN.                     **
C               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
C               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
C               **  SET CDF = 1.0 AND RETURN.                     **
C               ****************************************************
C
      DX=X
      ANU=NU
      DNU=NU
C
      IF(NU.LE.2)GOTO109
      SD=SQRT(ANU/(ANU-2.0))
      Z=X/SD
      IF(NU.LT.10.AND.Z.LT.-3000.0)GOTO107
      IF(NU.GE.10.AND.Z.LT.-150.0)GOTO107
      IF(NU.LT.10.AND.Z.GT.3000.0)GOTO108
      IF(NU.GE.10.AND.Z.GT.150.0)GOTO108
      GOTO109
  107 CDF=0.0
      GOTO9000
  108 CDF=1.0
      GOTO9000
  109 CONTINUE
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  DISTINGUISH BETWEEN THE SMALL AND MODERATE  **
C               **  DEGREES OF FREEDOM CASE VERSUS THE          **
C               **  LARGE DEGREES OF FREEDOM CASE               **
C               **************************************************
C
      IF(NU.LT.NUCUT)GOTO110
      GOTO250
C
C               ************************************************************
C               **  STEP 3.1--                                            **
C               **  TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE  **
C               **  METHOD UTILIZED--EXACT FINITE SUM                     **
C               **  (SEE AMS 55, PAGE 948, FORMULAE 26.7.3 AND 26.7.4).   **
C               ************************************************************
C
  110 CONTINUE
      C=DSQRT(DNU/(DX*DX+DNU))
      CSQ=DNU/(DX*DX+DNU)
      S=DX/DSQRT(DX*DX+DNU)
      IMAX=NU-2
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GOTO120
C
      SUM=C
      IF(NU.EQ.1)SUM=0.0D0
      TERM=C
      IMIN=3
      GOTO130
C
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
C
  130 IF(IMIN.GT.IMAX)GOTO160
      DO100I=IMIN,IMAX,2
      AI=I
      TERM=TERM*((AI-1.0D0)/AI)*CSQ
      SUM=SUM+TERM
  100 CONTINUE
C
  160 SUM=SUM*S
      IF(IEVODD.EQ.0)GOTO170
      SUM=(2.0D0/PI)*(DATAN(DX/DSQRT(DNU))+SUM)
  170 CDF=0.5D0+SUM/2.0D0
      GOTO9000
C
C               **************************************************************
C               **  STEP 3.2--                                              **
C               **  TREAT THE LARGE DEGREES OF FREEDOM CASE.                **
C               **  METHOD UTILIZED--TRUNCATED ASYMPTOTIC EXPANSION         **
C               **  (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 102, FORMULA 10;  **
C               **  SEE FEDERIGHI, PAGE 687).                               **
C               **************************************************************
C
  250 CONTINUE
      CALL NORCDF(X,CDFN)
      DCDFN=CDFN
      D1=DX
      D3=DX**3
      D5=DX**5
      D7=DX**7
      D9=DX**9
      D11=DX**11
      TERM1=B11*(D3+D1)/DNU
      TERM2=B21*(B22*D7+B23*D5+B24*D3+B25*D1)/(DNU**2)
      TERM3=B31*(B32*D11+B33*D9+B34*D7+B35*D5+B36*D3+B37*D1)/(DNU**3)
      DCDF=TERM1+TERM2+TERM3
      DCDF=DCDFN-(DCONST*(DEXP(-DX*DX/2.0D0)))*DCDF
      CDF=DCDF
      GOTO9000
C
CCCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
C
 8000 CONTINUE
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',
     1         'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
C
      DX=DBLE(X)
      DNU=DBLE(ANU)
C
      DTERM1=1.0D0/(1.0D0 + DX*DX/DNU)
      DTERM2=DNU/2.0D0
      DTERM3=0.5D0
      DTERM4=DBETAI(DTERM1,DTERM2,DTERM3)
      IF(DX.EQ.0.0D0)THEN
        DCDF=0.5D0
      ELSEIF(DX.LE.0.0D0)THEN
        DCDF=0.5D0*DTERM4
      ELSE
        DCDF=1.0D0 - 0.5D0*DTERM4
      ENDIF
      CDF=REAL(DCDF)
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TCRUDE( NDIM, MAXPTS, ABSEST, FINEST, IR )
*
*     Crude Monte-Carlo Algorithm for Deak method with
*      weighted results on restart
*
      INTEGER NDIM, MAXPTS, M, K, IR, NPTS
      DOUBLE PRECISION FINEST, ABSEST, X(100), SPMVT, UNI, 
     *     VARSQR, VAREST, VARPRD, FINDIF, FINVAL
      SAVE VAREST
      IF ( IR .LE. 0 ) THEN
         VAREST = 0.0D0
         FINEST = 0.0D0
      ENDIF
      FINVAL = 0.0D0
      VARSQR = 0.0D0
      DO 100 M = 1, MAXPTS
         FINDIF = ( SPMVT(NDIM) - FINVAL )/DBLE(M)
         FINVAL = FINVAL + FINDIF
         VARSQR = DBLE( M - 2 )*VARSQR/DBLE(M) + FINDIF**2 
  100 CONTINUE
      VARPRD = VAREST*VARSQR
      FINEST = FINEST + ( FINVAL - FINEST )/(1.0D0 + VARPRD)
      IF ( VARSQR .GT. 0.0D0 ) VAREST = (1.0D0 + VARPRD)/VARSQR
      ABSEST = 3.0D0*SQRT( VARSQR/( 1.0D0 + VARPRD ) )
C
      RETURN
      END
      FUNCTION TFN(X, FX)
C
C Two versions of algorithm AS 76 are given here; the original with one
C correction incorporated, and AS R55, also amended.   AS R55 requires
C AS 76.   N.B. The accuracy of AS 76 could be increased by using more
C Gaussian quadrature points, or better, by using Hermite integration.
C
C
C     ALGORITHM AS 76  APPL. STATIST. (1974) VOL.23, NO.3
C
C     Calculates the T-function of Owen, using Gaussian quadrature.
C     Incorporates correction AS R30 (vol.28, no.1, 1979)
C
      REAL U(5), R(5)
C
      DATA U /0.0744372, 0.2166977, 0.3397048, 0.4325317, 0.4869533/
      DATA R /0.1477621, 0.1346334, 0.1095432, 0.0747257, 0.0333357/
      DATA NG,    TP,    TV1,     TV2,     TV3,     TV4
     *   /  5, 0.159155, 1.E-35,  15.0,    15.0,   1.E-5 /
      DATA ZERO, QUART, HALF, ONE,  TWO
     *   / 0.0,  0.25,  0.5,  1.0,  2.0 /
C
C     Test for X near zero
C
      IF (ABS(X) .GE. TV1) GO TO 5
      TFN = TP * ATAN(FX)
      RETURN
C
C     Test for large values of abs(X)
C
    5 IF (ABS(X) .GT. TV2) GO TO 10
C
C     Test for FX near zero
C
      IF (ABS(FX) .GE. TV1) GO TO 15
   10 TFN = ZERO
      RETURN
C
C     Test whether abs(FX) is so large that it must be truncated
C
   15 XS = -HALF * X * X
      X2 = FX
      FXS = FX * FX
      IF (LOG(ONE + FXS) - XS * FXS .LT. TV3) GO TO 25
C
C     Computation of truncation point by Newton iteration
C
      X1 = HALF * FX
      FXS = QUART * FXS
   20 RT = FXS + ONE
      X2 = X1 + (XS * FXS + TV3 - LOG(RT)) / (TWO * X1 * (ONE/RT - XS))
      FXS = X2 * X2
      IF (ABS(X2 - X1) .LT. TV4) GO TO 25
      X1 = X2
      GO TO 20
C
C     Gaussian quadrature
C
   25 RT = ZERO
      DO 30 I = 1, NG
      R1 = ONE + FXS * (HALF + U(I))**2
      R2 = ONE + FXS * (HALF - U(I))**2
      RT = RT + R(I) * (EXP(XS * R1) / R1 + EXP(XS * R2) / R2)
   30 CONTINUE
      TFN = RT * X2 * TP
C
      RETURN
      END
      REAL FUNCTION THA(H1, H2, A1, A2)
C
C     AS R55  APPL. STATIST. (1985) VOL.34, NO.1
C
C     A remark on AS 76
C     Incorporating improvements in AS R80 (Appl. Statist. (1989) 
C     vol.38, no.3), and AS R89 (Appl. Statist. (1992) vol.41, no.2).
C
C     Computes T(H1/H2, A1/A2) for any real numbers H1, H2, A1 and A2
C
C     Auxiliary function required: ALNORM (= AS 66) and AS 76
C
      REAL A, A1, A2, G, H, H1, H2, TFN, ABSA, AH, GH, GAH,
     *  TWOPI, LAM, EX, C1, C2,
     *  ZERO, ONE, TWO, PT3, SEVEN, HALF, SIX, QUART
C
      DATA TWOPI /6.2831853/, ZERO /0.0/, ONE /1.0/, TWO /2.0/,
     *   PT3 /0.3/, SEVEN /7.0/, HALF /0.5/, SIX /6.0/, QUART /0.25/
C
      IF (H2 .NE. ZERO) GO TO 1
      THA = ZERO
      RETURN
C
    1 H = H1 / H2
      IF (A2 .EQ. ZERO) GO TO 2
      A = A1 / A2
      IF ((ABS(H) .LT. PT3) .AND. (ABS(A) .GT. SEVEN)) GO TO 6
C
C     Correction AS R89
C
      ABSA = ABS(A)
      IF (ABSA .GT. ONE) GO TO 7
      THA = TFN(H, A)
      RETURN
    7 AH = ABSA * H
CNIST GH = ALNORM(H, .FALSE.)
      CALL NORCDF(H,GH)
CNIST GAH = ALNORM(AH, .FALSE.)
      CALL NORCDF(AH,GAH)
      THA = HALF * (GH + GAH) - GH * GAH - TFN(AH, ONE/ABSA)
      IF (A .LT. ZERO) THA = - THA
      RETURN
C
 2    CONTINUE
CNIST G = ALNORM(H, .FALSE.)
      CALL NORCDF(H,G)
      IF (H .GE. ZERO) GO TO 3
      THA = G / TWO
      GO TO 4
    3 THA = (ONE - G) / TWO
    4 IF (A1 .GE. ZERO) RETURN
      THA = -THA
      RETURN
C
    6 LAM = ABS(A * H)
      EX = EXP(-LAM * LAM / TWO)
CNIST G = ALNORM(LAM, .FALSE.)
      CALL NORCDF(LAM,G)
      C1 = (EX/LAM + SQRT(TWOPI) * (G - HALF)) / (TWOPI)
      C2 = ((LAM * LAM + TWO) * EX/LAM**3 + SQRT(TWOPI) * (G - HALF))
     *       / (SIX * TWOPI)
      AH = ABS(H)
      THA = QUART - C1 * AH + C2 * AH**3
      THA = SIGN(THA, A)
C
      RETURN
      END
      SUBROUTINE TDCDF(DX,DNU,DCDF)
CCCCC SUBROUTINE TDCDF(DX,NU,DCDF)
C
C     DOUBLE PRECISION VERSION OF TCDF.  CALLED BY SKEW-T DISTRIBUTION.
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
C               --FEDERIGHI, EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S
C                 T-DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1959, PAGES 683-688.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 27-30.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 132-134.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003.12
C     ORIGINAL VERSION--DECEMBER  2003.
C     UPDATED         --JANUARY   2017. ALLOW NON-INTEGER DEGREES OF
C                                       FREEDOM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,DNU,PI,C,CSQ,S,SUM,TERM,AI
      DOUBLE PRECISION DSQRT,DATAN
      DOUBLE PRECISION DCONST
      DOUBLE PRECISION TERM1,TERM2,TERM3
      DOUBLE PRECISION DTERM1,DTERM2,DTERM3
      DOUBLE PRECISION DCDFN
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION B11
      DOUBLE PRECISION B21,B22,B23,B24,B25
      DOUBLE PRECISION B31,B32,B33,B34,B35,B36,B37
      DOUBLE PRECISION D1,D3,D5,D7,D9,D11
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
      DATA NUCUT/1000/
      DATA PI/3.14159265358979D0/
      DATA DCONST/0.3989422804D0/
      DATA B11/0.25D0/
      DATA B21/0.01041666666667D0/
      DATA B22,B23,B24,B25/3.0D0,-7.0D0,-5.0D0,-3.0D0/
      DATA B31/0.00260416666667D0/
      DATA B32,B33,B34,B35,B36,B37/1.0D0,-11.0D0,14.0D0,6.0D0,
     1                            -3.0D0,-15.0D0/
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      NU=INT(DNU)
      IF(ABS(DNU-DBLE(NU)).GT.0.000001D0)GOTO8000
C
      IF(NU.LE.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE DEGREES OF FREEDOM PARAMETER ',
     1         'TO THE TDCDF SUBROUTINE IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)NU
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8   ,'*****')
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GOTO9000
      ENDIF
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
C               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
C               **  SET CDF = 0.0 AND RETURN.                     **
C               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
C               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
C               **  SET CDF = 0.0 AND RETURN.                     **
C               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
C               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
C               **  SET CDF = 1.0 AND RETURN.                     **
C               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
C               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
C               **  SET CDF = 1.0 AND RETURN.                     **
C               ****************************************************
C
      ANU=NU
      DNU=NU
C
      IF(NU.LE.2)GOTO109
      SD=SQRT(ANU/(ANU-2.0))
      Z=REAL(DX)/SD
      IF(NU.LT.10.AND.Z.LT.-3000.0)GOTO107
      IF(NU.GE.10.AND.Z.LT.-150.0)GOTO107
      IF(NU.LT.10.AND.Z.GT.3000.0)GOTO108
      IF(NU.GE.10.AND.Z.GT.150.0)GOTO108
      GOTO109
  107 DCDF=0.0D0
      GOTO9000
  108 DCDF=1.0D0
      GOTO9000
  109 CONTINUE
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  DISTINGUISH BETWEEN THE SMALL AND MODERATE  **
C               **  DEGREES OF FREEDOM CASE VERSUS THE          **
C               **  LARGE DEGREES OF FREEDOM CASE               **
C               **************************************************
C
      IF(NU.LT.NUCUT)GOTO110
      GOTO250
C
C               ************************************************************
C               **  STEP 3.1--                                            **
C               **  TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE  **
C               **  METHOD UTILIZED--EXACT FINITE SUM                     **
C               **  (SEE AMS 55, PAGE 948, FORMULAE 26.7.3 AND 26.7.4).   **
C               ************************************************************
C
  110 CONTINUE
      C=DSQRT(DNU/(DX*DX+DNU))
      CSQ=DNU/(DX*DX+DNU)
      S=DX/DSQRT(DX*DX+DNU)
      IMAX=NU-2
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GOTO120
C
      SUM=C
      IF(NU.EQ.1)SUM=0.0D0
      TERM=C
      IMIN=3
      GOTO130
C
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
C
  130 IF(IMIN.GT.IMAX)GOTO160
      DO100I=IMIN,IMAX,2
      AI=I
      TERM=TERM*((AI-1.0D0)/AI)*CSQ
      SUM=SUM+TERM
  100 CONTINUE
C
  160 SUM=SUM*S
      IF(IEVODD.EQ.0)GOTO170
      SUM=(2.0D0/PI)*(DATAN(DX/DSQRT(DNU))+SUM)
  170 DCDF=0.5D0+SUM/2.0D0
      GOTO9000
C
C               **************************************************************
C               **  STEP 3.2--                                              **
C               **  TREAT THE LARGE DEGREES OF FREEDOM CASE.                **
C               **  METHOD UTILIZED--TRUNCATED ASYMPTOTIC EXPANSION         **
C               **  (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 102, FORMULA 10;  **
C               **  SEE FEDERIGHI, PAGE 687).                               **
C               **************************************************************
C
  250 CONTINUE
      CALL NODCDF(DX,DCDFN)
      D1=DX
      D3=DX**3
      D5=DX**5
      D7=DX**7
      D9=DX**9
      D11=DX**11
      TERM1=B11*(D3+D1)/DNU
      TERM2=B21*(B22*D7+B23*D5+B24*D3+B25*D1)/(DNU**2)
      TERM3=B31*(B32*D11+B33*D9+B34*D7+B35*D5+B36*D3+B37*D1)/(DNU**3)
      DCDF=TERM1+TERM2+TERM3
      DCDF=DCDFN-(DCONST*(DEXP(-DX*DX/2.0D0)))*DCDF
      GOTO9000
C
CCCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
C
 8000 CONTINUE
      IF(DNU.LE.0.0D0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',
     1         'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GOTO9000
      ENDIF
C
      DTERM1=1.0D0/(1.0D0 + DX*DX/DNU)
      DTERM2=DNU/2.0D0
      DTERM3=0.5D0
      DTERM4=DBETAI(DTERM1,DTERM2,DTERM3)
      IF(DX.EQ.0.0D0)THEN
        DCDF=0.5D0
      ELSEIF(DX.LE.0.0D0)THEN
        DCDF=0.5D0*DTERM4
      ELSE
        DCDF=1.0D0 - 0.5D0*DTERM4
      ENDIF
      CDF=REAL(DCDF)
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TDPDF(DX,NU,DPDF)
C
C     DOUBLE PRECISION VERSION OF TPDF.  USED BY SKEW T DISTRIBUTION.
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                                DX SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--DPDF   = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
C               --FEDERIGHI, EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S
C                 T-DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1959, PAGES 683-688.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 27-30.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 132-134.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2003.12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,DNU, DPDF
      DOUBLE PRECISION DSQTPI,DRATIO
      DOUBLE PRECISION DCONST,DPOWER
      DOUBLE PRECISION AI
      DOUBLE PRECISION DSQRT
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
      DATA DSQTPI/1.77245385090552D0/
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** FATAL ERROR--THE DEGREES OF FREEDOM PARAMETER ',
     1         'TO THE TDPDF SUBROUTINE IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
        CALL DPWRST('XXX','BUG ')
        DPDF=0.0
      ENDIF
C
C               ****************************************************
C               **  STEP 2--
C               **  COMPUTE THE CONSTANT = 1/(SQRT(NU)*BETA(1/2,NU/2))
C               **  = (1/(SQRT(NU)*SQRT(PI))) * (GAMMA((NU/2)+(1/2))/GAMMA(NU/2)
C               ****************************************************
C
      DNU=NU
C
      DRATIO=1.0D0
      IEVODD=NU-2*(NU/2)
      IMIN=3
      IF(IEVODD.EQ.0)IMIN=2
      IF(NU.LT.IMIN)GOTO250
      DO300I=IMIN,NU,2
      AI=I
      DRATIO=((AI-1.0D0)/AI)*DRATIO
  300 CONTINUE
  250 CONTINUE
      DRATIO=DRATIO*DNU
      IF(IEVODD.EQ.0)GOTO260
      DRATIO=DRATIO/DSQTPI
      GOTO400
  260 CONTINUE
      DRATIO=DRATIO*DSQTPI/2.0D0
  400 CONTINUE
C
      DCONST=DRATIO/(DSQTPI*DSQRT(DNU))
C
C               ************************************
C               **  STEP 3--                      **
C               **  COMPUTE THE DENSITY FUNCTION  **
C               ************************************
C
      DPOWER=-(DNU+1.0D0)/2.0D0
      DPDF=DCONST*((1.0D0+DX*DX/DNU)**DPOWER)
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE THRESH(Y,TVAL,N1,NT,IWRITE,Y2,TAG,NOUT,
     1                  ICASE,MAXNXT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--FOR EACH VALUE IN A GIVEN A SET OF THRESHOLDS (TVAL),
C              CREATE AN OUTPUT VECTOR EQUAL TO 1 IF THE INPUT RESPONSE
C              VARIABLE IS GREATER THAN (OR LESS THAN) THE THRESHOLD 
C              VALUE.  CREATE A CORREPONDING TAG VARIABLE.  FOR EXAMPLE,
C              IF THERE ARE 4 THRESHOLDS AND 25 OBSERVATIONS IN THE
C              RESPONSE VARIABLE, THE OUTPUT VARIABLES WILL HAVE
C              4*25=100 VALUES.  THIS COMMAND CAN BE HELPFUL IN
C              GENERATING ROC CURVES FROM RAW DATA.
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR CONTAINING
C                                THE INPUT RESPONSE VARIABLE.
C                     --TVAL   = THE SINGLE PRECISION VECTOR CONTAINING
C                                A LIST OF THRESHOLDS.
C                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR TVAL.
C                     --ICASE  = THE CHARACTER STRING THAT SPECIFIES
C                                THE DIRECTION OF THE THRESHOLD.
C     OUTPUT ARGUMENTS--Y2     = THE SINGLE PRECISION VECTOR
C                                CONTAINING 0 OR 1 DEPENDING ON WHETHER
C                                THE INPUT RESPONSE VARIABLE IS LESS
C                                THAN (OR GREATER THAN) THE THRESHOLD.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/7
C     ORIGINAL VERSION--JULY      2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TVAL(*)
      DIMENSION Y2(*)
      DIMENSION TAG(*)
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
      ISUBN1='THRE'
      ISUBN2='SH  '
C
      IERROR='NO'
      IWRITE='OFF'
      NOUT=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RESH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF THRESH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,NT
   52   FORMAT('IBUGA3,ISUBRO,N1,NT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N1
          WRITE(ICOUT,56)I,Y(I)
   56     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO65I=1,NT
          WRITE(ICOUT,66)I,TVAL(I)
   66     FORMAT('I,TVAL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
C
C    ********************************************
C    **  STEP 1--                              **
C    **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C    ********************************************
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN THRESHOLD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
  115   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THE NUMBER OF THRESHOLDS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)NT
  125   FORMAT('      THE NUMBER OF THRESHOLD VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NT*N1.GT.MAXNXT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      THE NUMBER OF OBSERVATIONS TIMES THE NUMBER ',
     1         'OF THRESHOLD VALUES TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)NT
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,135)MAXNXT
  135   FORMAT('      THE MAXIMUM NUMBER OF OUTPUT VALUES ALLOWED IS ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     ***************************************************
C     **  STEP 2--                                     **
C     **  GENERATE THE THRESHOLD VALUES                **
C     ***************************************************
C
      IF(ICASE.EQ.'MINI')THEN
        ICNT=0
        DO1010J=1,NT
          ACUT=TVAL(J)
          DO1020I=1,N1
            ICNT=ICNT+1
            IF(Y(I).GE.ACUT)THEN
              Y2(ICNT)=1.0
            ELSE
              Y2(ICNT)=0.0
            ENDIF
            TAG(ICNT)=REAL(J)
 1020     CONTINUE
 1010   CONTINUE
        NOUT=ICNT
      ELSEIF(ICASE.EQ.'MAXI')THEN
        ICNT=0
        DO2010J=1,NT
          ACUT=TVAL(J)
          DO2020I=1,N1
            ICNT=ICNT+1
            IF(Y(I).LE.ACUT)THEN
              Y2(ICNT)=1.0
            ELSE
              Y2(ICNT)=0.0
            ENDIF
            TAG(ICNT)=REAL(J)
 2020     CONTINUE
 2010   CONTINUE
        NOUT=ICNT
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RESH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF THRESH--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NOUT
          WRITE(ICOUT,9016)I,Y2(I),TAG(I)
 9016     FORMAT('I,Y2(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE TKTRBY(IBHIX,IBLOX,IBHIY,IBLOY,IFACTO,IX,IY)
C
C     PURPOSE--TRANSLATE 4 BYTES--
C              5 HIGH-ORDER BITS OF X
C              5 LOW -ORDER BITS OF X
C              5 HIGH-ORDER BITS OF Y
C              5 LOW -ORDER BITS OF Y
C              INTO 2 INTEGER TEKTRONIX COORDINATES
C              (0 TO 1023)
C
C     REFERENCE--TEKTRONIX 4014 TERMINAL USERS GUIDE, PAGE 3-31
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*1 IBHIX
      CHARACTER*1 IBLOX
      CHARACTER*1 IBHIY
      CHARACTER*1 IBLOY
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
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
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRBY')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TKTRBY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)IBHIX
   61 FORMAT('IBHIX = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)IBLOX
   62 FORMAT('IBLOX = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)IBHIY
   63 FORMAT('IBHIY = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)IBLOY
   64 FORMAT('IBLOY = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)IFACTO
   65 FORMAT('IFACTO = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
CCCCC IHIX=ICHAR(IBHIX)
      CALL DPCOAN(IBHIX,IHIX)
CCCCC ILOX=ICHAR(IBLOX)
      CALL DPCOAN(IBLOX,ILOX)
CCCCC IHIY=ICHAR(IBHIY)
      CALL DPCOAN(IBHIY,IHIY)
CCCCC ILOY=ICHAR(IBLOY)
      CALL DPCOAN(IBLOY,ILOY)
C
      IHIX2=MOD(IHIX,32)
      ILOX2=MOD(ILOX,32)
      IHIY2=MOD(IHIY,32)
      ILOY2=MOD(ILOY,32)
C
      IHIX3=IHIX2*32
      ILOX3=ILOX2
      IHIY3=IHIY2*32
      ILOY3=ILOY2
C
      IX3=IHIX3+ILOX3
      IY3=IHIY3+ILOY3
C
      IX4=IX3*4
      IY4=IY3*4
C
      IX=IX4/IFACTO
      IY=IY4/IFACTO
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRBY')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF TKTRBY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)IBHIX
 9021 FORMAT('IBHIX = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)IBLOX
 9022 FORMAT('IBLOX = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)IBHIY
 9023 FORMAT('IBHIY = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9024)IBLOY
 9024 FORMAT('IBLOY = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)IFACTO
 9025 FORMAT('IFACTO = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)IHIX,ILOX,IHIY,ILOY
 9031 FORMAT('IHIX,ILOX,IHIY,ILOY = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)IHIX2,ILOX2,IHIY2,ILOY2
 9032 FORMAT('IHIX2,ILOX2,IHIY2,ILOY2 = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9033)IHIX3,ILOX3,IHIY3,ILOY3
 9033 FORMAT('IHIX3,ILOX3,IHIY3,ILOY3 = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9034)IX3,IY3
 9034 FORMAT('IX3,IY3 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9035)IX4,IY4
 9035 FORMAT('IX4,IY4 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9036)IX,IY
 9036 FORMAT('IX,IY = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE TKTRPT(IXC,IYC,IFACTO,ICSTR,NCSTR,ISUBN0)
C
C     PURPOSE--TRANSLATE AN INTEGER PAIR OF COORDINATES
C              INTO A PACKED CHARACTER REPRESENTATION
C              THAT WILL BE UNDERSTOOD BY A TEKTRONIX
C              GRAPHICS DEVICE.
C     NOTE--THE RESULTING PACKED WORDS
C           WILL BE PLACED IN SPECIFIC ELEMENTS
C           OF THE CHARACTER*130 VARIABLE ICSTR(.:.).
C           THE VALUE OF THE VARIABLE    NCSTR
C           REPRESENTS THE NUMBER OF ELEMENTS IN ICSTR(.:.)
C           THAT HAVE ALREADY BEEN FILLED.
C           THE RESULTRING CHARACTER STING WILL GO INTO
C           THE NEXT AVAILABLE ELEMENTS OF ICSTR(.:.)
C           AND THE VALUE OF    NCSTR    WILL BE
C           UPDATED ACCORDINGLY.
C     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
C             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
C     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED TKTRPT
C                    (AND THEREBY HAVE WALKBACK INFORMATION).
C     REFERENCE--4105 PROGRAMMER'S REFERENCE MANUAL
C                PAGE 5-4
C     REFERENCE--MAHLON KELLY, BYTE, OCTOBER 1983,
C                PAGES 439 TO 442.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ISUBN0
C
      CHARACTER*130 ICSTR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
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
C-----DATA STATEMENTS----------------------------------------
C
      DATA K2/4/
      DATA K5/32/
      DATA K7/128/
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRPT')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TKTRPT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISUBN0
   52 FORMAT('ISUBN0 (NAME OF THE CALLING SUBROUTINE) = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IXC,IYC
   53 FORMAT('IXC,IYC = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IFACTO
   54 FORMAT('IFACTO = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)K2,K5,K7
   55 FORMAT('K2,K5,K7 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)IGUNIT
   56 FORMAT('IGUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)NCSTR
   63 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCSTR.LE.0)GOTO67
      DO65I=1,NCSTR
CCCCC IASCNE=ICHAR(ICSTR(I:I))
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
C
      IVX=IXC*IFACTO
      IVY=IYC*IFACTO
      IF(IVX.LT.0)IVX=0
      IF(IVY.LT.0)IVY=0
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  FORM THE HIGH Y 7-BIT BYTE--                     **
C               **  SHIFT THE Y VALUE TO THE RIGHT 7 PLACES;         **
C               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
C               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
C               *******************************************************
C
      NCSTR=NCSTR+1
      IBYTE1=MOD(IVY/K7,K5)+32
CCCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE1)
      CALL DPCONA(IBYTE1,ICSTR(NCSTR:NCSTR))
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  FORM THE EXTRA 7-BIT BYTE--                          **
C               **  KEEP ONLY THE RIGHT 2 PLACES OF THE Y VALUE;         **
C               **  THEN SHIFT THESE 2 PLACES TO THE LEFT  2 PLACES;     **
C               **  KEEP ONLY THE RIGHT 2 PLACES OF THE X VALUE;         **
C               **  PLACE A 1 IN THE 6TH AND 7TH PLACES FFOM THE RIGHT.  **
C               **  THEN MERGE THE 2 TOGETHER.                         **
C               ***********************************************************
C
      NCSTR=NCSTR+1
      IBYTE2=MOD(IVY,K2)*K2+MOD(IVX,K2)+96
CCCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE2)
      CALL DPCONA(IBYTE2,ICSTR(NCSTR:NCSTR))
C
C               ****************************************************************
C               **  STEP 3--                                                  **
C               **  FORM THE LOW Y 7-BIT BYTE--                               **
C               **  SHIFT THE Y VALUE TO THE RIGHT 2 PLACES;                  **
C               **  THEN KEEP ONLY THE RIGHT 5 PLACES;                        **
C               **  THEN PLACE A 1 IN THE 6TH AND 7TH PLACES FROM THE RIGHT.  **
C               ****************************************************************
C
      NCSTR=NCSTR+1
      IBYTE3=MOD(IVY/K2,K5)+96
CCCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE3)
      CALL DPCONA(IBYTE3,ICSTR(NCSTR:NCSTR))
C
C               *******************************************************
C               **  STEP 4--                                         **
C               **  FORM THE HIGH X 7-BIT BYTE--                     **
C               **  SHIFT THE X VALUE TO THE RIGHT 7 PLACES;         **
C               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
C               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
C               *******************************************************
C
      NCSTR=NCSTR+1
      IBYTE4=MOD(IVX/K7,K5)+32
CCCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE4)
      CALL DPCONA(IBYTE4,ICSTR(NCSTR:NCSTR))
C
C               *******************************************************
C               **  STEP 5--                                         **
C               **  FORM THE LOW X 7-BIT BYTE--                      **
C               **  SHIFT THE X VALUE TO THE RIGHT 2 PLACES;         **
C               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
C               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
C               *******************************************************
C
      NCSTR=NCSTR+1
      IBYTE5=MOD(IVX/K2,K5)+64
CCCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE5)
      CALL DPCONA(IBYTE5,ICSTR(NCSTR:NCSTR))
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRPT')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF TKTRPT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IXC,IYC
 9012 FORMAT('IXC,IYC = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IVX,IVY
 9013 FORMAT('IVX,IVY = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IFACTO
 9014 FORMAT('IFACTO = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)K2,K5,K7
 9015 FORMAT('K2,K5,K7 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IGUNIT
 9016 FORMAT('IGUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IBYTE1,IBYTE2,IBYTE3,IBYTE4,IBYTE5
 9017 FORMAT('IBYTE1,IBYTE2,IBYTE3,IBYTE4,IBYTE5 = ',5I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)NCSTR
 9023 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCSTR.LE.0)GOTO9027
      DO9025I=1,NCSTR
CCCCC IASCNE=ICHAR(ICSTR(I:I))
      CALL DPCOAN(ICSTR(I:I),IASCNE)
      WRITE(ICOUT,9026)I,ICSTR(I:I),IASCNE
 9026 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
 9027 CONTINUE
      WRITE(ICOUT,9029)IBUGG4,ISUBG4,IERRG4
 9029 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE TNPCDF(X,GAMMA,A,NU,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
C              DISTRIBUTION WITH DOUBLE PRECISION 
C              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
C              A, AND UPPER TRUNCATION POINT NU.
C              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
C              CUMULATIVE DISTRIBUTION FUNCTION
C
C              F(X;GAMMA,A,NU) = 1 - 
C                                A**GAMMA*(X**(-GAMMA)-NU**(-GAMMA))/
C                                (1 - (A/NU)**GAMMA)
C                                GAMMA > 0; 0 <= A <= X <= NU
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE 
C                                AT WHICH THE CUMULATIVE DISTRIBUTION 
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE GREATER THAN
C                                OR EQUAL TO ALOC.
C                     --GAMMA  = THE DOUBLE PRECISION VALUE 
C                                OF THE TAIL LENGTH PARAMETER.
C                     --A      = THE DOUBLE PRECISION VALUE 
C                                OF THE LOWER BOUND PARAMETER.
C                     --NU     = THE DOUBLE PRECISION VALUE 
C                                OF THE UPPER TRUNCATION PARAMETER.
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE UPPER-TRUNCATED PARETO
C             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
C             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
C                   NON-NEGATIVE.
C                 --X SHOULD BE GREATER THAN OR EQUAL TO A.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--MARCH     2008. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NU
      REAL CPUMIN
      REAL CPUMAX
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      CDF=0.0D0
      IF(X.LT.A)THEN
        GOTO9000
      ELSEIF(X.GE.NU)THEN
        CDF=1.0D0
        GOTO9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15) 
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPCDF IS ',
     1       'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25) 
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPCDF IS ',
     1       'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35) 
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPCDF IS ',
     1       'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      DTERM1=(A**GAMMA)*(X**(-GAMMA) - NU**(-GAMMA))
      DTERM2=1.0D0 - (A/NU)**GAMMA
      CDF=1.0D0 - (DTERM1/DTERM2)
C
 9000 CONTINUE
      RETURN
      END 
      DOUBLE PRECISION FUNCTION TNPFUN (GAMMAT,X)
C
C     PURPOSE--THIS ROUTINE IS USED IN FINDING THE MAXIMUM LIKELIHOOD
C              ESTIMATE OF GAMMA FOR THE TRUNCATED PARETO
C              DISTRIBUTION.  IN PARTICULAR, IT SOLVES THE
C              EQUATION:
C
C              (R/GAMMAHAT) +
C              R*(X(R+1)/X(X(1))**GAMMAHAT*LOG(X(R+1)/X(1))/
C              1 - (X(R+1)/X(1))**GAMMAHAT) -
C              SUM[i=1 TO R][LOG(X(i) - LN(X(R+1))] = 0
C
C
C              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
C              FUNCTION.
C
C     EXAMPLE--TRUNCATED PARETO MAXIMUM LIKELIHOOD Y
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/3
C     ORIGINAL VERSION--MARCH      2008.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION GAMMAT
      DOUBLE PRECISION X(*)
C
      DOUBLE PRECISION DMNMAX
      DOUBLE PRECISION DSUM
      COMMON/TNPCOM/DMNMAX,DSUM,IR2
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DR
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
C  COMPUTE SOME SUMS
C
      DTERM1=DLOG(DMNMAX)
      DR=DBLE(IR2)
      DTERM2=DR/GAMMAT
      DTERM3=DR*(DMNMAX**GAMMAT)*DTERM1/(1.0D0 - DMNMAX**GAMMAT)
      TNPFUN=DTERM2 + DTERM3 - DSUM
C
      RETURN
      END
      SUBROUTINE TNPML1(Y,N,IR,DTEMP1,
     1                  XMEAN,XSD,XMIN,XMAX,
     1                  AML,ANUML,GAMMML,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
C              FOR THE TRUNCATED PARETO DISTRIBUTION FOR THE RAW DATA
C              CASE (I.E., NO CENSORING AND NO GROUPING).
C
C              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
C              PERFORMED.
C
C              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
C              FROM MULTIPLE PLACES (DPMLTP WILL GENERATE THE OUTPUT
C              FOR THE TRUNCATED PARETO MLE COMMAND).
C
C              THE CONDITIONAL MAXIMUM LIKELIHOOD ESTIMATE OF
C              NU IS THE DATA MAXIMUM.
C
C              TO FIND THIS ESTIMATE, SORT THE DATA FROM LARGEST
C              TO SMALLEST VALUE.  IF THERE ARE R+1 POINTS, THE
C              MAXIMUM LIKELIHOOD ESTIMATE OF GAMMA IS THE SOLUTION
C              OF THE EQUATION
C
C              (R/GAMMAHAT) +
C              R*(X(R+1)/X(X(1))**GAMMAHAT*LOG(X(R+1)/X(1))/
C              1 - (X(R+1)/X(1))**GAMMAHAT) -
C              SUM[i=1 TO R][LOG(X(i) - LN(X(R+1))] = 0
C
C              THIS TERMINOLOGY IS USED BY ABAN, MEERSCHAERT, AND
C              PANORSKA.  THEY BASE THIS ON TAKING THE LARGEST
C              R+1 POINTS OUT OF N (I.E., THE TRUNCATED PARETO
C              IS FIT TO THE TAILS OF THE DATA).  IN DATAPLOT,
C              IF R IS SPECIFIED, IT IS ASUMED THAT WE ARE FITTING
C              THE ENTIRE DATA SET.  SO IN THE ABOVE FORMULA,
C              X(1) IS THE MAXIMUM AND X(R+1) IS THE MINIMUM
C              POINT INCLUDED IN THE COMPUTATION.
C                
C
C              ONCE WE HAVE THE ESTIMATE OF GAMMA, THE ESTIMATE
C              OF THE LOWER BOUND PARAMETER IS:
C
C              AHAT = R**(1/GAMMAHAT)*(X(R+1))*
C              [N - (N - R)*(X(R+1_/X(1))**GAMMAHAT]**(-1/GAMMAHAT)
C
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/07
C     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
C                                       SUBROUTINE (FROM DPMLTP),
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION Y(*)
      DOUBLE PRECISION DTEMP1(*)
C
      DOUBLE PRECISION TNPFUN
      EXTERNAL TNPFUN
C
      DOUBLE PRECISION DMNMAX
      DOUBLE PRECISION DSUM
      COMMON/TNPCOM/DMNMAX,DSUM,IR2
C
      DOUBLE PRECISION DAE
      DOUBLE PRECISION DRE
      DOUBLE PRECISION DXSTRT
      DOUBLE PRECISION DXLOW
      DOUBLE PRECISION DXUP
      DOUBLE PRECISION XLOWSV
      DOUBLE PRECISION XUPSV
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
      ISUBN1='TNPM'
      ISUBN2='L1  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TNPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CARRY OUT CALCULATIONS                **
C               **  FOR TRUNCATED PARETO MLE ESTIMATE     **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AML=CPUMIN
      ANUML=CPUMIN
      GAMMML=CPUMIN
C
      IWRITE='OFF'
      IDIST='TRUNCATED PARETO'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      AN=REAL(N)
      IF(IR.LE.3 .OR. IR.GT.N)THEN
        IR=N-1
        IRP1=N
      ENDIF
      CALL SORT(Y,N,Y)
      N=IRP1
C
      DO180I=1,N
        DTEMP1(I)=DBLE(Y(I))
  180 CONTINUE
      DSUM=0.0D0
      DO190I=IR,1,-1
        DSUM=DSUM + (DLOG(DTEMP1(I)) - DLOG(DBLE(XMIN)))
  190 CONTINUE
      DMNMAX=DTEMP1(1)/DTEMP1(N)
      IR2=IR
C
      ANUML=XMAX
C
      DXSTRT=0.8D0
      DXLOW=0.001D0
      DXUP=10.0D0
      DAE=2.0*0.000001D0*DXSTRT
      DRE=DAE
      IFLAG=0
      ITBRAC=0
      CALL DFZER2(TNPFUN,DXLOW,DXUP,DXSTRT,DRE,DAE,IFLAG,DTEMP1)
      GAMMML=REAL(DXLOW)
C
      AR=REAL(IR)
      AMNMAX=REAL(DMNMAX)
      AML=AR**(1.0/GAMMML)*XMIN*
     1    (AN - (AN - AR)*(AMNMAX**GAMMML))**(-1.0/GAMMML)
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF TNPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XSD,XMIN,XMAX
 9055   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,6G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9057)AML,ANUML,GAMMML
 9057   FORMAT('AML,ANUML,GAMMML = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TNPPDF(X,GAMMA,A,NU,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
C              DISTRIBUTION WITH DOUBLE PRECISION 
C              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
C              A, AND UPPER TRUNCATION POINT NU.
C              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
C              PROBABILITY DENSITY FUNCTION
C
C              f(X;GAMMA,A,NU) = GAMMA*(A**GAMMA)*(X**(-GAMMA - 1))/
C                                (1 - (A/NU)**GAMMA)
C                                GAMMA > 0; 0 <= A <= X <= NU
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE 
C                                AT WHICH THE PROBABILITY DENSITY 
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE GREATER THAN
C                                OR EQUAL TO ALOC.
C                     --GAMMA  = THE DOUBLE PRECISION VALUE 
C                                OF THE TAIL LENGTH PARAMETER.
C                     --A      = THE DOUBLE PRECISION VALUE 
C                                OF THE LOWER BOUND PARAMETER.
C                     --NU     = THE DOUBLE PRECISION VALUE 
C                                OF THE UPPER TRUNCATION PARAMETER.
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE UPPER-TRUNCATED PARETO
C             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
C             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
C                   NON-NEGATIVE.
C                 --X SHOULD BE GREATER THAN OR EQUAL TO A.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--MARCH     2008. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL CPUMIN
      REAL CPUMAX
      DOUBLE PRECISION NU
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0D0
      IF(X.LT.A .OR. X.GT.NU)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNPPDF IS ',
     1         'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5)A,NU
    5   FORMAT('      INTERVAL (',G15.7,',',G15.7,').')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15) 
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPPDF IS ',
     1       'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25) 
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPPDF IS ',
     1       'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35) 
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPPDF IS ',
     1       'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      DTERM1=GAMMA*(A**GAMMA)*(X**(-GAMMA-1.0D0))
      DTERM2=1.0D0 - (A/NU)**GAMMA
      PDF=DTERM1/DTERM2
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TNPPPF(P,GAMMA,A,NU,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
C              DISTRIBUTION WITH DOUBLE PRECISION 
C              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
C              A, AND UPPER TRUNCATION POINT NU.
C              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
C              PERCENT POINT FUNCTION
C
C              G(P;GAMMA,A,NU) = [C2 + (C3/C1)*(1-P)]**(-1/GAMMA)
C                                GAMMA > 0; A >= 0; 0 <= P <= 1
C
C     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE 
C                                AT WHICH THE PERCENT POINT 
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE DOUBLE PRECISION VALUE 
C                                OF THE TAIL LENGTH PARAMETER.
C                     --A      = THE DOUBLE PRECISION VALUE 
C                                OF THE LOWER BOUND PARAMETER.
C                     --NU     = THE DOUBLE PRECISION VALUE 
C                                OF THE UPPER TRUNCATION PARAMETER.
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF FOR THE UPPER-TRUNCATED PARETO
C             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
C             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
C                   NON-NEGATIVE.
C                 --0 <= P <= 1.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--MARCH     2008. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NU
      REAL CPUMIN
      REAL CPUMAX
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0D0
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNPPPF IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15) 
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPPPF IS ',
     1       'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25) 
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPPPF IS ',
     1       'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35) 
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPPPF IS ',
     1       'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',
     1         G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(P.LE.0.0D0)THEN
        PPF=A
      ELSEIF(P.GE.1.0D0)THEN
        PPF=NU
      ELSE
        C1=A**GAMMA
        C2=NU**(-GAMMA)
        C3=1.0D0 - (A/NU)**GAMMA
        PPF=(C2 + (C3/C1)*(1.0D0 - P))**(-1.0D0/GAMMA)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TNPRAN(N,GAMMA,A,NU,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TRUNCATED PARETO DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = GAMMA, LOWER
C              BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                     --A      = THE SINGLE PRECISION VALUE 
C                                OF THE LOWER BOUND PARAMETER.
C                     --NU     = THE SINGLE PRECISION VALUE OF THE
C                                UPPER TRUNCATION PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TRUNCATED PARETO DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE GAMMA, LOWER BOUND
C             PARAMETER VALUE A, AND UPPER TRUNCATION PARAMETER NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --GAMMA AND A SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TNPPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
C                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 VOL. 101, NO. 473, PP. 270-277.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008.3
C     ORIGINAL VERSION--MARCH     2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      REAL NU
      DOUBLE PRECISION DTEMP
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
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF TRUNCATED ',
     1         'PARETO RANDOM NUMBERS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15) 
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPRAN IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.LT.0.0)THEN
        WRITE(ICOUT,25) 
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPRAN ',
     1         'IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.GE.NU)THEN
        WRITE(ICOUT,35) 
   35   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPRAN IS ',
     1         'GREATER THAN OR EQUAL TO THE FOURTH ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)NU
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   48 FORMAT('***** THE VALUE OF THE LOWER BOUND PARAMETER IS ',
     1       G15.7)
   49 FORMAT('***** THE VALUE OF THE UPPER TRUNCATION PARAMETER IS ',
     1       G15.7)
C
      CALL UNIRAN(N,ISEED,X)
C
      DO100I=1,N
        CALL TNPPPF(DBLE(X(I)),DBLE(GAMMA),DBLE(A),DBLE(NU),DTEMP)
        X(I)=REAL(DTEMP)
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TNRCDF(DX,DA,DB,DU,DSD,DCDF)
C
C     NOTE--TRUNCATED-NORMAL PDF IS:
C              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
C                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
C           THE TNRCDF IS DEFINED FOR A<=X<=B
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/9
C     ORIGINAL VERSION--SEPTEMBER 1995.
C     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
C                                       DOUBLE PRECISION.
C     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
C                                       AS INDICATOR OF INFINITE
C                                       LIMIT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DU
      DOUBLE PRECISION DSD
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DARG1
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION CONST
      DOUBLE PRECISION ALL
      DOUBLE PRECISION AUL
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      DCDF=0.0D0
C
      ALL=DA
      AUL=DB
      IF(DA.EQ.DBLE(CPUMIN) .AND. DB.EQ.DBLE(CPUMIN))THEN
        DARG1=(DX-DU)/DSD
        CALL NODCDF(DARG1,DCDF)
        GOTO9999
      ELSEIF(DA.EQ.DBLE(CPUMIN))THEN
        ALL=DBLE(CPUMIN)
        AUL=DB
      ELSEIF(DB.EQ.DBLE(CPUMIN))THEN
        ALL=DA
        AUL=DBLE(CPUMIN)
      ELSEIF(DA.GT.DB)THEN
        ALL=DB
        AUL=DA
      ENDIF
C
      IF(DX.LE.ALL .AND. ALL.NE.DBLE(CPUMIN))THEN
        DCDF=0.0D0
        GOTO9999
      ELSEIF(DX.GE.AUL .AND. AUL.NE.DBLE(CPUMIN))THEN
        DCDF=1.0D0
        GOTO9999
      ENDIF 
C
      DARG1=(DX-DU)/DSD
      CALL NODCDF(DARG1,DTERM1)
C
      IF(AUL.EQ.DBLE(CPUMIN))THEN
        DTERM3=1.0D0
      ELSE
        CALL NODCDF((DBLE(AUL)-DU)/DSD,DTERM3)
      ENDIF
C
      IF(ALL.EQ.DBLE(CPUMIN))THEN
        DTERM2=0.0D0
      ELSE
        CALL NODCDF((DBLE(ALL)-DU)/DSD,DTERM2)
      ENDIF
C
      CONST=1.0D0/(DTERM3-DTERM2)
      DCDF=(DTERM1 - DTERM2)/(DTERM3 - DTERM2)
C
 9999 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION TNRFUN(XI)
C
C     PURPOSE--FOR A SINGLY TRUNCATED NORMAL DISTRIBUTION,
C              THE MAXIMUM LIKELIHOOD ESTIMATES OF THE MU
C              AND SIGMA PARAMETERS ARE: 
C
C                  SIGMAHAT = SQRT{S**2 + lambda(h,alphahat)*(XBAR - T)**2}
C                  MUHAT    = XBAR - lambda(h,alphahat)*(XBAR - T)
C
C              WHERE
C
C                   alphahat = S**2/(XBAR - T)**2
C                   h        = c/N
C                   N        = TOTAL NUMBER OF OBSERVATIONS
C                   n        = NUMBER OF NON-TRUNCATED OBSERVATIONS
C                   c        = NUMBER OF TRUNCATED OBSERVATIONS
C
C               XBAR AND S ARE THE MEAN AND SD OF THE NON-TRUNCATED
C               OBSERVATIONS.
C
C               LAMBDA(H,ALPHAHAT) IS A TABULATED VALUE IN THE
C               COHEN REFERENCE.  HOWEVER, WE DETERMINE IT BY
C               SOLVING THE FUNCTION
C
C                  ((1 - OMEGA(h,XI)*(OMEGA(h,XI) - XI))/
C                  (OMEGA(h,XI) - XI)**2) - S**2/(MU - T)**2
C
C               FOR XI WHERE
C
C                  OMEGA(h,XI) = (h/(1-h))*NORPDF(XI)/NORCDF(XI)
C
C               NOTE THAT XI IS THE STANDARDIZED TRUNCATION
C               POINT.  ONCE WE SOLVE FOR XI, WE PLUG IT INTO
C               THE FUNCTION
C
C                   LAMBDA = OMEGA(h,XI)/(OMEGA(h,XI) - XI)
C
C               NOTE THAT THERE MAY BE TWO SOLUTIONS TO THIS
C               EQUATION.  WE PICK THE ONE THAT RESULTS IN A
C               POSITIVE LAMBDA.
C
C               THIS FUNCTION IS USED IN SOLVING THE
C               LAMBDA(h,XI) FUNCTION FOR XI.
C
C     REFERENCE--CLIFFORD COHEN (1991), "TRUNCATED AND CENSORED
C                SAMPLES", MARCEL DEKKER INC., CHAPTER 2.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/12
C     ORIGINAL VERSION--DECEMBER  2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION XI
C
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DENOM
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DCDF
C
      DOUBLE PRECISION DH
      DOUBLE PRECISION DC1
      COMMON/TNRCOM/DC1,DH
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
      CALL NODPDF(XI,DPDF)
      CALL NODCDF(XI,DCDF)
      DOMEGA=(DH/(1.0D0-DH))*DPDF/DCDF
      DNUM=1.0D0 - DOMEGA*(DOMEGA - XI)
      DENOM=(DOMEGA - XI)**2
C
      TNRFUN=(DNUM/DENOM) - DC1
C
      RETURN
      END
      SUBROUTINE TNRPDF(DX,DA,DB,DU,DSD,DPDF)
C
C     NOTE--TRUNCATED-NORMAL PDF IS:
C              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
C                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
C           THE TNRPDF IS DEFINED FOR A<=X<=B
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
C                                       DOUBLE PRECISION.
C     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
C                                       AS INDICATOR OF INFINITE
C                                       LIMIT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DU
      DOUBLE PRECISION DSD
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DARG1
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION ALL
      DOUBLE PRECISION AUL
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
      DPDF=0.0D0
C
      ALL=DA
      AUL=DB
      IF(DA.EQ.DBLE(CPUMIN) .AND. DB.EQ.DBLE(CPUMIN))THEN
        DARG1=(DX-DU)/DSD
        CALL NODPDF(DARG1,DTERM1)
        DPDF=DTERM1/DSD
        GOTO9999
      ELSEIF(DA.EQ.DBLE(CPUMIN))THEN
        ALL=DBLE(CPUMIN)
        AUL=DB
      ELSEIF(DB.EQ.DBLE(CPUMIN))THEN
        ALL=DA
        AUL=DBLE(CPUMIN)
      ELSEIF(DA.GT.DB)THEN
        ALL=DB
        AUL=DA
      ENDIF
C
      IF((DX.LT.ALL.AND.ALL.NE.DBLE(CPUMIN)) .OR.
     1   (DX.GT.AUL.AND.AUL.NE.DBLE(CPUMIN)))THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)DA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)DB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNRPDF IS OUTSIDE ',
     1       'THE ALLOWABLE (A,B) INTERVAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF A IS ',G15.7)
   48 FORMAT('***** THE VALUE OF B IS ',G15.7)
C
      DARG1=(DX-DU)/DSD
      CALL NODPDF(DARG1,DTERM1)
      DTERM1=DTERM1/DSD
C
      IF(AUL.EQ.DBLE(CPUMIN))THEN
        DTERM2=1.0D0
      ELSE
        CALL NODCDF((AUL-DU)/DSD,DTERM2)
      ENDIF
C
      IF(ALL.EQ.DBLE(CPUMIN))THEN
        DTERM3=0.0D0
      ELSE
        CALL NODCDF((ALL-DU)/DSD,DTERM3)
      ENDIF
C
      DPDF=DTERM1/(DTERM2-DTERM3)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TNRPPF(P,A,B,U,SD,PPF)
C
C     PURPOSE   --PERCENT POINT FUNCTION FOR THE TRUNCATED NORMAL
C                 DISTRIBUTION.  USES A BISECTION METHOD.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--95/9
C     ORIGINAL VERSION--SEPTEMBER 1995.
C     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
C                                       DOUBLE PRECISION.
C     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
C                                       AS INDICATOR OF INFINITE
C                                       LIMIT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
      DOUBLE PRECISION P
      DOUBLE PRECISION P1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION U
      DOUBLE PRECISION SD
      DOUBLE PRECISION PPF
      DOUBLE PRECISION DPI
      DOUBLE PRECISION EPS
      DOUBLE PRECISION SIG
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION ALL
      DOUBLE PRECISION AUL
      DOUBLE PRECISION XL
      DOUBLE PRECISION XR
      DOUBLE PRECISION FCS
      DOUBLE PRECISION FXL
      DOUBLE PRECISION FXR
      DOUBLE PRECISION X
      DOUBLE PRECISION XINC
      DOUBLE PRECISION CDFL
      DOUBLE PRECISION CDFR
      DOUBLE PRECISION CDF
      DOUBLE PRECISION XRML
C
      CHARACTER*4 IDIR
C
      DATA DPI /3.14159265358979D0/
      DATA EPS /0.00000001D0/
      DATA SIG /1.0D-9/
      DATA ZERO /0.D0/
      DATA MAXIT /10000/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      ALL=A
      AUL=B
      IF(A.LE.DBLE(CPUMIN+1.0) .AND. B.LE.DBLE(CPUMIN+1.0))THEN
        CALL NODPPF(P,PPF)
        PPF=U + PPF/SD
        GOTO9999
      ELSEIF(A.LE.DBLE(CPUMIN+1.0))THEN
        ALL=DBLE(CPUMIN)
        AUL=B
      ELSEIF(B.LE.DBLE(CPUMIN+1.0))THEN
        ALL=A
        AUL=DBLE(CPUMIN)
      ELSEIF(A.GT.B)THEN
        ALL=B
        AUL=A
      ENDIF
C
      IFLAG=0
      IF(ALL.LE.DBLE(CPUMIN+1.0))THEN
        IF(P.LE.0.0D0)IFLAG=1
      ELSE
        IF(P.LT.0.0D0)IFLAG=1
      ENDIF
      IF(AUL.EQ.DBLE(CPUMIN))THEN
        IF(P.GE.1.0D0)IFLAG=1
      ELSE
        IF(P.GT.1.0D0)IFLAG=1
      ENDIF
C
      IF(IFLAG.EQ.1)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9999
      ELSEIF(SD.LE.0.0)THEN
        WRITE(ICOUT,35)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9999
      ENDIF
C
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNRPPF IS OUTSIDE ',
     1       ' THE ALLOWABLE (0,1) INTERVAL.')
   35 FORMAT('***** ERROR--THE FIFTH ARGUMENT TO TNRPPF IS NEGATIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C  FIND BRACKETING INTERVAL.
C  AFTER SUCCESSFULLY FIND BRACKETING INTERVAL, THEN SWITCH TO 
C  MORE EFFICIENT BISECTION METHOD.
C
      MAXIT=1000
      IF(ALL.GT.DBLE(CPUMINi+1.0) .AND. AUL.GT.DBLE(CPUMIN+1.0))THEN
        XL=ALL
        XR=AUL
        IF(P.LE.0.0D0)THEN
          PPF=ALL
          GOTO9999
        ELSEIF(P.GE.1.0D0)THEN
          PPF=AUL
          GOTO9999
        ENDIF
        GOTO99
      ELSEIF(ALL.GT.DBLE(CPUMIN+100.0))THEN
        XL=ALL
        XR=U
        XINC=SD
        IF(P.LE.0.0D0)THEN
          PPF=ALL
          GOTO9999
        ENDIF
        IDIR='RIGH'
      ELSEIF(AUL.GT.DBLE(CPUMIN+1.0))THEN
        XR=AUL
        XL=U
        XINC=SD
        IF(P.GE.1.0D0)THEN
          PPF=AUL
          GOTO9999
        ENDIF
        IDIR='LEFT'
      ENDIF
C
      ICOUNT=0
   91 CONTINUE
      IF(XL.LE.DBLE(CPUMIN+1.0))THEN
        CDFL=0.0D0
      ELSE
        CALL TNRCDF(XL,ALL,AUL,U,SD,CDFL)
      ENDIF
      IF(XR.LE.DBLE(CPUMIN+1.0))THEN
        CDFR=1.0D0
      ELSE
        CALL TNRCDF(XR,ALL,AUL,U,SD,CDFR)
      ENDIF
      IF(CDFL.LT.P .AND. CDFR.LT.P)THEN
        XR=XR+XINC
      ELSEIF(CDFL.GT.P .AND. CDFR.GT.P)THEN
        XL=XL-XINC
      ELSE
        GOTO99
      ENDIF
      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.MAXIT)THEN
        WRITE(ICOUT,96)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,97)P,A,B,U,SD
 97     FORMAT('P,A,B,U,SD=',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,98)XL,XR,ALL,AUL
 98     FORMAT('XL,XR,ALL,AUL=',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,909)CDFL,CDFR,ICOUNT
 909    FORMAT('CDFL,CDFR,ICOUNT=',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9999
      ENDIF
   96 FORMAT('***** ERROR--TNRPPF UNABLE TO FIND BRACKETING ',
     *       'INTERVAL.')
      GOTO91
C
C  BISECTION METHOD
C
   99 CONTINUE
      IC = 0
      FXL = -P
      FXR = 1.0D0 - P
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL TNRCDF(X,ALL,AUL,U,SD,CDF)
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
  130 FORMAT('***** ERROR--TNRPPF ROUTINE DID NOT CONVERGE.')
      GOTO9999
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TNRRAN(N,A,B,U,SD,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE THE TRUNCATED NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN = U AND STANDARD DEVIATION = SD.
C              THE TRUNCATED-NORMAL PDF IS:
C              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
C                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
C              THE TNRPDF IS DEFINED FOR A<=X<=B
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                       A      = A SINGLE PRECISION SCALAR THAT
C                                DEFINES THE LOWER TRUNCATION POINT.
C                       B      = A SINGLE PRECISION SCALAR THAT
C                                DEFINES THE UPPER TRUNCATION POINT.
C                       U      = A SINGLE PRECISION SCALAR THAT
C                                DEFINES THE LOCATION PARAMETER FOR
C                                THE PARENT NORMAL DISTRIBUTION.
C                       SD     = A SINGLE PRECISION SCALAR THAT
C                                DEFINES THE SCALE PARAMETER FOR
C                                THE PARENT NORMAL DISTRIBUTION.
C                       ISEED  = AN INTEGER NUMBER THAT DEFINES THE
C                                SEED FOR THE UNIFORM RANDOM NUMBER
C                                GENERATOR.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TRUNCATED NORMAL DISTRIBUTION.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG, SQRT, SIN, COS.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     METHOD--BOX-MULLER ALGORITHM USED TO GENERATE NORMAL RANDOM
C             NUMBERS, THEN REJECT IF GENERATED NUMBER OUTSIDE THE
C             TRUNCATION POINT.
C     REFERENCES--BOX AND MULLER, 'A NOTE ON THE GENERATION
C                 OF RANDOM NORMAL DEVIATES', JOURNAL OF THE
C                 ASSOCIATION FOR COMPUTING MACHINERY, 1958,
C                 PAGES 610-611.
C               --TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 33-34.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 39.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2003.7
C     ORIGINAL VERSION--JULY      2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(2)
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
      DATA PI/3.14159265359/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF TRUNCATED')
    6 FORMAT('      NORMAL RANDOM NUMBERS IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)SD
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   15 FORMAT('***** ERROR--THE REQUESTED STANDARD DEVIATION OF')
   16 FORMAT('      THE TRUNCATED NORMAL RANDOM NUMBERS IS ',
     1       'NON-POSITIVE.')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.7)
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C     THEN GENERATE 2 ADDITIONAL UNIFORM (0,1) RANDOM NUMBERS
C     (TO BE USED BELOW IN FORMING THE N-TH NORMAL
C     RANDOM NUMBER WHEN THE DESIRED SAMPLE SIZE N
C     HAPPENS TO BE ODD).
C
      CALL UNIRAN(2,ISEED,Y)
C
      NTEMP=2
      I=0
      INC=1
  100 CONTINUE
        I=I+INC
        IF(I.GT.N)GOTO9000
C
          CALL UNIRAN(NTEMP,ISEED,X(I))
C
C         GENERATE NORMAL RANDOM NUMBERS USING THE BOX-MULLER METHOD.
C
          IP1=I+1
          U1=X(I)
          IF(I.EQ.N)GOTO210
          U2=X(IP1)
          GOTO220
  210     U2=Y(2)
  220     ARG1=-2.0*LOG(U1)
          ARG2=2.0*PI*U2
          SQRT1=SQRT(ARG1)
          Z1=SQRT1*COS(ARG2)
          Z2=SQRT1*SIN(ARG2)
          Z1=U + SD*Z1
          Z2=U + SD*Z2
C
C         REJECT IF OUTSIDE THE BOUNDS
C
          INC=0
          IF(Z1.GE.A .AND. Z1.LE.B)THEN
            X(I)=Z1
            INC=INC + 1
          ENDIF
          IF(I.LT.N)THEN
            IF(Z2.GE.A .AND. Z2.LE.B)THEN
              X(IP1)=Z2
              INC=INC + 1
            ENDIF
          ENDIF
          GOTO100
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TNECDF(X,X0,U,SD,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
C              WITH MEAN = 0 AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
C              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S))  U<=X<=X0
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --X0     = SINGLE PRECISION VALUE DEFINING THE
C                                TRUNCATION POINT.
C                     --U      = SINGLE PRECISION VALUE DEFINING THE
C                                MEAN OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C                     --SD     = SINGLE PRECISION VALUE DEFINING THE
C                                SD OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2899
C     ORIGINAL VERSION--OCTOBER   1995. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DX0
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 ICOUTINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,ICOUTINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(X.LT.U.OR.X.GT.X0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)X0
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        IF(X.GE.X0)CDF=1.0
        GOTO9999
      ENDIF
      IF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9999
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9999
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9999
      ENDIF
    4 FORMAT('***** WARNING--THE FIRST  INPUT ARGUMENT TO ',
     1'THE TNECDF ROUTINE IS')
    5 FORMAT('      OUTSIDE THE (U,X0) INTERVAL')
   14 FORMAT('***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO ',
     1'THE TNECDF ROUTINE IS')
   15 FORMAT('      EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE THIRD INPUT ARGUMENT TO ',
     1'THE TNECDF ROUTINE IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE FOURTH INPUT ARGUMENT TO ',
     1'THE TNECDF ROUTINE IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF X0 IS ',E15.8,' *****')
   48 FORMAT('***** THE VALUE OF U IS ',E15.8,' *****')
C
C-----START POINT-----------------------------------------------------
C
      DX=DBLE((X-U)/SD)
      DX0=DBLE((X0-U)/SD)
      DTERM1=DEXP(DX)*(DEXP(-DX0)-1.0D0)
      DCDF=1.0D0/DTERM1
      ARG1=U
      CALL TNEPDF(ARG1,X0,U,SD,ARG2)
      CDF=SNGL(DCDF)+SD*ARG2
C
 9999 CONTINUE
      RETURN
      END 
      SUBROUTINE TNEPDF(X,X0,U,SD,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
C              WITH LOCATION = U AND SCALE = SD.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
C              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --X0     = SINGLE PRECISION VALUE DEFINING THE
C                                TRUNCATION POINT.
C                     --U      = SINGLE PRECISION VALUE DEFINING THE
C                                MEAN OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C                     --SD     = SINGLE PRECISION VALUE DEFINING THE
C                                SD OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2899
C     ORIGINAL VERSION--OCTOBER   1995. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DX0
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 ICOUTINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,ICOUTINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(X.LT.U.OR.X.GT.X0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)X0
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
      IF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
    4 FORMAT('***** FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO ',
     1'THE TNEPDF ROUTINE IS')
    5 FORMAT('      OUTSIDE THE (U,X0) INTERVAL')
   14 FORMAT('***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO ',
     1'THE TNEPDF ROUTINE IS')
   15 FORMAT('      EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE THIRD INPUT ARGUMENT TO ',
     1'THE TNEPDF ROUTINE IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE FOURTH INPUT ARGUMENT TO ',
     1'THE TNEPDF ROUTINE IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF X0 IS ',E15.8,' *****')
   48 FORMAT('***** THE VALUE OF U IS ',E15.8,' *****')
C
C-----START POINT-----------------------------------------------------
C
      DX=DBLE((X-U)/SD)
      DX0=DBLE((X0-U)/SD)
      DTERM1=-DX - DLOG(1.0D0-DEXP(-DX0)) - DLOG(DBLE(SD))
      DPDF=DEXP(DTERM1)
      PDF=SNGL(DPDF)
C
 9999 CONTINUE
      RETURN
      END 
      SUBROUTINE TNEPPF(P,X0,U,SD,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
C              THIS DISTRIBUTION IS DEFINED FOR U<=X<=X0
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --X0     = SINGLE PRECISION VALUE DEFINING THE
C                                TRUNCATION POINT.
C                     --U      = SINGLE PRECISION VALUE DEFINING THE
C                                MEAN OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C                     --SD     = SINGLE PRECISION VALUE DEFINING THE
C                                SD OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0 (INCLUSIVELY)
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
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
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--95/10
C     ORIGINAL VERSION--OCTOBER   1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
CCCCC DOUBLE PRECISION DP
CCCCC DOUBLE PRECISION DX0
CCCCC DOUBLE PRECISION DPPF
CCCCC DOUBLE PRECISION DTERM1
CCCCC DOUBLE PRECISION DTERM2
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA EPS /1.0E-6/
      DATA SIG /1.0E-5/
      DATA ZERO /0./
      DATA MAXIT /2000/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
    1 FORMAT('***** FATAL ERROR--THE 1ST  INPUT ARGUMENT TO THE ',
     1'TNEPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   14 FORMAT('***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO ',
     1'THE TNEPPF ROUTINE IS')
   15 FORMAT('      EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE THIRD INPUT ARGUMENT TO ',
     1'THE TNEPPF ROUTINE IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE FOURTH INPUT ARGUMENT TO ',
     1'THE TNEPPF ROUTINE IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF X0 IS ',E15.8,' *****')
   48 FORMAT('***** THE VALUE OF U IS ',E15.8,' *****')
C
      IF(P.EQ.0.0)THEN
        PPF=U
        GOTO9999
      ELSEIF(P.EQ.1.0)THEN
        PPF=X0
        GOTO9999
      ENDIF
C
CCCCC CALL TNEPDF(U,X0,U,SD,ARG2)
CCCCC DTERM1=DBLE(SD)*DBLE(ARG2)
CCCCC DP=DBLE(P)
CCCCC DX0=DBLE((X0-U)/SD)
CCCCC DTERM2=DEXP(-DX0)-1.0D0
CCCCC DTERM3=1.0D0/(DTERM2*(DP-DTERM1))
CCCCC IF(DTERM3.GT.0.0D0)THEN
CCCCC   DPPF=DLOG(1.0D0/(DTERM2*(DP-DTERM1)))
CCCCC ELSE
CCCCC   DPPF=0.0
CCCCC ENDIF
CCCCC PPF=U + S*SNGL(DPPF)
C
      IERR=0
      IC = 0
      XL = U
      XR = X0
      FXL = -P
      FXR = 1.0 - P
CCCCC INVALID P EXPLICITLY CHECKED FOR EARLIER.
CCCCC IF(FXL*FXR .GT. ZERO)GOTO50
C
C  BISECTION METHOD
C
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL TNECDF(X,X0,U,SD,P1)
      PPF=X
CCCCC IF(IERR.NE.0)THEN
CCCCC   WRITE(ICOUT,120)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC ENDIF
CC120 FORMAT('***** FATAL ERROR--ERROR IN BETCDF ROUTINE.  *****')
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
  130 FORMAT('***** FATAL ERROR--TNEPPF ROUTINE DID NOT CONVERGE. ***')
      GOTO9999
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TNERAN(N,X0,U,SD,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE THE TRUNCATED EXPONENTIAL DISTRIBUTION
C              WITH LOCATION = U AND SCALE = SD.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
C              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --X0     = SINGLE PRECISION VALUE DEFINING THE
C                                TRUNCATION POINT.
C                     --U      = SINGLE PRECISION VALUE DEFINING THE
C                                MEAN OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C                     --SD     = SINGLE PRECISION VALUE DEFINING THE
C                                SD OF THE PARENT EXPONENTIAL 
C                                DISTRIBUTION
C                       ISEED  = AN INTEGER NUMBER THAT DEFINES THE
C                                SEED FOR THE UNIFORM RANDOM NUMBER
C                                GENERATOR.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TRUNCATED EXPONENTIAL DISTRIBUTION.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     METHOD--BOX-MULLER ALGORITHM USED TO GENERATE NORMAL RANDOM
C             NUMBERS, THEN REJECT IF GENERATED NUMBER OUTSIDE THE
C             TRUNCATION POINT.
C     REFERENCES--TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 33-34.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 39.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 207-232.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGE 58.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2003.7
C     ORIGINAL VERSION--JULY      2003.
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
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.14159265359/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE REQUESTED NUMBER OF TRUNCATED')
    6 FORMAT('      EXPONENTIAL RANDOM NUMBERS IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      IF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)U
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)SD
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   14 FORMAT('***** FATAL DIAGNOSTIC--THE TRUNCATION PARAMETER FOR',
     1' THE TRUNCATED EXPONENTIAL DISTRIBUTION')
   15 FORMAT('      IS EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE LOCATION PARAMETER FOR THE ',
     1'TRUNCATED EXPONENTIAL DISTRIBUTION IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE SCALE PARAMETER FOR THE ',
     1'TRUNCATED EXPONENTIAL DISTRIBUTION IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE TRUNCATION PARAMETER IS ',E15.7)
   48 FORMAT('***** THE VALUE OF THE LOCATION PARAMETER IS ',E15.7)
   49 FORMAT('***** THE VALUE OF THE PARAMETER IS ',E15.7)
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C     THEN GENERATE 2 ADDITIONAL UNIFORM (0,1) RANDOM NUMBERS
C     (TO BE USED BELOW IN FORMING THE N-TH NORMAL
C     RANDOM NUMBER WHEN THE DESIRED SAMPLE SIZE N
C     HAPPENS TO BE ODD).
C
C
      NTEMP=1
      I=0
  100 CONTINUE
        I=I+1
        IF(I.GT.N)GOTO9000
  199   CONTINUE
C
        CALL UNIRAN(NTEMP,ISEED,X(I))
        X(I)=-LOG(X(I))
        X(I)=U + SD*X(I)
        IF(X(I).GT.X0)GOTO199
      GOTO100
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TOL(X,N,XMEAN,XSD,AN,
     1               X2TEMP,Y2TEMP,D2TEMP,
     1               ICASAN,ICAPSW,ICAPTY,IFORSW,
     1               PID,IVARID,IVARI2,NREPL,
     1               ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES NORMAL AND
C              DISTRIBUTION-FREE TOLERANCE LIMITS 
C              FOR THE DATA IN THE INPUT VECTOR X.
C              15 NORMAL TOLERANCE LIMITS ARE COMPUTED; AND 
C              30 DISTRIBUTION-FREE TOLERANCE LIMITS ARE COMPUTED.
C     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     OUTPUT--2 PAGES OF AUTOMATIC PRINTOUT--
C             1 PAGE GIVING NORMAL TOLERANCE LIMITS; AND
C             1 PAGE GIVING DISTRIBUTION-FREE TOLERANCE LIMITS.
C     PRINTING--YES.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, LOG, EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--GARDINER AND HULL (1966), "AN APPROXIMATION TO
C                 TWO-SIDED TOLERANCE LIMITS FOR NORMAL POPULATIONS",
C                 TECHNOMETRICS, 1966, PAGES 115-122.
C               --WILKS, ANNALS OF MATHEMATICAL STATISTICS, 1941, PAGE 92
C               --MOOD AND GRABLE, PAGES 416-417
C               --HOWE (1969), "TWO-SIDED TOLERANCE LIMITS FOR NORMAL
C                 POPULATIONS - SOME IMPROVEMENTS", JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION, VOL. 64, PP.
C                 610-620.
C               --GUENTHER (1977), "SAMPLING INSPECTION IN STATISTICAL
C                 QUALITY CONTROL", GRIFFIN'S STATISTICAL MONOGRAPHS,
C                 NUMBER 37, LONDON.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--JUNE      1972. 
C     UPDATED         --NOVEMBER  1975. 
C     UPDATED         --NOVEMBER  1998. CHANGES TO INCORPORATE INTO
C                                       DATAPLOT
C     UPDATED         --DECEMBER  2005. OPTIONALLY SELECT WHETHER
C                                       NORMAL/NON-PARAMETERIC CASES
C                                       PERFORMED
C     UPDATED         --MARCH     2011. USE DPDTA1 AND DPDTA5 TO PRINT
C                                       TABLES
C     UPDATED         --MAY       2014. SUPPORT FOR LOGNORMAL TOLERANCE
C                                       LIMITS
C     UPDATED         --MAY       2014. SUPPORT FOR "BOX-COX" TOLERANCE
C                                       LIMITS
C     UPDATED         --MAY       2018. SUPPORT FOR HOWE APPROXIMATION
C     UPDATED         --MAY       2018. SUPPORT FOR INDPENDENT DEGREES
C                                       OF FREEDOM
C     UPDATED         --MAY       2018. CORRECT LABELING OF CONFIDENCE
C                                       AND COVERAGE IN THE TABLE
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION X2TEMP(*)
      DIMENSION Y2TEMP(*)
      DIMENSION D2TEMP(*)
      DIMENSION PID(*)
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      PARAMETER (NCONF=6)
      PARAMETER (NCOV=3)
C
      DIMENSION PA(NCONF),PC(NCOV)
      DIMENSION RSMALL(5,6),USMALL(6,6)
      DIMENSION A(NCONF),B(NCONF),C(NCONF)
      DIMENSION Z1(NCOV)
      DIMENSION TMIN(NCOV,NCONF),TMAX(NCOV,NCONF),TK(NCOV,NCONF)
      DIMENSION P(10),C1(10),C2(10),C3(10)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=10)
      PARAMETER (MAXRO2=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*75 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
      INCLUDE 'DPCOST.INC'
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
C
      DATA PA/50.,75.,90.,95.,99.,99.9/
      DATA PC/90.,95.,99./
      DATA Z1(1),Z1(2),Z1(3)/-1.28155157,-1.64485363,-2.32634787/
      DATA A/.6745,1.1504,1.6449,1.9600,2.5758,3.2905/
      DATA B/.33734,.57335,.82140,.97910,1.2889,1.64038/
      DATA C/-0.15460,-0.02991,.22044,.40675,.85514,1.42601/
      DATA RSMALL(1,1),RSMALL(1,2),RSMALL(1,3),RSMALL(1,4),RSMALL(1,5),
     1RSMALL(1,6)            /1.0505,1.6859,2.2844,2.6463,3.3266,4.0903/
      DATA RSMALL(2,1),RSMALL(2,2),RSMALL(2,3),RSMALL(2,4),RSMALL(2,5),
     1RSMALL(2,6)            /0.8557,1.4333,2.0078,2.3624,3.0368,3.7983/
      DATA RSMALL(3,1),RSMALL(3,2),RSMALL(3,3),RSMALL(3,4),RSMALL(3,5),
     1RSMALL(3,6)            /0.7929,1.3412,1.8979,2.2457,2.9128,3.6708/
      DATA RSMALL(4,1),RSMALL(4,2),RSMALL(4,3),RSMALL(4,4),RSMALL(4,5),
     1RSMALL(4,6)            /0.7622,1.2940,1.8388,2.1815,2.8422,3.5965/
      DATA RSMALL(5,1),RSMALL(5,2),RSMALL(5,3),RSMALL(5,4),RSMALL(5,5),
     1RSMALL(5,6)            /0.7442,1.2654,1.8019,2.1408,2.7963,3.5472/
      DATA USMALL(1,1),USMALL(1,2),USMALL(1,3)/0.,0.,0./
      DATA USMALL(2,1),USMALL(2,2),USMALL(2,3)/7.9579,15.9472,79.7863/
      DATA USMALL(3,1),USMALL(3,2),USMALL(3,3)/3.0808,4.4154,9.9749/
      DATA USMALL(4,1),USMALL(4,2),USMALL(4,3)/2.2658,2.9200,5.1113/
      DATA USMALL(5,1),USMALL(5,2),USMALL(5,3)/1.9393,2.3724,3.6692/
      DATA USMALL(6,1),USMALL(6,2),USMALL(6,3)/1.7621,2.0893,3.0034/
      DATA P/50.,75.,90.,95.,97.5,99.,99.5,99.9,99.95,99.99/
C
      ISUBN1='TOL '
      ISUBN2='    '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TOL--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N,AN
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N,AN = ',3(A4,2X),I8,F12.2)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)XMEAN,XSD,PTOLDF,ITOLME,ITOLGC
   53   FORMAT('XMEAN,XSD,PTOLDF,ITOLME,ITOLGC = ',3G15.7,2(2X,A4))
        CALL DPWRST('XXX','WRIT')
        IF(XMEAN.EQ.CPUMIN)THEN
          DO56I=1,N
            WRITE(ICOUT,57)I,X(I)
   57       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(XMEAN.EQ.CPUMIN .AND. N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(XMEAN.EQ.CPUMIN)THEN
        HOLD=X(1)
        DO135I=2,N
          IF(X(I).NE.HOLD)GOTO139
  135   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,131)HOLD
  131   FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        GOTO9000
  139   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
C               **  LIMITS.                               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     MAY 2014: FOR LOGNORMAL TOLERANCE LIMITS, TAKE THE LOG OF THE
C               DATA, COMPUTE NORMAL TOLERANCE LIMITS, AND THEN
C               TAKE EXPONENT FOR THE LIMITS.
C
C               ALSO CAN DO A BOX-COX NORMALITY TRANSFORMATION.
C
C               NOTE THAT WE ONLY DO THIS FOR THE RAW DATA CASE.
C
      IF(ICASAN.EQ.'LNTO' .AND. XMEAN.EQ.CPUMIN)THEN
        DO210I=1,N
          IF(X(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,211)
  211       FORMAT('***** ERROR: LOGNORMAL TOLERANCE LIMITS--')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,212)I,X(I)
  212       FORMAT('      ROW ',I8,' IS NON-POSITIVE (',G15.7,')')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GOTO9000
          ELSE
            X(I)=LOG(X(I))
          ENDIF
  210   CONTINUE
      ELSEIF(ICASAN.EQ.'LNTO' .AND. XMEAN.NE.CPUMIN)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,216)
  216   FORMAT('      LOGNORMAL TOLERANCE LIMITS NOT SUPPORTED FOR ',
     1         'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.EQ.CPUMIN)THEN
C
C       NORMAL ORDER STATISTIC MEDIANS
C
        CALL UNIMED(N,X2TEMP)
        DO231I=1,N
          CALL NORPPF(X2TEMP(I),X2OUT)
          X2TEMP(I)=X2OUT
  231   CONTINUE
C
C       COMPUTE PPCC VALUE FOR UNTRANSFORMED DATA AND
C       RETURN CRITICAL VALUE FOR 3 <= N <= 1,000.
C
        CALL SORT(X,N,D2TEMP)
        AN1=N
        SUMY=0.0
        DO233I=1,N
          SUMY=SUMY+D2TEMP(I)
  233   CONTINUE
        XBAR=0.0
        YBAR=SUMY/AN1
C
        SUMX=0.0
        SUMY=0.0
        SUMXY=0.0
        DO234I=1,N
          SUMX=SUMX+(X2TEMP(I)-XBAR)*(X2TEMP(I)-XBAR)
          SUMY=SUMY+(D2TEMP(I)-YBAR)*(D2TEMP(I)-YBAR)
          SUMXY=SUMXY+(X2TEMP(I)-XBAR)*(D2TEMP(I)-YBAR)
  234   CONTINUE
        ARG=SUMX*SUMY
        CCORG=0.0
        IF(ARG.GT.0.0)CCORG=SUMXY/SQRT(ARG)
C
        IF(N.GE.3 .AND. N.LE.1000)THEN
          ALPHAT=0.05
          CALL NORPCV(ALPHAT,N,CV,
     1                ISUBRO,IBUGA3,IERROR)
        ELSE
          CV=CPUMIN
        ENDIF
C
C       SORT DATA AND SHIFT DATA TO MAKE IT POSITIVE
C
CCCCC   CALL SORT(X,N,D2TEMP)
        XMIN=D2TEMP(1)
        IF(XMIN.LE.0.0)THEN
          DO236I=1,N
            D2TEMP(I)=D2TEMP(I)-XMIN+1.0
  236     CONTINUE
          ALOC=XMIN-1.0
        ELSE
          ALOC=0.0
        ENDIF
C
        ALAMBA=-2.0
        ALAMSV=ALAMBA
        AINC=0.1
        CCMAX=CPUMIN
C
        DO240IDIS=1,41
C
C         LOG TRANSFORMATION
C
          IF(-0.001.LE.ALAMBA.AND.ALAMBA.LE.0.001)THEN
            ALAMBA=0.0
            DO241I=1,N
              Y2TEMP(I)=LOG(D2TEMP(I))
  241       CONTINUE
          ELSE
            DO246I=1,N
              Y2TEMP(I)=((D2TEMP(I)**ALAMBA)-1.0)/ALAMBA
  246       CONTINUE
          ENDIF
C
          AN1=N
          SUMY=0.0
          DO251I=1,N
            SUMY=SUMY+Y2TEMP(I)
  251     CONTINUE
          XBAR=0.0
          YBAR=SUMY/AN1
C
          SUMX=0.0
          SUMY=0.0
          SUMXY=0.0
          DO252I=1,N
            SUMX=SUMX+(X2TEMP(I)-XBAR)*(X2TEMP(I)-XBAR)
            SUMY=SUMY+(Y2TEMP(I)-YBAR)*(Y2TEMP(I)-YBAR)
            SUMXY=SUMXY+(X2TEMP(I)-XBAR)*(Y2TEMP(I)-YBAR)
  252     CONTINUE
          ARG=SUMX*SUMY
          CC=0.0
          IF(ARG.GT.0.0)CC=SUMXY/SQRT(ARG)
          IF(CC.GT.CCMAX)THEN
            CCMAX=CC
            ALAMSV=ALAMBA
          ENDIF
C
          ALAMBA=ALAMBA + AINC
C
  240   CONTINUE
C
C       NOW TRANSFORM THE DATA
C
        ALAMBA=ALAMSV
        IF(ALAMBA.EQ.0.0)THEN
          DO261I=1,N
            X(I)=LOG(D2TEMP(I))
  261     CONTINUE
        ELSE
          DO263I=1,N
            X(I)=((D2TEMP(I)**ALAMBA)-1.0)/ALAMBA
  263     CONTINUE
        ENDIF
C
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.NE.CPUMIN)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,276)
  276   FORMAT('      BOX-COX TOLERANCE LIMITS NOT SUPPORTED FOR ',
     1         'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.NE.CPUMIN)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,278)
  278   FORMAT('      BOX-COX TOLERANCE LIMITS NOT SUPPORTED FOR ',
     1         'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     COMPUTE NORMAL TOLERANCE LIMITS
C
      IF(XMEAN.EQ.CPUMIN)THEN
        AN=N
        CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
        CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      ELSE
        N=INT(AN+0.1)
      ENDIF
C
C     COMPUTE THE NORMAL TOLERANCE LIMITS
C
      IF(ICASAN.EQ.'TOLE' .OR. ICASAN.EQ.'NTOL' .OR.
     1   ICASAN.EQ.'LNTO' .OR. ICASAN.EQ.'BCTO')THEN
C
        IF(ITOLME.EQ.'HOWE')THEN
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
            WRITE(ICOUT,801)
  801       FORMAT('HOWE METHOD')
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          IF(PTOLDF.GT.0.0)THEN
            ANU=PTOLDF
          ELSE
            ANU=REAL(N-1)
          ENDIF
          NU=INT(ANU+0.5)
          IF(NU.LT.1)NU=1
          ANU=REAL(NU)
          AN=REAL(N)
          TERM2=ANU*(1.0 + (1.0/AN))
          DO 800 I=1,NCOV
             PCOV=PC(I)/100.
             TERM1=(1.0 + PCOV)/2.0
             CALL NORPPF(TERM1,Z)
             DO 900 J=1,NCONF
                PCONF=PA(J)/100.
                AVAL=1.0 - PCONF
                CALL CHSPPF(AVAL,NU,TERM3)
                AK=Z*SQRT(TERM2/TERM3)
C
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,902)I,J,NU,TERM1,Z,AVAL,TERM3,TERM2
  902             FORMAT('I,J,NU,TERM1,Z,AVAL,TERM3,TERM2 = ',
     1                   3I5,5G15.7)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
C
C               APPLY GUENTHER CORRECTION IF REQUESTED
C
                IF(ITOLGC.EQ.'ON')THEN
                  ANUM=AN - 3.0 - TERM3
                  DENOM=2.0*(AN+1)**2
                  TERM4=SQRT(1.0 + (ANUM/DENOM))
                  AK=TERM4*AK
                ENDIF
C
                TMIN(I,J)=XMEAN - AK*XSD
                TMAX(I,J)=XMEAN + AK*XSD
                TK(I,J)=AK
C
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,910)I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),
     1                            TMAX(I,J)
  910             FORMAT('I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),TMAX(I,J)=',
     1                   2I5,2F8.3,3G15.7)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
C
                IF(ICASAN.EQ.'LNTO')THEN
                  TMIN(I,J)=EXP(TMIN(I,J))
                  TMAX(I,J)=EXP(TMAX(I,J))
                ELSEIF(ICASAN.EQ.'BCTO')THEN
                  IF(ALAMBA.EQ.0.0)THEN
                    TMIN(I,J)=EXP(TMIN(I,J))
                    TMAX(I,J)=EXP(TMAX(I,J))
                    TMIN(I,J)=TMIN(I,J)+ALOC
                    TMAX(I,J)=TMAX(I,J)+ALOC
                  ELSE
                    TMIN(I,J)=(ALAMBA*TMIN(I,J) + 1.0)**(1.0/ALAMBA)
                    TMAX(I,J)=(ALAMBA*TMAX(I,J) + 1.0)**(1.0/ALAMBA)
                    TMIN(I,J)=TMIN(I,J)+ALOC
                    TMAX(I,J)=TMAX(I,J)+ALOC
                  ENDIF
                ENDIF
  900        CONTINUE
  800     CONTINUE
        ELSE
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
            WRITE(ICOUT,301)
  301       FORMAT('GARDINER AND HULL METHOD')
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          DO 300 I=1,NCOV
            Z=Z1(I)
            IF(PTOLDF.GT.0.0)THEN
              F=PTOLDF
              NU=INT(F+0.5)
              F=REAL(NU)
            ELSE
              F=N-1
              NU=N-1
            ENDIF
            IF(N.LE.6)THEN
              U=USMALL(N,I) 
            ELSE
              D1=1.0+Z*SQRT(2.0)/SQRT(F)
              D2=2.0*(Z**2-1.0)/(3.0*F)
              D3=(Z**3-7.0*Z)/(9.0*SQRT(2.0)*F**1.5)
              D4=(6.0*Z**4+14.0*Z**2-32.0)/(405.0*F**2.0) 
              D5=(9.0*Z**5+256.0*Z**3-433.0*Z)/(4860.0*SQRT(2.0)*F**2.5)
              D6=(12.0*Z**6-243.0*Z**4-923.0*Z**2+1472.0)/
     1           (25515.0*F**3.0)
              D7=(3753.0*Z**7+4353.0*Z**5-289517.0*Z**3-289717.0*Z)/
     1           (9185400.0*SQRT(2.0)*F**3.5)
              UNIV=D1+D2+D3-D4+D5+D6-D7
              U=1.0/UNIV
              U=SQRT(U)
            ENDIF
C
            DO 400 J=1,NCONF
              R=A(J)+(B(J)/(C(J)+AN)) 
              IF(N.LE.5)THEN
                R=RSMALL(N,J) 
              ELSE
                R=A(J)+(B(J)/(C(J)+AN)) 
              ENDIF
              AK=R*U
              TMIN(I,J)=XMEAN - AK*XSD
              TMAX(I,J)=XMEAN + AK*XSD
              TK(I,J)=AK
C
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,910)I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),
     1                            TMAX(I,J)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
C
              IF(ICASAN.EQ.'LNTO')THEN
                TMIN(I,J)=EXP(TMIN(I,J))
                TMAX(I,J)=EXP(TMAX(I,J))
              ELSEIF(ICASAN.EQ.'BCTO')THEN
                IF(ALAMBA.EQ.0.0)THEN
                  TMIN(I,J)=EXP(TMIN(I,J))
                  TMAX(I,J)=EXP(TMAX(I,J))
                  TMIN(I,J)=TMIN(I,J)+ALOC
                  TMAX(I,J)=TMAX(I,J)+ALOC
                ELSE
                  TMIN(I,J)=(ALAMBA*TMIN(I,J) + 1.0)**(1.0/ALAMBA)
                  TMAX(I,J)=(ALAMBA*TMAX(I,J) + 1.0)**(1.0/ALAMBA)
                  TMIN(I,J)=TMIN(I,J)+ALOC
                  TMAX(I,J)=TMAX(I,J)+ALOC
                ENDIF
              ENDIF
  400       CONTINUE
  300     CONTINUE
        ENDIF
C
        IF(ICASAN.EQ.'LNTO' .AND. XMEAN.EQ.CPUMIN)THEN
          DO410I=1,N
            X(I)=EXP(X(I))
  410     CONTINUE
        ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.EQ.CPUMIN)THEN
          IF(ALAMBA.EQ.0.0)THEN
            DO420I=1,N
              X(I)=EXP(D2TEMP(I))
  420       CONTINUE
          ELSE
            DO430I=1,N
              X(I)=(1.0 + ALAMBA*D2TEMP(I))**(1.0/ALAMBA)
  430       CONTINUE
          ENDIF
        ENDIF
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING FOR  **
C               **   NORMAL TOLERANCE LIMITS   **
C               *********************************
C
        ISTEPN='42'
        IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IPRINT.EQ.'OFF')GOTO9000
C
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
C
        IF(ICASAN.EQ.'LNTO')THEN
          ITITLE='Two-Sided Lognormal Tolerance Limits:'
          NCTITL=37
          ITITLZ='EXP(XBAR +/- K*S) for Log of Data'
          NCTITZ=33
        ELSEIF(ICASAN.EQ.'BCTO')THEN
          ITITLE='Two-Sided Box-Cox Transformed Tolerance Limits:'
          NCTITL=47
          ITITLZ=' '
          NCTITZ=0
        ELSE
          ITITLE='Two-Sided Normal Tolerance Limits:'
          NCTITL=34
          ITITLZ='(XBAR +/- K*S)'
          NCTITZ=14
        ENDIF
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(ITOLME.EQ.'HOWE')THEN
          IF(ITOLGC.EQ.'ON')THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Howe Method with Guenther Correction'
            NCTEXT(ICNT)=36
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Howe Method'
            NCTEXT(ICNT)=11
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ENDIF
        ELSEIF(ITOLME.EQ.'WALD')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Gardiner and Hull Method'
          NCTEXT(ICNT)=24
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(NREPL.GT.0)THEN
          IADD=1
          DO4101I=1,NREPL
            ICNT=ICNT+1
            ITEMP=I+IADD
            ITEXT(ICNT)='Factor Variable  : '
            WRITE(ITEXT(ICNT)(17:17),'(I1)')I
            WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
            NCTEXT(ICNT)=27
            AVALUE(ICNT)=PID(ITEMP)
            IDIGIT(ICNT)=NUMDIG
 4101     CONTINUE
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Summary Statistics:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(ICASAN.EQ.'BCTO')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=1
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Untransformed Data:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Value of Normal PPCC:'
          NCTEXT(ICNT)=21
          AVALUE(ICNT)=CCORG
          IDIGIT(ICNT)=NUMDIG
          IF(CV.NE.CPUMIN)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='5% Critical Value:'
            NCTEXT(ICNT)=18
            AVALUE(ICNT)=CV
            IDIGIT(ICNT)=NUMDIG
          ENDIF
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=1
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Transformed Data:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Value of Lambda:'
          NCTEXT(ICNT)=16
          AVALUE(ICNT)=ALAMBA
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Value of Normal PPCC:'
          NCTEXT(ICNT)=21
          AVALUE(ICNT)=CCMAX
          IDIGIT(ICNT)=NUMDIG
          IF(CV.NE.CPUMIN)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='5% Critical Value:'
            NCTEXT(ICNT)=18
            AVALUE(ICNT)=CV
            IDIGIT(ICNT)=NUMDIG
          ENDIF
        ENDIF
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Observations:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=AN
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Degrees of Freedom:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=NU
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        IF(ICASAN.EQ.'LNTO')THEN
          ITEXT(ICNT)='Sample Mean of Log of Data:'
          NCTEXT(ICNT)=27
        ELSEIF(ICASAN.EQ.'BCTO')THEN
          ITEXT(ICNT)='Sample Mean of Transformed Data:'
          NCTEXT(ICNT)=33
        ELSE
          ITEXT(ICNT)='Sample Mean:'
          NCTEXT(ICNT)=12
        ENDIF
        AVALUE(ICNT)=XMEAN
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        IF(ICASAN.EQ.'LNTO')THEN
          ITEXT(ICNT)='Sample SD of Log of Data:'
          NCTEXT(ICNT)=25
        ELSEIF(ICASAN.EQ.'BCTO')THEN
          ITEXT(ICNT)='Sample SD of Transformed Data:'
          NCTEXT(ICNT)=30
        ELSE
          ITEXT(ICNT)='Sample Standard Deviation:'
          NCTEXT(ICNT)=26
        ENDIF
        AVALUE(ICNT)=XSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        NUMROW=ICNT
        DO4020I=1,NUMROW
          NTOT(I)=15
 4020   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        ISTEPN='42A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
        ISTEPN='42D'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ITITL9=' '
        NCTIT9=0
        ITITLE='Coverage = 90%'
        NCTITL=14
C
        ITITL2(1,1)='Confidence'
        NCTIT2(1,1)=10
        ITITL2(2,1)='Value (%)'
        NCTIT2(2,1)=9
C
        ITITL2(1,2)='k'
        NCTIT2(1,2)=1
        ITITL2(2,2)='Factor'
        NCTIT2(2,2)=6
C
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
        ITITL2(2,3)='Limit'
        NCTIT2(2,3)=5
C
        ITITL2(1,4)='Upper'
        NCTIT2(1,4)=5
        ITITL2(2,4)='Limit'
        NCTIT2(2,4)=5
C
        NUMLIN=2
        NUMCOL=4
        NUMROW=NCONF
        NMAX=0
        DO4221I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IDIGIT(I)=NUMDIG
          ITYPCO(I)='NUME'
          IF(I.EQ.1)THEN
            NTOT(I)=12
            IDIGIT(I)=1
            IWHTML(1)=75
          ENDIF
          NMAX=NMAX+NTOT(I)
 4221   CONTINUE
        DO4223I=1,NUMROW
          DO4225J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 4225     CONTINUE
          JCNT=1
          AMAT(I,1)=PA(I)
          AMAT(I,2)=TK(1,I)
          AMAT(I,3)=TMIN(1,I)
          AMAT(I,4)=TMAX(1,I)
 4223   CONTINUE
C
        IWHTML(1)=150
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWRTF(1)=2000
        IWRTF(2)=IWRTF(1)+2000
        IWRTF(3)=IWRTF(2)+2000
        IWRTF(4)=IWRTF(3)+2000
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
        ITITLE='Coverage = 95%'
        NCTITL=14
C
        DO4233I=1,NCONF
          AMAT(I,2)=TK(2,I)
          AMAT(I,3)=TMIN(2,I)
          AMAT(I,4)=TMAX(2,I)
 4233   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
        ITITLE='Coverage = 99%'
        NCTITL=14
C
        DO4243I=1,NCONF
          AMAT(I,2)=TK(3,I)
          AMAT(I,3)=TMIN(3,I)
          AMAT(I,4)=TMAX(3,I)
 4243   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ENDIF
C
      IF(ICASAN.EQ.'TOLE' .OR. ICASAN.EQ.'NPTO')THEN
C
C       COMPUTE DISTRIBUTION-FREE TOLERANCE LIMITS
C
        K=N/2
        NUMSEC=3
        IF(K.LT.NUMSEC)NUMSEC=K 
C
C       DETERMINE THE SMALLEST 3 AND LARGEST 3 OBSERVATIONS
C
        CALL SORT(X,N,X)
        LOCMIN=1
        XMIN=X(1)
        XMIN2=X(2)
        XMIN3=X(3)
        XMAX=X(N)
        XMAX2=X(N-1)
        XMAX3=X(N-2)
C
        AN=REAL(N)
        AN1=AN-1.0
        AN2=AN-2.0
        AN3=AN-3.0
        AN4=AN-4.0
        AN5=AN-5.0
        AN6=AN-6.0
C
        DO1600I=1,10
          D=P(I)/100.0
          C1(I)=(D**AN1)*(-AN + AN1*D)
          C1(I)=1.0-C1(I)
          Q=1.0-D
          T=Q*AN
          C1(I)=1.0+AN1*Q
          C1(I)=1.0-(D**AN1)*C1(I)
          C1(I)=C1(I)*100.0
          IF(NUMSEC.EQ.1)GOTO1600 
          A0=6.0*D*D*D
          A1=2.0-7.0*D+11.0*D*D
          A2=-3.0+6.0*D 
          A3=1.0
          C2(I)=A0+A1*T+A2*T*T+A3*T*T*T
          C2(I)=1.0-(D**AN3)*C2(I)/6.0
          C2(I)=C2(I)*100.0
          IF(NUMSEC.EQ.2)GOTO1600 
          A0=120.0*D*D*D*D*D
          A1=24.0-126.0*D+274.0*D*D-326.0*D*D*D+274.0*D*D*D*D
          A2=-50.0+205.0*D-320.0*D*D+225.0*D*D*D
          A3=35.0-100.0*D+85.0*D*D
          A4=-10.0+15.0*D
          A5=1.0D0
          C3(I)=A0+A1*T+A2*T*T+A3*T*T*T+A4*T*T*T*T+A5*T*T*T*T*T 
          C3(I)=1.0-(D**AN5)*C3(I)/120.0
          C3(I)=C3(I)*100.0
 1600   CONTINUE
C
C               ****************************************
C               **   STEP 52--                        **
C               **   WRITE OUT EVERYTHING FOR         **
C               **   NONPARAMETRIC TOLERANCE LIMITS   **
C               ****************************************
C
        ISTEPN='52'
        IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IPRINT.EQ.'OFF')GOTO9000
C
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
C
        ITITLE='Two-Sided Distribution-Free Tolerance Limits'
        NCTITL=44
        ITITLZ=' '
        NCTITZ=0
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(NREPL.GT.0)THEN
          IADD=1
          DO5101I=1,NREPL
            ICNT=ICNT+1
            ITEMP=I+IADD
            ITEXT(ICNT)='Factor Variable  : '
            WRITE(ITEXT(ICNT)(17:17),'(I1)')I
            WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
            NCTEXT(ICNT)=27
            AVALUE(ICNT)=PID(ITEMP)
            IDIGIT(ICNT)=NUMDIG
 5101     CONTINUE
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Summary Statistics:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Observations:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=AN
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Mean:'
        NCTEXT(ICNT)=12
        AVALUE(ICNT)=XMEAN
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Standard Deviation:'
        NCTEXT(ICNT)=26
        AVALUE(ICNT)=XSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        NUMROW=ICNT
        DO5020I=1,NUMROW
          NTOT(I)=15
 5020   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        ISTEPN='42A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
        ISTEPN='42D'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        NUMROW=10
        NUMCOL=2
        NUMLIN=2
        NMAX=0
C
        ITITL9(1:17)='Involving X(3) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN3
        ITITL9(33:56)='     Involving X(N-2) = '
        WRITE(ITITL9(57:71),'(G15.7)')XMAX3
        NCTIT9=71
        ITITLE=' '
        NCTITL=0
C
        ITITL2(1,1)='Confidence'
        NCTIT2(1,1)=10
        ITITL2(2,1)='Value (%)'
        NCTIT2(2,1)=9
C
        ITITL2(1,2)='Coverage'
        NCTIT2(1,2)=8
        ITITL2(2,2)='Value (%)'
        NCTIT2(2,2)=9
C
        NMAX=0
        DO5221I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IDIGIT(I)=2
          ITYPCO(I)='NUME'
          IWHTML(I)=150
          IF(I.EQ.1)THEN
            NTOT(I)=12
            IWHTML(1)=75
          ENDIF
          NMAX=NMAX+NTOT(I)
 5221   CONTINUE
        DO5223I=1,NUMROW
          DO5225J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5225     CONTINUE
          AMAT(I,1)=C3(I)
          AMAT(I,2)=P(I)
 5223   CONTINUE
C
        IWRTF(1)=1600
        IWRTF(2)=IWRTF(1)+2000
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        IF(NUMSEC.GE.3)THEN
C
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGA3,IERROR)
C
        ENDIF
C
        ITITL9(1:17)='Involving X(2) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN2
        ITITL9(33:56)='     Involving X(N-1) = '
        WRITE(ITITL9(57:71),'(G15.7)')XMAX2
        NCTIT9=71
        ITITLE=' '
        NCTITL=0
C
        NMAX=0
        DO5321I=1,NUMCOL
          NTOT(I)=15
          IF(I.EQ.1)THEN
            NTOT(I)=12
          ENDIF
          NMAX=NMAX+NTOT(I)
 5321   CONTINUE
        DO5323I=1,NUMROW
          DO5325J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5325     CONTINUE
          AMAT(I,1)=C2(I)
          AMAT(I,2)=P(I)
 5323   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        IF(NUMSEC.GE.2)THEN
C
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGA3,IERROR)
C
        ENDIF
C
        ITITL9(1:17)='Involving X(1) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN
        ITITL9(33:54)='     Involving X(N) = '
        WRITE(ITITL9(55:69),'(G15.7)')XMAX
        NCTIT9=69
        ITITLE=' '
        NCTITL=0
C
        NMAX=0
        DO5421I=1,NUMCOL
          NTOT(I)=15
          IF(I.EQ.1)THEN
            NTOT(I)=12
          ENDIF
          NMAX=NMAX+NTOT(I)
 5421   CONTINUE
        DO5423I=1,NUMROW
          DO5425J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5425     CONTINUE
          AMAT(I,1)=C1(I)
          AMAT(I,2)=P(I)
 5423   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        CALL DPDTA4(ITITL9,NCTIT9,
     1              ITITLE,NCTITL,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              ISUBRO,IBUGA3,IERROR)
C
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOL2(X,N,XMEAN,XSD,AN,
     1                ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES ONE-SIDED NORMAL TOLERANCE
C              LIMITS FOR THE DATA IN THE INPUT VECTOR X.
C              15 NORMAL TOLERANCE LIMITS ARE COMPUTED.
C     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     OUTPUT--PAGE GIVING NORMAL TOLERANCE LIMITS.
C     PRINTING--YES.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--MARY NATRELLA (1963), "EXPERIMENTAL STATISTICS, NBS
C                 HANDBOOK 91", US DEPARTMENT OF COMMERCE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--AUGUST    2011.
C     UPDATED         --SEPTEMBER 2011. FIX BUG IN 99% CONFIDENCE
C                                       INTERVAL TABLE
C     UPDATED         --MAY       2018. ALLOW USER SPECIFIED DEGREES
C                                       OF FREEDOM
C     UPDATED         --MAY       2018. SUPPORT EITHER NORMAL OR
C                                       NON-CENTRAL T METHOD
C     UPDATED         --MAY       2018. CORRECT LABELING OF CONFIDENCE
C                                       AND COVERAGE IN THE TABLE
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION PID(*)
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      PARAMETER (NCOV=3)
      PARAMETER (NCONF=6)
      DIMENSION PA(NCONF)
      DIMENSION PC(NCOV)
      DIMENSION TMIN(NCOV,NCONF)
      DIMENSION TK(NCOV,NCONF)
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IMETH
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER(NUMCLI=3)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=10)
      PARAMETER (MAXRO2=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*75 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
      INCLUDE 'DPCOST.INC'
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
      DATA PA/50.,75.,90.,95.,99.,99.9/
      DATA PC/90.,95.,99./
C
      ISUBN1='TOL2'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TOL--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)XMEAN,XSD,AN
   53   FORMAT('XMEAN,XSD,AN = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(XMEAN.EQ.CPUMIN)THEN
          DO56I=1,N
            WRITE(ICOUT,57)I,X(I)
   57       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(XMEAN.EQ.CPUMIN .AND. N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: ONE SIDED TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(XMEAN.EQ.CPUMIN)THEN
        HOLD=X(1)
        DO135I=2,N
          IF(X(I).NE.HOLD)GOTO139
  135   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,131)HOLD
  131   FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        GOTO9000
  139   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
C               **  LIMITS.                               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE NORMAL TOLERANCE LIMITS
C
      IF(XMEAN.EQ.CPUMIN)THEN
        AN=N
        CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
        CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      ELSE
        N=INT(AN+0.1)
      ENDIF
C
      IF(ITOLM2.EQ.'DEFA')THEN
        IF(N.LE.100)THEN
          IMETH='NONC'
        ELSE
          IMETH='NORM'
        ENDIF
      ELSEIF(ITOLM2.EQ.'NONC')THEN
        IMETH='NONC'
      ELSEIF(ITOLM2.EQ.'NORM')THEN
        IMETH='NORM'
      ENDIF
C
C     COMPUTE THE NORMAL TOLERANCE LIMITS
C
      ICASPL='1LNT'
      DO 300 I=1,NCOV
        GAMMA=PC(I)/100.0
        DO 400 J=1,NCONF
          ALPHA=PA(J)/100.0
          CALL DPTOL3(X,N,XMEAN,XSD,AN,PTOLDF,
     1                ICASPL,ALPHA,GAMMA,ITOLGC,IMETH,
     1                AK,ALOWLM,AUPPLM,
     1                ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          TK(I,J)=AK
          IF(ICASAN.EQ.'LOWE')THEN
            TMIN(I,J)=ALOWLM
          ELSE
            TMIN(I,J)=AUPPLM
          ENDIF
  400   CONTINUE
  300 CONTINUE
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING FOR  **
C               **   NORMAL TOLERANCE LIMITS   **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
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
C
      IF(ICASAN.EQ.'LOWE')THEN
        ITITLE='One-Sided Lower Normal Tolerance Limits:'
        NCTITL=40
        ITITLZ='(XBAR - K*S)'
        NCTITZ=12
      ELSE
        ITITLE='One-Sided Upper Normal Tolerance Limits:'
        NCTITL=40
        ITITLZ='(XBAR - K*S)'
        NCTITZ=12
      ENDIF
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(IMETH.EQ.'NONC')THEN
        ICNT=1
        ITEXT(ICNT)='Non-Central t Based Approximation'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=1
        ITEXT(ICNT)='Normal Based Approximation'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(NREPL.GT.0)THEN
        IADD=1
        DO4101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
          ITEXT(ICNT)='Factor Variable  : '
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
          WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=PID(ITEMP)
          IDIGIT(ICNT)=NUMDIG
 4101   CONTINUE
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO4020I=1,NUMROW
        NTOT(I)=15
 4020 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Coverage = 90%'
      NCTITL=14
C
      ITITL2(1,1)='Confidence'
      NCTIT2(1,1)=10
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
C
      ITITL2(1,2)='k'
      NCTIT2(1,2)=1
      ITITL2(2,2)='Factor'
      NCTIT2(2,2)=6
C
      IF(ICASAN.EQ.'LOWE')THEN
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
      ELSE
        ITITL2(1,3)='Upper'
        NCTIT2(1,3)=5
      ENDIF
      ITITL2(2,3)='Limit'
      NCTIT2(2,3)=5
C
      NUMLIN=2
      NUMCOL=3
      NUMROW=NCONF
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
        IF(I.EQ.1)THEN
          NTOT(I)=12
          IDIGIT(I)=1
          IWHTML(1)=75
        ENDIF
        NMAX=NMAX+NTOT(I)
 4221 CONTINUE
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4225   CONTINUE
        JCNT=1
        AMAT(I,1)=PA(I)
        AMAT(I,2)=TK(1,I)
        AMAT(I,3)=TMIN(1,I)
 4223 CONTINUE
C
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE='Coverage = 95%'
      NCTITL=14
C
      DO4233I=1,NCONF
        AMAT(I,2)=TK(2,I)
        AMAT(I,3)=TMIN(2,I)
 4233 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE='Coverage = 99%'
      NCTITL=14
C
CCCCC 2011/09: CORRECT FOLLOWING LINE
      DO4243I=1,NCONF
        AMAT(I,2)=TK(3,I)
        AMAT(I,3)=TMIN(3,I)
 4243 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOLWEI(X,N,
     1                  MINMAX,IWEIBC,XTEMP,DTEMP,
     1                  ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES ONE-SIDED WEIBULL TOLERANCE
C              LIMITS FOR THE DATA IN THE INPUT VECTOR X.
C              BASE TOLERANCE LIMITS ON FACT THAT ONE-SIDED TOLERANCE
C              LIMITS ARE EQUIVALENT TO LOWER CONFIDENCE INTERVALS ON
C              PERCENTILES.
C     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     OUTPUT--PAGE GIVING NORMAL TOLERANCE LIMITS.
C     PRINTING--YES.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN
C                ENGINEERING", CAMBRIDGE UNIVERSITY PRESS,
C                1999, CHAPTER 17.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--AUGUST    2011.
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION PID(*)
      DIMENSION XTEMP(*)
      DOUBLE PRECISION DTEMP(*)
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWEIBC
      CHARACTER*4 IWEIFL
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      PARAMETER (NCOV=6)
      PARAMETER (NCONF=3)
      DIMENSION PA(NCONF)
      DIMENSION PC(NCOV)
      DIMENSION TMIN(NCOV,NCONF)
      DIMENSION TMAX(NCOV,NCONF)
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=10)
      PARAMETER (MAXRO2=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*75 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
      LOGICAL IFLAGS
      LOGICAL IFLAGE
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
C-----START POINT-----------------------------------------------------
C
      DATA PC/50.,75.,90.,95.,99.,99.9/
      DATA PA/90.,95.,99./
C
      ISUBN1='TOLW'
      ISUBN2='EI  '
C
      IWRITE='OFF'
      IERROR='NO'
      IWEIFL='WEIB'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LWEI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TOL--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)XMEAN,XSD,AN
   53   FORMAT('XMEAN,XSD,AN = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(XMEAN.EQ.CPUMIN)THEN
          DO56I=1,N
            WRITE(ICOUT,57)I,X(I)
   57       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: ONE SIDED WEIBULL TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
C               **  LIMITS.                               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE 2-PARAMETER WEIBULL TOLERANCE LIMITS
C
C     STEP 1: OBTAIN POINT ESTIMATES AND STANDARD ERRORS
C
      AN=REAL(N)
      CALL WEIML1(X,N,IWEIBC,IWEIFL,MINMAX,
     1            XTEMP,DTEMP,
     1            XMEAN,XSD,XVAR,XMIN,XMAX,
     1            ZMEAN,ZSD,
     1            SCALE,SCALSE,GAMMA,GAMMSE,
     1            GAMMBC,GABCSE,COVSE,COBCSE,
     1            ISUBRO,IBUGA3,IERROR)
       IF(IERROR.EQ.'YES')GOTO9000
C
C     STEP 2: FOR VARIOUS VALUES OF CONFIDENCE AND COVERAGE, COMPUTE
C             THE ONE-SIDED CONFIDENCE CONFIDENCE INTERVAL FOR THE
C             PERCENTILE (= THE ONE-SIDED TOLERANCE INTERVAL).
C
C             1. STANDARD ERROR USES TECHNIQUE DEMONSTRATED IN EXAMPLE 17.4
C                (P. 344) OF BURY.  THIS IS BASED ON PROPOGATION OF ERROR.
C
C             2. CONFIDENCE INTERVAL IS THEN GENERATED USING NORMAL
C                APPROXIMATION (EXAMPLE 17.7 OF BURY).  BURY ALSO DEMONSTRATES
C                A LIKELIHOOD RATIO APPROACH, BUT OMIT THIS FOR NOW.
C
      MINMAX=1
      IF(IWEIBC.EQ.'ON')THEN
        G=GAMMBC
        GSE=GABCSE
        COV=COBCSE
      ELSE
        G=GAMMA
        GSE=GAMMSE
        COV=COVSE
      ENDIF
C
      DO 300 I=1,NCONF
        ALPHA=PA(I)/100.0
        DO 400 J=1,NCOV
          GCOV=PC(J)/100.0
C
          IF(ICASAN.EQ.'LOWE' .OR. ICASAN.EQ.'TWOS')THEN
CCCCC       ALPHL=1.0 - ALPHA
            ALPHL=ALPHA
            IF(ICASAN.EQ.'TWOS')THEN
              ALPHL=1.0 - ALPHA
              ALPHL=1.0 - (ALPHL/2.0)
            ENDIF
            CALL NORPPF(ALPHL,Z95)
            QPTEMP=1.0 - GCOV
            CALL WEIPPF(QPTEMP,G,MINMAX,APPF)
            XQPHAT=SCALE*APPF
C
            C=LOG(1.0/(1.0 - QPTEMP))
            DA=C**(1.0/G)
            DB=-(SCALE*C**(1.0/G)*LOG(C)/(G**2))
            TERM1=(DA*SCALSE)**2
            TERM2=(DB*GSE)**2
            TERM3=2.0*DA*DB*COV*COV
            SEXQP=SQRT(TERM1 + TERM2 + TERM3)
            TMIN(J,I)=XQPHAT - Z95*SEXQP
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'LWEI')THEN
              WRITE(ICOUT,401)ALPHA,ALPHL,GCOV,G,SCALE
  401         FORMAT('ALPHA,ALPHL,GCOV,G,SCALE = ',5G15.7)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,403)Z95,QPTEMP,APPF,XQPHAT
  403         FORMAT('Z95,QPTEMP,APPF,XQPHAT = ',4G15.7)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,405)C,DA,DB,TERM1,TERM2
  405         FORMAT('C,DA,DB,TERM1,TERM2 = ',5G15.7)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,407)TERM3,SEXQP,TMIN(J,I)
  407         FORMAT('TERM3,SEXQP,TMIN(J,I) = ',3G15.7)
              CALL DPWRST('XXX','WRIT')
            ENDIF
C
          ELSEIF(ICASAN.EQ.'UPPE' .OR. ICASAN.EQ.'TWOS')THEN
            ALPHU=ALPHA
            IF(ICASAN.EQ.'TWOS')ALPHU=1.0 - ((1.0 - ALPHU)/2.0)
            CALL NORPPF(ALPHU,Z95)
            QPTEMP=GAMMA
            CALL WEIPPF(QPTEMP,G,MINMAX,APPF)
            XQPHAT=SCALE*APPF
C
            C=LOG(1.0/(1.0 - QPTEMP))
            DA=C**(1.0/G)
            DB=-(SCALE*C**(1.0/G)*LOG(C)/(G**2))
            TERM1=(DA*SCALSE)**2
            TERM2=(DB*GSE)**2
            TERM3=2.0*DA*DB*COV*COV
            SEXQP=SQRT(TERM1 + TERM2 + TERM3)
            TMAX(J,I)=XQPHAT + Z95*SEXQP
          ENDIF
  400   CONTINUE
  300 CONTINUE
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING FOR  **
C               **   WEIBULL TOLERANCE LIMITS  **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
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
C
      IF(ICASAN.EQ.'LOWE')THEN
        ITITLE='One-Sided Lower 2-Parameter Weibull Tolerance Limits:'
        NCTITL=53
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        ITITLE='One-Sided Upper 2-Parameter Weibull Tolerance Limits:'
        NCTITL=53
      ELSE
        ITITLE='Two-Sided 2-Parameter Weibull Tolerance Limits:'
        NCTITL=47
      ENDIF
      ITITLZ='(Based on Percentile Confidence Bounds)'
      NCTITZ=39
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(NREPL.GT.0)THEN
        IADD=1
        DO4101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
          ITEXT(ICNT)='Factor Variable  : '
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
          WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=PID(ITEMP)
          IDIGIT(ICNT)=NUMDIG
 4101   CONTINUE
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=XMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=XMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Estimate of Shape:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=G
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Estimate of Scale:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=SCALE
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO4020I=1,NUMROW
        NTOT(I)=15
 4020 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LWEI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LWEI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Confidence = 90%'
      NCTITL=16
C
      ITITL2(1,1)='Coverage'
      NCTIT2(1,1)=8
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
C
      ICOL=1
      IF(ICASAN.EQ.'LOWE')THEN
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Lower'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Upper'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
      ELSE
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Lower'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Upper'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
      ENDIF
C
      NUMLIN=2
      NUMCOL=ICOL
      NUMROW=NCOV
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
        IF(I.EQ.1)THEN
          NTOT(I)=12
          IDIGIT(I)=1
          IWHTML(1)=75
        ENDIF
        NMAX=NMAX+NTOT(I)
 4221 CONTINUE
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4225   CONTINUE
        JCNT=1
        AMAT(I,1)=PC(I)
        IF(ICASAN.EQ.'LOWE')THEN
          AMAT(I,2)=TMIN(I,1)
        ELSEIF(ICASAN.EQ.'UPPE')THEN
          AMAT(I,2)=TMAX(I,1)
        ELSE
          AMAT(I,2)=TMIN(I,1)
          AMAT(I,3)=TMAX(I,1)
        ENDIF
 4223 CONTINUE
C
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(3)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE='Confidence = 95%'
      NCTITL=16
C
      DO4233I=1,NCOV
        IF(ICASAN.EQ.'LOWE')THEN
          AMAT(I,2)=TMIN(I,2)
        ELSEIF(ICASAN.EQ.'UPPE')THEN
          AMAT(I,2)=TMAX(I,2)
        ELSE
          AMAT(I,2)=TMIN(I,2)
          AMAT(I,3)=TMAX(I,2)
        ENDIF
 4233 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE='Confidence = 99%'
      NCTITL=16
C
      DO4243I=1,NCONF
        IF(ICASAN.EQ.'LOWE')THEN
          AMAT(I,2)=TMIN(I,3)
        ELSEIF(ICASAN.EQ.'UPPE')THEN
          AMAT(I,2)=TMAX(I,3)
        ELSE
          AMAT(I,2)=TMIN(I,3)
          AMAT(I,3)=TMAX(I,3)
        ENDIF
 4243 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOPCDF(X,BETA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
C              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
C
C                  F(X;BETA) = (2*X - X**2)**BETA
C                              0 <= X <= 1, BETA > 0
C
C              WITH BETA DENOTING THE SHAPE PARAMETER.
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, PP. 33-43.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--FEBRUARY  2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION BETA
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(X.LT.0.0D0)THEN
        CDF=0.0D0
        GOTO9000
      ELSEIF(X.GT.1.0D0)THEN
        CDF=1.0D0
CCCCC   WRITE(ICOUT,2)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,46)X
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   CDF=0.0D0
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GOTO9000
      ENDIF
CCCC2 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPCDF IS ',
CCCCC1       'OUTSIDE THE (0,1) INTERVAL.')
   12 FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPCDF IS ',
     1       'NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LE.0.0D0)THEN
        CDF=0.0D0
      ELSEIF(X.GE.1.0D0)THEN
        CDF=1.0D0
      ELSE
        DTERM1=BETA*DLOG(2.0D0*X - X**2)
        CDF=DEXP(DTERM1)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOPLI1(Y,N,TEMP1,SHAPE,YLOWLM,YUPPLM,
     1                  ALIK,AIC,AICC,BIC,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE LOG-LIKELIHOOD FUNCTION FOR
C              THE TOPP AND LEONE DISTRIBUTION.  THIS IS FOR THE RAW
C              DATA CASE (I.E., NO GROUPING AND NO CENSORING).
C
C              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
C              PERFORMED.
C
C     REFERENCE --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATION", WORLD SCIENTIFIC, PP. 199-201.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2013/6
C     ORIGINAL VERSION--JUNE      2013.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*1 IBASLC
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DBETA
      DOUBLE PRECISION DN
      DOUBLE PRECISION DNP
      DOUBLE PRECISION DLIK
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DEPS
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
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
      ISUBN1='TOPM'
      ISUBN2='L1  '
C
      IERROR='NO'
C
      ALIK=CPUMIN
      AIC=CPUMIN
      AICC=CPUMIN
      BIC=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PLI1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TOPLI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)ICASPL,IBUGA3,ISUBRO
   52   FORMAT('ICASPL,IBUGA3,ISUBRO = ',2(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N,YLOWLM,YUPPLM,SHAPE
   55   FORMAT('N,YLOWLM,YUPPLM,SHAPE = ',I8,3G15.7)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  COMPUTE LIKELIHOOD FUNCTION         **
C               ******************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WLI1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
C
C     NOTE: WE ARE COMPUTING THE LOG-LIKELIHOOD FUNCTION FOR THE
C           STANDARD CASE (I.E., DATA ASSUMED TO BE IN (0,1)
C           INTERVAL).  IF DATA IS NOT IN THIS INTERVAL, THEN WE
C           NEED TO SCALE IT BASED ON THE VALUES OF XMIN AND XMAX.
C
C           IN ORDER TO KEEP LIKELIHOOD ON SAME SCALE AS
C           ORIGINAL DATA, ACCOMODATE LOWER/UPPER LIMIT
C           PARAMETERS.  USE 
C
C              f(x;shape,loc,scale) = f((x-loc)/scale;shape,0,1)/scale
C
C           SO CAN COMPUTE THE LOG LIKELIHOOD ON THE TRANSFORMED
C           DATA, BUT SUBTRACT THE TERM
C
C              N*LOG(SCALE)
C
      ZLOC=YLOWLM
      ZSCALE=YUPPLM - YLOWLM
      DO2110I=1,N
        TEMP1(I)=(Y(I) - ZLOC)/ZSCALE
 2110 CONTINUE
C
C     FOR THE TOPP AND LEONE FUNCTION, THE LOG-LIKLIHOOD FUNCTION IS:
C
C     N*LOG(BETA) + SUM[LOG(2*X(i)(] +
C     (BETA - 1)*SUM[LOG(2*X(i) - X(i)**2)]
C
      DEPS=1.0D-12
      DN=DBLE(N)
      DBETA=DBLE(SHAPE)
      DTERM1=DN*DLOG(DBETA) - DN*DLOG(DBLE(ZSCALE))
      DTERM2=DBETA - 1.0D0
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO1000I=1,N
        DX=DBLE(TEMP1(I))
        DSUM1=DSUM1 + DLOG(2.0D0 - 2.0D0*DX)
        DSUM2=DSUM2 + DLOG(2.0D0*DX - DX**2)
 1000 CONTINUE
C
      DLIK=DTERM1 + DSUM1 + DTERM2*DSUM2
      ALIK=REAL(DLIK)
      DNP=1.0D0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PLI1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF TOPLI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)DSUM1,DSUM2,DTERM1,DTERM3
 9013   FORMAT('DSUM1,DSUM2,DTERM1,DTERM3 = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)ALIK,AIC,AICC,BIC
 9014   FORMAT('ALIK,AIC,AICC,BIC = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TOPML1(Y,N,A,B,
     1                  XMIN,XMAX,XMEAN,XSD,
     1                  SHAPML,ZLOC,ZSCALE,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD
C              ESTIMATES FOR THE TOPP AND LEONE DISTRIBUTION
C
C              THE MAXIMUM LIKELIHOOD ESTIMATE OF BETA IS:
C
C                  BETAHAT = N/[SUM[i=1 to N]
C                            [LOG(1/(2*X(i)- X(i)**2))]
C
C     EXAMPLE--TOPP AND LEONE MAXIMUM LIKELIHOOD Y
C     REFERENCE --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH
C                 BOUNDED SUPPORT AND APPLICATIONS", WORLD
C                 SCIENTIFIC PUBLISHING CO., PP. 33-43.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/07
C     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
C                                       SUBROUTINE (FROM DPMLTO)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      INTEGER IFLAG
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
C
C---------------------------------------------------------------------
C
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
      ISUBN1='TOPM'
      ISUBN2='L1  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TOPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,A,B
   52   FORMAT('IBUGA3,ISUBRO,N,A,B = ',2(A4,2X),I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  CARRY OUT CALCULATIONS              **
C               **  FOR TOPP AND LEONE MLE ESTIMATE     **
C               ******************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDIST='TOPP AND LEONE'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CALL SORT(Y,N,Y)
      ZLOC=0.0
      ZSCALE=1.0
      EPS=1.0E-6
C
      IF(A.NE.CPUMIN .AND. A.LT.XMIN .AND. B.NE.CPUMIN .AND.
     1   B.GT.XMAX)THEN
        ZLOC=A
        ZSCALE=B-A
        EPS=0.0
      ELSEIF(XMIN.LE.0.0 .OR. XMAX.GE.1.0)THEN
CCCCC   EPS=(XMAX - XMIN)*0.0001
        EPS=(XMAX - XMIN)*0.01
        ZLOC=XMIN - EPS
        ZMAX=XMAX + EPS
        ZSCALE=ZMAX - ZLOC
CCCCC   ZLOC=XMIN
CCCCC   ZSCALE=XMAX - ZLOC
      ELSE
        EPS=0.0
      ENDIF
C
      DO2120I=1,N
        Y(I)=(Y(I) - ZLOC)/ZSCALE
 2120 CONTINUE
C
      DSUM=0.0D0
      DO2200J=1,N
        DX=Y(J)
        IF(DX.GT.0.0D0)THEN
          DSUM=DSUM + DLOG(1.0D0/(2.0D0*DX - DX**2))
        ENDIF
 2200 CONTINUE
      SHAPML=REAL(DBLE(N)/DSUM)
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF TOPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XSD,XMIN,XMAX
 9055   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9057)SHAPML
 9057   FORMAT('SHAPML = ',G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TOPPDF(X,BETA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C                  f(X;BETA) = BETA*(2-2*X)*(2*X-X**2)**(BETA-1)
C                              0 <= X <= 1, BETA > 0
C
C              WITH BETA DENOTING THE SHAPE PARAMETER.
C
C              THIS DISTRIBUTION HAS MOMENTS:
C
C              MEAN = 1 - 4**BETA*GAMMA(BETA+1)*GAMMA(BETA+1)/
C                     GAMMA(2*BETA+2)
C              VARIANCE = 2**(2*BETA+1)*B(BETA+1,BETA+1)*
C                         {1 - 2**(2*BETA-1)*B(BETA+1,BETA+1)} -
C                         2**(2*BETA+3)*B(BETA+2,BETA+1)*
C                         BETAI(0.5,BETA+2,BETA+1)
C
C              WITH B AND BETAI DENOTING THE BETA AND INCOMPLETE
C              BETA FUNCTIONS, RESPECTIVELY.
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, PP. 33-43.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--FEBRUARY  2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PDF
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(X.LT.0.0D0 .OR. X.GT.1.0D0)THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
      ELSEIF(BETA.LT.1.0D0 .AND. X.LE.0.0D0)THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
      ENDIF
    3 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPPDF IS ',
     1       'OUTSIDE THE (0,1) INTERVAL.')
   12 FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPPDF IS ',
     1       'NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(X.GE.1.0D0)THEN
        PDF=0.0D0
      ELSEIF(X.LE.0.0D0)THEN
        IF(BETA.EQ.1.0D0)THEN
          PDF=2.0D0
        ELSEIF(BETA.LT.1.0D0)THEN
          PDF=0.0D0
        ELSE
          DTERM1=DLOG(BETA) + DLOG(2.0D0 - 2.0D0*X) +
     1           (BETA-1.0D0)*DLOG(1.0D0*X - X**2)
          PDF=DEXP(DTERM1)
        ENDIF
      ELSE
        DTERM1=DLOG(BETA) + DLOG(2.0D0 - 2.0D0*X) +
     1         (BETA-1.0D0)*DLOG(2.0D0*X - X**2)
        PDF=DEXP(DTERM1)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOPPPF(P,BETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
C              THE PERCENT POINT FUNCTION IS:
C
C                  G(P;BETA) = 1 - SQRT(1 - P**(1/BETA))
C                              0 <= P <= 1, BETA > 0
C
C              WITH BETA DENOTING THE SHAPE PARAMETER.
C
C     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, PP. 33-43.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--FEBRUARY  2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION P
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PPF
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPPPF IS ',
     1       'OUTSIDE THE (0,1) INTERVAL.')
   12 FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPPPF IS ',
     1       'NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(P.LE.0.0D0)THEN
        PPF=0.0D0
      ELSEIF(P.GE.1.0D0)THEN
        PPF=1.0D0
      ELSE
        PPF=1.0D0 - DSQRT(1.0D0 - P**(1.0D0/BETA))
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TOPRAN(N,BETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TOPP AND LEONE DISTRIBUTION
C              WITH SHAPE PARAMETER  BETA.
C
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C                  f(X;BETA) = BETA*(2-2*X)*(2*X-X**2)**(BETA-1)
C                              0 <= X <= 1, BETA > 0
C
C              WITH BETA DENOTING THE SHAPE PARAMETER.
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SHAPE PARAMETER BETA.
C                                BETA SHOULD BE IN THE RANGE (0,1).
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TOPP AND LEONE DISTRIBUTION
C             WITH SHAPE PARAMETER BETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TOPPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, PP. 33-43.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHMOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.2
C     ORIGINAL VERSION--FEBRUARY  2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION BETA
      DOUBLE PRECISION DTEMP
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',
     1'TOPP AND LEONE RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
      IF(BETA.LE.0.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  201 FORMAT('***** ERROR--THE BETA SHAPE PARAMETER IS ',
     1       'NON-POSITIVE.')
  203 FORMAT('      THE VALUE OF BETA IS ',G15.7)
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TOPP AND LEONE DISTRIBUTION RANDOM
C     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO300I=1,N
        ZTEMP=X(I)
        CALL TOPPPF(DBLE(ZTEMP),BETA,DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TPDF(X,ANU,PDF)
CCCCC SUBROUTINE TPDF(X,NU,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
C               --FEDERIGHI, EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S
C                 T-DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1959, PAGES 683-688.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 27-30.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 132-134.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--AUGUST    1977.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
C                                       DEGREES OF FREEDOM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,DNU
      DOUBLE PRECISION DSQTPI,DRATIO
      DOUBLE PRECISION DCONST,DPOWER
      DOUBLE PRECISION AI
      DOUBLE PRECISION DSQRT
      DOUBLE PRECISION DPI
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DLNGAM
C
      EXTERNAL DLNGAM
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
      DATA DPI   / 3.14159265358979D+00/
      DATA DSQTPI/1.77245385090552D0/
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GOTO8000
C
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',
     1         'TO TPDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
C
C               ****************************************************************
C               **  STEP 2--
C               **  COMPUTE THE CONSTANT = 1/(SQRT(NU)*BETA(1/2,NU/2))
C               **  = (1/(SQRT(NU)*SQRT(PI))) * (GAMMA((NU/2)+(1/2))/GAMMA(NU/2)
C               ****************************************************************
C
      DX=X
      DNU=NU
C
      DRATIO=1.0D0
      IEVODD=NU-2*(NU/2)
      IMIN=3
      IF(IEVODD.EQ.0)IMIN=2
      IF(NU.LT.IMIN)GOTO250
      DO300I=IMIN,NU,2
      AI=I
      DRATIO=((AI-1.0D0)/AI)*DRATIO
  300 CONTINUE
  250 CONTINUE
      DRATIO=DRATIO*DNU
      IF(IEVODD.EQ.0)GOTO260
      DRATIO=DRATIO/DSQTPI
      GOTO400
  260 CONTINUE
      DRATIO=DRATIO*DSQTPI/2.0D0
  400 CONTINUE
C
      DCONST=DRATIO/(DSQTPI*DSQRT(DNU))
C
C               ************************************
C               **  STEP 3--                      **
C               **  COMPUTE THE DENSITY FUNCTION  **
C               ************************************
C
      DPOWER=-(DNU+1.0D0)/2.0D0
      PDF=DCONST*((1.0D0+DX*DX/DNU)**DPOWER)
      GOTO9000
C
CCCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
C
 8000 CONTINUE
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',
     1         'TO TPDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
C
      DX=DBLE(X)
      DNU=DBLE(ANU)
C
      DTERM1=(DNU+1.0D0)/2.0D0
      DNUM=DLNGAM(DTERM1)
C
      DTERM2=0.5D0*(DLOG(DNU) + DLOG(DPI))
      DTERM3=DLNGAM(DNU/2.0D0)
      DTERM4=((DNU+1.0D0)/2.0D0)*DLOG(1.0D0 + DX**2/DNU)
      DDENOM=DTERM2 + DTERM3 + DTERM4 + DTERM5
      DPDF=DNUM - DDENOM
      DPDF=DEXP(DPDF)
      PDF=REAL(DPDF)
C
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TPPF(P,ANU,PPF)
CCCCC SUBROUTINE TPPF(P,NU,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THE STUDENT'S T DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL X,
C              AND ITS PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (EXCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
C             VALUE PPF FOR THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSIN, DCOS, DSQRT, DATAN.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--FOR NU = 1 AND NU = 2, THE PERCENT POINT FUNCTION
C              FOR THE T DISTRIBUTION EXISTS IN SIMPLE CLOSED FORM
C              AND SO THE COMPUTED PERCENT POINTS ARE EXACT.
C            --FOR OTHER SMALL VALUES OF NU (NU BETWEEN 3 AND 6,
C              INCLUSIVELY), THE APPROXIMATION
C              OF THE T PERCENT POINT BY THE FORMULA
C              GIVEN IN THE REFERENCE BELOW IS AUGMENTED
C              BY 3 ITERATIONS OF NEWTON'S METHOD FOR
C              ROOT DETERMINATION.
C              THIS IMPROVES THE ACCURACY--ESPECIALLY FOR
C              VALUES OF P NEAR 0 OR 1.
C     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
C                 SERIES 55, 1964, PAGE 949, FORMULA 26.7.5.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGE 102,
C                 FORMULA 11.
C               --FEDERIGHI, 'EXTENDED TABLES OF THE
C                 PERCENTAGE POINTS OF STUDENT'S T
C                 DISTRIBUTION, JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION,
C                 1969, PAGES 683-688.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 120-123.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--OCTOBER   1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
C                                       DEGREES OF FREEDOM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION PI
      DOUBLE PRECISION SQRT2
      DOUBLE PRECISION DP
      DOUBLE PRECISION DNU
      DOUBLE PRECISION TERM1,TERM2,TERM3,TERM4,TERM5
      DOUBLE PRECISION DPPFN
      DOUBLE PRECISION DPPF,DCON,DARG,Z,S,C
      DOUBLE PRECISION B21
      DOUBLE PRECISION B31,B32,B33,B34
      DOUBLE PRECISION B41,B42,B43,B44,B45
      DOUBLE PRECISION B51,B52,B53,B54,B55,B56
      DOUBLE PRECISION D1,D3,D5,D7,D9
C
      DOUBLE PRECISION DC
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION EPS
      DOUBLE PRECISION SIG
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSD
      DOUBLE PRECISION XL
      DOUBLE PRECISION XR
      DOUBLE PRECISION XINC
      DOUBLE PRECISION X
      DOUBLE PRECISION FXL
      DOUBLE PRECISION FXR
      DOUBLE PRECISION P1
      DOUBLE PRECISION FCS
      DOUBLE PRECISION XRML
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION CDFL
      DOUBLE PRECISION CDFR
      DOUBLE PRECISION DBETAI
C
      EXTERNAL DBETAI
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
      DATA PI/3.14159265358979D0/
      DATA SQRT2/1.414213562D0/
      DATA B21/0.25D0/
      DATA B31,B32,B33,B34/0.01041666666667D0,5.0D0,16.0D0,3.0D0/
      DATA B41,B42,B43,B44,B45/0.00260416666667D0,3.0D0,19.0D0,17.0D0,
     1                         -15.0D0/
      DATA B51,B52,B53,B54,B55,B56/0.00001085069444D0,79.0D0,776.0D0,
     1                             1482.0D0,-1920.0D0,-945.0D0/
C
      DATA EPS /0.0001D0/
      DATA SIG /1.0D-7/
      DATA ZERO /0.0D0/
C
C-----START POINT-----------------------------------------------------
C
      S=0.0D0
      C=0.0D0
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TPPF ',
     1       'IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      IF(ANU.LT.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TPPF ',
     1         'IS LESS THAN 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ANU
        PPF=0.0
        GOTO9000
      ENDIF
C
      IF(P.EQ.0.5)THEN
        PPF=0.0
        GOTO9000
      ENDIF
C
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GOTO8000
C
      DNU=NU
      DP=P
      MAXIT=5
C
      IF(NU.EQ.1)THEN
C
C       TREAT THE NU = 1 (CAUCHY) CASE
C
        DARG=PI*DP
        PPF=-DCOS(DARG)/DSIN(DARG)
        GOTO9000
      ELSEIF(NU.EQ.2)THEN
C
C       TREAT THE NU = 2 CASE
C
        TERM1=SQRT2/2.0D0
        TERM2=2.0D0*DP-1.0D0
        TERM3=DSQRT(DP*(1.0D0-DP))
        PPF=TERM1*TERM2/TERM3
        GOTO9000
      ELSE
C
C       TREAT THE NU GREATER THAN OR EQUAL TO 3 CASE
C
        CALL NORPPF(P,PPFN)
        DPPFN=PPFN
        D1=DPPFN
        D3=DPPFN**3
        D5=DPPFN**5
        D7=DPPFN**7
        D9=DPPFN**9
        TERM1=D1
        TERM2=B21*(D3+D1)/DNU
        TERM3=B31*(B32*D5+B33*D3+B34*D1)/(DNU**2)
        TERM4=B41*(B42*D7+B43*D5+B44*D3+B45*D1)/(DNU**3)
        TERM5=B51*(B52*D9+B53*D7+B54*D5+B55*D3+B56*D1)/(DNU**4)
        DPPF=TERM1+TERM2+TERM3+TERM4+TERM5
        PPF=DPPF
        IF(NU.GE.7)GOTO9000
        IF(NU.EQ.3)THEN
C
C         AUGMENT THE RESULTS FOR THE NU = 3 CASE
C
          DCON=PI*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO350IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-(Z+S*C-DCON)/(2.0D0*C*C)
  350     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.4)THEN
C
C         AUGMENT THE RESULTS FOR THE NU = 4 CASE
C
          DCON=2.0D0*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO450IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-((1.0D0+0.5D0*C*C)*S-DCON)/(1.5D0*C*C*C)
  450     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.5)THEN
C
C         AUGMENT THE RESULTS FOR THE NU = 5 CASE
C
          DCON=PI*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO550IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-(Z+(C+(2.0D0/3.0D0)*C*C*C)*S-DCON)/
     1        ((8.0D0/3.0D0)*C**4)
  550     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.6)THEN
C
C         AUGMENT THE RESULTS FOR THE NU = 6 CASE
C
          DCON=2.0D0*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO650IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-((1.0D0+0.5D0*C*C+0.375D0*C**4)*S-DCON)/
     1        ((15.0D0/8.0D0)*C**5)
  650     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ENDIF
        GOTO9000
      ENDIF
C
C     CASE FOR FRACTIONAL DEGREES OF FREEDOM.  USE BISECTION
C     METHOD TO NUMERICALLY INVERT CDF FUNCTION.
C
 8000 CONTINUE
C
C     STEP 1: DETERMINE A BRACKETING INTERVAL.  USE 0 AS
C             EITHER THE LOWER OR UPPER LIMIT.
C
      MAXIT=3000
      IF(P.GT.0.5)THEN
        CALL NORPPF(P,XLTEMP)
        CALL CAUPPF(P,XRTEMP)
      ELSEIF(P.LT.0.5)THEN
        CALL NORPPF(P,XRTEMP)
        CALL CAUPPF(P,XLTEMP)
      ENDIF
      XL=DBLE(XLTEMP)
      XR=DBLE(XRTEMP)
      DNU=DBLE(ANU)
C
C  BISECTION METHOD
C
      DP=DBLE(P)
      IC = 0
      FXL = -DP
      FXR = 1.0D0 - DP
C
  105 CONTINUE
        X = (XL+XR)*0.5D0
        TERM1=1.0D0/(1.0D0 + X*X/DNU)
        TERM2=DNU/2.0D0
        TERM3=0.5D0
        TERM4=DBETAI(TERM1,TERM2,TERM3)
        IF(X.EQ.0.0D0)THEN
          DCDF=0.5D0
        ELSEIF(X.LE.0.0D0)THEN
          DCDF=0.5D0*TERM4
        ELSE
          DCDF=1.0D0 - 0.5D0*TERM4
        ENDIF
        P1=DCDF
        PPF=REAL(X)
        FCS = P1 - DP
        IF(FCS*FXL.GT.ZERO)THEN
          XL = X
          FXL = FCS
        ELSE
          XR = X
          FXR = FCS 
        ENDIF
        XRML = XR - XL
        IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GOTO9000
        IC = IC + 1
        IF(IC.LE.MAXIT)THEN
          GOTO105
        ELSE
          WRITE(ICOUT,130)
          CALL DPWRST('XXX','BUG ')
  130     FORMAT('***** ERROR--TPPF ROUTINE DID NOT CONVERGE')
        ENDIF
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TQLRAT(N,D,E2,IERR)
C***BEGIN PROLOGUE  TQLRAT
C***DATE WRITTEN   760101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  D4A5,D4C2A
C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
C***AUTHOR  SMITH, B. T., ET AL.
C***PURPOSE  Computes eigenvalues of symmetric tridiagonal matrix
C            a rational variant of the QL method.
C***DESCRIPTION
C
C     This subroutine is a translation of the ALGOL procedure TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) by Reinsch.
C
C     This subroutine finds the eigenvalues of a SYMMETRIC
C     TRIDIAGONAL matrix by the rational QL method.
C
C     On Input
C
C        N is the order of the matrix.
C
C        D contains the diagonal elements of the input matrix.
C
C        E2 contains the squares of the subdiagonal elements of the
C          input matrix in its last N-1 positions.  E2(1) is arbitrary.
C
C      On Output
C
C        D contains the eigenvalues in ascending order.  If an
C          error exit is made, the eigenvalues are correct and
C          ordered for indices 1,2,...IERR-1, but may not be
C          the smallest eigenvalues.
C
C        E2 has been destroyed.
C
C        IERR is set to
C          Zero       for normal return,
C          J          if the J-th eigenvalue has not been
C                     determined after 30 iterations.
C
C     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
C
C     Questions and comments should be directed to B. S. Garbow,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C     ------------------------------------------------------------------
C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
C                 1976.
C***ROUTINES CALLED  PYTHAG
C***END PROLOGUE  TQLRAT
C
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      REAL D(N),E2(N)
      REAL B,C,F,G,H,P,R,S,MACHEP
      REAL PYTHAG
C
      DATA MACHEP/1.0E0/
C***FIRST EXECUTABLE STATEMENT  TQLRAT
      IF (MACHEP .NE. 1.0E0) GO TO 10
   05 MACHEP = 0.5E0*MACHEP
      IF (1.0E0 + MACHEP .GT. 1.0E0) GO TO 05
      MACHEP = 2.0E0*MACHEP
C
   10 IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E2(I-1) = E2(I)
C
      F = 0.0E0
      B = 0.0E0
      E2(N) = 0.0E0
C
      DO 290 L = 1, N
         J = 0
         H = MACHEP * (ABS(D(L)) + SQRT(E2(L)))
         IF (B .GT. H) GO TO 105
         B = H
         C = B * B
C     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (E2(M) .LE. C) GO TO 120
C     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * S)
         R = PYTHAG(P,1.0E0)
         D(L) = S / (P + SIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0E0) G = B
         H = G
         S = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
            IF (G .EQ. 0.0E0) G = B
            H = G * P / R
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0E0) GO TO 210
         IF (ABS(E2(L)) .LE. ABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0E0) GO TO 130
  210    P = D(L) + F
C     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
      SUBROUTINE TRACDF(X,A,B,C,D,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
C              F(X,A,B,C,D) = 0                   X <  A
C                           = (B-A)/(D+C-B-A)*((X-A)/(B-A))**2   A<=X<B
C                           = ((B-A)+2*(X-B))/(D+C-B-A)          B<=X<C
C                           = 1-(D-C)/(D+C-B-A)*((D-X)/(D-C))**2 C<=X<D
C                           = 1                                  X>D
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(
     1'      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(A.LE.X .AND. X.LT.B)THEN
        TERM1=(B-A)/(D+C-B-A)
        TERM2=((X-A)/(B-A))**2
        CDF=TERM1*TERM2
      ELSEIF(B.LE.X .AND. X.LT.C)THEN
        CDF=((B-A) + 2*(X-B))/(D+C-B-A)
      ELSEIF(C.LE.X .AND. X.LT.D)THEN
        TERM1=(D-C)/(D+C-B-A)
        TERM2=((D-X)/(D-C))**2
        CDF=1.0 - TERM1*TERM2
      ELSEIF(X.GE.D)THEN
        CDF=1.0
      ELSE
        CDF=0.0
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     PURPOSE--PRINT OUT A TRACE LINE FOR DEBUGGING.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--JANUARY 1979.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
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
      WRITE(ICOUT,105)ISTEPN,ISUBN1,ISUBN2
  105 FORMAT('TRACE AT STEP ',A4,' OF ',A4,A4)
      CALL DPWRST('XXX','BUG ')
C
      RETURN
      END
      INTEGER FUNCTION TRADES (XPRIME, MPRIME, YPRIME, NPRIME)
C
C        ALGORITHM AS 304.4 APPL.STATIST. (1996), VOL.45, NO.3
C
C        Returns the number of 1-for-1 trades that refutes the null
C        hypothesis.  Assumes that XPRIME has the smaller mean and
C        that both arrays are sorted in ascending order.
C
C        DATAPLOT NOTE: UTILITY ROUTINE USED BY FISHER TWO SAMPLE
C                       RANDOMIZATION TEST
C
      INTEGER MPRIME, NPRIME
      REAL XPRIME(*), YPRIME(*)
C
      INTEGER I, J
C
      TRADES = 0
      I = 1
      J = 1
   10 IF (J .GT. NPRIME) GOTO 40
   20 IF (XPRIME(I) .GE. YPRIME(J)) GOTO 30
      I = I + 1
      IF (I .LE. MPRIME) GOTO 20
   30 TRADES = TRADES + (MPRIME - I + 1)
      J = J + 1
      IF (I .LE. MPRIME) GOTO 10
C
   40 RETURN
      END
      SUBROUTINE TRAN(N,ANU,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE STUDENT'S T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --NU     = THE INTEGER DEGREES OF FREEDOM
C                                (PARAMETER) FOR THE T
C                                DISTRIBUTION.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE STUDENT'S T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG, SQRT, SIN, COS.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--MOOD AND GRABLE, INTRODUCTION TO THE
C                 THEORY OF STATISTICS, 1963, PAGE 233.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGE 94.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGE 121.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--NOVEMBER  1975.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       2004. SUPPORT NON-INTEGER DEGREES
C                                       OF FREEDOM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(2),Z(2)
C
      CHARACTER*4 ICASE
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
      DATA PI/3.14159265359/
      DATA EPS/0.00001/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)GOTO50
      IF(ANU.LE.0.0)GOTO60
      GOTO90
   50 WRITE(ICOUT,5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      RETURN
   60 WRITE(ICOUT,15)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,48)ANU
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    5 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',
     1'TRAN   SUBROUTINE IS NON-POSITIVE *****')
   15 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',
     1'TRAN   SUBROUTINE IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F12.5,' *****')
C
      NU=INT(ANU+0.1)
      ANU2=REAL(NU)
      IF(ABS(ANU-ANU2).LE.EPS)THEN
        ICASE='INTE'
        IF(NU.EQ.0)THEN
          ICASE='REAL'
          ANU=EPS
        ENDIF
      ELSE
        ICASE='REAL'
      ENDIF
C
C     CASE 1: INTEGER DEGREES OF FREEDOM
C
      IF(ICASE.EQ.'INTE')THEN
C       GENERATE N STUDENT'S T RANDOM NUMBERS
C       USING THE DEFINITION THAT
C       A STUDENT'S T VARIATE WITH NU DEGREES OF FREEDOM
C       EQUALS A NORMAL VARIATE DIVIDED BY
C       A STANDARDIZED CHI VARIATE
C       (WHERE THE LATTER EQUALS SQRT(CHI-SQUARED/NU).
C       FIRST GENERATE A NORMAL RANDOM NUMBER,
C       THEN GENERATE A STANDARDIZED CHI RANDOM NUMBER,
C       THEN FORM THE RATIO OF THE FIRST DIVIDED BY
C       THE SECOND.
C
        ANU=NU
        DO100I=1,N
C
        CALL UNIRAN(2,ISEED,Y)
        ARG1=-2.0*LOG(Y(1))
        ARG2=2.0*PI*Y(2)
        ZNORM=(SQRT(ARG1))*(COS(ARG2))
C
        SUM=0.0
        DO200J=1,NU,2
        CALL UNIRAN(2,ISEED,Y)
        ARG1=-2.0*LOG(Y(1))
        ARG2=2.0*PI*Y(2)
        Z(1)=(SQRT(ARG1))*(COS(ARG2))
        Z(2)=(SQRT(ARG1))*(SIN(ARG2))
        SUM=SUM+Z(1)*Z(1)
        IF(J.EQ.NU)GOTO200
        SUM=SUM+Z(2)*Z(2)
  200   CONTINUE
C
        X(I)=ZNORM/SQRT(SUM/ANU)
C
  100  CONTINUE
C
      ELSE
        DO300I=1,N
          ATEMP=RDT(ANU,ISEED)
          X(I)=ATEMP
  300   CONTINUE
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN02(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 2, defined as
C
C      TRAN02(X) = integral 0 to X { t**2 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN02 = x to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large x contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN02 = VALINF  -  x**2 exp(-x)
C                    The recommended value is 2/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:    23 January, 1996
C
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN02'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 2 , 2.0 D 0 /
      DATA VALINF/0.32898 68133 69645 28729 D 1/
      DATA ATRAN/1.67176 04464 34538 50301  D    0,
     1          -0.14773 53599 46794 48986  D    0,
     2           0.14821 38199 46936 3384   D   -1,
     3          -0.14195 33032 63056 126    D   -2,
     4           0.13065 41324 41570 83     D   -3,
     5          -0.11715 57958 67579 0      D   -4,
     6           0.10333 49844 57557        D   -5,
     7          -0.90191 13042 227          D   -7,
     8           0.78177 16983 31           D   -8,
     9          -0.67445 65684 0            D   -9,
     X           0.57994 63945              D  -10,
     1          -0.49747 6185               D  -11,
     2           0.42596 097                D  -12,
     3          -0.36421 89                 D  -13,
     4           0.31108 6                  D  -14,
     5          -0.26547                    D  -15,
     6           0.2264                     D  -16,
     7          -0.193                      D  -17,
     8           0.16                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN02 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN02--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = ONE / (HALF * XK)
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW1 ) THEN
            TRAN02 =  ( X ** ( NUMJN - 1 ) ) / ( RNUMJN - ONE ) 
         ELSE
            T = ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
            TRAN02 = ( X ** ( NUMJN - 1 ) ) * CHEVAL(NTERMS,ATRAN,T) 
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X ) + 1
               T = EXP(-X) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG(X) - X + LOG(SUMEXP) 
         IF ( T .LT. XHIGH3 ) THEN
            TRAN02 = VALINF
         ELSE
            TRAN02 = VALINF - EXP(T) 
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN03(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 3, defined as
C
C      TRAN03(X) = integral 0 to X { t**3 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN03 = 0.0 to machine
C                    precision. The recommended value is
C                          square root of (2*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN03 = X**2/2 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN03 = VALINF  -  X**3 exp(-X)
C                    The recommended value is 3/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:   23 January, 1996
C
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN03'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0/
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 3 , 3.0 D 0 /
      DATA VALINF/0.72123 41418 95756 57124 D 1/
      DATA ATRAN/0.76201 25432 43872 00657  D    0,
     1          -0.10567 43877 05058 53250  D    0,
     2           0.11977 80848 19657 8097   D   -1,
     3          -0.12144 01520 36983 073    D   -2,
     4           0.11550 99769 39285 47     D   -3,
     5          -0.10581 59921 24422 9      D   -4,
     6           0.94746 63385 3018         D   -6,
     7          -0.83622 12128 581          D   -7,
     8           0.73109 09927 75           D   -8,
     9          -0.63505 94778 8            D   -9,
     X           0.54911 82819              D  -10,
     1          -0.47321 3954               D  -11,
     2           0.40676 948                D  -12,
     3          -0.34897 06                 D  -13,
     4           0.29892 3                  D  -14,
     5          -0.25574                    D  -15,
     6           0.2186                     D  -16,
     7          -0.187                      D  -17,
     8           0.16                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN03 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN03--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XLOW2 = SQRT( D1MACH(1) / HALF )
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN03 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN03 = ( X**(NUMJN-1) ) / ( RNUMJN - ONE )
            ELSE
               T = ( ( ( X*X ) / EIGHT ) - HALF ) - HALF
               TRAN03 = ( X**(NUMJN-1) ) * CHEVAL(NTERMS,ATRAN,T)
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT(XHIGH1/X) + 1
               T = EXP(-X)
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X )
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG(X) - X + LOG(SUMEXP)
         IF ( T .LT. XHIGH3 ) THEN
            TRAN03 = VALINF
         ELSE
            TRAN03 = VALINF - EXP(T)
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN04(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 4, defined as
C
C      TRAN04(X) = integral 0 to X { t**4 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN04 = 0.0 to machine
C                   precision. The recommended value is
C                          cube root of (3*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN04 = X**3/3 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN04 = VALINF  -  X**4 exp(-X)
C                    The recommended value is 4/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C
C    For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:   23 January, 1996
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN04'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 4 , 4.0 D 0 / 
      DATA VALINF/0.25975 75760 90673 16596 D 2/
      DATA ATRAN/0.48075 70994 61511 05786  D    0,
     1          -0.81753 78810 32108 3956   D   -1,
     2           0.10027 00665 97516 2973   D   -1,
     3          -0.10599 33935 98201 507    D   -2,
     4           0.10345 06245 03040 53     D   -3,
     5          -0.96442 70548 58991        D   -5,
     6           0.87455 44408 5147         D   -6,
     7          -0.77932 12079 811          D   -7,
     8           0.68649 88614 10           D   -8,
     9          -0.59995 71076 4            D   -9,
     X           0.52136 62413              D  -10,
     1          -0.45118 3819               D  -11,
     2           0.38921 592                D  -12,
     3          -0.33493 60                 D  -13,
     4           0.28766 7                  D  -14,
     5          -0.24668                    D  -15,
     6           0.2113                     D  -16,
     7          -0.181                      D  -17,
     8           0.15                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN04 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN04--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN04 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN04 =  ( X ** ( NUMJN-1 ) ) / ( RNUMJN - ONE ) 
            ELSE
               T = ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
               TRAN04 = ( X ** ( NUMJN-1 ) ) * CHEVAL(NTERMS,ATRAN,T) 
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X ) + 1
               T = EXP ( -X ) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE/ ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG( X ) - X + LOG( SUMEXP ) 
         IF ( T .LT. XHIGH3 ) THEN
            TRAN04 = VALINF
         ELSE
            TRAN04 = VALINF - EXP( T ) 
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN05(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order n, defined as
C
C      TRAN05(X) = integral 0 to X { t**5 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN05 = 0.0 to machine
C                   precision. The recommended value is
C                          4th root of (4*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN05 = X**4/4 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN05 = VALINF  -  X**5 exp(-X)
C                    The recommended value is 5/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:  23 January, 1996
C
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCC  CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN05'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 5 , 5.0 D 0 /
      DATA VALINF/0.12443 13306 17204 39116 D 3/
      DATA ATRAN/0.34777 77771 33910 78928  D    0,
     1          -0.66456 98897 60504 2801   D   -1,
     2           0.86110 72656 88330 882    D   -2,
     3          -0.93966 82223 75553 84     D   -3,
     4           0.93632 48060 81513 4      D   -4,
     5          -0.88571 31934 08328        D   -5,
     6           0.81191 49891 4503         D   -6,
     7          -0.72957 65423 277          D   -7,
     8           0.64697 14550 45           D   -8,
     9          -0.56849 02825 5            D   -9,
     X           0.49625 59787              D  -10,
     1          -0.43109 3996               D  -11,
     2           0.37310 094                D  -12,
     3          -0.32197 69                 D  -13,
     4           0.27722 0                  D  -14,
     5          -0.23824                    D  -15,
     6           0.2044                     D  -16,
     7          -0.175                      D  -17,
     8           0.15                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN05 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN05--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN05 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN05 =  ( X ** ( NUMJN - 1 ) ) / ( RNUMJN - ONE ) 
            ELSE
               T = ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
               TRAN05 = ( X ** ( NUMJN-1 ) ) * CHEVAL(NTERMS,ATRAN,T) 
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X )  + 1
               T = EXP ( -X ) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG ( X ) - X + LOG( SUMEXP ) 
         IF ( T .LT. XHIGH3 ) THEN
            TRAN05 = VALINF
         ELSE
            TRAN05 = VALINF - EXP( T ) 
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN06(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 6, defined as
C
C      TRAN06(X) = integral 0 to X { t**6 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN06 = 0.0 to machine
C                   precision. The recommended value is
C                          5th root of (5*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN06 = X**5/5 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN06 = VALINF  -  X**6 exp(-X)
C                    The recommended value is 6/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:   23 January, 1996
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN06'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 6 , 6.0 D 0 /
      DATA VALINF/0.73248 70046 28803 38059 D 3/
      DATA ATRAN/0.27127 33539 78400 08227  D    0,
     1          -0.55886 10553 19145 3393   D   -1,
     2           0.75391 95132 90083 056    D   -2,
     3          -0.84351 13857 92112 19     D   -3,
     4           0.85490 98079 67670 2      D   -4,
     5          -0.81871 54932 93098        D   -5,
     6           0.75754 24042 7986         D   -6,
     7          -0.68573 06541 831          D   -7,
     8           0.61170 03760 31           D   -8,
     9          -0.54012 70702 4            D   -9,
     X           0.47343 06435              D  -10,
     1          -0.41270 1055               D  -11,
     2           0.35825 603                D  -12,
     3          -0.30997 52                 D  -13,
     4           0.26750 1                  D  -14,
     5          -0.23036                    D  -15,
     6           0.1980                     D  -16,
     7          -0.170                      D  -17,
     8           0.15                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN06 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN06--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4 .0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN06 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN06 =  ( X ** ( NUMJN-1 ) ) / ( RNUMJN - ONE ) 
            ELSE
               T =  ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
               TRAN06 = ( X ** ( NUMJN-1 )  ) * CHEVAL(NTERMS,ATRAN,T) 
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4 .0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X ) + 1
               T = EXP( - X ) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG( X ) - X + LOG( SUMEXP ) 
         IF ( T .LT. XHIGH3 ) THEN
            TRAN06 = VALINF
         ELSE
            TRAN06 = VALINF - EXP( T ) 
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN07(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 7, defined as
C
C      TRAN07(X) = integral 0 to X { t**7 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN07 = 0.0 to machine
C                   precision. The recommended value is
C                          6th root of (6*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN07 = X**6/6 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN07 = VALINF  -  X**7 exp(-X)
C                    The recommended value is 7/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:   23 January, 1996
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN07'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0/
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 7 , 7.0 D 0/
      DATA VALINF/0.50820 80358 00489 10473 D 4/
      DATA ATRAN/0.22189 25073 40104 04423  D    0,
     1          -0.48167 51061 17799 3694   D   -1,
     2           0.67009 24481 03153 629    D   -2,
     3          -0.76495 18344 30825 57     D   -3,
     4           0.78634 85592 34869 0      D   -4,
     5          -0.76102 51808 87504        D   -5,
     6           0.70991 69629 9917         D   -6,
     7          -0.64680 25624 903          D   -7,
     8           0.58003 92339 60           D   -8,
     9          -0.51443 37014 9            D   -9,
     X           0.45259 44183              D  -10,
     1          -0.39580 0363               D  -11,
     2           0.34453 785                D  -12,
     3          -0.29882 92                 D  -13,
     4           0.25843 4                  D  -14,
     5          -0.22297                    D  -15,
     6           0.1920                     D  -16,
     7          -0.165                      D  -17,
     8           0.14                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN07 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN07--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x <= 4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN07 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN07 = ( X**(NUMJN-1) ) / ( RNUMJN - ONE )
            ELSE
               T = ( ( ( X*X ) / EIGHT ) - HALF ) - HALF
               TRAN07 = ( X**(NUMJN-1) ) * CHEVAL(NTERMS,ATRAN,T)
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1/X ) + 1
               T = EXP( -X )
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X )
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG(X) - X + LOG(SUMEXP)
         IF ( T .LT. XHIGH3 ) THEN
            TRAN07 = VALINF
         ELSE
            TRAN07 = VALINF - EXP(T)
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN08(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 8, defined as
C
C      TRAN08(X) = integral 0 to X { t**8 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN08 = 0.0 to machine
C                   precision. The recommended value is
C                          7th root of (7*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN08 = X**7/7 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN08 = VALINF  -  X**8 exp(-X)
C                    The recommended value is 8/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:  23 January, 1996
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCC  CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN08'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 8 , 8.0 D 0 /
      DATA VALINF/0.40484 39900 19011 15764 D 5/
      DATA ATRAN/0.18750 69577 40437 19233  D    0,
     1          -0.42295 27646 09367 3337   D   -1,
     2           0.60281 48569 29065 592    D   -2,
     3          -0.69961 05481 18147 76     D   -3,
     4           0.72784 82421 29878 9      D   -4,
     5          -0.71084 62500 50067        D   -5,
     6           0.66786 70689 0115         D   -6,
     7          -0.61201 57501 844          D   -7,
     8           0.55146 52644 74           D   -8,
     9          -0.49105 30705 2            D   -9,
     X           0.43350 00869              D  -10,
     1          -0.38021 8700               D  -11,
     2           0.33182 369                D  -12,
     3          -0.28845 12                 D  -13,
     4           0.24995 8                  D  -14,
     5          -0.21605                    D  -15,
     6           0.1863                     D  -16,
     7          -0.160                      D  -17,
     8           0.14                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN08 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN08--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN08 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN08 = ( X ** ( NUMJN - 1 ) ) / ( RNUMJN - ONE ) 
            ELSE
               T = ( ( ( X * X ) / EIGHT ) - HALF )  - HALF
               TRAN08 = ( X ** ( NUMJN - 1 ) ) * CHEVAL(NTERMS,ATRAN,T) 
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X ) + 1
               T = EXP ( - X ) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG( X ) - X + LOG( SUMEXP ) 
         IF ( T .LT. XHIGH3 ) THEN
            TRAN08 = VALINF
         ELSE
            TRAN08 = VALINF - EXP( T ) 
         ENDIF
      ENDIF
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRAN09(XVALUE)
C
C  DESCRIPTION:
C
C    This program calculates the transport integral of order 9, defined as
C
C      TRAN09(X) = integral 0 to X { t**9 exp(t)/[exp(t)-1]**2 } dt
C
C    The program uses a Chebyshev series, the coefficients of which are
C    given to an accuracy of 20 decimal places.
C
C
C  ERROR RETURNS:
C
C    If XVALUE < 0.0, an error message is printed, and the program 
C    returns the value 0.0. 
C
C
C  MACHINE-DEPENDENT CONSTANTS:
C
C    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
C                       The recommended value is such that
C                             ATRAN(NTERMS) < EPS/100
C
C    XLOW2 - DOUBLE PRECISION - The value below which TRAN09 = 0.0 to machine
C                   precision. The recommended value is
C                          8th root of (8*XMIN)
C
C    XLOW1 - DOUBLE PRECISION - The value below which TRAN09 = X**8/8 to
C                   machine precision. The recommended value is
C                             sqrt(8*EPSNEG)
C
C    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
C                    large X contains only one term. The recommended value
C                    is        - ln(EPS).
C
C    XHIGH2 - DOUBLE PRECISION - The value above which 
C                       TRAN09 = VALINF  -  X**9 exp(-X)
C                    The recommended value is 9/EPS
C
C    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
C                    for large x.
C
C     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
C
C     The machine-dependent constants are computed internally by
C     using the D1MACH subroutine.
C
C
C  INTRINSIC FUNCTIONS USED:
C
C     EXP, INT, LOG, SQRT
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C  AUTHOR:
C
C     DR. ALLAN J. MACLEOD,
C     DEPT. OF MATHEMATICS AND STATISTICS,
C     UNIVERSITY OF PAISLEY ,
C     HIGH ST.,
C     PAISLEY,
C     SCOTLAND.
C     PA1 2BE.
C
C     (e-mail: macl_ms0@paisley.ac.uk )
C
C
C  LATEST REVISION:   23 January, 1996
C
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,
     &     RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,
     &     XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*14
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
CCCCC DATA FNNAME/'TRAN09'/
CCCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0 D 0 , 0.5 D 0 , 1.0 D 0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0 D 0 , 8.0 D 0 , 100.0 D 0 /
      DATA NUMJN,RNUMJN/ 9 , 9.0 D 0 /
      DATA VALINF/0.36360 88055 88728 71397 D 6/
      DATA ATRAN/0.16224 04999 19498 46835  D    0,
     1          -0.37683 51452 19593 7773   D   -1,
     2           0.54766 97159 17719 770    D   -2,
     3          -0.64443 94500 94495 21     D   -3,
     4           0.67736 45285 28098 3      D   -4,
     5          -0.66681 34975 82042        D   -5,
     6           0.63047 56001 9047         D   -6,
     7          -0.58074 78663 611          D   -7,
     8           0.52555 13051 23           D   -8,
     9          -0.46968 86176 1            D   -9,
     X           0.41593 95065              D  -10,
     1          -0.36580 8491               D  -11,
     2           0.32000 794                D  -12,
     3          -0.27876 51                 D  -13,
     4           0.24201 7                  D  -14,
     5          -0.20953                    D  -15,
     6           0.1810                     D  -16,
     7          -0.156                      D  -17,
     8           0.13                       D  -18,
     9          -0.1                        D  -19/
C
C  Start execution
C
      X = XVALUE
C
C  Error test
C
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN09 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN09--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GOTO 19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
C
C   Code for x < =  4.0
C
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW2 ) THEN
            TRAN09 = ZERO
         ELSE
            IF ( X .LT. XLOW1 ) THEN
               TRAN09 = ( X ** ( NUMJN - 1 ) ) / ( RNUMJN - ONE ) 
            ELSE
               T = ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
               TRAN09 = ( X ** ( NUMJN-1 ) ) * CHEVAL(NTERMS,ATRAN,T) 
            ENDIF
         ENDIF
      ELSE
C 
C  Code for x > 4.0
C
         IF ( X .GT. XHIGH2 ) THEN
            SUMEXP = ONE
         ELSE
            IF ( X .LE. XHIGH1 ) THEN
               NUMEXP = INT ( XHIGH1 / X ) + 1
               T = EXP( -X ) 
            ELSE
               NUMEXP = 1
               T = ONE
            ENDIF
            RK = ZERO
            DO 100 K1 = 1 , NUMEXP
               RK = RK + ONE
  100       CONTINUE
            SUMEXP = ZERO
            DO 300 K1 = 1 , NUMEXP
               SUM2 = ONE
               XK = ONE / ( RK * X ) 
               XK1 = ONE
               DO 200 K2 = 1 , NUMJN
                  SUM2 = SUM2 * XK1 * XK + ONE
                  XK1 = XK1 + ONE
  200          CONTINUE
               SUMEXP = SUMEXP * T + SUM2
               RK = RK - ONE
  300       CONTINUE
         ENDIF
         T = RNUMJN * LOG( X ) - X + LOG( SUMEXP ) 
         IF ( T.LT.XHIGH3 ) THEN
            TRAN09 = VALINF
         ELSE
            TRAN09 = VALINF - EXP( T ) 
         ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE TRAPDF(X,A,B,C,D,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
C                  f(X,A,B,C,D) = U*((X-A)/(B-A))     A <= X <  B
C                               = U                   B <= X <  C
C                               = U*((D-X)/(D-C))     C <= X <  D
C                               = 0                   X < A, X >= D
C              WHERE
C                  U = 2/(D+C-B-A), A <= B <= C <= D
C              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF 
C              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(
     1'      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(A.LE.X .AND. X.LT.B)THEN
        U=2.0/(D+C-B-A)
        PDF=U*((X-A)/(B-A))
      ELSEIF(B.LE.X .AND. X.LT.C)THEN
        U=2.0/(D+C-B-A)
        PDF=U
      ELSEIF(C.LE.X .AND. X.LT.D)THEN
        U=2.0/(D+C-B-A)
        PDF=U*((D-X)/(D-C))
      ELSE
        PDF=0.0
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,IXP,IYP)
C
C     PURPOSE--TRANSLATE AND ROTATE POINTS X AND Y
C              WHICH ARE BEING GENERATED ALONG THE X AXIS
C              TO CORRESPONDING POINTS IN WHICH THE
C              ORIGIN (0,0) HAS BEEN TRANSLATED TO (X1,Y1)
C              AND THE LINE SEQUENCE HAS BEEN ROTATED BY AN ANGLE THETA.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--OCTOBER   1980
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C
C-----START POINT-----------------------------------------------------
C
      XROT=X*COS(THETA)-Y*SIN(THETA)
      YROT=X*SIN(THETA)+Y*COS(THETA)
C
      IF(DELX.GE.0.0)GOTO110
      GOTO120
C
  110 CONTINUE
      XP=X1+XROT
      YP=Y1+YROT
      GOTO900
C
  120 CONTINUE
      XP=X1-XROT
      YP=Y1-YROT
      GOTO900
C
  900 CONTINUE
      IXP=XP+0.5
      IYP=YP+0.5
C
      RETURN
      END
      SUBROUTINE TRAPPF(P,A,B,C,D,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
C              F(X,A,B,C,D) = 0                   X <  A
C                           = (B-A)/(D+C-B-A)*((X-A)/(B-A))**2   A<=X<B
C                           = ((B-A)+2*(X-B))/(D+C-B-A)          B<=X<C
C                           = 1-(D-C)/(D+C-B-A)*((D-X)/(D-C))**2 C<=X<D
C                           = 1                                  X>D
C              THE ALGORITHM FOR THE PPF IS TO COMPUTE THE CDF AT
C              X = A, X = B, X = C, AND X = D TO FIND THE APPROPRIATE
C              INTERVAL FOR P.  THEN INVERT THE APPROPRIATE EQUATION
C              ABOVE TO FIND THE PPF VALUE.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
C
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(
     1'      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
   22 FORMAT(
     1'***** FATAL ERROR--FOR THE TRAPEZOID PERCENT POINT FUNCTION,')
   23 FORMAT(
     1'      THE VALUE OF THE INPUTR ARGUMENT IS OUTSIDE THE ',
     1'ALLOWABLE (0,1] INTERVAL.')
   26 FORMAT(
     1'      VALUE OF INPUT ARGUMENT = ',E15.7)
C
C-----START POINT-----------------------------------------------------
C
      P1=0.0
      CALL TRACDF(B,A,B,C,D,P2)
      CALL TRACDF(C,A,B,C,D,P3)
      P4=1.0
C
      IF(P.EQ.0.0)THEN
        PPF=A
        GOTO9000
      ELSEIF(P.EQ.1.0)THEN
        PPF=D
        GOTO9000
      ELSEIF(P.EQ.P2)THEN
        PPF=B
        GOTO9000
      ELSEIF(P.EQ.P3)THEN
        PPF=C
        GOTO9000
      ENDIF
C
      IF(P.GE.P1 .AND. P.LE.P2)THEN
        TERM1=(B-A)/(D+C-B-A)
        TERM2=B-A
        PPF=TERM2*SQRT(P/TERM1) + A
      ELSEIF(P.GE.P2 .AND. P.LE.P3)THEN
        TERM1=B-A
        TERM2=D+C-B-A
        PPF=0.5*(P*TERM2-TERM1) + B
      ELSEIF(P.GE.P3 .AND. P.LE.P4)THEN
        TERM1=(D-C)/(D+C-B-A)
        TERM2=D-C
        PPF=D - TERM2*SQRT((1.0-P)/TERM1)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TRARAN(N,A,B,C,D,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TRAPEZOID DISTRIBUTION
C              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
C                  f(X,A,B,C,D) = U*((X-A)/(B-A))     A <= X <  B
C                               = U                   B <= X <  C
C                               = U*((D-X)/(D-C))     C <= X <  D
C                               = 0                   X < A, X >= D
C              WHERE
C                  U = 2/(D+C-B-A), A <= B <= C <= D
C              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF 
C              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TRAPEZOID DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TRAPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C               --TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 14-15.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003.6
C     ORIGINAL VERSION--JUNE      2003.
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE FIRST INPUT ARGUMENT TO THE ',
     1'TRARAN SUBROUTINE IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(
     1'      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
C
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TRAPEZOID RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
      P=X(I)
      CALL TRAPPF(P,A,B,C,D,PPF)
      X(I)=PPF
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TRED1(NM,N,A,D,E,E2)
C***BEGIN PROLOGUE  TRED1
C***DATE WRITTEN   760101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  D4C1B1
C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
C***AUTHOR  SMITH, B. T., ET AL.
C***PURPOSE  Reduce real symmetric matrix to symmetric tridiagonal
C            matrix using orthogonal similarity transformations.
C***DESCRIPTION
C
C     This subroutine is a translation of the ALGOL procedure TRED1,
C     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     This subroutine reduces a REAL SYMMETRIC matrix
C     to a symmetric tridiagonal matrix using
C     orthogonal similarity transformations.
C
C     On Input
C
C        NM must be set to the row dimension of two-dimensional
C          array parameters as declared in the calling program
C          dimension statement.
C
C        N is the order of the matrix.
C
C        A contains the real symmetric input matrix.  Only the
C          lower triangle of the matrix need be supplied.
C
C     On Output
C
C        A contains information about the orthogonal trans-
C          formations used in the reduction in its strict lower
C          triangle.  The full upper triangle of A is unaltered.
C
C        D contains the diagonal elements of the tridiagonal matrix.
C
C        E contains the subdiagonal elements of the tridiagonal
C          matrix in its last N-1 positions.  E(1) is set to zero.
C
C        E2 contains the squares of the corresponding elements of E.
C          E2 may coincide with E if the squares are not needed.
C
C     Questions and comments should be directed to B. S. Garbow,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C     ------------------------------------------------------------------
C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
C                 1976.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  TRED1
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),E2(N)
      REAL F,G,H,SCALE
C
C***FIRST EXECUTABLE STATEMENT  TRED1
      DO 100 I = 1, N
  100 D(I) = A(I,I)
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 1) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(A(I,K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = 0.0E0
         E2(I) = 0.0E0
         GO TO 290
C
  140    DO 150 K = 1, L
            A(I,K) = A(I,K) / SCALE
            H = H + A(I,K) * A(I,K)
  150    CONTINUE
C
         E2(I) = SCALE * SCALE * H
         F = A(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         A(I,L) = F - G
         IF (L .EQ. 1) GO TO 270
         F = 0.0E0
C
         DO 240 J = 1, L
            G = 0.0E0
C     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
  180       G = G + A(J,K) * A(I,K)
C
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
  200       G = G + A(K,J) * A(I,K)
C     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            F = F + E(J) * A(I,J)
  240    CONTINUE
C
         H = F / (H + H)
C     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = A(I,J)
            G = E(J) - H * F
            E(J) = G
C
            DO 260 K = 1, J
               A(J,K) = A(J,K) - F * E(K) - G * A(I,K)
  260    CONTINUE
C
  270    DO 280 K = 1, L
  280    A(I,K) = SCALE * A(I,K)
C
  290    H = D(I)
         D(I) = A(I,I)
         A(I,I) = H
  300 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRED2(NM,N,A,D,E,Z)
C***BEGIN PROLOGUE  TRED2
C***DATE WRITTEN   760101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  D4C1B1
C***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
C***AUTHOR  SMITH, B. T., ET AL.
C***PURPOSE  Reduce real symmetric matrix to symmetric tridiagonal
C            matrix using and accumulating orthogonal transformation
C***DESCRIPTION
C
C     This subroutine is a translation of the ALGOL procedure TRED2,
C     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     This subroutine reduces a REAL SYMMETRIC matrix to a
C     symmetric tridiagonal matrix using and accumulating
C     orthogonal similarity transformations.
C
C     On Input
C
C        NM must be set to the row dimension of two-dimensional
C          array parameters as declared in the calling program
C          dimension statement.
C
C        N is the order of the matrix.
C
C        A contains the real symmetric input matrix.  Only the
C          lower triangle of the matrix need be supplied.
C
C     On Output
C
C        D contains the diagonal elements of the tridiagonal matrix.
C
C        E contains the subdiagonal elements of the tridiagonal
C          matrix in its last N-1 positions.  E(1) is set to zero.
C
C        Z contains the orthogonal transformation matrix
C          produced in the reduction.
C
C        A and Z may coincide.  If distinct, A is unaltered.
C
C     Questions and comments should be directed to B. S. Garbow,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C     ------------------------------------------------------------------
C***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
C                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
C                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
C                 1976.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  TRED2
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),Z(NM,N)
      REAL F,G,H,HH,SCALE
C
C***FIRST EXECUTABLE STATEMENT  TRED2
      DO 100 I = 1, N
C
         DO 100 J = 1, I
            Z(I,J) = A(I,J)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 320
C     .......... FOR I=N STEP -1 UNTIL 2 DO -- ..........
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(Z(I,K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
C
  140    DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
C
         F = Z(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0E0
C
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / H
            G = 0.0E0
C     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
  180       G = G + Z(J,K) * Z(I,K)
C
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
  200       G = G + Z(K,J) * Z(I,K)
C     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
C
         HH = F / (H + H)
C     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
C
            DO 260 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  260    CONTINUE
C
  290    D(I) = H
  300 CONTINUE
C
  320 D(1) = 0.0E0
      E(1) = 0.0E0
C     .......... ACCUMULATION OF TRANSFORMATION MATRICES ..........
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0E0) GO TO 380
C
         DO 360 J = 1, L
            G = 0.0E0
C
            DO 340 K = 1, L
  340       G = G + Z(I,K) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  360    CONTINUE
C
  380    D(I) = Z(I,I)
         Z(I,I) = 1.0E0
         IF (L .LT. 1) GO TO 500
C
         DO 400 J = 1, L
            Z(I,J) = 0.0E0
            Z(J,I) = 0.0E0
  400    CONTINUE
C
  500 CONTINUE
C
      RETURN
      END
      SUBROUTINE TREGUP(NR,N,X,F,G,A,SC,SX,NWTAKE,STEPMX,
CDPLT SUBROUTINE TREGUP(NR,N,X,F,G,A,OPTFCN,SC,SX,NWTAKE,STEPMX,
     +     STEPTL,
     +     DLT,IRETCD,XPLSP,FPLSP,XPLS,FPLS,MXTAKE,IPR,METHOD,UDIAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C DECIDE WHETHER TO ACCEPT XPLS=X+SC AS THE NEXT ITERATE AND UPDATE THE
C TRUST REGION DLT.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT AT OLD ITERATE, G(X), OR APPROXIMATE
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
C                  LOWER TRIANGULAR PART AND DIAGONAL.
C                  HESSIAN OR APPROX IN UPPER TRIANGULAR PART
C OPTFCN       --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C SC(N)        --> CURRENT STEP
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C NWTAKE       --> BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C DLT         <--> TRUST REGION RADIUS
C IRETCD      <--> RETURN CODE
C                    =0 XPLS ACCEPTED AS NEXT ITERATE;
C                       DLT TRUST REGION FOR NEXT ITERATION.
C                    =1 XPLS UNSATISFACTORY BUT ACCEPTED AS NEXT ITERATE
C                       BECAUSE XPLS-X .LT. SMALLEST ALLOWABLE
C                       STEP LENGTH.
C                    =2 F(XPLS) TOO LARGE.  CONTINUE CURRENT ITERATION
C                       WITH NEW REDUCED DLT.
C                    =3 F(XPLS) SUFFICIENTLY SMALL, BUT QUADRATIC MODEL
C                       PREDICTS F(XPLS) SUFFICIENTLY WELL TO CONTINUE
C                       CURRENT ITERATION WITH NEW DOUBLED DLT.
C XPLSP(N)    <--> WORKSPACE [VALUE NEEDS TO BE RETAINED BETWEEN
C                  SUCCESIVE CALLS OF K-TH GLOBAL STEP]
C FPLSP       <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C METHOD       --> ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                    =1 LINE SEARCH
C                    =2 DOUBLE DOGLEG
C                    =3 MORE-HEBDON
C UDIAG(N)     --> DIAGONAL OF HESSIAN IN A(.,.)
C
      DIMENSION X(N),XPLS(N),G(N)
      DIMENSION SX(N),SC(N),XPLSP(N)
      DIMENSION A(NR,1)
      LOGICAL NWTAKE,MXTAKE
      DIMENSION UDIAG(N)
      DIMENSION FHAT(1)
C
      IPR=IPR
      MXTAKE=.FALSE.
      DO 100 I=1,N
        XPLS(I)=X(I)+SC(I)
  100 CONTINUE
      CALL OPTFCN(N,XPLS,FHAT)
      FPLS=FHAT(1)
      DLTF=FPLS-F
      SLP=DDOT(N,G,1,SC,1)
C
C NEXT STATEMENT ADDED FOR CASE OF COMPILERS WHICH DO NOT OPTIMIZE
C EVALUATION OF NEXT "IF" STATEMENT (IN WHICH CASE FPLSP COULD BE
C UNDEFINED).
      IF(IRETCD.EQ.4) FPLSP=0.0
C$    WRITE(IPR,961) IRETCD,FPLS,FPLSP,DLTF,SLP
      IF(IRETCD.NE.3 .OR. (FPLS.LT.FPLSP .AND. DLTF.LE. 1.E-4*SLP))
     +                                                     GO TO 130
C     IF(IRETCD.EQ.3 .AND. (FPLS.GE.FPLSP .OR. DLTF.GT. 1.E-4*SLP))
C     THEN
C
C       RESET XPLS TO XPLSP AND TERMINATE GLOBAL STEP
C
        IRETCD=0
        DO 110 I=1,N
          XPLS(I)=XPLSP(I)
  110   CONTINUE
        FPLS=FPLSP
        DLT=.5*DLT
C$      WRITE(IPR,951)
        GO TO 230
C     ELSE
C
C       FPLS TOO LARGE
C
  130   IF(DLTF.LE. 1.E-4*SLP) GO TO 170
C       IF(DLTF.GT. 1.E-4*SLP)
C       THEN
C$        WRITE(IPR,952)
          RLN=0.
          DO 140 I=1,N
            RLN=MAX(RLN,ABS(SC(I))/MAX(ABS(XPLS(I)),1./SX(I)))
  140     CONTINUE
C$        WRITE(IPR,962) RLN
          IF(RLN.GE.STEPTL) GO TO 150
C         IF(RLN.LT.STEPTL)
C         THEN
C
C           CANNOT FIND SATISFACTORY XPLS SUFFICIENTLY DISTINCT FROM X
C
            IRETCD=1
C$          WRITE(IPR,954)
            GO TO 230
C         ELSE
C
C           REDUCE TRUST REGION AND CONTINUE GLOBAL STEP
C
  150       IRETCD=2
            DLTMP=-SLP*DLT/(2.*(DLTF-SLP))
C$          WRITE(IPR,963) DLTMP
            IF(DLTMP.GE. .1*DLT) GO TO 155
C           IF(DLTMP.LT. .1*DLT)
C           THEN
              DLT=.1*DLT
              GO TO 160
C           ELSE
  155         DLT=DLTMP
C           ENDIF
  160       CONTINUE
C$          WRITE(IPR,955)
            GO TO 230
C         ENDIF
C       ELSE
C
C         FPLS SUFFICIENTLY SMALL
C
  170     CONTINUE
C$        WRITE(IPR,958)
          DLTFP=0.
          IF (METHOD .EQ. 3) GO TO 180
C
C         IF (METHOD .EQ. 2)
C         THEN
C
          DO 177 I = 1, N
             TEMP = 0.0
             DO 173 J = I, N
                TEMP = TEMP + (A(J, I)*SC(J))
  173        CONTINUE
             DLTFP = DLTFP + TEMP*TEMP
  177     CONTINUE
          GO TO 190
C
C         ELSE
C
  180     DO 187 I = 1, N
             DLTFP = DLTFP + UDIAG(I)*SC(I)*SC(I)
             IF (I .EQ. N) GO TO 187
             TEMP = 0
             IP1 = I + 1
             DO 183 J = IP1, N
                TEMP = TEMP + A(I, J)*SC(I)*SC(J)
  183        CONTINUE
             DLTFP = DLTFP + 2.0*TEMP
  187     CONTINUE
C
C         END IF
C
  190     DLTFP = SLP + DLTFP/2.0
C$        WRITE(IPR,964) DLTFP,NWTAKE
          IF(IRETCD.EQ.2 .OR. (ABS(DLTFP-DLTF).GT. .1*ABS(DLTF))
     +         .OR. NWTAKE .OR. (DLT.GT. .99*STEPMX)) GO TO 210
C         IF(IRETCD.NE.2 .AND. (ABS(DLTFP-DLTF) .LE. .1*ABS(DLTF))
C    +         .AND. (.NOT.NWTAKE) .AND. (DLT.LE. .99*STEPMX))
C         THEN
C
C           DOUBLE TRUST REGION AND CONTINUE GLOBAL STEP
C
            IRETCD=3
            DO 200 I=1,N
              XPLSP(I)=XPLS(I)
  200       CONTINUE
            FPLSP=FPLS
            DLT=MIN(2.*DLT,STEPMX)
C$          WRITE(IPR,959)
            GO TO 230
C         ELSE
C
C           ACCEPT XPLS AS NEXT ITERATE.  CHOOSE NEW TRUST REGION.
C
  210       CONTINUE
C$          WRITE(IPR,960)
            IRETCD=0
            IF(DLT.GT. .99*STEPMX) MXTAKE=.TRUE.
            IF(DLTF.LT. .1*DLTFP) GO TO 220
C           IF(DLTF.GE. .1*DLTFP)
C           THEN
C
C             DECREASE TRUST REGION FOR NEXT ITERATION
C
              DLT=.5*DLT
              GO TO 230
C           ELSE
C
C             CHECK WHETHER TO INCREASE TRUST REGION FOR NEXT ITERATION
C
  220         IF(DLTF.LE. .75*DLTFP) DLT=MIN(2.*DLT,STEPMX)
C           ENDIF
C         ENDIF
C       ENDIF
C     ENDIF
  230 CONTINUE
C$    WRITE(IPR,953)
C$    WRITE(IPR,956) IRETCD,MXTAKE,DLT,FPLS
C$    WRITE(IPR,957)
C$    WRITE(IPR,965) (XPLS(I),I=1,N)
      RETURN
C
CC951 FORMAT(55H TREGUP    RESET XPLS TO XPLSP. TERMINATION GLOBAL STEP)
CC952 FORMAT(26H TREGUP    FPLS TOO LARGE.)
CC953 FORMAT(38H0TREGUP    VALUES AFTER CALL TO TREGUP)
CC954 FORMAT(54H TREGUP    CANNOT FIND SATISFACTORY XPLS DISTINCT FROM,
CC   +       27H X.  TERMINATE GLOBAL STEP.)
CC955 FORMAT(53H TREGUP    REDUCE TRUST REGION. CONTINUE GLOBAL STEP.)
CC956 FORMAT(21H TREGUP       IRETCD=,I3/
CC   +       21H TREGUP       MXTAKE=,L1/
CC   +       21H TREGUP       DLT   =,E20.13/
CC   +       21H TREGUP       FPLS  =,E20.13)
CC957 FORMAT(32H TREGUP       NEW ITERATE (XPLS))
CC958 FORMAT(35H TREGUP    FPLS SUFFICIENTLY SMALL.)
CC959 FORMAT(54H TREGUP    DOUBLE TRUST REGION.  CONTINUE GLOBAL STEP.)
CC960 FORMAT(50H TREGUP    ACCEPT XPLS AS NEW ITERATE.  CHOOSE NEW,
CC   +       38H TRUST REGION.  TERMINATE GLOBAL STEP.)
CC961 FORMAT(18H TREGUP    IRETCD=,I5/
CC   +       18H TREGUP    FPLS  =,E20.13/
CC   +       18H TREGUP    FPLSP =,E20.13/
CC   +       18H TREGUP    DLTF  =,E20.13/
CC   +       18H TREGUP    SLP   =,E20.13)
CC962 FORMAT(18H TREGUP    RLN   =,E20.13)
CC963 FORMAT(18H TREGUP    DLTMP =,E20.13)
CC964 FORMAT(18H TREGUP    DLTFP =,E20.13/
CC   +       18H TREGUP    NWTAKE=,L1)
CC965 FORMAT(14H TREGUP       ,5(E20.13,3X))
      END
      SUBROUTINE TRESTR(POINTR, SBRGNS, PONTRS, RGNERS)
****BEGIN PROLOGUE TRESTR
****PURPOSE TRESTR maintains a heap for subregions.
****DESCRIPTION TRESTR maintains a heap for subregions.
*            The subregions are ordered according to the size of the
*            greatest error estimates of each subregion (RGNERS).
*
*   PARAMETERS
*
*     POINTR Integer.
*            The index for the subregion to be inserted in the heap.
*     SBRGNS Integer.
*            Number of subregions in the heap.
*     PONTRS Real array of dimension SBRGNS.
*            Used to store the indices for the greatest estimated errors
*            for each subregion.
*     RGNERS Real array of dimension SBRGNS.
*            Used to store the greatest estimated errors for each 
*            subregion.
*
****ROUTINES CALLED NONE
****END PROLOGUE TRESTR
*
*   Global variables.
*
      INTEGER POINTR, SBRGNS
      DOUBLE PRECISION PONTRS(*), RGNERS(*)
*
*   Local variables.
*
*   RGNERR Intermediate storage for the greatest error of a subregion.
*   SUBRGN Position of child/parent subregion in the heap.
*   SUBTMP Position of parent/child subregion in the heap.
*
      INTEGER SUBRGN, SUBTMP
      DOUBLE PRECISION RGNERR
*
****FIRST PROCESSING STATEMENT TRESTR
*     
      RGNERR = RGNERS(POINTR)
      IF ( POINTR .EQ. PONTRS(1)) THEN
*
*        Move the new subregion inserted at the top of the heap 
*        to its correct position in the heap.
*
         SUBRGN = 1
 10      SUBTMP = 2*SUBRGN
         IF ( SUBTMP .LE. SBRGNS ) THEN
            IF ( SUBTMP .NE. SBRGNS ) THEN
*     
*              Find maximum of left and right child.
*
               IF ( RGNERS(INT(PONTRS(SUBTMP))) .LT. 
     +              RGNERS(INT(PONTRS(SUBTMP+1))) ) SUBTMP = SUBTMP + 1
            ENDIF
*
*           Compare maximum child with parent.
*           If parent is maximum, then done.
*
            IF ( RGNERR .LT. RGNERS(INT(PONTRS(SUBTMP))) ) THEN
*     
*              Move the pointer at position subtmp up the heap.
*     
               PONTRS(SUBRGN) = PONTRS(SUBTMP)
               SUBRGN = SUBTMP
               GO TO 10
            ENDIF
         ENDIF
      ELSE
*
*        Insert new subregion in the heap.
*
         SUBRGN = SBRGNS
 20      SUBTMP = SUBRGN/2
         IF ( SUBTMP .GE. 1 ) THEN
*
*           Compare child with parent. If parent is maximum, then done.
*     
            IF ( RGNERR .GT. RGNERS(INT(PONTRS(SUBTMP))) ) THEN
*     
*              Move the pointer at position subtmp down the heap.
*
               PONTRS(SUBRGN) = PONTRS(SUBTMP)
               SUBRGN = SUBTMP
               GO TO 20
            ENDIF
         ENDIF
      ENDIF
      PONTRS(SUBRGN) = POINTR
*
****END TRESTR
*
      RETURN
      END
      SUBROUTINE TRIA25(X,M,N,RIGHT,X2,RIGHT2,IBUGA3)
C
C     PURPOSE--COMPUTE THE TRIANGULARIZED EQUIVALENT
C              OF THE M BY N MATRIX X.
C              THE TRIANGULARIZED EQUIVALENT
C              WILL BE FOUND IN THE UPPER RIGHT TRIANGLE
C              OF THE MATRIX X2.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION MATRIX
C                                WITH M ROWS AND N COLUMNS
C                                WHOSE TRIANGULARIZED
C                                EQUIVALENT IS DESIRED.
C                     --M      = THE INTEGER NUMBER OF
C                                ROWS IN X.
C                     --N      = THE INTEGER NUMBER OF
C                                COLUMNS IN X.
C                     --RIGHT  = THE SINGLE PRECISION VECTOR
C                                CONTAINING THE 'RIGHT-HAND
C                                SIDE' OF THE EQUATION.
C                     --IBUGA3 = A HOLLERITH BUG PARAMETER
C     OUTPUT ARGUMENTS--X2     = THE SINGLE PRECISION MATRIX
C                                WITH M ROWS AND N COLUMNS
C                                WITH THE TRIANGULARIZED
C                                EQUIVALENT OF X IN THE
C                                UPPER RIGHT TRIANGLE
C                                AND WITH ZEROS ELSEWHERE.
C                     --RIGHT2 = THE SINGLE PRECISION VECTOR
C                                CONTAINING THE ORIGINAL
C                                'RIGHT-HAND SIDE' BUT MODIFIED
C                                ACCORDING TO THE TRIANGULARIZATION
C                                THAT OCCURRED ON THE
C                                LEFT-HAND SIDE
C                                SIDE OF THE EQUATION.
C     NOTE--THE INPUT MATRIX X IS UNCHANGED
C           BY THIS SUBROUTINE.
C     NOTE--THE DIMENSIONS OF X AND X2 MUST BE THE SAME
C           IN THE CALLING ROUTINE AS IN THIS SUBROUTINE.
C           THEY HAVE BEEN SET HEREIN TO 25 BY 25,
C           AND HENCE THE 25 IN THE NAME OF THIS SUBROUTINE (TRIA25).
C     NOTE--TRIA25 IS IDENTICAL TO TRIA50 AND TRIANG
C           EXCEPT FOR THE DIMENSIONS.
C     NOTE--A CALL TO TRIA25 IS TYPICALLY
C           FOLLOWED BY A CALL TO BACK25
C           SO AS TO CARRY OUT THE
C           BACKSOLVING FOR THE COEFFICIENTS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--JUNE      1977.
C     UPDATED         --JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION X(25,25)
      DIMENSION RIGHT(*)
      DIMENSION X2(25,25)
      DIMENSION RIGHT2(*)
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
      ISUBN1='TRIA'
      ISUBN2='25  '
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TRIA25--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)M,N,IBUGA3
   52 FORMAT('M,N,IBUGA3 = ',2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,M
      WRITE(ICOUT,56)I,(X(I,J),J=1,N)
   56 FORMAT('I,X(I,.)   = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      DO60I=1,M
      WRITE(ICOUT,61)I,RIGHT(I)
   61 FORMAT('I,RIGHT(I) = ',I8,E10.3)
      CALL DPWRST('XXX','BUG ')
   60 CONTINUE
   90 CONTINUE
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  COPY THE X MATRIX INTO THE X2 MATRIX.          **
C               **  COPY THE VECTOR RIGHT INTO THE VECTOR RIGHT2.  **
C               *****************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO100I=1,M
      DO200J=1,N
      X2(I,J)=X(I,J)
  200 CONTINUE
      RIGHT2(I)=RIGHT(I)
  100 CONTINUE
C
C               *********************************************
C               **  STEP 2--                               **
C               **  DETERMINE K = THE MINIMUM OF M AND N.  **
C               *********************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      K=M
      IF(N.LT.M)K=N
      IF(K.EQ.1)GOTO9000
C
C               *********************************************************
C               **  STEP 3--                                           **
C               **  BEGIN GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.  **
C               **  OPERATE ON ONE ROW (OR COLUMN) AT A TIME.          **
C               **  THE ROW (OR COLUMN) OF INTEREST IS COLUMN J.       **
C               *********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      KM1=K-1
      DO400J=1,KM1
      JP1=J+1
C
C               ************************************************
C               **  STEP 3.1--                                **
C               **  FOR COLUMN J,                             **
C               **  DETERMINE THE ROW (ON OR BELOW DIAGONAL)  **
C               **  THAT HAS THE LARGEST ABSOLUTE VALUE.      **
C               **  THIS ROW WILL BE DESIGNATED AS ROW I2.    **
C               ************************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I2=J
      DO600I=JP1,M
      IF(ABS(X2(I,J)).GT.ABS(X2(I2,J)))I2=I
  600 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,610)J,I2
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,615)X2(I2,J)
  610 FORMAT('COLUMN J = ',I2,'  MAX FOUND IN ROW I2 = ',I2)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
  615 FORMAT('MAX VALUE = ',E12.5)
C
C               ****************************************************
C               **  STEP 3.2--                                    **
C               **  INTERCHANGE ROW I2 WITH ROW J                 **
C               **  BELOW AND TO THE RIGHT                        **
C               **  OF THE DIAGONAL ELEMENT X2(J,J) (INCLUSIVE).  **
C               ****************************************************
C
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(I2.EQ.J)GOTO700
      DO800J2=J,N
      HOLD=X2(J,J2)
      X2(J,J2)=X2(I2,J2)
      X2(I2,J2)=HOLD
  800 CONTINUE
      HOLD=RIGHT2(J)
      RIGHT2(J)=RIGHT2(I2)
      RIGHT2(I2)=HOLD
  700 CONTINUE
C
C               ****************************************************************
C               **  STEP 3.3--
C               **  MODIFY THE ROWS BELOW ROW J (& ONLY TO THE RIGHT OF COLUMN J
C               **  ALSO, ELIMINATE (SET TO 0) ELEMENTS IN COLUMN J
C               **  BELOW X2(J,J)
C               ****************************************************************
C
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1100I=JP1,M
      FACTOR=X2(I,J)/X2(J,J)
      DO1200J2=JP1,N
      X2(I,J2)=X2(I,J2)-FACTOR*X2(J,J2)
 1200 CONTINUE
      RIGHT2(I)=RIGHT2(I)-FACTOR*RIGHT2(J)
 1100 CONTINUE
C
      DO1300I=JP1,M
      X2(I,J)=0.0
 1300 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO1409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1400)J
 1400 FORMAT('***** IN TRIANG, AFTER OPERATING ON COLUMN ',I6)
      CALL DPWRST('XXX','BUG ')
      DO1405I=1,M
      WRITE(ICOUT,1410)I,(X2(I,J3),J3=1,N),RIGHT2(I)
 1410 FORMAT('I,X2(I,.),RIGHT2(I) = ',I8,11E8.1)
      CALL DPWRST('XXX','BUG ')
 1405 CONTINUE
 1409 CONTINUE
C
  400 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF TRIA25--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)M,N,IBUGA3
 9012 FORMAT('M,N,IBUGA3 = ',2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,M
      WRITE(ICOUT,9016)I,(X2(I,J),J=1,N)
 9016 FORMAT('I,X2(I,.)  = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      DO9020I=1,M
      WRITE(ICOUT,9021)I,RIGHT2(I)
 9021 FORMAT('I,RIGHT2(I)= ',I8,E10.3)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRICUB(RES,N,IWRITE,WEIGHT,IBUGA3,IERROR)
C     PURPOSE--DETERMINE THE N VERTICAL (ROBUST) WEIGHTS WEIGHT(.)
C              BASED ON A TRICUBE WEIGHTING SCHEME OF
C              THE RESIDUALS IN RES(.).
C     NOTE--IF ALL INPUT RESIDUALS ARE ZERO, THIS SUBROUTINE
C           WILL OUTPUT ALL WEIGHTS AS UNITY.
C     REFERENCE--CHAMBERS, ET AL.  GRAPHICAL METHODS FOR DATA ANALYSIS.
C                WADSWORTH, 1983, PAGES 98-101, 122-123.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--FEBRUARY   1988
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
      DIMENSION RES(*)
      DIMENSION WEIGHT(*)
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
      ISUBN1='TRIC'
      ISUBN2='UB  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TRICUB--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,IERROR
   52 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N.LE.0)GOTO63
      DO61I=1,N
      WRITE(ICOUT,62)I,RES(I)
   62 FORMAT('I,RES(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   61 CONTINUE
   63 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.GE.1)GOTO119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN TRICUB--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT FULL SAMPLE SIZE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)
  113 FORMAT('      FOR WHICH TRICUBE WEIGHTS')
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
      GOTO9000
  119 CONTINUE
C
C               ***********************************************
C               **  STEP 11--                                **
C               **  COMPUTE THE TRICUBE WEIGHTING           **
C               **     1) COMPUTE ABSOLUTE VALUE OF RESIDUALS
C               **     2) COMPUTE MEDIAN ABSOLUTE VALUE RESIDUAL
C               **     3) COMPUTE CUTOFF = +-6*M.A.R.
C               **     4) ASSIGN 0 WEIGHTS OUTSIDE OF REGION
C               **     5) ASSIGN TRICUBES INSIDE OF REGION
C               ***********************************************
C
      DO1100I=1,N
      WEIGHT(I)=ABS(RES(I))
 1100 CONTINUE
C
      CALL SORT(WEIGHT,N,WEIGHT)
      IEVODD=N-(N/2)*2
      NMID=N/2
      NMIDP1=NMID+1
      IF(IEVODD.EQ.0)XMEDAR=(WEIGHT(NMID)+WEIGHT(NMIDP1))/2.0
      IF(IEVODD.EQ.1)XMEDAR=WEIGHT(NMIDP1)
C
      IF(XMEDAR.EQ.0.0)GOTO1110
      GOTO1120
C
 1110 CONTINUE
      CONST=(-999.0)
      DO1111I=1,N
      WEIGHT(I)=1.0
 1111 CONTINUE
      GOTO1190
C
 1120 CONTINUE
      CONST=6.0*XMEDAR
      DO1121I=1,N
      U=RES(I)/CONST
      U2=ABS(U)
      WEIGHT(I)=0.0
      IF(-1.0.LE.U.AND.U.LE.1.0)WEIGHT(I)=(1.0-U2**3)**3
 1121 CONTINUE
      GOTO1190
C
 1190 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF TRICUB--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)N
 9013 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)XMEDAR
 9014 FORMAT('XMEDAR = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(N.LE.0)GOTO9023
      DO9021I=1,N
      WRITE(ICOUT,9022)I,RES(I),WEIGHT(I)
 9022 FORMAT('I,RES(I),WEIGHT(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9023 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      double precision function trigam(x, ifault)
      implicit double precision (a-h,o-z) 
c
c        algorithm as121   Appl. Statist. (1978) vol 27, no. 1
c
c        calculates trigamma(x) = d**2(log(gamma(x))) / dx**2
c
      double precision a, b, one, half, b2, b4, b6,b8, x, y, z, zero
      data a, b, one, half /1.0d-4, 5.0d0, 1.0d0, 0.5d0/
      data zero /0.0d0/
c
c        b2, b4, b6 and b8 are Bernoulli numbers
c
      data b2, b4, b6,b8
     */0.1666666667d0, -0.03333333333d0, 0.02380952381, -0.03333333333/
c
c        check for positive value of x
c
      trigam = zero
      ifault = 1
      if (x.le.zero) return
      ifault = 0
      z = x
c
c        use small value approximation if x .le. a
c
      if (z .gt. a) goto 10
      trigam = one / (z * z)
      return
c
c        increase argument to (x+i) .ge. b
c
   10 if (z .ge. b) goto 20
      trigam = trigam + one / (z * z)
      z = z + one
      goto 10
c
c        apply asymptotic formula if argument .ge. b
c
   20 y = one / (z * z)
      trigam = trigam + half * y +
     * (one + y * (b2 + y * (b4 + y * (b6 + y * b8)))) / z
      return
      end
                                   
      SUBROUTINE TRIGD1(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
C
C     PURPOSE--COMPUTE DERIVATIVES FOR
C              THE 6 (CIRCULAR) TRIGONOMETRIC FUNCTIONS.
C
C     NOTE--LF11 = CODED SIN FUNCTION
C           LF12 = CODED COS FUNCTION
C           LF13 = CODED TAN FUNCTION
C           LF14 = CODED COT FUNCTION
C           LF15 = CODED SEC FUNCTION
C           LF16 = CODED CSC FUNCTION
C
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JANUARY   1981.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
C
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
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
C-----START POINT-----------------------------------------------------
C
      I1P1=I1+1
      I1P2=I1+2
C
      IF(IHLF1.EQ.'SIN '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'COS '.AND.IHLF2.EQ.'    ')GOTO620
      IF(IHLF1.EQ.'TAN '.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'COT '.AND.IHLF2.EQ.'    ')GOTO640
      IF(IHLF1.EQ.'SEC '.AND.IHLF2.EQ.'    ')GOTO650
      IF(IHLF1.EQ.'CSC '.AND.IHLF2.EQ.'    ')GOTO660
C
C     TREAT THE SINE CASE
C
  610 CONTINUE
      I2=I2+1
      IDERZ1(I2)='COS '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE COSINE CASE
C
  620 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SIN '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE TANGENT CASE
C
  630 CONTINUE
      I2=I2+1
      IDERZ1(I2)='SEC '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE COTANGENT CASE
C
  640 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='CSC '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE SECANT CASE
C
  650 CONTINUE
      I2=I2+1
      IDERZ1(I2)='SEC '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='TAN '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE COSECANT CASE
C
  660 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='CSC '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='COT '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
  980 CONTINUE
  985 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRIGD2(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
C
C     PURPOSE--COMPUTE DERIVATIVES FOR
C              THE 6 INVERSE (CIRCULAR) TRIGONOMETRIC FUNCTIONS.
C
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JANUARY   1981.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
C
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
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
C-----START POINT-----------------------------------------------------
C
      I1P1=I1+1
      I1P2=I1+2
C
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'IN  ')GOTO710
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OS  ')GOTO720
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'AN  ')GOTO730
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OT  ')GOTO740
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'EC  ')GOTO750
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SC  ')GOTO760
C
C     TREAT THE ARCSINE CASE
C
  710 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE ARCCOSINE CASE
C
  720 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE ARCTANGENT CASE
C
  730 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='+   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE ARCCOTANGENT CASE
C
  740 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='+   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE ARCSECANT CASE
C
  750 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE ARCCOSECANT CASE
C
  760 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
  980 CONTINUE
  985 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRIGD3(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
C
C     PURPOSE--COMPUTE DERIVATIVES FOR
C              THE 6 HYPERBOLIC TRIGONOMETRIC FUNCTIONS.
C
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JANUARY   1981.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
C
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
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
C-----START POINT-----------------------------------------------------
C
      I1P1=I1+1
      I1P2=I1+2
C
      IF(IHLF1.EQ.'SINH'.AND.IHLF2.EQ.'    ')GOTO810
      IF(IHLF1.EQ.'COSH'.AND.IHLF2.EQ.'    ')GOTO820
      IF(IHLF1.EQ.'TANH'.AND.IHLF2.EQ.'    ')GOTO830
      IF(IHLF1.EQ.'COTH'.AND.IHLF2.EQ.'    ')GOTO840
      IF(IHLF1.EQ.'SECH'.AND.IHLF2.EQ.'    ')GOTO850
      IF(IHLF1.EQ.'CSCH'.AND.IHLF2.EQ.'    ')GOTO860
C
C     TREAT THE HYPERBOLIC SINE CASE
C
  810 CONTINUE
      I2=I2+1
      IDERZ1(I2)='COSH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC COSINE CASE
C
  820 CONTINUE
      I2=I2+1
      IDERZ1(I2)='SINH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC TANGENT CASE
C
  830 CONTINUE
      I2=I2+1
      IDERZ1(I2)='SECH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC COTANGENT CASE
C
  840 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='CSCH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC SECANT CASE
C
  850 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SECH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='TANH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC COSECANT CASE
C
  860 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='CSCH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='COTH'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
  980 CONTINUE
  985 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRIGD4(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
C
C     PURPOSE--COMPUTE DERIVATIVES FOR
C              THE 6 INVERSE HYPERBOLIC TRIGONOMETRIC FUNCTIONS.
C
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JANUARY   1981.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
C
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
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
C-----START POINT-----------------------------------------------------
C
      I1P1=I1+1
      I1P2=I1+2
C
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'INH ')GOTO910
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OSH ')GOTO920
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'ANH ')GOTO930
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OTH ')GOTO940
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'ECH ')GOTO950
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SCH ')GOTO960
C
C     TREAT THE HYPERBOLIC ARCSINE CASE
C
  910 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='+   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC ARCCOSINE CASE
C
  920 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC ARCTANGENT CASE
C
  930 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC ARCCOTANGENT CASE
C
  940 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC ARCSECANT CASE
C
  950 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(17)=')   '
      I2=I2+1
      IDERZ1(18)=')   '
      I2=I2+1
      IDERZ1(19)=')   '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
C     TREAT THE HYPERBOLIC ARCCOSECANT CASE
C
  960 CONTINUE
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='/   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='SQRT'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='+   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P1)
      IDERZ2(I2)=IFUNZ2(I1P1)
      IF(ITYPE.EQ.'EXP ')I2=I2+1
      IF(ITYPE.EQ.'EXP ')IDERZ1(I2)=IFUNZ1(I1P2)
      IF(ITYPE.EQ.'EXP ')IDERZ2(I2)=IFUNZ2(I1P2)
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='2   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
  980 CONTINUE
  985 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRIMME(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,XTEMP,
     1                  IUPPER,XTRIM,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE TRIMMED MEAN = THE
C              SAMPLE (ON EACH SIDE) TRIMMED MEAN
C              OF THE DATA IN THE INPUT VECTOR X.
C      NOTE--PROP1 % OF THE DATA IS TRIMMED FROM THE LEFT SIDE;
C            PROP2 % OF THE DATA IS TRIMMED FROM THE RIGHT SIDE.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMMED FROM LEFT SIDE.
C                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMMED FROM RIGHT SIDE.
C     OUTPUT ARGUMENTS--XTRIM  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE TRIMMED MEAN.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TRIMMED MEAN.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 15000.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
C               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 1967, PAGES 357, 387.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY, 1969).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION--JULY      1973.
C     UPDATED         --OCTOBER   2012. ALLOW TRIMMING TO BE SPECIFIED
C                                       IN TERMS OF THE NUMBER OF VALUES.
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
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DK
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
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
      ISUBN1='TRIM'
      ISUBN2='ME  '
C
      IERROR='NO'
CCCCC IUPPER=1000
C
      NPROP1=0
      NPROP2=0
      NPROP3=0
      ISTART=0
      ISTOP=0
      DSUM=0.0D0
      DK=0.0D0
      PROP3=0.0
      XTRIM1=0.0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRIMME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NTRIM1 NTRIM2
   52   FORMAT('IBUGA3,ISUBRO,N,NTRIM1,NTRIM2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2
   54   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************
C               **  COMPUTE THE TRIMMED MEAN  **
C               ********************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
      IF(N.EQ.1)THEN
        XTRIM=X(1)
        IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,91)
   91     FORMAT('DATA HAS ONLY A SINGLE OBSERVATION.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,93)XTRIM
   93     FORMAT('THE TRIMMED MEAN SET EQUAL TO ',G15.7)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
      ENDIF
C
      IF(N.LT.1 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN TRIMMED MEAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS MUST BE BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      1 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',
     1         'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      XTRIM=HOLD
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)
  137   FORMAT('ALL DATA VALUES HAVE THE SAME VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)XTRIM
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  139 CONTINUE
C
C     2012/10: TRIMMING MAY NOW BE SPECIFIED EITHER IN TERMS OF
C              THE PROPORTION OR IN A SPECIFIC NUMBER OF OBSERVATIONS
C              TO BE TRIMMED.
C
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
          GOTO9000
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
          GOTO9000
        ENDIF
      ENDIF
C
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
          GOTO9000
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
          GOTO9000
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 2--                   **
C               **  COMPUTE THE TRIMMED MEAN.  **
C               *********************************
C
      CALL SORT(X,N,XTEMP)
C
      IF(IFLAG1.EQ.0)THEN
        NPROP1=(PROP1/100.0)*AN+0.0001
        ISTART=NPROP1+1
      ELSE
        NPROP1=NTRIM1
        ISTART=NPROP1+1
      ENDIF
C
      IF(IFLAG2.EQ.0)THEN
        NPROP2=(PROP2/100.0)*AN+0.0001
        ISTOP=N-NPROP2
      ELSE
        NPROP2=NTRIM2
        ISTOP=N-NPROP2
      ENDIF
C
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
        GOTO9000
      ENDIF
C
      DSUM=0.0
      K=0
      DO200I=ISTART,ISTOP
        K=K+1
        DX=XTEMP(I)
        DSUM=DSUM+DX
  200 CONTINUE
      NPROP3=K
      DK=K
      XTRIM=DSUM/DK
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
        PROP3=100.00-PROP1-PROP2
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA WERE TRIMMED   FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA WERE TRIMMED   FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA REMAIN IN MIDDLE AFTER TRIMMING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRIM
  821   FORMAT('THE TRIMMED MEAN OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMME')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TRIMME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PROP1,PROP2,PROP3
 9014   FORMAT('PROP1,PROP2,PROP3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NPROP1,NPROP2,NPROP3
 9015   FORMAT('NPROP1,NPROP2,NPROP3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISTART,ISTOP,DSUM,DK
 9016   FORMAT('ISTART,ISTOP,DSUM,DK = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IERROR,XTRIM
 9018   FORMAT('IERROR,XTRIM = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TRIMSD(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,
     1                  XTEMP,XTRIM,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE TRIMMED STANDARD DEVIATION
C              OF THE DATA IN THE INPUT VECTOR X.
C      NOTE--PROP1 % OF THE DATA IS TRIMSDD FROM THE LEFT SIDE;
C            PROP2 % OF THE DATA IS TRIMSDD FROM THE RIGHT SIDE.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMSDD FROM LEFT SIDE.
C                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMSDD FROM RIGHT SIDE.
C     OUTPUT ARGUMENTS--XTRIM  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE TRIMMED SD.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TRIMMED SD.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
C               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 1967, PAGES 357, 387.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY, 1969).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.5
C     ORIGINAL VERSION--MAY       2007.
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
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DK
      DOUBLE PRECISION DX
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
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
      ISUBN1='TRIM'
      ISUBN2='SD  '
C
      IERROR='NO'
C
      NPROP1=0
      NPROP2=0
      NPROP3=0
      ISTART=0
      ISTOP=0
      DSUM=0.0D0
      DSUM2=0.0D0
      DK=0.0D0
      PROP3=0.0
      XTRIM=0.0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMSD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRIMSD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NTRIM1 NTRIM2
   52   FORMAT('IBUGA3,ISUBRO,N,NTRIM1,NTRIM2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2
   54   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************
C               **  COMPUTE THE TRIMMED SD    **
C               ********************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.2 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN TRIMMED STANDARD DEVIAION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS MUST BE BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      2 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',
     1         'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
        IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      XTRIM=0.0
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)
  137   FORMAT('ALL DATA VALUES HAVE THE SAME VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)XTRIM
   93   FORMAT('THE TRIMMED STANDARD DEVIATION SET EQUAL TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  139 CONTINUE
C
C     2012/10: TRIMMING MAY NOW BE SPECIFIED EITHER IN TERMS OF
C              THE PROPORTION OR IN A SPECIFIC NUMBER OF OBSERVATIONS
C              TO BE TRIMMED.
C
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
          GOTO9000
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
          GOTO9000
        ENDIF
      ENDIF
C
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
          GOTO9000
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
          GOTO9000
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 2--                   **
C               **  COMPUTE THE TRIMMED SD.    **
C               *********************************
C
      CALL SORT(X,N,XTEMP)
C
      IF(IFLAG1.EQ.0)THEN
        NPROP1=(PROP1/100.0)*AN+0.0001
        ISTART=NPROP1+1
      ELSE
        NPROP1=NTRIM1
        ISTART=NPROP1+1
      ENDIF
C
      IF(IFLAG2.EQ.0)THEN
        NPROP2=(PROP2/100.0)*AN+0.0001
        ISTOP=N-NPROP2
      ELSE
        NPROP2=NTRIM2
        ISTOP=N-NPROP2
      ENDIF
C
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
        GOTO9000
      ENDIF
C
      K=0
      DO200I=ISTART,ISTOP
        K=K+1
        DX=XTEMP(I)
        DSUM=DSUM+DX
  200 CONTINUE
      NPROP3=K
      DK=K
      DMEAN=DSUM/DK
C
      DSUM=0.0D0
      DO300I=ISTART,ISTOP
        DX=XTEMP(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DK-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XTRIM=REAL(DSD)
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
        PROP3=100.00-PROP1-PROP2
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA WERE TRIMMED FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA WERE TRIMMED FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',
     1         'OF THE DATA REMAIN AFTER TRIMMING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRIM
  821   FORMAT('THE TRIMMED STANDARD DEVIATION OF THE ',I8,
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMSD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TRIMSD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PROP1,PROP2,PROP3
 9014   FORMAT('PROP1,PROP2,PROP3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NPROP1,NPROP2,NPROP3
 9015   FORMAT('NPROP1,NPROP2,NPROP3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISTART,ISTOP,DSUM,DK
 9016   FORMAT('ISTART,ISTOP,DSUM,DK = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IERROR,XTRIM
 9018   FORMAT('IERROR,XTRIM = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TRIMSE(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,
     1                  XTEMP,XTEMP2,IUPPER,XTRMSE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE
C              SAMPLE STANDARD ERROR OF THE TRIMMED MEAN
C              OF THE DATA IN THE INPUT VECTOR X.
C      NOTE--PROP1 % OF THE DATA IS TRIMSED FROM THE LEFT SIDE;
C            PROP2 % OF THE DATA IS TRIMSED FROM THE RIGHT SIDE.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMSED FROM LEFT SIDE.
C                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
C                                OF OBSERVATIONS TO BE TRIMSED FROM RIGHT SIDE.
C     OUTPUT ARGUMENTS--XTRMSE  = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED SAMPLE TRIMSED MEAN.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TRIMMED MEAN STANDARD ERROR.
C     OTHER DATAPAC   SUBROUTINES NEEDED--WINSOR.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--DAVID, ORDER STATISTICS, 1970, PAGES 129, 136.
C               --CROW AND SIDDIQUI, 'ROBUST ESTIMATION OF LOCATION',
C                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C                 1967, PAGES 357, 387.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY, 1969).
C               --RAND R. WILCOX, INTRODUCTION TO ROBUST ESTIMATION
C                 AND HYPOTHESIS TESTING, ACADEMIC PRESS, 1997.
C                 THE FORMULA FOR THE STANDARD ERROR IS TAKEN FROM
C                 PAGE 36-38 OF THIS SOURCE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--WE DO NOT NEED TO ACTUALLY COMPUTE THE TRIMMED MEAN
C           TO OBTAIN THE STANDARD ERROR.  THE STANDARD ERROR IS:
C               s(w)/[(1-2*LAMBDA)*SQRT(N)]
C           WHERE s(w) IS THE WINSORIZED STANDARD DEVIATION AND
C           LAMBDA IS THE AMOUNT OF TRIMMING (AS A FRACTION).
C           NOTE THAT WE USE PROP1 + PROP2 RATHER THAN LAMBDA.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2002.7
C     ORIGINAL VERSION--JULY      2002.
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
      DIMENSION XTEMP(*)
      DIMENSION XTEMP2(*)
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
      ISUBN1='TRIM'
      ISUBN2='ME  '
C
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.2 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)IUPPER
  115   FORMAT('      MUST BE BETWEEN 2 AND ',I8,' (INCLUSIVELY).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      HOLD=X(1)
      DO135I=2,N
      IF(X(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** WARNING IN TRIMMED MEAN STANDARD ERROR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,137)HOLD
  137 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      XTRMSE=0.0
      GOTO9000
  139 CONTINUE
C
      IF(PROP1.LT.0.0 .OR. PROP1.GT.100.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
  141   FORMAT('***** ERROR IN TRIMMED MEAN STANDARD ERROR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,142)
  142   FORMAT('      PROP1 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,143)PROP1
  143   FORMAT('      THE VALUE OF PROP1 IS ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(PROP2.LT.0.0 .OR. PROP2.GT.100.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      PROP2 SHOULD BE BETWEEN 0 AND 100, BUT IS NOT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)PROP2
  153   FORMAT('      THE VALUE OF PROP2 IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  STEP 2: WINSORIZE THE DATA   **
C               ***********************************
C
      NTRIM1=-1
      NTRIM2=-1
      CALL WINSOR(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,
     1            XTEMP,IUPPER,XTEMP2,
     1            IBUGA3,ISUBRO,IERROR)
      CALL SD(XTEMP2,N,IWRITE,WINVAR,IBUGA3,IERROR)
C
      ALAM=(PROP1 + PROP2)/100.0
      AN=REAL(N)
C
      XTRMSE=WINVAR/((1.0-ALAM)*SQRT(AN))
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
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,820)PROP1,PROP2
  820   FORMAT(F7.2,'% TRIMMED BELOW AND ',F7.2,'% TRIMMED ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRMSE
  821   FORMAT('THE STANDARD ERROR OF THE TRIMMED MEAN OF THE ',I8,
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMSE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TRIMSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,N
 9012   FORMAT('IBUGA3,IERROR = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PROP1,PROP2
 9014   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)WINVAR,XTRMSE
 9018   FORMAT('WINVAR,XTRMSE = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TRICDF(X,C,ALOWLM,AUPPLM,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TRIANGULAR DISTRIBUTION.
C              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
C              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
C              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
C              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME 
C              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
C              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION
C              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
C              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
C              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
C              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
C              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
C              AND FOR C = 0
C              F(X) = 1+X                    FOR -1 LE X LE 0
C              F(X) = 1-X                    FOR  0 LT X LE 1
C              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
C              THE DISTRIBUTION THAT RESULTS
C              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
C              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
C              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
C              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
C              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
C              SO AS TO BE SYMMETRIC ABOUT 0.)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --C      = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--EVANS, HASTINGS, PEACOCK, STATISTICAL DISTRIBUTIONS
C                 2ND ED.--CHAPTER 39.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER 1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      IF(C.LT.A.OR. C.GT.B)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
C
      IF(X.LE.A)THEN
        CDF=0.0
        GOTO9000
      ELSEIF(X.GE.B)THEN
        CDF=1.0
        GOTO9000
      ENDIF
C
   12 FORMAT(
     1'***** ERROR--THE SECOND ARGUMENT TO TRICDF IS OUTSIDE THE')
    3 FORMAT(
     1'      (',G15.7,',',G15.7,') INTERVAL.')
   22 FORMAT(
     1'***** ERROR--THE THIRD AND FOURTH INPUT ARGUMENTS TO THE ',
     1'TRICDF SUBROUTINE')
   23 FORMAT(
     1'      (THE LOWER AND UPPER LIMITS) ARE EQUAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LE.C)THEN
        CDF=(X-A)**2/((B-A)*(C-A))
      ELSE
        CDF=1.0 - (B-X)**2/((B-A)*(B-C))
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      DOUBLE PRECISION FUNCTION TRIFUN (Q,X)
C
C     PURPOSE--THIS ROUTINE IS USED IN FINDING THE ROOT OF THE
C              FOLLOWING EQUATION:
C
C                  G(q) = (m - a(q))/(b(q) - a(q))
C                       = (m - a(p))*(1 - SQRT((1-r)/(1-q)))/
C                         (b(r) - m)*(1 - SQRT(p/q)) +
C                         (m - a(p))*(1 - SQRT((1-r)/(1-q)))
C
C              THIS IS USED TO FIND ESTIMATES FOR THE LOWER/UPPER
C              BOUND PARAMETERS OF THE TRIANGULAR DISTRIBUTION
C              USING THE QUANTILE METHOD OF KOTZ AND VAN DORP.
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 28-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/5
C     ORIGINAL VERSION--MAY        2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION Q
      DOUBLE PRECISION X(*)
C
      DOUBLE PRECISION M
      DOUBLE PRECISION P
      DOUBLE PRECISION R
      DOUBLE PRECISION AP
      DOUBLE PRECISION BR
      COMMON/TRICOM/M,P,R,AP,BR
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
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
      DTERM1=DSQRT((1.0D0 - R)/(1.0D0 - Q))
      DTERM2=(M - AP)*(1.0D0 - DTERM1)
      DTERM3=(BR - M)*(1.0D0 - DSQRT(P/Q)) + (M - AP)*(1.0D0 - DTERM1)
C
      TRIFUN=DTERM2/DTERM3 - Q
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU2 (A,B,IR,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE M(A,B,R)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT SOLVES
C
C              Mhat(r) = PROD[i=1 to r-1][(Z(i)-a)/(Z(r)-a]*
C                        PROD[i=r+1 to n][(b - Z(i))/(b-Z(r))]
C
C              NOTE THAT THIS FUNCTION COMPUTES Mhat FOR A
C              SPECIFIC VALUE OF R AND IT ASSUMES THE DATA
C              IS ALREADY SORTED.
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER IR
      INTEGER N
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
      DOUBLE PRECISION DPROD2
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
C
      DPROD1=1.0D0
      DPROD2=1.0D0
C
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO1210J=1,IRM1
          DTERM1=X(J) - A
          DTERM2=X(IR) - A
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
C
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO1220J=IRM1,N
          DTERM1=B - X(J)
          DTERM2=B - X(IR)
          DPROD2=DPROD2*(DTERM1/DTERM2)
 1220   CONTINUE
      ENDIF
      TRIFU2=DPROD1*DPROD2
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU3(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE G(A,B)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G(A,B) = LOG{M(A,B,RHAT(A,B)} - N*LOG(B-A)
C
C              IT IS ASSUMED THAT THE DATA IS ALREADY SORTED.
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DG
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      DG=DLOG(DMAXMR) - DBLE(N)*DLOG(B-A)
      TRIFU3=DG
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU4(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO A)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G'(A,B)(A) = M'(A,B,RHAT(A,B))(A)/M(A,B,RHAT(A,B)
C                           + N/(B-A)
C
C              WHERE
C
C              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
C                   {SUM[j=1 to Rhat-1]
C                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DSUM1=0.0D0
C
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO1210J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(J) - A)
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
C
      DMHATP=DMAXMR*DSUM1
      TRIFU4=(DMHATP/DMAXMR) + DBLE(N)/DBLE(B-A)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU5(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO B)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G'(A,B)(B) = M'(A,B,RHAT(A,B))(B)/M(A,B,RHAT(A,B)
C                           - N/(B-A)
C
C              WHERE
C
C              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
C                   {SUM[j=RHAT+1 to N]
C                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DSUM1=0.0D0
C
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO1210J=IRM1,N
          DTERM1=X(J) - X(IR)
          DTERM2=(B - X(IR))*(B - X(J))
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
C
      DMHATP=DMAXMR*DSUM1
      TRIFU5=(DMHATP/DMAXMR) - DBLE(N)/(B-A)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU6(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE M(A,B,RHAT) (WITH RESPECT TO A)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
C                   {SUM[j=1 to Rhat-1]
C                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DPROD1=1.0D0
C
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO1210J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(I) - A)
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
C
      TRIFU6=DMAXMR*DPROD1
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TRIFU7(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE M(A,B,RHAT) (WITH RESPECT TO B)
C              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
C                   {SUM[j=RHAT+1 to N]
C                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DPROD1=1.0D0
C
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO1210J=IRM1,N
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(B - X(IR))*DBLE(B - X(I))
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
C
      TRIFU7=DMAXMR*DPROD1
C
      RETURN
      END
      SUBROUTINE TRIFU8(A,B,X,IR,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE FUNCTION
C
C              B(a) = Max{r = 1, ..., n-1}[f(b)(a,r)]
C
C              WHERE
C
C              f(b)a,r) = {X(r+1) - X(r)*
C                         (((X(r)-a)/(X(r+1)-a))**r)**(n-r)/
C                         {1 - (((X(r)-a)/(X(r+1)-a))**r)**(n-r)}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXFB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
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
C
      DMAXFB=0.0D0
      DO100I=1,N-1
        IR=I
        IRP1=IR+1
        DX1=X(IR)
        DX2=X(IRP1)
        DTERM1=(DX1-A)/(DX2-A)
        DTERM2=DTERM1**IR
        DTERM3=1.0D0/DBLE(N-IR)
        DTERM4=DTERM2**DTERM3
        DTEMP1=(DX2 - DX1*DTERM4)/(1.0D0 - DTERM4)
C
        IF(DTEMP1.GT.DMAXFB)THEN
          DMAXFB=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
      B=DMAXFB
C
      RETURN
      END
      SUBROUTINE TRIML1(Y,N,MAXNXT,
     1                  Z,XTEMP,DTEMP1,
     1                  XMIN,XMAX,XMEAN,XSD,
     1                  A,B,ALOWQN,AUPPQN,
     1                  AQUANT,BQUANT,
     1                  AML,BML,CML,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE MOMENT AND MAXIMUM LIKELIHOOD
C              ESTIMATES FOR THE TRIANGULAR DISTRIBUTION FOR THE RAW DATA
C              CASE (I.E., NO CENSORING AND NO GROUPING).
C
C              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
C              PERFORMED.
C
C              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
C              FROM MULTIPLE PLACES (DPMLUN WILL GENERATE THE OUTPUT
C              FOR THE TRIANGULAR MLE COMMAND).
C
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                CHAPTER 1.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/10
C     ORIGINAL VERSION--OCTOBER   2009. EXTRACTED AS A SEPARATE
C                                       SUBROUTINE (FROM DPMLUN)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IQUAME
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      INTEGER IFLAG
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION XTEMP(*)
C
      DOUBLE PRECISION DTEMP1(*)
C
      DOUBLE PRECISION TRIFUN
      DOUBLE PRECISION TRIFU2
      DOUBLE PRECISION TRIFU3
      DOUBLE PRECISION TRIFU4
      DOUBLE PRECISION TRIFU5
      DOUBLE PRECISION TRIFU6
      DOUBLE PRECISION TRIFU7
      EXTERNAL TRIFUN
      EXTERNAL TRIFU2
      EXTERNAL TRIFU3
      EXTERNAL TRIFU4
      EXTERNAL TRIFU5
      EXTERNAL TRIFU6
      EXTERNAL TRIFU7
C
      DOUBLE PRECISION DAE
      DOUBLE PRECISION DRE
      DOUBLE PRECISION DXSTRT
      DOUBLE PRECISION DXLOW
      DOUBLE PRECISION DXUP
C
      DOUBLE PRECISION DM
      DOUBLE PRECISION DP
      DOUBLE PRECISION DR
      DOUBLE PRECISION DAP
      DOUBLE PRECISION DBR
      COMMON/TRICOM/DM,DP,DR,DAP,DBR
C
      DOUBLE PRECISION DPROD1
      DOUBLE PRECISION DPROD2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DMAXMR
C
      DOUBLE PRECISION DA
      DOUBLE PRECISION DLOWBK
      DOUBLE PRECISION DUPPBK
      DOUBLE PRECISION DLOWAK
      DOUBLE PRECISION DUPPAK
      DOUBLE PRECISION DBK
      DOUBLE PRECISION DMK
      DOUBLE PRECISION DGK
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
      ISUBN1='TRIM'
      ISUBN2='L1  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TRIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  CARRY OUT CALCULATIONS              **
C               **  FOR TRIANGULAR MLE ESTIMATE         **
C               ******************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDIST='TRIANGULAR'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C     USE METHOD OF KOTZ AND VAN DORP (PP. 11-16) TO COMPUTE THE
C     MODE (WHICH IS THE MAXIMUM LIKELIHOOD ESTIMATE OF THE
C     THRESHOLD PARAMETER.
C
C     STEP 1: SORT THE DATA AND STANDARDIZE (Y = A + (B-A)*Y
C             WHERE A AND B ARE THE LOWER AND UPPER LIMITS).
C             USE THE DATA MINIMUM AND MAXIMUM AS THE INITIAL
C             ESTIMATES OF THESE PARAMETERS.
C
      CALL SORT(Y,N,Y)
      DO1110I=1,N
        Z(I)=XMIN + (XMAX-XMIN)*Y(I)
 1110 CONTINUE
C
C     STEP 2: COMPUTE
C
C             Mhat(r) = PROD[i=1 to r-1][(Z(i)-a)/(Z(r)-a]*
C                       PROD[i=r+1 to n][(b - Z(i))/(b-Z(r))]
C
      DMAXMR=0.0D0
      IINDX=0
      EPS=1.0E-05
      IF(A.LT.XMIN .AND. B.GT.XMAX)THEN
        CONTINUE
      ELSE
        A=XMIN - EPS
        B=XMAX + EPS
      ENDIF
C
      DO1200I=1,N
C
        DPROD1=1.0D0
        DPROD2=1.0D0
C
        IR=I
        IRM1=IR-1
        IF(IRM1.GE.1)THEN
          DO1210J=1,IRM1
            DTERM1=DBLE(Z(J) - A)
            DTERM2=DBLE(Z(IR) - A)
            DPROD1=DPROD1*(DTERM1/DTERM2)
 1210     CONTINUE
        ENDIF
C
        IRM1=IR+1
        IF(IRM1.LE.N)THEN
          DO1220J=IRM1,N
            DTERM1=DBLE(B - Z(J))
            DTERM2=DBLE(B - Z(IR))
            DPROD2=DPROD2*(DTERM1/DTERM2)
 1220     CONTINUE
        ENDIF
C
        DTERM3=DPROD1*DPROD2
        IF(DTERM3.GT.DMAXMR)THEN
          DMAXMR=DTERM3
          IINDX=IR
        ENDIF
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IML1')THEN
          WRITE(ICOUT,1240)I,DPROD1,DPROD2,DTERM3
 1240     FORMAT('I,DPROD1,DPROD2,DTERM3=',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 1200 CONTINUE
C
C     STEP 3: MAXIMUM LIKELIHOOD ESTIMATE IS THEN Z(IINDX)
C             WHERE IINDX IS THE ORDER STATISTIC RESULTING
C             IN THE MAXIMUM VALUE OF Mhat.
C
      CML=Y(IINDX)
C
C     NOW ESTIMATE A AND B USING THE QUANTILE METHOD DESCRIBED
C     ON PAGE 28-30 OF KOTZ AND VAN DORP.
C
      IQUAME='ORDE'
      IF(ALOWQN.GT.0.001 .AND. ALOWQN.LE.0.20)THEN
        P=ALOWQN
      ELSE
        P=0.05
      ENDIF
      CALL QUANT(P,Y,N,IWRITE,XTEMP,MAXNXT,
     1           IQUAME,
     1           AP,IBUGA3,IERROR)
C
      IF(AUPPQN.GT.0.80 .AND. AUPPQN.LE.0.999)THEN
        R=AUPPQN
      ELSE
        R=0.95
      ENDIF
      CALL QUANT(R,Y,N,IWRITE,XTEMP,MAXNXT,
     1           IQUAME,
     1           BR,IBUGA3,IERROR)
C
      DO1301I=1,N
        DTEMP1(I)=DBLE(Z(I))
 1301 CONTINUE
C
      DM=DBLE(CML)
      DP=DBLE(P)
      DAP=DBLE(AP)
      DR=DBLE(R)
      DBR=DBLE(BR)
      DAE=1.0D-08
      DRE=DAE
      DXLOW=DP
      DXUP=DR
      DXSTRT=(DXLOW+DXUP)/2.0D0
      IFLAG=0
      CALL DFZER2(TRIFUN,DXLOW,DXUP,DXSTRT,DRE,DAE,IFLAG,DTEMP1)
C
      IF(IFLAG.EQ.2)THEN
C
C  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2111)
C2111   FORMAT('***** WARNING FROM TRIANGULAR MAXIMUM ',
CCCCC1         'LIKELIHOOD--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,2113)
C2113   FORMAT('      ESTIMATE OF SIGMA MAY NOT BE COMPUTED TO ',
CCCCC1         'DESIRED TOLERANCE.')
CCCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2121)
 2121   FORMAT('***** WARNING FROM TRIANGULAR MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2123)
 2123   FORMAT('      ESTIMATE OF A MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2131)
 2131   FORMAT('***** ERROR FROM TRIANGULAR MAXIMUM LIKELIHOOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2133)
 2133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2121)IDIST
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2143)
 2143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DQ=DXLOW
      DTERM1=DAP - DM*DSQRT(DP/DQ)
      DTERM2=1.0D0 - DSQRT(DP/DQ)
      AML=REAL(DTERM1/DTERM2)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,2241)DQ,DP,DAP
 2241   FORMAT('DQ,DP,DAP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2243)DM,DR,DBR
 2243   FORMAT('DN,DR,DBR = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2245)DTERM1,DTERM2,XMIN,AML
 2245   FORMAT('DTERM1,DTERM2,XMIN,AML = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DTERM1=DBR - DM*DSQRT((1.0D0-DR)/(1.0D0-DQ))
      DTERM2=1.0D0 - DSQRT((1.0D0-DR)/(1.0D0-DQ))
      BML=REAL(DTERM1/DTERM2)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,2247)DTERM1,DTERM2,XMAX,BML
 2247   FORMAT('DTERM1,DTERM2,XMAX,BML = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      AQUANT=AML
      BQUANT=BML
C
C     IMPLEMENT ESTIMATES FOR A AND B USING ML METHOD
C     DESCRIBED IN KOTZ AND VAN DORP.
C
C     STEP 1: IMPLEMENT BSEARCH ON PAGE 26
C
      DO2301I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2301 CONTINUE
C
      ITER=0
      MAXIT=100
C
      DA=DBLE(XMIN) - DBLE((XMAX - XMIN)*0.1)
      DLOWBK=DTEMP1(N)
      CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2310 CONTINUE
      ITER=ITER+1
      DBK=(DLOWBK + DUPPBK)/2.0D0
      DGK=TRIFU5(DA,DBK,DTEMP1,N)
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER.GT.MAXIT)GOTO2319
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GOTO2310
      ELSE
        GOTO2319
      ENDIF
C
 2319 CONTINUE
      BML=REAL(DBK)
C
C     STEP 2: IMPLEMENT ABSEARCH ON PAGE 27
C
      ITER=0
      ITER2=0
C
C     STEP 2A: LOWER/UPPER BOUNDS FOR A
C
      DUPPAK=DTEMP1(1)
      DLOWAK=DTEMP1(1) - (DTEMP1(N) - DTEMP1(1))
C
C     STEP 2B: BSEARCH USING LOWER BOUND FOR A
C
 2360 CONTINUE
      ITER2=ITER2+1
      DA=DLOWAK
      DLOWBK=DTEMP1(N)
      CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2361 CONTINUE
      ITER=ITER+1
      DBK=(DLOWBK + DUPPBK)/2.0D0
      DMK=TRIFU2(DA,DBK,IR,DTEMP1,N)
      DGK=TRIFU5(DA,DBK,DTEMP1,N)
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER.GT.MAXIT)GOTO2369
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GOTO2361
      ELSE
        GOTO2369
      ENDIF
C
 2369 CONTINUE
C
C     STEP 2C: COMPUTE G'(A,B)(A)
C
      DGK=TRIFU4(DA,DBK,DTEMP1,N)
      IF(DGK.LT.0.0)THEN
        DUPPAK=DLOWAK
        DLOWAK=DLOWAK - (DTEMP1(N) - DTEMP1(1))
        GOTO2360
      ENDIF
C
C     STEP 2D: BSEARCH WITH NEW VALUE OF A
C
 2380 CONTINUE
      ITER=0
      DA=(DLOWAK + DUPPAK)/2.0D0
      DLOWBK=DTEMP1(N)
      CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2381 CONTINUE
      ITER=ITER+1
      DBK=(DLOWBK + DUPPBK)/2.0D0
      DMK=TRIFU2(DA,DBK,IR,DTEMP1,N)
      DGK=TRIFU5(DA,DBK,DTEMP1,N)
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER.GT.MAXIT)GOTO2389
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GOTO2381
      ELSE
        GOTO2389
      ENDIF
C
 2389 CONTINUE
      DGK=TRIFU4(DA,DBK,DTEMP1,N)
      ITER2=ITER2+1
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER2.GT.MAXIT)GOTO2399
        IF(DGK.LT.0.0D0)THEN
          DUPPAK=DA
        ELSE
          DLOWAK=DA
        ENDIF
        IF((DUPPAK - DLOWAK).LT.1.0D-6)GOTO2399
        GOTO2380
      ELSE
        GOTO2399
      ENDIF
C
 2399 CONTINUE
      AML=REAL(DA)
      BML=REAL(DBK)
C
C     NOW UPDATE THE ESTIMATE OF C USING THE
C     FINAL PARAMETER ESTIMATES FOR A AND B.
C
      DMAXMR=0.0D0
      DO2410I=1,N
        IR=I
        DTERM1=TRIFU2(DA,DBK,IR,DTEMP1,N)
C
        IF(DTERM1.GT.DMAXMR)THEN
          DMAXMR=DTERM1
          IINDX=IR
        ENDIF
C
 2410 CONTINUE
C
      CML=Y(IINDX)
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF TRIML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XSD,XMIN,XMAX
 9055   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9056)AQUANT,BQUANT
 9056   FORMAT('AQUANT,BQUANT = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9057)AML,BML,CML
 9057   FORMAT('AML,BML,CML = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TRIPDF(X,C,ALOWLM,AUPPLM,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TRIANGULAR DISTRIBUTION.
C              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
C              AND STANDARD DEVIATION = SQRT(1/6) = 0.408248
C              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
C              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
C              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME 
C              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
C              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION
C              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
C              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
C              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
C              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
C              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
C              AND FOR C = 0
C              F(X) = 1+X                    FOR -1 LE X LE 0
C              F(X) = 1-X                    FOR  0 LT X LE 1
C              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
C              THE DISTRIBUTION THAT RESULTS
C              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
C              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
C              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
C              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
C              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
C              SO AS TO BE SYMMETRIC ABOUT 0.)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --C      = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--EVANS, HASTINGS, PEACOCK, STATISTICAL DISTRIBUTIONS
C                 2ND ED.--CHAPTER 39.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER 1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
      PDF=0.0
C
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(C.LT.A .OR. C.GT.B)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    2 FORMAT(
     1'***** ERROR--THE FIRST ARGUMENT TO TRIPDF IS OUTSIDE THE')
    3 FORMAT(
     1'      (',G15.7,',',G15.7,') INTERVAL.')
   12 FORMAT(
     1'***** ERROR--THE SECOND ARGUMENT TO TRIPDF IS OUTSIDE THE')
   22 FORMAT(
     1'***** ERROR--THE THIRD AND FOURTH ARGUMENTS TO TRIPDF (THE')
   23 FORMAT(
     1'      LOWER AND UPPER LIMITS) ARE EQUAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(C.EQ.A)THEN
        PDF=2.0*(B-X)/((B-A)*(B-C))
      ELSEIF(C.EQ.B)THEN
        PDF=2.0*(X-A)/((B-A)*(C-A))
      ELSE
        IF(X.LE.C)THEN
          PDF=2.0*(X-A)/((B-A)*(C-A))
        ELSE
          PDF=2.0*(B-X)/((B-A)*(B-C))
        ENDIF
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE TRIPPF(P,C,ALOWLM,AUPPLM,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TRIANGULAR
C              DISTRIBUTION ON THE INTERVAL (-1,1).
C              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
C              AND STANDARD DEVIATION = SQRT(1/6) = 0.408248
C              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
C              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
C              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME 
C              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
C              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION
C              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
C              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
C              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
C              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
C              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
C              AND FOR C = 0
C              F(X) = 1+X                    FOR -1 LE X LE 0
C              F(X) = 1-X                    FOR  0 LT X LE 1
C              (A TRIANGLE).
C              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
C              THE DISTRIBUTION THAT RESULTS
C              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
C              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
C              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
C              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
C              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
C              SO AS TO BE SYMMETRIC ABOUT 0.)
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--APRIL     1978.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 1994.  ACCOMODATE C PARAMETER.
C     UPDATED         --JANUARY   1995.  FIX FOR C <> 0.
C     UPDATED         --JANUARY   1995.  TEST FOR C OUT OF RANGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
      PPF=0.0
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT(
     1  '***** ERROR--THE FIRST ARGUMENT TO TRIPPF IS OUTSIDE THE ',
     1  '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(C.LT.A .OR. C.GT.B)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TRIPPF IS ',
     1         'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)A,B
   13   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
   22   FORMAT('***** ERROR--THE THIRD AND FOURTH ARGUMENTS TO TRIPPF ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
   23   FORMAT('      (THE LOWER AND UPPER LIMITS) ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      IF(P.EQ.0)THEN
        PPF=A
      ELSEIF(P.EQ.1.0)THEN
        PPF=B
      ELSE
        CALL TRICDF(C,C,ALOWLM,AUPPLM,PCUT)
        IF(P.LE.PCUT)THEN
          C1=(B-A)*(C-A)
          PPF=A + SQRT(P*C1)
        ELSE
          C2=(B-A)*(B-C)
          PPF=B - SQRT((1.0-P)*C2)
        ENDIF
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TRIRAN(N,C,ZLOWLM,ZUPPLM,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TRIANGULAR DISTRIBUTION
C              WITH MEAN = 0 AND STANDARD DEVIATION = ZZ.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = ZZZ
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TRIANGULAR DISTRIBUTION
C             WITH MEAN = 0 AND STANDARD DEVIATION = ZZZ
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TRIPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 14-15.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 36.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGE 230.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES ZZZ.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--JUNE      1978.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 1994.  FIX BUG
C     UPDATED         --SEPTEMBER 2001.  SUPPORT FOR C SHAPE PARAMETER
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TRIRAN IS ',
     1       'NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TRIANGULAR RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
CCCCC SEPTEMBER 1994.  FIX FOLLOWING LOOP.
CCCCC C=0.0
      A=MIN(ZLOWLM,ZUPPLM)
      B=MAX(ZLOWLM,ZUPPLM)
c
      IF(C.LE.A .OR. C .GE.B)THEN
        WRITE(ICOUT,210)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)C
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
  210 FORMAT('*****ERROR--THE SHAPE PARAMETER IS OUTSIDE THE ',
     1'ALLOWABLE (',G15.7,',',G15.7,') INTERVAL.')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DO100I=1,N
CCCCC   CALL TRIPPF(X(I),X(I))
        P=X(I)
        CALL TRIPPF(P,C,A,B,PPF)
        X(I)=PPF
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TRUNEG(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
C              TRUE NEGATIVES BETEEN TWO VARIABLES.
C
C              THIS IS SPECIFICALLY FOR THE 2X2 CASE.  THAT IS,
C              EACH VARIABLE HAS TWO MUTUALLY EXCLUSIVE
C              CHOICES CODED AS 1 (FOR SUCCESS) OR 0 (FOR
C              FAILURE).  A TRUE NEGATIVE IS DEFINED AS THE
C              CASE WHERE THE SECOND VARIABLE IS 0 AND THE FIRST
C              VARIABLE IS A 0.
C
C              A TYPICAL EXAMPLE WOULD BE WHERE VARIABLE ONE
C              DENOTES THE GROUND TRUTH AND A VALUE OF 1
C              INDICATES "PRESENT" AND A VALUE OF 0 INDICATES
C              "NOT PRESENT".  VARIABLE TWO REPRESENTS SOME TYPE
C              OF DETECTION DEVICE WHERE A VALUE OF 1 INDICATES
C              THE DEVICE DETECTED THE SPECIFIED OBJECT WHILE A
C              VALUE OF 0 INDICATES THAT THE OBJECT WAS NOT
C              DETECTED.  A TRUE NEGATIVE THEN IS THE CASE WHERE
C              THE DEVICE FAILED TO DETECT THE OBJECT WHEN IT WAS
C              NOT ACTUALY THERE.
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
C                                COMPUTED TRUE NEGATIVE PROPORTION
C                                BETWEEN THE 2 SETS OF DATA
C                                IN THE INPUT VECTORS X AND Y.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             SAMPLE TRUE NEGATIVE PROPORTION BETWEEN THE 2 SETS
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
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
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
      ISUBN1='TRUN'
      ISUBN2='EG  '
C
      IERROR='NO'
C
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRUNEG--')
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
 1201   FORMAT('***** ERROR IN THE TRUE NEGATIVE PROPORTION')
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
        STAT=REAL(N22)/REAL(NTEMP)
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
        STAT=0.0
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
        STAT=0.0
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
      STAT=REAL(N22)/REAL(N)
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
  811 FORMAT('THE TRUE NEGATIVE PROPORTION = ',G15.7)
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
 9011   FORMAT('***** AT THE END OF TRUNEG--')
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
      SUBROUTINE TRUPOS(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
C              TRUE POSITIVES BETWEEN TWO VARIABLES.
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
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
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
   51   FORMAT('***** AT THE BEGINNING OF TRUPOS--')
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
 1201   FORMAT('***** ERROR IN THE TRUE POSITIVE PROPORTION')
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
        STAT=REAL(N11)/REAL(NTEMP)
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
      STAT=REAL(N11)/REAL(N)
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
  811 FORMAT('THE TRUE POSITIVE PROPORTION = ',G15.7)
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
 9011   FORMAT('***** AT THE END OF TRUPOS--')
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
      SUBROUTINE TSOCDF(DX,DN,DTHETA,ALOWLM,AUPPLM,DCDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION.
C              THE CUMULATIVE DISTRIBUTION FUNCTION IS COMPUTED USING
C              THE FOLLOWING RELATION TO THE OGIVE DISTRIBUTION:
C
C              F(X;N,THETA) = THETA*G(X/THETA;N)                  0 < X < THETA
C                           = 1 - (1-THETA)*G((1-X)/(1-THETA);N)  THETA <= X < 1
C
C              WHERE F IS THE CUMULATIVE DISTRIBUTION OF THE TWO-SIDED
C              OGIVE DISTRIBUTION AND G IS THE CUMULATIVE DISTRIBUTION
C              OF THE OGIVE DISTRIBUTION.  THIS RELATION IS GIVEN ON
C              PAGE 230 OF KOTZ AND VAN DORP.
C
C              N DENOTES THE SHAPE PARAMETER AND THETA DENOTES THE
C              THRESHOLD PARAMETER.
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --DN     = THE DOUBLE PRECISION FIRST SHAPE
C                                PARAMETER
C                     --DTHETA = THE DOUBLE PRECISION SECOND SHAPE
C                                PARAMETER
C                     --ALOWLM = THE DOUBLE PRECISION LOWER LIMIT
C                                PARAMETER
C                     --AUPPLM = THE DOUBLE PRECISION UPPER LIMIT
C                                PARAMETER
C     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE TWO-SIDED OGIVE
C             DISTRIBUTION WITH SHAPE PARAMETERS N AND THETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--OGICDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION ALOWLM
      DOUBLE PRECISION AUPPLM
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION DTERM1
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
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      DCDF=0.0D0
C
      IF(DX.LE.A)THEN
        DCDF=0.0D0
        GOTO9000
      ELSEIF(DX.GE.B)THEN
        DCDF=1.0D0
        GOTO9000
      ELSEIF(DN.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DN
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GOTO9000
   12   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSOCDF IS ',
     1         'LESS THAN 0.5')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
   51   FORMAT('***** ERROR IN TSOCDF--LOWER LIMIT GREATER THAN OR ',
     1         'EQUAL TO UPPER LIMIT')
   52   FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
   53   FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
      ELSEIF(DTHETA.LT.A .OR. DTHETA.GT.B)THEN
        WRITE(ICOUT,61)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)DTHETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   61 FORMAT('***** ERROR IN TSOCDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
   63 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
C               ************************************
C               **  STEP 1--                      **
C               **  COMPUTE THE CDF     FUNCTION  **
C               ************************************
C
      DLOC=A
      DSCALE=B-A
      DX=(DX-DLOC)/DSCALE
      DTHETA=(DTHETA-DLOC)/DSCALE
      IF(DX.LT.DTHETA)THEN
        DX=DX/DTHETA
        CALL OGICDF(DX,DN,DCDF)
        DCDF=DTHETA*DCDF
      ELSE
        DX=(1.0D0 - DX)/(1.0D0 - DTHETA)
        CALL OGICDF(DX,DN,DCDF)
        DCDF=1.0D0 - (1.0D0 - DTHETA)*DCDF
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION TSOFUN(DX)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION
C              WITH SHAPE PARAMETERS N AND THETA.
C
C              BUT DEFINE AS FUNCTION TO BE USED FOR INTEGRATION
C              CODE CALLED BY OGICDF.  ALSO, THIS ROUTINE USES
C              DOUBLE PRECISION ARITHMETIC.
C     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--TSOFUN  = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE TWO-SIDED OGIVE DISTRIBUTION
C             WITH SHAPE PARAMETERS N AND THETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--TSOPDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TSOCOM/DN,DTHETA,DA,DB
C
      DOUBLE PRECISION DPDF
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
C-----START POINT-----------------------------------------------------
C
C               ************************************
C               **  STEP 1--                      **
C               **  COMPUTE THE DENSITY FUNCTION  **
C               ************************************
C
      CALL TSOPDF(DX,DN,DTHETA,DA,DB,DPDF)
      TSOFUN=DPDF
C
 9000 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION TSOFU2(DX)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION
C              WITH SHAPE PARAMETERS N AND THETA.
C
C              BUT DEFINE AS FUNCTION TO BE USED FOR INTEGRATION
C              CODE CALLED BY TSOPPF.  ALSO, THIS ROUTINE USES
C              DOUBLE PRECISION ARITHMETIC.
C     INPUT  ARGUMENTS--DX      = THE DOUBLE PRECISION VALUE AT
C                                 WHICH THE PROBABILITY DENSITY
C                                 FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--TSOFU2  = THE DOUBLE PRECISION CUMULATIVE
C                                 DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE TWO-SIDED OGIVE DISTRIBUTION
C             WITH SHAPE PARAMETERS N AND THETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--TSOCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DX
C
      DOUBLE PRECISION DP
      COMMON/TS2COM/DP
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TS3COM/DN,DTHETA,DA,DB
C
      DOUBLE PRECISION DTHETS
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
C-----START POINT-----------------------------------------------------
C
C               ************************************
C               **  STEP 1--                      **
C               **  COMPUTE THE DENSITY FUNCTION  **
C               ************************************
C
      DTHETS=DTHETA
      CALL TSOCDF(DX,DN,DTHETA,DA,DB,DCDF)
      TSOFU2=DP - DCDF
      DTHETA=DTHETS
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TSOPDF(X,N,THETA,ALOWLM,AUPPLM,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION.
C              THE PROBABILITY DENSITY FUNCTION FOR THE STANDARD
C              TWO-SIDED OGIVE DISTRIBUTION IS:
C
C                  f(X;N,THETA) =
C
C                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
C                      0 <= X <= THETA, N >= 0.5
C
C                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
C                      THETA < X <= 1, N >= 0.5
C
C              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING THE
C              THRESHOLD PARAMETER.
C
C              FOR THE GENERAL CASE, DO THE FOLLOWING:
C
C              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
C              2) LOCATION = A
C                 SCALE    = (B-A)
C              3) USE THE RELATION:
C
C                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --N      = THE DOUBLE PRECISION SHAPE PARAMETER
C                     --THETA  = THE DOUBLE PRECISION THRESHOLD PARAMETER
C                     --ALOWLM = THE DOUBLE PRECISION LOWER LIMIT PARAMETER
C                     --AUPPLM = THE DOUBLE PRECISION UPPER LIMIT PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN ALOWLM AND AUPPLM, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--OCTOBER     2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION N
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      REAL CPUMIN
      REAL CPUMAX
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      PDF=0.0
C
      IF((N.GT.0.5D0 .AND. N.LT.1.0D0) .AND.
     1    X.LE.0.0D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
    2   FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO TSOPDF IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
      ELSEIF(N.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
   12   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSOPDF IS ',
     1         'LESS THAN 0.5')
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
  101   FORMAT('***** ERROR IN TSOPDF--LOWER LIMIT GREATER THAN OR ',
     1         'EQUAL TO UPPER LIMIT')
  102   FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103   FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
      ELSEIF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  111 FORMAT('***** ERROR IN TSOPDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      THETSV=THETA
      THETA=(THETA-A)/(B-A)
      ALOC=A
      SCALE=(B-A)
      DX=(X-ALOC)/SCALE
C
      IF(DX.LE.A .AND. N.EQ.0.5D0)THEN
        PDF=1.0D0/(B-A)
      ELSEIF(DX.LE.A .AND. N.EQ.1.0D0)THEN
        PDF=1.0D0/(B-A)
      ELSEIF(DX.LE.THETA)THEN
        TERM1=N*(DX/THETA)**(N-1.0D0)
        TERM2=(4.0D0*N-2.0D0)/(3.0D0*N-1.0D0)
        TERM3=((2.0D0*N-2.0D0)/(3.0D0*N-1.0D0))
        TERM4=(DX/THETA)**N
        PDF=TERM1*(TERM2 - TERM3*TERM4)
        PDF=PDF/SCALE
      ELSE
        TERM1=N*((1.0D0-DX)/(1.0D0-THETA))**(N-1.0D0)
        TERM2=(4.0D0*N-2.0D0)/(3.0D0*N-1.0D0)
        TERM3=((2.0D0*N-2.0D0)/(3.0D0*N-1.0D0))
        TERM4=((1.0D0-DX)/(1.0D0-THETA))**N
        PDF=TERM1*(TERM2 - TERM3*TERM4)
        PDF=PDF/SCALE
      ENDIF
      THETA=THETSV
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TSOPPF(P,N,THETA,ALOWLM,AUPPLM,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION WITH
C              WITH SHAPE PARAMETERS N AND THETA.
C
C                  f(X;N,THETA) =
C
C                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
C                      0 <= X <= THETA, N >= 0.5
C
C                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
C                      THETA < X <= 1, N >= 0.5
C
C              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING
C              THE THRESHOLD PARAMETER.
C
C              FOR THE GENERAL CASE, DO THE FOLLOWING:
C
C              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
C              2) LOCATION = A
C                 SCALE    = (B-A)
C              3) USE THE RELATION:
C
C                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
C
C              THE PERCENT POINT FUNCTION IS COMPUTED BY NUMERICALLY
C              INVERTING THE TWO-SIDED OGIVE CUMULATIVE
C              DISTRIBUTION FUNCTION (WHICH IN TURN IS COMPUTED BY
C              NUMERICAL INTEGRATION OF THE PROBABILITYT DENSITY.
C
C     INPUT  ARGUMENTS--P       = THE DOUBLE PRECISION VALUE AT
C                                 WHICH THE PERCENT POINT
C                                 FUNCTION IS TO BE EVALUATED.
C                                 0 <= P <= 1
C                     --N       = THE DOUBLE PRECISION SHAPE PARAMETER
C                     --THETA   = THE DOUBLE PRECISION THRESHOLD
C                                 PARAMETER
C                     --ALOWLM  = THE DOUBLE PRECISION LOWER LIMIT
C                                 PARAMETER
C                     --AUPPLM  = THE DOUBLE PRECISION UPPER LIMIT
C                                 PARAMETER
C     OUTPUT ARGUMENTS--PPF     = THE DOUBLE PRECISION PERCENT POINT
C                                 FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
C             VALUE PPF FOR THE TWO-SIDED OGIVE
C             DISTRIBUTION WITH SHAPE PARAMETERS N AND THETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DFZERO.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OVTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION N
C
      EXTERNAL TSOFU2
C
      DOUBLE PRECISION DP
      COMMON/TS2COM/DP
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TS3COM/DN,DTHETA,DA,DB
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      REAL CPUMIN
      REAL CPUMAX
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C               *****************************************
C               **  STEP 1--                           **
C               **  CHECK FOR VALID PARAMETERS         **
C               *****************************************
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
CCCCC THETSV=THETA
CCCCC THETA=(THETA-A)/(B-A)
C
      PPF=0.0D0
C
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,48)P
        CALL DPWRST('XXX','WRIT')
        PPF=0.0D0
        GOTO9000
    4   FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO TSOPPF IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
C
      ELSEIF(N.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)N
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
   12   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO OGIPDF IS ',
     1         'LESS THAN 0.5')
   48   FORMAT('      VALUE OF ARGUMENT IS: ',G15.7)
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
   51   FORMAT('***** ERROR IN TSOPPF--LOWER LIMIT GREATER THAN OR ',
     1         'EQUAL TO UPPER LIMIT')
   52   FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
   53   FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
      ELSEIF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,61)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   61 FORMAT('***** ERROR IN TSOPPF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
   63 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
C     N = 0.5 AND N = 1.0 REDUCE TO THE UNIFORM DISTRIBUTION.
C
      IF(N.EQ.0.5D0 .OR. N.EQ.1.0D0)THEN
        PPF=A + (B-A)*P
        GOTO9000
      ELSEIF(P.LE.0.0D0)THEN
        PPF=A
        GOTO9000
      ELSEIF(P.GE.1.0D0)THEN
        PPF=B
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  COMPUTE THE PERCENT POINT FUNCTION.**
C               *****************************************
C
  300 CONTINUE
      XLOW=A
      XUP=B
      XUP2=(A+B)/2.0D0
      AE=1.D-9
      RE=1.D-9
      DN=N
      DTHETA=THETA
      DA=A
      DB=B
      DP=P
      CALL DFZERO(TSOFU2,XLOW,XUP,XUP2,RE,AE,IFLAG)
C
      PPF=XLOW
C
      IF(IFLAG.EQ.2)THEN
C
C  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
CCCCC   WRITE(ICOUT,999)
  999   FORMAT(1X)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,111)
CC111   FORMAT('***** WARNING FROM TSOPPF--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,113)
CC113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
CCCCC1         'TOLERANCE.')
CCCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING FROM TSOPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      PPF VALUE MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR FROM TSOPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
  141   FORMAT('***** WARNING FROM TSOPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,143)
  143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 9000 CONTINUE
      THETA=THETSV
      RETURN
      END
      SUBROUTINE TSORAN(N,AN,THETA,A,B,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TWO-SIDED OGIVE DISTRIBUTION WITH
C              SHAPE PARAMETERS N AND THETA.
C
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C                  f(X;N,THETA) =
C
C                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
C                      0 <= X <= THETA, N >= 0.5
C
C                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
C                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
C                      THETA < X <= 1, N >= 0.5
C
C              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING THE
C              THRESHOLD PARAMETER.
C
C              FOR THE GENERAL CASE, DO THE FOLLOWING:
C
C              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
C              2) LOCATION = A
C                 SCALE    = (B-A)
C              3) USE THE RELATION:
C
C                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
C
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --AN     = THE SINGLE PRECISION VALUE OF THE
C                                SHAPE PARAMETER AN.
C                     --THETA  = THE SINGLE PRECISION THRESHOLD
C                                PARAMETER
C                     --ALOWLM = THE SINGLE PRECISION LOWER LIMIT
C                                PARAMETER
C                     --AUPPLM = THE SINGLE PRECISION UPPER LIMIT
C                                PARAMETER
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TWO-SIDED OGIVE DISTRIBUTION
C             WITH SHAPE PARAMETER N AND THETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TSOPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND AN: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHMOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DOUBLE PRECISION DTEMP
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',
     1         'TWO-SIDED OGIVE RANDOM NUMBERS IS NON-POSITIVE')
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      ELSEIF(AN.LT.0.5)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)AN
        CALL DPWRST('XXX','BUG ')
        GOTO9000
  201   FORMAT('***** ERROR--THE N SHAPE PARAMETER IS ',
     1         'LESS THAN 0.5')
  203   FORMAT('      THE VALUE OF N IS ',G15.7)
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,100)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
  100   FORMAT('***** ERROR IN TWO-SIDED OGIVE RANDOM NUMBERS--')
  101   FORMAT('      LOWER LIMIT GREATER THAN OR EQUAL TO ',
     1         'UPPER LIMIT')
  102   FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103   FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
      ELSEIF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,100)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
  111   FORMAT('      THETA IS OUTSIDE THE LOWER AND UPPER LIMITS')
  113   FORMAT('      THE VALUE OF THETA IS ',G15.7)
      ENDIF
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TWO-SIDED OGIVE DISTRIBUTION RANDOM
C     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO300I=1,N
        CALL TSOPPF(DBLE(X(I)),DBLE(AN),DBLE(THETA),
     1              DBLE(A),DBLE(B),DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TSPCDF(X,THETA,AN,A,B,CDF)
C
C     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
C           CDF IS:
C              TSPCDF(X,THETA,N)
C                  = THETA*(X/THETA)**N          0 <= X <= THETA
C                  = 1 - (1-THETA)*((1-X)/(1-THETA))**N
C                                                THETA <= X <= 1
C                    N > 0, 0 <= THETA <= 1
C     NOTE--GENERAL TWO-SIDED POWER DISTRIBUTION CDF IS:
C              F(X;THETA,N,A,B)
C                  = ((A-THETA)/(B-A))*((X-A)/(THETA-A))**N 
C                                             A <= X <= THETA
C                  = 1 - ((THETA-B)/(B-A))*((B-X)/(B-THETA))**N 
C                                             THETA <= X <= B
C     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
C                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
C                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
C                      INSTEAD OF FROM THE CALLING ROUTINE.
C     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
C                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
C                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
C                 AMERICAN STATISTICIAN, VOLUME 56,
C                 NUMBER 2, MAY, 2002.
C               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/5
C     ORIGINAL VERSION--MAY       2002.
C     UPDATED         --JUNE      2007. ADD A AND B TO CALL LIST
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM3
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
      CDF=0.0
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPCDF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPCDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPCDF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
C
      IF(X.LT.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  131 FORMAT('***** WARNING IN TSPCDF--THE FIRST INPUT ARGUMENT IS ',
     1       'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
C
      IF(X.GT.B)THEN
        CDF=1.0
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  141 FORMAT('***** WARNING IN TSPCDF--THE FIRST INPUT ARGUMENT IS ',
     1       'GREATER THAN THE UPPER LIMIT')
C
      IF(X.LE.A)THEN
        CDF=0.0
      ELSEIF(X.GE.B)THEN
        CDF=1.0
      ELSE
C
        DX=DBLE(X)
        DTHETA=DBLE(THETA)
        DAN=DBLE(AN)
        DA=DBLE(A)
        DB=DBLE(B)
C
        IF(DX.LE.DTHETA)THEN
          DTERM1=DLOG(DTHETA-DA) - DLOG(DB-DA)
          DTERM3=DAN*(DLOG(DX-DA) - DLOG(DTHETA-DA))
          DCDF=DEXP(DTERM1 + DTERM3)
        ELSE
          DTERM1=DLOG(DB-DTHETA) - DLOG(DB-DA)
          DTERM3=DAN*(DLOG(DB-DX) - DLOG(DB-DTHETA))
          DCDF=1.0D0 - DEXP(DTERM1 + DTERM3)
        ENDIF
        CDF=REAL(DCDF)
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION TSPFU3(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE G(A,B)
C              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G(A,B) = N*{LOG{NUHAT(A,B)/(B-A) + 1/(NUHAT(A,B) - 1}
C
C              WHERE
C
C              NUHAT(A,B) = MAX{-N/LOG(MHAT(A,B,RHAT(A,B))),1)
C
C              IT IS ASSUMED THAT THE DATA IS ALREADY SORTED.
C     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 100-104.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/6
C     ORIGINAL VERSION--JUNE       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DG
      DOUBLE PRECISION NUHAT
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      NUHAT=-DBLE(N)/DLOG(DMAXMR)
      NUHAT=MAX(NUHAT,1.0D0)
      TSPFU3=DBLE(N)*(DLOG(NUHAT/(B-A)) + (1.0D0/NUHAT) - 1.0D0)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TSPFU4(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO A)
C              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G'(A,B)(A) = -{M'(A,B,RHAT(A,B))(A)/M(A,B,RHAT(A,B)}*
C                           {N/LOG(M(A,B,RHAT(A,B))) + 1}
C                           + N/(B-A)
C
C              WHERE
C
C              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
C                   {SUM[j=1 to Rhat-1]
C                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
C
C     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/7
C     ORIGINAL VERSION--JULY       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DSUM1=0.0D0
C
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO1210J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(J) - A)
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
      DMHATP=DMAXMR*DSUM1
C
      TSPFU4=(-DMHATP/DMAXMR)*((DBLE(N)/DLOG(DMAXMR)) + 1.0D0)
     1       + DBLE(N)/DBLE(B-A)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION TSPFU5(A,B,X,N)
C
C     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
C              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO B)
C              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
C              SPECIFICALLY, IT COMPUTES
C
C              G'(A,B)(B) = -{M'(A,B,RHAT(A,B))(B)/M(A,B,RHAT(A,B)}*
C                           {N/LOG(M(A,B,RHAT(A,B))) + 1}
C                           - N/(B-A)
C
C              WHERE
C
C              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
C                   {SUM[j=RHAT+1 to N]
C                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
C
C     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
C     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                PP. 16-30.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBUG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/7
C     ORIGINAL VERSION--JULY       2007.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
C
C---------------------------------------------------------------------
C
      INTEGER IR
      INTEGER IINDX
C
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
C
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
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
C
      DMAXMR=0.0D0
      DO100I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
C
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
C
  100 CONTINUE
C
      IR=IINDX
C
      DSUM1=0.0D0
C
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO1210J=IRM1,N
          DTERM1=X(J) - X(IR)
          DTERM2=(B - X(IR))*(B - X(J))
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
      DMHATP=DMAXMR*DSUM1
C
      TSPFU5=(-DMHATP/DMAXMR)*((DBLE(N)/DLOG(DMAXMR)) + 1.0D0)
     1       - DBLE(N)/DBLE(B-A)
C
      RETURN
      END
      SUBROUTINE TSPML1(Y,N,XTEMP,TEMP1,TEMP2,TEMP3,DTEMP1,
     1                  XMIN,XMAX,XMEAN,XSD,
     1                  AML,BML,THETML,ANML,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD
C              ESTIMATES FOR THE SHAPE PARAMETERS OF THE
C              TWO-SIDED POWER DISTRIBUTION.  IT CAN HANDLE EITHER
C              THE CASE WHERE THE LOWER/UPPER LIMITS ARE ESTIMATED
C              OR THE CASE WHERE THEY ARE NOT.
C     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
C     REFERENCES--"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
C                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
C                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
C                 AMERICAN STATISTICIAN, VOLUME 56,
C                 NUMBER 2, MAY, 2002.
C               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
C                 CHAPTER 1.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/07
C     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
C                                       SUBROUTINE (FROM DPMLTS)
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DOUBLE PRECISION DTEMP1(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION TRIFU2
      DOUBLE PRECISION TSPFU3
      DOUBLE PRECISION TSPFU4
      DOUBLE PRECISION TSPFU5
      EXTERNAL TRIFU2
      EXTERNAL TSPFU3
      EXTERNAL TSPFU4
      EXTERNAL TSPFU5
C
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DPROD2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DMAXMR
C
      DOUBLE PRECISION DA
      DOUBLE PRECISION DLOWBK
      DOUBLE PRECISION DUPPBK
      DOUBLE PRECISION DLOWAK
      DOUBLE PRECISION DUPPAK
      DOUBLE PRECISION DBK
      DOUBLE PRECISION DMK
      DOUBLE PRECISION DGK
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
      ISUBN1='TSPM'
      ISUBN2='L1  '
C
      IWRITE='OFF'
      IERROR='NO'
      AML=CPUMIN
      BML=CPUMIN
      THETML=CPUMIN
      ANML=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF TSPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 1--                                **
C               **  CARRY OUT CALCULATIONS                  **
C               **  FOR TWO-SIDED POWER/ MLE ESTIMATE       **
C               **********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDIST='TWO-SIDED POWER'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CALL SORT(Y,N,Y)
      DO2110I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2110 CONTINUE
C
CCCCC IMPLEMENT UPDATE MLE PROCEDURE FROM KOTZ AND VAN DORP.
CCCCC
CCCCC   1) ESTIMATES FOR LOWER AND UPPER BOUNDS ADDED
CCCCC   2) SEPARATE CASES FOR AN <= 1 AND AN > 1.
C
C          NOTE: CASE WHERE AN <= 1 HAS VALID ML ESTIMATES
C                FOR A AND B, BUT THE ML ESTIMATES FOR THETA
C                AND N IS UNSTABLE.  SO FOR NOW, JUST APPLY
C                THE N > 1 ESTIMATES.
C
C     IMPLEMENT ESTIMATES FOR A AND B USING ML METHOD
C     DESCRIBED IN KOTZ AND VAN DORP.
C
C     STEP 1: IMPLEMENT BSEARCH ON PAGE 26
C
CCCCC IF(ANSV.LE.1.0)THEN
CCCCC   AML=XMIN
CCCCC   BML=XMAX
CCCCC   DA=DBLE(AML)
CCCCC   DBK=DBLE(BML)
CCCCC ELSE
        ITER=0
        MAXIT=100
C
        DA=DBLE(XMIN) - DBLE((XMAX - XMIN)*0.1)
        DLOWBK=DTEMP1(N)
        CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2310   CONTINUE
        ITER=ITER+1
        DBK=(DLOWBK + DUPPBK)/2.0D0
        DGK=TSPFU5(DA,DBK,DTEMP1,N)
        IF(ABS(DGK).GE.1.0D-6)THEN
CCCCC     IF(ITER.GT.MAXIT)GOTO2319
          IF(ITER.GT.MAXIT)THEN
            IERROR='YES'
            GOTO9000
          ENDIF
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GOTO2310
        ELSE
          GOTO2319
        ENDIF
C
 2319   CONTINUE
        BML=REAL(DBK)
C
C       STEP 2: IMPLEMENT ABSEARCH ON PAGE 27
C
        ITER=0
        ITER2=0
C
C       STEP 2A: LOWER/UPPER BOUNDS FOR A
C
        DUPPAK=DTEMP1(1)
        DLOWAK=DTEMP1(1) - (DTEMP1(N) - DTEMP1(1))
C
C       STEP 2B: BSEARCH USING LOWER BOUND FOR A
C
 2360   CONTINUE
        ITER2=ITER2+1
        DA=DLOWAK
        DLOWBK=DTEMP1(N)
        CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2361   CONTINUE
        ITER=ITER+1
        DBK=(DLOWBK + DUPPBK)/2.0D0
        DMK=TRIFU2(DA,DBK,IR,DTEMP1,N)
        DGK=TSPFU5(DA,DBK,DTEMP1,N)
        IF(ABS(DGK).GE.1.0D-6)THEN
          IF(ITER.GT.MAXIT)GOTO2369
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GOTO2361
        ELSE
          GOTO2369
        ENDIF
C
 2369   CONTINUE
C
C       STEP 2C: COMPUTE G'(A,B)(A)
C
        DGK=TSPFU4(DA,DBK,DTEMP1,N)
        IF(DGK.LT.0.0)THEN
          DUPPAK=DLOWAK
          DLOWAK=DLOWAK - (DTEMP1(N) - DTEMP1(1))
          GOTO2360
        ENDIF
C
C       STEP 2D: BSEARCH WITH NEW VALUE OF A
C
 2380   CONTINUE
        ITER=0
        DA=(DLOWAK + DUPPAK)/2.0D0
        DLOWBK=DTEMP1(N)
        CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2381   CONTINUE
        ITER=ITER+1
        DBK=(DLOWBK + DUPPBK)/2.0D0
        DMK=TRIFU2(DA,DBK,IR,DTEMP1,N)
        DGK=TSPFU5(DA,DBK,DTEMP1,N)
        IF(ABS(DGK).GE.1.0D-6)THEN
          IF(ITER.GT.MAXIT)GOTO2389
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GOTO2381
        ELSE
          GOTO2389
        ENDIF
C
 2389   CONTINUE
        DGK=TSPFU4(DA,DBK,DTEMP1,N)
        ITER2=ITER2+1
        IF(ABS(DGK).GE.1.0D-6)THEN
          IF(ITER2.GT.MAXIT)GOTO2399
          IF(DGK.LT.0.0D0)THEN
            DUPPAK=DA
          ELSE
            DLOWAK=DA
          ENDIF
          IF((DUPPAK - DLOWAK).LT.1.0D-6)GOTO2399
          GOTO2380
        ELSE
          GOTO2399
        ENDIF
C
 2399   CONTINUE
        AML=REAL(DA)
        BML=REAL(DBK)
C
CCCCC ENDIF
C
C     NOW UPDATE THE ESTIMATE OF C USING THE
C     FINAL PARAMETER ESTIMATES FOR A AND B.
C
      DMAXMR=0.0D0
      DO2410I=1,N
        IR=I
        DTERM1=TRIFU2(DA,DBK,IR,DTEMP1,N)
C
        IF(DTERM1.GT.DMAXMR)THEN
          DMAXMR=DTERM1
          IINDX=IR
        ENDIF
C
 2410 CONTINUE
C
      IF(DMAXMR.LE.0.0D0)THEN
        IERROR='YES'
        GOTO9000
      ENDIF
C
      THETML=Y(IINDX)
      ANML=REAL(-DBLE(N)/DLOG(DMAXMR))
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF TSPML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XSD,XMIN,XMAX
 9055   FORMAT('N,XMEAN,XSD,XMIN,XMAX = ',I8,4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9057)AML,BML,THETML,ANML
 9057   FORMAT('AML,BML,THETML,ANML = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE TSPPDF(X,THETA,AN,A,B,PDF)
C
C     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
C                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
C                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
C                      INSTEAD OF FROM THE CALLING ROUTINE.
C     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
C           PDF IS:
C              TSPPDF(X,THETA,N)
C                  = N*(X/THETA)**(N-1)          0 < X <= THETA
C                  = N*((1-X)/(1-THETA))**(N-1)  THETA <= X < 1
C           THE GENERAL PDF IS:
C               f(X;THETA,N,A,B)
C                  = (N/(B-A))*((X-A)/(THETA-A))**(N-1)
C                                             A < X <= THETA
C                  = (N/(B-A))*((B-X)/(B-THETA))**(N-1)
C                                             THETA < X <= B
C     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
C                 ITS PRoPERTIES WITH APPLICATIONS IN FINANCIAL
C                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
C                 AMERICAN STATISTICIAN, VOLUME 56,
C                 NUMBER 2, MAY, 2002.
C               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/5
C     ORIGINAL VERSION--MAY       2002.
C     UPDATED         --JUNE      2007. INCLUDE A AND B EXPLICITLY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
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
      PDF=0.0
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPPDF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPPDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPPDF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
C
      IF(X.LE.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  131 FORMAT('***** ERROR IN TSPPDF--THE FIRST INPUT ARGUMENT IS ',
     1       'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
C
      IF(X.GE.B)THEN
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  141 FORMAT('***** ERROR IN TSPPDF--THE FIRST INPUT ARGUMENT IS ',
     1       'GREATER THAN THE UPPER LIMIT')
C
      DX=DBLE(X)
      DTHETA=DBLE(THETA)
      DAN=DBLE(AN)
      DA=DBLE(A)
      DB=DBLE(B)
C
      IF(DX.LE.DTHETA)THEN
        DTERM3=DLOG(DAN) - DLOG(DB-DA)
        DTERM4=(DAN-1.0D0)*(DLOG(DX-DA) - DLOG(DTHETA-DA))
        DPDF=DEXP(DTERM3 + DTERM4)
      ELSE
        DTERM3=DLOG(DAN) - DLOG(DB-DA)
        DTERM4=(DAN-1.0D0)*(DLOG(DB-DX) - DLOG(DB-DTHETA))
        DPDF=DEXP(DTERM3 + DTERM4)
      ENDIF
      PDF=REAL(DPDF)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TSPPPF(P,THETA,AN,A,B,PPF)
C
C     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
C                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
C                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
C                      INSTEAD OF FROM THE CALLING ROUTINE.
C     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
C           PPF IS:
C              TSPPPF(P,THETA,N)
C                  = THETA*(P/THETA)**(1/N)      0 < P <= THETA
C                  = 1 - (1-THETA)*((1-P)/(1-THETA))**(1/N)
C                                                P > THETA < 1
C     NOTE--GENERAL PPF IS:
C           G(P;THETA,N,A,B)
C               = A + {P*(THETA-A)**(N-1)*(B-A)}**(N)
C                     0 <= P <= (THETA-A)/(B-A)
C               = B - {(1-P)*(B-THETA)**(N-1)*(B-A)}**(N)
C                      (THETA-A)/(B-A) < P <= 1
C     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
C                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
C                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
C                 AMERICAN STATISTICIAN, VOLUME 56,
C                 NUMBER 2, MAY, 2002.
C               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/5
C     ORIGINAL VERSION--MAY       2002.
C     UPDATED         --JUNE      2007. EXPLICIT SUPPORT FOR A AND B
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DP
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
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
      PPF=0.0
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,151)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)P
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  151 FORMAT('***** ERROR IN TSPPPF--THE FIRTST INPUT ARGUMENT IS ',
     1       'OUTSIDE THE (0,1) INTERVAL')
  153 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPPPF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPPPF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPPPF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
C
      IF(P.LE.0.0)THEN
        PPF=A
      ELSEIF(P.GE.1.0)THEN
        PPF=B
      ELSE
        DP=DBLE(P)
        DTHETA=DBLE(THETA)
        DAN=DBLE(AN)
        DA=DBLE(A)
        DB=DBLE(B)
        DTERM1=(DTHETA-DA)/(DB-DA)
C
        IF(DP.LE.DTERM1)THEN
          DTERM3=DLOG(DP) + (DAN-1.0D0)*DLOG(DTHETA-DA) + DLOG(DB-DA)
          DTERM4=(1.0D0/DAN)*DTERM3
          DPPF=DA + DEXP(DTERM4)
        ELSE
          DTERM3=DLOG(1.0D0-DP) + (DAN-1.0D0)*DLOG(DB-DTHETA) +
     1           DLOG(DB-DA)
          DTERM4=(1.0D0/DAN)*DTERM3
          DPPF=DB - DEXP(DTERM4)
        ENDIF
        PPF=REAL(DPPF)
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TSPRAN(N,THETA,AN,A,B,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TWO-SIDED POWER DISTRIBUTION
C              WITH SHAPE PARAMETERS = THETA AND N.
C     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
C           PDF IS:
C              TSPPDF(X,THETA,N)
C                  = N*(X/THETA)**(N-1)          0 < X <= THETA
C                  = N*((1-X)/(1-THETA))**(N-1)  THETA <= X < 1
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --THETA  = THE SINGLE PRECISION VALUE OF THE
C                                SHAPE PARAMETER THETA.
C                                THETA SHOULD BE IN THE RANGE (0,1).
C                     --AN     = THE SINGLE PRECISION VALUE OF THE
C                                SHAPE PARAMETER N.
C                                AN SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TWO-SIDED POWER DISTRIBUTION
C             WITH SHAPE PARAMETERS = THETA AND N.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
C                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
C                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
C                 AMERICAN STATISTICIAN, VOLUME 56,
C                 NUMBER 2, MAY, 2002.
C               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--2002.5
C     ORIGINAL VERSION--MAY       2002.
C     UPDATED         --JUNE      2007. ADD A AND B TO CALL LIST
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
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPRAN--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPRAN--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPRAN--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  131 FORMAT('***** ERROR IN TWO-SIDED POWER RANDOM NUMBERS--')
  133 FORMAT('      THE REQUESTED NUMBER OF RANDOM NUMBERS IS ',
     1       'NON-POSITIVE')
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TWO-SIDED POWER DISTRIBUTION RANDOM
C     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
        CALL TSPPPF(X(I),THETA,AN,A,B,XTEMP)
        X(I)=XTEMP
  100 CONTINUE
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE TSSCDF(X,ALPHA,THETA,ALOWLM,AUPPLM,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
C              THE CUMULATIVE DISTRIBUTION FUNCITON IS:
C
C              F(X;ALPHA,THETA,A,B) =
C                  ALPHA*((X-A)/(B-A)) +
C                  (1-ALPHA)*((B-A)/(THETA-A))*((X-A)/(B-A))**2
C                  A <= X <= THETA
C
C                  1 - ALPHA*((B-X)/(B-A)) -
C                  (1-ALPHA)*((B-A)/(B-THETA))*((B-X)/(B-A))**2
C                  THETA < X <= B
C
C                  0 <= ALPHA <= 2
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C                     --THETA  = THE SINGLE PRECISION SHAPE PARAMETER
C                     --A      = THE SINGLE PRECISION LOWER LIMIT
C                                PARAMETER
C                     --B      = THE SINGLE PRECISION UPPER LIMIT
C                                PARAMETER
C     OUTPUT ARGUMENTS--CDF   = THE SINGLE PRECISION CUMULATIVE
C                               DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN A AND B, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER   2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      CDF=0.0
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  101 FORMAT('***** ERROR IN TSSCDF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  111 FORMAT('***** ERROR IN TSSCDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
  121 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSSCDF IS ',
     1       'OUTSIDE THE (0,2) INTERVAL.')
C
CCCCC IF(X.LT.A)THEN
CCCCC   WRITE(ICOUT,131)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,133)X
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,102)A
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   GOTO9000
CCCCC ENDIF
  131 FORMAT('***** ERROR IN TSSCDF--THE FIRST INPUT ARGUMENT IS ',
     1       'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
C
CCCCC IF(X.GT.B)THEN
CCCCC   WRITE(ICOUT,141)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,133)X
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   CDF=1.0
CCCCC   GOTO9000
CCCCC ENDIF
  141 FORMAT('***** ERROR IN TSSCDF--THE FIRST INPUT ARGUMENT IS ',
     1       'GREATER THAN THE UPPER LIMIT')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LE.A)THEN
        CDF=0.0
      ELSEIF(X.GE.B)THEN
        CDF=1.0
      ELSEIF(X.LE.THETA)THEN
        TERM1=ALPHA*(X-A)/(B-A)
        TERM2=(1.0-ALPHA)*(B-A)/(THETA-A)
        TERM3=((X-A)/(B-A))**2
        CDF=TERM1 + TERM2*TERM3
      ELSE
        TERM1=ALPHA*(B-X)/(B-A)
        TERM2=(1.0-ALPHA)*((B-A)/(B-THETA))
        TERM3=((B-X)/(B-A))**2
        CDF=1.0 - TERM1 - TERM2*TERM3
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TSSPDF(X,ALPHA,THETA,ALOWLM,AUPPLM,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
C              THE STANDARD PROBABILITY DENSITY FUNCTION IS:
C
C              f(X;ALPHA,THETA,A,B) =
C                  ALPHA/(B-A) + 2*(1-ALPHA)*(X-A)/
C                  {(B-A)*(THETA-A)}
C                  A <= X <= THETA, 0 <= ALPHA <= 2
C
C              f(X;ALPHA,THETA,A,B) =
C                  ALPHA/(B-A) + 2*(1-ALPHA)*(B-X)/
C                  {(B-A)*(B-THETA)}
C                  THETA < X <= B, 0 <= ALPHA <= 2
C
C              WITH ALPHA AND THETA DENOTING THE SHAPE PARAMETER
C              AND THE THRESHOLD PARAMETER, RESPECTIVELY.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --A       = THE SINGLE PRECISION LOWER LIMIT
C                                 PARAMETER
C                     --B       = THE SINGLE PRECISION UPPER LIMIT
C                                 PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER   2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      PDF=0.0
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  101 FORMAT('***** ERROR IN TSSPDF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  111 FORMAT('***** ERROR IN TSSPDF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
      IF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
  121 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSSPDF IS ',
     1       'OUTSIDE THE (0,2) INTERVAL.')
C
      IF(X.LT.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  131 FORMAT('***** ERROR IN TSSPDF--THE FIRST INPUT ARGUMENT IS ',
     1       'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
C
      IF(X.GT.B)THEN
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  141 FORMAT('***** ERROR IN TSSPDF--THE FIRST INPUT ARGUMENT IS ',
     1       'GREATER THAN THE UPPER LIMIT')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      TERM1=ALPHA/(B-A)
      TERM2=2.0*(1.0-ALPHA)/(B-A)
C
      IF(X.LE.A .OR. X.GE.B)THEN
        PDF=TERM1
      ELSEIF(X.LE.THETA)THEN
        TERM3=(X-A)/(THETA-A)
        PDF=TERM1 + TERM2*TERM3
      ELSE
        TERM3=(B-X)/(B-THETA)
        PDF=TERM1 + TERM2*TERM3
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE TSSPPF(P,ALPHA,THETA,ALOWLM,AUPPLM,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
C              INVERTING THE STANDARD FORM OF THE CDF FUNCTION
C              YIELDS THE FOLLOWING QUADRATIC EQUATION:
C
C              {-ALPHA +/- SQRT(ALPHA**2 + 4*P*(1-ALPHA)/THETA)}/
C              {2*(1-ALPHA)/THETA}
C              0 <= P <= THETA
C
C              {2*C1+ALPHA) +/- 
C              SQRT((-2*C1-ALPHA)**2 - 4*C1*(C1+ALPHA+P-1))}/
C              {2*C1}
C              THETA < P <= 1
C
C              WHERE
C
C              C1=(1-ALPHA)/(1-THETA)
C
C              THE PERCENT POINT FUNCTION IS COMPUTED BY
C              DETERMINING WHICH OF THE TWO ROOTS LIES IN THE
C              (0,1) INTERVAL.
C
C              FOR THE GENERAL FORM, TRANSFORM THETA TO
C              THE (0,1) SCALE AND USE THE RELATION
C
C                  G(P;ALPHA,THETA,A,B) = A +
C                                         (B-A)*G(P;ALPHA,THETA,0,1)
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --A       = THE SINGLE PRECISION LOWER LIMIT
C                                 PARAMETER
C                     --B       = THE SINGLE PRECISION UPPER LIMIT
C                                 PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--OCTOBER  2007
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      PPF=0.0
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO TSSPPF IS ',
     1       'OUTSIDE THE (0,1) INTERVAL.')
      ELSEIF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSSPPF IS ',
     1       'OUTSIDE THE (0,2) INTERVAL.')
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
  101 FORMAT('***** ERROR IN TSSPPF--LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
      ELSEIF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  111 FORMAT('***** ERROR IN TSSPPF--THETA IS OUTSIDE THE ',
     1       'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      THETSV=THETA
      THETA=(THETA-A)/(B-A)
C
C     IF ALPHA = 1, THEN WE HAVE A UNIFORM DISTRIBUTION.
C     HANDLE THIS SEPARATELY.
C
      IF(ALPHA.EQ.1.0)THEN
        PPF=A + (B-A)*P
      ELSEIF(P.LE.0.0)THEN
        PPF=A
      ELSEIF(P.GE.1.0)THEN
        PPF=B
      ELSEIF(P.LE.THETA)THEN
        TERM1=SQRT(ALPHA**2 + 4.0*P*(1.0-ALPHA)/THETA)
        TERM2=2.0*(1.0-ALPHA)/THETA
        PPF=(-ALPHA + TERM1)/TERM2
        IF(PPF.LT.0.0 .OR. PPF.GT.1.0)THEN
          PPF=(-ALPHA - TERM1)/TERM2
        ENDIF
        PPF=A + (B-A)*PPF
      ELSE
        CALL TSSCDF(THETA,ALPHA,THETA,A,B,ABNDLW)
        ATEMP=(1.0-ALPHA)/(1.0-THETA)
        A1=ATEMP
        B1=-2.0*ATEMP - ALPHA
        C1=ATEMP + ALPHA + P - 1
        TERM1=SQRT(B1**2 - 4.0*A1*C1)
        TERM2=2.0*A1
        PPF1=(-B1 + TERM1)/TERM2
        PPF1=A + (B-A)*PPF1
        PPF2=(-B1 - TERM1)/TERM2
        PPF2=A + (B-A)*PPF2
        PPF=PPF1
        IF(PPF.LT.ABNDLW .OR. PPF.GT.1.0)THEN
          PPF=PPF2
        ENDIF
      ENDIF
C
 9000 CONTINUE
      THETA=THETSV
      RETURN
      END
      SUBROUTINE TSSRAN(N,ALPHA,THETA,ALOWLM,AUPPLM,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE TWO-SIDED SLOPE DISTRIBUTION WITH
C              SHAPE PARAMETERS ALPHA AND THETA.
C
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C              f(X;ALPHA,THETA,A,B) =
C                  ALPHA/(B-A) + 2*(1-ALPHA)*(X-A)/
C                  {(B-A)*(THETA-A)}
C                  A <= X <= THETA, 0 <= ALPHA <= 2
C
C              f(X;ALPHA,THETA,A,B) =
C                  ALPHA/(B-A) + 2*(1-ALPHA)*(B-X)/
C                  {(B-A)*(B-THETA)}
C                  THETA < X <= B, 0 <= ALPHA <= 2
C
C              WITH ALPHA AND THETA DENOTING THE SHAPE PARAMETER
C              AND THE THRESHOLD PARAMETER, RESPECTIVELY.
C
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
C                     --A       = THE SINGLE PRECISION LOWER LIMIT
C                                 PARAMETER
C                     --B       = THE SINGLE PRECISION UPPER LIMIT
C                                 PARAMETER
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE TWO-SIDED SLOPE DISTRIBUTION
C             WITH SHAPE PARAMETER ALPHA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TSSPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 8.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHMOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
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
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
C
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',
     1'TWO-SIDED SLOPE RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
      IF(ALPHA.LE.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
  201 FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER IS ',
     1       'OUTSIDE THE (0,2) INTERVAL.')
  203 FORMAT('      THE VALUE OF ALPHA IS ',G15.7)
C
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  101 FORMAT('***** ERROR IN TWO-SIDED SLOPE RANDOM NUMBERS--')
  102 FORMAT('      LOWER LIMIT GREATER THAN OR ',
     1       'EQUAL TO UPPER LIMIT')
  103 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  104 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
C
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  111 FORMAT('      THETA IS OUTSIDE THE LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
C
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N TWO-SIDED SLOPE DISTRIBUTION RANDOM
C     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO300I=1,N
        CALL TSSPPF(X(I),ALPHA,THETA,A,B,XTEMP)
        X(I)=XTEMP
  300 CONTINUE
C
 9000 CONTINUE
      RETURN
      END

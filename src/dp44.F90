      SUBROUTINE TAGUCH(X,N,ICASPL,IWRITE,XTAGUC,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
!              OF THE DATA IN THE INPUT VECTOR X.
!              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
!              (FOR THE "TARGET IS BEST WITH
!              VARIANCE DEPENDENT ON MEAN" CASE) =
!              10 * LOG10 ( YBAR**2 / S**2)
!              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
!              (FOR THE "LARGE IS BEST" CASE) =
!              -10 * LOG10 (AVERAGE SUM OF SQUARED INVERSES)
!              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
!              (FOR THE "SMALL IS BEST" CASE) =
!              -10 * LOG10 (AVERAGE SUM OF SQUARED OBSERVATIONS)
!              THE SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO
!              (FOR THE "TARGET IS BEST WITH
!              VARIANCE NOT DEPENDENT ON MEAN" CASE) =
!              -10 * LOG10 (VARIANCE)
!              THE DENOMINATOR N-1 IS USED IN COMPUTING THE
!              SAMPLE STANDARD DEVIATION.
!              THE SAMPLE STANDARD DEVIATION = SQRT((THE SUM OF THE
!              SQUARED DEVIATIONS ABOUT THE SAMPLE MEAN)/(N-1)).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--XTAGUC = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TAGUCHI SIGNAL-TO-NOISE RATIO.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DABS, DLOG10.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--ELLIOT, JACK G.
!                 STATISTICAL METHODS AND APPLICATIONS
!                 ALLIED SIGNAL, 1987, PAGES 4-3 AND 4-4.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88.8
!     ORIGINAL VERSION--AUGUST    1988.
!     UPDATED         --MAY       1989.  SN0, SN+, SN-, SN00
!     UPDATED         --APRIL     1992.  DELETE DRATIO
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
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
      DOUBLE PRECISION DTERM
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
!
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TAGU'
      ISUBN2='CH  '
      IERROR='NO'
!
      DMEAN=0.0D0
      DSD=0.0D0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TAGUCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IWRITE,IBUGA3,N
   52   FORMAT('ICASPL,IWRITE,IBUGA3,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
!               **********************************************
!               **  COMPUTE  TAGUCHI SIGNAL-TO-NOISE RATIO  **
!               **********************************************
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
  111   FORMAT('***** ERROR IN TAGUCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',    &
               'VARIABLE FOR WHICH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)
  114   FORMAT('      THE TAGUCHI SIGNAL-TO-NOISE RATIO IS TO BE ',  &
               'COMPUTED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      MUST BE 1 OR LARGER.  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING IN TAGUCH--THE SECOND INPUT ARGUMENT ',  &
               '(N) HAS THE VALUE 1.')
        CALL DPWRST('XXX','BUG ')
        XTAGUC=0.0
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO I=2,N
        IF(X(I).NE.HOLD)GO TO 139
      ENDDO
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,136)HOLD
  136 FORMAT('***** WARNING IN TAGUCH--THE FIRST INPUT ARGUMENT ',   &
             'HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      XTAGUC=0.0
      GO TO 9000
  139 CONTINUE
!
!               **************************************************
!               **  STEP 10--                                   **
!               **  BRANCH TO THE APPROPRIATE SUBCASE           **
!               **************************************************
!
!CCCC THE FOLLOWING 4 LINES WERE FIXED MAY 1989
      IF(ICASPL.EQ.'SN0')GO TO 1100
      IF(ICASPL.EQ.'SN+')GO TO 1200
      IF(ICASPL.EQ.'SN-')GO TO 1300
      IF(ICASPL.EQ.'SN00')GO TO 1400
      GO TO 1100
!
!               ******************************************************
!               **  STEP 11--                                       **
!               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO       **
!               **  FOR THE "TARGET IS BEST" CASE                   **
!               **  (AND WITH THE VARIANCE CHANGING WITH THE MEAN)  **
!               ******************************************************
!
 1100 CONTINUE
      DN=N
      DSUM=0.0D0
      DO I=1,N
         DX=X(I)
         DSUM=DSUM+DX
      ENDDO
      DMEAN=DSUM/DN
!
      DSUM=0.0D0
      DO I=1,N
         DX=X(I)
         DSUM=DSUM+(DX-DMEAN)**2
      ENDDO
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
!
      DTERM=(-999.99D0)
      DTERM2=(-999.99D0)
      DTERM3=(-999.99D0)
      IF(DSD.NE.0.0D0)THEN
        DTERM=DMEAN/DSD
        DTERM2=DABS(DTERM)
        DTERM3=DLOG10(DTERM2)
      ENDIF
      XTAGUC=20.0D0*DTERM3
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
 1181   FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO (FOR THE "TARGET IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)
 1183   FORMAT('BEST WITH VARIANCE DEPENDENT ON MEAN" CASE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1184)N,XTAGUC
 1184   FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO   **
!               **  FOR THE "LARGE IS BEST" CASE                **
!               **************************************************
!
 1200 CONTINUE
      DN=N
      DSUM=0.0D0
      DO I=1,N
        DX=X(I)
        DARG=1.0D0/DX
        DSUM=DSUM+DARG*DARG
      ENDDO
      DTERM=DSUM/DN
!
      DTERM2=DABS(DTERM)
      DTERM3=DLOG10(DTERM2)
      XTAGUC=(-10.0D0*DTERM3)
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1281)
 1281   FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO (FOR THE "LARGE IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1283)N,XTAGUC
 1283   FORMAT('BEST" CASE) OF THE ',I8,' OBSERVATIONS = ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               **************************************************
!               **  STEP 13--                                   **
!               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO   **
!               **  FOR THE "SMALL IS BEST" CASE                **
!               **************************************************
!
 1300 CONTINUE
      DN=N
      DSUM=0.0D0
      DO I=1,N
         DX=X(I)
         DSUM=DSUM+DX*DX
      ENDDO
      DTERM=DSUM/DN
!
      DTERM2=DABS(DTERM)
      DTERM3=DLOG10(DTERM2)
      XTAGUC=(-10.0D0*DTERM3)
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1381)
 1381   FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO (FOR THE "SMALL IS ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1383)N,XTAGUC
 1383   FORMAT('BEST" CASE) OF THE ',I8,' OBSERVATIONS = ',E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               ********************************************************
!               **  STEP 14--                                         **
!               **  COMPUTE THE TAGUCHI SIGNAL-TO-NOISE RATIO         **
!               **  FOR THE "TARGET IS BEST" CASE                     **
!               **  (AND WITH THE VARIANCE NOT CHANGING WITH THE MEAN)**
!               ********************************************************
!
 1400 CONTINUE
      DN=N
      DSUM=0.0D0
      DO I=1,N
         DX=X(I)
         DSUM=DSUM+DX
      ENDDO
      DMEAN=DSUM/DN
!
      DSUM=0.0D0
      DO I=1,N
         DX=X(I)
         DSUM=DSUM+(DX-DMEAN)**2
      ENDDO
      DVAR=DSUM/(DN-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
!
      DTERM=-999.99D0
      DTERM2=-999.99D0
      DTERM3=(-999.99D0)
      IF(DSD.NE.0.0D0)THEN
        DTERM=DSD
        DTERM2=DABS(DTERM)
        DTERM3=DLOG10(DTERM2)
      ENDIF
      XTAGUC=(-20.0D0*DTERM3)
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1481)
 1481   FORMAT('THE TAGUCHI SIGNAL-TO-NOISE RATIO (FOR THE "TARGET IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1483)
 1483   FORMAT('BEST WITH VARIANCE NOT DEPENDENT ON MEAN" CASE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1484)N,XTAGUC
 1484   FORMAT('OF THE ',I8,' OBSERVATIONS = ',E15.7)
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
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TAGUCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IWRITE,IBUGA3,IERROR,N
 9012   FORMAT('ICASPL,IWRITE,IBUGA3,IERROR,N = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DMEAN,DSD,XTAGUC
 9014   FORMAT('DMEAN,DSD,XTAGUC = ',2D15.7,E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DTERM,DTERM2,DTERM3
 9015   FORMAT('DTERM,DTERM2,DTERM3 = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE TAGUCH
      SUBROUTINE TCDF(X,ANU,CDF)
!CCCC SUBROUTINE TCDF(X,NU,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!              THIS DISTRIBUTION IS DEFINED FOR ALL X.
!              THE PROBABILITY DENSITY FUNCTION IS GIVEN
!              IN THE REFERENCES BELOW.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --NU     = THE INTEGER NUMBER OF DEGREES
!                                OF FREEDOM.
!                                NU SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
!                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
!               --FEDERIGHI, EXTENDED TABLES OF THE
!                 PERCENTAGE POINTS OF STUDENT'S
!                 T-DISTRIBUTION, JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION,
!                 1959, PAGES 683-688.
!               --OWEN, HANDBOOK OF STATISTICAL TABLES,
!                 1962, PAGES 27-30.
!               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
!                 FOR STATISTICIANS, VOLUME 1, 1954,
!                 PAGES 132-134.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --MAY       1974.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --OCTOBER   1976.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
!                                       DEGREES OF FREEDOM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DBETAI
!
      EXTERNAL DBETAI
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA NUCUT/1000/
      DATA PI/3.14159265358979D0/
      DATA DCONST/0.3989422804D0/
      DATA B11/0.25D0/
      DATA B21/0.01041666666667D0/
      DATA B22,B23,B24,B25/3.0D0,-7.0D0,-5.0D0,-3.0D0/
      DATA B31/0.00260416666667D0/
      DATA B32,B33,B34,B35,B36,B37/1.0D0,-11.0D0,14.0D0,6.0D0,   &
                                  -3.0D0,-15.0D0/
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GO TO 8000
!
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',   &
               'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  STEP 2--                                      **
!               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
!               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
!               **  SET CDF = 0.0 AND RETURN.                     **
!               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
!               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
!               **  SET CDF = 0.0 AND RETURN.                     **
!               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
!               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
!               **  SET CDF = 1.0 AND RETURN.                     **
!               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
!               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
!               **  SET CDF = 1.0 AND RETURN.                     **
!               ****************************************************
!
      DX=X
      ANU=NU
      DNU=NU
!
      IF(NU.LE.2)GO TO 109
      SD=SQRT(ANU/(ANU-2.0))
      Z=X/SD
      IF(NU.LT.10.AND.Z.LT.-3000.0)GO TO 107
      IF(NU.GE.10.AND.Z.LT.-150.0)GO TO 107
      IF(NU.LT.10.AND.Z.GT.3000.0)GO TO 108
      IF(NU.GE.10.AND.Z.GT.150.0)GO TO 108
      GO TO 109
  107 CDF=0.0
      GO TO 9000
  108 CDF=1.0
      GO TO 9000
  109 CONTINUE
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  DISTINGUISH BETWEEN THE SMALL AND MODERATE  **
!               **  DEGREES OF FREEDOM CASE VERSUS THE          **
!               **  LARGE DEGREES OF FREEDOM CASE               **
!               **************************************************
!
      IF(NU.LT.NUCUT)GO TO 110
      GO TO 250
!
!               ************************************************************
!               **  STEP 3.1--                                            **
!               **  TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE  **
!               **  METHOD UTILIZED--EXACT FINITE SUM                     **
!               **  (SEE AMS 55, PAGE 948, FORMULAE 26.7.3 AND 26.7.4).   **
!               ************************************************************
!
  110 CONTINUE
      C=DSQRT(DNU/(DX*DX+DNU))
      CSQ=DNU/(DX*DX+DNU)
      S=DX/DSQRT(DX*DX+DNU)
      IMAX=NU-2
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GO TO 120
!
      SUM=C
      IF(NU.EQ.1)SUM=0.0D0
      TERM=C
      IMIN=3
      GO TO 130
!
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
!
  130 IF(IMIN.GT.IMAX)GO TO 160
      DO 100 I=IMIN,IMAX,2
      AI=I
      TERM=TERM*((AI-1.0D0)/AI)*CSQ
      SUM=SUM+TERM
  100 CONTINUE
!
  160 SUM=SUM*S
      IF(IEVODD.EQ.0)GO TO 170
      SUM=(2.0D0/PI)*(DATAN(DX/DSQRT(DNU))+SUM)
  170 CDF=0.5D0+SUM/2.0D0
      GO TO 9000
!
!               **************************************************************
!               **  STEP 3.2--                                              **
!               **  TREAT THE LARGE DEGREES OF FREEDOM CASE.                **
!               **  METHOD UTILIZED--TRUNCATED ASYMPTOTIC EXPANSION         **
!               **  (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 102, FORMULA 10;  **
!               **  SEE FEDERIGHI, PAGE 687).                               **
!               **************************************************************
!
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
      GO TO 9000
!
!CCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
!
 8000 CONTINUE
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',   &
               'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DNU=DBLE(ANU)
!
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
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TCDF
      SUBROUTINE TCRUDE( NDIM, MAXPTS, ABSEST, FINEST, IR )
!
!     Crude Monte-Carlo Algorithm for Deak method with
!      weighted results on restart
!
      INTEGER NDIM, MAXPTS, M, IR
      DOUBLE PRECISION FINEST, ABSEST, SPMVT,   &
           VARSQR, VAREST, VARPRD, FINDIF, FINVAL
      SAVE VAREST
!
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
!
      RETURN
      END SUBROUTINE TCRUDE
      FUNCTION TFN(X, FX)
!
! Two versions of algorithm AS 76 are given here; the original with one
! correction incorporated, and AS R55, also amended.   AS R55 requires
! AS 76.   N.B. The accuracy of AS 76 could be increased by using more
! Gaussian quadrature points, or better, by using Hermite integration.
!
!
!     ALGORITHM AS 76  APPL. STATIST. (1974) VOL.23, NO.3
!
!     Calculates the T-function of Owen, using Gaussian quadrature.
!     Incorporates correction AS R30 (vol.28, no.1, 1979)
!
      REAL U(5), R(5)
!
      DATA U /0.0744372, 0.2166977, 0.3397048, 0.4325317, 0.4869533/
      DATA R /0.1477621, 0.1346334, 0.1095432, 0.0747257, 0.0333357/
      DATA NG,    TP,    TV1,     TV2,     TV3,     TV4   &
         /  5, 0.159155, 1.E-35,  15.0,    15.0,   1.E-5 /
      DATA ZERO, QUART, HALF, ONE,  TWO   &
         / 0.0,  0.25,  0.5,  1.0,  2.0 /
!
!     Test for X near zero
!
      IF (ABS(X) .GE. TV1) GO TO 5
      TFN = TP * ATAN(FX)
      RETURN
!
!     Test for large values of abs(X)
!
    5 IF (ABS(X) .GT. TV2) GO TO 10
!
!     Test for FX near zero
!
      IF (ABS(FX) .GE. TV1) GO TO 15
   10 TFN = ZERO
      RETURN
!
!     Test whether abs(FX) is so large that it must be truncated
!
   15 XS = -HALF * X * X
      X2 = FX
      FXS = FX * FX
      IF (LOG(ONE + FXS) - XS * FXS .LT. TV3) GO TO 25
!
!     Computation of truncation point by Newton iteration
!
      X1 = HALF * FX
      FXS = QUART * FXS
   20 RT = FXS + ONE
      X2 = X1 + (XS * FXS + TV3 - LOG(RT)) / (TWO * X1 * (ONE/RT - XS))
      FXS = X2 * X2
      IF (ABS(X2 - X1) .LT. TV4) GO TO 25
      X1 = X2
      GO TO 20
!
!     Gaussian quadrature
!
   25 RT = ZERO
      DO 30 I = 1, NG
      R1 = ONE + FXS * (HALF + U(I))**2
      R2 = ONE + FXS * (HALF - U(I))**2
      RT = RT + R(I) * (EXP(XS * R1) / R1 + EXP(XS * R2) / R2)
   30 CONTINUE
      TFN = RT * X2 * TP
!
      RETURN
      END FUNCTION TFN
      REAL FUNCTION THA(H1, H2, A1, A2)
!
!     AS R55  APPL. STATIST. (1985) VOL.34, NO.1
!
!     A remark on AS 76
!     Incorporating improvements in AS R80 (Appl. Statist. (1989)
!     vol.38, no.3), and AS R89 (Appl. Statist. (1992) vol.41, no.2).
!
!     Computes T(H1/H2, A1/A2) for any real numbers H1, H2, A1 and A2
!
!     Auxiliary function required: ALNORM (= AS 66) and AS 76
!
      REAL A, A1, A2, G, H, H1, H2, TFN, ABSA, AH, GH, GAH,   &
        TWOPI, LAM, EX, C1, C2,   &
        ZERO, ONE, TWO, PT3, SEVEN, HALF, SIX, QUART
!
      DATA TWOPI /6.2831853/, ZERO /0.0/, ONE /1.0/, TWO /2.0/,   &
         PT3 /0.3/, SEVEN /7.0/, HALF /0.5/, SIX /6.0/, QUART /0.25/
!
      IF (H2 .NE. ZERO) GO TO 1
      THA = ZERO
      RETURN
!
    1 H = H1 / H2
      IF (A2 .EQ. ZERO) GO TO 2
      A = A1 / A2
      IF ((ABS(H) .LT. PT3) .AND. (ABS(A) .GT. SEVEN)) GO TO 6
!
!     Correction AS R89
!
      ABSA = ABS(A)
      IF (ABSA .GT. ONE) GO TO 7
      THA = TFN(H, A)
      RETURN
    7 AH = ABSA * H
!NIST GH = ALNORM(H, .FALSE.)
      CALL NORCDF(H,GH)
!NIST GAH = ALNORM(AH, .FALSE.)
      CALL NORCDF(AH,GAH)
      THA = HALF * (GH + GAH) - GH * GAH - TFN(AH, ONE/ABSA)
      IF (A .LT. ZERO) THA = - THA
      RETURN
!
 2    CONTINUE
!NIST G = ALNORM(H, .FALSE.)
      CALL NORCDF(H,G)
      IF (H .GE. ZERO) GO TO 3
      THA = G / TWO
      GO TO 4
    3 THA = (ONE - G) / TWO
    4 IF (A1 .GE. ZERO) RETURN
      THA = -THA
      RETURN
!
    6 LAM = ABS(A * H)
      EX = EXP(-LAM * LAM / TWO)
!NIST G = ALNORM(LAM, .FALSE.)
      CALL NORCDF(LAM,G)
      C1 = (EX/LAM + SQRT(TWOPI) * (G - HALF)) / (TWOPI)
      C2 = ((LAM * LAM + TWO) * EX/LAM**3 + SQRT(TWOPI) * (G - HALF))   &
             / (SIX * TWOPI)
      AH = ABS(H)
      THA = QUART - C1 * AH + C2 * AH**3
      THA = SIGN(THA, A)
!
      RETURN
      END FUNCTION THA
      SUBROUTINE TDCDF(DX,DNU,DCDF)
!CCCC SUBROUTINE TDCDF(DX,NU,DCDF)
!
!     DOUBLE PRECISION VERSION OF TCDF.  CALLED BY SKEW-T DISTRIBUTION.
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!              THIS DISTRIBUTION IS DEFINED FOR ALL X.
!              THE PROBABILITY DENSITY FUNCTION IS GIVEN
!              IN THE REFERENCES BELOW.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --NU     = THE INTEGER NUMBER OF DEGREES
!                                OF FREEDOM.
!                                NU SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
!                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
!               --FEDERIGHI, EXTENDED TABLES OF THE
!                 PERCENTAGE POINTS OF STUDENT'S
!                 T-DISTRIBUTION, JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION,
!                 1959, PAGES 683-688.
!               --OWEN, HANDBOOK OF STATISTICAL TABLES,
!                 1962, PAGES 27-30.
!               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
!                 FOR STATISTICIANS, VOLUME 1, 1954,
!                 PAGES 132-134.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.12
!     ORIGINAL VERSION--DECEMBER  2003.
!     UPDATED         --JANUARY   2017. ALLOW NON-INTEGER DEGREES OF
!                                       FREEDOM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA NUCUT/1000/
      DATA PI/3.14159265358979D0/
      DATA DCONST/0.3989422804D0/
      DATA B11/0.25D0/
      DATA B21/0.01041666666667D0/
      DATA B22,B23,B24,B25/3.0D0,-7.0D0,-5.0D0,-3.0D0/
      DATA B31/0.00260416666667D0/
      DATA B32,B33,B34,B35,B36,B37/1.0D0,-11.0D0,14.0D0,6.0D0,   &
                                  -3.0D0,-15.0D0/
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      NU=INT(DNU)
      IF(ABS(DNU-DBLE(NU)).GT.0.000001D0)GO TO 8000
!
      IF(NU.LE.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE DEGREES OF FREEDOM PARAMETER ',   &
               'TO THE TDCDF SUBROUTINE IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)NU
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8   ,'*****')
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  STEP 2--                                      **
!               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
!               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
!               **  SET CDF = 0.0 AND RETURN.                     **
!               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
!               **  STANDARD DEVIATIONS BELOW THE MEAN,           **
!               **  SET CDF = 0.0 AND RETURN.                     **
!               **  IF NU IS 3 THROUGH 9 AND X IS MORE THAN 3000  **
!               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
!               **  SET CDF = 1.0 AND RETURN.                     **
!               **  IF NU IS 10 OR LARGER AND X IS MORE THAN 150  **
!               **  STANDARD DEVIATIONS ABOVE THE MEAN,           **
!               **  SET CDF = 1.0 AND RETURN.                     **
!               ****************************************************
!
      ANU=NU
      DNU=NU
!
      IF(NU.LE.2)GO TO 109
      SD=SQRT(ANU/(ANU-2.0))
      Z=REAL(DX)/SD
      IF(NU.LT.10.AND.Z.LT.-3000.0)GO TO 107
      IF(NU.GE.10.AND.Z.LT.-150.0)GO TO 107
      IF(NU.LT.10.AND.Z.GT.3000.0)GO TO 108
      IF(NU.GE.10.AND.Z.GT.150.0)GO TO 108
      GO TO 109
  107 DCDF=0.0D0
      GO TO 9000
  108 DCDF=1.0D0
      GO TO 9000
  109 CONTINUE
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  DISTINGUISH BETWEEN THE SMALL AND MODERATE  **
!               **  DEGREES OF FREEDOM CASE VERSUS THE          **
!               **  LARGE DEGREES OF FREEDOM CASE               **
!               **************************************************
!
      IF(NU.LT.NUCUT)GO TO 110
      GO TO 250
!
!               ************************************************************
!               **  STEP 3.1--                                            **
!               **  TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE  **
!               **  METHOD UTILIZED--EXACT FINITE SUM                     **
!               **  (SEE AMS 55, PAGE 948, FORMULAE 26.7.3 AND 26.7.4).   **
!               ************************************************************
!
  110 CONTINUE
      C=DSQRT(DNU/(DX*DX+DNU))
      CSQ=DNU/(DX*DX+DNU)
      S=DX/DSQRT(DX*DX+DNU)
      IMAX=NU-2
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GO TO 120
!
      SUM=C
      IF(NU.EQ.1)SUM=0.0D0
      TERM=C
      IMIN=3
      GO TO 130
!
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
!
  130 IF(IMIN.GT.IMAX)GO TO 160
      DO 100 I=IMIN,IMAX,2
      AI=I
      TERM=TERM*((AI-1.0D0)/AI)*CSQ
      SUM=SUM+TERM
  100 CONTINUE
!
  160 SUM=SUM*S
      IF(IEVODD.EQ.0)GO TO 170
      SUM=(2.0D0/PI)*(DATAN(DX/DSQRT(DNU))+SUM)
  170 DCDF=0.5D0+SUM/2.0D0
      GO TO 9000
!
!               **************************************************************
!               **  STEP 3.2--                                              **
!               **  TREAT THE LARGE DEGREES OF FREEDOM CASE.                **
!               **  METHOD UTILIZED--TRUNCATED ASYMPTOTIC EXPANSION         **
!               **  (SEE JOHNSON AND KOTZ, VOLUME 2, PAGE 102, FORMULA 10;  **
!               **  SEE FEDERIGHI, PAGE 687).                               **
!               **************************************************************
!
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
      GO TO 9000
!
!CCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
!
 8000 CONTINUE
      IF(DNU.LE.0.0D0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',   &
               'TO TCDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GO TO 9000
      ENDIF
!
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
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TDCDF
      SUBROUTINE TDPDF(DX,NU,DPDF)
!
!     DOUBLE PRECISION VERSION OF TPDF.  USED BY SKEW T DISTRIBUTION.
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!              THIS DISTRIBUTION IS DEFINED FOR ALL X.
!              THE PROBABILITY DENSITY FUNCTION IS GIVEN
!              IN THE REFERENCES BELOW.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                DX SHOULD BE NON-NEGATIVE.
!                     --NU     = THE INTEGER NUMBER OF DEGREES
!                                OF FREEDOM.
!                                NU SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--DPDF   = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
!                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
!               --FEDERIGHI, EXTENDED TABLES OF THE
!                 PERCENTAGE POINTS OF STUDENT'S
!                 T-DISTRIBUTION, JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION,
!                 1959, PAGES 683-688.
!               --OWEN, HANDBOOK OF STATISTICAL TABLES,
!                 1962, PAGES 27-30.
!               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
!                 FOR STATISTICIANS, VOLUME 1, 1954,
!                 PAGES 132-134.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--2003.12
!     ORIGINAL VERSION--DECEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX,DNU, DPDF
      DOUBLE PRECISION DSQTPI,DRATIO
      DOUBLE PRECISION DCONST,DPOWER
      DOUBLE PRECISION AI
      DOUBLE PRECISION DSQRT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DSQTPI/1.77245385090552D0/
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** FATAL ERROR--THE DEGREES OF FREEDOM PARAMETER ',   &
               'TO THE TDPDF SUBROUTINE IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
        CALL DPWRST('XXX','BUG ')
        DPDF=0.0
      ENDIF
!
!               ****************************************************
!               **  STEP 2--
!               **  COMPUTE THE CONSTANT = 1/(SQRT(NU)*BETA(1/2,NU/2))
!               **  = (1/(SQRT(NU)*SQRT(PI))) * (GAMMA((NU/2)+(1/2))/GAMMA(NU/2)
!               ****************************************************
!
      DNU=NU
!
      DRATIO=1.0D0
      IEVODD=NU-2*(NU/2)
      IMIN=3
      IF(IEVODD.EQ.0)IMIN=2
      IF(NU.LT.IMIN)GO TO 250
      DO 300 I=IMIN,NU,2
      AI=I
      DRATIO=((AI-1.0D0)/AI)*DRATIO
  300 CONTINUE
  250 CONTINUE
      DRATIO=DRATIO*DNU
      IF(IEVODD.EQ.0)GO TO 260
      DRATIO=DRATIO/DSQTPI
      GO TO 400
  260 CONTINUE
      DRATIO=DRATIO*DSQTPI/2.0D0
  400 CONTINUE
!
      DCONST=DRATIO/(DSQTPI*DSQRT(DNU))
!
!               ************************************
!               **  STEP 3--                      **
!               **  COMPUTE THE DENSITY FUNCTION  **
!               ************************************
!
      DPOWER=-(DNU+1.0D0)/2.0D0
      DPDF=DCONST*((1.0D0+DX*DX/DNU)**DPOWER)
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TDPDF
      SUBROUTINE THRESH(Y,TVAL,N1,NT,IWRITE,Y2,TAG,NOUT,   &
                        ICASE,MAXNXT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--FOR EACH VALUE IN A GIVEN A SET OF THRESHOLDS (TVAL),
!              CREATE AN OUTPUT VECTOR EQUAL TO 1 IF THE INPUT RESPONSE
!              VARIABLE IS GREATER THAN (OR LESS THAN) THE THRESHOLD
!              VALUE.  CREATE A CORREPONDING TAG VARIABLE.  FOR EXAMPLE,
!              IF THERE ARE 4 THRESHOLDS AND 25 OBSERVATIONS IN THE
!              RESPONSE VARIABLE, THE OUTPUT VARIABLES WILL HAVE
!              4*25=100 VALUES.  THIS COMMAND CAN BE HELPFUL IN
!              GENERATING ROC CURVES FROM RAW DATA.
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE INPUT RESPONSE VARIABLE.
!                     --TVAL   = THE SINGLE PRECISION VECTOR CONTAINING
!                                A LIST OF THRESHOLDS.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR TVAL.
!                     --ICASE  = THE CHARACTER STRING THAT SPECIFIES
!                                THE DIRECTION OF THE THRESHOLD.
!     OUTPUT ARGUMENTS--Y2     = THE SINGLE PRECISION VECTOR
!                                CONTAINING 0 OR 1 DEPENDING ON WHETHER
!                                THE INPUT RESPONSE VARIABLE IS LESS
!                                THAN (OR GREATER THAN) THE THRESHOLD.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
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
!     VERSION NUMBER--2012/7
!     ORIGINAL VERSION--JULY      2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TVAL(*)
      DIMENSION Y2(*)
      DIMENSION TAG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='THRE'
      ISUBN2='SH  '
      IERROR='NO'
      IWRITE='OFF'
!
      NOUT=0
!
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
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y(I)
   56     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 65 I=1,NT
          WRITE(ICOUT,66)I,TVAL(I)
   66     FORMAT('I,TVAL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
!
!    ********************************************
!    **  STEP 1--                              **
!    **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!    ********************************************
!
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN THRESHOLD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
  115   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
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
        GO TO 9000
      ELSEIF(NT*N1.GT.MAXNXT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      THE NUMBER OF OBSERVATIONS TIMES THE NUMBER ',   &
               'OF THRESHOLD VALUES TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)NT
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,135)MAXNXT
  135   FORMAT('      THE MAXIMUM NUMBER OF OUTPUT VALUES ALLOWED IS ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ***************************************************
!     **  STEP 2--                                     **
!     **  GENERATE THE THRESHOLD VALUES                **
!     ***************************************************
!
      IF(ICASE.EQ.'MINI')THEN
        ICNT=0
        DO 1010 J=1,NT
          ACUT=TVAL(J)
          DO 1020 I=1,N1
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
        DO 2010 J=1,NT
          ACUT=TVAL(J)
          DO 2020 I=1,N1
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RESH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF THRESH--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,Y2(I),TAG(I)
 9016     FORMAT('I,Y2(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE THRESH
      SUBROUTINE TKTRBY(IBHIX,IBLOX,IBHIY,IBLOY,IFACTO,IX,IY)
!
!     PURPOSE--TRANSLATE 4 BYTES--
!              5 HIGH-ORDER BITS OF X
!              5 LOW -ORDER BITS OF X
!              5 HIGH-ORDER BITS OF Y
!              5 LOW -ORDER BITS OF Y
!              INTO 2 INTEGER TEKTRONIX COORDINATES
!              (0 TO 1023)
!
!     REFERENCE--TEKTRONIX 4014 TERMINAL USERS GUIDE, PAGE 3-31
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!
!-----NON-COMMON VARIABLES (GRAPHICS)----------------------------------
!
      CHARACTER*1 IBHIX
      CHARACTER*1 IBLOX
      CHARACTER*1 IBHIY
      CHARACTER*1 IBLOY
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRBY')GO TO 90
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
!
!CCCC IHIX=ICHAR(IBHIX)
      CALL DPCOAN(IBHIX,IHIX)
!CCCC ILOX=ICHAR(IBLOX)
      CALL DPCOAN(IBLOX,ILOX)
!CCCC IHIY=ICHAR(IBHIY)
      CALL DPCOAN(IBHIY,IHIY)
!CCCC ILOY=ICHAR(IBLOY)
      CALL DPCOAN(IBLOY,ILOY)
!
      IHIX2=MOD(IHIX,32)
      ILOX2=MOD(ILOX,32)
      IHIY2=MOD(IHIY,32)
      ILOY2=MOD(ILOY,32)
!
      IHIX3=IHIX2*32
      ILOX3=ILOX2
      IHIY3=IHIY2*32
      ILOY3=ILOY2
!
      IX3=IHIX3+ILOX3
      IY3=IHIY3+ILOY3
!
      IX4=IX3*4
      IY4=IY3*4
!
      IX=IX4/IFACTO
      IY=IY4/IFACTO
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRBY')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TKTRBY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IBHIX,IBLOX,IBHIY,IBLOY,IFACTO
 9021   FORMAT('IBHIX,IBLOX,IBHIY,IBLOY,IFACTO = ',4(A1,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IHIX,ILOX,IHIY,ILOY
 9031   FORMAT('IHIX,ILOX,IHIY,ILOY = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IHIX2,ILOX2,IHIY2,ILOY2
 9032   FORMAT('IHIX2,ILOX2,IHIY2,ILOY2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)IHIX3,ILOX3,IHIY3,ILOY3
 9033   FORMAT('IHIX3,ILOX3,IHIY3,ILOY3 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9034)IX3,IY3,IX4,IY4,IX,IY
 9034   FORMAT('IX3,IY3,IX4,IY4,IX,IY = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE TKTRBY
      SUBROUTINE TKTRPT(IXC,IYC,IFACTO,ICSTR,NCSTR,ISUBN0)
!
!     PURPOSE--TRANSLATE AN INTEGER PAIR OF COORDINATES
!              INTO A PACKED CHARACTER REPRESENTATION
!              THAT WILL BE UNDERSTOOD BY A TEKTRONIX
!              GRAPHICS DEVICE.
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
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED TKTRPT
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
!                 PHONE--301-921-3651
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
      DATA K2/4/
      DATA K5/32/
      DATA K7/128/
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
      IVX=IXC*IFACTO
      IVY=IYC*IFACTO
      IF(IVX.LT.0)IVX=0
      IF(IVY.LT.0)IVY=0
!
!               *******************************************************
!               **  STEP 1--                                         **
!               **  FORM THE HIGH Y 7-BIT BYTE--                     **
!               **  SHIFT THE Y VALUE TO THE RIGHT 7 PLACES;         **
!               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
!               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
!               *******************************************************
!
      NCSTR=NCSTR+1
      IBYTE1=MOD(IVY/K7,K5)+32
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE1)
      CALL DPCONA(IBYTE1,ICSTR(NCSTR:NCSTR))
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  FORM THE EXTRA 7-BIT BYTE--                          **
!               **  KEEP ONLY THE RIGHT 2 PLACES OF THE Y VALUE;         **
!               **  THEN SHIFT THESE 2 PLACES TO THE LEFT  2 PLACES;     **
!               **  KEEP ONLY THE RIGHT 2 PLACES OF THE X VALUE;         **
!               **  PLACE A 1 IN THE 6TH AND 7TH PLACES FFOM THE RIGHT.  **
!               **  THEN MERGE THE 2 TOGETHER.                         **
!               ***********************************************************
!
      NCSTR=NCSTR+1
      IBYTE2=MOD(IVY,K2)*K2+MOD(IVX,K2)+96
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE2)
      CALL DPCONA(IBYTE2,ICSTR(NCSTR:NCSTR))
!
!               ****************************************************************
!               **  STEP 3--                                                  **
!               **  FORM THE LOW Y 7-BIT BYTE--                               **
!               **  SHIFT THE Y VALUE TO THE RIGHT 2 PLACES;                  **
!               **  THEN KEEP ONLY THE RIGHT 5 PLACES;                        **
!               **  THEN PLACE A 1 IN THE 6TH AND 7TH PLACES FROM THE RIGHT.  **
!               ****************************************************************
!
      NCSTR=NCSTR+1
      IBYTE3=MOD(IVY/K2,K5)+96
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE3)
      CALL DPCONA(IBYTE3,ICSTR(NCSTR:NCSTR))
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  FORM THE HIGH X 7-BIT BYTE--                     **
!               **  SHIFT THE X VALUE TO THE RIGHT 7 PLACES;         **
!               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
!               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
!               *******************************************************
!
      NCSTR=NCSTR+1
      IBYTE4=MOD(IVX/K7,K5)+32
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE4)
      CALL DPCONA(IBYTE4,ICSTR(NCSTR:NCSTR))
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  FORM THE LOW X 7-BIT BYTE--                      **
!               **  SHIFT THE X VALUE TO THE RIGHT 2 PLACES;         **
!               **  THEN KEEP ONLY THE RIGHT 5 PLACES;               **
!               **  THEN PLACE A 1 IN THE 6TH PLACE FROM THE RIGHT.  **
!               *******************************************************
!
      NCSTR=NCSTR+1
      IBYTE5=MOD(IVX/K2,K5)+64
!CCCC ICSTR(NCSTR:NCSTR)=CHAR(IBYTE5)
      CALL DPCONA(IBYTE5,ICSTR(NCSTR:NCSTR))
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRPT')GO TO 9090
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
      IF(NCSTR.LE.0)GO TO 9027
      DO 9025 I=1,NCSTR
!CCCC IASCNE=ICHAR(ICSTR(I:I))
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
!
      RETURN
      END SUBROUTINE TKTRPT
      SUBROUTINE TNPCDF(X,GAMMA,A,NU,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
!              DISTRIBUTION WITH DOUBLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
!              A, AND UPPER TRUNCATION POINT NU.
!              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
!              CUMULATIVE DISTRIBUTION FUNCTION
!
!              F(X;GAMMA,A,NU) = 1 -
!                                A**GAMMA*(X**(-GAMMA)-NU**(-GAMMA))/
!                                (1 - (A/NU)**GAMMA)
!                                GAMMA > 0; 0 <= A <= X <= NU
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE
!                                AT WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE GREATER THAN
!                                OR EQUAL TO ALOC.
!                     --GAMMA  = THE DOUBLE PRECISION VALUE
!                                OF THE TAIL LENGTH PARAMETER.
!                     --A      = THE DOUBLE PRECISION VALUE
!                                OF THE LOWER BOUND PARAMETER.
!                     --NU     = THE DOUBLE PRECISION VALUE
!                                OF THE UPPER TRUNCATION PARAMETER.
!     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE UPPER-TRUNCATED PARETO
!             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
!             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
!                   NON-NEGATIVE.
!                 --X SHOULD BE GREATER THAN OR EQUAL TO A.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--MARCH     2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NU
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0D0
      IF(X.LT.A)THEN
        GO TO 9000
      ELSEIF(X.GE.NU)THEN
        CDF=1.0D0
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPCDF IS ',   &
             'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPCDF IS ',   &
             'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPCDF IS ',   &
             'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      DTERM1=(A**GAMMA)*(X**(-GAMMA) - NU**(-GAMMA))
      DTERM2=1.0D0 - (A/NU)**GAMMA
      CDF=1.0D0 - (DTERM1/DTERM2)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNPCDF
      DOUBLE PRECISION FUNCTION TNPFUN (GAMMAT,X)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE MAXIMUM LIKELIHOOD
!              ESTIMATE OF GAMMA FOR THE TRUNCATED PARETO
!              DISTRIBUTION.  IN PARTICULAR, IT SOLVES THE
!              EQUATION:
!
!              (R/GAMMAHAT) +
!              R*(X(R+1)/X(X(1))**GAMMAHAT*LOG(X(R+1)/X(1))/
!              1 - (X(R+1)/X(1))**GAMMAHAT) -
!              SUM[i=1 TO R][LOG(X(i) - LN(X(R+1))] = 0
!
!
!              CALLED BY DFZER2 ROUTINE FOR FINDING THE ROOT OF A
!              FUNCTION.
!
!     EXAMPLE--TRUNCATED PARETO MAXIMUM LIKELIHOOD Y
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/3
!     ORIGINAL VERSION--MARCH      2008.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION GAMMAT
      DOUBLE PRECISION X(*)
!
      DOUBLE PRECISION DMNMAX
      DOUBLE PRECISION DSUM
      COMMON/TNPCOM/DMNMAX,DSUM,IR2
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  COMPUTE SOME SUMS
!
      IF(ISUBG4.EQ.'PFUN')THEN
        WRITE(ICOUT,52)GAMMAT,X(1)
   52   FORMAT('GAMMAT,X(1) = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DTERM1=DLOG(DMNMAX)
      DR=DBLE(IR2)
      DTERM2=DR/GAMMAT
      DTERM3=DR*(DMNMAX**GAMMAT)*DTERM1/(1.0D0 - DMNMAX**GAMMAT)
      TNPFUN=DTERM2 + DTERM3 - DSUM
!
      RETURN
      END FUNCTION TNPFUN 
      SUBROUTINE TNPML1(Y,N,IR,DTEMP1,   &
                        XMEAN,XSD,XMIN,XMAX,   &
                        AML,ANUML,GAMMML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE TRUNCATED PARETO DISTRIBUTION FOR THE RAW DATA
!              CASE (I.E., NO CENSORING AND NO GROUPING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLTP WILL GENERATE THE OUTPUT
!              FOR THE TRUNCATED PARETO MLE COMMAND).
!
!              THE CONDITIONAL MAXIMUM LIKELIHOOD ESTIMATE OF
!              NU IS THE DATA MAXIMUM.
!
!              TO FIND THIS ESTIMATE, SORT THE DATA FROM LARGEST
!              TO SMALLEST VALUE.  IF THERE ARE R+1 POINTS, THE
!              MAXIMUM LIKELIHOOD ESTIMATE OF GAMMA IS THE SOLUTION
!              OF THE EQUATION
!
!              (R/GAMMAHAT) +
!              R*(X(R+1)/X(X(1))**GAMMAHAT*LOG(X(R+1)/X(1))/
!              1 - (X(R+1)/X(1))**GAMMAHAT) -
!              SUM[i=1 TO R][LOG(X(i) - LN(X(R+1))] = 0
!
!              THIS TERMINOLOGY IS USED BY ABAN, MEERSCHAERT, AND
!              PANORSKA.  THEY BASE THIS ON TAKING THE LARGEST
!              R+1 POINTS OUT OF N (I.E., THE TRUNCATED PARETO
!              IS FIT TO THE TAILS OF THE DATA).  IN DATAPLOT,
!              IF R IS SPECIFIED, IT IS ASUMED THAT WE ARE FITTING
!              THE ENTIRE DATA SET.  SO IN THE ABOVE FORMULA,
!              X(1) IS THE MAXIMUM AND X(R+1) IS THE MINIMUM
!              POINT INCLUDED IN THE COMPUTATION.
!
!
!              ONCE WE HAVE THE ESTIMATE OF GAMMA, THE ESTIMATE
!              OF THE LOWER BOUND PARAMETER IS:
!
!              AHAT = R**(1/GAMMAHAT)*(X(R+1))*
!              [N - (N - R)*(X(R+1_/X(1))**GAMMAHAT]**(-1/GAMMAHAT)
!
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/07
!     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLTP),
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
      DIMENSION Y(*)
      DOUBLE PRECISION DTEMP1(*)
!
      DOUBLE PRECISION TNPFUN
      EXTERNAL TNPFUN
!
      DOUBLE PRECISION DMNMAX
      DOUBLE PRECISION DSUM
      COMMON/TNPCOM/DMNMAX,DSUM,IR2
!
      DOUBLE PRECISION DAE
      DOUBLE PRECISION DRE
      DOUBLE PRECISION DXSTRT
      DOUBLE PRECISION DXLOW
      DOUBLE PRECISION DXUP
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TNPM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
      IRP1=0
!
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
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CARRY OUT CALCULATIONS                **
!               **  FOR TRUNCATED PARETO MLE ESTIMATE     **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AML=CPUMIN
      ANUML=CPUMIN
      GAMMML=CPUMIN
!
      IWRITE='OFF'
      IDIST='TRUNCATED PARETO'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      AN=REAL(N)
      IF(IR.LE.3 .OR. IR.GT.N)THEN
        IR=N-1
        IRP1=N
      ENDIF
      CALL SORT(Y,N,Y)
      N=IRP1
!
      DO 180 I=1,N
        DTEMP1(I)=DBLE(Y(I))
  180 CONTINUE
      DSUM=0.0D0
      DO 190 I=IR,1,-1
        DSUM=DSUM + (DLOG(DTEMP1(I)) - DLOG(DBLE(XMIN)))
  190 CONTINUE
      DMNMAX=DTEMP1(1)/DTEMP1(N)
      IR2=IR
!
      ANUML=XMAX
!
      DXSTRT=0.8D0
      DXLOW=0.001D0
      DXUP=10.0D0
      DAE=2.0*0.000001D0*DXSTRT
      DRE=DAE
      IFLAG=0
      ITBRAC=0
      CALL DFZER2(TNPFUN,DXLOW,DXUP,DXSTRT,DRE,DAE,IFLAG,DTEMP1)
      GAMMML=REAL(DXLOW)
!
      AR=REAL(IR)
      AMNMAX=REAL(DMNMAX)
      AML=AR**(1.0/GAMMML)*XMIN*   &
          (AN - (AN - AR)*(AMNMAX**GAMMML))**(-1.0/GAMMML)
!
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
!
      RETURN
      END SUBROUTINE TNPML1
      SUBROUTINE TNPPDF(X,GAMMA,A,NU,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
!              DISTRIBUTION WITH DOUBLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
!              A, AND UPPER TRUNCATION POINT NU.
!              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
!              PROBABILITY DENSITY FUNCTION
!
!              f(X;GAMMA,A,NU) = GAMMA*(A**GAMMA)*(X**(-GAMMA - 1))/
!                                (1 - (A/NU)**GAMMA)
!                                GAMMA > 0; 0 <= A <= X <= NU
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE
!                                AT WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE GREATER THAN
!                                OR EQUAL TO ALOC.
!                     --GAMMA  = THE DOUBLE PRECISION VALUE
!                                OF THE TAIL LENGTH PARAMETER.
!                     --A      = THE DOUBLE PRECISION VALUE
!                                OF THE LOWER BOUND PARAMETER.
!                     --NU     = THE DOUBLE PRECISION VALUE
!                                OF THE UPPER TRUNCATION PARAMETER.
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE UPPER-TRUNCATED PARETO
!             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
!             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
!                   NON-NEGATIVE.
!                 --X SHOULD BE GREATER THAN OR EQUAL TO A.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--MARCH     2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NU
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0D0
      IF(X.LT.A .OR. X.GT.NU)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNPPDF IS ',   &
               'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5)A,NU
    5   FORMAT('      INTERVAL (',G15.7,',',G15.7,').')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPPDF IS ',   &
             'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPPDF IS ',   &
             'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPPDF IS ',   &
             'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      DTERM1=GAMMA*(A**GAMMA)*(X**(-GAMMA-1.0D0))
      DTERM2=1.0D0 - (A/NU)**GAMMA
      PDF=DTERM1/DTERM2
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNPPDF
      SUBROUTINE TNPPPF(P,GAMMA,A,NU,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE UPPER-TRUNCATED PARETO
!              DISTRIBUTION WITH DOUBLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA, LOWER BOUND PARAMETER
!              A, AND UPPER TRUNCATION POINT NU.
!              THE UPPER-TRUNCATED PARETO DISTRIBUTION HAS THE
!              PERCENT POINT FUNCTION
!
!              G(P;GAMMA,A,NU) = [C2 + (C3/C1)*(1-P)]**(-1/GAMMA)
!                                GAMMA > 0; A >= 0; 0 <= P <= 1
!
!     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE DOUBLE PRECISION VALUE
!                                OF THE TAIL LENGTH PARAMETER.
!                     --A      = THE DOUBLE PRECISION VALUE
!                                OF THE LOWER BOUND PARAMETER.
!                     --NU     = THE DOUBLE PRECISION VALUE
!                                OF THE UPPER TRUNCATION PARAMETER.
!     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF FOR THE UPPER-TRUNCATED PARETO
!             DISTRIBUTION WITH TAIL LENGTH PARAMETER VALUE = GAMMA,
!             LOWER BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE AND A SHOULD BE
!                   NON-NEGATIVE.
!                 --0 <= P <= 1.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--MARCH     2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NU
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0D0
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNPPPF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPPPF IS ',   &
             'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.LT.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPPPF IS ',   &
             'NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(NU.LE.A)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO TNPPPF IS ',   &
             'LESS THAN OR EQUAL TO THE THIRD ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)A
   47   FORMAT('***** THE VALUE OF THE LOWER BOUND ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NU
   48   FORMAT('***** THE VALUE OF THE UPPER TRUNCATION ARGUMENT IS ',   &
               G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNPPPF
      SUBROUTINE TNPRAN(N,GAMMA,A,NU,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TRUNCATED PARETO DISTRIBUTION
!              WITH TAIL LENGTH PARAMETER VALUE = GAMMA, LOWER
!              BOUND PARAMETER A, AND UPPER TRUNCATION POINT NU.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
!                                TAIL LENGTH PARAMETER.
!                     --A      = THE SINGLE PRECISION VALUE
!                                OF THE LOWER BOUND PARAMETER.
!                     --NU     = THE SINGLE PRECISION VALUE OF THE
!                                UPPER TRUNCATION PARAMETER.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TRUNCATED PARETO DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER VALUE GAMMA, LOWER BOUND
!             PARAMETER VALUE A, AND UPPER TRUNCATION PARAMETER NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --GAMMA AND A SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TNPPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--ABAN, MEERSCHAERT, AND PANORSKA (2006), "PARAMETER
!                 ESTIMATION FOR THE TRUNCATED PARETO DISTRIBUTION",
!                 JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!                 VOL. 101, NO. 473, PP. 270-277.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008.3
!     ORIGINAL VERSION--MARCH     2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      REAL NU
      DOUBLE PRECISION DTEMP
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
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF TRUNCATED ',   &
               'PARETO RANDOM NUMBERS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNPRAN IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.LT.0.0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPRAN ',   &
               'IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.GE.NU)THEN
        WRITE(ICOUT,35)
   35   FORMAT('***** ERROR--THE THIRD ARGUMENT TO TNPRAN IS ',   &
               'GREATER THAN OR EQUAL TO THE FOURTH ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)NU
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   48 FORMAT('***** THE VALUE OF THE LOWER BOUND PARAMETER IS ',   &
             G15.7)
   49 FORMAT('***** THE VALUE OF THE UPPER TRUNCATION PARAMETER IS ',   &
             G15.7)
!
      CALL UNIRAN(N,ISEED,X)
!
      DO 100 I=1,N
        CALL TNPPPF(DBLE(X(I)),DBLE(GAMMA),DBLE(A),DBLE(NU),DTEMP)
        X(I)=REAL(DTEMP)
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNPRAN
      SUBROUTINE TNRCDF(DX,DA,DB,DU,DSD,DCDF)
!
!     NOTE--TRUNCATED-NORMAL PDF IS:
!              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
!                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
!           THE TNRCDF IS DEFINED FOR A<=X<=B
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/9
!     ORIGINAL VERSION--SEPTEMBER 1995.
!     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
!                                       DOUBLE PRECISION.
!     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
!                                       AS INDICATOR OF INFINITE
!                                       LIMIT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
      DOUBLE PRECISION CONST
      DOUBLE PRECISION ALL
      DOUBLE PRECISION AUL
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DCDF=0.0D0
!
      ALL=DA
      AUL=DB
      IF(DA.EQ.DBLE(CPUMIN) .AND. DB.EQ.DBLE(CPUMIN))THEN
        DARG1=(DX-DU)/DSD
        CALL NODCDF(DARG1,DCDF)
        GO TO 9999
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
!
      IF(DX.LE.ALL .AND. ALL.NE.DBLE(CPUMIN))THEN
        DCDF=0.0D0
        GO TO 9999
      ELSEIF(DX.GE.AUL .AND. AUL.NE.DBLE(CPUMIN))THEN
        DCDF=1.0D0
        GO TO 9999
      ENDIF
!
      DARG1=(DX-DU)/DSD
      CALL NODCDF(DARG1,DTERM1)
!
      IF(AUL.EQ.DBLE(CPUMIN))THEN
        DTERM3=1.0D0
      ELSE
        CALL NODCDF((DBLE(AUL)-DU)/DSD,DTERM3)
      ENDIF
!
      IF(ALL.EQ.DBLE(CPUMIN))THEN
        DTERM2=0.0D0
      ELSE
        CALL NODCDF((DBLE(ALL)-DU)/DSD,DTERM2)
      ENDIF
!
      CONST=1.0D0/(DTERM3-DTERM2)
      DCDF=(DTERM1 - DTERM2)/(DTERM3 - DTERM2)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNRCDF
      DOUBLE PRECISION FUNCTION TNRFUN(XI)
!
!     PURPOSE--FOR A SINGLY TRUNCATED NORMAL DISTRIBUTION,
!              THE MAXIMUM LIKELIHOOD ESTIMATES OF THE MU
!              AND SIGMA PARAMETERS ARE:
!
!                  SIGMAHAT = SQRT{S**2 + lambda(h,alphahat)*(XBAR - T)**2}
!                  MUHAT    = XBAR - lambda(h,alphahat)*(XBAR - T)
!
!              WHERE
!
!                   alphahat = S**2/(XBAR - T)**2
!                   h        = c/N
!                   N        = TOTAL NUMBER OF OBSERVATIONS
!                   n        = NUMBER OF NON-TRUNCATED OBSERVATIONS
!                   c        = NUMBER OF TRUNCATED OBSERVATIONS
!
!               XBAR AND S ARE THE MEAN AND SD OF THE NON-TRUNCATED
!               OBSERVATIONS.
!
!               LAMBDA(H,ALPHAHAT) IS A TABULATED VALUE IN THE
!               COHEN REFERENCE.  HOWEVER, WE DETERMINE IT BY
!               SOLVING THE FUNCTION
!
!                  ((1 - OMEGA(h,XI)*(OMEGA(h,XI) - XI))/
!                  (OMEGA(h,XI) - XI)**2) - S**2/(MU - T)**2
!
!               FOR XI WHERE
!
!                  OMEGA(h,XI) = (h/(1-h))*NORPDF(XI)/NORCDF(XI)
!
!               NOTE THAT XI IS THE STANDARDIZED TRUNCATION
!               POINT.  ONCE WE SOLVE FOR XI, WE PLUG IT INTO
!               THE FUNCTION
!
!                   LAMBDA = OMEGA(h,XI)/(OMEGA(h,XI) - XI)
!
!               NOTE THAT THERE MAY BE TWO SOLUTIONS TO THIS
!               EQUATION.  WE PICK THE ONE THAT RESULTS IN A
!               POSITIVE LAMBDA.
!
!               THIS FUNCTION IS USED IN SOLVING THE
!               LAMBDA(h,XI) FUNCTION FOR XI.
!
!     REFERENCE--CLIFFORD COHEN (1991), "TRUNCATED AND CENSORED
!                SAMPLES", MARCEL DEKKER INC., CHAPTER 2.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/12
!     ORIGINAL VERSION--DECEMBER  2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION XI
!
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DENOM
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DCDF
!
      DOUBLE PRECISION DH
      DOUBLE PRECISION DC1
      COMMON/TNRCOM/DC1,DH
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CALL NODPDF(XI,DPDF)
      CALL NODCDF(XI,DCDF)
      DOMEGA=(DH/(1.0D0-DH))*DPDF/DCDF
      DNUM=1.0D0 - DOMEGA*(DOMEGA - XI)
      DENOM=(DOMEGA - XI)**2
!
      TNRFUN=(DNUM/DENOM) - DC1
!
      RETURN
      END FUNCTION TNRFUN
      SUBROUTINE TNRPDF(DX,DA,DB,DU,DSD,DPDF)
!
!     NOTE--TRUNCATED-NORMAL PDF IS:
!              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
!                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
!           THE TNRPDF IS DEFINED FOR A<=X<=B
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
!                                       DOUBLE PRECISION.
!     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
!                                       AS INDICATOR OF INFINITE
!                                       LIMIT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DPDF=0.0D0
!
      ALL=DA
      AUL=DB
      IF(DA.EQ.DBLE(CPUMIN) .AND. DB.EQ.DBLE(CPUMIN))THEN
        DARG1=(DX-DU)/DSD
        CALL NODPDF(DARG1,DTERM1)
        DPDF=DTERM1/DSD
        GO TO 9999
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
!
      IF((DX.LT.ALL.AND.ALL.NE.DBLE(CPUMIN)) .OR.   &
         (DX.GT.AUL.AND.AUL.NE.DBLE(CPUMIN)))THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)DA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)DB
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNRPDF IS OUTSIDE ',   &
             'THE ALLOWABLE (A,B) INTERVAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF A IS ',G15.7)
   48 FORMAT('***** THE VALUE OF B IS ',G15.7)
!
      DARG1=(DX-DU)/DSD
      CALL NODPDF(DARG1,DTERM1)
      DTERM1=DTERM1/DSD
!
      IF(AUL.EQ.DBLE(CPUMIN))THEN
        DTERM2=1.0D0
      ELSE
        CALL NODCDF((AUL-DU)/DSD,DTERM2)
      ENDIF
!
      IF(ALL.EQ.DBLE(CPUMIN))THEN
        DTERM3=0.0D0
      ELSE
        CALL NODCDF((ALL-DU)/DSD,DTERM3)
      ENDIF
!
      DPDF=DTERM1/(DTERM2-DTERM3)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNRPDF
      SUBROUTINE TNRPPF(P,A,B,U,SD,PPF)
!
!     PURPOSE   --PERCENT POINT FUNCTION FOR THE TRUNCATED NORMAL
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
!     VERSION NUMBER--95/9
!     ORIGINAL VERSION--SEPTEMBER 1995.
!     UPDATED         --DECEMBER  2008. PERFORM COMPUTATIONS IN
!                                       DOUBLE PRECISION.
!     UPDATED         --DECEMBER  2008. CHANGE FROM -99.9 TO CPUMIN
!                                       AS INDICATOR OF INFINITE
!                                       LIMIT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION P
      DOUBLE PRECISION P1
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION U
      DOUBLE PRECISION SD
      DOUBLE PRECISION PPF
!CCCC DOUBLE PRECISION DPI
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
!
      CHARACTER*4 IDIR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA DPI /3.14159265358979D0/
      DATA EPS /0.00000001D0/
      DATA SIG /1.0D-9/
      DATA ZERO /0.D0/
      DATA MAXIT /10000/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      XINC=0.0
      XL=0.0
      XR=0.0
      ALL=A
      AUL=B
      IF(A.LE.DBLE(CPUMIN+1.0) .AND. B.LE.DBLE(CPUMIN+1.0))THEN
        CALL NODPPF(P,PPF)
        PPF=U + PPF/SD
        GO TO 9999
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
!
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
!
      IF(IFLAG.EQ.1)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GO TO 9999
      ELSEIF(SD.LE.0.0)THEN
        WRITE(ICOUT,35)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GO TO 9999
      ENDIF
!
    1 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNRPPF IS OUTSIDE ',   &
             ' THE ALLOWABLE (0,1) INTERVAL.')
   35 FORMAT('***** ERROR--THE FIFTH ARGUMENT TO TNRPPF IS NEGATIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!  FIND BRACKETING INTERVAL.
!  AFTER SUCCESSFULLY FIND BRACKETING INTERVAL, THEN SWITCH TO
!  MORE EFFICIENT BISECTION METHOD.
!
      MAXIT=1000
      IF(ALL.GT.DBLE(CPUMIN+1.0) .AND. AUL.GT.DBLE(CPUMIN+1.0))THEN
        XL=ALL
        XR=AUL
        IF(P.LE.0.0D0)THEN
          PPF=ALL
          GO TO 9999
        ELSEIF(P.GE.1.0D0)THEN
          PPF=AUL
          GO TO 9999
        ENDIF
        GO TO 99
      ELSEIF(ALL.GT.DBLE(CPUMIN+100.0))THEN
        XL=ALL
        XR=U
        XINC=SD
        IF(P.LE.0.0D0)THEN
          PPF=ALL
          GO TO 9999
        ENDIF
        IDIR='RIGH'
      ELSEIF(AUL.GT.DBLE(CPUMIN+1.0))THEN
        XR=AUL
        XL=U
        XINC=SD
        IF(P.GE.1.0D0)THEN
          PPF=AUL
          GO TO 9999
        ENDIF
        IDIR='LEFT'
      ENDIF
!
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
        GO TO 99
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
        GO TO 9999
      ENDIF
   96 FORMAT('***** ERROR--TNRPPF UNABLE TO FIND BRACKETING ',   &
             'INTERVAL.')
      GO TO 91
!
!  BISECTION METHOD
!
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
  130 FORMAT('***** ERROR--TNRPPF ROUTINE DID NOT CONVERGE.')
      GO TO 9999
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNRPPF
      SUBROUTINE TNRRAN(N,A,B,U,SD,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE THE TRUNCATED NORMAL (GAUSSIAN)
!              DISTRIBUTION WITH MEAN = U AND STANDARD DEVIATION = SD.
!              THE TRUNCATED-NORMAL PDF IS:
!              TNRPDF(X,A,B,U,S) = (1/S)*NORPDF((X-U)/S)/
!                                    [NORCDF((B-U)/S)-NORCDF((A-U)/S)]
!              THE TNRPDF IS DEFINED FOR A<=X<=B
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                       A      = A SINGLE PRECISION SCALAR THAT
!                                DEFINES THE LOWER TRUNCATION POINT.
!                       B      = A SINGLE PRECISION SCALAR THAT
!                                DEFINES THE UPPER TRUNCATION POINT.
!                       U      = A SINGLE PRECISION SCALAR THAT
!                                DEFINES THE LOCATION PARAMETER FOR
!                                THE PARENT NORMAL DISTRIBUTION.
!                       SD     = A SINGLE PRECISION SCALAR THAT
!                                DEFINES THE SCALE PARAMETER FOR
!                                THE PARENT NORMAL DISTRIBUTION.
!                       ISEED  = AN INTEGER NUMBER THAT DEFINES THE
!                                SEED FOR THE UNIFORM RANDOM NUMBER
!                                GENERATOR.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TRUNCATED NORMAL DISTRIBUTION.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG, SQRT, SIN, COS.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     METHOD--BOX-MULLER ALGORITHM USED TO GENERATE NORMAL RANDOM
!             NUMBERS, THEN REJECT IF GENERATED NUMBER OUTSIDE THE
!             TRUNCATION POINT.
!     REFERENCES--BOX AND MULLER, 'A NOTE ON THE GENERATION
!                 OF RANDOM NORMAL DEVIATES', JOURNAL OF THE
!                 ASSOCIATION FOR COMPUTING MACHINERY, 1958,
!                 PAGES 610-611.
!               --TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 33-34.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 39.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--2003.7
!     ORIGINAL VERSION--JULY      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(2)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.14159265359/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
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
        GO TO 9000
      ENDIF
   15 FORMAT('***** ERROR--THE REQUESTED STANDARD DEVIATION OF')
   16 FORMAT('      THE TRUNCATED NORMAL RANDOM NUMBERS IS ',   &
             'NON-POSITIVE.')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.7)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!     THEN GENERATE 2 ADDITIONAL UNIFORM (0,1) RANDOM NUMBERS
!     (TO BE USED BELOW IN FORMING THE N-TH NORMAL
!     RANDOM NUMBER WHEN THE DESIRED SAMPLE SIZE N
!     HAPPENS TO BE ODD).
!
      CALL UNIRAN(2,ISEED,Y)
!
      NTEMP=2
      I=0
      INC=1
  100 CONTINUE
        I=I+INC
        IF(I.GT.N)GO TO 9000
!
          CALL UNIRAN(NTEMP,ISEED,X(I))
!
!         GENERATE NORMAL RANDOM NUMBERS USING THE BOX-MULLER METHOD.
!
          IP1=I+1
          U1=X(I)
          IF(I.EQ.N)GO TO 210
          U2=X(IP1)
          GO TO 220
  210     U2=Y(2)
  220     ARG1=-2.0*LOG(U1)
          ARG2=2.0*PI*U2
          SQRT1=SQRT(ARG1)
          Z1=SQRT1*COS(ARG2)
          Z2=SQRT1*SIN(ARG2)
          Z1=U + SD*Z1
          Z2=U + SD*Z2
!
!         REJECT IF OUTSIDE THE BOUNDS
!
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
          GO TO 100
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNRRAN
      SUBROUTINE TNECDF(X,X0,U,SD,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
!              WITH MEAN = 0 AND STANDARD DEVIATION = 1.
!              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
!              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S))  U<=X<=X0
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --X0     = SINGLE PRECISION VALUE DEFINING THE
!                                TRUNCATION POINT.
!                     --U      = SINGLE PRECISION VALUE DEFINING THE
!                                MEAN OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!                     --SD     = SINGLE PRECISION VALUE DEFINING THE
!                                SD OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2899
!     ORIGINAL VERSION--OCTOBER   1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DX0
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9999
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
        GO TO 9999
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9999
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9999
      ENDIF
    4 FORMAT('***** WARNING--THE FIRST  INPUT ARGUMENT TO ',   &
      'THE TNECDF ROUTINE IS')
    5 FORMAT('      OUTSIDE THE (U,X0) INTERVAL')
   14 FORMAT('***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO ',   &
      'THE TNECDF ROUTINE IS')
   15 FORMAT('      EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE THIRD INPUT ARGUMENT TO ',   &
      'THE TNECDF ROUTINE IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE FOURTH INPUT ARGUMENT TO ',   &
      'THE TNECDF ROUTINE IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF X0 IS ',E15.8,' *****')
   48 FORMAT('***** THE VALUE OF U IS ',E15.8,' *****')
!
!-----START POINT-----------------------------------------------------
!
      DX=DBLE((X-U)/SD)
      DX0=DBLE((X0-U)/SD)
      DTERM1=DEXP(DX)*(DEXP(-DX0)-1.0D0)
      DCDF=1.0D0/DTERM1
      ARG1=U
      CALL TNEPDF(ARG1,X0,U,SD,ARG2)
      CDF=SNGL(DCDF)+SD*ARG2
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNECDF
      SUBROUTINE TNEPDF(X,X0,U,SD,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
!              WITH LOCATION = U AND SCALE = SD.
!              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
!              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --X0     = SINGLE PRECISION VALUE DEFINING THE
!                                TRUNCATION POINT.
!                     --U      = SINGLE PRECISION VALUE DEFINING THE
!                                MEAN OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!                     --SD     = SINGLE PRECISION VALUE DEFINING THE
!                                SD OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2899
!     ORIGINAL VERSION--OCTOBER   1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DX0
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9999
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
        GO TO 9999
      ENDIF
      IF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
      IF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
    4 FORMAT('***** FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO ',   &
      'THE TNEPDF ROUTINE IS')
    5 FORMAT('      OUTSIDE THE (U,X0) INTERVAL')
   14 FORMAT('***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO ',   &
      'THE TNEPDF ROUTINE IS')
   15 FORMAT('      EITHER NON-POSITIVE OR LESS THAN U.')
   24 FORMAT('***** FATAL DIAGNOSTIC--THE THIRD INPUT ARGUMENT TO ',   &
      'THE TNEPDF ROUTINE IS NEGATIVE.')
   34 FORMAT('***** FATAL DIAGNOSTIC--THE FOURTH INPUT ARGUMENT TO ',   &
      'THE TNEPDF ROUTINE IS NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF X0 IS ',E15.8,' *****')
   48 FORMAT('***** THE VALUE OF U IS ',E15.8,' *****')
!
!-----START POINT-----------------------------------------------------
!
      DX=DBLE((X-U)/SD)
      DX0=DBLE((X0-U)/SD)
      DTERM1=-DX - DLOG(1.0D0-DEXP(-DX0)) - DLOG(DBLE(SD))
      DPDF=DEXP(DTERM1)
      PDF=SNGL(DPDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNEPDF
      SUBROUTINE TNEPPF(P,X0,U,SD,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TRUNCATED EXPONENTIAL DISTRIBUTION
!              THIS DISTRIBUTION IS DEFINED FOR U<=X<=X0
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --X0     = SINGLE PRECISION VALUE DEFINING THE
!                                TRUNCATION POINT.
!                     --U      = SINGLE PRECISION VALUE DEFINING THE
!                                MEAN OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!                     --SD     = SINGLE PRECISION VALUE DEFINING THE
!                                SD OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0 (INCLUSIVELY)
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CHAPTER 19.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--95/10
!     ORIGINAL VERSION--OCTOBER   1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC DOUBLE PRECISION DP
!CCCC DOUBLE PRECISION DX0
!CCCC DOUBLE PRECISION DPPF
!CCCC DOUBLE PRECISION DTERM1
!CCCC DOUBLE PRECISION DTERM2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA EPS /1.0E-6/
      DATA SIG /1.0E-5/
      DATA ZERO /0./
      DATA MAXIT /2000/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0.OR.P.GT.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TNEPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TNEPPF ',   &
              'IS EITHER NON-POSITIVE OR LESS THAN U.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
   48   FORMAT('***** THE VALUE OF U IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
   24   FORMAT('***** ERROR--THE THIRD ARGUMENT TO ',   &
               'THE TNEPPF ROUTINE IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)U
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
   34   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO ',   &
               'THE TNEPPF ROUTINE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)SD
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      IF(P.EQ.0.0)THEN
        PPF=U
        GO TO 9999
      ELSEIF(P.EQ.1.0)THEN
        PPF=X0
        GO TO 9999
      ENDIF
!
!CCCC CALL TNEPDF(U,X0,U,SD,ARG2)
!CCCC DTERM1=DBLE(SD)*DBLE(ARG2)
!CCCC DP=DBLE(P)
!CCCC DX0=DBLE((X0-U)/SD)
!CCCC DTERM2=DEXP(-DX0)-1.0D0
!CCCC DTERM3=1.0D0/(DTERM2*(DP-DTERM1))
!CCCC IF(DTERM3.GT.0.0D0)THEN
!CCCC   DPPF=DLOG(1.0D0/(DTERM2*(DP-DTERM1)))
!CCCC ELSE
!CCCC   DPPF=0.0
!CCCC ENDIF
!CCCC PPF=U + S*SNGL(DPPF)
!
      IERR=0
      IC = 0
      XL = U
      XR = X0
      FXL = -P
      FXR = 1.0 - P
!CCCC INVALID P EXPLICITLY CHECKED FOR EARLIER.
!CCCC IF(FXL*FXR .GT. ZERO)GO TO 50
!
!  BISECTION METHOD
!
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL TNECDF(X,X0,U,SD,P1)
      PPF=X
!CCCC IF(IERR.NE.0)THEN
!CCCC   WRITE(ICOUT,120)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC ENDIF
!C120 FORMAT('***** ERROR--ERROR IN BETCDF ROUTINE.  *****')
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
  130 FORMAT('***** ERROR--TNEPPF ROUTINE DID NOT CONVERGE.')
      GO TO 9999
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TNEPPF
      SUBROUTINE TNERAN(N,X0,U,SD,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE THE TRUNCATED EXPONENTIAL DISTRIBUTION
!              WITH LOCATION = U AND SCALE = SD.
!              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X,
!              LESS THAN OR EQUAL TO THE TRUNCATION VALUE X0.
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!              F(X) = EXP(-(X-U)/S)/(S*(1-EXP(-(X0-U)/S)).
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --X0     = SINGLE PRECISION VALUE DEFINING THE
!                                TRUNCATION POINT.
!                     --U      = SINGLE PRECISION VALUE DEFINING THE
!                                MEAN OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!                     --SD     = SINGLE PRECISION VALUE DEFINING THE
!                                SD OF THE PARENT EXPONENTIAL
!                                DISTRIBUTION
!                       ISEED  = AN INTEGER NUMBER THAT DEFINES THE
!                                SEED FOR THE UNIFORM RANDOM NUMBER
!                                GENERATOR.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TRUNCATED EXPONENTIAL DISTRIBUTION.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     METHOD--BOX-MULLER ALGORITHM USED TO GENERATE NORMAL RANDOM
!             NUMBERS, THEN REJECT IF GENERATED NUMBER OUTSIDE THE
!             TRUNCATION POINT.
!     REFERENCES--TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 33-34.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 39.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 1970, PAGES 207-232.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 58.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--2003.7
!     ORIGINAL VERSION--JULY      2003.
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
!-----DATA STATEMENTS-------------------------------------------------
!
!CCCC DATA PI/3.14159265359/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF TRUNCATED ',   &
               'EXPONENTIAL RANDOM NUMBERS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(X0.LT.AMAX1(0.0,U))THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE TRUNCATION PARAMETER FOR THE ',   &
               'THE TRUNCATED EXPONENTIAL DISTRIBUTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)
   15   FORMAT('      IS EITHER NON-POSITIVE OR LESS THAN U.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X0
   46   FORMAT('***** THE VALUE OF THE TRUNCATION PARAMETER IS ',   &
               G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)U
   48   FORMAT('***** THE VALUE OF THE LOCATION PARAMETER IS ',   &
               G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(U.LT.0.0)THEN
        WRITE(ICOUT,24)
   24   FORMAT('***** ERROR--THE LOCATION PARAMETER FOR THE ',   &
               'TRUNCATED EXPONENTIAL DISTRIBUTION IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)U
   49   FORMAT('***** THE VALUE OF THE PARAMETER IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(SD.LE.0.0)THEN
        WRITE(ICOUT,34)
   34   FORMAT('***** ERROR--THE SCALE PARAMETER FOR THE ',   &
               'TRUNCATED EXPONENTIAL DISTRIBUTION IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,49)SD
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!     THEN GENERATE 2 ADDITIONAL UNIFORM (0,1) RANDOM NUMBERS
!     (TO BE USED BELOW IN FORMING THE N-TH NORMAL
!     RANDOM NUMBER WHEN THE DESIRED SAMPLE SIZE N
!     HAPPENS TO BE ODD).
!
!
      NTEMP=1
      I=0
  100 CONTINUE
        I=I+1
        IF(I.GT.N)GO TO 9000
  199   CONTINUE
!
        CALL UNIRAN(NTEMP,ISEED,X(I))
        X(I)=-LOG(X(I))
        X(I)=U + SD*X(I)
        IF(X(I).GT.X0)GO TO 199
      GO TO 100
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TNERAN
      SUBROUTINE TOL(X,N,XMEAN,XSD,AN,   &
                     X2TEMP,Y2TEMP,D2TEMP,   &
                     ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                     PID,IVARID,IVARI2,NREPL,   &
                     ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES NORMAL AND
!              DISTRIBUTION-FREE TOLERANCE LIMITS
!              FOR THE DATA IN THE INPUT VECTOR X.
!              15 NORMAL TOLERANCE LIMITS ARE COMPUTED; AND
!              30 DISTRIBUTION-FREE TOLERANCE LIMITS ARE COMPUTED.
!     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED OR SORTED) OBSERVATIONS.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     OUTPUT--2 PAGES OF AUTOMATIC PRINTOUT--
!             1 PAGE GIVING NORMAL TOLERANCE LIMITS; AND
!             1 PAGE GIVING DISTRIBUTION-FREE TOLERANCE LIMITS.
!     PRINTING--YES.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, LOG, EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--GARDINER AND HULL (1966), "AN APPROXIMATION TO
!                 TWO-SIDED TOLERANCE LIMITS FOR NORMAL POPULATIONS",
!                 TECHNOMETRICS, 1966, PAGES 115-122.
!               --WILKS, ANNALS OF MATHEMATICAL STATISTICS, 1941, PAGE 92
!               --MOOD AND GRABLE, PAGES 416-417
!               --HOWE (1969), "TWO-SIDED TOLERANCE LIMITS FOR NORMAL
!                 POPULATIONS - SOME IMPROVEMENTS", JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION, VOL. 64, PP.
!                 610-620.
!               --GUENTHER (1977), "SAMPLING INSPECTION IN STATISTICAL
!                 QUALITY CONTROL", GRIFFIN'S STATISTICAL MONOGRAPHS,
!                 NUMBER 37, LONDON.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --NOVEMBER  1998. CHANGES TO INCORPORATE INTO
!                                       DATAPLOT
!     UPDATED         --DECEMBER  2005. OPTIONALLY SELECT WHETHER
!                                       NORMAL/NON-PARAMETERIC CASES
!                                       PERFORMED
!     UPDATED         --MARCH     2011. USE DPDTA1 AND DPDTA5 TO PRINT
!                                       TABLES
!     UPDATED         --MAY       2014. SUPPORT FOR LOGNORMAL TOLERANCE
!                                       LIMITS
!     UPDATED         --MAY       2014. SUPPORT FOR "BOX-COX" TOLERANCE
!                                       LIMITS
!     UPDATED         --MAY       2018. SUPPORT FOR HOWE APPROXIMATION
!     UPDATED         --MAY       2018. SUPPORT FOR INDPENDENT DEGREES
!                                       OF FREEDOM
!     UPDATED         --MAY       2018. CORRECT LABELING OF CONFIDENCE
!                                       AND COVERAGE IN THE TABLE
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION X2TEMP(*)
      DIMENSION Y2TEMP(*)
      DIMENSION D2TEMP(*)
      DIMENSION PID(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      PARAMETER (NCONF=6)
      PARAMETER (NCOV=3)
!
      DIMENSION PA(NCONF),PC(NCOV)
      DIMENSION RSMALL(5,6),USMALL(6,6)
      DIMENSION A(NCONF),B(NCONF),C(NCONF)
      DIMENSION Z1(NCOV)
      DIMENSION TMIN(NCOV,NCONF),TMAX(NCOV,NCONF),TK(NCOV,NCONF)
      DIMENSION P(10),C1(10),C2(10),C3(10)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      DATA PA/50.,75.,90.,95.,99.,99.9/
      DATA PC/90.,95.,99./
      DATA Z1(1),Z1(2),Z1(3)/-1.28155157,-1.64485363,-2.32634787/
      DATA A/.6745,1.1504,1.6449,1.9600,2.5758,3.2905/
      DATA B/.33734,.57335,.82140,.97910,1.2889,1.64038/
      DATA C/-0.15460,-0.02991,.22044,.40675,.85514,1.42601/
      DATA RSMALL(1,1),RSMALL(1,2),RSMALL(1,3),RSMALL(1,4),RSMALL(1,5),   &
      RSMALL(1,6)            /1.0505,1.6859,2.2844,2.6463,3.3266,4.0903/
      DATA RSMALL(2,1),RSMALL(2,2),RSMALL(2,3),RSMALL(2,4),RSMALL(2,5),   &
      RSMALL(2,6)            /0.8557,1.4333,2.0078,2.3624,3.0368,3.7983/
      DATA RSMALL(3,1),RSMALL(3,2),RSMALL(3,3),RSMALL(3,4),RSMALL(3,5),   &
      RSMALL(3,6)            /0.7929,1.3412,1.8979,2.2457,2.9128,3.6708/
      DATA RSMALL(4,1),RSMALL(4,2),RSMALL(4,3),RSMALL(4,4),RSMALL(4,5),   &
      RSMALL(4,6)            /0.7622,1.2940,1.8388,2.1815,2.8422,3.5965/
      DATA RSMALL(5,1),RSMALL(5,2),RSMALL(5,3),RSMALL(5,4),RSMALL(5,5),   &
      RSMALL(5,6)            /0.7442,1.2654,1.8019,2.1408,2.7963,3.5472/
      DATA USMALL(1,1),USMALL(1,2),USMALL(1,3)/0.,0.,0./
      DATA USMALL(2,1),USMALL(2,2),USMALL(2,3)/7.9579,15.9472,79.7863/
      DATA USMALL(3,1),USMALL(3,2),USMALL(3,3)/3.0808,4.4154,9.9749/
      DATA USMALL(4,1),USMALL(4,2),USMALL(4,3)/2.2658,2.9200,5.1113/
      DATA USMALL(5,1),USMALL(5,2),USMALL(5,3)/1.9393,2.3724,3.6692/
      DATA USMALL(6,1),USMALL(6,2),USMALL(6,3)/1.7621,2.0893,3.0034/
      DATA P/50.,75.,90.,95.,97.5,99.,99.5,99.9,99.95,99.99/
!
      ISUBN1='TOL '
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
!
      CCORG=CPUMIN
      CCMAX=CPUMIN
      ALOC=CPUMIN
      ALAMBA=CPUMIN
!
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
          DO 56 I=1,N
            WRITE(ICOUT,57)I,X(I)
   57       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(XMEAN.EQ.CPUMIN .AND. N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.',   &
               '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(XMEAN.EQ.CPUMIN)THEN
        HOLD=X(1)
        DO 135 I=2,N
          IF(X(I).NE.HOLD)GO TO 139
  135   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,131)HOLD
  131   FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        GO TO 9000
  139   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
!               **  LIMITS.                               **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     MAY 2014: FOR LOGNORMAL TOLERANCE LIMITS, TAKE THE LOG OF THE
!               DATA, COMPUTE NORMAL TOLERANCE LIMITS, AND THEN
!               TAKE EXPONENT FOR THE LIMITS.
!
!               ALSO CAN DO A BOX-COX NORMALITY TRANSFORMATION.
!
!               NOTE THAT WE ONLY DO THIS FOR THE RAW DATA CASE.
!
      IF(ICASAN.EQ.'LNTO' .AND. XMEAN.EQ.CPUMIN)THEN
        DO 210 I=1,N
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
            GO TO 9000
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
  216   FORMAT('      LOGNORMAL TOLERANCE LIMITS NOT SUPPORTED FOR ',   &
               'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.EQ.CPUMIN)THEN
!
!       NORMAL ORDER STATISTIC MEDIANS
!
        CALL UNIMED(N,X2TEMP)
        DO 231 I=1,N
          CALL NORPPF(X2TEMP(I),X2OUT)
          X2TEMP(I)=X2OUT
  231   CONTINUE
!
!       COMPUTE PPCC VALUE FOR UNTRANSFORMED DATA AND
!       RETURN CRITICAL VALUE FOR 3 <= N <= 1,000.
!
        CALL SORT(X,N,D2TEMP)
        AN1=N
        SUMY=0.0
        DO 233 I=1,N
          SUMY=SUMY+D2TEMP(I)
  233   CONTINUE
        XBAR=0.0
        YBAR=SUMY/AN1
!
        SUMX=0.0
        SUMY=0.0
        SUMXY=0.0
        DO 234 I=1,N
          SUMX=SUMX+(X2TEMP(I)-XBAR)*(X2TEMP(I)-XBAR)
          SUMY=SUMY+(D2TEMP(I)-YBAR)*(D2TEMP(I)-YBAR)
          SUMXY=SUMXY+(X2TEMP(I)-XBAR)*(D2TEMP(I)-YBAR)
  234   CONTINUE
        ARG=SUMX*SUMY
        CCORG=0.0
        IF(ARG.GT.0.0)CCORG=SUMXY/SQRT(ARG)
!
        IF(N.GE.3 .AND. N.LE.1000)THEN
          ALPHAT=0.05
          CALL NORPCV(ALPHAT,N,CV,   &
                      ISUBRO,IBUGA3,IERROR)
        ELSE
          CV=CPUMIN
        ENDIF
!
!       SORT DATA AND SHIFT DATA TO MAKE IT POSITIVE
!
!CCCC   CALL SORT(X,N,D2TEMP)
        XMIN=D2TEMP(1)
        IF(XMIN.LE.0.0)THEN
          DO 236 I=1,N
            D2TEMP(I)=D2TEMP(I)-XMIN+1.0
  236     CONTINUE
          ALOC=XMIN-1.0
        ELSE
          ALOC=0.0
        ENDIF
!
        ALAMBA=-2.0
        ALAMSV=ALAMBA
        AINC=0.1
        CCMAX=CPUMIN
!
        DO 240 IDIS=1,41
!
!         LOG TRANSFORMATION
!
          IF(-0.001.LE.ALAMBA.AND.ALAMBA.LE.0.001)THEN
            ALAMBA=0.0
            DO 241 I=1,N
              Y2TEMP(I)=LOG(D2TEMP(I))
  241       CONTINUE
          ELSE
            DO 246 I=1,N
              Y2TEMP(I)=((D2TEMP(I)**ALAMBA)-1.0)/ALAMBA
  246       CONTINUE
          ENDIF
!
          AN1=N
          SUMY=0.0
          DO 251 I=1,N
            SUMY=SUMY+Y2TEMP(I)
  251     CONTINUE
          XBAR=0.0
          YBAR=SUMY/AN1
!
          SUMX=0.0
          SUMY=0.0
          SUMXY=0.0
          DO 252 I=1,N
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
!
          ALAMBA=ALAMBA + AINC
!
  240   CONTINUE
!
!       NOW TRANSFORM THE DATA
!
        ALAMBA=ALAMSV
        IF(ALAMBA.EQ.0.0)THEN
          DO 261 I=1,N
            X(I)=LOG(D2TEMP(I))
  261     CONTINUE
        ELSE
          DO 263 I=1,N
            X(I)=((D2TEMP(I)**ALAMBA)-1.0)/ALAMBA
  263     CONTINUE
        ENDIF
!
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.NE.CPUMIN)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,276)
  276   FORMAT('      BOX-COX TOLERANCE LIMITS NOT SUPPORTED FOR ',   &
               'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.NE.CPUMIN)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,278)
  278   FORMAT('      BOX-COX TOLERANCE LIMITS NOT SUPPORTED FOR ',   &
               'SUMMARY DATA CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     COMPUTE NORMAL TOLERANCE LIMITS
!
      IF(XMEAN.EQ.CPUMIN)THEN
        AN=N
        CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
        CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      ELSE
        N=INT(AN+0.1)
      ENDIF
!
!     COMPUTE THE NORMAL TOLERANCE LIMITS
!
      IF(ICASAN.EQ.'TOLE' .OR. ICASAN.EQ.'NTOL' .OR.   &
         ICASAN.EQ.'LNTO' .OR. ICASAN.EQ.'BCTO')THEN
!
        IF(ITOLME.EQ.'HOWE')THEN
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
            WRITE(ICOUT,801)
  801       FORMAT('HOWE METHOD')
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
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
!
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,902)I,J,NU,TERM1,Z,AVAL,TERM3,TERM2
  902             FORMAT('I,J,NU,TERM1,Z,AVAL,TERM3,TERM2 = ',   &
                         3I5,5G15.7)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
!
!               APPLY GUENTHER CORRECTION IF REQUESTED
!
                IF(ITOLGC.EQ.'ON')THEN
                  ANUM=AN - 3.0 - TERM3
                  DENOM=2.0*(AN+1)**2
                  TERM4=SQRT(1.0 + (ANUM/DENOM))
                  AK=TERM4*AK
                ENDIF
!
                TMIN(I,J)=XMEAN - AK*XSD
                TMAX(I,J)=XMEAN + AK*XSD
                TK(I,J)=AK
!
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,910)I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),   &
                                  TMAX(I,J)
  910             FORMAT('I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),TMAX(I,J)=',   &
                         2I5,2F8.3,3G15.7)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
!
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
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
            WRITE(ICOUT,301)
  301       FORMAT('GARDINER AND HULL METHOD')
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
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
              D6=(12.0*Z**6-243.0*Z**4-923.0*Z**2+1472.0)/   &
                 (25515.0*F**3.0)
              D7=(3753.0*Z**7+4353.0*Z**5-289517.0*Z**3-289717.0*Z)/   &
                 (9185400.0*SQRT(2.0)*F**3.5)
              UNIV=D1+D2+D3-D4+D5+D6-D7
              U=1.0/UNIV
              U=SQRT(U)
            ENDIF
!
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
!
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')THEN
                  WRITE(ICOUT,910)I,J,PCOV,PCONF,TK(I,J),TMIN(I,J),   &
                                  TMAX(I,J)
                  CALL DPWRST('XXX','WRIT')
                ENDIF
!
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
!
        IF(ICASAN.EQ.'LNTO' .AND. XMEAN.EQ.CPUMIN)THEN
          DO 410 I=1,N
            X(I)=EXP(X(I))
  410     CONTINUE
        ELSEIF(ICASAN.EQ.'BCTO' .AND. XMEAN.EQ.CPUMIN)THEN
          IF(ALAMBA.EQ.0.0)THEN
            DO 420 I=1,N
              X(I)=EXP(D2TEMP(I))
  420       CONTINUE
          ELSE
            DO 430 I=1,N
              X(I)=(1.0 + ALAMBA*D2TEMP(I))**(1.0/ALAMBA)
  430       CONTINUE
          ENDIF
        ENDIF
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING FOR  **
!               **   NORMAL TOLERANCE LIMITS   **
!               *********************************
!
        ISTEPN='42'
        IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
!
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        IF(NREPL.GT.0)THEN
          IADD=1
          DO 4101 I=1,NREPL
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Summary Statistics:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
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
!
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
!
        NUMROW=ICNT
        DO 4020 I=1,NUMROW
          NTOT(I)=15
 4020   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        ISTEPN='42A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        ISTEPN='42D'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Coverage = 90%'
        NCTITL=14
!
        ITITL2(1,1)='Confidence'
        NCTIT2(1,1)=10
        ITITL2(2,1)='Value (%)'
        NCTIT2(2,1)=9
!
        ITITL2(1,2)='k'
        NCTIT2(1,2)=1
        ITITL2(2,2)='Factor'
        NCTIT2(2,2)=6
!
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
        ITITL2(2,3)='Limit'
        NCTIT2(2,3)=5
!
        ITITL2(1,4)='Upper'
        NCTIT2(1,4)=5
        ITITL2(2,4)='Limit'
        NCTIT2(2,4)=5
!
        NUMLIN=2
        NUMCOL=4
        NUMROW=NCONF
        NMAX=0
        DO 4221 I=1,NUMCOL
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
        DO 4223 I=1,NUMROW
          DO 4225 J=1,NUMCOL
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
!
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
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
        ITITLE='Coverage = 95%'
        NCTITL=14
!
        DO 4233 I=1,NCONF
          AMAT(I,2)=TK(2,I)
          AMAT(I,3)=TMIN(2,I)
          AMAT(I,4)=TMAX(2,I)
 4233   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
        ITITLE='Coverage = 99%'
        NCTITL=14
!
        DO 4243 I=1,NCONF
          AMAT(I,2)=TK(3,I)
          AMAT(I,3)=TMIN(3,I)
          AMAT(I,4)=TMAX(3,I)
 4243   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ENDIF
!
      IF(ICASAN.EQ.'TOLE' .OR. ICASAN.EQ.'NPTO')THEN
!
!       COMPUTE DISTRIBUTION-FREE TOLERANCE LIMITS
!
        K=N/2
        NUMSEC=3
        IF(K.LT.NUMSEC)NUMSEC=K
!
!       DETERMINE THE SMALLEST 3 AND LARGEST 3 OBSERVATIONS
!
        CALL SORT(X,N,X)
        LOCMIN=1
        XMIN=X(1)
        XMIN2=X(2)
        XMIN3=X(3)
        XMAX=X(N)
        XMAX2=X(N-1)
        XMAX3=X(N-2)
!
        AN=REAL(N)
        AN1=AN-1.0
        AN2=AN-2.0
        AN3=AN-3.0
        AN4=AN-4.0
        AN5=AN-5.0
        AN6=AN-6.0
!
        DO 1600 I=1,10
          D=P(I)/100.0
          C1(I)=(D**AN1)*(-AN + AN1*D)
          C1(I)=1.0-C1(I)
          Q=1.0-D
          T=Q*AN
          C1(I)=1.0+AN1*Q
          C1(I)=1.0-(D**AN1)*C1(I)
          C1(I)=C1(I)*100.0
          IF(NUMSEC.EQ.1)GO TO 1600
          A0=6.0*D*D*D
          A1=2.0-7.0*D+11.0*D*D
          A2=-3.0+6.0*D
          A3=1.0
          C2(I)=A0+A1*T+A2*T*T+A3*T*T*T
          C2(I)=1.0-(D**AN3)*C2(I)/6.0
          C2(I)=C2(I)*100.0
          IF(NUMSEC.EQ.2)GO TO 1600
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
!
!               ****************************************
!               **   STEP 52--                        **
!               **   WRITE OUT EVERYTHING FOR         **
!               **   NONPARAMETRIC TOLERANCE LIMITS   **
!               ****************************************
!
        ISTEPN='52'
        IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL ')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
        ITITLE='Two-Sided Distribution-Free Tolerance Limits'
        NCTITL=44
        ITITLZ=' '
        NCTITZ=0
!
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        IF(NREPL.GT.0)THEN
          IADD=1
          DO 5101 I=1,NREPL
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Summary Statistics:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
        NUMROW=ICNT
        DO 5020 I=1,NUMROW
          NTOT(I)=15
 5020   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        ISTEPN='42A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        ISTEPN='42D'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        NUMROW=10
        NUMCOL=2
        NUMLIN=2
        NMAX=0
!
        ITITL9(1:17)='Involving X(3) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN3
        ITITL9(33:56)='     Involving X(N-2) = '
        WRITE(ITITL9(57:71),'(G15.7)')XMAX3
        NCTIT9=71
        ITITLE=' '
        NCTITL=0
!
        ITITL2(1,1)='Confidence'
        NCTIT2(1,1)=10
        ITITL2(2,1)='Value (%)'
        NCTIT2(2,1)=9
!
        ITITL2(1,2)='Coverage'
        NCTIT2(1,2)=8
        ITITL2(2,2)='Value (%)'
        NCTIT2(2,2)=9
!
        NMAX=0
        DO 5221 I=1,NUMCOL
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
        DO 5223 I=1,NUMROW
          DO 5225 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5225     CONTINUE
          AMAT(I,1)=C3(I)
          AMAT(I,2)=P(I)
 5223   CONTINUE
!
        IWRTF(1)=1600
        IWRTF(2)=IWRTF(1)+2000
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        IF(NUMSEC.GE.3)THEN
!
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGA3,IERROR)
!
        ENDIF
!
        ITITL9(1:17)='Involving X(2) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN2
        ITITL9(33:56)='     Involving X(N-1) = '
        WRITE(ITITL9(57:71),'(G15.7)')XMAX2
        NCTIT9=71
        ITITLE=' '
        NCTITL=0
!
        NMAX=0
        DO 5321 I=1,NUMCOL
          NTOT(I)=15
          IF(I.EQ.1)THEN
            NTOT(I)=12
          ENDIF
          NMAX=NMAX+NTOT(I)
 5321   CONTINUE
        DO 5323 I=1,NUMROW
          DO 5325 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5325     CONTINUE
          AMAT(I,1)=C2(I)
          AMAT(I,2)=P(I)
 5323   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        IF(NUMSEC.GE.2)THEN
!
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGA3,IERROR)
!
        ENDIF
!
        ITITL9(1:17)='Involving X(1) = '
        WRITE(ITITL9(18:32),'(G15.7)')XMIN
        ITITL9(33:54)='     Involving X(N) = '
        WRITE(ITITL9(55:69),'(G15.7)')XMAX
        NCTIT9=69
        ITITLE=' '
        NCTITL=0
!
        NMAX=0
        DO 5421 I=1,NUMCOL
          NTOT(I)=15
          IF(I.EQ.1)THEN
            NTOT(I)=12
          ENDIF
          NMAX=NMAX+NTOT(I)
 5421   CONTINUE
        DO 5423 I=1,NUMROW
          DO 5425 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            AMAT(I,J)=0.0
 5425     CONTINUE
          AMAT(I,1)=C1(I)
          AMAT(I,2)=P(I)
 5423   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOL
      SUBROUTINE TOL2(X,N,XMEAN,XSD,AN,   &
                      ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES ONE-SIDED NORMAL TOLERANCE
!              LIMITS FOR THE DATA IN THE INPUT VECTOR X.
!              15 NORMAL TOLERANCE LIMITS ARE COMPUTED.
!     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED OR SORTED) OBSERVATIONS.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     OUTPUT--PAGE GIVING NORMAL TOLERANCE LIMITS.
!     PRINTING--YES.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--MARY NATRELLA (1963), "EXPERIMENTAL STATISTICS, NBS
!                 HANDBOOK 91", US DEPARTMENT OF COMMERCE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     ORIGINAL VERSION--AUGUST    2011.
!     UPDATED         --SEPTEMBER 2011. FIX BUG IN 99% CONFIDENCE
!                                       INTERVAL TABLE
!     UPDATED         --MAY       2018. ALLOW USER SPECIFIED DEGREES
!                                       OF FREEDOM
!     UPDATED         --MAY       2018. SUPPORT EITHER NORMAL OR
!                                       NON-CENTRAL T METHOD
!     UPDATED         --MAY       2018. CORRECT LABELING OF CONFIDENCE
!                                       AND COVERAGE IN THE TABLE
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION PID(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      PARAMETER (NCOV=3)
      PARAMETER (NCONF=6)
      DIMENSION PA(NCONF)
      DIMENSION PC(NCOV)
      DIMENSION TMIN(NCOV,NCONF)
      DIMENSION TK(NCOV,NCONF)
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IMETH
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA PA/50.,75.,90.,95.,99.,99.9/
      DATA PC/90.,95.,99./
!
      ISUBN1='TOL2'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
!
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
          DO 56 I=1,N
            WRITE(ICOUT,57)I,X(I)
   57       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(XMEAN.EQ.CPUMIN .AND. N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: ONE SIDED TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.',   &
               '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(XMEAN.EQ.CPUMIN)THEN
        HOLD=X(1)
        DO 135 I=2,N
          IF(X(I).NE.HOLD)GO TO 139
  135   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,131)HOLD
  131   FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        GO TO 9000
  139   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
!               **  LIMITS.                               **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE NORMAL TOLERANCE LIMITS
!
      IF(XMEAN.EQ.CPUMIN)THEN
        AN=N
        CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
        CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      ELSE
        N=INT(AN+0.1)
      ENDIF
!
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
!
!     COMPUTE THE NORMAL TOLERANCE LIMITS
!
      ICASPL='1LNT'
      DO 300 I=1,NCOV
        GAMMA=PC(I)/100.0
        DO 400 J=1,NCONF
          ALPHA=PA(J)/100.0
          CALL DPTOL3(X,N,XMEAN,XSD,AN,PTOLDF,   &
                      ICASPL,ALPHA,GAMMA,ITOLGC,IMETH,   &
                      AK,ALOWLM,AUPPLM,   &
                      ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          TK(I,J)=AK
          IF(ICASAN.EQ.'LOWE')THEN
            TMIN(I,J)=ALOWLM
          ELSE
            TMIN(I,J)=AUPPLM
          ENDIF
  400   CONTINUE
  300 CONTINUE
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING FOR  **
!               **   NORMAL TOLERANCE LIMITS   **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(NREPL.GT.0)THEN
        IADD=1
        DO 4101 I=1,NREPL
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      NUMROW=ICNT
      DO 4020 I=1,NUMROW
        NTOT(I)=15
 4020 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Coverage = 90%'
      NCTITL=14
!
      ITITL2(1,1)='Confidence'
      NCTIT2(1,1)=10
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
!
      ITITL2(1,2)='k'
      NCTIT2(1,2)=1
      ITITL2(2,2)='Factor'
      NCTIT2(2,2)=6
!
      IF(ICASAN.EQ.'LOWE')THEN
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
      ELSE
        ITITL2(1,3)='Upper'
        NCTIT2(1,3)=5
      ENDIF
      ITITL2(2,3)='Limit'
      NCTIT2(2,3)=5
!
      NUMLIN=2
      NUMCOL=3
      NUMROW=NCONF
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4225   CONTINUE
        JCNT=1
        AMAT(I,1)=PA(I)
        AMAT(I,2)=TK(1,I)
        AMAT(I,3)=TMIN(1,I)
 4223 CONTINUE
!
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE='Coverage = 95%'
      NCTITL=14
!
      DO 4233 I=1,NCONF
        AMAT(I,2)=TK(2,I)
        AMAT(I,3)=TMIN(2,I)
 4233 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE='Coverage = 99%'
      NCTITL=14
!
!CCCC 2011/09: CORRECT FOLLOWING LINE
      DO 4243 I=1,NCONF
        AMAT(I,2)=TK(3,I)
        AMAT(I,3)=TMIN(3,I)
 4243 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOL2
      SUBROUTINE TOLWEI(X,N,   &
                        MINMAX,IWEIBC,XTEMP,DTEMP,   &
                        ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES ONE-SIDED WEIBULL TOLERANCE
!              LIMITS FOR THE DATA IN THE INPUT VECTOR X.
!              BASE TOLERANCE LIMITS ON FACT THAT ONE-SIDED TOLERANCE
!              LIMITS ARE EQUIVALENT TO LOWER CONFIDENCE INTERVALS ON
!              PERCENTILES.
!     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED OR SORTED) OBSERVATIONS.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     OUTPUT--PAGE GIVING NORMAL TOLERANCE LIMITS.
!     PRINTING--YES.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCE--KARL BURY, "STATISTICAL DISTRIBUTIONS IN
!                ENGINEERING", CAMBRIDGE UNIVERSITY PRESS,
!                1999, CHAPTER 17.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     ORIGINAL VERSION--AUGUST    2011.
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION PID(*)
      DIMENSION XTEMP(*)
      DOUBLE PRECISION DTEMP(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 IWEIBC
      CHARACTER*4 IWEIFL
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      PARAMETER (NCOV=6)
      PARAMETER (NCONF=3)
      DIMENSION PA(NCONF)
      DIMENSION PC(NCOV)
      DIMENSION TMIN(NCOV,NCONF)
      DIMENSION TMAX(NCOV,NCONF)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA PC/50.,75.,90.,95.,99.,99.9/
      DATA PA/90.,95.,99./
!
      ISUBN1='TOLW'
      ISUBN2='EI  '
      IWRITE='OFF'
      IERROR='NO'
      IWEIFL='WEIB'
!
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
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: ONE SIDED WEIBULL TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3.',   &
               '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
        IF(X(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  139 CONTINUE
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CARRY OUT CALCULATIONS FOR TOLERANCE  **
!               **  LIMITS.                               **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE 2-PARAMETER WEIBULL TOLERANCE LIMITS
!
!     STEP 1: OBTAIN POINT ESTIMATES AND STANDARD ERRORS
!
      AN=REAL(N)
      CALL WEIML1(X,N,IWEIBC,IWEIFL,MINMAX,   &
                  XTEMP,DTEMP,   &
                  XMEAN,XSD,XVAR,XMIN,XMAX,   &
                  ZMEAN,ZSD,   &
                  SCALE,SCALSE,GAMMA,GAMMSE,   &
                  GAMMBC,GABCSE,COVSE,COBCSE,   &
                  ISUBRO,IBUGA3,IERROR)
       IF(IERROR.EQ.'YES')GO TO 9000
!
!     STEP 2: FOR VARIOUS VALUES OF CONFIDENCE AND COVERAGE, COMPUTE
!             THE ONE-SIDED CONFIDENCE CONFIDENCE INTERVAL FOR THE
!             PERCENTILE (= THE ONE-SIDED TOLERANCE INTERVAL).
!
!             1. STANDARD ERROR USES TECHNIQUE DEMONSTRATED IN EXAMPLE 17.4
!                (P. 344) OF BURY.  THIS IS BASED ON PROPOGATION OF ERROR.
!
!             2. CONFIDENCE INTERVAL IS THEN GENERATED USING NORMAL
!                APPROXIMATION (EXAMPLE 17.7 OF BURY).  BURY ALSO DEMONSTRATES
!                A LIKELIHOOD RATIO APPROACH, BUT OMIT THIS FOR NOW.
!
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
!
      DO 300 I=1,NCONF
        ALPHA=PA(I)/100.0
        DO 400 J=1,NCOV
          GCOV=PC(J)/100.0
!
          IF(ICASAN.EQ.'LOWE' .OR. ICASAN.EQ.'TWOS')THEN
!CCCC       ALPHL=1.0 - ALPHA
            ALPHL=ALPHA
            IF(ICASAN.EQ.'TWOS')THEN
              ALPHL=1.0 - ALPHA
              ALPHL=1.0 - (ALPHL/2.0)
            ENDIF
            CALL NORPPF(ALPHL,Z95)
            QPTEMP=1.0 - GCOV
            CALL WEIPPF(QPTEMP,G,MINMAX,APPF)
            XQPHAT=SCALE*APPF
!
            C=LOG(1.0/(1.0 - QPTEMP))
            DA=C**(1.0/G)
            DB=-(SCALE*C**(1.0/G)*LOG(C)/(G**2))
            TERM1=(DA*SCALSE)**2
            TERM2=(DB*GSE)**2
            TERM3=2.0*DA*DB*COV*COV
            SEXQP=SQRT(TERM1 + TERM2 + TERM3)
            TMIN(J,I)=XQPHAT - Z95*SEXQP
!
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
!
          ELSEIF(ICASAN.EQ.'UPPE' .OR. ICASAN.EQ.'TWOS')THEN
            ALPHU=ALPHA
            IF(ICASAN.EQ.'TWOS')ALPHU=1.0 - ((1.0 - ALPHU)/2.0)
            CALL NORPPF(ALPHU,Z95)
            QPTEMP=GAMMA
            CALL WEIPPF(QPTEMP,G,MINMAX,APPF)
            XQPHAT=SCALE*APPF
!
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
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING FOR  **
!               **   WEIBULL TOLERANCE LIMITS  **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'LWEI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(NREPL.GT.0)THEN
        IADD=1
        DO 4101 I=1,NREPL
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      NUMROW=ICNT
      DO 4020 I=1,NUMROW
        NTOT(I)=15
 4020 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LWEI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LWEI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Confidence = 90%'
      NCTITL=16
!
      ITITL2(1,1)='Coverage'
      NCTIT2(1,1)=8
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
!
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
!
      NUMLIN=2
      NUMCOL=ICOL
      NUMROW=NCOV
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
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
!
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
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE='Confidence = 95%'
      NCTITL=16
!
      DO 4233 I=1,NCOV
        IF(ICASAN.EQ.'LOWE')THEN
          AMAT(I,2)=TMIN(I,2)
        ELSEIF(ICASAN.EQ.'UPPE')THEN
          AMAT(I,2)=TMAX(I,2)
        ELSE
          AMAT(I,2)=TMIN(I,2)
          AMAT(I,3)=TMAX(I,2)
        ENDIF
 4233 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE='Confidence = 99%'
      NCTITL=16
!
      DO 4243 I=1,NCONF
        IF(ICASAN.EQ.'LOWE')THEN
          AMAT(I,2)=TMIN(I,3)
        ELSEIF(ICASAN.EQ.'UPPE')THEN
          AMAT(I,2)=TMAX(I,3)
        ELSE
          AMAT(I,2)=TMIN(I,3)
          AMAT(I,3)=TMAX(I,3)
        ENDIF
 4243 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOLWEI
      SUBROUTINE TOPCDF(X,BETA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
!
!                  F(X;BETA) = (2*X - X**2)**BETA
!                              0 <= X <= 1, BETA > 0
!
!              WITH BETA DENOTING THE SHAPE PARAMETER.
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, PP. 33-43.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION BETA
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(X.LT.0.0D0)THEN
        CDF=0.0D0
        GO TO 9000
      ELSEIF(X.GT.1.0D0)THEN
        CDF=1.0D0
!CCCC   WRITE(ICOUT,2)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,46)X
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   CDF=0.0D0
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GO TO 9000
      ENDIF
!CCC2 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPCDF IS ',
!CCCC1       'OUTSIDE THE (0,1) INTERVAL.')
   12 FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPCDF IS ',   &
             'NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LE.0.0D0)THEN
        CDF=0.0D0
      ELSEIF(X.GE.1.0D0)THEN
        CDF=1.0D0
      ELSE
        DTERM1=BETA*DLOG(2.0D0*X - X**2)
        CDF=DEXP(DTERM1)
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOPCDF
      SUBROUTINE TOPLI1(Y,N,TEMP1,SHAPE,YLOWLM,YUPPLM,   &
                        ALIK,AIC,AICC,BIC,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE LOG-LIKELIHOOD FUNCTION FOR
!              THE TOPP AND LEONE DISTRIBUTION.  THIS IS FOR THE RAW
!              DATA CASE (I.E., NO GROUPING AND NO CENSORING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!     REFERENCE --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATION", WORLD SCIENTIFIC, PP. 199-201.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/6
!     ORIGINAL VERSION--JUNE      2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
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
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TOPM'
      ISUBN2='L1  '
      IERROR='NO'
!
      ALIK=CPUMIN
      AIC=CPUMIN
      AICC=CPUMIN
      BIC=CPUMIN
!
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WLI1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
!
!     NOTE: WE ARE COMPUTING THE LOG-LIKELIHOOD FUNCTION FOR THE
!           STANDARD CASE (I.E., DATA ASSUMED TO BE IN (0,1)
!           INTERVAL).  IF DATA IS NOT IN THIS INTERVAL, THEN WE
!           NEED TO SCALE IT BASED ON THE VALUES OF XMIN AND XMAX.
!
!           IN ORDER TO KEEP LIKELIHOOD ON SAME SCALE AS
!           ORIGINAL DATA, ACCOMODATE LOWER/UPPER LIMIT
!           PARAMETERS.  USE
!
!              f(x;shape,loc,scale) = f((x-loc)/scale;shape,0,1)/scale
!
!           SO CAN COMPUTE THE LOG LIKELIHOOD ON THE TRANSFORMED
!           DATA, BUT SUBTRACT THE TERM
!
!              N*LOG(SCALE)
!
      ZLOC=YLOWLM
      ZSCALE=YUPPLM - YLOWLM
      DO 2110 I=1,N
        TEMP1(I)=(Y(I) - ZLOC)/ZSCALE
 2110 CONTINUE
!
!     FOR THE TOPP AND LEONE FUNCTION, THE LOG-LIKLIHOOD FUNCTION IS:
!
!     N*LOG(BETA) + SUM[LOG(2*X(i)(] +
!     (BETA - 1)*SUM[LOG(2*X(i) - X(i)**2)]
!
      DEPS=1.0D-12
      DN=DBLE(N)
      DBETA=DBLE(SHAPE)
      DTERM1=DN*DLOG(DBETA) - DN*DLOG(DBLE(ZSCALE))
      DTERM2=DBETA - 1.0D0
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 1000 I=1,N
        DX=DBLE(TEMP1(I))
        DSUM1=DSUM1 + DLOG(2.0D0 - 2.0D0*DX)
        DSUM2=DSUM2 + DLOG(2.0D0*DX - DX**2)
 1000 CONTINUE
!
      DLIK=DTERM1 + DSUM1 + DTERM2*DSUM2
      ALIK=REAL(DLIK)
      DNP=1.0D0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
!
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
!
      RETURN
      END SUBROUTINE TOPLI1
      SUBROUTINE TOPML1(Y,N,A,B,   &
                        XMIN,XMAX,XMEAN,XSD,   &
                        SHAPML,ZLOC,ZSCALE,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD
!              ESTIMATES FOR THE TOPP AND LEONE DISTRIBUTION
!
!              THE MAXIMUM LIKELIHOOD ESTIMATE OF BETA IS:
!
!                  BETAHAT = N/[SUM[i=1 to N]
!                            [LOG(1/(2*X(i)- X(i)**2))]
!
!     EXAMPLE--TOPP AND LEONE MAXIMUM LIKELIHOOD Y
!     REFERENCE --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH
!                 BOUNDED SUPPORT AND APPLICATIONS", WORLD
!                 SCIENTIFIC PUBLISHING CO., PP. 33-43.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/07
!     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLTO)
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
      INTEGER IFLAG
!
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
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
      ISUBN1='TOPM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
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
!               **  FOR TOPP AND LEONE MLE ESTIMATE     **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='TOPP AND LEONE'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CALL SORT(Y,N,Y)
      ZLOC=0.0
      ZSCALE=1.0
      EPS=1.0E-6
!
      IF(A.NE.CPUMIN .AND. A.LT.XMIN .AND. B.NE.CPUMIN .AND.   &
         B.GT.XMAX)THEN
        ZLOC=A
        ZSCALE=B-A
        EPS=0.0
      ELSEIF(XMIN.LE.0.0 .OR. XMAX.GE.1.0)THEN
!CCCC   EPS=(XMAX - XMIN)*0.0001
        EPS=(XMAX - XMIN)*0.01
        ZLOC=XMIN - EPS
        ZMAX=XMAX + EPS
        ZSCALE=ZMAX - ZLOC
!CCCC   ZLOC=XMIN
!CCCC   ZSCALE=XMAX - ZLOC
      ELSE
        EPS=0.0
      ENDIF
!
      DO 2120 I=1,N
        Y(I)=(Y(I) - ZLOC)/ZSCALE
 2120 CONTINUE
!
      DSUM=0.0D0
      DO 2200 J=1,N
        DX=Y(J)
        IF(DX.GT.0.0D0)THEN
          DSUM=DSUM + DLOG(1.0D0/(2.0D0*DX - DX**2))
        ENDIF
 2200 CONTINUE
      SHAPML=REAL(DBLE(N)/DSUM)
!
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
!
      RETURN
      END SUBROUTINE TOPML1
      SUBROUTINE TOPPDF(X,BETA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!                  f(X;BETA) = BETA*(2-2*X)*(2*X-X**2)**(BETA-1)
!                              0 <= X <= 1, BETA > 0
!
!              WITH BETA DENOTING THE SHAPE PARAMETER.
!
!              THIS DISTRIBUTION HAS MOMENTS:
!
!              MEAN = 1 - 4**BETA*GAMMA(BETA+1)*GAMMA(BETA+1)/
!                     GAMMA(2*BETA+2)
!              VARIANCE = 2**(2*BETA+1)*B(BETA+1,BETA+1)*
!                         {1 - 2**(2*BETA-1)*B(BETA+1,BETA+1)} -
!                         2**(2*BETA+3)*B(BETA+2,BETA+1)*
!                         BETAI(0.5,BETA+2,BETA+1)
!
!              WITH B AND BETAI DENOTING THE BETA AND INCOMPLETE
!              BETA FUNCTIONS, RESPECTIVELY.
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, PP. 33-43.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PDF
      DOUBLE PRECISION DTERM1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(X.LT.0.0D0 .OR. X.GT.1.0D0)THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
      ELSEIF(BETA.LT.1.0D0 .AND. X.LE.0.0D0)THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
      ENDIF
    3 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPPDF IS ',   &
             'OUTSIDE THE (0,1) INTERVAL.')
   12 FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPPDF IS ',   &
             'NON-POSITIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      IF(X.GE.1.0D0)THEN
        PDF=0.0D0
      ELSEIF(X.LE.0.0D0)THEN
        IF(BETA.EQ.1.0D0)THEN
          PDF=2.0D0
        ELSEIF(BETA.LT.1.0D0)THEN
          PDF=0.0D0
        ELSE
          DTERM1=DLOG(BETA) + DLOG(2.0D0 - 2.0D0*X) +   &
                 (BETA-1.0D0)*DLOG(1.0D0*X - X**2)
          PDF=DEXP(DTERM1)
        ENDIF
      ELSE
        DTERM1=DLOG(BETA) + DLOG(2.0D0 - 2.0D0*X) +   &
               (BETA-1.0D0)*DLOG(2.0D0*X - X**2)
        PDF=DEXP(DTERM1)
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOPPDF
      SUBROUTINE TOPPPF(P,BETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TOP AND LEONE DISTRIBUTION.
!              THE PERCENT POINT FUNCTION IS:
!
!                  G(P;BETA) = 1 - SQRT(1 - P**(1/BETA))
!                              0 <= P <= 1, BETA > 0
!
!              WITH BETA DENOTING THE SHAPE PARAMETER.
!
!     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, PP. 33-43.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION P
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PPF
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0D0
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TOPPPF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TOPPPF IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      IF(P.LE.0.0D0)THEN
        PPF=0.0D0
      ELSEIF(P.GE.1.0D0)THEN
        PPF=1.0D0
      ELSE
        PPF=1.0D0 - DSQRT(1.0D0 - P**(1.0D0/BETA))
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOPPPF
      SUBROUTINE TOPRAN(N,BETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TOPP AND LEONE DISTRIBUTION
!              WITH SHAPE PARAMETER  BETA.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!                  f(X;BETA) = BETA*(2-2*X)*(2*X-X**2)**(BETA-1)
!                              0 <= X <= 1, BETA > 0
!
!              WITH BETA DENOTING THE SHAPE PARAMETER.
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER BETA.
!                                BETA SHOULD BE IN THE RANGE (0,1).
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TOPP AND LEONE DISTRIBUTION
!             WITH SHAPE PARAMETER BETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TOPPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, PP. 33-43.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHMOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.2
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION BETA
      DOUBLE PRECISION DTEMP
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
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',   &
      'TOPP AND LEONE RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
      IF(BETA.LE.0.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  201 FORMAT('***** ERROR--THE BETA SHAPE PARAMETER IS ',   &
             'NON-POSITIVE.')
  203 FORMAT('      THE VALUE OF BETA IS ',G15.7)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TOPP AND LEONE DISTRIBUTION RANDOM
!     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 300 I=1,N
        ZTEMP=X(I)
        CALL TOPPPF(DBLE(ZTEMP),BETA,DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TOPRAN
      SUBROUTINE TPDF(X,ANU,PDF)
!CCCC SUBROUTINE TPDF(X,NU,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!              THIS DISTRIBUTION IS DEFINED FOR ALL X.
!              THE PROBABILITY DENSITY FUNCTION IS GIVEN
!              IN THE REFERENCES BELOW.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --NU     = THE INTEGER NUMBER OF DEGREES
!                                OF FREEDOM.
!                                NU SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
!                 SERIES 55, 1964, PAGE 948, FORMULAE 26.7.3 AND 26.7.4.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 94-129.
!               --FEDERIGHI, EXTENDED TABLES OF THE
!                 PERCENTAGE POINTS OF STUDENT'S
!                 T-DISTRIBUTION, JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION,
!                 1959, PAGES 683-688.
!               --OWEN, HANDBOOK OF STATISTICAL TABLES,
!                 1962, PAGES 27-30.
!               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
!                 FOR STATISTICIANS, VOLUME 1, 1954,
!                 PAGES 132-134.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--AUGUST    1977.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
!                                       DEGREES OF FREEDOM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      EXTERNAL DLNGAM
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA DPI   / 3.14159265358979D+00/
      DATA DSQTPI/1.77245385090552D0/
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GO TO 8000
!
      IF(NU.LE.0)THEN
        WRITE(ICOUT,115)
  115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',   &
               'TO TPDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,147)NU
  147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
!
!               ****************************************************************
!               **  STEP 2--
!               **  COMPUTE THE CONSTANT = 1/(SQRT(NU)*BETA(1/2,NU/2))
!               **  = (1/(SQRT(NU)*SQRT(PI))) * (GAMMA((NU/2)+(1/2))/GAMMA(NU/2)
!               ****************************************************************
!
      DX=X
      DNU=NU
!
      DRATIO=1.0D0
      IEVODD=NU-2*(NU/2)
      IMIN=3
      IF(IEVODD.EQ.0)IMIN=2
      IF(NU.LT.IMIN)GO TO 250
      DO 300 I=IMIN,NU,2
      AI=I
      DRATIO=((AI-1.0D0)/AI)*DRATIO
  300 CONTINUE
  250 CONTINUE
      DRATIO=DRATIO*DNU
      IF(IEVODD.EQ.0)GO TO 260
      DRATIO=DRATIO/DSQTPI
      GO TO 400
  260 CONTINUE
      DRATIO=DRATIO*DSQTPI/2.0D0
  400 CONTINUE
!
      DCONST=DRATIO/(DSQTPI*DSQRT(DNU))
!
!               ************************************
!               **  STEP 3--                      **
!               **  COMPUTE THE DENSITY FUNCTION  **
!               ************************************
!
      DPOWER=-(DNU+1.0D0)/2.0D0
      PDF=DCONST*((1.0D0+DX*DX/DNU)**DPOWER)
      GO TO 9000
!
!CCCC OCTOBER 2006: FRACTIONAL DEGREES OF FREEDOM CASE.
!
 8000 CONTINUE
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,8115)
 8115   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT ',   &
               'TO TPDF IS NON-POSITIVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8147)ANU
 8147   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DNU=DBLE(ANU)
!
      DTERM1=(DNU+1.0D0)/2.0D0
      DNUM=DLNGAM(DTERM1)
!
      DTERM2=0.5D0*(DLOG(DNU) + DLOG(DPI))
      DTERM3=DLNGAM(DNU/2.0D0)
      DTERM4=((DNU+1.0D0)/2.0D0)*DLOG(1.0D0 + DX**2/DNU)
      DDENOM=DTERM2 + DTERM3 + DTERM4
      DPDF=DNUM - DDENOM
      DPDF=DEXP(DPDF)
      PDF=REAL(DPDF)
!
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TPDF
      SUBROUTINE TPPF(P,ANU,PPF)
!CCCC SUBROUTINE TPPF(P,NU,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!              THE STUDENT'S T DISTRIBUTION USED
!              HEREIN IS DEFINED FOR ALL X,
!              AND ITS PROBABILITY DENSITY FUNCTION IS GIVEN
!              IN THE REFERENCES BELOW.
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 (EXCLUSIVELY)
!                                AND 1.0 (EXCLUSIVELY))
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --NU     = THE INTEGER NUMBER OF DEGREES
!                                OF FREEDOM.
!                                NU SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
!             VALUE PPF FOR THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
!                   AND 1.0 (EXCLUSIVELY).
!     OTHER DATAPAC   SUBROUTINES NEEDED--NORPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DSIN, DCOS, DSQRT, DATAN.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--FOR NU = 1 AND NU = 2, THE PERCENT POINT FUNCTION
!              FOR THE T DISTRIBUTION EXISTS IN SIMPLE CLOSED FORM
!              AND SO THE COMPUTED PERCENT POINTS ARE EXACT.
!            --FOR OTHER SMALL VALUES OF NU (NU BETWEEN 3 AND 6,
!              INCLUSIVELY), THE APPROXIMATION
!              OF THE T PERCENT POINT BY THE FORMULA
!              GIVEN IN THE REFERENCE BELOW IS AUGMENTED
!              BY 3 ITERATIONS OF NEWTON'S METHOD FOR
!              ROOT DETERMINATION.
!              THIS IMPROVES THE ACCURACY--ESPECIALLY FOR
!              VALUES OF P NEAR 0 OR 1.
!            --2022/10: FOR DEGREES OF FREEDOM FROM 7 TO 30, USE THE
!              HILL ALGORITHM AS THIS SEEMS TO BE MORE ACCURATE.
!     REFERENCES--NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED MATHMATICS
!                 SERIES 55, 1964, PAGE 949, FORMULA 26.7.5.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGE 102,
!                 FORMULA 11.
!               --FEDERIGHI, 'EXTENDED TABLES OF THE
!                 PERCENTAGE POINTS OF STUDENT'S T
!                 DISTRIBUTION, JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION,
!                 1969, PAGES 683-688.
!               --Hill, G.W (1970) "Algorithm 396: Student's t-quantiles"
!                 CACM 13(10), 619-620.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGES 120-123.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--OCTOBER   1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2006. SUPPORT FOR FRACTIONAL
!                                       DEGREES OF FREEDOM
!     UPDATED         --OCTOBER   2022. USE HILL ALGORITHM FOR
!                                       7 <= NU <= 30
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      DOUBLE PRECISION EPS
      DOUBLE PRECISION SIG
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION XL
      DOUBLE PRECISION XR
      DOUBLE PRECISION X
      DOUBLE PRECISION FXL
      DOUBLE PRECISION FXR
      DOUBLE PRECISION P1
      DOUBLE PRECISION FCS
      DOUBLE PRECISION XRML
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DBETAI
      DOUBLE PRECISION DX
!
      EXTERNAL DBETAI
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.14159265358979D0/
      DATA SQRT2/1.414213562D0/
      DATA B21/0.25D0/
      DATA B31,B32,B33,B34/0.01041666666667D0,5.0D0,16.0D0,3.0D0/
      DATA B41,B42,B43,B44,B45/0.00260416666667D0,3.0D0,19.0D0,17.0D0,   &
                               -15.0D0/
      DATA B51,B52,B53,B54,B55,B56/0.00001085069444D0,79.0D0,776.0D0,   &
                                   1482.0D0,-1920.0D0,-945.0D0/
!
      DATA HP/1.5707963268/
!
      DATA EPS /0.0001D0/
      DATA SIG /1.0D-7/
      DATA ZERO /0.0D0/
!
!-----START POINT-----------------------------------------------------
!
      S=0.0D0
      C=0.0D0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TPPF IS OUTSIDE ',   &
               'THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU.LT.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TPPF ',   &
               'IS LESS THAN 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ANU
        PPF=0.0
        GO TO 9000
      ENDIF
!
      IF(P.EQ.0.5)THEN
        PPF=0.0
        GO TO 9000
      ENDIF
!
      NU=INT(ANU)
      IF(ABS(ANU-REAL(NU)).GT.0.000001)GO TO 8000
!
!     CASE FOR INTEGER DEGREES OF FREEDOM
!
      DNU=NU
      DP=P
      MAXIT=5
!
      IF(NU.EQ.1)THEN
!
!       TREAT THE NU = 1 (CAUCHY) CASE
!
        DARG=PI*DP
        PPF=-DCOS(DARG)/DSIN(DARG)
        GO TO 9000
      ELSEIF(NU.EQ.2)THEN
!
!       TREAT THE NU = 2 CASE
!
        TERM1=SQRT2/2.0D0
        TERM2=2.0D0*DP-1.0D0
        TERM3=DSQRT(DP*(1.0D0-DP))
        PPF=TERM1*TERM2/TERM3
        GO TO 9000
      ELSEIF(NU.GE.7 .AND. NU.LE.30)THEN
!
!       FOR 7 <= NU <= 30, USE HILL ALGORITHM
!
        RL=1.
        IF(P .GE. 0.5) THEN
          PT=2.*(1. - P)
        ELSE
          RL=-1.
          PT=2.*P
        ENDIF
!
        N=NU
        RN=REAL(N)
        A=1./(RN-0.5)
        B=48./(A*A)
        C=((20700.*A/B-98.)*A-16.)*A+96.36
        D=((94.5/(B+C)-3.)/B+1.)*SQRT(A*HP)*RN
        XT=D*P
        Y=XT**(2./RN)
        IF(Y .LE. 0.05+A) THEN
          Y=((1./(((RN+6.)/(RN*Y)-0.089*D-0.822)*(RN+2.)*3.)+   &
            0.5/(RN+4.))*Y-1.)*(RN+1.)/(RN+2.)+1./Y
        ELSE
          PP=0.5*PT
          CALL NODPPF(DBLE(PP),DX)
          XT=REAL(DX)
          Y=XT*XT
          IF(N .GE. 5) C=C+0.3*(RN-4.5)*(XT+0.6)
          C=(((0.05*D*XT-5.)*XT-7.)*XT-2.)*XT+B+C
          Y=(((((0.4*Y+6.3)*Y+36.)*Y+94.5)/C-Y-3.)/B+1.)*XT
          Y=A*Y*Y
          IF(Y .LE. 0.002) Y=0.5*Y*Y+Y
          IF(Y .GT. 0.002) Y=EXP(Y)-1.
        ENDIF
!
        PPF=SQRT(RN*Y)*RL
        GO TO 9000
      ELSE
!
!       TREAT THE NU GREATER THAN OR EQUAL TO 3 CASE
!
!CCCC   CALL NORPPF(P,PPFN)
!CCCC   DPPFN=PPFN
        CALL NODPPF(DBLE(P),DPPFN)
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
        IF(NU.GE.7)GO TO 9000
        IF(NU.EQ.3)THEN
!
!         AUGMENT THE RESULTS FOR THE NU = 3 CASE
!
          DCON=PI*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO 350 IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-(Z+S*C-DCON)/(2.0D0*C*C)
  350     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.4)THEN
!
!         AUGMENT THE RESULTS FOR THE NU = 4 CASE
!
          DCON=2.0D0*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO 450 IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-((1.0D0+0.5D0*C*C)*S-DCON)/(1.5D0*C*C*C)
  450     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.5)THEN
!
!         AUGMENT THE RESULTS FOR THE NU = 5 CASE
!
          DCON=PI*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO 550 IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-(Z+(C+(2.0D0/3.0D0)*C*C*C)*S-DCON)/   &
              ((8.0D0/3.0D0)*C**4)
  550     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ELSEIF(NU.EQ.6)THEN
!
!         AUGMENT THE RESULTS FOR THE NU = 6 CASE
!
          DCON=2.0D0*(DP-0.5D0)
          DARG=DPPF/DSQRT(DNU)
          Z=DATAN(DARG)
          DO 650 IPASS=1,MAXIT
            S=DSIN(Z)
            C=DCOS(Z)
            Z=Z-((1.0D0+0.5D0*C*C+0.375D0*C**4)*S-DCON)/   &
              ((15.0D0/8.0D0)*C**5)
  650     CONTINUE
          PPF=DSQRT(DNU)*S/C
        ENDIF
        GO TO 9000
      ENDIF
!
!     CASE FOR FRACTIONAL DEGREES OF FREEDOM.  USE BISECTION
!     METHOD TO NUMERICALLY INVERT CDF FUNCTION.
!
 8000 CONTINUE
!
!     STEP 1: DETERMINE A BRACKETING INTERVAL.  USE 0 AS
!             EITHER THE LOWER OR UPPER LIMIT.
!
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
!
!  BISECTION METHOD
!
      DP=DBLE(P)
      IC = 0
      FXL = -DP
      FXR = 1.0D0 - DP
!
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
        IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GO TO 9000
        IC = IC + 1
        IF(IC.LE.MAXIT)THEN
          GO TO 105
        ELSE
          WRITE(ICOUT,130)
          CALL DPWRST('XXX','BUG ')
  130     FORMAT('***** ERROR--TPPF ROUTINE DID NOT CONVERGE')
        ENDIF
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TPPF
      SUBROUTINE TQLRAT(N,D,E2,IERR)
!***BEGIN PROLOGUE  TQLRAT
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***CATEGORY NO.  D4A5,D4C2A
!***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Computes eigenvalues of symmetric tridiagonal matrix
!            a rational variant of the QL method.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TQLRAT,
!     ALGORITHM 464, COMM. ACM 16, 689(1973) by Reinsch.
!
!     This subroutine finds the eigenvalues of a SYMMETRIC
!     TRIDIAGONAL matrix by the rational QL method.
!
!     On Input
!
!        N is the order of the matrix.
!
!        D contains the diagonal elements of the input matrix.
!
!        E2 contains the squares of the subdiagonal elements of the
!          input matrix in its last N-1 positions.  E2(1) is arbitrary.
!
!      On Output
!
!        D contains the eigenvalues in ascending order.  If an
!          error exit is made, the eigenvalues are correct and
!          ordered for indices 1,2,...IERR-1, but may not be
!          the smallest eigenvalues.
!
!        E2 has been destroyed.
!
!        IERR is set to
!          Zero       for normal return,
!          J          if the J-th eigenvalue has not been
!                     determined after 30 iterations.
!
!     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  PYTHAG
!***END PROLOGUE  TQLRAT
!
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      REAL D(N),E2(N)
      REAL B,C,F,G,H,P,R,S,MACHEP
      REAL PYTHAG
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA MACHEP/1.0E0/
!
!***FIRST EXECUTABLE STATEMENT  TQLRAT
!
      C=CPUMIN
!
      IF (MACHEP .NE. 1.0E0) GO TO 10
   05 MACHEP = 0.5E0*MACHEP
      IF (1.0E0 + MACHEP .GT. 1.0E0) GO TO 05
      MACHEP = 2.0E0*MACHEP
!
   10 IERR = 0
      IF (N .EQ. 1) GO TO 1001
!
      DO 100 I = 2, N
         E2(I-1) = E2(I)
  100 CONTINUE
!
      F = 0.0E0
      B = 0.0E0
      E2(N) = 0.0E0
!
      DO 290 L = 1, N
         J = 0
         H = MACHEP * (ABS(D(L)) + SQRT(E2(L)))
         IF (B .GT. H) GO TO 105
         B = H
         C = B * B
!     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    CONTINUE
         DO 110 M = L, N
            IF (E2(M) .LE. C) GO TO 120
!     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
!
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
!     .......... FORM SHIFT ..........
         L1 = L + 1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * S)
         R = PYTHAG(P,1.0E0)
         D(L) = S / (P + SIGN(R,P))
         H = G - D(L)
!
         DO 140 I = L1, N
            D(I) = D(I) - H
  140    CONTINUE
!
         F = F + H
!     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0E0) G = B
         H = G
         S = 0.0E0
         MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
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
!
         E2(L) = S * G
         D(L) = H
!     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0E0) GO TO 210
         IF (ABS(E2(L)) .LE. ABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0E0) GO TO 130
  210    CONTINUE
         P = D(L) + F
!     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
!     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
!
  250    I = 1
  270    D(I) = P
  290 CONTINUE
!
      GO TO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END SUBROUTINE TQLRAT
      SUBROUTINE TRACDF(X,A,B,C,D,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
!              F(X,A,B,C,D) = 0                   X <  A
!                           = (B-A)/(D+C-B-A)*((X-A)/(B-A))**2   A<=X<B
!                           = ((B-A)+2*(X-B))/(D+C-B-A)          B<=X<C
!                           = 1-(D-C)/(D+C-B-A)*((D-X)/(D-C))**2 C<=X<D
!                           = 1                                  X>D
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
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
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(   &
      '      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRACDF
      SUBROUTINE TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     PURPOSE--PRINT OUT A TRACE LINE FOR DEBUGGING.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JANUARY 1979.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      WRITE(ICOUT,105)ISTEPN,ISUBN1,ISUBN2
  105 FORMAT('TRACE AT STEP ',A4,' OF ',A4,A4)
      CALL DPWRST('XXX','BUG ')
!
      RETURN
      END SUBROUTINE TRACE2
      INTEGER FUNCTION TRADES (XPRIME, MPRIME, YPRIME, NPRIME)
!
!        ALGORITHM AS 304.4 APPL.STATIST. (1996), VOL.45, NO.3
!
!        Returns the number of 1-for-1 trades that refutes the null
!        hypothesis.  Assumes that XPRIME has the smaller mean and
!        that both arrays are sorted in ascending order.
!
!        DATAPLOT NOTE: UTILITY ROUTINE USED BY FISHER TWO SAMPLE
!                       RANDOMIZATION TEST
!
      INTEGER MPRIME, NPRIME
      REAL XPRIME(*), YPRIME(*)
!
      INTEGER I, J
!
      TRADES = 0
      I = 1
      J = 1
   10 IF (J .GT. NPRIME) GO TO  40
   20 IF (XPRIME(I) .GE. YPRIME(J)) GO TO  30
      I = I + 1
      IF (I .LE. MPRIME) GO TO  20
   30 TRADES = TRADES + (MPRIME - I + 1)
      J = J + 1
      IF (I .LE. MPRIME) GO TO  10
!
   40 RETURN
      END FUNCTION TRADES 
      SUBROUTINE TRAN(N,ANU,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE STUDENT'S T DISTRIBUTION
!              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --NU     = THE INTEGER DEGREES OF FREEDOM
!                                (PARAMETER) FOR THE T
!                                DISTRIBUTION.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE STUDENT'S T DISTRIBUTION
!             WITH DEGREES OF FREEDOM PARAMETER = NU.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG, SQRT, SIN, COS.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--MOOD AND GRABLE, INTRODUCTION TO THE
!                 THEORY OF STATISTICS, 1963, PAGE 233.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGE 94.
!               --HASTINGS AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--A HANDBOOK FOR
!                 STUDENTS AND PRACTITIONERS, 1975,
!                 PAGE 121.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MAY       2004. SUPPORT NON-INTEGER DEGREES
!                                       OF FREEDOM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(*)
      DIMENSION Y(2),Z(2)
!
      CHARACTER*4 ICASE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.14159265359/
      DATA EPS/0.00001/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)GO TO 50
      IF(ANU.LE.0.0)GO TO 60
      GO TO 90
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
    5 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',   &
      'TRAN   SUBROUTINE IS NON-POSITIVE *****')
   15 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
      'TRAN   SUBROUTINE IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F12.5,' *****')
!
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
!
!     CASE 1: INTEGER DEGREES OF FREEDOM
!
      IF(ICASE.EQ.'INTE')THEN
!       GENERATE N STUDENT'S T RANDOM NUMBERS
!       USING THE DEFINITION THAT
!       A STUDENT'S T VARIATE WITH NU DEGREES OF FREEDOM
!       EQUALS A NORMAL VARIATE DIVIDED BY
!       A STANDARDIZED CHI VARIATE
!       (WHERE THE LATTER EQUALS SQRT(CHI-SQUARED/NU).
!       FIRST GENERATE A NORMAL RANDOM NUMBER,
!       THEN GENERATE A STANDARDIZED CHI RANDOM NUMBER,
!       THEN FORM THE RATIO OF THE FIRST DIVIDED BY
!       THE SECOND.
!
        ANU=NU
        DO 100 I=1,N
!
        CALL UNIRAN(2,ISEED,Y)
        ARG1=-2.0*LOG(Y(1))
        ARG2=2.0*PI*Y(2)
        ZNORM=(SQRT(ARG1))*(COS(ARG2))
!
        SUM=0.0
        DO 200 J=1,NU,2
        CALL UNIRAN(2,ISEED,Y)
        ARG1=-2.0*LOG(Y(1))
        ARG2=2.0*PI*Y(2)
        Z(1)=(SQRT(ARG1))*(COS(ARG2))
        Z(2)=(SQRT(ARG1))*(SIN(ARG2))
        SUM=SUM+Z(1)*Z(1)
        IF(J.EQ.NU)GO TO 200
        SUM=SUM+Z(2)*Z(2)
  200   CONTINUE
!
        X(I)=ZNORM/SQRT(SUM/ANU)
!
  100  CONTINUE
!
      ELSE
        DO 300 I=1,N
          ATEMP=RDT(ANU,ISEED)
          X(I)=ATEMP
  300   CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE TRAN
      DOUBLE PRECISION FUNCTION TRAN02(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 2, defined as
!
!      TRAN02(X) = integral 0 to X { t**2 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN02 = x to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large x contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN02 = VALINF  -  x**2 exp(-x)
!                    The recommended value is 2/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:    23 January, 1996
!
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN02'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 2 , 2.0D0 /
      DATA VALINF/0.32898681336964528729D1/
      DATA ATRAN/1.67176044643453850301D0,   &
                -0.14773535994679448986D0,   &
                 0.1482138199469363384D-1,   &
                -0.141953303263056126D-2,   &
                 0.13065413244157083D-3,   &
                -0.1171557958675790D-4,   &
                 0.103334984457557D-5,   &
                -0.9019113042227D-7,   &
                 0.781771698331D-8,   &
                -0.67445656840D-9,   &
                 0.5799463945D-10,   &
                -0.497476185D-11,   &
                 0.42596097D-12,   &
                -0.3642189D-13,   &
                 0.311086D-14,   &
                -0.26547D-15,   &
                 0.2264D-16,   &
                -0.193D-17,   &
                 0.16D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN02 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN02--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = ONE / (HALF * XK)
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
      IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW1 ) THEN
            TRAN02 =  ( X ** ( NUMJN - 1 ) ) / ( RNUMJN - ONE )
         ELSE
            T = ( ( ( X * X ) / EIGHT ) - HALF ) - HALF
            TRAN02 = ( X ** ( NUMJN - 1 ) ) * CHEVAL(NTERMS,ATRAN,T)
         ENDIF
      ELSE
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN02
      DOUBLE PRECISION FUNCTION TRAN03(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 3, defined as
!
!      TRAN03(X) = integral 0 to X { t**3 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN03 = 0.0 to machine
!                    precision. The recommended value is
!                          square root of (2*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN03 = X**2/2 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN03 = VALINF  -  X**3 exp(-X)
!                    The recommended value is 3/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:   23 January, 1996
!
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN03'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0/
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 3 , 3.0D0 /
      DATA VALINF/0.72123414189575657124D1/
      DATA ATRAN/0.76201254324387200657D0,   &
                -0.10567438770505853250D0,   &
                 0.1197780848196578097D-1,   &
                -0.121440152036983073D-2,   &
                 0.11550997693928547D-3,   &
                -0.1058159921244229D-4,   &
                 0.94746633853018D-6,   &
                -0.8362212128581D-7,   &
                 0.731090992775D-8,   &
                -0.63505947788D-9,   &
                 0.5491182819D-10,   &
                -0.473213954D-11,   &
                 0.40676948D-12,   &
                -0.3489706D-13,   &
                 0.298923D-14,   &
                -0.25574D-15,   &
                 0.2186D-16,   &
                -0.187D-17,   &
                 0.16D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN03 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN03--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XLOW2 = SQRT( D1MACH(1) / HALF )
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN03
      DOUBLE PRECISION FUNCTION TRAN04(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 4, defined as
!
!      TRAN04(X) = integral 0 to X { t**4 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN04 = 0.0 to machine
!                   precision. The recommended value is
!                          cube root of (3*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN04 = X**3/3 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN04 = VALINF  -  X**4 exp(-X)
!                    The recommended value is 4/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!
!    For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:   23 January, 1996
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN04'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 4 , 4.0D0 /
      DATA VALINF/0.25975757609067316596D2/
      DATA ATRAN/0.48075709946151105786D0,   &
                -0.8175378810321083956D-1,   &
                 0.1002700665975162973D-1,   &
                -0.105993393598201507D-2,   &
                 0.10345062450304053D-3,   &
                -0.964427054858991D-5,   &
                 0.87455444085147D-6,   &
                -0.7793212079811D-7,   &
                 0.686498861410D-8,   &
                -0.59995710764D-9,   &
                 0.5213662413D-10,   &
                -0.451183819D-11,   &
                 0.38921592D-12,   &
                -0.3349360D-13,   &
                 0.287667D-14,   &
                -0.24668D-15,   &
                 0.2113D-16,   &
                -0.181D-17,   &
                 0.15D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN04 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN04--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN04
      DOUBLE PRECISION FUNCTION TRAN05(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order n, defined as
!
!      TRAN05(X) = integral 0 to X { t**5 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN05 = 0.0 to machine
!                   precision. The recommended value is
!                          4th root of (4*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN05 = X**4/4 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN05 = VALINF  -  X**5 exp(-X)
!                    The recommended value is 5/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:  23 January, 1996
!
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCC  CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN05'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 5 , 5.0D0 /
      DATA VALINF/0.12443133061720439116D3/
      DATA ATRAN/0.34777777713391078928D0,   &
                -0.6645698897605042801D-1,   &
                 0.861107265688330882D-2,   &
                -0.93966822237555384D-3,   &
                 0.9363248060815134D-4,   &
                -0.885713193408328D-5,   &
                 0.81191498914503D-6,   &
                -0.7295765423277D-7,   &
                 0.646971455045D-8,   &
                -0.56849028255D-9,   &
                 0.4962559787D-10,   &
                -0.431093996D-11,   &
                 0.37310094D-12,   &
                -0.3219769D-13,   &
                 0.277220D-14,   &
                -0.23824D-15,   &
                 0.2044D-16,   &
                -0.175D-17,   &
                 0.15D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN05 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN05--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN05
      DOUBLE PRECISION FUNCTION TRAN06(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 6, defined as
!
!      TRAN06(X) = integral 0 to X { t**6 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN06 = 0.0 to machine
!                   precision. The recommended value is
!                          5th root of (5*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN06 = X**5/5 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN06 = VALINF  -  X**6 exp(-X)
!                    The recommended value is 6/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:   23 January, 1996
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN06'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 6 , 6.0D0 /
      DATA VALINF/0.73248700462880338059D3/
      DATA ATRAN/0.27127335397840008227D0,   &
                -0.5588610553191453393D-1,   &
                 0.753919513290083056D-2,   &
                -0.84351138579211219D-3,   &
                 0.8549098079676702D-4,   &
                -0.818715493293098D-5,   &
                 0.75754240427986D-6,   &
                -0.6857306541831D-7,   &
                 0.611700376031D-8,   &
                -0.54012707024D-9,   &
                 0.4734306435D-10,   &
                -0.412701055D-11,   &
                 0.35825603D-12,   &
                -0.3099752D-13,   &
                 0.267501D-14,   &
                -0.23036D-15,   &
                 0.1980D-16,   &
                -0.170D-17,   &
                 0.15D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN06 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN06--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4 .0
!
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
!
!  Code for x > 4 .0
!
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
      END FUNCTION TRAN06
      DOUBLE PRECISION FUNCTION TRAN07(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 7, defined as
!
!      TRAN07(X) = integral 0 to X { t**7 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN07 = 0.0 to machine
!                   precision. The recommended value is
!                          6th root of (6*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN07 = X**6/6 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN07 = VALINF  -  X**7 exp(-X)
!                    The recommended value is 7/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:   23 January, 1996
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN07'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0/
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 7 , 7.0D0/
      DATA VALINF/0.50820803580048910473D4/
      DATA ATRAN/0.22189250734010404423D0,   &
                -0.4816751061177993694D-1,   &
                 0.670092448103153629D-2,   &
                -0.76495183443082557D-3,   &
                 0.7863485592348690D-4,   &
                -0.761025180887504D-5,   &
                 0.70991696299917D-6,   &
                -0.6468025624903D-7,   &
                 0.580039233960D-8,   &
                -0.51443370149D-9,   &
                 0.4525944183D-10,   &
                -0.395800363D-11,   &
                 0.34453785D-12,   &
                -0.2988292D-13,   &
                 0.258434D-14,   &
                -0.22297D-15,   &
                 0.1920D-16,   &
                -0.165D-17,   &
                 0.14D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN07 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN07--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x <= 4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN07
      DOUBLE PRECISION FUNCTION TRAN08(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 8, defined as
!
!      TRAN08(X) = integral 0 to X { t**8 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN08 = 0.0 to machine
!                   precision. The recommended value is
!                          7th root of (7*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN08 = X**7/7 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN08 = VALINF  -  X**8 exp(-X)
!                    The recommended value is 8/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:  23 January, 1996
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCC  CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN08'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 8 , 8.0D0 /
      DATA VALINF/0.40484399001901115764D5/
      DATA ATRAN/0.18750695774043719233D0,   &
                -0.4229527646093673337D-1,   &
                 0.602814856929065592D-2,   &
                -0.69961054811814776D-3,   &
                 0.7278482421298789D-4,   &
                -0.710846250050067D-5,   &
                 0.66786706890115D-6,   &
                -0.6120157501844D-7,   &
                 0.551465264474D-8,   &
                -0.49105307052D-9,   &
                 0.4335000869D-10,   &
                -0.380218700D-11,   &
                 0.33182369D-12,   &
                -0.2884512D-13,   &
                 0.249958D-14,   &
                -0.21605D-15,   &
                 0.1863D-16,   &
                -0.160D-17,   &
                 0.14D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN08 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN08--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN08
      DOUBLE PRECISION FUNCTION TRAN09(XVALUE)
!
!  DESCRIPTION:
!
!    This program calculates the transport integral of order 9, defined as
!
!      TRAN09(X) = integral 0 to X { t**9 exp(t)/[exp(t)-1]**2 } dt
!
!    The program uses a Chebyshev series, the coefficients of which are
!    given to an accuracy of 20 decimal places.
!
!
!  ERROR RETURNS:
!
!    If XVALUE < 0.0, an error message is printed, and the program
!    returns the value 0.0.
!
!
!  MACHINE-DEPENDENT CONSTANTS:
!
!    NTERMS - INTEGER - The number of terms of the array ATRAN to be used.
!                       The recommended value is such that
!                             ATRAN(NTERMS) < EPS/100
!
!    XLOW2 - DOUBLE PRECISION - The value below which TRAN09 = 0.0 to machine
!                   precision. The recommended value is
!                          8th root of (8*XMIN)
!
!    XLOW1 - DOUBLE PRECISION - The value below which TRAN09 = X**8/8 to
!                   machine precision. The recommended value is
!                             sqrt(8*EPSNEG)
!
!    XHIGH1 - DOUBLE PRECISION - The value above which the exponential series for
!                    large X contains only one term. The recommended value
!                    is        - ln(EPS).
!
!    XHIGH2 - DOUBLE PRECISION - The value above which
!                       TRAN09 = VALINF  -  X**9 exp(-X)
!                    The recommended value is 9/EPS
!
!    XHIGH3 - DOUBLE PRECISION - The value of ln(EPSNEG). Used to prevent overflow
!                    for large x.
!
!     For values of EPS, EPSNEG, and XMIN refer to the file MACHCON.TXT
!
!     The machine-dependent constants are computed internally by
!     using the D1MACH subroutine.
!
!
!  INTRINSIC FUNCTIONS USED:
!
!     EXP, INT, LOG, SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!  AUTHOR:
!
!     DR. ALLAN J. MACLEOD,
!     DEPT. OF MATHEMATICS AND STATISTICS,
!     UNIVERSITY OF PAISLEY ,
!     HIGH ST.,
!     PAISLEY,
!     SCOTLAND.
!     PA1 2BE.
!
!     (e-mail: macl_ms0@paisley.ac.uk )
!
!
!  LATEST REVISION:   23 January, 1996
!
      INTEGER K1,K2,NTERMS,NUMEXP,NUMJN
      DOUBLE PRECISION ATRAN(0:19),CHEVAL,EIGHT,FOUR,HALF,ONE,ONEHUN,RK,   &
           RNUMJN,SUMEXP,SUM2,T,VALINF,X,XHIGH1,XHIGH2,   &
           XHIGH3,XK,XK1,XLOW1,XLOW2,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*14
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'TRAN09'/
!CCCC DATA ERRMSG/'ARGUMENT < 0.0'/
      DATA ZERO,HALF,ONE/ 0.0D0 , 0.5D0 , 1.0D0 /
      DATA FOUR,EIGHT,ONEHUN/ 4.0D0 , 8.0D0 , 100.0D0 /
      DATA NUMJN,RNUMJN/ 9 , 9.0D0 /
      DATA VALINF/0.36360880558872871397D6/
      DATA ATRAN/0.16224049991949846835D0,   &
                -0.3768351452195937773D-1,   &
                 0.547669715917719770D-2,   &
                -0.64443945009449521D-3,   &
                 0.6773645285280983D-4,   &
                -0.666813497582042D-5,   &
                 0.63047560019047D-6,   &
                -0.5807478663611D-7,   &
                 0.525551305123D-8,   &
                -0.46968861761D-9,   &
                 0.4159395065D-10,   &
                -0.365808491D-11,   &
                 0.32000794D-12,   &
                -0.2787651D-13,   &
                 0.242017D-14,   &
                -0.20953D-15,   &
                 0.1810D-16,   &
                -0.156D-17,   &
                 0.13D-18,   &
                -0.1D-19/
!
      XLOW1 = 0.0D0
      XLOW2 = 0.0D0
!
!  Start execution
!
      X = XVALUE
!
!  Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         TRAN09 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM TRAN09--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      XK = D1MACH(3)
      T = XK / ONEHUN
      IF ( X .LE. FOUR ) THEN
         DO 10 NTERMS = 19 , 0 , -1
            IF ( ABS(ATRAN(NTERMS)) .GT. T ) GO TO  19
 10      CONTINUE
 19      XLOW1 = SQRT( EIGHT * XK )
         XK1 = RNUMJN - ONE
         XLOW2 = ( XK1 * D1MACH(1) ) ** (ONE/XK1)
      ELSE
         XHIGH1 = - LOG(D1MACH(4))
         XHIGH2 = RNUMJN / XK
         XHIGH3 = LOG(XK)
      ENDIF
!
!   Code for x < =  4.0
!
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
!
!  Code for x > 4.0
!
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
      END FUNCTION TRAN09
      SUBROUTINE TRAPDF(X,A,B,C,D,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
!                  f(X,A,B,C,D) = U*((X-A)/(B-A))     A <= X <  B
!                               = U                   B <= X <  C
!                               = U*((D-X)/(D-C))     C <= X <  D
!                               = 0                   X < A, X >= D
!              WHERE
!                  U = 2/(D+C-B-A), A <= B <= C <= D
!              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF
!              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
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
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
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
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(   &
      '      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRAPDF
      SUBROUTINE TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,IXP,IYP)
!
!     PURPOSE--TRANSLATE AND ROTATE POINTS X AND Y
!              WHICH ARE BEING GENERATED ALONG THE X AXIS
!              TO CORRESPONDING POINTS IN WHICH THE
!              ORIGIN (0,0) HAS BEEN TRANSLATED TO (X1,Y1)
!              AND THE LINE SEQUENCE HAS BEEN ROTATED BY AN ANGLE THETA.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--OCTOBER   1980
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'RANS')THEN
        WRITE(ICOUT,52)X,Y,THETA,DELX,DELY
   52   FORMAT('X,Y,THETA,DELX,DELY = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      XROT=X*COS(THETA)-Y*SIN(THETA)
      YROT=X*SIN(THETA)+Y*COS(THETA)
!
      IF(DELX.GE.0.0)GO TO 110
      GO TO 120
!
  110 CONTINUE
      XP=X1+XROT
      YP=Y1+YROT
      GO TO 900
!
  120 CONTINUE
      XP=X1-XROT
      YP=Y1-YROT
      GO TO 900
!
  900 CONTINUE
      IXP=INT(XP+0.5)
      IYP=INT(YP+0.5)
!
      RETURN
      END SUBROUTINE TRANS
      SUBROUTINE TRAPPF(P,A,B,C,D,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TRAPEZOID DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
!              F(X,A,B,C,D) = 0                   X <  A
!                           = (B-A)/(D+C-B-A)*((X-A)/(B-A))**2   A<=X<B
!                           = ((B-A)+2*(X-B))/(D+C-B-A)          B<=X<C
!                           = 1-(D-C)/(D+C-B-A)*((D-X)/(D-C))**2 C<=X<D
!                           = 1                                  X>D
!              THE ALGORITHM FOR THE PPF IS TO COMPUTE THE CDF AT
!              X = A, X = B, X = C, AND X = D TO FIND THE APPROPRIATE
!              INTERVAL FOR P.  THEN INVERT THE APPROPRIATE EQUATION
!              ABOVE TO FIND THE PPF VALUE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
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
        GO TO 9000
      ENDIF
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
!
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(   &
      '      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
   22 FORMAT(   &
      '***** FATAL ERROR--FOR THE TRAPEZOID PERCENT POINT FUNCTION,')
   23 FORMAT(   &
      '      THE VALUE OF THE INPUTR ARGUMENT IS OUTSIDE THE ',   &
      'ALLOWABLE (0,1] INTERVAL.')
   26 FORMAT(   &
      '      VALUE OF INPUT ARGUMENT = ',E15.7)
!
!-----START POINT-----------------------------------------------------
!
      P1=0.0
      CALL TRACDF(B,A,B,C,D,P2)
      CALL TRACDF(C,A,B,C,D,P3)
      P4=1.0
!
      IF(P.EQ.0.0)THEN
        PPF=A
        GO TO 9000
      ELSEIF(P.EQ.1.0)THEN
        PPF=D
        GO TO 9000
      ELSEIF(P.EQ.P2)THEN
        PPF=B
        GO TO 9000
      ELSEIF(P.EQ.P3)THEN
        PPF=C
        GO TO 9000
      ENDIF
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRAPPF
      SUBROUTINE TRARAN(N,A,B,C,D,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TRAPEZOID DISTRIBUTION
!              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
!                  f(X,A,B,C,D) = U*((X-A)/(B-A))     A <= X <  B
!                               = U                   B <= X <  C
!                               = U*((D-X)/(D-C))     C <= X <  D
!                               = 0                   X < A, X >= D
!              WHERE
!                  U = 2/(D+C-B-A), A <= B <= C <= D
!              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF
!              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TRAPEZOID DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TRAPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!               --TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.6
!     ORIGINAL VERSION--JUNE      2003.
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE FIRST INPUT ARGUMENT TO THE ',   &
      'TRARAN SUBROUTINE IS NON-POSITIVE *****')
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
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE TRAPEZOID DISTRIBUTION, THE FOUR')
   13 FORMAT(   &
      '      SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
!
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TRAPEZOID RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
      P=X(I)
      CALL TRAPPF(P,A,B,C,D,PPF)
      X(I)=PPF
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRARAN
      SUBROUTINE TRED1(NM,N,A,D,E,E2)
!***BEGIN PROLOGUE  TRED1
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***CATEGORY NO.  D4C1B1
!***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Reduce real symmetric matrix to symmetric tridiagonal
!            matrix using orthogonal similarity transformations.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TRED1,
!     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     This subroutine reduces a REAL SYMMETRIC matrix
!     to a symmetric tridiagonal matrix using
!     orthogonal similarity transformations.
!
!     On Input
!
!        NM must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        N is the order of the matrix.
!
!        A contains the real symmetric input matrix.  Only the
!          lower triangle of the matrix need be supplied.
!
!     On Output
!
!        A contains information about the orthogonal trans-
!          formations used in the reduction in its strict lower
!          triangle.  The full upper triangle of A is unaltered.
!
!        D contains the diagonal elements of the tridiagonal matrix.
!
!        E contains the subdiagonal elements of the tridiagonal
!          matrix in its last N-1 positions.  E(1) is set to zero.
!
!        E2 contains the squares of the corresponding elements of E.
!          E2 may coincide with E if the squares are not needed.
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  TRED1
!
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),E2(N)
      REAL F,G,H,SCALE
!
!***FIRST EXECUTABLE STATEMENT  TRED1
      DO 100 I = 1, N
         D(I) = A(I,I)
  100 CONTINUE
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 1) GO TO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
            SCALE = SCALE + ABS(A(I,K))
  120    CONTINUE
!
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    CONTINUE
         E(I) = 0.0E0
         E2(I) = 0.0E0
         GO TO 290
!
  140    CONTINUE
         DO 150 K = 1, L
            A(I,K) = A(I,K) / SCALE
            H = H + A(I,K) * A(I,K)
  150    CONTINUE
!
         E2(I) = SCALE * SCALE * H
         F = A(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         A(I,L) = F - G
         IF (L .EQ. 1) GO TO 270
         F = 0.0E0
!
         DO 240 J = 1, L
            G = 0.0E0
!     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
                G = G + A(J,K) * A(I,K)
  180       CONTINUE
!
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
!
            DO 200 K = JP1, L
               G = G + A(K,J) * A(I,K)
  200       CONTINUE
!     .......... FORM ELEMENT OF P ..........
  220       CONTINUE
            E(J) = G / H
            F = F + E(J) * A(I,J)
  240    CONTINUE
!
         H = F / (H + H)
!     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = A(I,J)
            G = E(J) - H * F
            E(J) = G
!
            DO 265 K = 1, J
               A(J,K) = A(J,K) - F * E(K) - G * A(I,K)
  265       CONTINUE
  260    CONTINUE
!
  270    CONTINUE
         DO 280 K = 1, L
            A(I,K) = SCALE * A(I,K)
  280    CONTINUE
!
  290    CONTINUE
         H = D(I)
         D(I) = A(I,I)
         A(I,I) = H
  300 CONTINUE
!
      RETURN
      END SUBROUTINE TRED1
      SUBROUTINE TRED2(NM,N,A,D,E,Z)
!***BEGIN PROLOGUE  TRED2
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***CATEGORY NO.  D4C1B1
!***KEYWORDS  EIGENVALUES,EIGENVECTORS,EISPACK
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Reduce real symmetric matrix to symmetric tridiagonal
!            matrix using and accumulating orthogonal transformation
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TRED2,
!     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     This subroutine reduces a REAL SYMMETRIC matrix to a
!     symmetric tridiagonal matrix using and accumulating
!     orthogonal similarity transformations.
!
!     On Input
!
!        NM must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        N is the order of the matrix.
!
!        A contains the real symmetric input matrix.  Only the
!          lower triangle of the matrix need be supplied.
!
!     On Output
!
!        D contains the diagonal elements of the tridiagonal matrix.
!
!        E contains the subdiagonal elements of the tridiagonal
!          matrix in its last N-1 positions.  E(1) is set to zero.
!
!        Z contains the orthogonal transformation matrix
!          produced in the reduction.
!
!        A and Z may coincide.  If distinct, A is unaltered.
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  TRED2
!
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),Z(NM,N)
      REAL F,G,H,HH,SCALE
!
!***FIRST EXECUTABLE STATEMENT  TRED2
      DO 100 I = 1, N
!
         DO 110 J = 1, I
            Z(I,J) = A(I,J)
  110    CONTINUE
  100 CONTINUE
!
      IF (N .EQ. 1) GO TO 320
!     .......... FOR I=N STEP -1 UNTIL 2 DO -- ..........
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
            SCALE = SCALE + ABS(Z(I,K))
  120    CONTINUE
!
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
!
  140    CONTINUE
         DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
!
         F = Z(I,L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0E0
!
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / H
            G = 0.0E0
!     .......... FORM ELEMENT OF A*U ..........
            DO 180 K = 1, J
               G = G + Z(J,K) * Z(I,K)
  180       CONTINUE
!
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
!
            DO 200 K = JP1, L
               G = G + Z(K,J) * Z(I,K)
  200       CONTINUE
!     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
!
         HH = F / (H + H)
!     .......... FORM REDUCED A ..........
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
!
            DO 265 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  265       CONTINUE
  260    CONTINUE
!
  290    D(I) = H
  300 CONTINUE
!
  320 D(1) = 0.0E0
      E(1) = 0.0E0
!     .......... ACCUMULATION OF TRANSFORMATION MATRICES ..........
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0E0) GO TO 380
!
         DO 360 J = 1, L
            G = 0.0E0
!
            DO 340 K = 1, L
               G = G + Z(I,K) * Z(K,J)
  340       CONTINUE
!
            DO 365 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  365       CONTINUE
  360    CONTINUE
!
  380    CONTINUE
         D(I) = Z(I,I)
         Z(I,I) = 1.0E0
         IF (L .LT. 1) GO TO 500
!
         DO 400 J = 1, L
            Z(I,J) = 0.0E0
            Z(J,I) = 0.0E0
  400    CONTINUE
!
  500 CONTINUE
!
      RETURN
      END SUBROUTINE TRED2
      SUBROUTINE TREGUP(NR,N,X,F,G,A,SC,SX,NWTAKE,STEPMX,   &
           STEPTL,   &
           DLT,IRETCD,XPLSP,FPLSP,XPLS,FPLS,MXTAKE,IPR,METHOD,UDIAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! PURPOSE
! -------
! DECIDE WHETHER TO ACCEPT XPLS=X+SC AS THE NEXT ITERATE AND UPDATE THE
! TRUST REGION DLT.
!
! PARAMETERS
! ----------
! NR           --> ROW DIMENSION OF MATRIX
! N            --> DIMENSION OF PROBLEM
! X(N)         --> OLD ITERATE X[K-1]
! F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
! G(N)         --> GRADIENT AT OLD ITERATE, G(X), OR APPROXIMATE
! A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
!                  LOWER TRIANGULAR PART AND DIAGONAL.
!                  HESSIAN OR APPROX IN UPPER TRIANGULAR PART
! OPTFCN       --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
! SC(N)        --> CURRENT STEP
! SX(N)        --> DIAGONAL SCALING MATRIX FOR X
! NWTAKE       --> BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
! STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
! STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
!                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
! DLT         <--> TRUST REGION RADIUS
! IRETCD      <--> RETURN CODE
!                    =0 XPLS ACCEPTED AS NEXT ITERATE;
!                       DLT TRUST REGION FOR NEXT ITERATION.
!                    =1 XPLS UNSATISFACTORY BUT ACCEPTED AS NEXT ITERATE
!                       BECAUSE XPLS-X .LT. SMALLEST ALLOWABLE
!                       STEP LENGTH.
!                    =2 F(XPLS) TOO LARGE.  CONTINUE CURRENT ITERATION
!                       WITH NEW REDUCED DLT.
!                    =3 F(XPLS) SUFFICIENTLY SMALL, BUT QUADRATIC MODEL
!                       PREDICTS F(XPLS) SUFFICIENTLY WELL TO CONTINUE
!                       CURRENT ITERATION WITH NEW DOUBLED DLT.
! XPLSP(N)    <--> WORKSPACE [VALUE NEEDS TO BE RETAINED BETWEEN
!                  SUCCESIVE CALLS OF K-TH GLOBAL STEP]
! FPLSP       <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
! XPLS(N)     <--  NEW ITERATE X[K]
! FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
! MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
! IPR          --> DEVICE TO WHICH TO SEND OUTPUT
! METHOD       --> ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
!                    =1 LINE SEARCH
!                    =2 DOUBLE DOGLEG
!                    =3 MORE-HEBDON
! UDIAG(N)     --> DIAGONAL OF HESSIAN IN A(.,.)
!
      DIMENSION X(N),XPLS(N),G(N)
      DIMENSION SX(N),SC(N),XPLSP(N)
      DIMENSION A(NR,1)
      LOGICAL NWTAKE,MXTAKE
      DIMENSION UDIAG(N)
      DIMENSION FHAT(1)
!
      IPR=IPR
      MXTAKE=.FALSE.
      DO 100 I=1,N
        XPLS(I)=X(I)+SC(I)
  100 CONTINUE
      CALL OPTFCN(N,XPLS,FHAT)
      FPLS=FHAT(1)
      DLTF=FPLS-F
      SLP=DDOT(N,G,1,SC,1)
!
! NEXT STATEMENT ADDED FOR CASE OF COMPILERS WHICH DO NOT OPTIMIZE
! EVALUATION OF NEXT "IF" STATEMENT (IN WHICH CASE FPLSP COULD BE
! UNDEFINED).
      IF(IRETCD.EQ.4) FPLSP=0.0
!$    WRITE(IPR,961) IRETCD,FPLS,FPLSP,DLTF,SLP
      IF(IRETCD.NE.3 .OR. (FPLS.LT.FPLSP .AND. DLTF.LE. 1.E-4*SLP))   &
                                                           GO TO 130
!     IF(IRETCD.EQ.3 .AND. (FPLS.GE.FPLSP .OR. DLTF.GT. 1.E-4*SLP))
!     THEN
!
!       RESET XPLS TO XPLSP AND TERMINATE GLOBAL STEP
!
        IRETCD=0
        DO 110 I=1,N
          XPLS(I)=XPLSP(I)
  110   CONTINUE
        FPLS=FPLSP
        DLT=.5*DLT
!$      WRITE(IPR,951)
        GO TO 230
!     ELSE
!
!       FPLS TOO LARGE
!
  130   IF(DLTF.LE. 1.E-4*SLP) GO TO 170
!       IF(DLTF.GT. 1.E-4*SLP)
!       THEN
!$        WRITE(IPR,952)
          RLN=0.
          DO 140 I=1,N
            RLN=MAX(RLN,ABS(SC(I))/MAX(ABS(XPLS(I)),1./SX(I)))
  140     CONTINUE
!$        WRITE(IPR,962) RLN
          IF(RLN.GE.STEPTL) GO TO 150
!         IF(RLN.LT.STEPTL)
!         THEN
!
!           CANNOT FIND SATISFACTORY XPLS SUFFICIENTLY DISTINCT FROM X
!
            IRETCD=1
!$          WRITE(IPR,954)
            GO TO 230
!         ELSE
!
!           REDUCE TRUST REGION AND CONTINUE GLOBAL STEP
!
  150       IRETCD=2
            DLTMP=-SLP*DLT/(2.*(DLTF-SLP))
!$          WRITE(IPR,963) DLTMP
            IF(DLTMP.GE. .1*DLT) GO TO 155
!           IF(DLTMP.LT. .1*DLT)
!           THEN
              DLT=.1*DLT
              GO TO 160
!           ELSE
  155         DLT=DLTMP
!           ENDIF
  160       CONTINUE
!$          WRITE(IPR,955)
            GO TO 230
!         ENDIF
!       ELSE
!
!         FPLS SUFFICIENTLY SMALL
!
  170     CONTINUE
!$        WRITE(IPR,958)
          DLTFP=0.
          IF (METHOD .EQ. 3) GO TO 180
!
!         IF (METHOD .EQ. 2)
!         THEN
!
          DO 177 I = 1, N
             TEMP = 0.0
             DO 173 J = I, N
                TEMP = TEMP + (A(J, I)*SC(J))
  173        CONTINUE
             DLTFP = DLTFP + TEMP*TEMP
  177     CONTINUE
          GO TO 190
!
!         ELSE
!
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
!
!         END IF
!
  190     DLTFP = SLP + DLTFP/2.0
!$        WRITE(IPR,964) DLTFP,NWTAKE
          IF(IRETCD.EQ.2 .OR. (ABS(DLTFP-DLTF).GT. .1*ABS(DLTF))   &
               .OR. NWTAKE .OR. (DLT.GT. .99*STEPMX)) GO TO 210
!         IF(IRETCD.NE.2 .AND. (ABS(DLTFP-DLTF) .LE. .1*ABS(DLTF))
!    +         .AND. (.NOT.NWTAKE) .AND. (DLT.LE. .99*STEPMX))
!         THEN
!
!           DOUBLE TRUST REGION AND CONTINUE GLOBAL STEP
!
            IRETCD=3
            DO 200 I=1,N
              XPLSP(I)=XPLS(I)
  200       CONTINUE
            FPLSP=FPLS
            DLT=MIN(2.*DLT,STEPMX)
!$          WRITE(IPR,959)
            GO TO 230
!         ELSE
!
!           ACCEPT XPLS AS NEXT ITERATE.  CHOOSE NEW TRUST REGION.
!
  210       CONTINUE
!$          WRITE(IPR,960)
            IRETCD=0
            IF(DLT.GT. .99*STEPMX) MXTAKE=.TRUE.
            IF(DLTF.LT. .1*DLTFP) GO TO 220
!           IF(DLTF.GE. .1*DLTFP)
!           THEN
!
!             DECREASE TRUST REGION FOR NEXT ITERATION
!
              DLT=.5*DLT
              GO TO 230
!           ELSE
!
!             CHECK WHETHER TO INCREASE TRUST REGION FOR NEXT ITERATION
!
  220         IF(DLTF.LE. .75*DLTFP) DLT=MIN(2.*DLT,STEPMX)
!           ENDIF
!         ENDIF
!       ENDIF
!     ENDIF
  230 CONTINUE
!$    WRITE(IPR,953)
!$    WRITE(IPR,956) IRETCD,MXTAKE,DLT,FPLS
!$    WRITE(IPR,957)
!$    WRITE(IPR,965) (XPLS(I),I=1,N)
      RETURN
!
!C951 FORMAT(55H TREGUP    RESET XPLS TO XPLSP. TERMINATION GLOBAL STEP)
!C952 FORMAT(26H TREGUP    FPLS TOO LARGE.)
!C953 FORMAT(38H0TREGUP    VALUES AFTER CALL TO TREGUP)
!C954 FORMAT(54H TREGUP    CANNOT FIND SATISFACTORY XPLS DISTINCT FROM,
!C   +       27H X.  TERMINATE GLOBAL STEP.)
!C955 FORMAT(53H TREGUP    REDUCE TRUST REGION. CONTINUE GLOBAL STEP.)
!C956 FORMAT(21H TREGUP       IRETCD=,I3/
!C   +       21H TREGUP       MXTAKE=,L1/
!C   +       21H TREGUP       DLT   =,E20.13/
!C   +       21H TREGUP       FPLS  =,E20.13)
!C957 FORMAT(32H TREGUP       NEW ITERATE (XPLS))
!C958 FORMAT(35H TREGUP    FPLS SUFFICIENTLY SMALL.)
!C959 FORMAT(54H TREGUP    DOUBLE TRUST REGION.  CONTINUE GLOBAL STEP.)
!C960 FORMAT(50H TREGUP    ACCEPT XPLS AS NEW ITERATE.  CHOOSE NEW,
!C   +       38H TRUST REGION.  TERMINATE GLOBAL STEP.)
!C961 FORMAT(18H TREGUP    IRETCD=,I5/
!C   +       18H TREGUP    FPLS  =,E20.13/
!C   +       18H TREGUP    FPLSP =,E20.13/
!C   +       18H TREGUP    DLTF  =,E20.13/
!C   +       18H TREGUP    SLP   =,E20.13)
!C962 FORMAT(18H TREGUP    RLN   =,E20.13)
!C963 FORMAT(18H TREGUP    DLTMP =,E20.13)
!C964 FORMAT(18H TREGUP    DLTFP =,E20.13/
!C   +       18H TREGUP    NWTAKE=,L1)
!C965 FORMAT(14H TREGUP       ,5(E20.13,3X))
      END SUBROUTINE TREGUP
      SUBROUTINE TRESTR(POINTR, SBRGNS, PONTRS, RGNERS)
!***BEGIN PROLOGUE TRESTR
!***PURPOSE TRESTR maintains a heap for subregions.
!***DESCRIPTION TRESTR maintains a heap for subregions.
!            The subregions are ordered according to the size of the
!            greatest error estimates of each subregion (RGNERS).
!
!   PARAMETERS
!
!     POINTR Integer.
!            The index for the subregion to be inserted in the heap.
!     SBRGNS Integer.
!            Number of subregions in the heap.
!     PONTRS Real array of dimension SBRGNS.
!            Used to store the indices for the greatest estimated errors
!            for each subregion.
!     RGNERS Real array of dimension SBRGNS.
!            Used to store the greatest estimated errors for each
!            subregion.
!
!***ROUTINES CALLED NONE
!***END PROLOGUE TRESTR
!
!   Global variables.
!
      INTEGER POINTR, SBRGNS
      DOUBLE PRECISION PONTRS(*), RGNERS(*)
!
!   Local variables.
!
!   RGNERR Intermediate storage for the greatest error of a subregion.
!   SUBRGN Position of child/parent subregion in the heap.
!   SUBTMP Position of parent/child subregion in the heap.
!
      INTEGER SUBRGN, SUBTMP
      DOUBLE PRECISION RGNERR
!
!***FIRST PROCESSING STATEMENT TRESTR
!
      RGNERR = RGNERS(POINTR)
      IF ( POINTR .EQ. PONTRS(1)) THEN
!
!        Move the new subregion inserted at the top of the heap
!        to its correct position in the heap.
!
         SUBRGN = 1
 10      SUBTMP = 2*SUBRGN
         IF ( SUBTMP .LE. SBRGNS ) THEN
            IF ( SUBTMP .NE. SBRGNS ) THEN
!
!              Find maximum of left and right child.
!
               IF ( RGNERS(INT(PONTRS(SUBTMP))) .LT.   &
                    RGNERS(INT(PONTRS(SUBTMP+1))) ) SUBTMP = SUBTMP + 1
            ENDIF
!
!           Compare maximum child with parent.
!           If parent is maximum, then done.
!
            IF ( RGNERR .LT. RGNERS(INT(PONTRS(SUBTMP))) ) THEN
!
!              Move the pointer at position subtmp up the heap.
!
               PONTRS(SUBRGN) = PONTRS(SUBTMP)
               SUBRGN = SUBTMP
               GO TO 10
            ENDIF
         ENDIF
      ELSE
!
!        Insert new subregion in the heap.
!
         SUBRGN = SBRGNS
 20      SUBTMP = SUBRGN/2
         IF ( SUBTMP .GE. 1 ) THEN
!
!           Compare child with parent. If parent is maximum, then done.
!
            IF ( RGNERR .GT. RGNERS(INT(PONTRS(SUBTMP))) ) THEN
!
!              Move the pointer at position subtmp down the heap.
!
               PONTRS(SUBRGN) = PONTRS(SUBTMP)
               SUBRGN = SUBTMP
               GO TO 20
            ENDIF
         ENDIF
      ENDIF
      PONTRS(SUBRGN) = POINTR
!
!***END TRESTR
!
      RETURN
      END SUBROUTINE TRESTR
      SUBROUTINE TRIA25(X,M,N,RIGHT,X2,RIGHT2,IBUGA3)
!
!     PURPOSE--COMPUTE THE TRIANGULARIZED EQUIVALENT
!              OF THE M BY N MATRIX X.
!              THE TRIANGULARIZED EQUIVALENT
!              WILL BE FOUND IN THE UPPER RIGHT TRIANGLE
!              OF THE MATRIX X2.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION MATRIX
!                                WITH M ROWS AND N COLUMNS
!                                WHOSE TRIANGULARIZED
!                                EQUIVALENT IS DESIRED.
!                     --M      = THE INTEGER NUMBER OF
!                                ROWS IN X.
!                     --N      = THE INTEGER NUMBER OF
!                                COLUMNS IN X.
!                     --RIGHT  = THE SINGLE PRECISION VECTOR
!                                CONTAINING THE 'RIGHT-HAND
!                                SIDE' OF THE EQUATION.
!                     --IBUGA3 = A HOLLERITH BUG PARAMETER
!     OUTPUT ARGUMENTS--X2     = THE SINGLE PRECISION MATRIX
!                                WITH M ROWS AND N COLUMNS
!                                WITH THE TRIANGULARIZED
!                                EQUIVALENT OF X IN THE
!                                UPPER RIGHT TRIANGLE
!                                AND WITH ZEROS ELSEWHERE.
!                     --RIGHT2 = THE SINGLE PRECISION VECTOR
!                                CONTAINING THE ORIGINAL
!                                'RIGHT-HAND SIDE' BUT MODIFIED
!                                ACCORDING TO THE TRIANGULARIZATION
!                                THAT OCCURRED ON THE
!                                LEFT-HAND SIDE
!                                SIDE OF THE EQUATION.
!     NOTE--THE INPUT MATRIX X IS UNCHANGED
!           BY THIS SUBROUTINE.
!     NOTE--THE DIMENSIONS OF X AND X2 MUST BE THE SAME
!           IN THE CALLING ROUTINE AS IN THIS SUBROUTINE.
!           THEY HAVE BEEN SET HEREIN TO 25 BY 25,
!           AND HENCE THE 25 IN THE NAME OF THIS SUBROUTINE (TRIA25).
!     NOTE--TRIA25 IS IDENTICAL TO TRIA50 AND TRIANG
!           EXCEPT FOR THE DIMENSIONS.
!     NOTE--A CALL TO TRIA25 IS TYPICALLY
!           FOLLOWED BY A CALL TO BACK25
!           SO AS TO CARRY OUT THE
!           BACKSOLVING FOR THE COEFFICIENTS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1977.
!     UPDATED         --JULY      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION X(25,25)
      DIMENSION RIGHT(*)
      DIMENSION X2(25,25)
      DIMENSION RIGHT2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TRIA'
      ISUBN2='25  '
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF TRIA25--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)M,N,IBUGA3
   52 FORMAT('M,N,IBUGA3 = ',2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,M
      WRITE(ICOUT,56)I,(X(I,J),J=1,N)
   56 FORMAT('I,X(I,.)   = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      DO 60 I=1,M
      WRITE(ICOUT,61)I,RIGHT(I)
   61 FORMAT('I,RIGHT(I) = ',I8,E10.3)
      CALL DPWRST('XXX','BUG ')
   60 CONTINUE
   90 CONTINUE
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  COPY THE X MATRIX INTO THE X2 MATRIX.          **
!               **  COPY THE VECTOR RIGHT INTO THE VECTOR RIGHT2.  **
!               *****************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 100 I=1,M
      DO 200 J=1,N
      X2(I,J)=X(I,J)
  200 CONTINUE
      RIGHT2(I)=RIGHT(I)
  100 CONTINUE
!
!               *********************************************
!               **  STEP 2--                               **
!               **  DETERMINE K = THE MINIMUM OF M AND N.  **
!               *********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      K=M
      IF(N.LT.M)K=N
      IF(K.EQ.1)GO TO 9000
!
!               *********************************************************
!               **  STEP 3--                                           **
!               **  BEGIN GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.  **
!               **  OPERATE ON ONE ROW (OR COLUMN) AT A TIME.          **
!               **  THE ROW (OR COLUMN) OF INTEREST IS COLUMN J.       **
!               *********************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      KM1=K-1
      DO 400 J=1,KM1
      JP1=J+1
!
!               ************************************************
!               **  STEP 3.1--                                **
!               **  FOR COLUMN J,                             **
!               **  DETERMINE THE ROW (ON OR BELOW DIAGONAL)  **
!               **  THAT HAS THE LARGEST ABSOLUTE VALUE.      **
!               **  THIS ROW WILL BE DESIGNATED AS ROW I2.    **
!               ************************************************
!
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=J
      DO 600 I=JP1,M
      IF(ABS(X2(I,J)).GT.ABS(X2(I2,J)))I2=I
  600 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,610)J,I2
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,615)X2(I2,J)
  610 FORMAT('COLUMN J = ',I2,'  MAX FOUND IN ROW I2 = ',I2)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
  615 FORMAT('MAX VALUE = ',E12.5)
!
!               ****************************************************
!               **  STEP 3.2--                                    **
!               **  INTERCHANGE ROW I2 WITH ROW J                 **
!               **  BELOW AND TO THE RIGHT                        **
!               **  OF THE DIAGONAL ELEMENT X2(J,J) (INCLUSIVE).  **
!               ****************************************************
!
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(I2.EQ.J)GO TO 700
      DO 800 J2=J,N
      HOLD=X2(J,J2)
      X2(J,J2)=X2(I2,J2)
      X2(I2,J2)=HOLD
  800 CONTINUE
      HOLD=RIGHT2(J)
      RIGHT2(J)=RIGHT2(I2)
      RIGHT2(I2)=HOLD
  700 CONTINUE
!
!               ****************************************************************
!               **  STEP 3.3--
!               **  MODIFY THE ROWS BELOW ROW J (& ONLY TO THE RIGHT OF COLUMN J
!               **  ALSO, ELIMINATE (SET TO 0) ELEMENTS IN COLUMN J
!               **  BELOW X2(J,J)
!               ****************************************************************
!
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1100 I=JP1,M
      FACTOR=X2(I,J)/X2(J,J)
      DO 1200 J2=JP1,N
      X2(I,J2)=X2(I,J2)-FACTOR*X2(J,J2)
 1200 CONTINUE
      RIGHT2(I)=RIGHT2(I)-FACTOR*RIGHT2(J)
 1100 CONTINUE
!
      DO 1300 I=JP1,M
      X2(I,J)=0.0
 1300 CONTINUE
!
      IF(IBUGA3.EQ.'OFF')GO TO 1409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1400)J
 1400 FORMAT('***** IN TRIANG, AFTER OPERATING ON COLUMN ',I6)
      CALL DPWRST('XXX','BUG ')
      DO 1405 I=1,M
      WRITE(ICOUT,1410)I,(X2(I,J3),J3=1,N),RIGHT2(I)
 1410 FORMAT('I,X2(I,.),RIGHT2(I) = ',I8,11E8.1)
      CALL DPWRST('XXX','BUG ')
 1405 CONTINUE
 1409 CONTINUE
!
  400 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF TRIA25--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)M,N,IBUGA3
 9012 FORMAT('M,N,IBUGA3 = ',2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,M
      WRITE(ICOUT,9016)I,(X2(I,J),J=1,N)
 9016 FORMAT('I,X2(I,.)  = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      DO 9020 I=1,M
      WRITE(ICOUT,9021)I,RIGHT2(I)
 9021 FORMAT('I,RIGHT2(I)= ',I8,E10.3)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE TRIA25
      SUBROUTINE TRICUB(RES,N,IWRITE,WEIGHT,IBUGA3,IERROR)
!
!     PURPOSE--DETERMINE THE N VERTICAL (ROBUST) WEIGHTS WEIGHT(.)
!              BASED ON A TRICUBE WEIGHTING SCHEME OF
!              THE RESIDUALS IN RES(.).
!     NOTE--IF ALL INPUT RESIDUALS ARE ZERO, THIS SUBROUTINE
!           WILL OUTPUT ALL WEIGHTS AS UNITY.
!     REFERENCE--CHAMBERS, ET AL.  GRAPHICAL METHODS FOR DATA ANALYSIS.
!                WADSWORTH, 1983, PAGES 98-101, 122-123.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY   1988
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
      DIMENSION RES(*)
      DIMENSION WEIGHT(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TRIC'
      ISUBN2='UB  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRICUB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IERROR,IWRITE,N
   52   FORMAT('IBUGA3,IERROR,IWRITE = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GE.1)THEN
          DO 61 I=1,N
           WRITE(ICOUT,62)I,RES(I)
   62      FORMAT('I,RES(I) = ',I8,G15.7)
           CALL DPWRST('XXX','BUG ')
   61    CONTINUE
        ENDIF
      ENDIF
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
  111   FORMAT('***** ERROR IN TRICUB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT FULL SAMPLE SIZE FOR WHICH TRICUBE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)
  114   FORMAT('      WEIGHTS ARE TO BE COMPUTED, MUST BE 1 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)N
  116   FORMAT('      THE FULL SAMPLE SIZE N = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***************************************************
!               **  STEP 11--                                    **
!               **  COMPUTE THE TRICUBE WEIGHTING                **
!               **     1) COMPUTE ABSOLUTE VALUE OF RESIDUALS    **
!               **     2) COMPUTE MEDIAN ABSOLUTE VALUE RESIDUAL **
!               **     3) COMPUTE CUTOFF = +-6*M.A.R.            **
!               **     4) ASSIGN 0 WEIGHTS OUTSIDE OF REGION     **
!               **     5) ASSIGN TRICUBES INSIDE OF REGION       **
!               ***************************************************
!
      DO 1100 I=1,N
        WEIGHT(I)=ABS(RES(I))
 1100 CONTINUE
!
      CALL SORT(WEIGHT,N,WEIGHT)
      IEVODD=N-(N/2)*2
      NMID=N/2
      NMIDP1=NMID+1
      XMEDAR=0.0
      IF(IEVODD.EQ.0)XMEDAR=(WEIGHT(NMID)+WEIGHT(NMIDP1))/2.0
      IF(IEVODD.EQ.1)XMEDAR=WEIGHT(NMIDP1)
!
      IF(XMEDAR.EQ.0.0)THEN
        CONST=(-999.0)
        DO 1111 I=1,N
          WEIGHT(I)=1.0
 1111   CONTINUE
      ELSE
        CONST=6.0*XMEDAR
        DO 1121 I=1,N
          U=RES(I)/CONST
          U2=ABS(U)
          WEIGHT(I)=0.0
          IF(-1.0.LE.U.AND.U.LE.1.0)WEIGHT(I)=(1.0-U2**3)**3
 1121   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF TRICUB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,XMEDAR
 9014   FORMAT('IERROR,XMEDAR = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(N.GE.1)THEN
          DO 9021 I=1,N
            WRITE(ICOUT,9022)I,RES(I),WEIGHT(I)
 9022       FORMAT('I,RES(I),WEIGHT(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE TRICUB
      double precision function trigam(x, ifault)
      implicit double precision (a-h,o-z)
!
!        algorithm as121   Appl. Statist. (1978) vol 27, no. 1
!
!        calculates trigamma(x) = d**2(log(gamma(x))) / dx**2
!
      double precision a, b, one, half, b2, b4, b6,b8, x, y, z, zero
      data a, b, one, half /1.0d-4, 5.0d0, 1.0d0, 0.5d0/
      data zero /0.0d0/
!
!        b2, b4, b6 and b8 are Bernoulli numbers
!
      data b2, b4, b6,b8   &
      /0.1666666667d0, -0.03333333333d0, 0.02380952381, -0.03333333333/
!
!        check for positive value of x
!
      trigam = zero
      ifault = 1
      if (x.le.zero) return
      ifault = 0
      z = x
!
!        use small value approximation if x .le. a
!
      if (z .gt. a) go to 10
      trigam = one / (z * z)
      return
!
!        increase argument to (x+i) .ge. b
!
   10 if (z .ge. b) go to 20
      trigam = trigam + one / (z * z)
      z = z + one
      go to 10
!
!        apply asymptotic formula if argument .ge. b
!
   20 y = one / (z * z)
      trigam = trigam + half * y +   &
       (one + y * (b2 + y * (b4 + y * (b6 + y * b8)))) / z
      return
      end
                                                                                                                                  
      SUBROUTINE TRIGD1(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
!
!     PURPOSE--COMPUTE DERIVATIVES FOR
!              THE 6 (CIRCULAR) TRIGONOMETRIC FUNCTIONS.
!
!     NOTE--LF11 = CODED SIN FUNCTION
!           LF12 = CODED COS FUNCTION
!           LF13 = CODED TAN FUNCTION
!           LF14 = CODED COT FUNCTION
!           LF15 = CODED SEC FUNCTION
!           LF16 = CODED CSC FUNCTION
!
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
!
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      I1P1=I1+1
      I1P2=I1+2
!
      IF(IHLF1.EQ.'SIN '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'COS '.AND.IHLF2.EQ.'    ')GO TO 620
      IF(IHLF1.EQ.'TAN '.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'COT '.AND.IHLF2.EQ.'    ')GO TO 640
      IF(IHLF1.EQ.'SEC '.AND.IHLF2.EQ.'    ')GO TO 650
      IF(IHLF1.EQ.'CSC '.AND.IHLF2.EQ.'    ')GO TO 660
!
!     TREAT THE SINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE COSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE TANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE COTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE SECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE COSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
  980 CONTINUE
  985 CONTINUE
!
      RETURN
      END SUBROUTINE TRIGD1
      SUBROUTINE TRIGD2(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
!
!     PURPOSE--COMPUTE DERIVATIVES FOR
!              THE 6 INVERSE (CIRCULAR) TRIGONOMETRIC FUNCTIONS.
!
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
!
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      I1P1=I1+1
      I1P2=I1+2
!
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'IN  ')GO TO 710
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OS  ')GO TO 720
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'AN  ')GO TO 730
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OT  ')GO TO 740
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'EC  ')GO TO 750
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SC  ')GO TO 760
!
!     TREAT THE ARCSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE ARCCOSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE ARCTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE ARCCOTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE ARCSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE ARCCOSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
  980 CONTINUE
  985 CONTINUE
!
      RETURN
      END SUBROUTINE TRIGD2
      SUBROUTINE TRIGD3(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
!
!     PURPOSE--COMPUTE DERIVATIVES FOR
!              THE 6 HYPERBOLIC TRIGONOMETRIC FUNCTIONS.
!
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
!
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      I1P1=I1+1
      I1P2=I1+2
!
      IF(IHLF1.EQ.'SINH'.AND.IHLF2.EQ.'    ')GO TO 810
      IF(IHLF1.EQ.'COSH'.AND.IHLF2.EQ.'    ')GO TO 820
      IF(IHLF1.EQ.'TANH'.AND.IHLF2.EQ.'    ')GO TO 830
      IF(IHLF1.EQ.'COTH'.AND.IHLF2.EQ.'    ')GO TO 840
      IF(IHLF1.EQ.'SECH'.AND.IHLF2.EQ.'    ')GO TO 850
      IF(IHLF1.EQ.'CSCH'.AND.IHLF2.EQ.'    ')GO TO 860
!
!     TREAT THE HYPERBOLIC SINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC COSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC TANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC COTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC SECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC COSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
  980 CONTINUE
  985 CONTINUE
!
      RETURN
      END SUBROUTINE TRIGD3
      SUBROUTINE TRIGD4(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
!
!     PURPOSE--COMPUTE DERIVATIVES FOR
!              THE 6 INVERSE HYPERBOLIC TRIGONOMETRIC FUNCTIONS.
!
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 ITYPE
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
!
      DIMENSION IFUNZ1(*)
      DIMENSION IFUNZ2(*)
      DIMENSION IDERZ1(*)
      DIMENSION IDERZ2(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      I1P1=I1+1
      I1P2=I1+2
!
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'INH ')GO TO 910
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OSH ')GO TO 920
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'ANH ')GO TO 930
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OTH ')GO TO 940
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'ECH ')GO TO 950
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SCH ')GO TO 960
!
!     TREAT THE HYPERBOLIC ARCSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC ARCCOSINE CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC ARCTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC ARCCOTANGENT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC ARCSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
!     TREAT THE HYPERBOLIC ARCCOSECANT CASE
!
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
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
  980 CONTINUE
  985 CONTINUE
!
      RETURN
      END SUBROUTINE TRIGD4
      SUBROUTINE TRIMME(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,XTEMP,   &
                        IUPPER,XTRIM,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE TRIMMED MEAN = THE
!              SAMPLE (ON EACH SIDE) TRIMMED MEAN
!              OF THE DATA IN THE INPUT VECTOR X.
!      NOTE--PROP1 % OF THE DATA IS TRIMMED FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS TRIMMED FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMMED FROM LEFT SIDE.
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMMED FROM RIGHT SIDE.
!     OUTPUT ARGUMENTS--XTRIM  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE TRIMMED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TRIMMED MEAN.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 15000.
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
!                 PHONE--301-921-3651
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
      ISUBN1='TRIM'
      ISUBN2='ME  '
      IERROR='NO'
!
!CCCC IUPPER=1000
!
      NPROP1=0
      NPROP2=0
      NPROP3=0
      ISTART=0
      ISTOP=0
      DSUM=0.0D0
      DK=0.0D0
      PROP3=0.0
      XTRIM1=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRIMME--')
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
!               ********************************
!               **  COMPUTE THE TRIMMED MEAN  **
!               ********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
          GO TO 9000
        ENDIF
      ENDIF
!
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
      XTRIM=HOLD
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)
  137   FORMAT('ALL DATA VALUES HAVE THE SAME VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)XTRIM
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
!               *********************************
!               **  STEP 2--                   **
!               **  COMPUTE THE TRIMMED MEAN.  **
!               *********************************
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
      K=0
      DO 200 I=ISTART,ISTOP
        K=K+1
        DX=XTEMP(I)
        DSUM=DSUM+DX
  200 CONTINUE
      NPROP3=K
      DK=K
      XTRIM=DSUM/DK
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
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE TRIMMED   FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE TRIMMED   FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA REMAIN IN MIDDLE AFTER TRIMMING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRIM
  821   FORMAT('THE TRIMMED MEAN OF THE ',I8,' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE TRIMME
      SUBROUTINE TRIMSD(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,IUPPER,   &
                        XTEMP,XTRIM,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SAMPLE TRIMMED STANDARD DEVIATION
!              OF THE DATA IN THE INPUT VECTOR X.
!      NOTE--PROP1 % OF THE DATA IS TRIMSDD FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS TRIMSDD FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMSDD FROM LEFT SIDE.
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMSDD FROM RIGHT SIDE.
!     OUTPUT ARGUMENTS--XTRIM  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE TRIMMED SD.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TRIMMED SD.
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
!     VERSION NUMBER--2007.5
!     ORIGINAL VERSION--MAY       2007.
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
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
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
      ISUBN1='TRIM'
      ISUBN2='SD  '
      IERROR='NO'
!
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
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IMSD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TRIMSD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NTRIM1,NTRIM2
   52   FORMAT('IBUGA3,ISUBRO,N,NTRIM1,NTRIM2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PROP1,PROP2
   54   FORMAT('PROP1,PROP2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************
!               **  COMPUTE THE TRIMMED SD    **
!               ********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
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
  115   FORMAT('      2 AND ',I8,' (INCLUSIVELY).  SUCH WAS NOT THE ',   &
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
!               *********************************
!               **  STEP 2--                   **
!               **  COMPUTE THE TRIMMED SD.    **
!               *********************************
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
      K=0
      DO 200 I=ISTART,ISTOP
        K=K+1
        DX=XTEMP(I)
        DSUM=DSUM+DX
  200 CONTINUE
      NPROP3=K
      DK=K
      DMEAN=DSUM/DK
!
      DSUM=0.0D0
      DO 300 I=ISTART,ISTOP
        DX=XTEMP(I)
        DSUM=DSUM+(DX-DMEAN)**2
  300 CONTINUE
      DVAR=DSUM/(DK-1.0D0)
      DSD=0.0D0
      IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
      XTRIM=REAL(DSD)
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
        WRITE(ICOUT,811)PROP1,NPROP1
  811   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE TRIMMED FROM BELOW')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)PROP2,NPROP2
  812   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA WERE TRIMMED FROM ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)PROP3,NPROP3
  813   FORMAT(8X,F10.4,' PERCENT (= ',I8,' OBSERVATIONS) ',   &
               'OF THE DATA REMAIN AFTER TRIMMING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRIM
  821   FORMAT('THE TRIMMED STANDARD DEVIATION OF THE ',I8,   &
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
!
      RETURN
      END SUBROUTINE TRIMSD
      SUBROUTINE TRIMSE(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,   &
                        XTEMP,XTEMP2,IUPPER,XTRMSE,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE
!              SAMPLE STANDARD ERROR OF THE TRIMMED MEAN
!              OF THE DATA IN THE INPUT VECTOR X.
!      NOTE--PROP1 % OF THE DATA IS TRIMSED FROM THE LEFT SIDE;
!            PROP2 % OF THE DATA IS TRIMSED FROM THE RIGHT SIDE.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --PROP1  = THE SINGLE PRECISION PROPORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMSED FROM LEFT SIDE.
!                     --PROP2  = THE SINGLE PRECISION PORTION (0 TO 100)
!                                OF OBSERVATIONS TO BE TRIMSED FROM RIGHT SIDE.
!     OUTPUT ARGUMENTS--XTRMSE  = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SAMPLE TRIMSED MEAN.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TRIMMED MEAN STANDARD ERROR.
!     OTHER DATAPAC   SUBROUTINES NEEDED--WINSOR.
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
!               --RAND R. WILCOX, INTRODUCTION TO ROBUST ESTIMATION
!                 AND HYPOTHESIS TESTING, ACADEMIC PRESS, 1997.
!                 THE FORMULA FOR THE STANDARD ERROR IS TAKEN FROM
!                 PAGE 36-38 OF THIS SOURCE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--WE DO NOT NEED TO ACTUALLY COMPUTE THE TRIMMED MEAN
!           TO OBTAIN THE STANDARD ERROR.  THE STANDARD ERROR IS:
!               s(w)/[(1-2*LAMBDA)*SQRT(N)]
!           WHERE s(w) IS THE WINSORIZED STANDARD DEVIATION AND
!           LAMBDA IS THE AMOUNT OF TRIMMING (AS A FRACTION).
!           NOTE THAT WE USE PROP1 + PROP2 RATHER THAN LAMBDA.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--2002.7
!     ORIGINAL VERSION--JULY      2002.
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
      DIMENSION XTEMP(*)
      DIMENSION XTEMP2(*)
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
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.2 .OR. N.GT.IUPPER)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE')
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
        GO TO 9000
      ENDIF
!
      HOLD=X(1)
      DO 135 I=2,N
      IF(X(I).NE.HOLD)GO TO 139
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
      GO TO 9000
  139 CONTINUE
!
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
!
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
!
!               ***********************************
!               **  STEP 2: WINSORIZE THE DATA   **
!               ***********************************
!
      NTRIM1=-1
      NTRIM2=-1
      CALL WINSOR(X,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,   &
                  XTEMP,IUPPER,XTEMP2,   &
                  IBUGA3,ISUBRO,IERROR)
      CALL SD(XTEMP2,N,IWRITE,WINVAR,IBUGA3,IERROR)
!
      ALAM=(PROP1 + PROP2)/100.0
      AN=REAL(N)
!
      XTRMSE=WINVAR/((1.0-ALAM)*SQRT(AN))
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
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,820)PROP1,PROP2
  820   FORMAT(F7.2,'% TRIMMED BELOW AND ',F7.2,'% TRIMMED ABOVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,821)N,XTRMSE
  821   FORMAT('THE STANDARD ERROR OF THE TRIMMED MEAN OF THE ',I8,   &
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
!
      RETURN
      END SUBROUTINE TRIMSE
      SUBROUTINE TRICDF(X,C,ALOWLM,AUPPLM,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TRIANGULAR DISTRIBUTION.
!              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
!              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
!              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
!              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME
!              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
!              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION
!              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
!              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
!              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
!              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
!              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
!              AND FOR C = 0
!              F(X) = 1+X                    FOR -1 LE X LE 0
!              F(X) = 1-X                    FOR  0 LT X LE 1
!              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
!              THE DISTRIBUTION THAT RESULTS
!              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
!              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
!              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
!              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
!              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
!              SO AS TO BE SYMMETRIC ABOUT 0.)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --C      = THE SINGLE PRECISION SHAPE PARAMETER
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
!     REFERENCES--EVANS, HASTINGS, PEACOCK, STATISTICAL DISTRIBUTIONS
!                 2ND ED.--CHAPTER 39.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
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
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--SEPTEMBER 1994.
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
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      IF(C.LT.A.OR. C.GT.B)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
!
      IF(X.LE.A)THEN
        CDF=0.0
        GO TO 9000
      ELSEIF(X.GE.B)THEN
        CDF=1.0
        GO TO 9000
      ENDIF
!
   12 FORMAT(   &
      '***** ERROR--THE SECOND ARGUMENT TO TRICDF IS OUTSIDE THE')
    3 FORMAT(   &
      '      (',G15.7,',',G15.7,') INTERVAL.')
   22 FORMAT(   &
      '***** ERROR--THE THIRD AND FOURTH INPUT ARGUMENTS TO THE ',   &
      'TRICDF SUBROUTINE')
   23 FORMAT(   &
      '      (THE LOWER AND UPPER LIMITS) ARE EQUAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LE.C)THEN
        CDF=(X-A)**2/((B-A)*(C-A))
      ELSE
        CDF=1.0 - (B-X)**2/((B-A)*(B-C))
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRICDF
      DOUBLE PRECISION FUNCTION TRIFUN (Q,X)
!
!     PURPOSE--THIS ROUTINE IS USED IN FINDING THE ROOT OF THE
!              FOLLOWING EQUATION:
!
!                  G(q) = (m - a(q))/(b(q) - a(q))
!                       = (m - a(p))*(1 - SQRT((1-r)/(1-q)))/
!                         (b(r) - m)*(1 - SQRT(p/q)) +
!                         (m - a(p))*(1 - SQRT((1-r)/(1-q)))
!
!              THIS IS USED TO FIND ESTIMATES FOR THE LOWER/UPPER
!              BOUND PARAMETERS OF THE TRIANGULAR DISTRIBUTION
!              USING THE QUANTILE METHOD OF KOTZ AND VAN DORP.
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 28-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/5
!     ORIGINAL VERSION--MAY        2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION Q
      DOUBLE PRECISION X(*)
!
      DOUBLE PRECISION M
      DOUBLE PRECISION P
      DOUBLE PRECISION R
      DOUBLE PRECISION AP
      DOUBLE PRECISION BR
      COMMON/TRICOM/M,P,R,AP,BR
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'IFUN')THEN
        WRITE(ICOUT,52)Q,X(1)
   52   FORMAT('Q,X(1) = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DTERM1=DSQRT((1.0D0 - R)/(1.0D0 - Q))
      DTERM2=(M - AP)*(1.0D0 - DTERM1)
      DTERM3=(BR - M)*(1.0D0 - DSQRT(P/Q)) + (M - AP)*(1.0D0 - DTERM1)
!
      TRIFUN=DTERM2/DTERM3 - Q
!
      RETURN
      END FUNCTION TRIFUN 
      DOUBLE PRECISION FUNCTION TRIFU2 (A,B,IR,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE M(A,B,R)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT SOLVES
!
!              Mhat(r) = PROD[i=1 to r-1][(Z(i)-a)/(Z(r)-a]*
!                        PROD[i=r+1 to n][(b - Z(i))/(b-Z(r))]
!
!              NOTE THAT THIS FUNCTION COMPUTES Mhat FOR A
!              SPECIFIC VALUE OF R AND IT ASSUMES THE DATA
!              IS ALREADY SORTED.
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER IR
      INTEGER N
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
      DOUBLE PRECISION DPROD2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      DPROD1=1.0D0
      DPROD2=1.0D0
!
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO 1210 J=1,IRM1
          DTERM1=X(J) - A
          DTERM2=X(IR) - A
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
!
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO 1220 J=IRM1,N
          DTERM1=B - X(J)
          DTERM2=B - X(IR)
          DPROD2=DPROD2*(DTERM1/DTERM2)
 1220   CONTINUE
      ENDIF
      TRIFU2=DPROD1*DPROD2
!
      RETURN
      END FUNCTION TRIFU2 
      DOUBLE PRECISION FUNCTION TRIFU3(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE G(A,B)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G(A,B) = LOG{M(A,B,RHAT(A,B)} - N*LOG(B-A)
!
!              IT IS ASSUMED THAT THE DATA IS ALREADY SORTED.
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DG
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      DG=DLOG(DMAXMR) - DBLE(N)*DLOG(B-A)
      TRIFU3=DG
!
      RETURN
      END FUNCTION TRIFU3
      DOUBLE PRECISION FUNCTION TRIFU4(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO A)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G'(A,B)(A) = M'(A,B,RHAT(A,B))(A)/M(A,B,RHAT(A,B)
!                           + N/(B-A)
!
!              WHERE
!
!              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
!                   {SUM[j=1 to Rhat-1]
!                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DSUM1=0.0D0
!
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO 1210 J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(J) - A)
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
!
      DMHATP=DMAXMR*DSUM1
      TRIFU4=(DMHATP/DMAXMR) + DBLE(N)/DBLE(B-A)
!
      RETURN
      END FUNCTION TRIFU4
      DOUBLE PRECISION FUNCTION TRIFU5(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO B)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G'(A,B)(B) = M'(A,B,RHAT(A,B))(B)/M(A,B,RHAT(A,B)
!                           - N/(B-A)
!
!              WHERE
!
!              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
!                   {SUM[j=RHAT+1 to N]
!                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DSUM1=0.0D0
!
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO 1210 J=IRM1,N
          DTERM1=X(J) - X(IR)
          DTERM2=(B - X(IR))*(B - X(J))
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
!
      DMHATP=DMAXMR*DSUM1
      TRIFU5=(DMHATP/DMAXMR) - DBLE(N)/(B-A)
!
      RETURN
      END FUNCTION TRIFU5
      DOUBLE PRECISION FUNCTION TRIFU6(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE M(A,B,RHAT) (WITH RESPECT TO A)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
!                   {SUM[j=1 to Rhat-1]
!                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      DMAXMR=0.0D0
      IINDX=0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DPROD1=1.0D0
!
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO 1210 J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(I) - A)
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
!
      TRIFU6=DMAXMR*DPROD1
!
      RETURN
      END FUNCTION TRIFU6
      DOUBLE PRECISION FUNCTION TRIFU7(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE M(A,B,RHAT) (WITH RESPECT TO B)
!              FUNCTION IN THE TRIANGULAR MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
!                   {SUM[j=RHAT+1 to N]
!                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DPROD1
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DPROD1=1.0D0
!
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO 1210 J=IRM1,N
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(B - X(IR))*DBLE(B - X(I))
          DPROD1=DPROD1*(DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
!
      TRIFU7=DMAXMR*DPROD1
!
      RETURN
      END FUNCTION TRIFU7
      SUBROUTINE TRIFU8(A,B,X,IR,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE FUNCTION
!
!              B(a) = Max{r = 1, ..., n-1}[f(b)(a,r)]
!
!              WHERE
!
!              f(b)a,r) = {X(r+1) - X(r)*
!                         (((X(r)-a)/(X(r+1)-a))**r)**(n-r)/
!                         {1 - (((X(r)-a)/(X(r+1)-a))**r)**(n-r)}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXFB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DX2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXFB=0.0D0
      DO 100 I=1,N-1
        IR=I
        IRP1=IR+1
        DX1=X(IR)
        DX2=X(IRP1)
        DTERM1=(DX1-A)/(DX2-A)
        DTERM2=DTERM1**IR
        DTERM3=1.0D0/DBLE(N-IR)
        DTERM4=DTERM2**DTERM3
        DTEMP1=(DX2 - DX1*DTERM4)/(1.0D0 - DTERM4)
!
        IF(DTEMP1.GT.DMAXFB)THEN
          DMAXFB=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
      B=DMAXFB
!
      RETURN
      END SUBROUTINE TRIFU8
      SUBROUTINE TRIML1(Y,N,MAXNXT,   &
                        Z,XTEMP,DTEMP1,   &
                        XMIN,XMAX,XMEAN,XSD,   &
                        A,B,ALOWQN,AUPPQN,   &
                        AQUANT,BQUANT,   &
                        AML,BML,CML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MOMENT AND MAXIMUM LIKELIHOOD
!              ESTIMATES FOR THE TRIANGULAR DISTRIBUTION FOR THE RAW DATA
!              CASE (I.E., NO CENSORING AND NO GROUPING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLUN WILL GENERATE THE OUTPUT
!              FOR THE TRIANGULAR MLE COMMAND).
!
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                CHAPTER 1.
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
      CHARACTER*4 IQUAME
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      INTEGER IFLAG
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION XTEMP(*)
!
      DOUBLE PRECISION DTEMP1(*)
!
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
!
      DOUBLE PRECISION DAE
      DOUBLE PRECISION DRE
      DOUBLE PRECISION DXSTRT
      DOUBLE PRECISION DXLOW
      DOUBLE PRECISION DXUP
!
      DOUBLE PRECISION DM
      DOUBLE PRECISION DP
      DOUBLE PRECISION DR
      DOUBLE PRECISION DAP
      DOUBLE PRECISION DBR
      COMMON/TRICOM/DM,DP,DR,DAP,DBR
!
      DOUBLE PRECISION DPROD1
      DOUBLE PRECISION DPROD2
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DMAXMR
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DLOWBK
      DOUBLE PRECISION DUPPBK
      DOUBLE PRECISION DLOWAK
      DOUBLE PRECISION DUPPAK
      DOUBLE PRECISION DBK
      DOUBLE PRECISION DMK
      DOUBLE PRECISION DGK
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TRIM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
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
!               **  FOR TRIANGULAR MLE ESTIMATE         **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'IML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='TRIANGULAR'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!     USE METHOD OF KOTZ AND VAN DORP (PP. 11-16) TO COMPUTE THE
!     MODE (WHICH IS THE MAXIMUM LIKELIHOOD ESTIMATE OF THE
!     THRESHOLD PARAMETER.
!
!     STEP 1: SORT THE DATA AND STANDARDIZE (Y = A + (B-A)*Y
!             WHERE A AND B ARE THE LOWER AND UPPER LIMITS).
!             USE THE DATA MINIMUM AND MAXIMUM AS THE INITIAL
!             ESTIMATES OF THESE PARAMETERS.
!
      CALL SORT(Y,N,Y)
      DO 1110 I=1,N
        Z(I)=XMIN + (XMAX-XMIN)*Y(I)
 1110 CONTINUE
!
!     STEP 2: COMPUTE
!
!             Mhat(r) = PROD[i=1 to r-1][(Z(i)-a)/(Z(r)-a]*
!                       PROD[i=r+1 to n][(b - Z(i))/(b-Z(r))]
!
      DMAXMR=0.0D0
      IINDX=0
      EPS=1.0E-05
      IF(A.LT.XMIN .AND. B.GT.XMAX)THEN
        CONTINUE
      ELSE
        A=XMIN - EPS
        B=XMAX + EPS
      ENDIF
!
      DO 1200 I=1,N
!
        DPROD1=1.0D0
        DPROD2=1.0D0
!
        IR=I
        IRM1=IR-1
        IF(IRM1.GE.1)THEN
          DO 1210 J=1,IRM1
            DTERM1=DBLE(Z(J) - A)
            DTERM2=DBLE(Z(IR) - A)
            DPROD1=DPROD1*(DTERM1/DTERM2)
 1210     CONTINUE
        ENDIF
!
        IRM1=IR+1
        IF(IRM1.LE.N)THEN
          DO 1220 J=IRM1,N
            DTERM1=DBLE(B - Z(J))
            DTERM2=DBLE(B - Z(IR))
            DPROD2=DPROD2*(DTERM1/DTERM2)
 1220     CONTINUE
        ENDIF
!
        DTERM3=DPROD1*DPROD2
        IF(DTERM3.GT.DMAXMR)THEN
          DMAXMR=DTERM3
          IINDX=IR
        ENDIF
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IML1')THEN
          WRITE(ICOUT,1240)I,DPROD1,DPROD2,DTERM3
 1240     FORMAT('I,DPROD1,DPROD2,DTERM3=',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 1200 CONTINUE
!
!     STEP 3: MAXIMUM LIKELIHOOD ESTIMATE IS THEN Z(IINDX)
!             WHERE IINDX IS THE ORDER STATISTIC RESULTING
!             IN THE MAXIMUM VALUE OF Mhat.
!
      CML=Y(IINDX)
!
!     NOW ESTIMATE A AND B USING THE QUANTILE METHOD DESCRIBED
!     ON PAGE 28-30 OF KOTZ AND VAN DORP.
!
      IQUAME='ORDE'
      IF(ALOWQN.GT.0.001 .AND. ALOWQN.LE.0.20)THEN
        P=ALOWQN
      ELSE
        P=0.05
      ENDIF
      CALL QUANT(P,Y,N,IWRITE,XTEMP,MAXNXT,   &
                 IQUAME,   &
                 AP,IBUGA3,IERROR)
!
      IF(AUPPQN.GT.0.80 .AND. AUPPQN.LE.0.999)THEN
        R=AUPPQN
      ELSE
        R=0.95
      ENDIF
      CALL QUANT(R,Y,N,IWRITE,XTEMP,MAXNXT,   &
                 IQUAME,   &
                 BR,IBUGA3,IERROR)
!
      DO 1301 I=1,N
        DTEMP1(I)=DBLE(Z(I))
 1301 CONTINUE
!
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
!
      IF(IFLAG.EQ.2)THEN
!
!  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2111)
!2111   FORMAT('***** WARNING FROM TRIANGULAR MAXIMUM ',
!CCCC1         'LIKELIHOOD--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,2113)
!2113   FORMAT('      ESTIMATE OF SIGMA MAY NOT BE COMPUTED TO ',
!CCCC1         'DESIRED TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
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
!
      DQ=DXLOW
      DTERM1=DAP - DM*DSQRT(DP/DQ)
      DTERM2=1.0D0 - DSQRT(DP/DQ)
      AML=REAL(DTERM1/DTERM2)
!
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
!
      DTERM1=DBR - DM*DSQRT((1.0D0-DR)/(1.0D0-DQ))
      DTERM2=1.0D0 - DSQRT((1.0D0-DR)/(1.0D0-DQ))
      BML=REAL(DTERM1/DTERM2)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'IML1')THEN
        WRITE(ICOUT,2247)DTERM1,DTERM2,XMAX,BML
 2247   FORMAT('DTERM1,DTERM2,XMAX,BML = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      AQUANT=AML
      BQUANT=BML
!
!     IMPLEMENT ESTIMATES FOR A AND B USING ML METHOD
!     DESCRIBED IN KOTZ AND VAN DORP.
!
!     STEP 1: IMPLEMENT BSEARCH ON PAGE 26
!
      DO 2301 I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2301 CONTINUE
!
      ITER=0
      MAXIT=100
!
      DA=DBLE(XMIN) - DBLE((XMAX - XMIN)*0.1)
      DLOWBK=DTEMP1(N)
      CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2310 CONTINUE
      ITER=ITER+1
      DBK=(DLOWBK + DUPPBK)/2.0D0
      DGK=TRIFU5(DA,DBK,DTEMP1,N)
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER.GT.MAXIT)GO TO 2319
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GO TO 2310
      ELSE
        GO TO 2319
      ENDIF
!
 2319 CONTINUE
      BML=REAL(DBK)
!
!     STEP 2: IMPLEMENT ABSEARCH ON PAGE 27
!
      ITER=0
      ITER2=0
!
!     STEP 2A: LOWER/UPPER BOUNDS FOR A
!
      DUPPAK=DTEMP1(1)
      DLOWAK=DTEMP1(1) - (DTEMP1(N) - DTEMP1(1))
!
!     STEP 2B: BSEARCH USING LOWER BOUND FOR A
!
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
        IF(ITER.GT.MAXIT)GO TO 2369
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GO TO 2361
      ELSE
        GO TO 2369
      ENDIF
!
 2369 CONTINUE
!
!     STEP 2C: COMPUTE G'(A,B)(A)
!
      DGK=TRIFU4(DA,DBK,DTEMP1,N)
      IF(DGK.LT.0.0)THEN
        DUPPAK=DLOWAK
        DLOWAK=DLOWAK - (DTEMP1(N) - DTEMP1(1))
        GO TO 2360
      ENDIF
!
!     STEP 2D: BSEARCH WITH NEW VALUE OF A
!
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
        IF(ITER.GT.MAXIT)GO TO 2389
        IF(DGK.LT.0.0D0)THEN
          DUPPBK=DBK
        ELSE
          DLOWBK=DBK
        ENDIF
        GO TO 2381
      ELSE
        GO TO 2389
      ENDIF
!
 2389 CONTINUE
      DGK=TRIFU4(DA,DBK,DTEMP1,N)
      ITER2=ITER2+1
      IF(ABS(DGK).GE.1.0D-6)THEN
        IF(ITER2.GT.MAXIT)GO TO 2399
        IF(DGK.LT.0.0D0)THEN
          DUPPAK=DA
        ELSE
          DLOWAK=DA
        ENDIF
        IF((DUPPAK - DLOWAK).LT.1.0D-6)GO TO 2399
        GO TO 2380
      ELSE
        GO TO 2399
      ENDIF
!
 2399 CONTINUE
      AML=REAL(DA)
      BML=REAL(DBK)
!
!     NOW UPDATE THE ESTIMATE OF C USING THE
!     FINAL PARAMETER ESTIMATES FOR A AND B.
!
      DMAXMR=0.0D0
      DO 2410 I=1,N
        IR=I
        DTERM1=TRIFU2(DA,DBK,IR,DTEMP1,N)
!
        IF(DTERM1.GT.DMAXMR)THEN
          DMAXMR=DTERM1
          IINDX=IR
        ENDIF
!
 2410 CONTINUE
!
      CML=Y(IINDX)
!
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
!
      RETURN
      END SUBROUTINE TRIML1
      SUBROUTINE TRIPDF(X,C,ALOWLM,AUPPLM,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TRIANGULAR DISTRIBUTION.
!              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
!              AND STANDARD DEVIATION = SQRT(1/6) = 0.408248
!              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
!              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
!              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME
!              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
!              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION
!              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
!              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
!              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
!              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
!              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
!              AND FOR C = 0
!              F(X) = 1+X                    FOR -1 LE X LE 0
!              F(X) = 1-X                    FOR  0 LT X LE 1
!              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
!              THE DISTRIBUTION THAT RESULTS
!              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
!              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
!              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
!              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
!              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
!              SO AS TO BE SYMMETRIC ABOUT 0.)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --C      = THE SINGLE PRECISION SHAPE PARAMETER
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
!     REFERENCES--EVANS, HASTINGS, PEACOCK, STATISTICAL DISTRIBUTIONS
!                 2ND ED.--CHAPTER 39.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
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
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--SEPTEMBER 1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
      PDF=0.0
!
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(C.LT.A .OR. C.GT.B)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    2 FORMAT(   &
      '***** ERROR--THE FIRST ARGUMENT TO TRIPDF IS OUTSIDE THE')
    3 FORMAT(   &
      '      (',G15.7,',',G15.7,') INTERVAL.')
   12 FORMAT(   &
      '***** ERROR--THE SECOND ARGUMENT TO TRIPDF IS OUTSIDE THE')
   22 FORMAT(   &
      '***** ERROR--THE THIRD AND FOURTH ARGUMENTS TO TRIPDF (THE')
   23 FORMAT(   &
      '      LOWER AND UPPER LIMITS) ARE EQUAL.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRIPDF
      SUBROUTINE TRIPPF(P,C,ALOWLM,AUPPLM,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TRIANGULAR
!              DISTRIBUTION ON THE INTERVAL (-1,1).
!              THIS DISTRIBUTION HAS MEAN = 0.0 ((A+B+C)/3)
!              AND STANDARD DEVIATION = SQRT(1/6) = 0.408248
!              THE TRIANGULAR DISTRIBUTION HAS LOWER LIMIT A AND
!              UPPER LIMIT B, WHICH DATAPLOT DEFINES TO BE -1 AND 1
!              RESPECTIVELY.  IT HAS SHAPE PARAMETER C.  SOME
!              DEFINE THE STANDARD DISTRIBUTION TO BE A = 0, B = 1,
!              C = 0.5, WHEREAS DATAPLOT USES A = -1, B = 1, C = 0.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION
!              F(X) = 2(X-A)/[(B-A)(C-A)]    FOR A <= X <= C
!              F(X) = 2(B-X)/[(B-A)(B-C)]    FOR C <= X <= B
!              FOR THE GIVEN VALUES OF A AND B, THIS REDUCES TO
!              F(X) = (X+1)/(C+1)            FOR -1 <= X <= C
!              F(X) = (1-X)/(1-C)            FOR  C <= X <= 1
!              AND FOR C = 0
!              F(X) = 1+X                    FOR -1 LE X LE 0
!              F(X) = 1-X                    FOR  0 LT X LE 1
!              (A TRIANGLE).
!              THIS DISTRIBUTION IS IMPORTANT IN THAT IT IS
!              THE DISTRIBUTION THAT RESULTS
!              FROM THE CONVOLUTION OF 2 UNIFORM DISTRIBUTIONS.
!              (BUT NOTE THAT THE TRIANGULAR DISTRIBUTION DEFINED HEREIN
!              IS NOT DEFINED OVER 0 TO 2 AS ONE WOULD EXPECT
!              FROM CONVOLVING 2 UNIFORMS EACH DEFINED OVER 0 TO 1,
!              BUT RATHER HAS BEEN DISPLACED TO -1 TO 1
!              SO AS TO BE SYMMETRIC ABOUT 0.)
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
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--APRIL     1978.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 1994.  ACCOMODATE C PARAMETER.
!     UPDATED         --JANUARY   1995.  FIX FOR C <> 0.
!     UPDATED         --JANUARY   1995.  TEST FOR C OUT OF RANGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
      PPF=0.0
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
    2   FORMAT(   &
        '***** ERROR--THE FIRST ARGUMENT TO TRIPPF IS OUTSIDE THE ',   &
        '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(C.LT.A .OR. C.GT.B)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TRIPPF IS ',   &
               'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)A,B
   13   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(A.EQ.B)THEN
        WRITE(ICOUT,22)
   22   FORMAT('***** ERROR--THE THIRD AND FOURTH ARGUMENTS TO TRIPPF ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
   23   FORMAT('      (THE LOWER AND UPPER LIMITS) ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRIPPF
      SUBROUTINE TRIRAN(N,C,ZLOWLM,ZUPPLM,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TRIANGULAR DISTRIBUTION
!              WITH MEAN = 0 AND STANDARD DEVIATION = ZZ.
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = ZZZ
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TRIANGULAR DISTRIBUTION
!             WITH MEAN = 0 AND STANDARD DEVIATION = ZZZ
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TRIPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGE 230.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES ZZZ.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--JUNE      1978.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 1994.  FIX BUG
!     UPDATED         --SEPTEMBER 2001.  SUPPORT FOR C SHAPE PARAMETER
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** ERROR--THE FIRST ARGUMENT TO TRIRAN IS ',   &
             'NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TRIANGULAR RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
!CCCC SEPTEMBER 1994.  FIX FOLLOWING LOOP.
!CCCC C=0.0
      A=MIN(ZLOWLM,ZUPPLM)
      B=MAX(ZLOWLM,ZUPPLM)
!
      IF(C.LE.A .OR. C .GE.B)THEN
        WRITE(ICOUT,210)A,B
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)C
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
  210 FORMAT('*****ERROR--THE SHAPE PARAMETER IS OUTSIDE THE ',   &
      'ALLOWABLE (',G15.7,',',G15.7,') INTERVAL.')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DO 100 I=1,N
!CCCC   CALL TRIPPF(X(I),X(I))
        P=X(I)
        CALL TRIPPF(P,C,A,B,PPF)
        X(I)=PPF
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TRIRAN
      SUBROUTINE TRUNEG(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
!              TRUE NEGATIVES BETEEN TWO VARIABLES.
!
!              THIS IS SPECIFICALLY FOR THE 2X2 CASE.  THAT IS,
!              EACH VARIABLE HAS TWO MUTUALLY EXCLUSIVE
!              CHOICES CODED AS 1 (FOR SUCCESS) OR 0 (FOR
!              FAILURE).  A TRUE NEGATIVE IS DEFINED AS THE
!              CASE WHERE THE SECOND VARIABLE IS 0 AND THE FIRST
!              VARIABLE IS A 0.
!
!              A TYPICAL EXAMPLE WOULD BE WHERE VARIABLE ONE
!              DENOTES THE GROUND TRUTH AND A VALUE OF 1
!              INDICATES "PRESENT" AND A VALUE OF 0 INDICATES
!              "NOT PRESENT".  VARIABLE TWO REPRESENTS SOME TYPE
!              OF DETECTION DEVICE WHERE A VALUE OF 1 INDICATES
!              THE DEVICE DETECTED THE SPECIFIED OBJECT WHILE A
!              VALUE OF 0 INDICATES THAT THE OBJECT WAS NOT
!              DETECTED.  A TRUE NEGATIVE THEN IS THE CASE WHERE
!              THE DEVICE FAILED TO DETECT THE OBJECT WHEN IT WAS
!              NOT ACTUALY THERE.
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
!                                COMPUTED TRUE NEGATIVE PROPORTION
!                                BETWEEN THE 2 SETS OF DATA
!                                IN THE INPUT VECTORS X AND Y.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             SAMPLE TRUE NEGATIVE PROPORTION BETWEEN THE 2 SETS
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
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
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
      ISUBN1='TRUN'
      ISUBN2='EG  '
      IERROR='NO'
!
!
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
 1201   FORMAT('***** ERROR IN THE TRUE NEGATIVE PROPORTION')
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
        STAT=REAL(N22)/REAL(NTEMP)
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
        STAT=0.0
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
        STAT=0.0
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
      STAT=REAL(N22)/REAL(N)
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
  811 FORMAT('THE TRUE NEGATIVE PROPORTION = ',G15.7)
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
!
      RETURN
      END SUBROUTINE TRUNEG
      SUBROUTINE TRUPOS(X,Y,N,IWRITE,XIDTEM,STAT,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROPORTION OF
!              TRUE POSITIVES BETWEEN TWO VARIABLES.
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
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
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
      IERROR='NO'
!
!
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
 1201   FORMAT('***** ERROR IN THE TRUE POSITIVE PROPORTION')
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
        STAT=REAL(N11)/REAL(NTEMP)
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
      STAT=REAL(N11)/REAL(N)
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
  811 FORMAT('THE TRUE POSITIVE PROPORTION = ',G15.7)
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
!
      RETURN
      END SUBROUTINE TRUPOS
      SUBROUTINE TSOCDF(DX,DN,DTHETA,ALOWLM,AUPPLM,DCDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS COMPUTED USING
!              THE FOLLOWING RELATION TO THE OGIVE DISTRIBUTION:
!
!              F(X;N,THETA) = THETA*G(X/THETA;N)                  0 < X < THETA
!                           = 1 - (1-THETA)*G((1-X)/(1-THETA);N)  THETA <= X < 1
!
!              WHERE F IS THE CUMULATIVE DISTRIBUTION OF THE TWO-SIDED
!              OGIVE DISTRIBUTION AND G IS THE CUMULATIVE DISTRIBUTION
!              OF THE OGIVE DISTRIBUTION.  THIS RELATION IS GIVEN ON
!              PAGE 230 OF KOTZ AND VAN DORP.
!
!              N DENOTES THE SHAPE PARAMETER AND THETA DENOTES THE
!              THRESHOLD PARAMETER.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --DN     = THE DOUBLE PRECISION FIRST SHAPE
!                                PARAMETER
!                     --DTHETA = THE DOUBLE PRECISION SECOND SHAPE
!                                PARAMETER
!                     --ALOWLM = THE DOUBLE PRECISION LOWER LIMIT
!                                PARAMETER
!                     --AUPPLM = THE DOUBLE PRECISION UPPER LIMIT
!                                PARAMETER
!     OUTPUT ARGUMENTS--DCDF   = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE TWO-SIDED OGIVE
!             DISTRIBUTION WITH SHAPE PARAMETERS N AND THETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--OGICDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION ALOWLM
      DOUBLE PRECISION AUPPLM
      DOUBLE PRECISION A
      DOUBLE PRECISION B
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      DCDF=0.0D0
!
      IF(DX.LE.A)THEN
        DCDF=0.0D0
        GO TO 9000
      ELSEIF(DX.GE.B)THEN
        DCDF=1.0D0
        GO TO 9000
      ELSEIF(DN.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)DN
        CALL DPWRST('XXX','BUG ')
        DCDF=0.0D0
        GO TO 9000
   12   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSOCDF IS ',   &
               'LESS THAN 0.5')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
   51   FORMAT('***** ERROR IN TSOCDF--LOWER LIMIT GREATER THAN OR ',   &
               'EQUAL TO UPPER LIMIT')
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
        GO TO 9000
      ENDIF
   61 FORMAT('***** ERROR IN TSOCDF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
   63 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
!               ************************************
!               **  STEP 1--                      **
!               **  COMPUTE THE CDF     FUNCTION  **
!               ************************************
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSOCDF
      DOUBLE PRECISION FUNCTION TSOFUN(DX)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION
!              WITH SHAPE PARAMETERS N AND THETA.
!
!              BUT DEFINE AS FUNCTION TO BE USED FOR INTEGRATION
!              CODE CALLED BY OGICDF.  ALSO, THIS ROUTINE USES
!              DOUBLE PRECISION ARITHMETIC.
!     INPUT  ARGUMENTS--DX     = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--TSOFUN  = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE TWO-SIDED OGIVE DISTRIBUTION
!             WITH SHAPE PARAMETERS N AND THETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--TSOPDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TSOCOM/DN,DTHETA,DA,DB
!
      DOUBLE PRECISION DPDF
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               ************************************
!               **  STEP 1--                      **
!               **  COMPUTE THE DENSITY FUNCTION  **
!               ************************************
!
      CALL TSOPDF(DX,DN,DTHETA,DA,DB,DPDF)
      TSOFUN=DPDF
!
      RETURN
      END FUNCTION TSOFUN
      DOUBLE PRECISION FUNCTION TSOFU2(DX)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION
!              WITH SHAPE PARAMETERS N AND THETA.
!
!              BUT DEFINE AS FUNCTION TO BE USED FOR INTEGRATION
!              CODE CALLED BY TSOPPF.  ALSO, THIS ROUTINE USES
!              DOUBLE PRECISION ARITHMETIC.
!     INPUT  ARGUMENTS--DX      = THE DOUBLE PRECISION VALUE AT
!                                 WHICH THE PROBABILITY DENSITY
!                                 FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--TSOFU2  = THE DOUBLE PRECISION CUMULATIVE
!                                 DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF FOR THE TWO-SIDED OGIVE DISTRIBUTION
!             WITH SHAPE PARAMETERS N AND THETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--TSOCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DX
!
      DOUBLE PRECISION DP
      COMMON/TS2COM/DP
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TS3COM/DN,DTHETA,DA,DB
!
      DOUBLE PRECISION DTHETS
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               ************************************
!               **  STEP 1--                      **
!               **  COMPUTE THE DENSITY FUNCTION  **
!               ************************************
!
      DTHETS=DTHETA
      CALL TSOCDF(DX,DN,DTHETA,DA,DB,DCDF)
      TSOFU2=DP - DCDF
      DTHETA=DTHETS
!
      RETURN
      END FUNCTION TSOFU2
      SUBROUTINE TSOPDF(X,N,THETA,ALOWLM,AUPPLM,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION.
!              THE PROBABILITY DENSITY FUNCTION FOR THE STANDARD
!              TWO-SIDED OGIVE DISTRIBUTION IS:
!
!                  f(X;N,THETA) =
!
!                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
!                      0 <= X <= THETA, N >= 0.5
!
!                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
!                      THETA < X <= 1, N >= 0.5
!
!              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING THE
!              THRESHOLD PARAMETER.
!
!              FOR THE GENERAL CASE, DO THE FOLLOWING:
!
!              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
!              2) LOCATION = A
!                 SCALE    = (B-A)
!              3) USE THE RELATION:
!
!                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --N      = THE DOUBLE PRECISION SHAPE PARAMETER
!                     --THETA  = THE DOUBLE PRECISION THRESHOLD PARAMETER
!                     --ALOWLM = THE DOUBLE PRECISION LOWER LIMIT PARAMETER
!                     --AUPPLM = THE DOUBLE PRECISION UPPER LIMIT PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN ALOWLM AND AUPPLM, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--OCTOBER     2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION N
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      PDF=0.0
!
      IF((N.GT.0.5D0 .AND. N.LT.1.0D0) .AND.   &
          X.LE.0.0D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
    2   FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO TSOPDF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
      ELSEIF(N.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
   12   FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSOPDF IS ',   &
               'LESS THAN 0.5')
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
  101   FORMAT('***** ERROR IN TSOPDF--LOWER LIMIT GREATER THAN OR ',   &
               'EQUAL TO UPPER LIMIT')
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
        GO TO 9000
      ENDIF
  111 FORMAT('***** ERROR IN TSOPDF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      THETSV=THETA
      THETA=(THETA-A)/(B-A)
      ALOC=A
      SCALE=(B-A)
      DX=(X-ALOC)/SCALE
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSOPDF
      SUBROUTINE TSOPPF(P,N,THETA,ALOWLM,AUPPLM,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TWO-SIDED OGIVE DISTRIBUTION WITH
!              WITH SHAPE PARAMETERS N AND THETA.
!
!                  f(X;N,THETA) =
!
!                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
!                      0 <= X <= THETA, N >= 0.5
!
!                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
!                      THETA < X <= 1, N >= 0.5
!
!              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING
!              THE THRESHOLD PARAMETER.
!
!              FOR THE GENERAL CASE, DO THE FOLLOWING:
!
!              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
!              2) LOCATION = A
!                 SCALE    = (B-A)
!              3) USE THE RELATION:
!
!                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
!
!              THE PERCENT POINT FUNCTION IS COMPUTED BY NUMERICALLY
!              INVERTING THE TWO-SIDED OGIVE CUMULATIVE
!              DISTRIBUTION FUNCTION (WHICH IN TURN IS COMPUTED BY
!              NUMERICAL INTEGRATION OF THE PROBABILITYT DENSITY.
!
!     INPUT  ARGUMENTS--P       = THE DOUBLE PRECISION VALUE AT
!                                 WHICH THE PERCENT POINT
!                                 FUNCTION IS TO BE EVALUATED.
!                                 0 <= P <= 1
!                     --N       = THE DOUBLE PRECISION SHAPE PARAMETER
!                     --THETA   = THE DOUBLE PRECISION THRESHOLD
!                                 PARAMETER
!                     --ALOWLM  = THE DOUBLE PRECISION LOWER LIMIT
!                                 PARAMETER
!                     --AUPPLM  = THE DOUBLE PRECISION UPPER LIMIT
!                                 PARAMETER
!     OUTPUT ARGUMENTS--PPF     = THE DOUBLE PRECISION PERCENT POINT
!                                 FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
!             VALUE PPF FOR THE TWO-SIDED OGIVE
!             DISTRIBUTION WITH SHAPE PARAMETERS N AND THETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DFZERO.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND N: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OVTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION N
!
      EXTERNAL TSOFU2
!
      DOUBLE PRECISION DP
      COMMON/TS2COM/DP
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      COMMON/TS3COM/DN,DTHETA,DA,DB
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               *****************************************
!               **  STEP 1--                           **
!               **  CHECK FOR VALID PARAMETERS         **
!               *****************************************
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
!CCCC THETSV=THETA
!CCCC THETA=(THETA-A)/(B-A)
!
      PPF=0.0D0
!
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,48)P
        CALL DPWRST('XXX','WRIT')
        GO TO 9000
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO TSOPPF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
!
      ELSEIF(N.LT.0.5D0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO OGIPDF IS ',   &
               'LESS THAN 0.5')
   48   FORMAT('      VALUE OF ARGUMENT IS: ',G15.7)
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
   51   FORMAT('***** ERROR IN TSOPPF--LOWER LIMIT GREATER THAN OR ',   &
               'EQUAL TO UPPER LIMIT')
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
        GO TO 9000
      ENDIF
   61 FORMAT('***** ERROR IN TSOPPF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
   63 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
!     N = 0.5 AND N = 1.0 REDUCE TO THE UNIFORM DISTRIBUTION.
!
      IF(N.EQ.0.5D0 .OR. N.EQ.1.0D0)THEN
        PPF=A + (B-A)*P
        GO TO 9000
      ELSEIF(P.LE.0.0D0)THEN
        PPF=A
        GO TO 9000
      ELSEIF(P.GE.1.0D0)THEN
        PPF=B
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 2--                           **
!               **  COMPUTE THE PERCENT POINT FUNCTION.**
!               *****************************************
!
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
!
      PPF=XLOW
!
      IF(IFLAG.EQ.2)THEN
!
!  NOTE: SUPPRESS THIS MESSAGE FOR NOW.
!CCCC   WRITE(ICOUT,999)
  999   FORMAT(1X)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,111)
!C111   FORMAT('***** WARNING FROM TSOPPF--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,113)
!C113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
!CCCC1         'TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
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
!
 9000 CONTINUE
!CCCC THETA=THETSV
      RETURN
      END SUBROUTINE TSOPPF
      SUBROUTINE TSORAN(N,AN,THETA,A,B,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TWO-SIDED OGIVE DISTRIBUTION WITH
!              SHAPE PARAMETERS N AND THETA.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!                  f(X;N,THETA) =
!
!                      N*(X/THETA)**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*(X/THETA)**N}
!                      0 <= X <= THETA, N >= 0.5
!
!                      N*((1-X)/(1-THETA))**(N-1)*{(4*N-2)/(3*N-1) -
!                      ((2*N-2)/(3*N-1))*((1-X)/(1-THETA))**N}
!                      THETA < X <= 1, N >= 0.5
!
!              WITH N DENOTING THE SHAPE PARAMETER AND THETA DENOTING THE
!              THRESHOLD PARAMETER.
!
!              FOR THE GENERAL CASE, DO THE FOLLOWING:
!
!              1) TRANSFORM THETA:  THETA'=(THETA-A)/(B-A)
!              2) LOCATION = A
!                 SCALE    = (B-A)
!              3) USE THE RELATION:
!
!                     f(X;N,THETA,A,B) = f((X-LOC)/SCALE);N,THETA,0,1)/SCALE
!
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --AN     = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER AN.
!                     --THETA  = THE SINGLE PRECISION THRESHOLD
!                                PARAMETER
!                     --ALOWLM = THE SINGLE PRECISION LOWER LIMIT
!                                PARAMETER
!                     --AUPPLM = THE SINGLE PRECISION UPPER LIMIT
!                                PARAMETER
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TWO-SIDED OGIVE DISTRIBUTION
!             WITH SHAPE PARAMETER N AND THETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TSOPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND AN: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHMOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.10
!     ORIGINAL VERSION--OCTOBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DOUBLE PRECISION DTEMP
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
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',   &
               'TWO-SIDED OGIVE RANDOM NUMBERS IS NON-POSITIVE')
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
      ELSEIF(AN.LT.0.5)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)AN
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
  201   FORMAT('***** ERROR--THE N SHAPE PARAMETER IS ',   &
               'LESS THAN 0.5')
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
        GO TO 9000
  100   FORMAT('***** ERROR IN TWO-SIDED OGIVE RANDOM NUMBERS--')
  101   FORMAT('      LOWER LIMIT GREATER THAN OR EQUAL TO ',   &
               'UPPER LIMIT')
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
        GO TO 9000
  111   FORMAT('      THETA IS OUTSIDE THE LOWER AND UPPER LIMITS')
  113   FORMAT('      THE VALUE OF THETA IS ',G15.7)
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TWO-SIDED OGIVE DISTRIBUTION RANDOM
!     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 300 I=1,N
        CALL TSOPPF(DBLE(X(I)),DBLE(AN),DBLE(THETA),   &
                    DBLE(A),DBLE(B),DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSORAN
      SUBROUTINE TSPCDF(X,THETA,AN,A,B,CDF)
!
!     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
!           CDF IS:
!              TSPCDF(X,THETA,N)
!                  = THETA*(X/THETA)**N          0 <= X <= THETA
!                  = 1 - (1-THETA)*((1-X)/(1-THETA))**N
!                                                THETA <= X <= 1
!                    N > 0, 0 <= THETA <= 1
!     NOTE--GENERAL TWO-SIDED POWER DISTRIBUTION CDF IS:
!              F(X;THETA,N,A,B)
!                  = ((A-THETA)/(B-A))*((X-A)/(THETA-A))**N
!                                             A <= X <= THETA
!                  = 1 - ((THETA-B)/(B-A))*((B-X)/(B-THETA))**N
!                                             THETA <= X <= B
!     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
!                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
!                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
!                      INSTEAD OF FROM THE CALLING ROUTINE.
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002/5
!     ORIGINAL VERSION--MAY       2002.
!     UPDATED         --JUNE      2007. ADD A AND B TO CALL LIST
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM3
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CDF=0.0
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPCDF--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPCDF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPCDF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
!
      IF(X.LT.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  131 FORMAT('***** WARNING IN TSPCDF--THE FIRST INPUT ARGUMENT IS ',   &
             'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
!
      IF(X.GT.B)THEN
        CDF=1.0
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  141 FORMAT('***** WARNING IN TSPCDF--THE FIRST INPUT ARGUMENT IS ',   &
             'GREATER THAN THE UPPER LIMIT')
!
      IF(X.LE.A)THEN
        CDF=0.0
      ELSEIF(X.GE.B)THEN
        CDF=1.0
      ELSE
!
        DX=DBLE(X)
        DTHETA=DBLE(THETA)
        DAN=DBLE(AN)
        DA=DBLE(A)
        DB=DBLE(B)
!
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
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TSPCDF
      DOUBLE PRECISION FUNCTION TSPFU3(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE G(A,B)
!              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G(A,B) = N*{LOG{NUHAT(A,B)/(B-A) + 1/(NUHAT(A,B) - 1}
!
!              WHERE
!
!              NUHAT(A,B) = MAX{-N/LOG(MHAT(A,B,RHAT(A,B))),1)
!
!              IT IS ASSUMED THAT THE DATA IS ALREADY SORTED.
!     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 100-104.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION NUHAT
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      NUHAT=-DBLE(N)/DLOG(DMAXMR)
      NUHAT=MAX(NUHAT,1.0D0)
      TSPFU3=DBLE(N)*(DLOG(NUHAT/(B-A)) + (1.0D0/NUHAT) - 1.0D0)
!
      RETURN
      END FUNCTION TSPFU3
      DOUBLE PRECISION FUNCTION TSPFU4(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO A)
!              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G'(A,B)(A) = -{M'(A,B,RHAT(A,B))(A)/M(A,B,RHAT(A,B)}*
!                           {N/LOG(M(A,B,RHAT(A,B))) + 1}
!                           + N/(B-A)
!
!              WHERE
!
!              M'(A,B,RHAT(A,B))(A) = M(A,B,RHAT(A,B))*
!                   {SUM[j=1 to Rhat-1]
!                   [(Z(j) - Z(RHAT))/(Z(RHAT)-A)*(Z(J) - A)]}
!
!     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/7
!     ORIGINAL VERSION--JULY       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DSUM1=0.0D0
!
      IRM1=IR-1
      IF(IRM1.GE.1)THEN
        DO 1210 J=1,IRM1
          DTERM1=DBLE(X(J) - X(IR))
          DTERM2=DBLE(X(IR) - A)*DBLE(X(J) - A)
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
      DMHATP=DMAXMR*DSUM1
!
      TSPFU4=(-DMHATP/DMAXMR)*((DBLE(N)/DLOG(DMAXMR)) + 1.0D0)   &
             + DBLE(N)/DBLE(B-A)
!
      RETURN
      END FUNCTION TSPFU4
      DOUBLE PRECISION FUNCTION TSPFU5(A,B,X,N)
!
!     PURPOSE--THIS ROUTINE IS USED TO COMPUTE THE PARTIAL
!              DERIVATIVE OF THE G(A,B) (WITH RESPECT TO B)
!              FUNCTION IN THE TWO-SIDED POWER MAXIMUM LIKELIHOOD.
!              SPECIFICALLY, IT COMPUTES
!
!              G'(A,B)(B) = -{M'(A,B,RHAT(A,B))(B)/M(A,B,RHAT(A,B)}*
!                           {N/LOG(M(A,B,RHAT(A,B))) + 1}
!                           - N/(B-A)
!
!              WHERE
!
!              M'(A,B,RHAT(A,B))(B) = M(A,B,RHAT(A,B))*
!                   {SUM[j=RHAT+1 to N]
!                   [(Z(j) - Z(RHAT))/(B - Z(RHAT))*(B - Z(J))]}
!
!     EXAMPLE--TRIANGULAR MAXIMUM LIKELIHOOD Y
!     REFERENCE--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                PP. 16-30.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/7
!     ORIGINAL VERSION--JULY       2007.
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION X(*)
      INTEGER N
!
!---------------------------------------------------------------------
!
      INTEGER IR
      INTEGER IINDX
!
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DMAXMR
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMHATP
!
      DOUBLE PRECISION TRIFU2
      EXTERNAL TRIFU2
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IINDX=0
      DMAXMR=0.0D0
      DO 100 I=1,N
        IR=I
        DTEMP1=TRIFU2(A,B,IR,X,N)
!
        IF(DTEMP1.GT.DMAXMR)THEN
          DMAXMR=DTEMP1
          IINDX=IR
        ENDIF
!
  100 CONTINUE
!
      IR=IINDX
!
      DSUM1=0.0D0
!
      IRM1=IR+1
      IF(IRM1.LE.N)THEN
        DO 1210 J=IRM1,N
          DTERM1=X(J) - X(IR)
          DTERM2=(B - X(IR))*(B - X(J))
          DSUM1=DSUM1 + (DTERM1/DTERM2)
 1210   CONTINUE
      ENDIF
      DMHATP=DMAXMR*DSUM1
!
      TSPFU5=(-DMHATP/DMAXMR)*((DBLE(N)/DLOG(DMAXMR)) + 1.0D0)   &
             - DBLE(N)/DBLE(B-A)
!
      RETURN
      END FUNCTION TSPFU5
      SUBROUTINE TSPML1(Y,N,XTEMP,TEMP1,TEMP2,TEMP3,DTEMP1,   &
                        XMIN,XMAX,XMEAN,XSD,   &
                        AML,BML,THETML,ANML,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD
!              ESTIMATES FOR THE SHAPE PARAMETERS OF THE
!              TWO-SIDED POWER DISTRIBUTION.  IT CAN HANDLE EITHER
!              THE CASE WHERE THE LOWER/UPPER LIMITS ARE ESTIMATED
!              OR THE CASE WHERE THEY ARE NOT.
!     EXAMPLE--TWO-SIDED POWER MAXIMUM LIKELIHOOD Y
!     REFERENCES--"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
!                 ITS PROPERTIES WITH APPLICATIONS IN FINANCIAL
!                 ENGINEERING", J. RENE VAN DORP AND SAMUEL KOTZ,
!                 AMERICAN STATISTICIAN, VOLUME 56,
!                 NUMBER 2, MAY, 2002.
!               --KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTIFIC,
!                 CHAPTER 1.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/07
!     ORIGINAL VERSION--JULY      2010. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLTS)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DOUBLE PRECISION DTEMP1(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION TRIFU2
      DOUBLE PRECISION TSPFU3
      DOUBLE PRECISION TSPFU4
      DOUBLE PRECISION TSPFU5
      EXTERNAL TRIFU2
      EXTERNAL TSPFU3
      EXTERNAL TSPFU4
      EXTERNAL TSPFU5
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DMAXMR
!
      DOUBLE PRECISION DA
      DOUBLE PRECISION DLOWBK
      DOUBLE PRECISION DUPPBK
      DOUBLE PRECISION DLOWAK
      DOUBLE PRECISION DUPPAK
      DOUBLE PRECISION DBK
      DOUBLE PRECISION DMK
      DOUBLE PRECISION DGK
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TSPM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
      AML=CPUMIN
      BML=CPUMIN
      THETML=CPUMIN
      ANML=CPUMIN
      IINDX=0
!
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
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               **********************************************
!               **  STEP 1--                                **
!               **  CARRY OUT CALCULATIONS                  **
!               **  FOR TWO-SIDED POWER/ MLE ESTIMATE       **
!               **********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='TWO-SIDED POWER'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CALL SORT(Y,N,Y)
      DO 2110 I=1,N
        DTEMP1(I)=DBLE(Y(I))
 2110 CONTINUE
!
!CCCC IMPLEMENT UPDATE MLE PROCEDURE FROM KOTZ AND VAN DORP.
!CCCC
!CCCC   1) ESTIMATES FOR LOWER AND UPPER BOUNDS ADDED
!CCCC   2) SEPARATE CASES FOR AN <= 1 AND AN > 1.
!
!          NOTE: CASE WHERE AN <= 1 HAS VALID ML ESTIMATES
!                FOR A AND B, BUT THE ML ESTIMATES FOR THETA
!                AND N IS UNSTABLE.  SO FOR NOW, JUST APPLY
!                THE N > 1 ESTIMATES.
!
!     IMPLEMENT ESTIMATES FOR A AND B USING ML METHOD
!     DESCRIBED IN KOTZ AND VAN DORP.
!
!     STEP 1: IMPLEMENT BSEARCH ON PAGE 26
!
!CCCC IF(ANSV.LE.1.0)THEN
!CCCC   AML=XMIN
!CCCC   BML=XMAX
!CCCC   DA=DBLE(AML)
!CCCC   DBK=DBLE(BML)
!CCCC ELSE
        ITER=0
        MAXIT=100
!
        DA=DBLE(XMIN) - DBLE((XMAX - XMIN)*0.1)
        DLOWBK=DTEMP1(N)
        CALL TRIFU8(DA,DUPPBK,DTEMP1,IR,N)
 2310   CONTINUE
        ITER=ITER+1
        DBK=(DLOWBK + DUPPBK)/2.0D0
        DGK=TSPFU5(DA,DBK,DTEMP1,N)
        IF(ABS(DGK).GE.1.0D-6)THEN
!CCCC     IF(ITER.GT.MAXIT)GO TO 2319
          IF(ITER.GT.MAXIT)THEN
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GO TO 2310
        ELSE
          GO TO 2319
        ENDIF
!
 2319   CONTINUE
        BML=REAL(DBK)
!
!       STEP 2: IMPLEMENT ABSEARCH ON PAGE 27
!
        ITER=0
        ITER2=0
!
!       STEP 2A: LOWER/UPPER BOUNDS FOR A
!
        DUPPAK=DTEMP1(1)
        DLOWAK=DTEMP1(1) - (DTEMP1(N) - DTEMP1(1))
!
!       STEP 2B: BSEARCH USING LOWER BOUND FOR A
!
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
          IF(ITER.GT.MAXIT)GO TO 2369
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GO TO 2361
        ELSE
          GO TO 2369
        ENDIF
!
 2369   CONTINUE
!
!       STEP 2C: COMPUTE G'(A,B)(A)
!
        DGK=TSPFU4(DA,DBK,DTEMP1,N)
        IF(DGK.LT.0.0)THEN
          DUPPAK=DLOWAK
          DLOWAK=DLOWAK - (DTEMP1(N) - DTEMP1(1))
          GO TO 2360
        ENDIF
!
!       STEP 2D: BSEARCH WITH NEW VALUE OF A
!
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
          IF(ITER.GT.MAXIT)GO TO 2389
          IF(DGK.LT.0.0D0)THEN
            DUPPBK=DBK
          ELSE
            DLOWBK=DBK
          ENDIF
          GO TO 2381
        ELSE
          GO TO 2389
        ENDIF
!
 2389   CONTINUE
        DGK=TSPFU4(DA,DBK,DTEMP1,N)
        ITER2=ITER2+1
        IF(ABS(DGK).GE.1.0D-6)THEN
          IF(ITER2.GT.MAXIT)GO TO 2399
          IF(DGK.LT.0.0D0)THEN
            DUPPAK=DA
          ELSE
            DLOWAK=DA
          ENDIF
          IF((DUPPAK - DLOWAK).LT.1.0D-6)GO TO 2399
          GO TO 2380
        ELSE
          GO TO 2399
        ENDIF
!
 2399   CONTINUE
        AML=REAL(DA)
        BML=REAL(DBK)
!
!CCCC ENDIF
!
!     NOW UPDATE THE ESTIMATE OF C USING THE
!     FINAL PARAMETER ESTIMATES FOR A AND B.
!
      DMAXMR=0.0D0
      DO 2410 I=1,N
        IR=I
        DTERM1=TRIFU2(DA,DBK,IR,DTEMP1,N)
!
        IF(DTERM1.GT.DMAXMR)THEN
          DMAXMR=DTERM1
          IINDX=IR
        ENDIF
!
 2410 CONTINUE
!
      IF(DMAXMR.LE.0.0D0)THEN
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      THETML=Y(IINDX)
      ANML=REAL(-DBLE(N)/DLOG(DMAXMR))
!
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
        DO 9066 I=1,MIN(N,100)
          WRITE(ICOUT,9067)I,XTEMP(I),TEMP1(I),TEMP2(I),TEMP3(I)
 9067     FORMAT('I,XTEMP(I),TEMP1(I),TEMP2(I),TEMP3(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','WRIT')
 9066   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE TSPML1
      SUBROUTINE TSPPDF(X,THETA,AN,A,B,PDF)
!
!     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
!                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
!                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
!                      INSTEAD OF FROM THE CALLING ROUTINE.
!     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
!           PDF IS:
!              TSPPDF(X,THETA,N)
!                  = N*(X/THETA)**(N-1)          0 < X <= THETA
!                  = N*((1-X)/(1-THETA))**(N-1)  THETA <= X < 1
!           THE GENERAL PDF IS:
!               f(X;THETA,N,A,B)
!                  = (N/(B-A))*((X-A)/(THETA-A))**(N-1)
!                                             A < X <= THETA
!                  = (N/(B-A))*((B-X)/(B-THETA))**(N-1)
!                                             THETA < X <= B
!     REFERENCE --"THE STANDARD TWO-SIDED POWER DISTRIBUTION AND
!                 ITS PRoPERTIES WITH APPLICATIONS IN FINANCIAL
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002/5
!     ORIGINAL VERSION--MAY       2002.
!     UPDATED         --JUNE      2007. INCLUDE A AND B EXPLICITLY
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      PDF=0.0
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPPDF--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPPDF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPPDF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
!
      IF(X.LE.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  131 FORMAT('***** ERROR IN TSPPDF--THE FIRST INPUT ARGUMENT IS ',   &
             'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
!
      IF(X.GE.B)THEN
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  141 FORMAT('***** ERROR IN TSPPDF--THE FIRST INPUT ARGUMENT IS ',   &
             'GREATER THAN THE UPPER LIMIT')
!
      DX=DBLE(X)
      DTHETA=DBLE(THETA)
      DAN=DBLE(AN)
      DA=DBLE(A)
      DB=DBLE(B)
!
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
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TSPPDF
      SUBROUTINE TSPPPF(P,THETA,AN,A,B,PPF)
!
!     NOTE--JUNE 2006: SINCE THETA IS CONSTRAINED BY LOWER AND
!                      UPPER LIMIT, UPDATE TO INCLUDE LOWER AND
!                      UPPER LIMIT PARAMETERS (A, B) EXPLICITLY
!                      INSTEAD OF FROM THE CALLING ROUTINE.
!     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
!           PPF IS:
!              TSPPPF(P,THETA,N)
!                  = THETA*(P/THETA)**(1/N)      0 < P <= THETA
!                  = 1 - (1-THETA)*((1-P)/(1-THETA))**(1/N)
!                                                P > THETA < 1
!     NOTE--GENERAL PPF IS:
!           G(P;THETA,N,A,B)
!               = A + {P*(THETA-A)**(N-1)*(B-A)}**(N)
!                     0 <= P <= (THETA-A)/(B-A)
!               = B - {(1-P)*(B-THETA)**(N-1)*(B-A)}**(N)
!                      (THETA-A)/(B-A) < P <= 1
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002/5
!     ORIGINAL VERSION--MAY       2002.
!     UPDATED         --JUNE      2007. EXPLICIT SUPPORT FOR A AND B
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DAN
      DOUBLE PRECISION DP
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      PPF=0.0
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,151)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  151 FORMAT('***** ERROR IN TSPPPF--THE FIRTST INPUT ARGUMENT IS ',   &
             'OUTSIDE THE (0,1) INTERVAL')
  153 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPPPF--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPPPF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPPPF--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
!
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
!
        IF(DP.LE.DTERM1)THEN
          DTERM3=DLOG(DP) + (DAN-1.0D0)*DLOG(DTHETA-DA) + DLOG(DB-DA)
          DTERM4=(1.0D0/DAN)*DTERM3
          DPPF=DA + DEXP(DTERM4)
        ELSE
          DTERM3=DLOG(1.0D0-DP) + (DAN-1.0D0)*DLOG(DB-DTHETA) +   &
                 DLOG(DB-DA)
          DTERM4=(1.0D0/DAN)*DTERM3
          DPPF=DB - DEXP(DTERM4)
        ENDIF
        PPF=REAL(DPPF)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TSPPPF
      SUBROUTINE TSPRAN(N,THETA,AN,A,B,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TWO-SIDED POWER DISTRIBUTION
!              WITH SHAPE PARAMETERS = THETA AND N.
!     NOTE--STANDARD TWO-SIDED POWER DISTRIBUTION (STSP).
!           PDF IS:
!              TSPPDF(X,THETA,N)
!                  = N*(X/THETA)**(N-1)          0 < X <= THETA
!                  = N*((1-X)/(1-THETA))**(N-1)  THETA <= X < 1
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --THETA  = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER THETA.
!                                THETA SHOULD BE IN THE RANGE (0,1).
!                     --AN     = THE SINGLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER N.
!                                AN SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TWO-SIDED POWER DISTRIBUTION
!             WITH SHAPE PARAMETERS = THETA AND N.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
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
!     VERSION NUMBER--2002.5
!     ORIGINAL VERSION--MAY       2002.
!     UPDATED         --JUNE      2007. ADD A AND B TO CALL LIST
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
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR IN TSPRAN--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  111 FORMAT('***** ERROR IN TSPRAN--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
      IF(AN.LE.0.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)AN
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  121 FORMAT('***** ERROR IN TSPRAN--N IS NON-POSITIVE')
  123 FORMAT('      THE VALUE OF N IS ',G15.7)
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  131 FORMAT('***** ERROR IN TWO-SIDED POWER RANDOM NUMBERS--')
  133 FORMAT('      THE REQUESTED NUMBER OF RANDOM NUMBERS IS ',   &
             'NON-POSITIVE')
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TWO-SIDED POWER DISTRIBUTION RANDOM
!     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
        CALL TSPPPF(X(I),THETA,AN,A,B,XTEMP)
        X(I)=XTEMP
  100 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE TSPRAN
      SUBROUTINE TSSCDF(X,ALPHA,THETA,ALOWLM,AUPPLM,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
!              THE CUMULATIVE DISTRIBUTION FUNCITON IS:
!
!              F(X;ALPHA,THETA,A,B) =
!                  ALPHA*((X-A)/(B-A)) +
!                  (1-ALPHA)*((B-A)/(THETA-A))*((X-A)/(B-A))**2
!                  A <= X <= THETA
!
!                  1 - ALPHA*((B-X)/(B-A)) -
!                  (1-ALPHA)*((B-A)/(B-THETA))*((B-X)/(B-A))**2
!                  THETA < X <= B
!
!                  0 <= ALPHA <= 2
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!                     --THETA  = THE SINGLE PRECISION SHAPE PARAMETER
!                     --A      = THE SINGLE PRECISION LOWER LIMIT
!                                PARAMETER
!                     --B      = THE SINGLE PRECISION UPPER LIMIT
!                                PARAMETER
!     OUTPUT ARGUMENTS--CDF   = THE SINGLE PRECISION CUMULATIVE
!                               DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN A AND B, INCLUSIVELY.
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
!     ORIGINAL VERSION--SEPTEMBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      CDF=0.0
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN TSSCDF--LOWER LIMIT GREATER THAN OR ',   &
               'EQUAL TO UPPER LIMIT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
  102   FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
  103   FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
  111  FORMAT('***** ERROR IN TSSCDF--THETA IS OUTSIDE THE ',   &
              'LOWER AND UPPER LIMITS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
  113   FORMAT('      THE VALUE OF THETA IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,121)
  121   FORMAT('***** ERROR--THE SECOND ARGUMENT TO TSSCDF IS ',   &
               'OUTSIDE THE (0,2) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!CCCC IF(X.LT.A)THEN
!CCCC   WRITE(ICOUT,131)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,133)X
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,102)A
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
!CCCC ENDIF
!C131 FORMAT('***** ERROR IN TSSCDF--THE FIRST INPUT ARGUMENT IS ',
!CCCC1       'LESS THAN THE LOWER LIMIT')
!C133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
!
!CCCC IF(X.GT.B)THEN
!CCCC   WRITE(ICOUT,141)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,133)X
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   CDF=1.0
!CCCC   GO TO 9000
!CCCC ENDIF
!C141 FORMAT('***** ERROR IN TSSCDF--THE FIRST INPUT ARGUMENT IS ',
!CCCC1       'GREATER THAN THE UPPER LIMIT')
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSSCDF
      SUBROUTINE TSSPDF(X,ALPHA,THETA,ALOWLM,AUPPLM,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
!              THE STANDARD PROBABILITY DENSITY FUNCTION IS:
!
!              f(X;ALPHA,THETA,A,B) =
!                  ALPHA/(B-A) + 2*(1-ALPHA)*(X-A)/
!                  {(B-A)*(THETA-A)}
!                  A <= X <= THETA, 0 <= ALPHA <= 2
!
!              f(X;ALPHA,THETA,A,B) =
!                  ALPHA/(B-A) + 2*(1-ALPHA)*(B-X)/
!                  {(B-A)*(B-THETA)}
!                  THETA < X <= B, 0 <= ALPHA <= 2
!
!              WITH ALPHA AND THETA DENOTING THE SHAPE PARAMETER
!              AND THE THRESHOLD PARAMETER, RESPECTIVELY.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --A       = THE SINGLE PRECISION LOWER LIMIT
!                                 PARAMETER
!                     --B       = THE SINGLE PRECISION UPPER LIMIT
!                                 PARAMETER
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
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--SEPTEMBER   2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      PDF=0.0
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  101 FORMAT('***** ERROR IN TSSPDF--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  102 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  103 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
      IF(THETA.LT.A .OR. THETA.GT.B)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  111 FORMAT('***** ERROR IN TSSPDF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
      IF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
  121 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSSPDF IS ',   &
             'OUTSIDE THE (0,2) INTERVAL.')
!
      IF(X.LT.A)THEN
        WRITE(ICOUT,131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  131 FORMAT('***** ERROR IN TSSPDF--THE FIRST INPUT ARGUMENT IS ',   &
             'LESS THAN THE LOWER LIMIT')
  133 FORMAT('      THE VALUE OF THE FIRST INPUT ARGUMENT IS ',G15.7)
!
      IF(X.GT.B)THEN
        WRITE(ICOUT,141)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  141 FORMAT('***** ERROR IN TSSPDF--THE FIRST INPUT ARGUMENT IS ',   &
             'GREATER THAN THE UPPER LIMIT')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      TERM1=ALPHA/(B-A)
      TERM2=2.0*(1.0-ALPHA)/(B-A)
!
      IF(X.LE.A .OR. X.GE.B)THEN
        PDF=TERM1
      ELSEIF(X.LE.THETA)THEN
        TERM3=(X-A)/(THETA-A)
        PDF=TERM1 + TERM2*TERM3
      ELSE
        TERM3=(B-X)/(B-THETA)
        PDF=TERM1 + TERM2*TERM3
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSSPDF
      SUBROUTINE TSSPPF(P,ALPHA,THETA,ALOWLM,AUPPLM,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE TWO-SIDED SLOPE DISTRIBUTION.
!              INVERTING THE STANDARD FORM OF THE CDF FUNCTION
!              YIELDS THE FOLLOWING QUADRATIC EQUATION:
!
!              {-ALPHA +/- SQRT(ALPHA**2 + 4*P*(1-ALPHA)/THETA)}/
!              {2*(1-ALPHA)/THETA}
!              0 <= P <= THETA
!
!              {2*C1+ALPHA) +/-
!              SQRT((-2*C1-ALPHA)**2 - 4*C1*(C1+ALPHA+P-1))}/
!              {2*C1}
!              THETA < P <= 1
!
!              WHERE
!
!              C1=(1-ALPHA)/(1-THETA)
!
!              THE PERCENT POINT FUNCTION IS COMPUTED BY
!              DETERMINING WHICH OF THE TWO ROOTS LIES IN THE
!              (0,1) INTERVAL.
!
!              FOR THE GENERAL FORM, TRANSFORM THETA TO
!              THE (0,1) SCALE AND USE THE RELATION
!
!                  G(P;ALPHA,THETA,A,B) = A +
!                                         (B-A)*G(P;ALPHA,THETA,0,1)
!
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --A       = THE SINGLE PRECISION LOWER LIMIT
!                                 PARAMETER
!                     --B       = THE SINGLE PRECISION UPPER LIMIT
!                                 PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
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
!     ORIGINAL VERSION--OCTOBER  2007
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
      THETSV=THETA
      PPF=0.0
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO TSSPPF IS ',   &
             'OUTSIDE THE (0,1) INTERVAL.')
      ELSEIF(ALPHA.LT.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO TSSPPF IS ',   &
             'OUTSIDE THE (0,2) INTERVAL.')
      ELSEIF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
  101 FORMAT('***** ERROR IN TSSPPF--LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
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
        GO TO 9000
      ENDIF
  111 FORMAT('***** ERROR IN TSSPPF--THETA IS OUTSIDE THE ',   &
             'LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
      THETA=(THETA-A)/(B-A)
!
!     IF ALPHA = 1, THEN WE HAVE A UNIFORM DISTRIBUTION.
!     HANDLE THIS SEPARATELY.
!
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
!
 9000 CONTINUE
      THETA=THETSV
      RETURN
      END SUBROUTINE TSSPPF
      SUBROUTINE TSSRAN(N,ALPHA,THETA,ALOWLM,AUPPLM,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE TWO-SIDED SLOPE DISTRIBUTION WITH
!              SHAPE PARAMETERS ALPHA AND THETA.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!              f(X;ALPHA,THETA,A,B) =
!                  ALPHA/(B-A) + 2*(1-ALPHA)*(X-A)/
!                  {(B-A)*(THETA-A)}
!                  A <= X <= THETA, 0 <= ALPHA <= 2
!
!              f(X;ALPHA,THETA,A,B) =
!                  ALPHA/(B-A) + 2*(1-ALPHA)*(B-X)/
!                  {(B-A)*(B-THETA)}
!                  THETA < X <= B, 0 <= ALPHA <= 2
!
!              WITH ALPHA AND THETA DENOTING THE SHAPE PARAMETER
!              AND THE THRESHOLD PARAMETER, RESPECTIVELY.
!
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --ALPHA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --THETA   = THE SINGLE PRECISION SHAPE PARAMETER
!                     --A       = THE SINGLE PRECISION LOWER LIMIT
!                                 PARAMETER
!                     --B       = THE SINGLE PRECISION UPPER LIMIT
!                                 PARAMETER
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE TWO-SIDED SLOPE DISTRIBUTION
!             WITH SHAPE PARAMETER ALPHA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, TSSPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND ALPHA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 8.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHMOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
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
      A=MIN(ALOWLM,AUPPLM)
      B=MAX(ALOWLM,AUPPLM)
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',   &
      'TWO-SIDED SLOPE RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
      IF(ALPHA.LE.0.0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
  201 FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER IS ',   &
             'OUTSIDE THE (0,2) INTERVAL.')
  203 FORMAT('      THE VALUE OF ALPHA IS ',G15.7)
!
      IF(A.GE.B)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  101 FORMAT('***** ERROR IN TWO-SIDED SLOPE RANDOM NUMBERS--')
  102 FORMAT('      LOWER LIMIT GREATER THAN OR ',   &
             'EQUAL TO UPPER LIMIT')
  103 FORMAT('      THE VALUE OF THE LOWER LIMIT IS ',G15.7)
  104 FORMAT('      THE VALUE OF THE UPPER LIMIT IS ',G15.7)
!
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
        GO TO 9000
      ENDIF
  111 FORMAT('      THETA IS OUTSIDE THE LOWER AND UPPER LIMITS')
  113 FORMAT('      THE VALUE OF THETA IS ',G15.7)
!
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N TWO-SIDED SLOPE DISTRIBUTION RANDOM
!     NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 300 I=1,N
        CALL TSSPPF(X(I),ALPHA,THETA,A,B,XTEMP)
        X(I)=XTEMP
  300 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE TSSRAN
      SUBROUTINE TTT(X,TAG,NX,ICASE,IWRITE,Y,NOUT,TEMP1,TEMP2,MAXNXT,   &
                     IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE TOTAL TIME ON TEST OF AN ARRAY.  THE TAG VARIABLE
!              IDENTIFIES CENSORED DATA (1 = FAILURE TIME, 0 = CENSORED).
!
!              FOR UNCENSORED DATA, THE FORMULA IS (X IS SORTED)
!
!                  TTT(i) = SUM[j=1 to i][(N-j+1)*(X(j) - X(j-1))]
!                  X(0) = 0
!
!              THE SCALED TIME TO FAILURE IS
!
!                  TTT*(i) = TTT(i)/TTT(n)
!
!              FOR CENSORED DATA, USE
!
!                  TTT*(i) = TTT(i)/TTT(r)
!
!              WHERE r IS THE MAXIMUM UNCENSORED FAILURE TIME
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     REFERENCE--HORST RINNE (2010), "Location–Scale Distributions:
!                Linear Estimation and Probability Plotting Using
!                MATLAB".
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2020/06
!     ORIGINAL VERSION--JUNE      2020.
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
      CHARACTER*4 ICASE2
!
      DOUBLE PRECISION DSUM
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='TTT '
      ISUBN2='    '
      IERROR='NO'
!
      NOUT=0
      NR=0
      NCENS=0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTT ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF TTT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,NX,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASE,NX,MAXNXT = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I),TAG(I)
   56     FORMAT('I,X(I), TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **************************************
!               **  CHECK FOR NEGATIVE DATA         **
!               **************************************
      DO 90 I=1,NX
        IF(X(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,93)
   93     FORMAT('      THE TOTAL TIME ON TEST STATISTIC IS NOT ',   &
                 'SUPPORTED FOR NEGATIVE DATA.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,95)I,X(I)
   95     FORMAT('      ROW ',I8,' HAS THE VALUE ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
   90 CONTINUE
!
!               **************************************
!               **  COMPUTE TOTAL TIME TO FAILURE   **
!               **************************************
!
      CALL SORTC(X,TAG,NX,X,TAG)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!     CHECK CENSORING VARIABLE.  TYPICALLY, THE CENSORING VARIABLE WILL
!     BE CODED AS 0 => CENSORED VALUE AND 1 => UNCENSORED VALUE.  WITH
!     CODING, THIS WILL BE 1 => CENSORED AND 2 => UNCENSORED SO
!     SUBTRACT 1 TO RESTORE 0/1 CODING.  IF 0/1 CODING IS NOT USED, THE
!     SMALLER VALUE IS INTERPRETED AS CENSORED AND THE LARGER VALUE AS
!     UNCENSORED.  IF THERE IS ONLY 1 DISTINCT VALUE, THEN ASSUME NO
!     CENSORING.  IF THERE ARE MORE THAN 2 DISTINCT VALUES, THEN TREAT
!     AS AN ERROR.
!
      CALL CODE(TAG,NX,IWRITE,TEMP1,TEMP2,MAXNXT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      DO 100 II=1,NX
        TAG(II)=TEMP1(II) - 1.0
  100 CONTINUE
      CALL DISTIN(TAG,NX,IWRITE,TEMP2,NDIST,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(NDIST.EQ.1)THEN
        ICASE2='UNCE'
      ELSEIF(NDIST.EQ.2)THEN
        ICASE2='CENS'
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** EROR IN TIME TO FAILURE (TTT)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      THERE ARE MORE THAN TWO DISTINCT VALUES FOR THE ',   &
               'CENSORING VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NDIST
  105   FORMAT('      THE NUMBER OF DISTINCT VALUE = ',I8)
        CALL DPWRST('XXX','BUG ')
        ICASE2='ERRO'
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     CHECK THAT ALL CENSORED VALUES ARE AT THE END
!
      IF(ICASE2.EQ.'CENS')THEN
        IFLAGC=0
        NCENS=0
        NR=0
        DO 120 I=1,NX
          IF(TAG(I).EQ.1.0)THEN
            NR=NR+1
            IF(IFLAGC.EQ.1)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,123)
  123         FORMAT('      AN UNCENSORED VALUE OCCURRED AFTER A ',   &
                     'CENSORED VALUE.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
          ELSEIF(TAG(I).EQ.0.0)THEN
            NCENS=NCENS+1
            IFLAGC=1
          ENDIF
  120   CONTINUE
      ENDIF
!
      AN=REAL(NX)
      DO 200 I=1,NX
        DSUM=0.0D0
        DO 210 J=1,I
          C=AN - REAL(J) + 1.0
          TERM1=X(J)
          IF(J.GT.1)THEN
            TERM2=X(J-1)
          ELSE
            TERM2=0.0
          ENDIF
          DSUM=DSUM + DBLE(C*(TERM1 - TERM2))
  210   CONTINUE
        Y(I)=REAL(DSUM)
  200 CONTINUE
      NOUT=NX
      IF(ICASE2.EQ.'CENS')NOUT=NR
!
      AFACT=0.0
      IF(ICASE.EQ.'SCAL')THEN
        AFACT=Y(NOUT)
        IF(AFACT.EQ.0.0)AFACT=1.0
        DO 310 I=1,NOUT
          Y(I)=Y(I)/AFACT
  310   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTT ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF CUMHAZ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,ICASE2,NOUT,AFACT
 9012   FORMAT('IERROR,ICASE2,NOUT,AFACT = ',2(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,X(I),TAG(I),Y(I)
 9016     FORMAT('I,X(I),TAG(I),Y(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE TTT

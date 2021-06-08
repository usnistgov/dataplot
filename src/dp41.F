      SUBROUTINE MAINGR(ANOPL1,ANOPL2,NPLOTV,NPLOTP,NS,ICASPL,
     1                  MAXNPP,ISEED,IBOOSS,
     1                  IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                  IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,
     1                  BARHEF,BARWEF,
     1                  IRHSTG,IHSTCW,IHSTEB,IHSTOU,IASHWT,
     1                  IHSTMC,IHSTOP,
     1                  ICAPSW,IFORSW,IGUIFL,IERRFA,IFRALI,
     1                  IAND1,IAND2,ICONT,NUMHPP,NUMVPP,MAXNXT,
     1                  ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAINGR.
C              (THE   GR    AT THE END OF    MAINGR   STANDS FOR   GRAPHICS)
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES GRAPHICS COMMANDS.
C              THE GRAPHICS COMMANDS SEARCHED FOR BY MAINGR ARE AS FOLLOWS--
C
C                     ANOP PLOT (= PROPORTION PLOT)
C                     ... BOX PLOT
C                     BOX-COX NORMALITY PLOT
C                     BOX-COX HOMOSCEDASTICITY PLOT
C                     BOX-COX SYMMETRY PLOT (NOT DONE)
C                     BOX-COX LINEARITY PLOT
C                     BOX-COX STANDARDIZED EFFECTS PLOT (NOT DONE)
C                     COMPLEX DEMODULATION ... PLOT
C                     CONTOUR PLOT
C                     ... CONTROL CHART
C                     ... CORRELATION PLOT
C                     ... FFT PLOT             (NOT DONE)
C                     ... FREQUENCY PLOT
C                     ... HISTOGRAM
C                     ... HOMOSCEDASTICITY PLOT
C                     ... I PLOT
C                     INTERACTION PLOT
C                     LAG ... PLOT
C                     ... NORMALITY PLOT
C                     PERCENT POINT PLOT
C                     ... PERIODOGRAM
C                     PIE CHART
C                     PLOT
C                     ... PROBABILITY PLOT
C                     ... PPCC (PROBABILITY PLOT CORRELATION
C                              COEFFICIENT) PLOT
C                     ... ROOTOGRAM
C                     RUN SEQUENCE PLOT
C                     RUNS ... PLOT
C                     ... SPECTRAL PLOT
C                     3-D PLOT
C                     3-D ... FREQUENCY  PLOT   (NOT DONE)
C                     3-D ... HISTOGRAM         (NOT DONE)
C                     4-PLOT ... ANALYSIS (DONE IN MAIN)
C                     BAR CHART
C                     STEM AND LEAF DIAGRAM
C                     ... STATISTIC PLOT
C                     YOUDEN PLOT
C                     ... BIHISTOGRAM
C                     ERROR BAR PLOT    OCTOBER 1988
C                     FRACTAL PLOT      DECEMBER 1988
C                     POINCARE PLOT     DECEMBER 1988
C                     (REPLACED BY PHASE PLANE DIAGRAM  JULY 1989)
C                     JACKNIFE ... STATISTIC PLOT   JANUARY 1989
C                     BOOTSTRAP ... STATISTIC PLOT  JANUARY 1989
C                     DEX/DOE EXP DESIGN ... PLOT MAY       1989
C                     TAIL AREA PLOT                        1989
C                     NORMAL PLOT                 MAY       1990
C                     PHD PLOT (KER-CHAU LIE)     OCTOBER   1991
C                         (NOT IMPLEMENTED YET)
C                     BLOCK  PLOT                 APRIL     1992.
C                     <STAT> BLOCK                JUNE      1992.
C                     SYMBOL PLOT                 AUGUST    1992.
C                     VECTOR PLOT                 AUGUST    1992
C                     ANDREWS PLOT                NOVEMBER  1992
C                     PARTIAL AUTOCORR. PLOT      FEBRUARY  1993
C                     Q ... CONTROL CHART         DECEMBER  1993
C                     CME (CONT. MEAN EXCEEDANCE) PLOT DECEMBER 1993
C                     CONDITIONAL ... PLOT        DECEMBER  1993
C                     ... COMOVEMENT  PLOT        OCTOBER   1997
C                     KAPLAN MEIER    PLOT        MAY       1998
C                     DUANE           PLOT        MAY       1998
C                     EMPIRICAL CDF   PLOT        MAY       1998
C                     EXPONENTIAL HAZARD PLOT     MAY       1998
C                     NORMAL      HAZARD PLOT     MAY       1998
C                     LOGNORMAL   HAZARD PLOT     MAY       1998
C                     WEIBULL     HAZARD PLOT     MAY       1998
C                     HOTELLING CONTROL CHART     MAY       1998
C                     SEASONAL SUBSERIES PLOT     FEBRUARY  1999
C                     SPREAD-LOCATION PLOT        AUGUST    1999
C                     TUKEY MEAN-DIFFERENCE PLOT  SEPTEMBER 1999
C                     INTERACTION   PLOT          OCTOBER   1999
C                     ... INTERACTION STAT PLOT   OCTOBER   1999
C                     CROSS TABULATE <STAT> PLOT  DECEMBER  1999
C                     DEX CONTOUR PLOT            JANUARY   2000
C                     YATES CUBE  PLOT            JANUARY   2000
C                     BAG PLOT                    JANUARY   2001
C                         (NOT IMPLEMENTED YET)
C                     KERNEL DENSITY PLOT         AUGUST    2001
C                     CONSENSUS MEAN PLOT         AUGUST    2001
C                     PARTIAL RESIDUAL PLOT       JUNE      2002
C                     PARTIAL REGRESSION PLOT     JUNE      2002
C                     PARTIAL LEVERAGE PLOT       JUNE      2002
C                     CCPR PLOT                   JUNE      2002
C                     INFLUENCE CURVE <STAT> PLOT JULY      2002
C                     SHIFT PLOT                  FEBRUARY  2003
C                     VIOLIN PLOT                 FEBRUARY  2003
C                     PARALLEL COORDINATES PLOT   MARCH     2003
C                     PEAKS OVER THRESHOLD PLOT   APRIL     2005
C                     REPAIR PLOT                 OCTOBER   2006
C                     MEAN REPAIR FUNCTION PLOT   OCTOBER   2006
C                     TRILINEAR PLOT              DECEMBER  2006
C                     ROC CURVE                   APRIL     2007
C                     ROSE PLOT                   APRIL     2007
C                     BIVARIATE NORMAL TOLERANCE
C                         REGION PLOT             MAY       2007
C                     BIVARIATE NORMAL CONFIDENCE
C                         REGION PLOT             NOVEMBER  2013
C                     BINARY <TYPE> PLOT          MAY       2007
C                     ORD PLOT                    MAY       2007
C                     POISSON PLOT                MAY       2007
C                     BINOMIAL PLOT               MAY       2007
C                     NEGATIVE BINOMIAL PLOT      MAY       2007
C                     GEOMETRIC PLOT              MAY       2007
C                     LOGARITHMIC SERIES PLOT     MAY       2007
C                     ASSOCIATION PLOT            JUNE      2007
C                     SIEVE PLOT                  JUNE      2007
C                     PSUEDO ROC CURVE            JULY      2007
C                     LEVEL PLOT                  MARCH     2008
C                     (DISCRETE CONTOUR PLOT)
C                     IMAGE PLOT                  MARCH     2008
C                     SPATIAL DISTRIBUTION PLOT   APRIL     2008
C                     (UNDER DEVELOPMENT)
C                     FLUCUATION PLOT             MAY       2008
C                     STRIP PLOT                  OCTOBER   2008
C                     DETECTIION LIMIT PLOT       DECEMBER  2008
C                     (UNDER DEVELOPMENT)
C                     TABULATION PLOT             SEPTEMBER 2009
C                     ISO 13528 PLOT              FEBRUARY  2012
C                     ISO 13528 ZSCORE PLOT       FEBRUARY  2012
C                     ISO 13528 JSCORE PLOT       FEBRUARY  2012
C                     ISO 13528 RLP PLOT          FEBRUARY  2012
C                     FRECHET PLOT                OCTOBER   2013
C                     DISTRIBUTIONAL FIT PLOT     AUGUST    2014
C                     LORENZ CURVE                FEBRUARY  2015
C                     H CONSISTENCY PLOT          MAY       2015
C                     K CONSISTENCY PLOT          MAY       2015
C                     COCHRAN VARIANCE PLOT       MAY       2015
C                     MOVING STATISTIC PLOT       MAY       2015
C                     CUMULATIVE STATISTIC PLOT   MAY       2015
C                     TWO-WAY <ROW/COLUMN> PLOT   JUNE      2015
C                     TWO FACTOR PLOT             JUNE      2015
C                     EMPIRICAL QUANTILE PLOT     FEBRUARY  2017
C                     TIQ PLOT                    MARCH     2017
C                     QUANTILE BOX PLOT           MARCH     2017
C                     BLAND ALTMAN PLOT           JULY      2017
C                     NORM KERN DENSITY MIXT PLOT JULY      2017
C                     DEX ORDER PLOT              FEBRUARY  2018
C                     CLASSIFICATION ... PLOT     FEBRUARY  2019
C                     TOTAL TIME ON TEST PLOT     JUNE      2020
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MARCH     1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C        ETC.
C     UPDATED         --AUGUST    1987. BOX-COX STANDARDIZED EFFECTS PLOT
C     UPDATED         --JANUARY   1988. (... STATISTIC PLOTS)
C     UPDATED         --JANUARY   1988. (... CHARTS)
C     UPDATED         --FEBRUARY  1988. PROFILE PLOT
C     UPDATED         --FEBRUARY  1988. STAR PLOT
C     UPDATED         --AUGUST    1988. CONTOUR PLOT
C     UPDATED         --AUGUST    1988. PARETO PLOT
C     UPDATED         --SEPTEMBER 1988. EQUATE PROPROTION PLOT TO ANOP PLOT
C     UPDATED         --SEPTEMBER 1988. YOUDEN PLOT (= PLOT WITH 3 ARGS)
C     UPDATED         --SEPTEMBER 1988. BIHISTOGRAM
C     UPDATED         --NOVEMBER  1988. ERROR BAR PLOT
C     UPDATED         --DECEMBER  1988. ISEED ARGUMENT--FRACTAL PLOT
C     UPDATED         --DECEMBER  1988. POINCARE PLOT
C     UPDATED         --JANUARY   1989. JACKNIFE ... STAT PLOTS
C     UPDATED         --JANUARY   1989. BOOTSTRAP ... STAT PLOTS
C     UPDATED         --FEBRUARY  1989. CONTINUE CHARACTER CONFLICT (ALAN)
C     UPDATED         --APRIL     1989. SCATTER PLOT (= SYNONYM FOR PLOT)
C     UPDATED         --MAY       1989. DEX/DOE ... PLOT
C     UPDATED         --MAY       1989. TAIL AREA PLOT
C     UPDATED         --JULY      1989. POINCARE PLOT TO PHASE PLANE DIAG
C     UPDATED         --MAY       1990. NORMAL PLOT
C     UPDATED         --OCTOBER   1991. PHD PLOT (NOT DONE YET)
C     UPDATED         --APRIL     1992. BLOCK PLOT
C     UPDATED         --JUNE      1992. <STAT> BLOCK PLOT
C     UPDATED         --AUGUST    1992. VECTOR PLOT, SYMBOL PLOT
C     UPDATED         --NOVEMBER  1992. ANDREWS PLOT
C     UPDATED         --FEBRUARY  1993. PARTIAL AUTOCORRELATION PLOT
C     UPDATED         --JULY      1993. ARGUMENTS TO FRACTAL PLOT
C     UPDATED         --AUGUST    1993. CONFLICT WITH MEDIAN POLISH
C     UPDATED         --DECEMBER  1993. ADD ARG IN CALL DPPP()
C     UPDATED         --DECEMBER  1993. Q ... CONTROL CHART
C     UPDATED         --DECEMBER  1993. CME PLOT
C     UPDATED         --DECEMBER  1993. COND. ... EXCEEDANCE PLOT
C     UPDATED         --DECEMBER  1994. AUGMENT DPPARE() ARG. LIST
C     UPDATED         --MARCH     1995. ADD MAXNXT TO DPBLOC 
C     UPDATED         --MARCH     1996. ADD IRHSTG TO DPHIST 
C     UPDATED         --OCTOBER   1997. COMOVEMENT PLOT
C     UPDATED         --OCTOBER   1997. AUTO COMOVEMENT PLOT
C     UPDATED         --MAY       1998. KAPLAN MEIER PLOT
C     UPDATED         --MAY       1998. DUANE PLOT
C     UPDATED         --MAY       1998. EMPIRICAL CDF PLOT
C     UPDATED         --SEPTEMBER 1998. HOTELLING CONTROL CHART
C     UPDATED         --FEBRUARY  1999. SEASONAL SUBSERIES PLOT
C     UPDATED         --AUGUST    1999. SPREAD-LOCATION PLOT
C     UPDATED         --SEPTEMBER 1999. TUKEY MEAN-DIFFERENCE PLOT
C     UPDATED         --OCTOBER   1999. INTERACTION PLOT
C     UPDATED         --OCTOBER   1999. INTERACTION STATISTIC PLOT
C     UPDATED         --DECEMBER  1999. IMPLEMENT SUB-REGIONS
C     UPDATED         --DECEMBER  1999. SAVE SOME INTERNAL PARAMETERS
C                                       FOR ALL PLOTS
C     UPDATED         --DECEMBER  1999. CROSS TABULATE PLOT
C     UPDATED         --JANUARY   2000. DEX CONTOUR PLOT
C     UPDATED         --JANUARY   2001. BAG PLOT (NOT WORKING)
C     UPDATED         --AUGUST    2001. KERNEL DENSITY PLOT
C     UPDATED         --AUGUST    2001. CONSENSUS MEAN PLOT
C     UPDATED         --MARCH     2002. ROBUSTNESS PLOT SYNONUM
C                                       FOR BLOCK PLOT
C     UPDATED         --JULY      2002. INFLUENCE CURVE
C     UPDATED         --OCTOBER   2002. CALL LIST TO CONSENUSE MEAN
C                                       PLOT
C     UPDATED         --FEBRUARY  2003. SHIFT PLOT
C     UPDATED         --FEBRUARY  2003. VIOLIN PLOT
C     UPDATED         --MARCH     2003. PARALLEL COORDINATES PLOT
C     UPDATED         --SEPTEMBER 2003. BCA <BOOTSTRAP/JACKINFE>
C     UPDATED         --MAY       2004. KOLMOGOROV SMIRNOV PLOT AS
C                                       VARIANT OF PPCC PLOT
C     UPDATED         --SEPTEMBER 2004. CALL LIST TO DPHIST
C     UPDATED         --APRIL     2005. PEAKS OVER THRESHOLD PLOT
C     UPDATED         --MARCH     2006. ADD IFORSW TO CONSENSUS MEAN
C                                       PLOT
C     UPDATED         --OCTOBER   2006. REPAIR PLOT
C     UPDATED         --OCTOBER   2006. MEAN REPAIR FUNCTION PLOT
C     UPDATED         --DECEMBER  2006. TRILINEAR PLOT
C     UPDATED         --APRIL     2007. ROC CURVE
C     UPDATED         --APRIL     2007. ROSE PLOT
C     UPDATED         --MAY       2007. BIVARIATE NORMAL TOLERANCE
C                                       REGION PLOT
C     UPDATED         --MAY       2007. BINARY PLOT
C     UPDATED         --MAY       2007. ORD PLOT
C     UPDATED         --JUNE      2007. ASSOCIATION PLOT
C     UPDATED         --JUNE      2007. SIEVE PLOT
C     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO
C                                       COMMON
C     UPDATED         --JANUARY   2008. ADJUST USE OF DPCOZ3.INC
C                                       STORAGE
C     UPDATED         --MARCH     2008. LEVEL (DISCRETE CONTOUR) PLOT
C     UPDATED         --MARCH     2008. IMAGE PLOT
C     UPDATED         --APRIL     2008. SPATIAL DISTRIBUTION PLOT
C                                       (STILL UNDER DEVELOPMENT)
C     UPDATED         --MAY       2008. FLUCUATION PLOT
C     UPDATED         --OCTOBER   2008. STRIP PLOT
C     UPDATED         --SEPTEMBER 2009. TABLE <STAT> PLOT
C     UPDATED         --OCTOBER   2009. "BATCH MULTIPLE" OPTION
C                                       FOR STRIP PLOT
C     UPDATED         --JANUARY   2010. CALL LIST TO DPHIST
C     UPDATED         --FEBRUARY  2012. ISO 13528 PLOT
C     UPDATED         --FEBRUARY  2012. ISO 13528 ZSCORE PLOT
C     UPDATED         --FEBRUARY  2012. ISO 13528 JSCORE PLOT
C     UPDATED         --FEBRUARY  2012. ISO 13528 RLP PLOT
C     UPDATED         --OCTOBER   2013. FRECHET PLOT
C     UPDATED         --NOVEMBER  2013. BIVARIATE NORMAL CONFIDENCE
C                                       REGION PLOT
C     UPDATED         --AUGUST    2014. DISTRIBUTIONAL FIT PLOT
C     UPDATED         --FEBRUARY  2015. LORENZ CURVE
C     UPDATED         --MAY       2015. H CONSISTENCY PLOT
C     UPDATED         --MAY       2015. K CONSISTENCY PLOT
C     UPDATED         --MAY       2015. COCHRAN VARIANCE PLOT
C     UPDATED         --MAY       2015. <stat> CUMULATIVE STATISTIC PLOT
C     UPDATED         --MAY       2015. <stat> MOVING STATISTIC PLOT
C     UPDATED         --JUNE      2015. TWO WAY <ROW/COLUMN> PLOT
C     UPDATED         --JUNE      2015. TWO FACTOR PLOT
C     UPDATED         --JUNE      2016. <stat> WINDOW STATISTIC PLOT
C     UPDATED         --FEBRUARY  2017. EMPIRICAL QUANTILE PLOT
C     UPDATED         --MARCH     2017. TIQ PLOT
C     UPDATED         --JULY      2017. BLAND ALTMAN PLOT
C     UPDATED         --JULY      2017. NORMAL KERNEL DENSITY MIXTURE PLOT
C     UPDATED         --FEBRUARY  2018. DEX ORDER PLOT
C     UPDATED         --FEBRUARY  2019. CLASSIFICATION ... PLOT
C     UPDATED         --MARCH     2019. CALL LIST TO DPBLOC
C     UPDATED         --JUNE      2020. TOTAL TIME ON TEST PLOT
C     UPDATED         --APRIL     2021. IMPROVE ALGORITHM FOR
C                                       DEFAULT SUBREGIONS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
CCCCC CHARACTER*4 ICASSW
      CHARACTER*4 IX1TSV
      CHARACTER*4 IX2TSV
      CHARACTER*4 IY1TSV
      CHARACTER*4 IY2TSV
      CHARACTER*4 IX1ZSV
      CHARACTER*4 IX2ZSV
      CHARACTER*4 IY1ZSV
      CHARACTER*4 IY2ZSV
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IDIREC
      CHARACTER*4 IWRITE
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IRHSTG
      CHARACTER*4 IBCABT
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IHSTOP
      CHARACTER*4 IASHWT
      CHARACTER*4 IGUIFL
      CHARACTER*4 IERRFA
      CHARACTER*4 IFRALI
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASZZ
C
      DIMENSION TEMPZZ(2)
CCCCC DIMENSION TEMP(*)
CCCCC DIMENSION TEMP2(*)
CCCCC DIMENSION TEMP3(*)
CCCCC DIMENSION XTEMP1(*)
CCCCC DIMENSION XTEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOCO.INC'
      INCLUDE 'DPCOHO.INC'
C
CCCCC TO AVOID NAME CONFLICTS, ONLY BRING IN THE SPECIFIC
CCCCC COMMON BLOCK (NOT ALL OF DPCOST.INC)
C
      CHARACTER*4  IERRST
      COMMON/CSETG/IERRST
C
C
      INCLUDE 'DPCOZ3.INC'
C
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      EQUIVALENCE (G3RBAG(KGARB5),TEMP(1))
      EQUIVALENCE (G3RBAG(KGARB6),TEMP2(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
CCCCC ICONT=IDEVCN(1)
CCCCC ICOLOR=IDEVCL(1)
CCCCC NUMHPP=IDEVPP(1,1)
CCCCC NUMVPP=IDEVPP(1,2)
      ISUBN1='MAIN'
      ISUBN2='GR  '
C
      NACC=0
      NREJ=0
      NTOT=0
C
      IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAINGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ICOLOR,NUMHPP,NUMVPP
   52   FORMAT('ICONT,ICOLOR,NUMHPP,NUMVPP = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGGR,IBUGG2,IBUGG3
   53   FORMAT('IBUGGR,IBUGG2,IBUGG3 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGPL,IBUGP,IBUGP1,IBUGP2,IBUGP3
   54   FORMAT('IBUGPL,IBUGP,IBUGP1,IBUGP2,IBUGP3 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IBUGCO,IBUGEV,IBUGQ,ISUBRO
   55   FORMAT('IBUGCO,IBUGEV,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IANGLU,MAXNPP,ISEED,IBOOSS
   57   FORMAT('IANGLU,MAXNPP,ISEED,IBOOSS = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ICASPL,IAND1,IAND2,IFENSW
   59   FORMAT('ICASPL,IAND1,IAND2,IFENSW = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IFOUND,IERROR,ICOM,ICOM2
   60   FORMAT('IFOUND,IERROR,ICOM,ICOM2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)NUMARG,MAXNPP,ANOPL1,ANOPL2
   68   FORMAT('NUMARG,MAXNPP,ANOPL1,ANOPL2 = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IX1TSC,IX2TSC,IY1TSC,IY2TSC
   81   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)IX1TSV,IX2TSV,IY1TSV,IY2TSV
   82   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
      IF(ICOM.EQ.'LET ')GOTO9000
      IBCABT='OFF'
C
C               ***********************************************
C               **  TREAT THE EMPIRICAL QUANTILE PLOT  CASE  **
C               **            QUANTILE BOX PLOT        CASE  **
C               ***********************************************
C
      IF((ICOM.EQ.'EMPI' .AND. IHARG(1).EQ.'QUAN') .OR.
     1   (IHARG(1).EQ.'EMPI' .AND. IHARG(2).EQ.'QUAN'))THEN
        CALL DPEQFU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'QUAN' .AND. IHARG(1).EQ.'BOX ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
        CALL DPEQFU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************
C               **  TREAT THE BOX PLOT CASE  **
C               *******************************
C
      IF(
     1  ICOM.EQ.'BOX' .OR. IHARG(1).EQ.'BOX' .OR.
     1  IHARG(2).EQ.'BOX')THEN
        CALL DPBOX(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             ICONT,IFENSW,IBUGG2,IBUGG3,IBUGQ,ISUBRO,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************************
C               **  TREAT THE DISTRIBUTIONAL FIT PLOT CASE  **
C               **********************************************
C
      IF(ICOM.EQ.'DIST' .AND. IHARG(1).EQ.'FIT ' .AND.
     1  IHARG(2).EQ.'PLOT')THEN
        CALL DPDFPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(IHARG(1).EQ.'DIST' .AND. IHARG(2).EQ.'FIT ' .AND.
     1  IHARG(3).EQ.'PLOT')THEN
        CALL DPDFPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE VIOLIN PLOT CASE  **
C               **********************************
C
      IF(
     1  ICOM.EQ.'VIOL' .OR. IHARG(1).EQ.'VIOL' .OR.
     1  IHARG(2).EQ.'VIOL')THEN
        CALL DPVIOL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ICONT,IFENSW,IKDETY,IKDENP,PKDEWI,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************************
C               **  TREAT THE COMPLEX DEMODULATION ... PLOT CASE  **
C               ****************************************************
C
CCCCC IF(ICOM.EQ.'COMP')GOTO200
      IF(ICOM.EQ.'COMP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'DEMO')THEN
        CALL DPCD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IANGLU,DEMOFR,DEMODF,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE ... CONTROL CHART CASE  **
C               ****************************************
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT                FEBRUARY 1989
CCCCC AND REPLACED BY THE SUCCEEDING LINE                 FEBRUARY 1989
CCCCC TO AVOID A CONFLICT WITH THE CONTINUE CHARACTER     FEBRUARY 1989
CCCCC IF(ICOM.EQ.'CONT')GOTO300
CCCCC ADD HOTELLING CONTROL CHART (= MULTIVARIATE CONTROL
CCCCC CHART)                                             SEPTEMBER 1998
CCCCC SUPPORT FOUR DISTINCT CASES FOR HOTELLING CONTROL  FEBRUARY 2003
CCCCC CHART:
CCCCC   1) PHASE I HOTELLING CONTROL CHART
CCCCC   2) PHASE I HOTELLING INDIVIDUAL CONTROL CHART
CCCCC   3) PHASE II HOTELLING CONTROL CHART
CCCCC   4) PHASE II HOTELLING INDIVIDUAL CONTROL CHART
CCCCC IF PHASE <I/II> OMITTED, ASSUME A PHASE I CHART.
C
      IF(ICOM.EQ.'CONT'.AND.ICOM2.NE.'INUE')GOTO300
      IF(ICOM.EQ.'CONT'.AND.ICOM2.NE.'OUR ')GOTO300
      IF(ICOM.EQ.'CONT'.AND.IHARG(1).NE.'LOOP')GOTO300
C
      IF(ICOM.EQ.'PHAS')THEN
        IF(IHARG(1).EQ.'I'.OR.IHARG(1).EQ.'ONE'.OR.IHARG(1).EQ.'1')THEN
          CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'II'.OR.IHARG(1).EQ.'TWO'.OR.
     1         IHARG(1).EQ.'2')THEN
          CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'HOTE'.OR.
     1  (ICOM.EQ.'MULT'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CONT'))THEN
        CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1  ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C     2015/09: CHECK FOR CONFLICT WITH CONTOUR OR DEX CONTOUR
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT'.AND.
     1   IHARG2(1).NE.'OUR ')GOTO300
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')GOTO300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CONT'.AND.
     1   IHARG2(2).NE.'OUR ')GOTO300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHAR')GOTO300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CONT'.AND.
     1   IHARG2(3).NE.'OUR ')GOTO300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHAR')GOTO300
      GOTO399
C
  300 CONTINUE
      CALL DPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1          ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
  399 CONTINUE
C
C               *******************************************
C               **  TREAT THE ... CORRELATION PLOT CASE  **
C               *******************************************
C
C 2012/1: FOLD IN COMOVEMENT PLOT IN WITH CORRELATION PLOT.
C
      IF(ICOM.EQ.'AUTO' .OR. ICOM.EQ.'CROS' .OR.
     1   ICOM.EQ.'PART' .OR. ICOM.EQ.'COMO' .OR.
     1   IHARG(1).EQ.'AUTO' .OR. IHARG(2).EQ.'CROS' .OR.
     1   IHARG(1).EQ.'PART' .OR. IHARG(1).EQ.'COMO')THEN
        CALL DPCORR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE ... FREQUENCY PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'FREQ' .OR. IHARG(1).EQ.'FREQ' .OR.
     1   IHARG(2).EQ.'FREQ' .OR. IHARG(3).EQ.'FREQ' .OR.
     1   IHARG(4).EQ.'FREQ')THEN
        CALL DPFREQ(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              CLLIMI,CLWIDT,
     1              IRHSTG,IHSTCW,IHSTEB,IHSTOU,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE ... HISTOGRAM CASE  **
C               ************************************
C
      IF(ICOM.EQ.'HIST' .OR. ICOM.EQ.'ASH ')GOTO600
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HIST')GOTO600
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'HIST')GOTO600
      GOTO699
C
  600 CONTINUE
      CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1CLLIMI,CLWIDT,
CCCCC MARCH 1996.  ADD FOLLOWING LINE
     1IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,IHSTMC,IHSTOP,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
  699 CONTINUE
C
C               *****************************
C               **  TREAT THE I PLOT CASE  **
C               *****************************
C
C     10/18/2013: THERE ARE A NUMBER OF NEW VARIANTS TO THIS
C                 COMMAND.  SO CALL THIS ROUTINE AND LET DPI
C                 DETERMINE IF A VALID I PLOT COMMAND HAS BEEN
C                 ENTERED.
C
CCCCC IF(
CCCCC1  ICOM.EQ.'I' .OR. IHARG(1).EQ.'I' .OR.
CCCCC1  IHARG(2).EQ.'I' .OR. IHARG(3).EQ.'I')THEN
        CALL DPI(NPLOTV,NPLOTP,NS,ICASPL,ISEED,IAND1,IAND2,
     1           ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
CCCCC ENDIF
C
C               ***********************************
C               **  TREAT THE LAG ... PLOT CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'LAG' .OR. IHARG(1).EQ.'LAG')THEN
        CALL DPLAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE PERCENT POINT PLOT CASE  **
C               *****************************************
C
      IMAX=NUMARG-1
      IF(IMAX.LE.0)GOTO1099
      IFLAG1=0
      IFLAG2=0
      IFLAG3=0
      IF(ICOM.EQ.'PERC' .AND. ICOM2.NE.'ENTI')IFLAG1=1
      DO1010I=1,NUMARG
        IF(IHARG(I).EQ.'PERC' .AND. IHARG2(I).NE.'ENTI')IFLAG1=1
        IF(IHARG(I).EQ.'POIN')IFLAG2=1
        IF(IHARG(I).EQ.'PLOT')IFLAG3=1
 1010 CONTINUE
      IF(IFLAG1*IFLAG2*IFLAG3.EQ.1)THEN
        CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              CLLIMI,CLWIDT,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
 1099 CONTINUE
C
C               **************************************
C               **  TREAT THE ... PERIODOGRAM CASE  **
C               **************************************
C
CCCCC 2012/1: HANDLE WITH SPECTRAL PLOT
C
CCCCC IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ASD')GOTO9399
CCCCC IF(ICOM.EQ.'PERI')GOTO1100
CCCCC IF(ICOM2.EQ.'PERI')GOTO1100
CCCCC IF(ICOM2.EQ.'SPER')GOTO1100
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PERI')GOTO1100
CCCCC GOTO1199
C
C1100 CONTINUE
CCCCC CALL DPPERI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES')GOTO9000
C
C1199 CONTINUE
C
C               ********************************
C               **  TREAT THE PIE CHART CASE  **
C               ********************************
C
      IF(ICOM.EQ.'PIE')THEN
        CALL DPPIE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE PLOT CASE.          **
C               **  TREAT THE YOUDEN PLOT         **
C               **  AS A SPECIAL CASE OF PLOT     **
C               **  (PLOT WITH 3 ARGUMENTS).      **
C               **  TREAT THE SCATTER PLOT        **
C               **  AS A SYNONYM FOR PLOT         **
C               ************************************
C
      IF((ICOM.EQ.'YOUD' .OR. ICOM.EQ.'SCAT') .AND.
     1   IHARG(1).NE.'INDE')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGA2,IERROR)
      ELSEIF(ICOM.NE.'PLOT')THEN
        GOTO1399
      ENDIF
C
      IAND1=IAND2
      CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IANGLU,MAXNPP,
     1            IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,
     1            IFOUND,IERROR)
C
      IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
        WRITE(ICOUT,333)IFOUND,IERROR,IAND1,IAND2
  333   FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'YES')GOTO9000
CCCCC IF(IAND2.EQ.'YES')GOTO100
CCCCC IF(IAND2.EQ.'NO')GOTO9000
C
 1399 CONTINUE
C
C               ****************************************************
C               **  TREAT THE ... MOVING     STATISTIC PLOT CASE **
C               **  TREAT THE ... CUMULATIVE STATISTIC PLOT CASE **
C               **  TREAT THE ... WINDOW     STATISTIC PLOT CASE **
C               ***************************************************
C
      IF(ICOM.EQ.'FLUC')GOTO6399
      DO6302I=1,NUMARG
        IF(IHARG(I).EQ.'INTE'.AND.IHARG2(I).EQ.'RACT')GOTO6399
        IF(IHARG(I).EQ.'INFL'.AND.IHARG2(I).EQ.'UENC')GOTO6399
        IF(IHARG(I).EQ.'BLOC')GOTO6399
 6302 CONTINUE
      DO6303I=1,NUMARG-1
        IF(IHARG(I).EQ.'PROB' .AND. IHARG(I+1).EQ.'PLOT')GOTO6399
        IF(IHARG(I).EQ.'PPCC' .AND. IHARG(I+1).EQ.'PLOT')GOTO6399
 6303 CONTINUE
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GOTO6300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GOTO6300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GOTO6300
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GOTO6300
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GOTO6300
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GOTO6300
      GOTO6399
C
 6300 CONTINUE
      CALL DPMOSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            MAXNXT,ISEED,FILWID,
     1            ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6399 CONTINUE
C
C               ***********************************************
C               **  TREAT THE <DIST> TIQP        PLOT  CASE  **
C               ***********************************************
C
      IF(ICOM.EQ.'TIQ ' .OR. IHARG(1).EQ.'TIQ ' .OR.
     1   IHARG(2).EQ.'TIQ ' .OR. IHARG(3).EQ.'TIQ ')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'TRUN' .AND. IHARG(1).EQ.'INFO' .AND.
     1   IHARG(2).EQ.'QUAN')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(IHARG(1).EQ.'TRUN' .OR. IHARG(2).EQ.'TRUN' .OR.
     1   IHARG(3).EQ.'TRUN' .OR. IHARG(4).EQ.'TRUN')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE ... STATISTIC PLOT CASE **
C               ****************************************
C
      IF(ICOM.EQ.'FLUC')GOTO6699
      DO6602I=1,NUMARG
        IF(IHARG(I).EQ.'INTE'.AND.IHARG2(I).EQ.'RACT')GOTO6699
        IF(IHARG(I).EQ.'INFL'.AND.IHARG2(I).EQ.'UENC')GOTO6699
        IF(IHARG(I).EQ.'BLOC')GOTO6699
 6602 CONTINUE
      DO6603I=1,NUMARG-1
        IF(IHARG(I).EQ.'PROB' .AND. IHARG(I+1).EQ.'PLOT')GOTO6699
        IF(IHARG(I).EQ.'PPCC' .AND. IHARG(I+1).EQ.'PLOT')GOTO6699
 6603 CONTINUE
      DO6604I=1,NUMARG-2
        IF(IHARG(I).EQ.'CUMU' .AND. IHARG(I+1).EQ.'STAT' .AND.
     1     IHARG(I+2).EQ.'PLOT')GOTO6699
        IF(IHARG(I).EQ.'MOVI' .AND. IHARG(I+1).EQ.'STAT' .AND.
     1     IHARG(I+2).EQ.'PLOT')GOTO6699
 6604 CONTINUE
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GOTO6600
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GOTO6600
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GOTO6600
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GOTO6600
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GOTO6600
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GOTO6600
      GOTO6699
C
 6600 CONTINUE
      CALL DPSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1          MAXNXT,ISEED,ICONT,
     1          ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6699 CONTINUE
C
C               *******************************************
C               **  TREAT THE ... PROBABILITY PLOT CASE  **
C               *******************************************
C
      IMAX=NUMARG-1
      IF(IMAX.GT.1)THEN
        DO1410I=1,NUMARG
          IF(IHARG(I).EQ.'PROB')THEN
            CALL DPPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES')GOTO9000
          ENDIF
 1410   CONTINUE
      ENDIF
C
C               ************************************
C               **  TREAT THE ... PPCC PLOT CASE  **
C               ************************************
C
C     SINCE A NUMBER OF GOODNESS-OF-FIT STATISTICS ARE NOW
C     SUPPORTED, JUST CALL THIS COMMAND AND SEE IF DPPPCC
C     RECOGNIZES ONE OF THE SUPPORTED STATISTICS.  NO NEED TO
C     DUPLICATE HERE.
C
      CALL DPPPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICASP2,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
C               ****************************************
C               **  TREAT THE RUN SEQUENCE PLOT CASE  **
C               ****************************************
C
      IF((ICOM.EQ.'RUN'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'SEQU') .OR.
     1  (IHARG(1).EQ.'RUN'.AND.IHARG(2).EQ.'SEQU') .OR.
     1  (IHARG(2).EQ.'RUN'.AND.IHARG(3).EQ.'SEQU'))THEN
        CALL DPRUNS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE RUNS ... PLOT CASE  **
C               ************************************
C
      IF(ICOM.EQ.'RUNS'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPRUPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE ... SPECTRAL PLOT CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'AUTO')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'PLAN')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'I   ')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'1   ')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'ONE ')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'II  ')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'2   ')GOTO1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'TWO ')GOTO1899
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'SPLIN')GOTO1899
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'FIT')GOTO1899
C
      IF(ICOM.EQ.'AUTO' .OR. IHARG(1).EQ.'AUTO')GOTO1800
      IF(ICOM.EQ.'SPEC' .OR. IHARG(1).EQ.'SPEC')GOTO1800
      IF(ICOM.EQ.'PERI' .OR. IHARG(1).EQ.'PERI')GOTO1800
      IF(ICOM.EQ.'COSP' .OR. IHARG(1).EQ.'COSP')GOTO1800
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'SPEC')GOTO1800
      IF(IHARG(1).EQ.'QUAD' .AND. IHARG(2).EQ.'SPEC')GOTO1800
      IF(ICOM.EQ.'CROS'.AND.IHARG(1).EQ.'SPEC')GOTO1800
      IF(IHARG(1).EQ.'CROS'.AND.IHARG(2).EQ.'SPEC')GOTO1800
      IF(ICOM.EQ.'COHE' .OR. IHARG(1).EQ.'COHE')GOTO1800
      IF(ICOM.EQ.'AMPL' .OR. IHARG(1).EQ.'AMPL')GOTO1800
      IF(ICOM.EQ.'PHAS' .OR. IHARG(1).EQ.'PHAS')GOTO1800
      IF(ICOM.EQ.'GAIN' .OR. IHARG(1).EQ.'GAIN')GOTO1800
      IF(ICOM.EQ.'ARGA' .OR. IHARG(1).EQ.'ARGA')GOTO1800
      GOTO1899
C
 1800 CONTINUE
      CALL DPSPEC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 1899 CONTINUE
C
C               *********************************************
C               **  TREAT THE 3-D ... FREQUENCY PLOT CASE  **
C               *********************************************
C
C  NOTE: THIS COMMAND IS NOT IMPLEMENTED YET.
C
CCCCC IF(ICOM.EQ.'3D' .AND. IHARG(1).EQ.'FREQ')THEN
C
CCCCC   CALL DP3DFR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1              IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CCCCC ENDIF
CCCCC IF(IFOUND.EQ.'YES')GOTO9000
C
C2099 CONTINUE
C
C               ****************************************
C               **  TREAT THE 3-D ... HISTOGRAM CASE  **
C               ****************************************
C
C  NOTE: THIS COMMAND IS NOT IMPLEMENTED YET.
C
CCCCC IF(ICOM.EQ.'3D' .AND. IHARG(1).EQ.'HIST')THEN
CCCCC   CALL DP3DHI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1              IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CCCCC ENDIF
CCCCC IF(IFOUND.EQ.'YES')GOTO9000
C
C2199 CONTINUE
C
C               *******************************
C               **  TREAT THE 3-D PLOT CASE  **
C               *******************************
C
      IF(ICOM.EQ.'3D' .OR. ICOM.EQ.'3DPL' .OR.
     1  (ICOM.EQ.'3' .AND. IHARG(1).NE.'PARA'))THEN
        CALL DP3DPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,IFORSW,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,
     1              IFOUND,IERROR)
C
        IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
          WRITE(ICOUT,1933)IFOUND,IERROR,IAND1,IAND2
 1933     FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      IF(IFOUND.EQ.'YES')GOTO9000
CCCCC IF(IAND2.EQ.'YES')GOTO100
CCCCC IF(IAND2.EQ.'NO')GOTO9000
C
C               ***********************************************
C               **  TREAT THE BOX-COX NORMALITY        PLOT  **
C               **  TREAT THE BOX-COX LINEARITY        PLOT  **
C               **  TREAT THE BOX-COX HOMOSCEDASTICITY PLOT  **
C               ***********************************************
C
      IF(
     1  (ICOM.EQ.'BOX' .AND. IHARG(1).EQ.'COX') .OR.
     1   (IHARG(1).EQ.'BOX' .AND. IHARG(2).EQ.'COX'))THEN
         CALL DPBCNP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1               IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE PROPORTION PLOT CASE  **
C               **  = THE ANOP PLOT CASE            **
C               **************************************
C
      IF(
     1  (ICOM.EQ.'PROP'.AND.IHARG(1).EQ.'PLOT') .OR.
     1  (ICOM.EQ.'ANOP'.AND.IHARG(1).EQ.'PLOT') .OR.
     1  (ICOM.EQ.'ANAL'.AND.IHARG(1).EQ.'OF  ' .AND.
     1   IHARG(2).EQ.'PROP'.AND.IHARG(3).EQ.'PLOT') .OR.
     1  ICOM.EQ.'MULT')THEN
C
        CALL DPANPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,
     1              ANOPL1,ANOPL2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES')GOTO9000
C
      ENDIF
C
C               ************************************
C               **  TREAT THE BAR PLOT CASE       **
C               ************************************
C
      IF(ICOM.EQ.'BAR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')
     1GOTO2600
      IF(ICOM.EQ.'BAR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')
     1GOTO2600
      GOTO2699
C
 2600 CONTINUE
      CALL DPBARP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 2699 CONTINUE
C
C               *******************************
C               **  TREAT THE FFT PLOT CASE  **
C               *******************************
C
CCCCC IF(ICOM.EQ.'FFT'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')
CCCCC1GOTO2700
CCCCC GOTO2799
C
C2700 CONTINUE
CCCCC CALL DPFFTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES')GOTO9000
C
C2799 CONTINUE
C
C               ************************************
C               **  TREAT THE ... ROOTOGRAM CASE  **
C               ************************************
C
      IF(ICOM.EQ.'ROOT')GOTO2800
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ROOT')GOTO2800
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ROOT')GOTO2800
      GOTO2899
C
 2800 CONTINUE
CCCCC CALL DPROGR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1CLLIMI,CLWIDT,
CCCCC1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1CLLIMI,CLWIDT,
     1IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,IHSTMC,IHSTOP,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 2899 CONTINUE
C
C               ********************************************
C               **  TREAT THE STEM AND LEAF DIAGRAM CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'STEM')THEN
        CALL DPSTEM(IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************************
C               **  TREAT THE ALLAN VARIANCE PLOT CASE             **
C               **  TREAT THE ALLAN STANDARD DEVIATION PLOT CASE   **
C               *****************************************************
C
      IF(ICOM.EQ.'ALLA' .OR. ICOM.EQ.'AV' .OR. ICOM.EQ.'ASD' .OR.
     1   ICOM.EQ.'AS  ' .OR.
     1   IHARG(1).EQ.'ALLA' .OR. IHARG(1).EQ.'AV  ' .OR.
     1   IHARG(1).EQ.'ASD ' .OR. IHARG(1).EQ.'AS  ')THEN
        CALL DPALLA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************************
C               **  TREAT THE COMPLEX REMODULATION PLOT CASE      **
C               ****************************************************
C
      IF(ICOM.EQ.'REMO')GOTO3300
      IF(ICOM.EQ.'COMP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'REMO')GOTO3300
      GOTO3399
C
 3300 CONTINUE
      CALL DPREMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,DEMOFR,DEMODF,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 3399 CONTINUE
C
C               ************************************
C               **  TREAT THE SYMMETRY PLOT CASE  **
C               ************************************
C
      IF(ICOM.EQ.'SYMM' .OR. IHARG(1).EQ.'SYMM')THEN
        CALL DPSYMM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************************
C               **  TREAT THE BOX-COX SYMMETRY PLOT CASE  **
C               ********************************************
C
CNNNN IF(NUMARG.GE.2.AND.ICOM.EQ.'BOX'.AND.
CNNNN1IHARG(1).EQ.'COX'.AND.IHARG(2).EQ.'SYMM')GOTO4200
CNNNN GOTO4299
C
C4200 CONTINUE
CNNNN CALL DPBCSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CNNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CNNNN IF(IFOUND.EQ.'YES')GOTO9000
C
C4299 CONTINUE
C
C               *********************************************
C               **  TREAT THE QUANTILE-QUANTILE PLOT CASE  **
C               *********************************************
C
      IF(ICOM.EQ.'QUAN' .OR.
     1  ((ICOM.EQ.'HIGH'.OR.ICOM.EQ.'SUBS') .AND.
     1    IHARG(1).EQ.'QUAN'))THEN
        CALL DPQUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,IBOOSS,ISEED,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE BAG               PLOT CASE  **
C               *********************************************
C
C     THIS IS NOT YET IMPLEMENTED.
C
      IF(ICOM.EQ.'BAG ')THEN
CCCCC   CALL DPBAGP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1              ISEED,MAXNPP,
CCCCC1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC   IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************************
C               **  TREAT THE HOMOSCEDASTICITY PLOT CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'REPL'.AND.IHARG(1).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'MULT'.AND.IHARG(1).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'SUMM'.AND.
     1   IHARG(2).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'HOMO'.AND.
     1   IHARG(2).EQ.'SUMM')GOTO4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'SUBS'.AND.
     1   IHARG(2).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'SUMM'.AND.
     1   IHARG(2).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'HOMO'.AND.
     1   IHARG(2).EQ.'SUMM')GOTO4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HIGH'.AND.
     1   IHARG(2).EQ.'HOMO')GOTO4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO'.AND.
     1   IHARG(2).EQ.'HIGH')GOTO4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO'.AND.
     1   IHARG(2).EQ.'SUBS')GOTO4400
      GOTO4499
C
 4400 CONTINUE
      CALL DPHOMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1ISEED,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 4499 CONTINUE
C
C               ***************************************
C               **  TREAT THE BIHISTOGRAM PLOT CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'BIHI' .OR. IHARG(1).EQ.'BIHI' .OR.
     1   IHARG(2).EQ.'BIHI' .OR. IHARG(3).EQ.'BIHI' .OR.
     1   ICOM.EQ.'BIRO' .OR. IHARG(1).EQ.'BIRO' .OR.
     1   IHARG(2).EQ.'BIRO' .OR. IHARG(3).EQ.'BIRO' .OR.
     1   (ICOM.EQ.'BIAS' .AND. ICOM2.EQ.'H   ') .OR.
     1   IHARG(1).EQ.'BIAS' .OR. IHARG(2).EQ.'BIAS')THEN
        CALL DPBIHI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              CLLIMI,CLWIDT,
     1              IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE YOUDEN PLOT CASE    **
C               ************************************
C
CNNNN IF(ICOM.EQ.'YOUDEN')GOTO4700
CNNNN GOTO4799
C
C4700 CONTINUE
CNNNN CALL DPYOUD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CNNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CNNNN IF(IFOUND.EQ.'YES')GOTO9000
C
C4799 CONTINUE
C
C               ************************************
C               **  TREAT THE GANOVA PLOT CASE    **
C               ************************************
C
CNNNN IF(ICOM.EQ.'GANO'.AND.ICOM2.EQ.'VA  ')GOTO4800
CNNNN GOTO4899
C
C4800 CONTINUE
CNNNN CALL DPGANO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CNNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CNNNN IF(IFOUND.EQ.'YES')GOTO9000
C
C4899 CONTINUE
C
C               *************************************
C               **  TREAT THE DRAFTSMAN PLOT CASE  **
C               *************************************
C
CNNNN IF(ICOM.EQ.'DRSF')GOTO6100
CNNNN GOTO6199
C
C6100 CONTINUE
CNNNN CALL DPDRAF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CNNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
CNNNN IF(IFOUND.EQ.'YES')GOTO9000
C
C6199 CONTINUE
C
C               ***********************************
C               **  TREAT THE CONTOUR PLOT CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'CONT'.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPCOPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
        IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
           WRITE(ICOUT,6233)IFOUND,IERROR,IAND1,IAND2
 6233      FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
           CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************************
C               **  TREAT THE BOX-COX  STANDARDIZED EFFECTS PLOT CASE  **
C               *********************************************************
C
      IF(NUMARG.GE.3.AND.ICOM.EQ.'BOX'.AND.
     1IHARG(1).EQ.'COX'.AND.IHARG(2).EQ.'STAN'.AND.
     1IHARG(3).EQ.'EFFE')GOTO6400
      GOTO6499
C
 6400 CONTINUE
CCCCC CALL DPBCSE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES')GOTO9000
C
 6499 CONTINUE
C
C               ************************************
C               **  TREAT THE WEIBULL  PLOT CASE  **
C               ************************************
C
C     OCTOBER 2013: ADD FRECHET PLOT
C
      IF((ICOM.EQ.'WEIB' .OR. ICOM.EQ.'FREC') .AND.
     1   IHARG(1).EQ.'PLOT')GOTO6510
      IF(ICOM.EQ.'HIGH' .AND.
     1   (IHARG(1).EQ.'WEIB' .OR. IHARG(1).EQ.'FREC') .AND.
     1   IHARG(2).EQ.'PLOT')GOTO6510
      IF(ICOM.EQ.'SUBS' .AND.
     1   (IHARG(1).EQ.'WEIB' .OR. IHARG(1).EQ.'FREC') .AND.
     1   IHARG(2).EQ.'PLOT')GOTO6510
      GOTO6599
C
 6510 CONTINUE
      CALL DPWEIB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IANGLU,MAXNPP,
     1            IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1            IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6599 CONTINUE
C
CCCCCC ADD FOLLOWING COMMAND DECEMBER 1999.
C               ****************************************************
C               **  TREAT THE CROSS TABULATE <STATISTIC> PLOT CASE**
C               ****************************************************
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'CROS'.AND.IHARG(1).EQ.'TABU')THEN
        DO16602I=2,NUMARG
          IF(IHARG(I).EQ.'PLOT')GOTO16600
16602   CONTINUE
        GOTO16699
C
16600   CONTINUE
        CALL DPCRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1    TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1    MAXNXT,
     1    ISEED,
     1    ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
16699 CONTINUE
C
C               ***********************************
C               **  TREAT THE PROFILE PLOT CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'PROF')GOTO6700
      GOTO6799
C
 6700 CONTINUE
      CALL DPPROF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6799 CONTINUE
C
C               ***********************************
C               **  TREAT THE STAR    PLOT CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'STAR')GOTO6800
      GOTO6899
C
 6800 CONTINUE
      CALL DPSTAR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6899 CONTINUE
C
C               **********************************
C               **  TREAT THE PARETO PLOT CASE  **
C               **********************************
C
      IF(ICOM.EQ.'PARE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'PLOT')GOTO6900
      GOTO6999
C
 6900 CONTINUE
      IDIREC='DECR'
CCCCC THE FOLLOWING ARGUMENT LIST WAS AUGMENTED   DECEMBER 1994
      CALL DPPARE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1ICONT,IDIREC,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
     1ICONT,IDIREC,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 6999 CONTINUE
C
C               *************************************
C               **  TREAT THE ERROR BAR PLOT CASE  **
C               *************************************
C
      IF(ICOM.EQ.'ERRO')GOTO7100
      GOTO7199
C
 7100 CONTINUE
      IF(IHARG(1).EQ.'PROB' .AND. IHARG(2).EQ.'PLOT')GOTO7199
      IF(IHARG(1).EQ.'PPCC' .AND. IHARG(2).EQ.'PLOT')GOTO7199
      IF(IHARG(1).EQ.'KOLM' .AND. IHARG(2).EQ.'SMIR')GOTO7199
      IF(IHARG(1).EQ.'CHI ' .AND. IHARG(2).EQ.'SQUA')GOTO7199
      IF(IHARG(1).EQ.'CHIS' .AND. IHARG(2).EQ.'GOOD')GOTO7199
      CALL DPERBA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 7199 CONTINUE
C
C               *************************************
C               **  TREAT THE FRACTAL PLOT CASE    **
C               *************************************
C
      IF(ICOM.EQ.'FRAC' .AND.
     1  (IHARG(1).EQ.'ITER' .OR. IHARG(1).EQ.'TYPE'))THEN
        CALL DPFRAC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,
     1              IANGLU,ISEED,
CCCCC               JULY 1993.  ADD FOLLOWING LINE
     1              IFRAIT,IFRATY,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS CHANGED FROM POINCARE PLOT   JULY 1989
CCCCC TO PHASE PLANE DIAGRAM                                 JULY 1989
C               ******************************************
C               **  TREAT THE PHASE PLANE DIAGRAM CASE  **
C               ******************************************
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'PHAS'.AND. IHARG(1).EQ.'PLAN' .AND.
     1  (IHARG(2).EQ.'DIAG' .OR. IHARG(2).EQ.'PLOT'))THEN
        CALL DPPPD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************************
C               **  TREAT THE JACKNIFE  ... STATISTIC PLOT CASE **
C               **  AND   THE BOOTSTRAP ... STATISTIC PLOT CASE **
C               **************************************************
C
CCCCC SEPTEMBER 2003: ADD BCA BOOTSTRAP/JACKNIFE
C
      IF(ICOM.EQ.'JACK')GOTO7400
      IF(ICOM.EQ.'BOOT')GOTO7400
      IF(ICOM.EQ.'BCA'.AND.
     1  (IHARG(1).EQ.'BOOT'.OR.IHARG(1).EQ.'JACK'))GOTO7400
      GOTO7499
C
 7400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FIT')GOTO7499
C
      IF(ICOM.EQ.'BCA')THEN
        ICOM=IHARG(1)
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IBCABT='ON'
      ENDIF
C
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.7.AND.IHARG(7).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.8.AND.IHARG(8).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.9.AND.IHARG(9).EQ.'PLOT')GOTO7410
      IF(NUMARG.GE.10.AND.IHARG(10).EQ.'PLOT')GOTO7410
      GOTO7499
 7410 CONTINUE
      CALL DPJBSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBOOSS,ISEED,IBCABT,
CCCCC1TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1MAXNXT,
     1ICAPSW,ICAPTY,IFORSW,
     1CLLIMI,CLWIDT,
     1ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 7499 CONTINUE
C
C               ****************************************
C               **  TREAT THE DEX CONTOUR PLOT CASE   **
C               ****************************************
C
      IF(ICOM.EQ.'DEX'.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'CONT'.AND.
     1   IHARG(2).EQ.'PLOT')THEN
        CALL DPDCNT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE YATES CUBE  PLOT CASE   **
C               ****************************************
C
      IF((ICOM.EQ.'DEX'.OR.ICOM.EQ.'YATE').AND.NUMARG.GE.2.AND.
     1   IHARG(1).EQ.'CUBE'.AND.IHARG(2).EQ.'PLOT')THEN
        CALL DPYACB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE DEX/DOE ... PLOT CASE   **
C               ****************************************
C
      IF(ICOM.EQ.'DEX ' .OR. ICOM.EQ.'DEXP' .OR. ICOM.EQ.'DOE ' .OR.
     1   ICOM.EQ.'DOX ' .OR. ICOM.EQ.'CLAS')THEN
        CALL DPDEXP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              MAXNXT,ISEED,ICONT,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE TAIL AREA PLOT CASE     **
C               **  (A SYNONYM IS SURVIVAL PLOT)      **
C               **  (MAY 1989)                        **
C               ****************************************
C
      IF(ICOM.EQ.'TAIL' .OR. ICOM.EQ.'SURV' .OR.
     1   IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV')THEN
        CALL DPTAIL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED JULY 2017
C               **************************************************
C               **  TREAT THE NORMAL KERNEL DENSITY MIXTURE     **
C               **  PLOT CASE                                   **
C               **************************************************
C
      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'KERN' .AND.
     1   IHARG(2).EQ.'DENS' .AND. IHARG(3).EQ.'MIXT' .AND.
     1   IHARG(4).EQ.'PLOT')THEN
          CALL DPNMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED MAY 1998
C               **************************************************
C               **  TREAT THE <NORMAL/LOGNORMAL/WEIBULL/HAZARD> **
C               **  PLOT CASE                                   **
C               **************************************************
C
      IF(ICOM.EQ.'NORM'.OR.ICOM.EQ.'LOGN'.OR.ICOM.EQ.'EXPO'.OR.
     1  ICOM.EQ.'WEIB'.OR.ICOM.EQ.'GUMB')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HAZA'.AND.
     1     IHARG(2).EQ.'PLOT')THEN
          CALL DPHAZA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IANGLU,MAXNPP,
     1                IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1                IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,
     1                IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
      IF(ICOM.EQ.'EXTR'.AND.IHARG(1).EQ.'VALU')THEN
        IF(NUMARG.GE.3.AND.IHARG(2).EQ.'HAZA'.AND.
     1     IHARG(3).EQ.'PLOT')THEN
          CALL DPHAZA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IANGLU,MAXNPP,
     1                IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1                IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,
     1                IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED MAY 1990
C               ************************************
C               **  TREAT THE NORMAL   PLOT CASE  **
C               ************************************
C
      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'PLOT')GOTO7710
      IF(ICOM.EQ.'HIGH' .AND. IHARG(1).EQ.'NORM' .AND.
     1   IHARG(2).EQ.'PLOT')GOTO7710
      IF(ICOM.EQ.'SUBS' .AND. IHARG(1).EQ.'NORM' .AND.
     1   IHARG(2).EQ.'PLOT')GOTO7710
      GOTO7799
C
 7710 CONTINUE
      CALL DPNORM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IANGLU,MAXNPP,
     1            IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1            IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 7799 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED    APRIL 1992 (JJF)
C               *********************************
C               **  TREAT THE BLOCK PLOT CASE  **
C               *********************************
C
CCCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT   JUNE 1992  JJF
CCCCC TO ACCOMODATE THE <STAT> BLOCK PLOTS   JUNE 1992   JJF
CCCCC IF(NUMARG.GE.1.AND.ICOM.EQ.'BLOC'.AND.
CCCCC1IHARG(1).EQ.'PLOT')GOTO7800
CCCCC GOTO7899
C
CCCCC THE FOLLOWING 10 LINES WERE ADDED TO AVOID     AUGUST 1993
CCCCC A CONFLICT WITH   MEIDAN POLISH   COMMAND      AUGUST 1993
      IF(ICOM.EQ.'ROBU'.AND.IHARG(1).EQ.'SMOO')GOTO7899
      IF(NUMARG.GE.1)THEN
         IF((ICOM.EQ.'BLOC'.OR.ICOM.EQ.'ROBU').AND.
     1      IHARG(1).EQ.'PLOT')GOTO7800
      ENDIF
      IF(NUMARG.GE.2)THEN
         IF((IHARG(1).EQ.'BLOC'.OR.IHARG(1).EQ.'ROBU').AND.
     1     IHARG(2).EQ.'PLOT')GOTO7800
      ENDIF
      IF(NUMARG.GE.3)THEN
         IF((IHARG(2).EQ.'BLOC'.OR.IHARG(2).EQ.'ROBU').AND.
     1     IHARG(3).EQ.'PLOT')GOTO7800
      ENDIF
CCCCC FOLLOWING 3 LINES ADDED MARCH 1995.
      IF(NUMARG.GE.4)THEN
         IF((IHARG(3).EQ.'BLOC'.OR.IHARG(3).EQ.'ROBU').AND.
     1      IHARG(4).EQ.'PLOT')GOTO7800
      ENDIF
      GOTO7899
C
 7800 CONTINUE
CCCCC MARCH 1995.  ADD MAXNXT TO ARGUMENT LIST.
CCCCC MARCH 2019.  ADD ICHMAP TO ARGUMENT LIST.
      CALL DPBLOC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            BARHEF,BARWEF,MAXNXT,ISEED,ICHMAP,ICONT,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 7899 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED OCTOBER 1991 (JJF)
C               *********************************
C               **  TREAT THE PHD PLOT CASE    **
C               *********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'PHD'.AND.
     1IHARG(1).EQ.'PLOT')THEN
CCCCC   CALL DPPHDP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1              TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
CCCCC1              ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED    AUGUST 1992 (ALAN)
C               *********************************
C               **  TREAT THE VECTOR PLOT CASE **
C               *********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VECT'.AND.
     1   IHARG(1).EQ.'PLOT')THEN
        CALL DPVECT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IVCFMT,IVCARR,IANGLU,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED    AUGUST 1992 (ALAN)
C               *********************************
C               **  TREAT THE SYMBOL PLOT CASE **
C               *********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SYMB'.AND.
     1   IHARG(1).EQ.'PLOT')THEN
        CALL DPPLSY(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED    NOVEMBER 1992 (ALAN)
C               **********************************
C               **  TREAT THE ANDREWS PLOT CASE **
C               **********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'ANDR'.AND.
     1   IHARG(1).EQ.'PLOT')THEN
CCCCC   PANINC=0.1
        CALL DPANDR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ANDINC,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED    MARCH 2003 (ALAN)
C               ***********************************************
C               **  TREAT THE PARALLEL COORDINATES PLOT CASE **
C               ***********************************************
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'PARA'.AND.
     1IHARG(1).EQ.'COOR'.AND.IHARG(2).EQ.'PLOT')THEN
        CALL DPPCPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(NUMARG.GE.3.AND.ICOM.EQ.'GROU'.AND.
     1       IHARG(1).EQ.'PARA'.AND. IHARG(2).EQ.'COOR'.AND.
     1       IHARG(3).EQ.'PLOT')THEN
        CALL DPPCPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED      DECEMBER 1993
C               ******************************************
C               **  TREAT THE Q ... CONTROL CHART CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'Q' .AND. IHARG(1).NE.'QUAN')THEN
        CALL DPQCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED      DECEMBER 1993
C               ************************************************
C               **  TREAT THE CME PLOT CASE                   **
C               **  TREAT THE COND. ... EXCEEDANCE PLOT CASE  **
C               ************************************************
C
C  MAY 1998.  CHECK FOR CONFLICT WITH "CME ESTIMATE" OR 
C             "CME GENERALIZED PARETO".
      IF(ICOM.EQ.'CME')GOTO8500
      IF(ICOM.EQ.'COND')GOTO8500
      IF(ICOM.EQ.'YANG')GOTO8500
      IF(ICOM.EQ.'LIFE')GOTO8500
      IF(ICOM.EQ.'MEAN')GOTO8500
      GOTO8599
C
 8500 CONTINUE
      IF(NUMARG.GE.1.AND.(IHARG(1).EQ.'ESTI'.OR.IHARG(1).EQ.'GENE'))
     1GOTO8599
      CALL DPCME(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 8599 CONTINUE
C
C               *******************************************
C               **  TREAT THE ... COMOVEMENT  PLOT CASE  **
C               *******************************************
C
C NOTE: FOLD COMOVEMENT PLOT IN WITH CORRELATION PLOT.
C
CCCCC IF(ICOM.EQ.'AUTO' .OR. ICOM.EQ.'CROS' .OR. ICOM.EQ.'COMO' .OR.
CCCCC1   IHARG(1).EQ.'AUTO' .OR. IHARG(1).EQ.'CROS' .OR.
CCCCC1   IHARG(1).EQ.'COMO')THEN
CCCCC   CALL DPCOMV(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC   IF(IFOUND.EQ.'YES')GOTO9000
CCCCC ENDIF
C
C               ****************************************
C               **  TREAT THE KAPLAN MEIER PLOT CASE  **
C               **  (MAY 1998)                        **
C               ****************************************
C
      IF(ICOM.EQ.'KAPL' .OR. ICOM.EQ.'MODI')THEN
        CALL DPKAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE TOTAL TIME ON TEST PLOT **
C               **  CASE (JUNE 2020)                  **
C               ****************************************
C
      IF(ICOM.EQ.'TOTA' .OR. ICOM.EQ.'TTT ')THEN
        CALL DPTTTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE DUANE        PLOT CASE  **
C               **  (MAY 1998)                        **
C               ****************************************
C
      IF(ICOM.EQ.'DUAN')THEN
        CALL DPDUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE EMPIRICAL CDF PLOT CASE **
C               **  (MAY 1998)                        **
C               ****************************************
C
      IF(ICOM.EQ.'EMPI' .OR. ICOM.EQ.'ECDF')THEN
        CALL DPECDF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE SEASONAL SUBSERIES PLOT CASE **
C               **  (FEBRUARY 1999)                        **
C               *********************************************
C
      IF(ICOM.EQ.'SEAS' .OR. IHARG(1).EQ.'SEAS')THEN
        CALL DPSESB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE SPREAD-LOCATION    PLOT CASE **
C               **  (AUGUST   1999)                        **
C               *********************************************
C
      IF(ICOM.EQ.'SPRE' .OR. IHARG(1).EQ.'SPRE')THEN
        CALL DPSLOC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE TUKEY MEAN-DIFFERENCE PLOT CASE **
C               ************************************************
C
      IF((ICOM.EQ.'TUKE'.AND.IHARG(1).NE.'LAMB') .OR.
     1   (ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'TUKE') .OR.
     1   (ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'TUKE'))THEN
        CALL DPTUMD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE SHIFT                 PLOT CASE **
C               ************************************************
C
      IF(ICOM.EQ.'SHIF' .OR.
     1  (ICOM.EQ.'HIGH' .AND. IHARG(1).EQ.'SHIF') .OR.
     1  (ICOM.EQ.'SUBS' .AND. IHARG(1).EQ.'SHIF'))THEN
        CALL DPSHPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES' .OR. IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE BLAND ALTMAN          PLOT CASE **
C               ************************************************
C
      IF((ICOM.EQ.'BLAN'.AND.IHARG(1).EQ.'ALTM') .OR.
     1   (ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'BLAN') .OR.
     1   (ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'BLAN'))THEN
        CALL DPBAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,ISEED,IBOOSS,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE INTERACTION           PLOT CASE **
C               ************************************************
C
      IF(ICOM.EQ.'INTE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'PLOT')GOTO9500
      GOTO9599
C
 9500 CONTINUE
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG2,IERROR)
      CALL DPINPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1MAXNPP,
     1ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C 
 9599 CONTINUE
C
C               ****************************************************
C               **  TREAT THE ... STATISTIC INTERACTION PLOT CASE **
C               ****************************************************
C
      IF(NUMARG.LT.2)GOTO9699
      DO9602I=1,NUMARG-1
        IF(IHARG(I).EQ.'INTE'.AND.IHARG(I+1).EQ.'PLOT')GOTO9600
 9602 CONTINUE
      IF(NUMARG.LT.3)GOTO9699
      DO9604I=1,NUMARG-2
        IF(IHARG(I).EQ.'INTE'.AND.IHARG(I+1).EQ.'STAT'.AND.
     1     IHARG(I+2).EQ.'PLOT')GOTO9600
 9604 CONTINUE
      GOTO9699
C
 9600 CONTINUE
      CALL DPISP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1MAXNXT,
     1ISEED,
     1ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GOTO9000
C
 9699 CONTINUE
C
C               *******************************************
C               **  TREAT THE KERNEL DENSITY PLOT  CASE  **
C               *******************************************
C
      IF((ICOM.EQ.'KERN' .OR. ICOM.EQ.'DENS') .OR.
     1   IHARG(1).EQ.'KERN' .OR. IHARG(2).EQ.'KERN' .OR.
     1   IHARG(3).EQ.'KERN')THEN
        CALL DPKDEN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IKDENP,PKDEWI,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE LORENZ CURVE         CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'LORE' .OR.
     1   IHARG(1).EQ.'LORE' .OR. IHARG(2).EQ.'LORE' .OR.
     1   IHARG(3).EQ.'LORE')THEN
        CALL DPLORE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE H CONSISTENCY PLOT   CASE  **
C               *******************************************
C
      IF(
     1  (ICOM.EQ.'H   ' .AND. IHARG(1).EQ.'CONS' .AND.
     1   IHARG(2).EQ.'PLOT') .OR.
     1  (ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'CONS' .AND.
     1   IHARG(2).EQ.'PLOT') .OR.
     1  (ICOM.EQ.'COCH' .AND. IHARG(1).EQ.'VARI' .AND.
     1   IHARG(2).EQ.'PLOT')
     1  )THEN
        CALL DPHKCP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE TWO FACTOR    PLOT   CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'FACT' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
        CALL DPTWFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE CONSENSUS MEAN PLOT  CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'CONS')THEN
        IF(NUMARG.GE.2.AND.
     1    IHARG(1).EQ.'MEAN'.AND.IHARG(2).EQ.'PLOT')THEN
          CALL DPCMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                ICAPSW,ICAPTY,
     1                IFORSW,ISEED,IBOOSS,
     1                ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *********************************************
C               **  TREAT THE PARTIAL REGRESSION PLOT CASE **
C               **  TREAT THE PARTIAL RESIDUAL   PLOT CASE **
C               **  TREAT THE PARTIAL LEVERAGE   PLOT CASE **
C               *********************************************
C
      IF(ICOM.EQ.'PART')THEN
        IF(NUMARG.GE.2.AND.
     1    (IHARG(1).EQ.'REGR'.AND.IHARG(2).EQ.'PLOT') .OR.
     1    (IHARG(1).EQ.'RESI'.AND.IHARG(2).EQ.'PLOT') .OR.
     1    (IHARG(1).EQ.'LEVE'.AND.IHARG(2).EQ.'PLOT'))THEN
          ICASPL='PREG'
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'ADDE')THEN
        ICASPL='PREG'
        IF(NUMARG.GE.2.AND.
     1    IHARG(1).EQ.'VARI'.AND.IHARG(2).EQ.'PLOT')THEN
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'COMP')THEN
        ICASPL='PREG'
        IF(NUMARG.GE.3.AND.
     1    IHARG(1).EQ.'PLUS'.AND.IHARG(2).EQ.'RESI'.AND.
     1    IHARG(3).EQ.'PLOT')THEN
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'CCPR')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
          ICASPL='CCPR'
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************************************
C               **  TREAT THE ... INFLUENCE CURVE CASE **
C               *****************************************
C
      IF(NUMARG.GE.2)THEN
        DO9710I=1,NUMARG-1
          IF(IHARG(I).EQ.'INFL' .AND. IHARG(I+1).EQ.'CURV')THEN
            CALL DPINCU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC1           TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1           MAXNXT,
     1           ISEED,
     1           ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES')GOTO9000
            GOTO9719
          ENDIF
 9710   CONTINUE
      ENDIF
 9719 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED      APRIL    2005
C               ************************************************
C               **  TREAT THE PEAKS OVER THRESHOLD PLOT  CASE **
C               **            POT                  PLOT       **
C               ************************************************
C
      IF(ICOM.EQ.'PEAK')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'OVER'.AND.
     1     IHARG(2).EQ.'THRE'.AND.IHARG(3).EQ.'PLOT')THEN
          CALL DPPOTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBOOSS,ISEED,
     1                ICAPSW,ICAPTY,IFORSW,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'POT ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
          CALL DPPOTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IBOOSS,ISEED,
     1                ICAPSW,ICAPTY,IFORSW,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *************************************************
C               **  TREAT THE REPAIR PLOT CASE                 **
C               **  (OCTOBER  2006)                            **
C               *************************************************
C
      IF(ICOM.EQ.'REPA')THEN
        CALL DPRPLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************************
C               **  TREAT THE MEAN REPAIR  FUNCTION PLOT CASE  **
C               **  (OCTOBER  2006)                            **
C               *************************************************
C
      IF(ICOM.EQ.'MEAN' .OR. ICOM.EQ.'AVER')THEN
        CALL DPMRFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE TRILINEAR PLOT CASE.  **
C               **************************************
C
      IF(ICOM.EQ.'TRIL' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPTRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE ROC         PLOT CASE.  **
C               **  TREAT THE PSUEDO ROC  PLOT CASE.  **
C               ****************************************
C
      IF(ICOM.EQ.'ROC ')THEN
        CALL DPROC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'PSUE' .AND. IHARG(1).EQ.'ROC ')THEN
        CALL DPROC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE ROSE      PLOT CASE.  **
C               **************************************
C
      IF(ICOM.EQ.'ROSE')THEN
        CALL DPROSE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE BIVARIATE NORMAL TOLERANCE **
C               **  REGION   PLOT CASE.                  **
C               *******************************************
C
      IF(ICOM.EQ.'BIVA' .OR. ICOM.EQ.'POIN')THEN
        CALL DPBNTR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE BINARY    PLOT CASE.  **
C               **************************************
C
      IF(ICOM.EQ.'BINA' .AND. IHARG(1).NE.'TABU')THEN
        CALL DPBIPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE ORD           PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'ORD ')THEN
        CALL DPORD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE POISSON       PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'POIS' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'GEOM' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'BINO' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'LOGA' .AND. IHARG(1).EQ.'SERI' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'NEGA' .AND. IHARG(1).EQ.'BINO' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             CLLIMI,CLWIDT,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE ASSOCIATION   PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'ASSO' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPASSO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE SIEVE         PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'SIEV' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPSIEV(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE LEVEL         PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'LEVE' .AND. IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        CALL DPLEPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'DISC' .AND. IHARG(1).EQ.'CONT' .AND.
     1  IHARG(2).EQ.'PLOT')THEN
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        CALL DPLEPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE IMAGE         PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'IMAG' .AND. IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        CALL DPIMAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************************
C               **  TREAT THE SPATIAL DISTRIBUTION  PLOT CASE  **
C               *************************************************
C
      IF(ICOM.EQ.'SPAT' .AND. IHARG(1).EQ.'DIST' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        CALL DPSDPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE FLUCUATION    PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'FLUC')THEN
        CALL DPFLUC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE STRIP         PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'STRI'.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'STRI'.AND.
     1       IHARG(2).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
C
C     FOLLOWING SECTION ADDED TO SUPPORT "BATCH MULTIPLE"
C     OPTION FOR STRIP PLOT--10/2009
C
      ELSEIF(ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'MULT'.AND.
     1       IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'MULT'.AND.IHARG(1).EQ.'BATC'.AND.
     1       IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE DETECTION LIMIT PLOT CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'DETE'.AND.IHARG(1).EQ.'LIMI'.AND.
     1   IHARG(2).EQ.'PLOT')THEN
        CALL DPDLPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'NORM'.AND.IHARG(1).EQ.'DETE'.AND.
     1       IHARG(2).EQ.'LIMI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPDLPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1              ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE TABULATION    PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'TABU' .OR.
     1   (ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'TABU'))THEN
        CALL DPTAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************************
C               **  TREAT THE ISO 13528 ZSCORE PLOT CASE  **
C               **            ISO 13528 JSCORE PLOT CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.
     1  (IHARG(2).EQ.'ZSCO' .OR. IHARG(2).EQ.'JSCO').AND.
     1   IHARG(3).EQ.'PLOT')THEN
        CALL DPZSCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE ISO 13528     PLOT CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
        CALL DPISOP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************************
C               **  TREAT THE ISO 13528 RLP PLOT  CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.
     1   IHARG(2).EQ.'RLP' .AND. IHARG(3).EQ.'PLOT')THEN
        CALL DPRLPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************************
C               **  TREAT THE TWO-WAY <ROW/COLUMN> PLOT  CASE  **
C               *************************************************
C
      IF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'WAY ')THEN
        CALL DPTWPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              ICAPSW,ICAPTY,IFORSW,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  END OF SEARCH FOR GRAPHICS COMMANDS  **
C               *******************************************
      IFOUND='NO'
      IERROR='NO'
      GOTO9001
C
C               *******************************************
C               **  STEP 90A--                           **
C               **  DO THE FOLLOWING FOR ALL PLOTS:      **
C               **  1) SAVE SOME INTERNAL PARAMETERS     **
C               **  2) IMPLEMENT SUB-REGIONS             **
C               *******************************************
C
 9000 CONTINUE
      IF(IFOUND.EQ.'NO')GOTO9001
      IF(IERROR.EQ.'YES')GOTO9001
      IF(NPLOTP.LT.1)GOTO9001
      IF(ICASPL(1:2).EQ.'3D')GOTO9001
C
C  FIND PLOT MIN AND MAX AND CORRESPONDING INDEX AND SAVE AS
C  INTERNAL PARAMETERS.
C
      AYMIN=CPUMAX
      AYMAX=CPUMIN
      AXMIN=CPUMAX
      AXMAX=CPUMIN
      IYMIN=0
      IYMAX=0
      IXMIN=0
      IXMAX=0
      DO10001I=1,NPLOTP
        IF(Y(I).LT.AYMIN)THEN
          AYMIN=Y(I)
          IYMIN=I
        ENDIF
        IF(Y(I).GT.AYMAX)THEN
          AYMAX=Y(I)
          IYMAX=I
        ENDIF
        IF(X(I).LT.AXMIN)THEN
          AXMIN=X(I)
          IXMIN=I
        ENDIF
        IF(X(I).GT.AXMAX)THEN
          AXMAX=X(I)
          IXMAX=I
        ENDIF
10001 CONTINUE
      ISUBN0='INGR'
      IH='PLOT'
      IH2='YMAX'
      VALUE0=AYMAX
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='YMAX'
      IH2='INDE'
      VALUE0=REAL(IYMAX)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='YMIN'
      VALUE0=AYMIN
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='YMIN'
      IH2='INDE'
      VALUE0=REAL(IYMIN)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='XMAX'
      VALUE0=AXMAX
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='XMAX'
      IH2='INDE'
      VALUE0=REAL(IXMAX)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='XMIN'
      VALUE0=AXMIN
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='XMIN'
      IH2='INDE'
      VALUE0=REAL(IXMIN)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
C  FIND CORRELATION OF PLOT POINTS.  FIND 2 CORRELATIIONS:
C  ONE WITH ALL POINTS, ONE WITH TAGPLO=1.
C
      IWRITE='OFF'
      CALL CORR(Y,X,NPLOTP,IWRITE,ACORR,IBUGG3,IERROR)
      IH='PLOT'
      IH2='CORR'
      VALUE0=ACORR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      J=0
      DO10101I=1,NPLOTP
        IF(D(I).EQ.1.0)THEN
          J=J+1
          TEMP(J)=Y(I)
          TEMP2(J)=X(I)
        ENDIF
10101 CONTINUE
      ACORR=0.0
      IF(J.GE.1)CALL CORR(TEMP,TEMP2,J,IWRITE,ACORR,IBUGG3,IERROR)
      IH='PLOT'
      IH2='COR1'
      VALUE0=ACORR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
C  IMPLEMENT SUB-REGIONS
C
      NUMSBR=0
      DO10200I=MAXSUB,1,-1
        IF(ISUBSW(I).EQ.'ON')THEN
          NUMSBR=NUMSBR+1
          IF(NPLOTP+5.GT.MAXPOP)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,10205)
10205       FORMAT('***** FROM MAINGR--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,10208)I
10208       FORMAT('      UNABLE TO IMPLEMENT SUB-REGION ',I5)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,10212)MAXPOP
10212       FORMAT('      THE NUMBER OF PLOT POINTS WOULD EXCEED ',
     1             'MAXIMUM OF ',I8,'.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,10214)NPLOTP
10214       FORMAT('      THE CURRENT NUMBER OF PLOT POINTS = ',I8)
            CALL DPWRST('XXX','BUG ')
            GOTO10299
          ELSE
            DO10220II=NPLOTP,1,-1
              X(II+5)=X(II)
              Y(II+5)=Y(II)
              X3D(II+5)=X3D(II)
              DSIZE(II+5)=DSIZE(II)
              DSYMB(II+5)=DSYMB(II)
              DCOLOR(II+5)=DCOLOR(II)
              DFILL(II+5)=DFILL(II)
              D(II+5)=D(II)+1.0
10220       CONTINUE
            NPLOTP=NPLOTP+5
C
            TEMPZZ(1)=AXMIN
            TEMPZZ(2)=AXMAX
            NZTEMP=2
            ICASZZ='X'
            CALL DPFRLI(ICASZZ,IFRALI,TEMPZZ,NZTEMP,
     1                  GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1                  IX1TSC,IX1TSW,IY1TSC,IY1TSW,
     1                  IX1JSW,IY1JSW,
     1                  NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW,
     1                  PX1COO,X1COOR,NX1COO,
     1                  PY1COO,Y1COOR,NY1COO,
     1                  PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,
     1                  PY1CMN,Y1COMN,NY1CMN,PY1TOB,PY1TOT,
     1                  ITICUN,PXMIN,PXMAX,PYMIN,PYMAX,
     1                  AVALXL,AVALXU,
     1                  IBUGG3,ISUBRO,IERROR)
C
            IF(ASUBXL(I).EQ.CPUMIN)THEN
CCCCC         X(1)=AXMIN
              X(1)=AVALXL
            ELSE
              X(1)=ASUBXL(I)
            ENDIF
C
            IF(ASUBXU(I).EQ.CPUMAX)THEN
CCCCC         X(2)=AXMAX
              X(2)=AVALXU
            ELSE
              X(2)=ASUBXU(I)
            ENDIF
C
            IF(ASUBXU(I).EQ.CPUMAX)THEN
CCCCC         X(3)=AXMAX
              X(3)=AVALXU
            ELSE
              X(3)=ASUBXU(I)
            ENDIF
C
            IF(ASUBXL(I).EQ.CPUMIN)THEN
CCCCC         X(4)=AXMIN
              X(4)=AVALXL
            ELSE
              X(4)=ASUBXL(I)
            ENDIF
C
            TEMPZZ(1)=AYMIN
            TEMPZZ(2)=AYMAX
            NZTEMP=2
            ICASZZ='Y'
            CALL DPFRLI(ICASZZ,IFRALI,TEMPZZ,NZTEMP,
     1                  GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1                  IX1TSC,IX1TSW,IY1TSC,IY1TSW,
     1                  IX1JSW,IY1JSW,
     1                  NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW,
     1                  PX1COO,X1COOR,NX1COO,
     1                  PY1COO,Y1COOR,NY1COO,
     1                  PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,
     1                  PY1CMN,Y1COMN,NY1CMN,PY1TOB,PY1TOT,
     1                  ITICUN,PXMIN,PXMAX,PYMIN,PYMAX,
     1                  AVALYL,AVALYU,
     1                  IBUGG3,ISUBRO,IERROR)
C
            IF(ASUBYL(I).EQ.CPUMIN)THEN
CCCCC         Y(1)=AYMIN
              Y(1)=AVALYL
            ELSE
              Y(1)=ASUBYL(I)
            ENDIF
C
            IF(ASUBYL(I).EQ.CPUMIN)THEN
CCCCC         Y(2)=AYMIN
              Y(2)=AVALYL
            ELSE
              Y(2)=ASUBYL(I)
            ENDIF
C
            IF(ASUBYU(I).EQ.CPUMAX)THEN
CCCCC         Y(3)=AYMAX
              Y(3)=AVALYU
            ELSE
              Y(3)=ASUBYU(I)
            ENDIF
C
            IF(ASUBYU(I).EQ.CPUMAX)THEN
CCCCC         Y(4)=AYMAX
              Y(4)=AVALYU
            ELSE
              Y(4)=ASUBYU(I)
            ENDIF
C
            X(5)=X(1)
            Y(5)=Y(1)
            DO10225JJ=1,5
              X3D(JJ)=1.0
              DSIZE(JJ)=1.0
              DSYMB(JJ)=1.0
              DCOLOR(JJ)=1.0
              DFILL(JJ)=1.0
              D(JJ)=1.0
10225       CONTINUE
          ENDIF
        ENDIF
10200 CONTINUE
      NACC=0
      NREJ=0
      NTOT=0
      IF(NUMSBR.GT.0)THEN
        NSTRT=NUMSBR*4+1
        IF(NSTRT.GT.NPLOTP)GOTO10299
        NTOT=0
        NACC=0
        NREJ=0
        XLOW=X(1)
        XHIGH=X(2)
        YLOW=Y(1)
        YHIGH=Y(4)
        DO10260I=NSTRT,NPLOTP
          NTOT=NTOT+1
          XPNT=X(I)
          YPNT=Y(I)
          IF(
     1      (XPNT.LT.XLOW.OR. XPNT.GT.XHIGH) .OR.
     1      (YPNT.LT.YLOW.OR.YPNT.GT.YHIGH)
     1       )THEN
            NREJ=NREJ+1
          ELSE
            NACC=NACC+1
          ENDIF
10260   CONTINUE
      ENDIF
10299 CONTINUE
      IH='NACC'
      IH2='EPT '
      VALUE0=REAL(NACC)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='NREJ'
      IH2='ECT '
      VALUE0=REAL(NREJ)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
      IH='NTOT'
      IH2='AL  '
      VALUE0=REAL(NTOT)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9001 CONTINUE
C
C     APRIL 2007.  CHECK FOR FATAL ERROR
C
      IERRST=IERROR
C
      IF(IERROR.EQ.'YES')THEN
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1              ISUBN1,ISUBN2,ICASPL,
     1              IBUGG2,ISUBRO,IERROR)
      ENDIF
C
      IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAINGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAININ(IBUGIN,ICOMHO,ICOMH2,IRSCNT)
C
C     PURPOSE--THIS IS SUBROUTINE MAININ.
C              (THE   IN    AT THE END OF    MAINPC   STANDS FOR   INITIAL
C              THIS SUBROUTINE INITIALIZES ALL NEEDED CONSTANTS
C              FOR THE   AREAS--MC = MACHINE CONSTANTS
C                             --DB = DEBUGGING
C                             --HK = HOUSEKEEPING
C                             --PC = PLOT CONTROL
C                             --OD = OUTPUT DEVICES
C                             --SU = SUPPORT
C                             --GR = GRAPHICS
C                             --AN = ANALYSIS
C                             --DA = DATA
C                             --DG = DIAGRAMMATIC GRAPHICS
C                             --H2 = HOUSEKEEPING (PART 2)
C                             --3D = 3-DIMENSIONAL
C     THIS ROUTINE IS TYPICALLY CALLED ONLY ONCE PER DATAPLOT RUN
C     (IMMEDIATELY AFTER SIGN-ON).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --MAY       1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1986.
C     UPDATED         --SEPTEMBER 1988. GENERAL 3-D
C     UPDATED         --DECEMBER  1988. RESET2
C     UPDATED         --MAY       1989. INITIALIZE DES. OF EXP. COMMON
C     UPDATED         --AUGUST    1990. INITIALIZE WINDOW SYSTEM
C     UPDATED         --DECEMBER  2015. ADD "IRSCNT".  IF IRSCNT > 0,
C                                       DO NOT RESET DEVICE 1 UNDER
C                                       WINDOWS (THIS CAUSES A CRASH
C                                       WITH THE QWIN DEVICE).
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGIN
      CHARACTER*4 ICOMHO
      CHARACTER*4 ICOMH2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF MAININ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,ICOMHO,ICOMH2,IRSCNT
   53   FORMAT('IBUGPC,ICOMHO,ICONH2,IRSCNT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************************
C               **  STEP 1--
C               **  INITIALIZE VARIABLES AND PARAMETERS.
C               **  11 INITIALIZATION SUBROUTINES ARE CALLED--
C               **        INITMC--INITIALIZE MACHINE CONSTANTS
C               **        INITFO--INITIALIZE FILE OPERATIONS
C               **        INITHK--INITIALIZE HOUSEKEEPING           VARIABLES AN
C               **        INITDA--INITIALIZE DATA                   VARIABLES.
C               **        INITPC--INITIALIZE PLOT CONTROL  COMMANDS VARIABLES AN
C               **        INITDG--INITIALIZE DIAGRAMMATIC GRAPHICS  COMMANDS VAR
C               **        INITOD--INITIALIZE OUTPUT DEVICE COMMANDS VARIABLES AN
C               **        INITSU--INITIALIZE SUPPORT       COMMANDS VARIABLES AN
C               **        INITH2--INITIALIZE HOUSEKEEPING (PART 2)  VARIABLES AN
C               **        INITDB--INITIALIZE DEBUGGING              VARIABLES.
C               **        INIT3D--INITIALIZE 3-DIMENSIONAL          VARIABLES.
C               ****************************************************************
C
      IBUGIN='OFF'
      IFLAG=0
      IF(ICOMHO.EQ.'RESE'.AND.ICOMH2.EQ.'T2  ')IFLAG=1
      IF(IFLAG.EQ.0)THEN
        CALL INITMC(IBUGIN)
        CALL INITFO(IBUGIN)
      ENDIF
C
      CALL INITHK(IBUGIN)
      CALL INITDA(IBUGIN)
      CALL INITPC(IBUGIN)
CCCCC CALL INITDG(IBUGIN)
C     DIAGRAMMATIC GRAPHICS INITIALIZATION  IS NOW DONE (NOV 1983)
C     IN INITPC
C
      IF(IFLAG.EQ.0 .AND. IRSCNT.EQ.0)THEN
        CALL INITOD(IBUGIN)
      ENDIF
C
      CALL INITSU(IBUGIN)
CCCCC THE FOLLOWING DES. OF EXP. LINE WAS ADDED MAY 1989
      CALL INITDE(IBUGIN)
      CALL INIT3D(IBUGIN)
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1990
CCCCC CALL INITWI(IBUGIN)
C
      CALL INITH2(IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1IVALUE,VALUE,NUMNAM,MAXN,MAXCOL,IBUGIN)
      CALL INITDB
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF MAININ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGIN
 9013   FORMAT('IBUGIN = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAINOD(IBUGOD,IBUGO2,ISUBRO,
     1                  ICAPSW,
     1                  IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAINOD.
C              (THE   OD    AT THE END OF    MAINOD   STANDS FOR   OUTPUT DEVICE
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES OUTPUT DEVICE COMMANDS.
C              THE OUTPUT DEVICE COMMANDS SEARCHED FOR BY MAINOD ARE AS FOLLOWS-
C
C                   1) DEVICE ... POWER                  ON/OFF
C                   2) DEVICE ... MANUFACTURER           A MANUFACTURER AND MODE
C                   3) DEVICE ... CONTINUOUS             ON/OFF
C                   4) DEVICE ... COLOR                  ON/OFF
C                   5) DEVICE ... PICTURE POINTS         2 NUMBERS
C                   6) DEVICE ... UNIT NUMBER            A NUMBER
C
C                   7) TERMINAL   POWER                  ON/OFF
C                   8) TERMINAL   MANUFACTURER           A MANUFACTURER AND MODE
C                   8) TERMINAL   CONTINUOUS             ON/OFF
C                   9) TERMINAL   COLOR                  ON/OFF
C                  10) TERMINAL   PICTURE POINTS         2 NUMBERS
C                   6) TERMINAL UNIT NUMBER            A NUMBER
C
C                  11) POWER                             ON/OFF
C                  12) MANUFACTURER                      A MANUFACTURER AND MODE
C                  13) CONTINUOUS                        ON/OFF
C                  14) COLOR                             ON/OFF
C                  15) PICTURE POINTS                    2 NUMBERS
C                   16) UNIT NUMBER            A NUMBER
C
C                  16) DISCRETE                          ON/OFF
C                  17) DISCRETE NARROW-WIDTH             ON/OFF
C                  18) DISCRETE WIDE-CARRIAGE            ON/OFF
C                  19) BATCH                             ON/OFF
C
C                  20) FILE                              ON/OFF
C                  20) CALCOMP                           ON/OFF
C                  21) VERSATEC                          ON/OFF
C                  22) ZETA                              ON/OFF
C
C                  22) METAFILE                              ON/OFF
C
C                  23) HARDCOPY                          ON/OFF AND OPTIONALLY A
C                  24) PENPLOTTER                        ON/OFF AND OPTIONALLY A
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  1989.  2 OFFSET ARGUMENTS IN CALLS TO DPDEMN
C     UPDATED         --FEBRUARY  1989.  ADD CHECKS FOR NEW DEVICES (ALAN)--
C                                           GENERAL CGM (OR CGM)
C                                           QUIC (OR QMS)
C                                           POSTSCRIPT
C                                           PCL (OR LASERJET)
C                                           DICOMED
C     UPDATED         --MARCH     1990.   ADD CHECK FOR X11 DEVICE
C     UPDATED         --MAY       1990.   CHECK FOR [HPGL/ZETA/CALC] PEN MAP,
C                                         DISTINGUISH BETWEEN ON/OFF AND
C                                         OPEN/CLOSE
C     UPDATED         --JANUARY   1991.   ADD REGIS TO PEN MAP COMMAND
C     UPDATED         --MAY       1991.   ADD TURBO-C/VGA (JJF)
C     UPDATED         --JUNE      1991.   ADD X11 TO PEN MAP COMMAND
C     UPDATED         --OCTOBER   1991.   ADD "POSTSCRIPT SHOW FONT" COMMAND
C     UPDATED         --APRIL     1992.   PRINT PLOT, P, PP
C     UPDATED         --MAY       1992.   POSTSCRIPT BLANK PAGE SWITCH
C     UPDATED         --JUNE      1992.   ARGUMENT LIST TO DPDEMN
C     UPDATED         --AUGUST    1992.   ADD "SHOW COLORS" COMMAND.
C     UPDATED         --APRIL     1993.   CHECK FOR CONFLICT WITH
C                                         P CONTROL CHART (ALAN)
C     UPDATED         --OCTOBER   1993.   BUG FOR DISCRETE ON
C     UPDATED         --DECEMBER   1993.  COMMENT OUT   GENERAL
C     UPDATED         --MAY        1994.  CHECK CONFLICT BETWEEN REGIS
C                                         AND REGION
C     UPDATED         --SEPTEMBER  1994.  CHECK CONFLICT BETWEEN DISCR
C                                         AND DISCR UNIFORM PROB PLOT
C     UPDATED         --APRIL      1995.  CHECK CONFLICT BETWEEN POWER
C                                         AND POWER NORMAL AND POWER
C                                         LOGNORMAL (PROB PLOT, PPCC 
C                                         PLOT)
C     UPDATED         --OCTOBER    1995.  CHECK CONFLICT BETWEEN GENERAL
C                                         AND GENERALIZED EXTREME VALUE
C                                         AND GENERALIZED HALF LOGISTIC
C                                         (PROB AND PPCC PLOTS)
C     UPDATED         --DECEMBER   1995.  CHECK CONFLICT BETWEEN GENERAL
C                                         AND GENERALIZED LOGISTIC
C     UPDATED         --FEBRUARY   1996.  CHECK CONFLICT BETWEEN GENERAL
C                                         AND GENERALIZED EXPONENTIAL
C     UPDATED         --JULY       1996.  DEVICE ... FONT COMMAND
C     UPDATED         --OCTOBER    1996.  ADD CHECKS FOR NEW DEVICES (ALAN)--
C                                           MICROSOFT QUICKWIN  
C                                           PBM (PORTABLE BIT MAP)
C     UPDATED         --JUNE       1998.  NAME CONFLICT WITH POWER MLE
C     UPDATED         --JUNE       2000.  ADD CHECKS FOR NEW DEVICES (ALAN)--
C                                           OPEN-GL
C                                           GD JPEG
C                                           GD PNG
C                                           GD WBMP
C                                           WINDOWS BITMAP
C     UPDATED         --MARCH      2002.  ADD CHECKS FOR NEW DEVICES (ALAN)--
C                                           SVG
C     UPDATED         --SEPTEMBER  2002.  ICAPSW FOR DPDEMN, DPDEPW
C     UPDATED         --SEPTEMBER  2007.  IERRST
C     UPDATED         --SEPTEMBER  2011.  VIEW PLOT COMMAND
C     UPDATED         --OCTOBER    2016.  UPDATES TO VIEW PLOT COMMAND
C     UPDATED         --DECEMBER   2018.  DEVICE SCALE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP
      CHARACTER*4 ICAPSW
C
      CHARACTER*4 IBUGOD
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IFTYPE
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE3
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASEZ
C
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXFNC) :: ICMDTI
      CHARACTER (LEN=MAXFNC) :: IFILEZ
      CHARACTER (LEN=MAXFNC) :: ITEMP
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOF2.INC'
CCCCC THE FOLLOWING LINE WAS ADDED   MAY 1992 (JJF)
      INCLUDE 'DPCODV.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      I=1
      IOP='-999'
      ISUBN1='MAIN'
      ISUBN2='OD  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGOD.EQ.'ON'.OR.ISUBRO.EQ.'INOD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAINOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGOD,IBUGO2,ISUBRO
   53   FORMAT('IBUGOD,IBUGO2,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IFOUND,IERROR,ICOM,ICOM2,IPSTBP,NUMARG
   60   FORMAT('IFOUND,IERROR,ICOM,ICOM2,IPSTBP,NUMARG = ',
     1         5(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  TREAT THE GENERAL (= DEVICE-INDEPENDENT) CASE  **
C               *****************************************************
C
C     CHECK FOR NAME CONFLICTS WITH "GENERAL"
C
      IF(NUMARG.GE.2)THEN
        IF(IHARG(2).EQ.'PROB')GOTO9000
        IF(IHARG(2).EQ.'PPCC')GOTO9000
      ELSEIF(NUMARG.GE.1)THEN
        IF(IHARG(1).EQ.'JACC')GOTO9000
        IF(IHARG(1).EQ.'PARE')GOTO9000
        IF(IHARG(1).EQ.'LOGI')GOTO9000
        IF(IHARG(1).EQ.'PPCC')GOTO9000
        IF(IHARG(1).EQ.'PROB')GOTO9000
        IF(IHARG(1).EQ.'GAMM')GOTO9000
        IF(IHARG(1).EQ.'EXTR')GOTO9000
        IF(IHARG(1).EQ.'HALF')GOTO9000
        IF(IHARG(1).EQ.'LOGI')GOTO9000
        IF(IHARG(1).EQ.'EXPO')GOTO9000
        IF(IHARG(1).EQ.'LAMB')GOTO9000
        IF(IHARG(1).EQ.'TRAP')GOTO9000
        IF(IHARG(1).EQ.'MCLE')GOTO9000
        IF(IHARG(1).EQ.'INVE'.AND.IHARG(2).EQ.'GAUS')GOTO9000
        IF(IHARG(1).EQ.'ASYM'.AND.IHARG(2).EQ.'LAPL')GOTO9000
        IF(IHARG(1).EQ.'ASYM'.AND.IHARG(2).EQ.'DOUB')GOTO9000
        IF(IHARG(1).EQ.'TUKE'.AND.IHARG(2).EQ.'LAMB')GOTO9000
        IF(IHARG(1).EQ.'LOGA'.AND.IHARG(2).EQ.'SERI')GOTO9000
        IF(IHARG(1).EQ.'NEGA'.AND.IHARG(2).EQ.'BINO')GOTO9000
        IF(IHARG(1).EQ.'LOST'.AND.IHARG(2).EQ.'GAME')GOTO9000
        IF(IHARG(1).EQ.'TOPP'.AND.IHARG(2).EQ.'LEON')GOTO9000
        IF(IHARG(1).EQ.'TOPP'.AND.IHARG(2).EQ.'AND '.AND.
     1       IHARG(3).EQ.'LEON')GOTO9000
      ENDIF
C
C     DEVICE PEN MAP CASE
C
      IF((ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'MAP')  .OR.
     1   (ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'PEN')  .OR.
     1   (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'PEN')  .OR.
     1   (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'MAP')  .OR.
     1   (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PEN')  .OR.
     1   (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MAP')  .OR.
     1   (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'PEN')  .OR.
     1   (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'MAP')  .OR.
     1   (ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'COLO') .OR.
     1   (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'COLO') .OR.
     1   (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'COLO') .OR.
     1   (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'COLO') .OR.
     1   (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.
     1    IHARG(1).EQ.'MAP')                     .OR.
     1   (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.
     1    IHARG(1).EQ.'PEN')                     .OR.
     1   (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.
     1    IHARG(1).EQ.'COLO')                    .OR.
     1   (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'MAP')  .OR.
     1   (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'PEN')  .OR.
     1   (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'COLO'))THEN
        CALL DPDEPM(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IBUGO2,ISUBRO,IFOUND,IERROR)
        GOTO9000
C
C     FOLLOWING LINES ADDED OCTOBER, 1991.  ADD "POSTSCRIPT SHOW FONTS" COMMAND
C
      ELSEIF((ICOM.EQ.'POST'.AND.IHARG(1).EQ.'SHOW') .OR.
     1       (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'LIST') .OR.
     1       (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'PRIN') .OR.
     1       (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'FONT') .OR.
     1       (ICOM.EQ.'SHOW' .AND. IHARG(1).EQ.'FONT'))THEN
        CALL DPDEFN(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IBUGO2,ISUBRO,IFOUND,IERROR)
        GOTO9000
C
C     SHOW COLORS CASE
C
      ELSEIF(ICOM.EQ.'SHOW' .AND. IHARG(1).EQ.'COLO')THEN
        CALL DPDEPM(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IBUGO2,ISUBRO,IFOUND,IERROR)
      GOTO9000
C
C     GENERAL DEVICE (METAFILE)
C
      ELSEIF((ICOM.EQ.'GENE' .AND. NUMARG.LT.1) .OR.
     1        ICOM.EQ.'CGM ' .OR.
     1       (ICOM.EQ.'DEVI'.AND.NUMARG.GE.1.AND.
     1        IHARG(1).EQ.'GENE') .OR.
     1       (ICOM.EQ.'DEVI'.AND.NUMARG.GE.1.AND.
     1        IHARG(1).EQ.'INDE'))THEN
        IOP='ON'
        IF(NUMARG.GE.1.AND.IHARG(NUMARG).EQ.'OFF')IOP='OFF'
        ICOM='DEVI'
        ICOM2='CE  '
C
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
C
        IF(IOP.EQ.'ON')THEN
          IHARG(2)='MANU'
          IHARG2(2)='FACT'
          IARGT(2)='WORD'
          IHARG(3)='GENE'
          IHARG2(3)='RAL '
          IARGT(3)='WORD'
          NUMARG=3
          IF(IHARG(4).EQ.'CODE')NUMARG=4
          IF(IHARG(4).EQ.'CGM')NUMARG=4
          CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                IPL1NU,IPL1NA,IPL2NU,IPL2NA,
     1                IPL1CS,IPL2CS,
     1                IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1                IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1                NUMDEV,MAXDEV,
     1                IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1                IDNVOF,IDNHOF,
     1                ICAPSW,ICAPNU,
     1                IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSE
          IHARG(2)='POWE'
          IHARG2(2)='R   '
          IARGT(2)='WORD'
          IHARG(3)='OFF '
          IHARG2(3)='    '
          IARGT(3)='WORD'
          NUMARG=3
          CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                IPL1NU,IPL1NA,IPL2NU,IPL2NA,
     1                IDEFPO,
     1                NUMDEV,MAXDEV,
     1                IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                IDNVOF,IDNHOF,
     1                ICAPSW,ICAPNU,
     1                IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
        GOTO1099
C
      ELSE
        GOTO1099
      ENDIF
C
 1099 CONTINUE
C
CCCCC THE FOLLOWING SECTION  WAS ADDED   APRIL 1992  (JJF)
C               ****************************************
C               **  TREAT THE P CASE                  **
C               **  TREAT THE PP CASE                 **
C               **  TREAT THE PRINT PLOT CASE         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGOD.EQ.'ON'.OR.ISUBRO.EQ.'INOD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC APRIL 1993    CHECK FOR CONFLICT WITH P CHART
CCCCC APRIL 1993    AND P CONTROL CHART (ALAN)
      IF(ICOM.EQ.'P'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GOTO9000
      IF(ICOM.EQ.'P'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')GOTO9000
C
      IF(ICOM.EQ.'P' .OR. ICOM.EQ.'PP' .OR.
     1  (NUMARG.GE.1 .AND. ICOM.EQ.'PRIN' .AND.
     1   IHARG(1).EQ.'PLOT' .AND. IHARG2(1).EQ.'    '))THEN
C
        IFOUND='YES'
        IF(IPL2CS.NE.'CLOS')THEN
           CALL DPDEV(3,'CLOS','POST',ICAPSW,IBUGOD,ISUBRO,IERROR)
           IF(IERROR.EQ.'YES')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7011)
 7011         FORMAT('***** ERROR IN MAINOD')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7012)
 7012         FORMAT('      IN ATTEMPTING TO CLOSE DEVICE 3')
              CALL DPWRST('XXX','BUG ')
              GOTO9000
           ENDIF
        ENDIF
        IFTYPE='POST'
        CALL PRINFI(IPL2NA,IFTYPE,IBUGO2,ISUBRO,IERROR)
        GOTO9000
C
      ENDIF
C
CCCCC THE FOLLOWING SECTION  WAS ADDED   SEPTEMBER 2011
C               ****************************************
C               **  TREAT THE PSVIEW CASE             **
C               **  (VIEWS DPPL2F.DAT FILE)           **
C               ****************************************
C
      IF(ICOM.EQ.'PSVI' .OR.
     1  (NUMARG.EQ.0 .AND. ICOM.EQ.'SHOW'))THEN
        IFOUND='YES'
C
C       VIEW DEVICE 3 OUTPUT
C
        IF(NUMARG.LE.0)THEN
          IF(IPL2CS.NE.'CLOS')THEN
             CALL DPDEV(3,'CLOS','POST',ICAPSW,IBUGOD,ISUBRO,IERROR)
             IF(IERROR.EQ.'YES')THEN
               WRITE(ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,7011)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,7012)
               CALL DPWRST('XXX','BUG ')
               GOTO9000
             ENDIF
          ENDIF
          ICASE3='IPL2'
          CALL VIEWFI(IPL2NA,ICASE3,IBUGO2,ISUBRO,IERROR)
          GOTO9000
        ELSEIF((IHARG(1).LE.'DEVI' .AND. IHARG(2).EQ.'2') .OR.
     1         (IHARG(1).EQ.'DPPL' .AND. IHARG2(1)(1:2).EQ.'1F') .OR.
     1         (IHARG(1).EQ.'IPL1' .AND. IHARG2(1).EQ.'NA  '))THEN
C
C       VIEW DEVICE 2 OUTPUT.  SET PSVIEW CLOSE FILE COMMAND SPECIFIES
C       WHETHER USER WANTS TO CLOSE FILE OR NOT.
C
          IF(IPSVCL.EQ.'ON')THEN
            IF(IPL1CS.NE.'CLOS')THEN
              CALL DPDEV(2,'CLOS','POST',ICAPSW,IBUGOD,ISUBRO,IERROR)
              IF(IERROR.EQ.'YES')THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,7011)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,7014)
 7014           FORMAT('      IN ATTEMPTING TO CLOSE DEVICE 2')
                CALL DPWRST('XXX','BUG ')
                GOTO9000
              ELSE
                IF(IFEEDB.EQ.'ON')THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,7016)
 7016             FORMAT('      DEVICE 2 OUTPUT FILE HAS BEEN CLOSED.')
                  CALL DPWRST('XXX','BUG ')
                ENDIF
              ENDIF
            ENDIF
          ELSE
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7018)
 7018         FORMAT('      DEVICE 2 OUTPUT FILE HAS NOT BEEN ',
     1               'CLOSED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7019)
 7019         FORMAT('      THE LAST PLOT MAY NOT BE COMPLETE.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
          ICASE3='IPL1'
          CALL VIEWFI(IPL1NA,ICASE3,IBUGO2,ISUBRO,IERROR)
          GOTO9000
        ELSEIF(NUMARG.GE.1)THEN
C
C         ARBITRARY FILE NAME
C
          IWORD=2
          MAXTMP=80
          ICASEZ='NULL'
          ICMDTI='THE POSTSCRIPT FILE NAME FOR THE PSVIEW COMMAND = '
          CALL DPEXFN(IANS,IANSLC,ICANS,MAXTMP,IWIDTH,NUMARG,
     1                ISTRIN,IWORD,ICMDTI,ITEMP,
     1                ICASEZ,IFILEZ,NCFILE,
     1                IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(NCFILE.LE.80)THEN
            ICASE3='FILE'
            CALL VIEWFI(ISTRIN,ICASE3,IBUGO2,ISUBRO,IERROR)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7013)
 7013       FORMAT('      THE SPECIFIED FILE NAME HAS MORE THAN 80 ',
     1             'CHARACTERS.')
            IERROR='YES'
          ENDIF
          GOTO9000
        ENDIF
      ENDIF
C
C               ***********************************
C               **  PRE-TREAT THE TERMINAL CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'TERM'.AND.IHARG(1).EQ.'CHAR')GOTO9000
      IF(ICOM.EQ.'TERM')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
      ENDIF
C
C               ************************************************
C               **  TREAT THE DEVICE ... POWER CASE           **
C               **  TREAT THE DEVICE ... CONTINUOUS CASE      **
C               **  TREAT THE DEVICE ... COLOR      CASE      **
C               **  TREAT THE DEVICE ... PICTURE POINTS CASE  **
C               **  TREAT THE DEVICE ... UNIT        CASE     **
C               **  TREAT THE DEVICE ... FONT       CASE      **
C               **  TREAT THE DEVICE ... SCALE      CASE      **
C               **  TREAT THE DEVICE ...  (MANUFACTURER) CASE **
C               ************************************************
C
      IF(ICOM.EQ.'DEVI' .OR. ICOM.EQ.'TERM')THEN
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IPL1NU,IPL1NA,
     1              IPL2NU,IPL2NA,
     1              IDEFPO,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IDNVOF,IDNHOF,
     1              ICAPSW,ICAPNU,
     1              IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDECN(IHARG,IARGT,IARG,NUMARG,
     1              IDEFCN,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDECL(IHARG,IARGT,IARG,NUMARG,
     1              IDEFDC,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDEPP(IHARG,IARGT,IARG,NUMARG,
     1              IDEFVP,IDEFHP,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDEUN(IHARG,IARGT,IARG,NUMARG,
     1              IDEFUN,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDEFT(IHARG,IARGT,IARG,NUMARG,
     1              IDEFFN,NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,
     1              IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDESC(IHARG,IARGT,IARG,ARG,NUMARG,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,
     1              IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1              PDSCAL,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IPL1NU,IPL1NA,
     1              IPL2NU,IPL2NA,
     1              IPL1CS,IPL2CS,
     1              IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1              IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1              IDNVOF,IDNHOF,
     1              ICAPSW,ICAPNU,
     1              IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS INSERTED BY ALAN.  FEBRUARY 1989
CCCCC MAY, 1990. DISTINGUISH BETWEEN ON/OFF AND OPEN/CLOSE
C               *****************************************************
C               **  TREAT THE DEVICE ... ON/OFF (OR OPEN/CLOSE) CASE*
C               *****************************************************
C
      IF(NUMARG.GE.1)THEN
        IF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.
     1     IHARG(NUMARG).EQ.'OFF')THEN
          IOP='OFF'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.
     1     IHARG(NUMARG).EQ.'CLOS')THEN
          IOP='CLOS'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.
     1     IHARG(NUMARG).EQ.'ON')THEN
          IOP='ON'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.
     1     IHARG(NUMARG).EQ.'OPEN')THEN
          IOP='OPEN'
        ELSE
          GOTO1799
        ENDIF
C
        IF(NUMARG.LE.1)THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGO2,IERROR)
          IHARG(1)='1   '
          IHARG2(1)='    '
          IARGT(1)='NUMB'
          IARG(1)=1
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGO2,IERROR)
          IHARG(1)=IHARG(2)
          IHARG2(1)=IHARG2(2)
          IARGT(1)=IARGT(2)
          IARG(1)=IARG(2)
        ENDIF
C
        IHARG(2)='POWE'
        IHARG2(2)='ER  '
        IARGT(2)='WORD'
        IHARG(3)=IOP
        IHARG2(3)='    '
        IARGT(3)='WORD'
        NUMARG=3
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IPL1NU,IPL1NA,
     1              IPL2NU,IPL2NA,
     1              IDEFPO,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IDNVOF,IDNHOF,
     1              ICAPSW,ICAPNU,
     1              IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
 1799 CONTINUE
C
C               ****************************
C               **  TREAT THE POWER CASE  **
C               ****************************
C
CCCCC MAY 1995.  CHECK NAME CONFLICTS
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'NORM')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LOGN')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'EXPO')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LOG ')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'FUNC')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'PROB')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'PPCC')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'MAXI')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'MLE ')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'KS  ')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'KOLM')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LAW ')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'CHI ')GOTO9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'CHIS')GOTO9000
C
      IF(ICOM.EQ.'POWE')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1              IPL1NU,IPL1NA,IPL2NU,IPL2NA,
     1              IDEFPO,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IDNVOF,IDNHOF,
     1              ICAPSW,ICAPNU,
     1              IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************
C               **  TREAT THE CONTINUITY CASE  **
C               *********************************
C
      IF((ICOM.EQ.'CONT'.AND.ICOM2.EQ.'INUO') .OR.
     1   (ICOM.EQ.'CONT'.AND.ICOM2.EQ.'INUI'))THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDECN(IHARG,IARGT,IARG,NUMARG,
     1              IDEFCN,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE COLOR CASE  **
C               ****************************
C
      IF(ICOM.EQ.'COLO')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDECL(IHARG,IARGT,IARG,NUMARG,
     1              IDEFDC,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  TREAT THE PICTURE POINTS CASE  **
C               *************************************
C
      IF(ICOM.EQ.'PICT' .OR. ICOM.EQ.'PP')THEN
        ISHIFT=2
        IF(ICOM.EQ.'PP')ISHIFT=3
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)='PICT'
        IHARG2(2)='TURE'
        IARGT(2)='WORD'
        IF(ICOM.EQ.'NE')THEN
          IHARG(3)='POIN'
          IHARG2(3)='TS  '
          IARGT(3)='WORD'
        ENDIF
        CALL DPDEPP(IHARG,IARGT,IARG,NUMARG,
     1              IDEFVP,IDEFHP,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  TREAT THE UNIT NUMBER CASE  **
C               *************************************
C
      IF(ICOM.EQ.'UNIT')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDEUN(IHARG,IARGT,IARG,NUMARG,
     1              IDEFUN,
     1              NUMDEV,MAXDEV,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE EXPLICIT MANUFACTURER CASE   **
C               **  (FOR A SUBSET OF AVAILABLE TERMINALS)  **
C               *********************************************
C
      IF(ICOM.EQ.'TEKT'.AND.IHARG(1).NE.'META')GOTO3600
      IF(ICOM.EQ.'HEWL')GOTO3600
      IF(ICOM.EQ.'HP')GOTO3600
      IF(ICOM.EQ.'HPGL')GOTO3600
      IF(ICOM.EQ.'RAMT')GOTO3600
      IF(ICOM.EQ.'TELE')GOTO3600
      IF(ICOM.EQ.'VT')GOTO3600
      IF(ICOM.EQ.'DEC')GOTO3600
CCCCC MAY, 1994.  CHECK FOR CONFLICT WITH REGION COMMAND.
CCCCC IF(ICOM.EQ.'REGI')GOTO3600
      IF(ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   ')GOTO3600
      IF(ICOM.EQ.'RAMT')GOTO3600
CCCCC THE FOLLOWING 5 LINES WERE ADDED BY ALAN.  FEBRUARY 1989
      IF(ICOM.EQ.'SUN')GOTO3600
      IF(ICOM.EQ.'PCL')GOTO3600
      IF(ICOM.EQ.'POST')GOTO3600
CCCCC MARCH 1995.  ADD FOLLOWING 3 LINES
      IF(ICOM.EQ.'ENCA')THEN
        IF(IHARG(1).EQ.'POST'.OR.IHARG(1).EQ.'PS')THEN
          ICOM='POST'
          IHARG(1)='ENCA'
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1    IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GOTO3600
      ENDIF
CCCCC OCTOBER 1996.  ADD FOLLOWING LINES
      IF(ICOM.EQ.'DISP')THEN
        IF(IHARG(1).EQ.'POST'.OR.IHARG(1).EQ.'PS')THEN
          ICOM='POST'
          IHARG(1)='DISP'
          GOTO3600
        ENDIF
      ENDIF
C
      IF(ICOM.EQ.'PS  ')THEN
        ICOM='POST'
        GOTO3600
      ENDIF
      IF(ICOM.EQ.'EPS ')THEN
        IF(IHARG(1).EQ.'POST')THEN
          ICOM='POST'
          IHARG(1)='ENCA'
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1    IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GOTO3600
      ENDIF
C
      IF(ICOM.EQ.'DICO')GOTO3600
      IF((ICOM.EQ.'QUIC'.AND.ICOM2.EQ.'KWIN').OR.
     1 (ICOM.EQ.'QUIC'.AND.ICOM2.EQ.'K-WI').OR.
     1 (ICOM.EQ.'MS'.AND.IHARG(1).EQ.'WIND').OR.
     1 (ICOM.EQ.'MICR'.AND.IHARG(1).EQ.'WIND'))THEN
        ICOM='QWIN'
        IHARG(1)='    '
        IARGT(1)='WORD'
        GOTO3600
      ENDIF
      IF(ICOM.EQ.'QUIC')GOTO3600
CCCCC FOLLOWING LINE ADDED MARCH 1990 BY ALAN.
      IF(ICOM.EQ.'X11 ')GOTO3600
CCCCC FOLLOWING 2 LINES ADDED FOR CONFLICT WITH DISCRET UNIFORM 
CCCCC PROBABILITY PLOT.   SEPTEMBER 1994.
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'UNIF')GOTO9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'PROB')GOTO9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'ARCS')GOTO9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'WEIB')GOTO9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'CONT'.AND.
     1   IHARG(2).EQ.'PLOT')GOTO9000
      IF(ICOM.EQ.'DISC')GOTO3600
CCCCC NOVEMBER 2008: CHECK FOR CONFLICT WITH "BATCH STRIP PLOT"
      IF(NUMARG.GE.2.AND.ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'STRI'.AND.
     1   IHARG(2).EQ.'PLOT')GOTO9000
      IF(NUMARG.GE.3.AND.ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'MULT'.AND.
     1   IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')GOTO9000
      IF(ICOM.EQ.'BATC')GOTO3600
CCCCC SEPTEMBER 1997.  CHECK FOR CONFLICT WITH ANDERSON DARLING TEST
CCCCC IF(ICOM.EQ.'ANDE')GOTO3600
      IF(ICOM.EQ.'ANDE')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DARL')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TEST')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NORM')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WEIB')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LOGI')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GOTO9000
        GOTO3600
      ENDIF
      IF(ICOM.EQ.'AJ')GOTO3600
      IF(ICOM.EQ.'HAZE')GOTO3600
      IF(ICOM.EQ.'OMRO')GOTO3600
      IF(ICOM.EQ.'TERM'.AND.ICOM2.EQ.'INET')GOTO3600
      IF(ICOM.EQ.'TEXA')GOTO3600
      IF(ICOM.EQ.'TI')GOTO3600
CCCCC THE FOLLOWING 4 LINES WERE ADDED MAY 1991 (JJF)
 
      IF(ICOM.EQ.'TURB')GOTO3600
      IF(ICOM.EQ.'TC')GOTO3600
      IF(ICOM.EQ.'VGA')GOTO3600
      IF(ICOM.EQ.'EGA')GOTO3600
      IF(ICOM.EQ.'LAHE ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'INTE')THEN
          ICOM='INTE'
          IHARG(1)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'WINT')THEN
          ICOM='WINT'
          IHARG(1)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIN '.AND.
     1         IHARG(2).EQ.'INTE')THEN
          ICOM='WINT'
          IHARG(1)='    '
          IHARG(2)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIND'.AND.
     1         IHARG(2).EQ.'INTE')THEN
          ICOM='WINT'
          IHARG(1)='    '
          IHARG(2)='    '
          NUMARG=0
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1    IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GOTO3600
      ENDIF
C
      IF(ICOM.EQ.'GKS ')GOTO3600
      IF(ICOM.EQ.'GD  ')GOTO3600
      IF(ICOM.EQ.'SVG ')GOTO3600
      IF(ICOM.EQ.'OPEN'.AND.ICOM2.EQ.'GL  ')THEN
          ICOM='OPGL'
          GOTO3600
      ENDIF
      IF(ICOM.EQ.'OPEN'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GL  ')THEN
          ICOM='OPGL'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGO2,IERROR)
          GOTO3600
      ENDIF
C
      GOTO3699
C
 3600 CONTINUE
C
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGO2,IERROR)
      IHARG(1)='1   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=1
      IHARG(2)=ICOM
      IHARG2(2)=ICOM2
      IARGT(2)='WORD'
CCCCC OCTOBER 1993.  FIX BUG WHERE DISCRETE ON, BATCH ON ACT
CCCCC LIKE DISCRETE OFF, ETC.  STRIP OFF ON ARGUMENT.
      IF(IHARG(2).EQ.'DISC'.OR.IHARG(2).EQ.'BATC')THEN
        IF(NUMARG.GE.3.AND.IHARG(NUMARG).EQ.'ON')THEN
          IHARG(NUMARG)='    '
          NUMARG=NUMARG-1
        ENDIF
      ENDIF
CCCCC END CHANGE
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IPL1CS,IPL2CS,
     1IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3699 CONTINUE
C
C               *********************************************
C               **  TREAT THE DISCRETE CASE                **
C               **  TREAT THE DISCRETE NARROW-WIDTH CASE   **
C               **  TREAT THE DISCRETE WIDE-CARRIAGE CASE  **
C               **  TREAT THE BATCH    CASE                **
C               *********************************************
C
      IF(ICOM.EQ.'DISC')GOTO4100
      IF(ICOM.EQ.'BATC')GOTO4100
      GOTO4199
C
 4100 CONTINUE
      ISHIFT=3
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGO2,IERROR)
      IHARG(1)='1   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=1
      IHARG(2)='MANU'
      IHARG2(2)='FACT'
      IARGT(2)='WORD'
      IHARG(3)=ICOM
      IHARG2(3)=ICOM2
      IARGT(3)='WORD'
CCCCC OCTOBER 1993.  FIX BUG WHERE DISCRETE ON, BATCH ON ACT
CCCCC LIKE DISCRETE OFF, ETC.  STRIP OFF ON ARGUMENT.
      IF(IHARG(2).EQ.'DISC'.OR.IHARG(2).EQ.'BATC')THEN
        IF(NUMARG.GE.3.AND.IHARG(NUMARG).EQ.'ON')THEN
          IHARG(NUMARG)='    '
          NUMARG=NUMARG-1
        ENDIF
      ENDIF
CCCCC END CHANGE
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IPL1CS,IPL2CS,
     1IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4199 CONTINUE
C
C
C               *********************************
C               **  TREAT THE PENPLOTTER CASE  **
C               *********************************
C
      IF(ICOM.EQ.'PENP')GOTO4200
      GOTO4299
C
 4200 CONTINUE
      IF(NUMARG.LE.0)IOP='ON'
      IF(NUMARG.GE.1)IOP=IHARG(1)
      IF(IOP.EQ.'OPEN')IOP='ON'
      IF(IOP.EQ.'AUTO')IOP='ON'
      IF(IOP.EQ.'DEFA')IOP='ON'
      IF(IOP.EQ.'CLOS')IOP='OFF'
C
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGO2,IERROR)
      IHARG(1)='1   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=1
C
      IF(IOP.EQ.'ON')GOTO4210
      GOTO4220
C
 4210 CONTINUE
      IHARG(2)='MANU'
      IHARG2(2)='FACT'
      IARGT(2)='WORD'
      IHARG(3)='TEKT'
      IHARG2(3)='RONI'
      IARGT(3)='WORD'
      IHARG(4)='4662'
      IHARG2(I)='    '
      IARGT(4)='WORD'
      NUMARG=4
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IPL1CS,IPL2CS,
     1IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO4299
C
 4220 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IDEFPO,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO4299
C
 4299 CONTINUE
C
C               *******************************
C               **  TREAT THE HARDCOPY CASE  **
C               *******************************
C
      IF(ICOM.EQ.'HARD')GOTO4300
      GOTO4399
C
 4300 CONTINUE
      CALL DPHAPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1            ICOPSW,NUMCOP,
     1            IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4399 CONTINUE
C
C               ******************************
C               **  TREAT THE FILE   CASE  **
C               **  TREAT THE CALCOMP CASE  **
C               **  TREAT THE VERSATEC CASE  **
C               **  TREAT THE ZETA     CASE  **
C               ******************************
C
      IF(ICOM.EQ.'TEKT'.AND.IHARG(1).EQ.'META')GOTO5100
      IF(ICOM.EQ.'CALC')GOTO5100
      IF(ICOM.EQ.'VERS')GOTO5100
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'CHI ')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'CHIS')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'KS  ')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'KOLM')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PROB')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PPCC')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MLE ')GOTO9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MAXI')GOTO9000
      IF(ICOM.EQ.'ZETA')GOTO5100
      GOTO5199
C
 5100 CONTINUE
      IDMANU(1)=ICOM
      IDMODE(1)='    '
      IDMOD2(1)='    '
      IDMOD3(1)='    '
      IF(NUMARG.LE.0)IOP='ON'
      IF(NUMARG.GE.1)IOP=IHARG(1)
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'META')IOP='ON'
      IF(IOP.EQ.'OPEN')IOP='ON'
      IF(IOP.EQ.'AUTO')IOP='ON'
      IF(IOP.EQ.'DEFA')IOP='ON'
      IF(IOP.EQ.'CLOS')IOP='OFF'
C
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGO2,IERROR)
      IHARG(1)='2   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=2
C
      IF(IOP.EQ.'ON')GOTO5110
      GOTO5120
C
 5110 CONTINUE
      IHARG(2)='MANU'
      IHARG2(2)='FACT'
      IARGT(2)='WORD'
      IHARG(3)=IDMANU(1)
      IHARG2(3)='    '
      IARGT(3)='WORD'
      IHARG(4)=IDMODE(1)
      IHARG2(4)='    '
      IARGT(4)='WORD'
      IHARG(5)=IDMOD2(1)
      IHARG2(5)='    '
      IARGT(5)='WORD'
      IHARG(6)=IDMOD3(1)
      IHARG2(6)='    '
      IARGT(6)='WORD'
      NUMARG=6
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IPL1CS,IPL2CS,
     1IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5199
C
 5120 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IDEFPO,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5199
C
 5199 CONTINUE
C
C               ******************************
C               **  TREAT THE GENERAL METAFILE   CASE  **
C               ******************************
C
      IF(ICOM.EQ.'META')GOTO5200
      IF(ICOM.EQ.'GENE'.AND.IHARG(1).EQ.'META')GOTO5200
      GOTO5299
C
 5200 CONTINUE
      IF(NUMARG.LE.0)IOP='ON'
      IF(NUMARG.GE.1)IOP=IHARG(1)
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'META')IOP='ON'
      IF(IOP.EQ.'OPEN')IOP='ON'
      IF(IOP.EQ.'AUTO')IOP='ON'
      IF(IOP.EQ.'DEFA')IOP='ON'
      IF(IOP.EQ.'CLOS')IOP='OFF'
C
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGO2,IERROR)
      IHARG(1)='3   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=3
C
      IF(IOP.EQ.'ON')GOTO5210
      GOTO5220
C
 5210 CONTINUE
      IHARG(2)='MANU'
      IHARG2(2)='FACT'
      IARGT(2)='WORD'
      IHARG(3)='META'
      IHARG2(3)='FILE'
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IPL1CS,IPL2CS,
     1IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5299
C
 5220 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IPL1NU,IPL1NA,
     1IPL2NU,IPL2NA,
     1IDEFPO,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,
     1ICAPSW,ICAPNU,
     1IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5299
C
 5299 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED   MAY 1992 (JJF)
C               *********************************
C               **  TREAT THE BLANK PAGE CASE  **
C               *********************************
C
      IF(ICOM.EQ.'BLAN' .AND. IHARG(1).NE.'ALTM')THEN
        CALL DPBLPA(IHARG,NUMARG,
     1              IPSTBP,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  OUTPUT DEVICE COMMAND NOT FOUND--  **
C               **  BRANCH TO EXIT.                    **
C               *****************************************
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IERRST=IERROR
C
C     SEPTEMBER 2012.  CHECK FOR FATAL ERROR
C
      IF(IERROR.EQ.'YES')THEN
        ICASE2='DEVI'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1              ISUBN1,ISUBN2,ICASE2,
     1              IBUGO2,ISUBRO,IERROR)
      ENDIF
C
C
      IF(IBUGOD.EQ.'ON'.OR.ISUBRO.EQ.'INOD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAINOD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAINPC(IBUGPC,IBUGP2,IBUGQ,ISUBRO,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
CCCCC ADD FOLLOWING LINE AUGUST 1999.
     1                  IMPARG,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  IERASV,
     1                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED    APRIL 1992
     1                  BARHEF,BARWEF,
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
     1                  ITIAUT,IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,
     1                  IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAINPC.
C              (THE   PC    AT THE END OF    MAINPC   STANDS FOR   PLOT
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES PLOT CONTROL CO
C              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAINPC ARE AS F
C
C                      ARROW ... COLOR                   A COLOR
C                      ARROW ... COORDINATES             2 NUMBERS
C                      BACKGROUND COLOR                  A COLOR
C                      BELL                              ON/OFF
C                      BOX ... COLOR                     A COLOR
C                      BOX ... CORNER COORDINATES        4 NUMBERS
C                      CHARACTERS                        A LIST OF CHARA
C                      CHARACTER COLORS                  A LIST OF COLOR
C                      CHARACTER SIZES                   A LIST OF NUMBE
C                      CHARACTER FILL                    A LIST OF ON/OF
C                      EYE COORDINATES                   3 NUMBERS
C                      ...FRAME                          ON/OFF
C                      ...FRAME COLOR                    A COLOR
C                      FRAME CORNER COORDINATES          4 NUMBERS
C                      WINDOW CORNER COORDINATES         4 NUMBERS
C                      ...GRID                           ON/OFF
C                      GRID COLOR                        A COLOR
C                      GRID PATTERN                      PATTERN
C                      ...LABEL                          A STRING OF CHA
C                      LABEL COLOR                       A COLOR
C                      LABEL SIZE                        A NUMBER
C                      LEGEND ...                        A STRING OF CHA
C                      LEGEND ... COLOR                  A COLOR
C                      LEGEND ... COORDINATES            2 NUMBERS
C                      LEGEND ... SIZE                   A NUMBER
C                      ...LIMITS                         2 NUMBERS
C                      LINES                             A LIST OF LINE
C                      LINE  COLORS                      A LIST OF COLOR
C                      LINE  THICKNESSES                 A LIST OF THICK
C                      ...LOG                            ON/OFF
C                      MARGIN COLOR                      A COLOR
C                      ...MAXIMUM                        A NUMBER
C                      ...MINIMUM                        A NUMBER
C                      NEGATE                            ON/OFF
C                      ORIGIN COORDINATES                3 NUMBERS
C                      PEDESTAL                          ON/OFF
C                      PEDESTAL COLOR                    A COLOR
C                      PEDESTAL HEIGHT                   A NUMBER
C                      PRE-SORT                          ON/OFF
C                      SEGMENT ... COLOR                 A COLOR
C                      SEGMENT ... COORDINATES           2 NUMBERS
C                      SEQUENCE                          ON/OFF
C                      ...TIC                            ON/OFF
CCCCCC                 ...TIC COLOR                      A COLOR
CCCCCC                 ...TIC DECIMALS                   A NUMBER
CCCCCC                 ...TIC COORDINATES                A LIST OF NUMBE
C                      ...TIC POSITION (JUSTIFICATION)   INSIDE/OUTSIDE/
C                      ...TIC SIZE                       A NUMBER
C                      ...TIC LABELS                     ON/OFF
C                      ...TIC LABEL COLOR                A COLOR
C                      ...TIC LABEL SIZE                 A NUMBER
C                      TITLE                             A STRING OF CHA
C                      TITLE COLOR                       A COLOR
C                      TITLE SIZE                        A NUMBER
C                      VISIBLE                           ON/OFF
C
C                      BAR SWITCH                        A SERIES OF ON/
C                      BAR WIDTH                         A SERIES OF NUM
C                      BAR BASE                          A SERIES OF NUM
C                      BAR BORDER COLOR                  A SERIES OF COL
C                      BAR BORDER THICKNESS              A SERIES OF NUM
C                      BAR BORDER LINE                   A SERIES OF LIN
C                      BAR FILL SWITCH                   A SERIES OF ON/
C                      BAR FILL COLOR                    A SERIES OF COL
C                      BAR PATTERN TYPE                  A SERIES OF PAT
C                      BAR PATTERN COLOR                 A SERIES OF COL
C                      BAR PATTERN SPACING               A SERIES OF NUM
C                      BAR PATTERN THICKNESS             A SERIES OF NUM
C                      BAR PATTERN LINE                  A SERIES OF LIN
C                      BAR TYPES                         A SERIES OF NUMBERS
C
C                      BAR EXPANSION FACTORS             2 NUMBERS
C
C                      REGION BASE                       A SERIES OF NUM
C                      REGION BORDER COLOR               A SERIES OF COL
C                      REGION BORDER THICKNESS           A SERIES OF NUM
C                      REGION BORDER LINE                A SERIES OF LIN
C                      REGION FILL SWITCH                A SERIES OF ON/
C                      REGION FILL COLOR                 A SERIES OF COL
C                      REGION PATTERN TYPE               A SERIES OF PAT
C                      REGION PATTERN COLOR              A SERIES OF COL
C                      REGION PATTERN SPACING            A SERIES OF NUM
C                      REGION PATTERN THICKNESS          A SERIES OF NUM
C                      REGION PATTERN LINE               A SERIES OF LIN
C
C                      TEXT BORDER COLOR                 A SERIES OF COL
C                      TEXT BORDER THICKNESS             A SERIES OF NUM
C                      TEXT BORDER LINE                  A SERIES OF LIN
C                      TEXT FILL SWITCH                  A SERIES OF ON/
C                      TEXT FILL COLOR                   A SERIES OF COL
C                      TEXT PATTERN TYPE                 A SERIES OF PAT
C                      TEXT PATTERN COLOR                A SERIES OF COL
C                      TEXT PATTERN SPACING              A SERIES OF NUM
C                      TEXT PATTERN THICKNESS            A SERIES OF NUM
C                      TEXT PATTERN LINE                 A SERIES OF LIN
C
C                      MAJOR ...TIC MARK NUMBER          A NUMBER
C                      MINOR ...TIC MARK NUMBER          A NUMBER
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --APRIL     1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JULY      1986.
C     UPDATED         --SEPTEMBER 1988. 3D PROJECTION (ORTHOGRAP./PERSPECT.)
C     UPDATED         --SEPTEMBER 1988. INCLUDE DPCO3D.INC
C     UPDATED         --APRIL     1992. BAR EXPANSION FACTORS ... ...
C     UPDATED         --AUGUST    1992. ADD SWITCHES FOR AUTOMATIC
C     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR AUTOMATIC SWITCHES
C     UPDATED         --AUGUST    1999. ARGUMENT LIST TO MAIPC2
C     UPDATED         --SEPTEMBER 2007. IERRST
C     UPDATED         --SEPTEMBER 2012. SET FATAL ERROR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
C
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
C
CCCCC THE FOLLOWING 6 LINES WERE ADDED    SEPTEMBER 1993
      CHARACTER*4 ITIAUT
      CHARACTER*4 IX1AUT
      CHARACTER*4 IX2AUT
      CHARACTER*4 IX3AUT
      CHARACTER*4 IY1AUT
      CHARACTER*4 IY2AUT
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'INPC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAINPC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,IBUGP2,ISUBRO,IANGLU
   53   FORMAT('IBUGPC,IBUGP2,ISUBRO,IANGLU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ICOM,ICOM2,NUMARG
   67   FORMAT('ICOM,ICOM2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IMPSW,IMPNR,IMPNC,IMPCO
   81   FORMAT('IMPSW,IMPNR,IMPNC,IMPCO = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)PMXMIN,PMXMAX,PMYMIN,PMYMAX
   82   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)IERASV,I3DPRO,IERASW
   83   FORMAT('IERASV,I3DPRO,IERASW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)PWXMIS,PWXMAS,PWYMIS,PWYMAS
   84   FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   86   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)PXMIN,PXMAX,PYMIN,PYMAX
   87   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
      CALL MAIPC1(IBUGPC,IBUGP2,IBUGQ,ISUBRO,
     1            IVGMSW,IHGMSW,
     1            IMPSW,IMPNR,IMPNC,IMPCO,
     1            PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1            IERASV,ICHAOF,ICHADY,ICHAVN,
     1            PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
     1            IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,IRGBMX,
     1            IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      CALL MAIPC2(IBUGPC,IBUGP2,ISUBRO,
     1            IVGMSW,IHGMSW,
     1            IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
CCCCC ADD FOLLOWING LINE AUGUST 1999.
     1            IMPARG,
     1            PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1            IERASV,
     1            PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
     1            ITIAUT,IRGBMX,
     1            IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      CALL MAIPC3(IBUGPC,IBUGP2,ISUBRO,
     1            IVGMSW,IHGMSW,
     1            IMPSW,IMPNR,IMPNC,IMPCO,
     1            PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1            IERASV,
     1            PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED    APRIL 1992
     1            BARHEF,BARWEF,IRGBMX,
     1            IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      CALL MAIPC4(IBUGPC,IBUGP2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IERRST=IERROR
C
C     SEPTEMBER 2012.  CHECK FOR FATAL ERROR
C
      IF(IERROR.EQ.'YES')THEN
        ISUBN1='MAIN'
        ISUBN2='IN  '
        ICASE2='INPC'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1              ISUBN1,ISUBN2,ICASE2,
     1              IBUGP2,ISUBRO,IERROR)
      ENDIF
C
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'INPC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)
 9031   FORMAT('***** AT THE END       OF MAINPC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)IFOUND,IERROR,IANGLU
 9033   FORMAT('IFOUND,IERROR,IANGLU = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IMPSW,IMPNR,IMPNC,IMPCO
 9041   FORMAT('IMPSW,IMPNR,IMPNC,IMPCO = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)PMXMIN,PMXMAX,PMYMIN,PMYMAX
 9042   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)IERASV,I3DPRO,IERASW
 9043   FORMAT('IERASV,I3DPRO,IERASW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9044)PWXMIS,PWXMAS,PWYMIS,PWYMAS
 9044   FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9046)PWXMIN,PWXMAX,PWYMIN,PWYMAX
 9046   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9047)PXMIN,PXMAX,PYMIN,PYMAX
 9047   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
CCCCC   THE FOLLOWING 2 LINES WERE ADDED    APRIL 1992
        WRITE(ICOUT,9048)BARHEF,BARWEF
 9048   FORMAT('BARHEF,BARWEF = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END
      SUBROUTINE MAIPC1(IBUGPC,IBUGP2,IBUGQ,ISUBRO,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  IERASV,ICHAOF,ICHADY,ICHAVN,
     1                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
     1                  IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,IRGBMX,
     1                  IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAIPC1.
C              (THE   PC    AT THE END OF    MAIPC1   STANDS FOR PLOT CONTROL
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
C              PLOT CONTROL COMMANDS (PART 1).
C              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAIPC1 ARE AS F
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
C     UPDATED--JULY 1987        LEGEND HW
C     UPDATED--FEBRUARY      1988 FURTHER RESOLVE CONFLICT--MIN VS MIN PLOT
C                                                           AND MAX VS MAX PLO
C     UPDATED--MARCH     1988.  FURTHER RESOLVE CONFLICT--MIN VS MIN PLOT
C                                                         AND MAX VS MAX PLO
C     UPDATED--SEPTEMBER 1988.  MOVE EYE/ORIGIN/PEDESTAL COMMANDS
C                               TO MAIPC4 FOR GENERAL 3-D.
C     UPDATED         --SEPTEMBER 1988.  CHANGE 'BACK' TO 'BACKGROU'
C     UPDATED         --DECEMBER  1988.  LABEL AND LEGEND DEFAULT WIDTH
C     UPDATED         --FEBRUARY  1989.  ADD MANY ATTRIBUTE COMMANDS (ALAN)
C     UPDATED         --MAY       1989.  DES. OF EXP. WIDTH/DEPTH/HOR. AXIS
C     UPDATED         --JULY      1989.  ...LABEL DISPLACEMENT
C     UPDATED         --FEBRUARY  1992. FIX LEGEND DIRECTION CONFLICT
C     UPDATED         --APRIL     1992. IDEXHO TO IDEXHA
C     UPDATED         --AUGUST    1992. ADD SWITCHES FOR AUTOMATIC
C     UPDATED         --AUGUST    1992. BOX SHADOW HEIGHT/WIDTH
C     UPDATED         --AUGUST    1992. BOX FILL COLOR
C     UPDATED         --AUGUST    1992. BOX FILL PATTERN
C     UPDATED         --AUGUST    1992. BOX FILL THICK
C     UPDATED         --AUGUST    1992. BOX FILL GAP
C     UPDATED         --MARCH     1993. BUG IN CALL TO DPBOTH
C     UPDATED         --SEPTEMBER 1993. LOWER CASE LABELS
C     UPDATED         --SEPTEMBER 1993. LOWER CASE LEGENDS
C     UPDATED         --SEPTEMBER 1993. 3-D FRAME SWITCH
C     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR AUTOMATIC SWITCHES
C     UPDATED         --OCTOBER   1993. BACKGROUND COLOR SETS THE 
C                                       MARGIN COLOR AS WELL
C     UPDATED         --DECEMBER  1994. EXACT CHARACTER MAPPING
C     UPDATED         --JANUARY   1995. FIX DEFAULT CHAR SIZE
C     UPDATED         --APRIL     1995. CHECK FOR COMMAND CONFLICT
C     UPDATED         --AUGUST    1995. SEGMENT PATTERN, FRAME PATTERN,
C                                       BUG (DASH2, ETC)
C     UPDATED         --NOVEMBER  1997. CALL TO DPLIM
C     UPDATED         --JANUARY   1998. NAME CONFLICTS FOR MAXI, MINI
C     UPDATED         --FEBRUARY  1998. LINE/CHAR <SAVE/RESTORE>
C     UPDATED         --OCTOBER   1999. LABEL JUSTIFICIATION
C     UPDATED         --OCTOBER   1999. LABEL OFFSET
C     UPDATED         --DECEMBER  1999. LEGEND UNITS
C     UPDATED         --OCTOBER   2018. LABEL COORDINATES
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR (MODIFY
C                                       CALL LISTS FOR A NUMBER OF
C                                       ROUTINES)
C     UPDATED         --MARCH     2021. CALL LIST TO DPLIM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHADY
      CHARACTER*8 ICHAVN
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 ICASCL
C
CCCCC THE FOLLOWING 5 LINES WERE ADDED   SEPTEMBER 1993
      CHARACTER*4 IX1AUT
      CHARACTER*4 IX2AUT
      CHARACTER*4 IX3AUT
      CHARACTER*4 IY1AUT
      CHARACTER*4 IY2AUT
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
CCCCC THE FOLLOWING DES. OF EXP. LINE WAS ADDED MAY 1989
      INCLUDE 'DPCODE.INC'
CCCCC THE FOLLOWING 3D LINE WAS ADDED SEPTEMBER 1993
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAIPC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV
   53   FORMAT('IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ICOM,ICOM2,NUMARG
   67   FORMAT('ICOM,ICOM2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)ICHADY,ICHAOF,ICHAVN
   68   FORMAT('ICHADY,ICHAOF,ICHAVN = ',2(A4,2X),A8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2
   81   FORMAT('IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2 = ',A4,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)PMXMIN,PMXMAX,PMYMIN,PMYMAX
   82   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)PWXMIS,PWXMAS,PWYMIS,PWYMAS
   84   FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   86   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)PXMIN,PXMAX,PYMIN,PYMAX
   87   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,88)IVGMSW,IHGMSW
   88   FORMAT('IVGMSW,IHGMSW = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
C               ********************************************
C               **  TREAT THE ARROW ... COLOR CASE        **
C               **            ARROW ... PATTERN CASE      **
C               **            ARROW ... THICKNESS CASE    **
C               **            ARROW ... COORDINATES CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'ARRO')THEN
        CALL DPARCL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IDEFCO,MAXARR,IARRCO,IARRC2,IRGBMX,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPARPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,
     1              MAXARR,IARRPA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPARTH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1              MAXARR,PARRTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPARCO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1              MAXNAM,IANS,IWIDTH,
     1              MAXARR,PARRXC,PARRYC,NUMARR,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               ***************************************
C               **  TREAT THE BACKGROUND COLOR CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'GROU')THEN
        CALL DPBACL(IHARG,IARG,NUMARG,IDEFBK,IRGBMX,IBACCO,IBACC2,
     1              IFOUND,IERROR)
        IF(IERROR.EQ.'NO')IMARCO=IBACCO
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE BELL CASE  **
C               ***************************
C
      IF(ICOM.EQ.'BELL')THEN
        CALL DPBELL(IHARG,NUMARG,IBELSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************************
C               **  TREAT THE BOX ... CORNER COORDINATES CASE  **
C               **            BOX ... COLOR CASE               **
C               **            BOX ... PATTERN CASE             **
C               **            BOX ... THICKNESS CASE           **
C               **            BOX ... FILL COLOR CASE          **
C               **            BOX ... FILL PATTTERN CASE       **
C               **            BOX ... FILL LINE     CASE       **
C               **            BOX ... FILL THICKNESS CASE      **
C               **            BOX ... FILL GAP       CASE      **
C               **            BOX ... SHADOW HW CASE           **
C               *************************************************
C
      IF(ICOM.EQ.'BOX')THEN
C
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'COOR') .OR.
     1     (NUMARG.GE.2.AND.IHARG(2).EQ.'COOR') .OR.
     1     (NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.
     1      IHARG(2).EQ.'COOR') .OR.
     1     (NUMARG.GE.3.AND.IHARG(2).EQ.'CORN'.AND.
     1      IHARG(3).EQ.'COOR'))THEN
          CALL DPBOCC(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1                MAXNAM,IANS,IWIDTH,
     1                MAXBOX,PBOXXC,PBOXYC,NUMBOX,IBUGP2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'COLO') .OR.
     1     (NUMARG.GE.2.AND.IHARG(2).EQ.'COLO'.AND.
     1      IHARG(1).NE.'FILL'.AND.IHARG(1).NE.'RGB'))THEN
          ICASCL='STAN'
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,
     1                MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.
     1      IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,
     1                MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NUMARG.GE.3.AND.IHARG(2).EQ.'RGB '.AND.
     1     IHARG(3).EQ.'COLO'.AND.IHARG(1).NE.'FILL')THEN
          ICASCL='RGB'
          IF(IARGT(1).EQ.'NUMB')THEN
            ISHIFT=1
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,ISUBRO,IERROR)
          ELSE
            ISHIFT=2
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
          ENDIF
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,
     1                MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NUMARG.GE.4.AND.IHARG(3).EQ.'RGB '.AND.
     1     IHARG(4).EQ.'COLO'.AND.IHARG(2).NE.'FILL')THEN
          ICASCL='RGB'
          IF(IARGT(1).EQ.'NUMB')THEN
            ISHIFT=2
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,ISUBRO,IERROR)
          ELSE
            ISHIFT=3
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
          ENDIF
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,
     1                MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'PATT') .OR.
     1     (NUMARG.GE.2.AND.IHARG(2).EQ.'PATT'.AND.
     1      IHARG(1).NE.'FILL'))THEN
          CALL DPBOPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFFI,
     1                MAXBOX,IBOBPA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'THIC') .OR.
     1     (NUMARG.GE.2.AND.IHARG(2).EQ.'THIC'.AND.
     1      IHARG(1).NE.'FILL'))THEN
          CALL DPBOTH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1                MAXBOX,PBOPTH,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO')THEN
            ICASCL='STAN'
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,
     1                  IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'COLO')THEN
            ICASCL='STAN'
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,
     1                  IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
        IF(NUMARG.GE.3)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'RGB '.AND.
     1       IHARG(3).EQ.'COLO')THEN
            ICASCL='RGB'
            ISHIFT=1
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
            IHARG(1)='FILL'
            IHARG2(1)='    '
            IHARG(2)='COLO'
            IHARG2(2)='    '
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,
     1                  IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.4)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'RGB '.AND.
     1       IHARG(4).EQ.'COLO')THEN
            ICASCL='RGB'
            ISHIFT=1
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,ISUBRO,IERROR)
            IHARG(2)='FILL'
            IHARG2(2)='    '
            IHARG(3)='COLO'
            IHARG2(3)='    '
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,
     1                  IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'PATT')THEN
            CALL DPBOFP(IHARG,IARGT,IARG,NUMARG,IDEFFI,
     1                  MAXBOX,IBOFPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'PATT')THEN
            CALL DPBOFP(IHARG,IARGT,IARG,NUMARG,IDEFFI,
     1                  MAXBOX,IBOFPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
C
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'LINE')THEN
            CALL DPBOFL(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,
     1                  MAXBOX,IBOPPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'LINE')THEN
            CALL DPBOFL(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,
     1                  MAXBOX,IBOPPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'THIC')THEN
            CALL DPBOFT(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1                  MAXBOX,PBOFTH,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'THIC')THEN
            CALL DPBOFT(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1                  MAXBOX,PBOFTH,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'GAP')THEN
            CALL DPBOFG(IHARG,IARGT,IARG,ARG,NUMARG,PDEFGA,
     1                  MAXBOX,PBOPGA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'GAP')THEN
            CALL DPBOFG(IHARG,IARGT,IARG,ARG,NUMARG,PDEFGA,
     1                  MAXBOX,PBOPGA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
        IF(NUMARG.GE.1)THEN
          IF(IHARG(1).EQ.'SHAD')THEN
             CALL DPBSHW(IHARG,IARGT,IARG,ARG,NUMARG,PDEFSH,PDEFSW,
     1                   MAXBOX,PBOSHE,PBOSWI,IFOUND,IERROR)
             IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.2)THEN
          IF(IHARG(2).EQ.'SHAD')THEN
            CALL DPBSHW(IHARG,IARGT,IARG,ARG,NUMARG,PDEFSH,PDEFSW,
     1                  MAXBOX,PBOSHE,PBOSWI,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
      ENDIF
C
C               *************************************************
C               **  TREAT THE FRAME (CORNER) COORDINATES CASE  **
C               *************************************************
C
      IF(ICOM.EQ.'FRAM')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.
     1     IHARG(2).EQ.'COOR')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')THEN
          CONTINUE
        ELSE
          GOTO1299
        ENDIF
        CALL DPFRCC(IHARG,IHARG2,IARGT,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1              MAXNAM,IANS,IWIDTH,
     1              PXMIN,PXMAX,PYMIN,PYMAX,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
 1299 CONTINUE
C
C               **************************************
C               **  TREAT THE FRAME COLOR CASE      **
C               **            FRAME PATTERN CASE    **
C               **            FRAME THICKNESS CASE  **
C               **            FRAME CASE            **
C               **************************************
C
      IF(ICOM.EQ.'XFRA' .OR. ICOM.EQ.'X1FR' .OR. ICOM.EQ.'X2FR' .OR.
     1   ICOM.EQ.'YFRA' .OR. ICOM.EQ.'Y1FR' .OR. ICOM.EQ.'Y2FR' .OR.
     1   ICOM.EQ.'XYFR' .OR. ICOM.EQ.'YXFR' .OR. ICOM.EQ.'FRAM' .OR.
     1   ICOM.EQ.'3DFR')THEN
C
        IF(ICOM.EQ.'3DFR')GOTO1310
C
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPFRCL(ICOM,IHARG,IARG,NUMARG,
     1                IDEFCO,ICASCL,IRGBMX,
     1                IX1FCO,IX2FCO,IY1FCO,IY2FCO,
     1                IX1FC2,IX2FC2,IY1FC2,IY2FC2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.
     1         IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPFRCL(ICOM,IHARG,IARG,NUMARG,
     1                IDEFCO,ICASCL,IRGBMX,
     1                IX1FCO,IX2FCO,IY1FCO,IY2FCO,
     1                IX1FC2,IX2FC2,IY1FC2,IY2FC2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')THEN
          CALL DPFRPA(ICOM,IHARG,IHARG2,NUMARG,
     1                IDEFPA,IX1FPA,IX2FPA,IY1FPA,IY2FPA,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')THEN
          CALL DPFRTH(ICOM,IHARG,ARG,NUMARG,
     1                PDEFTH,PFRATH,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
 1310   CONTINUE
C
        CALL DPFRAM(ICOM,IHARG,NUMARG,
     1              IX1FSW,IX2FSW,IY1FSW,IY2FSW,FRAM3D,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE GRID  THICKNESS CASE  **
C               **            GRID  COLOR CASE      **
C               **            GRID  PATTERN CASE    **
C               **            GRID CASE             **
C               **************************************
C
      IF(ICOM.EQ.'XGRI' .OR. ICOM.EQ.'YGRI' .OR. ICOM.EQ.'XYGR' .OR.
     1   ICOM.EQ.'YXGR' .OR. ICOM.EQ.'GRID')THEN
C
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')THEN
          CALL DPGRTH(ICOM,IHARG,ARG,NUMARG,
     1                PDEFTH,PVGRTH,PHGRTH,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPGRCL(ICOM,IHARG,IARG,NUMARG,
     1                IDEFCO,ICASCL,IRGBMX,
     1                IVGRCO,IHGRCO,IVGRC2,IHGRC2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.
     1         IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPGRCL(ICOM,IHARG,IARG,NUMARG,
     1                IDEFCO,ICASCL,IRGBMX,
     1                IVGRCO,IHGRCO,IVGRC2,IHGRC2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')THEN
          CALL DPGRPA(ICOM,IHARG,IHARG2,NUMARG,
     1                IDEFPA,IVGRPA,IHGRPA,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSE
C
          CALL DPGRID(ICOM,IHARG,NUMARG,IVGRSW,IHGRSW,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ENDIF
C
      ENDIF
C
C               ********************************************
C               **  TREAT THE LABEL FONT            CASE  **
C               **            LABEL CASE            CASE  **
C               **            LABEL FILL            CASE  **
C               **            LABEL JUSTIFICATION   CASE  **
C               **            LABEL THICKNESS       CASE  **
C               **            LABEL DISPLACEMENT    CASE  **
C               **            LABEL OFFSET          CASE  **
C               **            LABEL ANGLE           CASE  **
C               **            LABEL DIRECTION       CASE  **
C               **            LABEL COLORS          CASE  **
C               **            LABEL SIZES           CASE  **
C               **            LABEL REFERENCE POINT CASE  **
C               **            LABEL                 CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'LABE' .OR. ICOM.EQ.'XLAB' .OR. ICOM.EQ.'X1LA' .OR.
     1   ICOM.EQ.'X2LA' .OR. ICOM.EQ.'X3LA' .OR. ICOM.EQ.'YLAB' .OR.
     1   ICOM.EQ.'Y1LA' .OR. ICOM.EQ.'Y2LA')THEN
C
        CALL DPLAFO(ICOM,IHARG,NUMARG,
     1              IDEFFO,IX1LFO,IX2LFO,IX3LFO,IY1LFO,IY2LFO,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLACA(ICOM,IHARG,NUMARG,
     1              IDEFCA,IX1LCA,IX2LCA,IX3LCA,IY1LCA,IY2LCA,
     1              IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLAFI(ICOM,IHARG,NUMARG,
     1              IDEFFI,IX1LFI,IX2LFI,IX3LFI,IY1LFI,IY2LFI,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLAJU(ICOM,IHARG,NUMARG,
     1              IDEFJU,IX1LJU,IX2LJU,IX3LJU,IY1LJU,IY2LJU,
     1              IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLATH(ICOM,IHARG,ARG,NUMARG,
     1              PDEFTH,PX1LTH,PX2LTH,PX3LTH,PY1LTH,PY2LTH,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLADS(ICOM,IHARG,ARG,NUMARG,
     1              PDEFDS,PX1LDS,PX2LDS,PX3LDS,PY1LDS,PY2LDS,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLAOF(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              PDEFOF,PX1LOF,PX2LOF,PX3LOF,PY1LOF,PY2LOF,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLAAN(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              ADEFAN,PX1LAN,PX2LAN,PX3LAN,PY1LAN,PY2LAN,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLADI(ICOM,IHARG,NUMARG,
     1              IDEFDI,IX1LDI,IX2LDI,IX3LDI,IY1LDI,IY2LDI,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLACL(ICOM,IARG,IHARG,NUMARG,
     1              IDEFCO,IX1LCO,IX2LCO,IX3LCO,IY1LCO,IY2LCO,
     1              IX1LC2,IX2LC2,IX3LC2,IY1LC2,IY2LC2,IRGBMX,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLASZ(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              PDEFHE,PDEFWI,
     1              PX1LHE,PX1LWI,PX1LVG,PX1LHG,
     1              PX2LHE,PX2LWI,PX2LVG,PX2LHG,
     1              PX3LHE,PX3LWI,PX3LVG,PX3LHG,
     1              PY1LHE,PY1LWI,PY1LVG,PY1LHG,
     1              PY2LHE,PY2LWI,PY2LVG,PY2LHG,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLACO(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              PX1LXC,PX1LYC,PX2LXC,PX2LYC,PX3LXC,PX3LYC,
     1              PY1LXC,PY1LYC,PY2LXC,PY2LYC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLAB(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
     1             IX1LTE,NCX1LA,IX1AUT,
     1             IX2LTE,NCX2LA,IX2AUT,
     1             IX3LTE,NCX3LA,IX3AUT,
     1             IY1LTE,NCY1LA,IY1AUT,
     1             IY2LTE,NCY2LA,IY2AUT,
     1             IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               ***********************************************
C               **  TREAT THE LEGEND ... FONT          CASE  **
C               **            LEGEND ... CASE          CASE  **
C               **            LEGEND ... JUSTIFICATION CASE  **
C               **            LEGEND ... DIRECTION     CASE  **
C               **            LEGEND ... UNITS         CASE  **
C               **            LEGEND ... FILL          CASE  **
C               **            LEGEND ... THICKNESS     CASE  **
C               **            LEGEND ... ANGLE         CASE  **
C               **            LEGEND ... COLORS        CASE  **
C               **            LEGEND ... COORDINATES   CASE  **
C               **            LEGEND ... SIZES         CASE  **
C               **            LEGEND ... HW            CASE  **
C               **            LEGEND ...               CASE  **
C               ***********************************************
C
      IF(ICOM.EQ.'LEGE')THEN
C
        CALL DPLEFO(IHARG,IARGT,IARG,NUMARG,IDEFFO,
     1              MAXLEG,ILEGFO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLECA(IHARG,IARGT,IARG,NUMARG,IDEFCA,
     1              MAXLEG,ILEGCA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEJU(IHARG,IARGT,IARG,NUMARG,IDEFJU,
     1              MAXLEG,ILEGJU,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEDI(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFDI,
     1              MAXLEG,ILEGDI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEUN(IHARG,IARGT,IARG,NUMARG,IDEFUZ,
     1              MAXLEG,ILEGUN,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEFI(IHARG,IARGT,IARG,NUMARG,IDEFFI,
     1              MAXLEG,ILEGFI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLETH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1              MAXLEG,PLEGTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEAN(IHARG,IARGT,IARG,ARG,NUMARG,ADEFAN,
     1              MAXLEG,ALEGAN,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLECL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,IDEFCO,
     1              MAXLEG,ILEGCO,ILEGC2,IRGBMX,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLECO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1              MAXNAM,IANS,IWIDTH,
     1              MAXLEG,PLEGXC,PLEGYC,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLESZ(IHARG,IARGT,IARG,ARG,NUMARG,
     1              PDEFHE,PDEFWI,MAXLEG,
     1              PLEGHE,PLEGWI,PLEGVG,PLEGHG,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEHW(IHARG,IARGT,IARG,ARG,NUMARG,
     1              PDEFHE,MAXLEG,PLEGHE,PLEGWI,PLEGVG,PLEGHG,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPLEG(IHARG,IARG,ARG,IARGT,NUMARG,IANS,IANSLC,IWIDTH,
     1             ILEGNA,ILEGST,ILEGSP,NUMLEG,MAXLEG,
     1             ILEGTE,NCLEG,MXCLEG,IFOUND,IERROR,IBUGP2)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               ********************************
C               **  TREAT THE ...LIMITS CASE  **
C               ********************************
C
      IF(ICOM.EQ.'XLIM' .OR. ICOM.EQ.'X1LI' .OR. ICOM.EQ.'X2LI' .OR.
     1   ICOM.EQ.'YLIM' .OR. ICOM.EQ.'Y1LI' .OR. ICOM.EQ.'Y2LI' .OR.
     1   ICOM.EQ.'XYLI' .OR. ICOM.EQ.'YXLI' .OR. ICOM.EQ.'LIMI')THEN
C
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'DETE')GOTO2499
C
        CALL DPLIM(ICOM,IHARG,IARGT,ARG,NUMARG,
     1             GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1             GX2MIN,GX2MAX,GY2MIN,GY2MAX,
     1             FX1MIN,FX1MAX,FY1MIN,FY1MAX,
     1             FX2MIN,FX2MAX,FY2MIN,FY2MAX,
     1             IX1MIN,IX1MAX,IY1MIN,IY1MAX,
     1             IX2MIN,IX2MAX,IY2MIN,IY2MAX,
     1             FX1MNZ,FX1MXZ,FX2MNZ,FX2MXZ,
     1             FY1MNZ,FY1MXZ,FY2MNZ,FY2MXZ,
     1             GX1MNX,GX1MXX,GY1MNX,GY1MXX,
     1             GX2MNX,GX2MXX,GY2MNX,GY2MXX,
     1             IX1MNX,IX1MXX,IY1MNX,IY1MXX,
     1             IX2MNX,IX2MXX,IY2MNX,IY2MXX,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
 2499 CONTINUE
C
C               ****************************************
C               **  TREAT THE LINE COLORS       CASE  **
C               **  TREAT THE LINE THICKNESS    CASE  **
C               **  TREAT THE LINE UNITS        CASE  **
C               **  TREAT THE LINE              CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'LINE')THEN
C
        IF(IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPLICL(IHARG,IARG,NUMARG,IDEFCO,MAXLIN,ILINCO,ILINC2,
     1                IRGBMX,ICASCL,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPLICL(IHARG,IARG,NUMARG,IDEFCO,MAXLIN,ILINCO,ILINC2,
     1                IRGBMX,ICASCL,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'UNIT')THEN
          CALL DPLIUN(IHARG,NUMARG,MAXLIN,ILINTY,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'THIC')THEN
          CALL DPLITH(IHARG,IARGT,ARG,NUMARG,PDEFLT,MAXLIN,PLINTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSE
          IF(ICOM2.NE.'AR  ')THEN
            CALL DPLINE(IHARG,IHARG2,NUMARG,MAXLIN,ILINPA,ILINPO,
     1                  IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
        ENDIF
C
      ENDIF
C
C               *****************************
C               **  TREAT THE ...LOG CASE  **
C               *****************************
C
      IF(ICOM.EQ.'XLOG' .OR. ICOM.EQ.'X1LO' .OR. ICOM.EQ.'X2LO' .OR.
     1   ICOM.EQ.'YLOG' .OR. ICOM.EQ.'Y1LO' .OR. ICOM.EQ.'Y2LO' .OR.
     1   ICOM.EQ.'LOG ' .OR. ICOM.EQ.'LOGL' .OR.
     1  (ICOM.EQ.'XYLO'.AND.ICOM2.EQ.'G   ') .OR.
     1  (ICOM.EQ.'YXLO'.AND.ICOM2.EQ.'G   '))THEN
C
CCCCC   APRIL 1995.  CHECK FOR LOG LOGISTIC PROB PLOT, LOG LOGISTIC PPCC
CCCCC                PLOT (ALSO ENTERED AS LOGLOGISTIC PROB PLOT)
CCCCC   SEPTEMBER 2001.  CHECK FOR LOG DOUBLE EXPO PROB PLOT,
CCCCC                    LOG DOUBLE EPXO PPCC PLOT
C
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROB')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PROB')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PROB')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PROB')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PPCC')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PPCC')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PPCC')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PPCC')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KOLM')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KOLM')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KOLM')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KOLM')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KS  ')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KS  ')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KS  ')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KS  ')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANDE')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ANDE')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ANDE')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ANDE')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AD  ')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AD  ')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'AD  ')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'AD  ')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHI ')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHI ')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHI ')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'CHI ')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHIS')GOTO2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHIS')GOTO2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHIS')GOTO2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'CHIS')GOTO2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'BETA')GOTO2899
C
        CALL DPTISC(ICOM,IHARG,NUMARG,
     1              IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
 2899 CONTINUE
C
C               ***********************************
C               **  TREAT THE MARGIN COLOR CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'MARG'.AND.
     1  (IHARG(1).EQ.'COLO'.OR.IHARG(1).EQ.'RGB '))THEN
        CALL DPMACL(IHARG,IARG,NUMARG,IDEFMC,IRGBMX,IMARCO,IMARC2,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************
C               **  TREAT THE ...MAXIMUM CASE  **
C               *********************************
C
      IF(ICOM.EQ.'XMAX' .OR. ICOM.EQ.'X1MA' .OR. ICOM.EQ.'X2MA' .OR.
     1   ICOM.EQ.'YMAX' .OR. ICOM.EQ.'Y1MA' .OR. ICOM.EQ.'Y2MA' .OR.
     1   ICOM.EQ.'XYMA' .OR. ICOM.EQ.'YXMA' .OR. ICOM.EQ.'MAXI' .OR.
     1   ICOM.EQ.'MAX ')THEN
        IF(NUMARG.GE.1.AND.ICOM.EQ.'MAXI')THEN
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'STAT'.AND.IHARG(2).EQ.'PLOT')
     1      GOTO3099
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BLOC'.AND.IHARG(2).EQ.'PLOT')
     1      GOTO3099
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIND'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'CUMU'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MOVI'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GOTO3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GOTO3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GOTO3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIXO')GOTO3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'GRUB')GOTO3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'TIET')GOTO3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'DIXO')GOTO3099
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RECO'.AND.IHARG(2).EQ.'LENG')
     1      GOTO3099
        ENDIF
C
        CALL DPMAX(ICOM,IHARG,IARGT,ARG,NUMARG,
     1             GX1MAX,GY1MAX,GX2MAX,GY2MAX,
     1             IX1MAX,IY1MAX,IX2MAX,IY2MAX,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
 3099 CONTINUE
C
C               *********************************
C               **  TREAT THE ...MINIMUM CASE  **
C               *********************************
C
      IF(ICOM.EQ.'XMIN' .OR. ICOM.EQ.'X1MI' .OR. ICOM.EQ.'X2MI' .OR.
     1   ICOM.EQ.'YMIN' .OR. ICOM.EQ.'Y1MI' .OR. ICOM.EQ.'Y2MI' .OR.
     1   ICOM.EQ.'XYMI' .OR. ICOM.EQ.'YXMI' .OR. ICOM.EQ.'MINI' .OR.
     1   ICOM.EQ.'MIN ')THEN
C
        IF(NUMARG.GE.1.AND.ICOM.EQ.'MINI')THEN
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'STAT'.AND.IHARG(2).EQ.'PLOT')
     1      GOTO3199
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BLOC'.AND.IHARG(2).EQ.'PLOT')
     1      GOTO3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIND'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'CUMU'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MOVI'.AND.IHARG(2).EQ.'STAT'
     1      .AND.IHARG(3).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GOTO3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GOTO3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GOTO3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIXO')GOTO3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'GRUB')GOTO3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'TIET')GOTO3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'DIXO')GOTO3199
        ENDIF
C
        CALL DPMIN(ICOM,IHARG,IARGT,ARG,NUMARG,
     1             GX1MIN,GY1MIN,GX2MIN,GY2MIN,
     1             IX1MIN,IY1MIN,IX2MIN,IY2MIN,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
 3199 CONTINUE
C
C               *******************************
C               **  TREAT THE PRE-SORT CASE  **
C               *******************************
C
      IF(ICOM.EQ.'PRE')THEN
        CALL DPPRES(IHARG,NUMARG,ISORSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************************
C               **  TREAT THE ...WEIB (SCALE) AXIS CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'XWEI' .OR. ICOM.EQ.'X1WE' .OR. ICOM.EQ.'X2WE' .OR.
     1   ICOM.EQ.'YWEI' .OR. ICOM.EQ.'Y1WE' .OR. ICOM.EQ.'Y2WE' .OR.
     1  (ICOM.EQ.'XYWE'.AND.ICOM2.EQ.'IB  ') .OR.
     1  (ICOM.EQ.'YXWE'.AND.ICOM2.EQ.'IB  '))THEN
C
        CALL DPTIS2(ICOM,IHARG,NUMARG,
     1              IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               *********************************************
C               **  TREAT THE SEGMENT ... COLOR CASE       **
C               **            SEGMENT ... PATTERN CASE     **
C               **            SEGMENT ... THICKNESS CASE   **
C               **            SEGMENT ... COORDINATES CASE **
C               *********************************************
C
      IF(ICOM.EQ.'SEGM')THEN
        CALL DPSECL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IDEFCO,MAXSEG,ISEGCO,ISEGC2,IRGBMX,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPSEPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,
     1              MAXSEG,ISEGPA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPSETH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,
     1              MAXSEG,PSEGTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPSECO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,
     1              MAXSEG,PSEGXC,PSEGYC,NUMSEG,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               *******************************
C               **  TREAT THE SEQUENCE CASE  **
C               *******************************
C
      IF(ICOM.EQ.'SEQU')THEN
        CALL DPSEQ(IHARG,IARGT,IARG,NUMARG,
     1             ISEQSW,NUMSEQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************************************
C               **  TREAT THE CHARACTER COLORS CASE              **
C               **            CHARACTER FONT   CASE              **
C               **            CHARACTER CASE   CASE              **
C               **            CHARACTER MAPPING CASE             **
C               **            CHARACTER THICKNESS CASE           **
C               **            CHARACTER SIZES CASE               **
C               **            CHARACTER FILL   CASE              **
C               **            CHARACTER WIDTH CASE               **
C               **            CHARACTER JUSTIFICATION CASE       **
C               **            CHARACTER OFFSET CASE              **
C               **            CHARACTER ANGLE CASE               **
C               **            CHARACTER HW (HEIGHT & WIDTH) CASE **
C               **            CHARACTER UNIT   CASE              **
C               **            CHARACTERS CASE                    **
C               ***************************************************
C
      IF(ICOM.EQ.'CHAR')THEN
        IF(IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,
     1                ICHAC2,IRGBMX,ICASCL,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,
     1                ICHAC2,IRGBMX,ICASCL,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'FONT')THEN
          CALL DPCHFO(IHARG,NUMARG,IDEFFO,MAXCHA,ICHAFO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'CASE')THEN
          CALL DPCHCA(IHARG,NUMARG,IDEFCA,MAXCHA,ICHACA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'MAP'.OR.IHARG(1).EQ.'MAPP')THEN
          CALL DPCMAP(IHARG,NUMARG,IDCMAP,ICHMAP,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'THIC')THEN
          CALL DPCHTH(IHARG,ARG,NUMARG,PDEFTH,MAXCHA,PCHATH,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'SIZE' .OR. IHARG(1).EQ.'HEIG')THEN
          CALL DPCHSZ(PDEFHE,MAXCHA,PCHAHE,PCHAWI,PCHAVG,PCHAHG,
     1                IBUGP2,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          CALL DPCHFI(IHARG,NUMARG,IDEFFI,MAXCHA,ICHAFI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'WIDT'.OR.IHARG(2).EQ.'WIDT')THEN
          CALL DPCHWI(IHARG,IARGT,ARG,NUMARG,
     1                PDEFWI,MAXCHA,PCHAWI,PCHAHG,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'JUST'.AND.IHARG2(1).EQ.'IFIC')THEN
          CALL DPCHJU(IHARG,NUMARG,MAXCHA,ICHAJU,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF((IHARG(1).EQ.'OFFS'.AND.IHARG2(1).EQ.'ET  ').OR.
     1         (IHARG(1).EQ.'DISP'.AND.IHARG2(1).EQ.'LACE'))THEN
          CALL DPCHOF(IHARG,IARGT,ARG,NUMARG,
     1                MAXCHA,PCHAHO,PCHAVO,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'ANGL'.AND.IHARG2(1).EQ.'E   ')THEN
          CALL DPCHAN(MAXCHA,ACHAAN,IBUGP2,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'HW')THEN
CCCCC     CALL DPCHHW(IHARG,IARGT,ARG,NUMARG,
          CALL DPCHHW(MAXCHA,PCHAHE,PCHAWI,PDEFHE,PDEFWI,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'UNIT')THEN
          CALL DPCHUN(IHARG,NUMARG,MAXCHA,ICHATY,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSE
          CALL DPCHAR(MAXCHA,ICHAPA,ICHAPO,
     1                IBUGP2,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
CCCCC THE FOLLOWING ANIMATION SWITCH CHUNK WAS ADDED APRIL 1989
C               **************************************************
C               **  TREAT THE ANIMATION SWITCH CASE             **
C               **************************************************
C
      IF(ICOM.EQ.'ANIM' .OR. ICOM.EQ.'UNDR')THEN
        CALL DPANIM(IHARG,NUMARG,IANISW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING 3 DES. FOP EXP. SECTIONS WERE ADDED MAY 1989
C               ******************************************
C               **  TREAT THE DEX WIDTH            CASE **
C               **            DEX DEPTH            CASE **
C               **            DEX HORIZONTAL AXIS  CASE **
C               ******************************************
C
      IF(ICOM.EQ.'DEX')THEN
C
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')THEN
          CALL DPDEWI(IHARG,ARG,NUMARG,DEFDEW,
     1                DEXWID,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'DEPT')THEN
          CALL DPDEDE(IHARG,IARG,NUMARG,IDEDED,
     1                IDEXDE,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'HORI'.AND.
     1         IHARG(2).EQ.'AXIS')THEN
          CALL DPDEHA(IHARG,NUMARG,IDEFHA,
     1                IDEXHA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAIPC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)DEXWID,IDEXDE,IDEXHA
 9051   FORMAT('DEXWID,IDEXDE,IDEXHA = ',E15.7,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAIPC2(IBUGPC,IBUGP2,ISUBRO,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2,
     1                  IMPARG,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  IERASV,
     1                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,
     1                  ITIAUT,IRGBMX,
     1                  IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAIPC2.
C              (THE   PC    AT THE END OF    MAIPC2   STANDS FOR PLOT CONTROL
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
C              PLOT CONTROL COMMANDS (PART 2).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
C     UPDATED  JANUARY    1988.  (OPTIONAL OMISSION OF WORD  MAJOR)
C     UPDATED         --SEPTEMBER 1988. 3D PROJECTION (ORTHOGRAP./PERSPECT.)
C     UPDATED         --SEPTEMBER 1988. PROJECTION (ORTHOGRAPHIC/PERSPECTIVE)
C                       MOVED TO MAIPC4 FOR GENERAL 3-D.
C     UPDATED         --SEPTEMBER 1988.  VISIBLE
C                       MOVED TO MAIPC4 FOR GENERAL 3-D.
C     UPDATED         --DECEMBER 1988.  TIC/TIC LABEL/TITLE SIZE DEFAULT WIDTH
C     UPDATED         --FEBRUARY 1989.  ADDED MANY ATTRIBUTE COMMANDS (ALAN)
C     UPDATED         --JULY     1989.  TITLE DISPLACEMENT
C     UPDATED         --MAY      1990.  TIC MARK OFFSET
C     UPDATED         --AUGUST   1990.  MP FOR MULTIPLOT
C     UPDATED         --AUGUST   1990.  WINDOW SYSTEM
C     UPDATED         --AUGUST   1990.  WINDOW POINTER
C     UPDATED         --AUGUST   1990.  WINDOW SYSTEM COMMON
C     UPDATED         --AUGUST   1991.  TIC LABEL DISPLACEMENT
C     UPDATED         --APRIL    1992.  GRID PATTERN CODE REDUNDANT
C     UPDATED         --AUGUST   1992.  ADD TITLE SWITCH FOR AUTOMATIC
C     UPDATED         --DECEMBER 1992.  FIX CALL TO DPTLDS
C     UPDATED         --SEPTEMBER 1993. LOWER CASE--TIC LABEL CONTENTS
C     UPDATED         --SEPTEMBER 1993. LOWER CASE FOR TITLE
C     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR ITIAUT
C     UPDATED         --AUGUST    1995. DASH2 BUG (VARIOUS)
C     UPDATED         --APRIL     1997. PIXMAP TITLE COMMAND
C     UPDATED         --SEPTEMBER 1998. CALL TO DPMULT
C     UPDATED         --AUGUST    1999. CALL TO DPMULT
C     UPDATED         --NOVEMBER  1999. SUBREGION SWITCH
C     UPDATED         --MAY       2015. EMBED
C                                       EMBDED HW
C                                       EMBDED CORNER COORDINATES
C                                       EMBDED POSITION
C                                       EMBDED HORIZONTAL JUSTIFICATION
C                                       EMBDED VERTICAL JUSTIFICATION
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 ICASCL
      CHARACTER*4 ITIAUT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOWI.INC'
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAIPC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV
   53   FORMAT('IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ICOM,ICOM2,NUMARG
   67   FORMAT('ICOM,ICOM2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2
   81   FORMAT('IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2 = ',A4,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)PMXMIN,PMXMAX,PMYMIN,PMYMAX
   82   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)PWXMIS,PWXMAS,PWYMIS,PWYMAS
   84   FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   86   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)PXMIN,PXMAX,PYMIN,PYMAX
   87   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
C               *************************************************
C               **  TREAT THE ...TIC THICKNESS           CASE  **
C               **  TREAT THE ...TIC                     CASE  **
C               **  TREAT THE ...TIC COLOR               CASE  **
C               **  TREAT THE ...TIC DECIMALS            CASE  **
C               **  TREAT THE ...TIC POSITION            CASE  **
C               **  TREAT THE ...TIC SIZE                CASE  **
C               **  TREAT THE ...TIC OFFSET              CASE  **
C               **  TREAT THE ...TIC LABEL DISPLACEMENT  CASE  **
C               **  TREAT THE ...TIC LABEL FONT          CASE  **
C               **  TREAT THE ...TIC LABEL CASE          CASE  **
C               **  TREAT THE ...TIC LABEL JUSTIFICATION CASE  **
C               **  TREAT THE ...TIC LABEL DIRECTION     CASE  **
C               **  TREAT THE ...TIC LABEL FILL          CASE  **
C               **  TREAT THE ...TIC LABEL THICKNESS     CASE  **
C               **  TREAT THE ...TIC LABEL ANGLE         CASE  **
C               **  TREAT THE ...TIC LABEL               CASE  **
C               **  TREAT THE ...TIC LABEL COLOR         CASE  **
C               **  TREAT THE ...TIC LABEL SIZE          CASE  **
C               **  TREAT THE ...TIC LABEL FORMAT        CASE  **
C               **  TREAT THE ...TIC LABEL CONTENTS      CASE  **
C               **  TREAT THE ...TIC LABEL GAP           CASE  **
C               *************************************************
C
      IF(ICOM.EQ.'XTIC' .OR. ICOM.EQ.'X1TI' .OR.
     1   ICOM.EQ.'X2TI' .OR. ICOM.EQ.'YTIC' .OR.
     1   ICOM.EQ.'Y1TI' .OR. ICOM.EQ.'Y2TI' .OR.
     1   ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.
     1   ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
C
        CALL DPTCTH(ICOM,IHARG,ARG,NUMARG,PDEFTH,PTICTH,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTCCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IDEFCO,IRGBMX,
     1              IX1TCO,IX2TCO,IY1TCO,IY2TCO,
     1              IX1TC2,IX2TC2,IY1TC2,IY2TC2,
     1              IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTCDP(ICOM,IHARG,IARG,NUMARG,IDEFDP,
     1              IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTCJU(ICOM,IHARG,NUMARG,
     1              IX1TJU,IX2TJU,IY1TJU,IY2TJU,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTCSZ(ICOM,IHARG,IARGT,ARG,NUMARG,DEFTL,
     1              PX1TLE,PX2TLE,PY1TLE,PY2TLE,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTCOF(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              DEFTOF,IDEFTU,ITICUN,
     1              PX1TOL,PX2TOL,PY1TOB,PY2TOB,
     1              PX1TOR,PX2TOR,PY1TOT,PY2TOT,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTIC(ICOM,IHARG,NUMARG,
     1             IX1TSW,IX2TSW,IY1TSW,IY2TSW,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
CCCCC   DECEMBER 1992.  FIX BUG.  PDEFHG AND PDEFVG ARE THE DEFAULT
CCCCC                   HORIZONTAL AND VERTICAL GAPS, NOT THE DEFAULT
CCCCC                   DISPLACEMENT.
C
        PJUNK=PDEFDS-0.5
        CALL DPTLDS(ICOM,IHARG,IARGT,ARG,NUMARG,PDEFDS,PJUNK,
     1              PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLFO(ICOM,IHARG,NUMARG,IDEFFO,
     1              IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLCA(ICOM,IHARG,NUMARG,IDEFCA,
     1              IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLJU(ICOM,IHARG,NUMARG,IDEFJU,
     1              IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLDI(ICOM,IHARG,NUMARG,IDEFDI,
     1              IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLFI(ICOM,IHARG,NUMARG,IDEFFI,
     1              IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLTH(ICOM,IHARG,ARG,NUMARG,PDEFTH,PTIZTH,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLAN(ICOM,IHARG,ARG,NUMARG,ADEFAN,
     1              AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IDEFCO,IRGBMX,
     1              IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO,
     1              IX1ZC2,IX2ZC2,IY1ZC2,IY2ZC2,
     1              IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLSZ(ICOM,IHARG,IARGT,ARG,NUMARG,PDEFHE,PDEFWI,
     1              PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
     1              PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
     1              PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
     1              PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLFM(ICOM,IHARG,NUMARG,IDETLF,
     1              IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTLCN(ICOM,IHARG,NUMARG,IANS,IANSLC,IWIDTH,
     1              IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPTL(ICOM,IHARG,NUMARG,
     1            IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW,
     1            IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C     2019/02: ADD TIC MARK LABEL GAP COMMAND
C
CCCCC   CALL DPTLGA(ICOM,IHARG,IARGT,ARG,NUMARG,
CCCCC1              PX1ZGA,PX2ZGA,PY1ZGA,PY2ZGA,
CCCCC1              IFOUND,IERROR)
CCCCC   IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               **********************************
C               **  TREAT THE TITLE FONT  CASE  **
C               **********************************
C
      IF(ICOM.EQ.'TITL')THEN
        CALL DPTIFO(IHARG,NUMARG,IDEFFO,ITITFO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               **********************************
C               **  TREAT THE TITLE CASE  CASE  **
C               **********************************
C
        CALL DPTICA(IHARG,NUMARG,IDEFCA,ITITCA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               **************************************
C               **  TREAT THE TITLE THICKNESS CASE  **
C               **************************************
C
        CALL DPTITH(IHARG,ARG,NUMARG,PDEFTH,PTITTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
CCCCC   THE FOLLOWING SECTION WAS ADDED JULY 1989
C               **************************************
C               **  TREAT THE TITLE DISPLACEMENT CASE  **
C               **************************************
C
        CALL DPTIDS(IHARG,ARG,NUMARG,PDEFDS,PTITDS,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               **********************************
C               **  TREAT THE TITLE COLOR CASE  **
C               **********************************
C
        CALL DPTICL(IHARG,IARG,NUMARG,IDEFCO,IRGBMX,ITITCO,ITITC2,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               **********************************
C               **  TREAT THE TITLE SIZE  CASE  **
C               **********************************
C
        CALL DPTISZ(IHARG,IARGT,ARG,NUMARG,
     1             PDEFHE,PDEFWI,
     1             PTITHE,PTITWI,PTITVG,PTITHG,
     1             IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ****************************
C               **  TREAT THE TITLE CASE  **
C               ****************************
C
        CALL DPTIT(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
     1             ITITTE,NCTITL,ITIAUT,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE NEGATE CASE  **
C               *****************************
C
      IF(ICOM.EQ.'NEGA')THEN
        CALL DPNEGA(IHARG,NUMARG,INEGSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************************
C               **  TREAT THE WINDOW (CORNER) COORDINATES CASE **
C               *************************************************
C
      IF(ICOM.EQ.'WIND')GOTO5400
      GOTO5499
C
 5400 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.
     1IHARG(2).EQ.'COOR')GOTO5411
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')
     1GOTO5430
      GOTO5499
 5411 CONTINUE
      ISHIFT=1
      GOTO5420
 5420 CONTINUE
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      GOTO5430
 5430 CONTINUE
      CALL DPWICC(IHARG,IHARG2,IARGT,ARG,NUMARG,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,
     1PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5499 CONTINUE
C
C               *********************************
C               **  TREAT THE HORIZONTAL CASE  **
C               *********************************
C
      IF(ICOM.EQ.'HORI'.AND.IHARG(1).EQ.'SWIT')THEN
        CALL DPHRIZ(IHARG,NUMARG,IHORSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************************
C               **  TREAT THE MAJOR TIC MARK NUMBER   CASE  **
C               **********************************************
C
      IF(ICOM.EQ.'MAJO')GOTO5800
C  FEBRUARY, 1988: CHECK FOR "MINOR TIC MARK NUMBER"
      IF(ICOM.EQ.'MINO')GOTO5899
C  END CHANGE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GOTO5800
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'NUMB')GOTO5800
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'NUMB')GOTO5800
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'NUMB')GOTO5800
      GOTO5899
C
 5800 CONTINUE
      CALL DPMATN(ICOM,IHARG,IARGT,IARG,NUMARG,
     1IX1JSW,IX2JSW,IY1JSW,IY2JSW,
     1NMJX1T,NMJX2T,NMJY1T,NMJY2T,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
 
 5899 CONTINUE
C
C               **********************************************
C               **  TREAT THE MINOR TIC MARK NUMBER   CASE  **
C               **********************************************
C
      IF(ICOM.EQ.'MINO')GOTO5900
      GOTO5999
C
 5900 CONTINUE
      CALL DPMITN(IHARG,IARGT,IARG,NUMARG,
     1IX1NSW,IX2NSW,IY1NSW,IY2NSW,
     1NMNX1T,NMNX2T,NMNY1T,NMNY2T,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5999 CONTINUE
C
C               *****************************************
C               **  TREAT THE ...TIC LABEL HW    CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'XTIC')GOTO6000
      IF(ICOM.EQ.'X1TI')GOTO6000
      IF(ICOM.EQ.'X2TI')GOTO6000
      IF(ICOM.EQ.'YTIC')GOTO6000
      IF(ICOM.EQ.'Y1TI')GOTO6000
      IF(ICOM.EQ.'Y2TI')GOTO6000
      IF(ICOM.EQ.'TIC')GOTO6000
      IF(ICOM.EQ.'TICS')GOTO6000
      IF(ICOM.EQ.'XYTI')GOTO6000
      IF(ICOM.EQ.'YXTI')GOTO6000
      GOTO6099
C
 6000 CONTINUE
      CALL DPTLHW(ICOM,IHARG,IARGT,ARG,NUMARG,
     1PDEFHE,PDEFWI,
     1PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
     1PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
     1PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
     1PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 6099 CONTINUE
C
C               ********************************************
C               **  TREAT THE MAJOR TIC COORDINATES CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'MAJO')THEN
        CALL DPMJTC(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              IX1TSW,IX2TSW,IY1TSW,IY2TSW,
     1              X1COOR,X2COOR,Y1COOR,Y2COOR,
     1              NX1COO,NX2COO,NY1COO,NY2COO,
     1              MAXTIC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************************
C               **  TREAT THE MINOR TIC COORDINATES CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'MINO')THEN
        CALL DPMNTC(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              X1COMN,X2COMN,Y1COMN,Y2COMN,
     1              NX1CMN,NX2CMN,NY1CMN,NY2CMN,
     1              MAXTIC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***********************************
C               **  TREAT THE FILL  COLORS CASE  **
C               ***********************************
C
CCCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'COLO')GOTO6500
CCCCC GOTO6599
C
C6500 CONTINUE
CCCCC CALL DPFICO(IHARG,NUMARG,IDEFFC,MAXFIL,IFILCO,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6599 CONTINUE
C
C               ***********************************
C               **  TREAT THE FILL SPACING CASE  **
C               ***********************************
C
CCCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'SPAC')GOTO6600
CCCCC GOTO6699
C
C6600 CONTINUE
CCCCC CALL DPFISP(IHARG,IARGT,ARG,NUMARG,PDPFFG,MAXFIL,PFILSP,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6699 CONTINUE
C
C               *************************************
C               **  TREAT THE FILL THICKNESS CASE  **
C               *************************************
C
CCCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'THIC')GOTO6700
CCCCC GOTO6799
C
C6700 CONTINUE
CCCCC CALL DPFITH(IHARG,IARGT,ARG,NUMARG,PDEFFT,MAXFIL,PFILTH,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6799 CONTINUE
C
C               ********************************
C               **  TREAT THE FILL  BASE CASE **
C               ********************************
C
CCCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'BASE')GOTO6800
CCCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'REFE')GOTO6800
CCCCC1 GOTO6899
C
C6800 CONTINUE
CCCCC CALL DPFIBA(IHARG,IARGT,ARG,NUMARG,ADEFFB,MAXFIL,AFILBA,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6899 CONTINUE
C
C               ***********************************
C               **  TREAT THE FILL (SWITCH) CASE **
C               ***********************************
C
CCCCC IF(ICOM.EQ.'FILL')GOTO6900
CCCCC GOTO6999
C
C6900 CONTINUE
CCCCC IF(IHARG(1).EQ.'ON')GOTO6910
CCCCC IF(IHARG(2).EQ.'ON')GOTO6910
CCCCC IF(IHARG(1).EQ.'OFF')GOTO6910
CCCCC IF(IHARG(2).EQ.'OFF')GOTO6910
CCCCC GOTO6999
C6910 CONTINUE
CCCCC CALL DPFISW(IHARG,NUMARG,IDEFFS,MAXFIL,IFILSW,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6999 CONTINUE
C
C               *************************************
C               **  TREAT THE FILL (PATTERN) CASE  **
C               *************************************
C
CCCCC IF(ICOM.EQ.'FILL')GOTO7000
CCCCC GOTO7099
C
C7000 CONTINUE
CCCCC CALL DPFIPA(IHARG,NUMARG,IDEFFP,MAXFIL,IFILPA,
CCCCC1IBUGP2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C7099 CONTINUE
C
C               *************************************
C               **  TREAT THE PATTERN LINE   CASE  **
C               *************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'LINE')GOTO7100
      GOTO7199
C
 7100 CONTINUE
      CALL DPPALI(IHARG,NUMARG,IDEFPL,MAXPAT,IPATLI,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7199 CONTINUE
C
C               **************************************
C               **  TREAT THE PATTERN SPACING CASE  **
C               **************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'SPAC')GOTO7200
      GOTO7299
C
 7200 CONTINUE
      CALL DPPASP(IHARG,IARGT,ARG,NUMARG,
     1            PDEFPG,MAXPAT,PPATSP,
     1            IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7299 CONTINUE
C
C               ****************************************
C               **  TREAT THE PATTERN THICKNESS CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'THIC')GOTO7300
      GOTO7399
C
 7300 CONTINUE
      CALL DPPATH(IHARG,IARGT,ARG,NUMARG,PDEFPT,MAXPAT,PPATTH,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7399 CONTINUE
C
C               ****************************************
C               **  TREAT THE PATTERN HEIGHT    CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'HEIG')GOTO7400
      GOTO7499
C
 7400 CONTINUE
      CALL DPPAHE(IHARG,IARGT,ARG,NUMARG,PDEFPH,MAXPAT,PPATHE,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7499 CONTINUE
C
C               ****************************************
C               **  TREAT THE PATTERN WIDTH     CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'WIDT')GOTO7500
      GOTO7599
C
 7500 CONTINUE
      CALL DPPAWI(IHARG,IARGT,ARG,NUMARG,PDEFPW,MAXPAT,PPATWI,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7599 CONTINUE
C
C               *************************************
C               **  TREAT THE PATTERN COLOR  CASE  **
C               *************************************
C
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'COLO')GOTO7600
      GOTO7699
C
 7600 CONTINUE
      CALL DPPACO(IHARG,NUMARG,IDEFPC,MAXPAT,IPATCO,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7699 CONTINUE
C
C               **************************************
C               **  TREAT THE PATTERN (SWITCH) CASE **
C               **************************************
C
      IF(ICOM.EQ.'PATT')GOTO7700
      GOTO7799
C
 7700 CONTINUE
      IF(IHARG(1).EQ.'ON')GOTO7710
      IF(IHARG(2).EQ.'ON')GOTO7710
      IF(IHARG(1).EQ.'OFF')GOTO7710
      IF(IHARG(2).EQ.'OFF')GOTO7710
      GOTO7799
 7710 CONTINUE
      CALL DPPASW(IHARG,NUMARG,IDEFPS,MAXPAT,IPATSW,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7799 CONTINUE
C
C               ****************************************
C               **  TREAT THE PATTERN (PATTERN) CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'PATT')GOTO7800
      GOTO7899
C
 7800 CONTINUE
      CALL DPPAPA(IHARG,NUMARG,IDEFPP,MAXPAT,IPATPA,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7899 CONTINUE
C
C               **************************************
C               **  TREAT THE SPIKE COLORS CASE     **
C               **  TREAT THE SPIKE THICKNESS CASE  **
C               **  TREAT THE SPIKE LINE      CASE  **
C               **  TREAT THE SPIKE BASE CASE       **
C               **  TREAT THE SPIKE DIRECTION CASE  **
C               **  TREAT THE SPIKE (SWITCH) CASE   **
C               **  TREAT THE SPIKE (PATTERN) CASE  **
C               **        (SAME AS SPIKE LINES CASE)**
C               **************************************
C
      IF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'COLO')THEN
        ICASCL='STAN'
        CALL DPSPCO(IHARG,IARG,NUMARG,IDEFSC,MAXSPI,ISPICO,
     1              ISPIC2,IRGBMX,ICASCL,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'RGB '.AND.
     1       IHARG(2).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGP2,IERROR)
        IHARG(1)='COLO'
        IHARG2(1)='    '
        CALL DPSPCO(IHARG,IARG,NUMARG,IDEFSC,MAXSPI,ISPICO,
     1              ISPIC2,IRGBMX,ICASCL,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'THIC')THEN
        CALL DPSPTH(IHARG,IARGT,ARG,NUMARG,PDEFST,MAXSPI,PSPITH,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'LINE')THEN
        CALL DPSPLI(IHARG,IHARG2,NUMARG,IDEFSL,MAXSPI,ISPILI,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.
     1      (IHARG(1).EQ.'BASE' .OR. IHARG(1).EQ.'REFE'))THEN
        CALL DPSPBA(ADEFSB,MAXSPI,ASPIBA,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'DIRE')THEN
        CALL DPSPDI(IHARG,NUMARG,IDEFSD,MAXSPI,ISPIDI,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'SPIK')THEN
        IF(IHARG(1).EQ.'ON' .OR. IHARG(2).EQ.'ON' .OR.
     1     IHARG(1).EQ.'OFF' .OR. IHARG(2).EQ.'OFF')THEN
          CALL DPSPSW(IHARG,NUMARG,IDEFSS,MAXSPI,ISPISW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSE
          CALL DPSPPA(IHARG,IHARG2,NUMARG,IDEFSL,MAXSPI,ISPILI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               ***************************************
C               **  TREAT THE SUBREGION (SWITCH) CASE**
C               ***************************************
C
      IF(ICOM.EQ.'SUBR')THEN
        IF(IHARG(1).EQ.'ON'.OR.IHARG(2).EQ.'ON'.OR.
     1     IHARG(1).EQ.'OFF'.OR.IHARG(2).EQ.'OFF')THEN
          CALL DPSBSW(IHARG,NUMARG,IDEFSB,MAXSUB,ISUBSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               ***************************************
C               **  TREAT THE SUBREGION LIMITS   CASE**
C               ***************************************
C
      IF(ICOM.EQ.'SUBR')THEN
        CALL DPSBLI(ICOM,IHARG,IARGT,ARG,NUMARG,
     1              ASUBXL,ASUBXU,ASUBYL,ASUBYU,
     1              MAXSUB,
     1              IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C
C               ***************************
C               **  TREAT THE MINOR GRID CASE  **
C               ***************************
C
      IF(ICOM.EQ.'XGMI')GOTO8800
      IF(ICOM.EQ.'YGMI')GOTO8800
      IF(ICOM.EQ.'XYGM')GOTO8800
      IF(ICOM.EQ.'YXGM')GOTO8800
      IF(ICOM.EQ.'GMIN')GOTO8800
      IF(ICOM.EQ.'MINO')GOTO8800
      GOTO8899
C
 8800 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO8899
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GOTO8899
      CALL DPGRMN(ICOM,IHARG,NUMARG,IVGMSW,IHGMSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8899 CONTINUE
C
C               ****************************************************
C               **  TREAT THE MULTIPLOT (CORNER) COORDINATES CASE **
C               ****************************************************
C
      IF((ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'CORN') .OR.
     1   (ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'COOR') .OR.
     1   (ICOM.EQ.'MP  ' .AND. IHARG(1).EQ.'CORN') .OR.
     1   (ICOM.EQ.'MP  ' .AND. IHARG(1).EQ.'COOR'))THEN
C
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.
     1     IHARG(2).EQ.'COOR')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
        ENDIF
C
        CALL DPMUCC(IHARG,IHARG2,IARGT,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1              NUMNAM,MAXNAM,IANS,IWIDTH,
     1              PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               ****************************************************
C               **  TREAT THE    END OF MULTIPLOT    CASE         **
C               ****************************************************
C
      IF(ICOM.EQ.'END')GOTO9200
CCCCC THE FOLLOWING 2 LINES WERE ADDED AUGUST 1990
      IF(ICOM.EQ.'EOMP')GOTO9210
      IF(ICOM.EQ.'EMP')GOTO9210
      GOTO9299
C
 9200 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'OF'.AND.
     1IHARG(2).EQ.'MULT')GOTO9210
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'MULT')
     1GOTO9210
      GOTO9299
 9210 CONTINUE
      CALL DPENMU(IMPSW,
     1IERASV,
     1PWXMIS,PWXMAS,PWYMIS,PWYMAS,
     1IERASW,
     1PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9299 CONTINUE
C
C               ****************************************************
C               **  TREAT THE MULTIPLOT CASE                      **
C               ****************************************************
C
      IF(ICOM.EQ.'MULT' .OR. ICOM.EQ.'MP')GOTO9300
      GOTO9399
C
 9300 CONTINUE
      IF(ICOM2.EQ.'IPLE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ESD ')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FREQ')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'FREQ')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'FREQ')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'FREQ')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KERN')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KERN')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KERN')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KERN')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LORE')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'LORE')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'LORE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ADJA')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ADJA')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ADJA')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ALLA')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ALLA')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AV')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AV')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AS')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AS')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ASD')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ASD')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SPEC')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PERI')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AUTO')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PART')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CO  ')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COSP')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'QUAD')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CROS')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COHE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AMPL')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PHAS')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GAIN')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ARGA')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TAIL')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SURV')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIFF')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LEVE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COCH')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROP')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANOP')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANAL')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'COMM' .AND.
     1   IHARG(2).EQ.'WEIB')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'EMPI' .AND.
     1   IHARG(2).EQ.'QUAN')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CUMU' .AND.
     1   IHARG(2).EQ.'SUM')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LJUN' .AND.
     1   IHARG(2).EQ.'BOX')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BOX' .AND.
     1   IHARG(2).EQ.'COX')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'BOX')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'BOX')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SYMM')GOTO9399
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'I')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'I')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEAN'.AND.
     1   IHARG(2).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'MEAN'.AND.
     1   IHARG(3).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEDI'.AND.
     1   IHARG(2).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'MEDI'.AND.
     1   IHARG(3).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'TRIM'.AND.
     1   IHARG(2).EQ.'MEAN')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'TRIM'.AND.
     1   IHARG(3).EQ.'MEAN')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BIWE'.AND.
     1   IHARG(2).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'BIWE'.AND.
     1   IHARG(3).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'QUAN'.AND.
     1   IHARG(2).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'QUAN'.AND.
     1   IHARG(3).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE '.AND.
     1   IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'ERRO')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ONE '.AND.
     1   IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'ERRO')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TWO '.AND.
     1   IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'ERRO')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'TWO '.AND.
     1   IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'ERRO')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE '.AND.
     1   IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'DEVI')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ONE '.AND.
     1   IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'DEVI')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TWO '.AND.
     1   IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'DEVI')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'TWO '.AND.
     1   IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'DEVI')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM'.AND.
     1   IHARG(2).EQ.'TOLE')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'NORM'.AND.
     1   IHARG(3).EQ.'TOLE')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM'.AND.
     1   IHARG(2).EQ.'PRED')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'NORM'.AND.
     1   IHARG(3).EQ.'PRED')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'AGRE'.AND.
     1   IHARG(2).EQ.'COUL')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AGRE'.AND.
     1   IHARG(3).EQ.'COUL')GOTO9399
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'VIOL')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'VIOL')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HOMO')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'PERC'.AND.
     1   IHARG(2).EQ.'POIN'.AND.IHARG(3).EQ.'PLOT')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'BEST' .AND.
     1   IHARG(2).EQ.'DIST'.AND.IHARG(3).EQ.'FIT')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RUN ' .AND.
     1   IHARG(2).EQ.'SEQU')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'KRUS' .AND.
     1   IHARG(2).EQ.'WALL')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SQUA' .AND.
     1   IHARG(2).EQ.'RANK')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEDI' .AND.
     1   IHARG(2).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANDE' .AND.
     1   IHARG(2).EQ.'DARL')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'VAN ' .AND.
     1   IHARG(2).EQ.'DER '.AND.IHARG(3).EQ.'WAER')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE ' .AND.
     1   IHARG(2).EQ.'WAY '.AND.IHARG(3).EQ.'NORM')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WILK' .AND.
     1   IHARG(2).EQ.'SHAP')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SHAP' .AND.
     1   IHARG(2).EQ.'WILK')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM' .AND.
     1   IHARG(2).EQ.'TOLE')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NONP' .AND.
     1   IHARG(2).EQ.'TOLE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TOLE')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BEST' .AND.
     1   IHARG(2).EQ.'DIST')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'T   '.AND.
     1   IHARG(2).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'T   '.AND.
     1   IHARG(3).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(3).EQ.'T   '.AND.
     1   IHARG(4).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.5.AND.IHARG(4).EQ.'T   '.AND.
     1   IHARG(5).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.6.AND.IHARG(5).EQ.'T   '.AND.
     1   IHARG(6).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BART'.AND.
     1   IHARG(2).EQ.'TEST')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DM  '.AND.
     1   IHARG(2).EQ.'BART')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIXO' .AND.
     1   IHARG(2).EQ.'MASS'.AND.IHARG(3).EQ.'BART')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'F   '.AND.
     1   IHARG(2).EQ.'LOC ')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SUMM')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CAPA')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'RUNS')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'JARQ'.AND.
     1   IHARG(2).EQ.'BERA')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PRED')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LOWE')GOTO9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'UPPE')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SD  ' .AND.
     1   IHARG(2).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'STAN' .AND.
     1   IHARG(2).EQ.'DEVI'.AND.IHARG(3).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'STAN' .AND.
     1   IHARG(2).EQ.'DEVI'.AND.IHARG(3).EQ.'PRED')GOTO9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ONE ' .AND.
     1   IHARG(2).EQ.'SIDE')GOTO9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'DIST' .AND.
     1   IHARG(2).EQ.'FIT '.AND.IHARG(3).EQ.'PLOT')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.
     1   IHARG(2).EQ.'OF  '.AND. IHARG(3).EQ.'VARI' .AND.
     1   IHARG(4).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.
     1   IHARG(2).EQ.'OF  '.AND. IHARG(3).EQ.'DISP' .AND.
     1   IHARG(4).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.
     1   IHARG(2).EQ.'OF  '.AND.IHARG(3).EQ.'QUAR' .AND.
     1   IHARG(4).EQ.'DISP'.AND.IHARG(5).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.
     1   IHARG(2).EQ.'OF  '.AND.IHARG(3).EQ.'QUAR' .AND.
     1   IHARG(4).EQ.'VARI'.AND.IHARG(5).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'QUAR' .AND.
     1   IHARG(2).EQ.'COEF'.AND.IHARG(3).EQ.'OF  ' .AND.
     1   IHARG(4).EQ.'DISP'.AND.IHARG(5).EQ.'CONF')GOTO9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'QUAR' .AND.
     1   IHARG(2).EQ.'COEF'.AND.IHARG(3).EQ.'OF  ' .AND.
     1   IHARG(4).EQ.'VARI'.AND.IHARG(5).EQ.'CONF')GOTO9399
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.
     1IHARG2(1).EQ.'    ')
     1GOTO9311
      GOTO9330
 9311 CONTINUE
      ISHIFT=1
      GOTO9320
 9320 CONTINUE
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      GOTO9330
 9330 CONTINUE
      CALL DPMULT(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,
     1IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
CCCCC ADD FOLLOWING LINE.  AUGUST 1999.
     1IMPARG,
CCCCC ADD FOLLOWING LINE.  SEPTEMBER 1998.
     1AMPSCH,AMPSCW,
     1PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1IERASW,
     1PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1IERASV,
     1PWXMIS,PWXMAS,PWYMIS,PWYMAS,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9399 CONTINUE
C
C               ****************************************************
C               **  TREAT THE EMBED     CASE                      **
C               ****************************************************
C
      IF(ICOM.EQ.'EMBE')THEN
        CALL DPEMBE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,ICOM,IERASW,
     1              IEMBSW,IEMCNT,PEMXC1,PEMXC2,PEMYC1,PEMYC2,
     1              PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1990
C               *************************************
C               **  TREAT THE WINDOW SYSTEM   CASE **
C               *************************************
C
CCCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'SYST')GOTO11100
CCCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'MANA')GOTO11100
CCCCC  GOTO11199
C
C11100 CONTINUE
CCCCC  CALL DPWISY(IHARG,NUMARG,IDEFWS,IWINSY,
CCCCC 1IBUGP2,IFOUND,IERROR)
CCCCC  IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C11199 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1990
C               *************************************
C               **  TREAT THE WINDOW POINTER CASE  **
C               *************************************
C
CCCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'POIN')GOTO11200
CCCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'SELE')GOTO11200
CCCCC  GOTO11299
C
C11200 CONTINUE
CCCCC  CALL DPWIPO(IHARG,NUMARG,IDEFWP,IWINPO,
CCCCC 1IBUGP2,IFOUND,IERROR)
CCCCC  IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ***********************************
C               **  TREAT THE PIXMAP TITLE CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'PIXM'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'TITL')THEN
        CALL DPPMTI(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAIPC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAIPC3(IBUGPC,IBUGP2,ISUBRO,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  IERASV,
     1                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1992
     1                  BARHEF,BARWEF,IRGBMX,
     1                  IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAIPC3.
C              (THE   PC    AT THE END OF    MAIPC3   STANDS FOR PLOT CONTROL
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
C              PLOT CONTROL COMMANDS (PART 3).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
C     UPDATED        --APRIL  1992.  BAR EXPANSION FACTORS ... ...
C     UPDATED        --OCTOBER1993.  ARGUMENTS TO BAR BASE (DPBABA)
C     UPDATED        --OCTOBER1993.  ARGUMENTS TO REGION BASE (DPREBA)
C     UPDATED        --MARCH  1994.  ARGUMENTS TO REGION BASE (DPREBA)
C     UPDATED        --AUGUST    1995. DASH2 BUG (VARIOUS)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
C
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 ICASCL
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAIPC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV
   53   FORMAT('IBUGPC,IBUGP2,ISUBRO,IANGLU,IERASV = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ICOM,ICOM2,NUMARG
   67   FORMAT('ICOM,ICOM2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IMPSW,IMPNR,IMPNC,IMPCO
   81   FORMAT('IMPSW,IMPNR,IMPNC,IMPCO = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)PMXMIN,PMXMAX,PMYMIN,PMYMAX
   82   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)PWXMIS,PWXMAS,PWYMIS,PWYMAS
   84   FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   86   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)PXMIN,PXMAX,PYMIN,PYMAX
   87   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,88)IVGMSW,IHGMSW
   88   FORMAT('IVGMSW,IHGMSW = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
C               *****************************************
C               **  TREAT THE ORIENTATION SWITCH CASE  **
C               *****************************************
C
C
      IF(ICOM.EQ.'ORIE')THEN
        CALL DPORSW(IHARG,NUMARG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C     ----------BARS--------------------------------------------------
C
C               ******************************************
C               **  STEP XX--                           **
C               **  TREAT THE VARIOUS BAR ... COMMANDS  **
C               ******************************************
C
C               **********************************************
C               **  TREAT THE BAR PATTERN LINE (TYPE)  CASE **
C               **  TREAT THE BAR PATTERN COLOR        CASE **
C               **  TREAT THE BAR PATTERN THICKNESS    CASE **
C               **  TREAT THE BAR PATTERN SPACING      CASE **
C               **  TREAT THE BAR PATTERN (TYPE)       CASE **
C               **  TREAT THE BAR FILL COLOR           CASE **
C               **  TREAT THE BAR FILL (SWITCH)        CASE **
C               **  TREAT THE BAR BORDER COLOR         CASE **
C               **  TREAT THE BAR BORDER THICKNESS     CASE **
C               **  TREAT THE BAR BORDER LINE (TYPE)   CASE **
C               **  TREAT THE BAR WIDTH                CASE **
C               **  TREAT THE BAR BASE                 CASE **
C               **  TREAT THE BAR (SWITCH)             CASE **
C               **  TREAT THE BAR DIMENSION            CASE **
C               **  TREAT THE BAR DIRECTION            CASE **
C               **  TREAT THE BAR EXPANSION FACTORS   CASE **
C               **        (USED ONLY BY BLOCK PLOT COMMAND)**
C               **********************************************
C
      IF(ICOM.EQ.'BAR')THEN
        IF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE')THEN
          IF(IHARG(3).NE.'TYPE')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
            IHARG(3)='TYPE'
            IHARG2(3)='    '
          ENDIF
C
          CALL DPBPLI(IHARG,IHARG2,NUMARG,IDEBPL,MAXBAR,IBAPLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBPCO(IHARG,IARG,NUMARG,IDEBPC,ICASCL,MAXBAR,IRGBMX,
     1                IBAPCO,IBAPC2,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'RGB '.AND.
     1         IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='PATT'
          IHARG2(1)='ERN '
          IHARG(2)='COLO'
          IHARG2(2)='R   '
          CALL DPBPCO(IHARG,IARG,NUMARG,IDEBPC,ICASCL,MAXBAR,IRGBMX,
     1                IBAPCO,IBAPC2,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPBPTH(IHARG,IARGT,ARG,NUMARG,PDEBPT,MAXBAR,PBAPTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'SPAC')THEN
          CALL DPBPSP(IHARG,IARGT,ARG,NUMARG,PDEBPS,MAXBAR,PBAPSP,
     1                IBUGP2,IFOUND,IERROR)
        ELSEIF(IHARG(1).EQ.'SPAC')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='PATT'
          IHARG2(2)='ERN '
          CALL DPBPSP(IHARG,IARGT,ARG,NUMARG,PDEBPS,MAXBAR,PBAPSP,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'TYPE')THEN
          CALL DPBPTY(IHARG,NUMARG,IDEBPT,MAXBAR,IBAPTY,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(ICOM.EQ.'BAR'.AND.IHARG(1).EQ.'PATT')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPBPTY(IHARG,NUMARG,IDEBPT,MAXBAR,IBAPTY,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'COLO')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          ICASCL='STAN'
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,
     1                ICASCL,IBAFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,
     1                ICASCL,IBAFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,
     1                ICASCL,IBAFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'RGB '.AND.
     1         IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,
     1                ICASCL,IBAFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'SWIT')THEN
          CALL DPBFSW(IHARG,NUMARG,IDEBFS,MAXBAR,IBAFSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='SWIT'
          IHARG2(2)='CH  '
          CALL DPBFSW(IHARG,NUMARG,IDEBFS,MAXBAR,IBAFSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBBCO(IHARG,IARG,NUMARG,IDEBBC,ICASCL,MAXBAR,IRGBMX,
     1                IBABCO,IBABC2,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'RGB '.AND.
     1         IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='BORD'
          IHARG2(1)='ER  '
          IHARG(2)='COLO'
          IHARG2(2)='R   '
          CALL DPBBCO(IHARG,IARG,NUMARG,IDEBBC,ICASCL,MAXBAR,IRGBMX,
     1                IBABCO,IBABC2,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPBBTH(IHARG,IARGT,ARG,NUMARG,PDEBBT,MAXBAR,PBABTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'LINE'.AND.
     1         IHARG(3).EQ.'TYPE')THEN
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'BORD'.AND.
     1        (IHARG(2).EQ.'TYPE' .OR. IHARG(2).EQ.'LINE'))THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'BORD')THEN
          ISHIFT=2
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'WIDT')THEN
          CALL DPBAWI(ADEBWI,MAXBAR,ABARWI,IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BASE'.OR.IHARG(1).EQ.'REFE')THEN
          CALL DPBABA(ADEBBA,MAXBAR,ABARBA,IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'DIME')THEN
          CALL DPBATY(IHARG,NUMARG,IDEBTY,MAXBAR,IBARTY,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'DIRE')THEN
          CALL DPBADI(IHARG,NUMARG,IDEBDI,MAXBAR,IBARDI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'EXPA' .OR. IHARG(1).EQ.'FACT')THEN
          CALL DPBAEF(IHARG,IARGT,ARG,NUMARG,BARHEF,BARWEF,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'SWIT')THEN
          CALL DPBASW(IHARG,NUMARG,IDEBSW,MAXBAR,IBARSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(ICOM.EQ.'BAR')THEN
          IF(IHARG(1).EQ.'ON'  .OR. IHARG(2).EQ.'ON' .OR.
     1       IHARG(1).EQ.'OFF' .OR. IHARG(2).EQ.'OFF')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
            IHARG(1)='SWIT'
            IHARG2(1)='CH  '
          ENDIF
          CALL DPBASW(IHARG,NUMARG,IDEBSW,MAXBAR,IBARSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ENDIF
      ENDIF
C
C               **********************************
C               **  END POINT FOR BAR COMMANDS  **
C               **********************************
C
C     ----------END OF BARS---------------------------------------
C
C     ----------REGIONS-----------------------------------------------
C
C               ********************************************
C               **  STEP XX--                             **
C               **  TREAT THE VARIOUS REGION ... COMMANDS **
C               ********************************************
C
      IF(ICOM.EQ.'REGI' .AND. ICOM2.EQ.'S   ')GOTO21799
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'PATT')THEN
C
C               ********************************************************
C               **  TREAT THE REGION PATTERN LINE (TYPE)         CASE **
C               **  TREAT THE REGION PATTERN COLOR               CASE **
C               **  TREAT THE REGION PATTERN THICKNESS           CASE **
C               **  TREAT THE REGION PATTERN SPACING             CASE **
C               **  TREAT THE REGION PATTERN (TYPE)              CASE **
C               ********************************************************
C
        IF(IHARG(2).EQ.'LINE')THEN
          IF(IHARG(2).EQ.'LINE' .AND. IHARG(3).NE.'TYPE')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
            IHARG(3)='TYPE'
            IHARG2(3)='    '
          ENDIF
          CALL DPRPLI(IHARG,IHARG2,NUMARG,IDERPL,MAXREG,IREPLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRPCO(IHARG,IARG,NUMARG,IDERPC,MAXREG,IREPCO,
     1                ICASCL,IREPC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'RGB ' .OR. IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='PATT'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPRPCO(IHARG,IARG,NUMARG,IDERPC,MAXREG,IREPCO,
     1                ICASCL,IREPC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'THIC')THEN
          CALL DPRPTH(IHARG,IARGT,ARG,NUMARG,PDERPT,MAXREG,PREPTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'SPAC')THEN
          CALL DPRPSP(IHARG,IARGT,ARG,NUMARG,PDERPS,MAXREG,PREPSP,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'TYPE')THEN
          CALL DPRPTY(IHARG,NUMARG,IDERPT,MAXREG,IREPTY,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).NE.'TYPE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPRPTY(IHARG,NUMARG,IDERPT,MAXREG,IREPTY,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'COLO')THEN
        ICASCL='STAN'
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGP2,IERROR)
        IHARG(1)='FILL'
        IHARG2(1)='    '
        IHARG(2)='COLO'
        IHARG2(2)='    '
        CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,
     1              ICASCL,IREFC2,IRGBMX,
     1              IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'FILL')THEN
C
C               *******************************************
C               **  TREAT THE REGION FILL COLOR  CASE    **
C               **  TREAT THE REGION FILL (SWITCH) CASE  **
C               *******************************************
C
        IF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,
     1                ICASCL,IREFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(2).EQ.'RGB '.AND.IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,
     1                ICASCL,IREFC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'SWIT')THEN
          CALL DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ENDIF
C
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGP2,IERROR)
        IHARG(1)='FILL'
        IHARG2(1)='    '
        IHARG(2)='SWIT'
        IHARG2(2)='CH  '
        CALL DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE REGION BORDER COLOR      CASE   **
C               **  TREAT THE REGION BORDER THICKNESS  CASE   **
C               **  TREAT THE REGION BORDER LINE (TYPE)  CASE **
C               ************************************************
C
      IF(ICOM.EQ.'REGI'.AND.IHARG(1).EQ.'BORD')THEN
        IF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRBCO(IHARG,IARG,NUMARG,IDERBC,MAXREG,IREBCO,
     1                ICASCL,IREBC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'RGB ' .AND. IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='BORD'
          IHARG2(1)='ER  '
          CALL DPRBCO(IHARG,IARG,NUMARG,IDERBC,MAXREG,IREBCO,
     1                ICASCL,IREBC2,IRGBMX,
     1                IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'THIC')THEN
          CALL DPRBTH(IHARG,IARGT,ARG,NUMARG,PDERBT,MAXREG,PREBTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')THEN
          CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(2).EQ.'TYPE' .OR. IHARG(2).EQ.'LINE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGP2,IERROR)
        IHARG(2)='LINE'
        IHARG2(2)='    '
        IHARG(3)='TYPE'
        IHARG2(3)='    '
        CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***********************************
C               **  TREAT THE REGION   BASE CASE **
C               ***********************************
C
      IF(ICOM.EQ.'REGI' .AND.
     1  (IHARG(1).EQ.'BASE' .OR. IHARG(1).EQ.'REFE'))THEN
C
        CALL DPREBA(ADERBA,MAXREG,AREGBA,IREBIN,IREBPL,
     1              IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  END POINT FOR REGION COMMANDS  **
C               *************************************
C
21799 CONTINUE
C
C     ----------END OF REGIONS------------------------------------------
C
C     ----------MARKERS-------------------------------------------------
C
C               *********************************************
C               **  STEP XX--                              **
C               **  TREAT THE VARIOUS MARKER ... COMMANDS  **
C               *********************************************
C
      IF(ICOM.EQ.'MARK')GOTO31000
      GOTO32999
31000 CONTINUE
C
C               *************************************************
C               **  TREAT THE MARKER PATTERN LINE (TYPE)  CASE **
C               *************************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')GOTO31120
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'LINE')GOTO31100
      GOTO31199
C
31100 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(3)='TYPE'
      IHARG2(3)='    '
31120 CONTINUE
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC CALL DPMPLI(IHARG,NUMARG,IDEMPL,MAXMAR,IMAPLI,
      CALL DPMPLI(IHARG,IHARG2,NUMARG,IDEMPL,MAXMAR,IMAPLI,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31199 CONTINUE
C
C               *******************************************
C               **  TREAT THE MARKER PATTERN COLOR  CASE **
C               *******************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'COLO')GOTO31200
      GOTO31299
C
31200 CONTINUE
      CALL DPMPCO(IHARG,NUMARG,IDEMPC,MAXMAR,IMAPCO,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31299 CONTINUE
C
C               ***********************************************
C               **  TREAT THE MARKER PATTERN THICKNESS  CASE **
C               ***********************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'THIC')GOTO31300
      GOTO31399
C
31300 CONTINUE
      CALL DPMPTH(IHARG,IARGT,ARG,NUMARG,PDEMPT,MAXMAR,PMAPTH,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31399 CONTINUE
C
C               ***********************************************
C               **  TREAT THE MARKER PATTERN SPACING    CASE **
C               ***********************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'SPAC')GOTO31400
      GOTO31499
C
31400 CONTINUE
      CALL DPMPSP(IHARG,IARGT,ARG,NUMARG,PDEMPS,MAXMAR,PMAPSP,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31499 CONTINUE
C
C               **********************************************
C               **  TREAT THE MARKER PATTERN (TYPE)  CASE   **
C               **********************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.
     1IHARG(2).EQ.'TYPE')GOTO31520
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT')GOTO31500
      GOTO31599
C
31500 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(2)='TYPE'
      IHARG2(2)='    '
31520 CONTINUE
      CALL DPMPTY(IHARG,NUMARG,IDEMPT,MAXMAR,IMAPTY,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31599 CONTINUE
C
C               ****************************************
C               **  TREAT THE MARKER FILL COLOR  CASE **
C               ****************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL'.AND.
     1IHARG(2).EQ.'COLO')GOTO31750
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'COLO')GOTO31710
      GOTO31799
C
31710 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(1)='FILL'
      IHARG2(1)='    '
      IHARG(2)='COLO'
      IHARG2(2)='    '
      GOTO31750
C
31750 CONTINUE
      CALL DPMFCO(IHARG,NUMARG,IDEMFC,MAXMAR,IMAFCO,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31799 CONTINUE
C
C               *******************************************
C               **  TREAT THE MARKER FILL (SWITCH) CASE  **
C               *******************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL'.AND.
     1IHARG(2).EQ.'SWIT')GOTO31820
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL')GOTO31800
      GOTO31899
C
31800 CONTINUE
      IF(IHARG(1).EQ.'ON')GOTO31810
      IF(IHARG(2).EQ.'ON')GOTO31810
      IF(IHARG(1).EQ.'OFF')GOTO31810
      IF(IHARG(2).EQ.'OFF')GOTO31810
      GOTO31899
31810 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(2)='SWIT'
      IHARG2(2)='CH  '
31820 CONTINUE
      CALL DPMFSW(IHARG,NUMARG,IDEMFS,MAXMAR,IMAFSW,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
31899 CONTINUE
C
C               ******************************************
C               **  TREAT THE MARKER BORDER COLOR  CASE **
C               ******************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.
     1IHARG(2).EQ.'COLO')GOTO32100
      GOTO32199
C
32100 CONTINUE
      CALL DPMBCO(IHARG,NUMARG,IDEMBC,MAXMAR,IMABCO,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
32199 CONTINUE
C
C               **********************************************
C               **  TREAT THE MARKER BORDER THICKNESS  CASE **
C               **********************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.
     1IHARG(2).EQ.'THIC')GOTO32200
      GOTO32299
C
32200 CONTINUE
      CALL DPMBTH(IHARG,IARGT,ARG,NUMARG,PDEMBT,MAXMAR,PMABTH,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
32299 CONTINUE
C
C               **************************************************
C               **  TREAT THE MARKER BORDER LINE (TYPE)  CASE   **
C               **************************************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.
     1IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')GOTO32330
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.
     1IHARG(2).EQ.'TYPE')GOTO32320
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.
     1IHARG(2).EQ.'LINE')GOTO32320
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD')GOTO32310
      GOTO32399
C
32310 CONTINUE
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(2)='LINE'
      IHARG2(2)='    '
      IHARG(3)='TYPE'
      IHARG2(3)='    '
      GOTO32330
C
32320 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGP2,IERROR)
      IHARG(2)='LINE'
      IHARG2(2)='    '
      IHARG(3)='TYPE'
      IHARG2(3)='    '
      GOTO32330
C
32330 CONTINUE
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC CALL DPMBLI(IHARG,NUMARG,IDEMBL,MAXMAR,IMABLI,
      CALL DPMBLI(IHARG,IHARG2,NUMARG,IDEMBL,MAXMAR,IMABLI,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
32399 CONTINUE
C
C               ***********************************
C               **  TREAT THE MARKER   BASE CASE **
C               ***********************************
C
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BASE')GOTO32600
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'REFE')GOTO32600
      GOTO32699
C
32600 CONTINUE
      CALL DPMABA(IHARG,IARGT,ARG,NUMARG,ADEMBA,MAXMAR,AMARBA,
     1IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
32699 CONTINUE
C
C               *************************************
C               **  END POINT FOR MARKER COMMANDS  **
C               *************************************
C
32999 CONTINUE
C
C     ----------END OF MARKERS---------------------------------------
C
C     ----------TEXTS--------------------------------------------------
C
C               ***********************************************
C               **  STEP XX--                                **
C               **  TREAT THE VARIOUS TEXT ... COMMANDS      **
C               **  TREAT THE TEXT PATTERN LINE (TYPE)  CASE **
C               **  TREAT THE TEXT PATTERN COLOR        CASE **
C               **  TREAT THE TEXT PATTERN THICKNESS    CASE **
C               **  TREAT THE TEXT PATTERN SPACING      CASE **
C               **  TREAT THE TEXT PATTERN (TYPE)       CASE **
C               **  TREAT THE TEXT FILL COLOR           CASE **
C               **  TREAT THE TEXT FILL (SWITCH)        CASE **
C               **  TREAT THE TEXT BORDER COLOR         CASE **
C               **  TREAT THE TEXT BORDER THICKNESS     CASE **
C               **  TREAT THE TEXT BORDER LINE (TYPE)   CASE **
C               **  TREAT THE TEXT BASE                 CASE **
C               ***********************************************
C
      IF(ICOM.EQ.'TEXT')THEN
C
        IF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE'.AND.
     1     IHARG(3).EQ.'TYPE')THEN
          CALL DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'COLO'.AND.
     1         IHARG2(2).EQ.'R   ')THEN
          CALL DPTPCO(IHARG,NUMARG,IDETPC,MAXTEX,ITEPCO,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPTPTH(IHARG,IARGT,ARG,NUMARG,PDETPT,MAXTEX,PTEPTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'SPAC')THEN
          CALL DPTPSP(IHARG,IARGT,ARG,NUMARG,PDETPS,MAXTEX,PTEPSP,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'TYPE')THEN
          CALL DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'PATT')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO'.AND.
     1         IHARG2(2).EQ.'R   ')THEN
          CALL DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'COLO'.AND.IHARG2(1).EQ.'R   ')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'SWIT')THEN
          CALL DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          IF(IHARG(1).EQ.'ON'.OR.IHARG(2).EQ.'ON'.OR.
     1       IHARG(1).EQ.'OFF'.OR.IHARG(2).EQ.'OFF')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IBUGP2,IERROR)
            IHARG(2)='SWIT'
            IHARG2(2)='CH  '
            CALL DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,
     1                  IBUGP2,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
          ENDIF
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'COLO'.AND.
     1         IHARG2(2).EQ.'R   ')THEN
          CALL DPTBCO(IHARG,NUMARG,IDETBC,MAXTEX,ITEBCO,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPTBTH(IHARG,IARGT,ARG,NUMARG,PDETBT,MAXTEX,PTEBTH,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'LINE'.AND.
     1         IHARG(3).EQ.'TYPE')THEN
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'BORD'.AND.
     1        (IHARG(2).EQ.'LINE' .OR. IHARG(2).EQ.'TYPE'))THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(ICOM.EQ.'TEXT'.AND.IHARG(1).EQ.'BORD')THEN
          ISHIFT=2
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,
     1                IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
CCCCC   ELSEIF(IHARG(1).EQ.'BASE'.OR.IHARG(1).EQ.'REFE')THEN
CCCCC     CALL DPTEBA(IHARG,IARGT,ARG,NUMARG,ADETBA,MAXTEX,ATEXBA,
CCCCC1                IBUGP2,IFOUND,IERROR)
CCCCC     IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               ***********************************
C               **  END POINT FOR TEXT COMMANDS  **
C               ***********************************
C
C
C     ----------END OF TEXTS---------------------------------------
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAIPC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAIPC4(IBUGPC,IBUGP2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--THIS IS SUBROUTING MAIPC4.
C              (THE   PC    AT THE END OF    MAIPC4   STANDS FOR PLOT CONTROL
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
C              PLOT CONTROL COMMANDS (PART 1).
C              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAIPC4
C              ARE THE FOLLOWING 3D-RELATED COMMANDS--
C
C                 EYE (COORDINATES)
C                 ORIGIN COORDINATES
C                 VISIBLE (HIDDENLINES, BACKLINES)
C                 PROJECTION
C
C                 PEDESTAL ON/OFF
C                 PEDESTAL BASE
C                 PEDESTAL SIZE
C                 PEDESTAL COLOR
C                 PEDESTAL GRID
C                 PEDESTAL GRID PATTERN
C                 PEDESTAL GRID COLOR
C
C                 BASEPLANE ON/OFF
C                 BASEPLANE COLOR
C                 BASEPLANE GRID
C                 BASEPLANE GRID PATTERN
C                 BASEPLANE GRID COLOR
C
C                 BACKPLANE ON/OFF
C                 BACKPLANE COLOR
C                 BACKPLANE GRID
C                 BACKPLANE GRID PATTERN
C                 BACKPLANE GRID COLOR
C
C                 SIDEFACE ON/OFF
C                 SIDEFACE COLOR
C                 SIDEFACE GRID
C                 SIDEFACE GRID PATTERN
C                 SIDEFACE GRID COLOR
C
C                 TIC PLANE
C
C                 ROTATE EYE
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER  --88.10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C     UPDATED         --APRIL     1992. DEPBA=DEFBA COMMENTED OUT
C     UPDATED         --SEPTEMBER 1993. ALLOW EYE FOR EYE COOR
C     UPDATED         --SEPTEMBER 1993. NEW COMMAND--ROTATE EYE
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAIPC4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGPC,IBUGP2,ISUBRO
   53   FORMAT('IBUGPC,IBUGP2,ISUBRO = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ICOM,ICOM2,NUMARG
   67   FORMAT('ICOM,ICOM2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
CCCCC THE FOLLOWING SECTION WAS REWRITTEN    SEPTEMBER 1993
C               ***************************************
C               **  TREAT THE EYE (COORDINATES) CASE **
C               ***************************************
C
      IF(ICOM.EQ.'EYE')THEN
         IF(NUMARG.GE.1)THEN
            IF(IHARG(1).EQ.'COOR')THEN
               ISHIFT=1
               CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,
     1         IARGT,NUMARG,IBUGPC,IERROR)
            ENDIF
         ENDIF
         CALL DPEYCO(IHARG,IARGT,ARG,NUMARG,
     1   AEYEXC,AEYEYC,AEYEZC,
     1   X3DEYE,Y3DEYE,Z3DEYE,
     1   IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE ORIGIN COORDINATES CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'ORIG')THEN
        CALL DPORCO(IHARG,IARGT,ARG,NUMARG,
     1              AORIXC,AORIYC,AORIZC,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************
C               **  TREAT THE VISIBLE CASE   **
C               **  HIDDEN LINES, BACKLINES  **
C               *******************************
C
      IF(ICOM.EQ.'VISI')GOTO1300
      IF(ICOM.EQ.'HIDD')GOTO1300
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'LINE')GOTO1300
      IF(ICOM.EQ.'BACK'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'LINE')
     1GOTO1300
      GOTO1399
C
 1300 CONTINUE
      CALL DPVIS(IHARG,NUMARG,IVISSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1399 CONTINUE
C
C               **************************************************
C               **  TREAT THE PROJECTION CASE (3D)              **
C               **************************************************
C
      IF(ICOM.EQ.'PROJ')GOTO1400
      IF(ICOM.EQ.'ORTH')GOTO1400
      IF(ICOM.EQ.'PERS')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      CALL DPPROJ(ICOM,IHARG,NUMARG,I3DPRO,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1499 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED    SEPTEMBER 1993
C               **************************************
C               **  TREAT THE ROTATE EYE CASE       **
C               **************************************
C
      IF(ICOM.EQ.'ROTA')THEN
         CALL DPROEY(IHARG,IARGT,ARG,NUMARG,
     1   X3DEYE,Y3DEYE,Z3DEYE,
     1   X3DMID,Y3DMID,Z3DMID,
     1   AEYEXC,AEYEYC,AEYEZC,
     1   IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C     -----PEDESTAL-----
C
C               ******************************************
C               **  TREAT THE PEDESTAL GRID COLOR CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'PEDE')GOTO2100
      GOTO2199
C
 2100 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GOTO2110
      GOTO2199
 2110 CONTINUE
      CALL DPPEGC(IHARG,NUMARG,IDEPGC,IPEDGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2199 CONTINUE
C
C               *********************************************
C               **  TREAT THE PEDESTAL GRID PATTERN  CASE  **
C               *********************************************
C
      IF(ICOM.EQ.'PEDE')GOTO2200
      GOTO2299
C
 2200 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GOTO2210
      GOTO2299
 2210 CONTINUE
      CALL DPPEGP(IHARG,NUMARG,IDEPGP,IPEDGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2299 CONTINUE
C
C               *************************************
C               **  TREAT THE PEDESTAL GRID  CASE  **
C               *************************************
C
      IF(ICOM.EQ.'PEDE')GOTO2300
      GOTO2399
C
 2300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GOTO2310
      GOTO2399
 2310 CONTINUE
      CALL DPPEGR(IHARG,NUMARG,IDEPGR,IPEDGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2399 CONTINUE
C
C               **************************************
C               **  TREAT THE PEDESTAL CASE         **
C               **  TREAT THE PEDESTAL COLOR  CASE  **
C               **  TREAT THE PEDESTAL SIZE   CASE  **
C               **  TREAT THE PEDESTAL BASE   CASE  **
C               **************************************
C
      IF(ICOM.EQ.'PEDE')THEN
        IF(NUMARG.GE.1.AND.
     1    (IHARG(1).EQ.'COLO'.OR.IHARG(1).EQ.'RGB '))THEN
          CALL DPPECL(IHARG,IARG,NUMARG,IDEPCO,IRGBMX,IPEDCO,IPEDC2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.
     1        (IHARG(1).EQ.'SIZE' .OR. IHARG(1).EQ.'HEIG'))THEN
          CALL DPPESZ(IHARG,IARGT,ARG,NUMARG,
     1                ADEPSZ,APEDSZ,
     1                IFOUND,IERROR)
           IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'BASE')THEN
          CALL DPPEBA(IHARG,IARGT,ARG,NUMARG,
     1                ADEPBA,APEDBA,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        ELSE
          CALL DPPED(IHARG,NUMARG,IPEDSW,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C     -----BASEPLANE-----
C
C               ******************************************
C               **  TREAT THE BASEPLANE GRID COLOR CASE **
C               ******************************************
C
      IF(ICOM.EQ.'BASE')GOTO3100
      GOTO3199
C
 3100 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GOTO3110
      GOTO3199
 3110 CONTINUE
      CALL DPBSGC(IHARG,NUMARG,IDBSGC,IBSPGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3199 CONTINUE
C
C               *********************************************
C               **  TREAT THE BASEPLANE GRID PATTERN  CASE **
C               *********************************************
C
      IF(ICOM.EQ.'BASE')GOTO3200
      GOTO3299
C
 3200 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GOTO3210
      GOTO3299
 3210 CONTINUE
      CALL DPBSGP(IHARG,NUMARG,IDBSGP,IBSPGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3299 CONTINUE
C
C               *************************************
C               **  TREAT THE BASEPLANE GRID  CASE **
C               *************************************
C
      IF(ICOM.EQ.'BASE')GOTO3300
      GOTO3399
C
 3300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GOTO3310
      GOTO3399
 3310 CONTINUE
      CALL DPBSGR(IHARG,NUMARG,IDBSGR,IBSPGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3399 CONTINUE
C
C               *************************************
C               **  TREAT THE BASEPLANE COLOR CASE **
C               *************************************
C
      IF(ICOM.EQ.'BASE')GOTO3400
      GOTO3499
C
 3400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO3410
      GOTO3499
 3410 CONTINUE
      CALL DPBSCL(IHARG,NUMARG,IDBSCO,IBSPCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3499 CONTINUE
C
C               *******************************
C               **  TREAT THE BASEPLANE CASE **
C               *******************************
C
      IF(ICOM.EQ.'BASE')GOTO3500
      GOTO3599
C
 3500 CONTINUE
      CALL DPBSP(IHARG,NUMARG,IBSPSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 3599 CONTINUE
C
C     -----BACKPLANE-----
C
C               ******************************************
C               **  TREAT THE BACKPLANE GRID COLOR CASE **
C               ******************************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GOTO4100
      GOTO4199
C
 4100 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GOTO4110
      GOTO4199
 4110 CONTINUE
      CALL DPBKGC(IHARG,NUMARG,IDBKGC,IBKPGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4199 CONTINUE
C
C               *********************************************
C               **  TREAT THE BACKPLANE GRID PATTERN  CASE **
C               *********************************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GOTO4200
      GOTO4299
C
 4200 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GOTO4210
      GOTO4299
 4210 CONTINUE
      CALL DPBKGP(IHARG,NUMARG,IDBKGP,IBKPGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4299 CONTINUE
C
C               *************************************
C               **  TREAT THE BACKPLANE GRID  CASE **
C               *************************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GOTO4300
      GOTO4399
C
 4300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GOTO4310
      GOTO4399
 4310 CONTINUE
      CALL DPBKGR(IHARG,NUMARG,IDBKGR,IBKPGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4399 CONTINUE
C
C               *************************************
C               **  TREAT THE BACKPLANE COLOR CASE **
C               *************************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GOTO4400
      GOTO4499
C
 4400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO4410
      GOTO4499
 4410 CONTINUE
      CALL DPBKCL(IHARG,NUMARG,IDBKCO,IBKPCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4499 CONTINUE
C
C               *******************************
C               **  TREAT THE BACKPLANE CASE **
C               *******************************
C
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GOTO4500
      GOTO4599
C
 4500 CONTINUE
      CALL DPBKP(IHARG,NUMARG,IBKPSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4599 CONTINUE
C
C     -----SIDEFACE-----
C
C               ******************************************
C               **  TREAT THE SIDEFACE GRID COLOR CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'SIDE')GOTO5100
      GOTO5199
C
 5100 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GOTO5110
      GOTO5199
 5110 CONTINUE
      CALL DPSDGC(IHARG,NUMARG,IDSDGC,ISDFGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5199 CONTINUE
C
C               *********************************************
C               **  TREAT THE SIDEFACE GRID PATTERN  CASE  **
C               *********************************************
C
      IF(ICOM.EQ.'SIDE')GOTO5200
      GOTO5299
C
 5200 CONTINUE
      IF(NUMARG.GE.2.AND.
     1IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GOTO5210
      GOTO5299
 5210 CONTINUE
      CALL DPSDGP(IHARG,NUMARG,IDSDGP,ISDFGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5299 CONTINUE
C
C               *************************************
C               **  TREAT THE SIDEFACE GRID  CASE  **
C               *************************************
C
      IF(ICOM.EQ.'SIDE')GOTO5300
      GOTO5399
C
 5300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GOTO5310
      GOTO5399
 5310 CONTINUE
      CALL DPSDGR(IHARG,NUMARG,IDSDGR,ISDFGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5399 CONTINUE
C
C               *************************************
C               **  TREAT THE SIDEFACE COLOR CASE  **
C               *************************************
C
      IF(ICOM.EQ.'SIDE')GOTO5400
      GOTO5499
C
 5400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO5410
      GOTO5499
 5410 CONTINUE
      CALL DPSDCL(IHARG,NUMARG,IDSDCO,ISDFCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5499 CONTINUE
C
C               *******************************
C               **  TREAT THE SIDEFACE CASE  **
C               *******************************
C
      IF(ICOM.EQ.'SIDE')GOTO5500
      GOTO5599
C
 5500 CONTINUE
      CALL DPSDF(IHARG,NUMARG,ISDFSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5599 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGPC.EQ.'ON' .OR. ISUBRO.EQ.'IPC4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAIPC4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAINSU(IDEFSE,ISEED,ANOPL1,ANOPL2,
     1                  ISQUAR,IBOOSS,IDEBOO,
     1                  IANSSV,IREPMX,ILISMX,IPOINT,
     1                  ISACNC,IAUTSW,IAUTEX,ITOPIC,MAXNXT,IPROSW,
     1                  IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,
     1                  IOFILE,IMALEV,IPROGR,ICONCL,
     1                  ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,
     1                  ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
     1                  IBASLC,IREPCH,IOSW,ICAPSW,IPRDEF,
     1                  IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                  IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,
     1                  ICPREH,NCPREH,ICPOSH,NCPOSH,IOUTTY,IPRITY,
     1                  IHELMX,IFTEXP,IFTORD,ALOWFR,ALOWDG,
     1                  IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,
     1                  IREARW,IWRIRW,
     1                  IUNFOF,IUNFNR,IUNFMC,
     1                  IRHSTG,IMPSW,IERRFA,IGUIFL,IGUIFB,
     1                  ITABTI,NCTABT,ITABBR,ITABSP,ITABWD,ITABHT,
     1                  IANSLO,ILOOST,ILOOLI,NUMLIL,NUMLOS,IWIDLL,
     1                  IIFSW,NUMIF,
     1                  NPLOTP,IFOUND,IERROR)
C
CCCCC IBASLC WAS ADDED TO ABOVE INPUT ARGUMENT LIST       JUNE 1989
CCCCC ICAPSW AND IPRDEF WERE ADDED TO ABOVE ARGUMENT LIST JUNE 1989
CCCCC ADD "LOOP" ARGUMENTS (FOR READ COMMAND) JANUARY 2015.
C
C     PURPOSE--THIS IS SUBROUTING MAINSU.
C
C              (THE   SU    AT THE END OF    MAINSU   STANDS FOR   SUPPORT)
C              THIS SUBROUTINE SEARCHES FOR AND EXECUTES SUPPORT COMMANDS.
C              THE SUPPORT COMMANDS SEARCHED FOR BY MAINSU ARE AS FOLLOWS--
C
C                     ADD                            N/A                 ADD CAL
C                     ANOP LIMITS (= PROPORTION LIMITS)     +-INFINITY
C                     BAUD                           9600                BAUD 12
C                     BUGS                           N/A                 BUGS
C                     CLASS ... LOWER                AUTOMATIC           CLASS L
C                     CLASS ... UPPER                AUTOMATIC FROM DATA CLASS U
C                     CLASS ... WIDTH                AUTOMATIC FROM DATA CLASS W
C                     COLUMN LIMITS                  1 132               COLUMN
C                     COMMENT                        N/A                 COMMENT
C                     CURSOR SIZE                    1.0                 CURSOR
C                     DEFAULT COMMAND                NO COMMAND          DEFAULT
C                     DELETE                         N/A                 DELETE
C                     DEMODULATION FREQUENCY         0.25                DEMODUL
C                     DIMENSION                      1000 ROWS 10 COLS   DIMENSI
C                     DOUBLE PRECISION               OFF = SING. PREC.   DOUBLE
C                     ECHO                           OFF = NO ECHO       ECHO ON
C                     END                            N/A                 END
C                     ERASE DELAY                    1                   ERASE D
C                     FEEDBACK                       ON = FEEDBACK       FEEDBAC
C                     FILTER WIDTH                   3                   FILTER
C                     FIT CONSTRAINT                 ALL UNCONSTRAINED   FIT CON
C                     FIT ITERATIONS                 50                  FIT ITE
C                     FIT STANDARD DEVIATION         .000005             FIT STA
C                     HARDCOPY DELAY                 1                   HARDCOP
C                     HELP                           N/A                 HELP PL
C                     HOST                           THE    LOCAL    HOST    HOS
C                     HOST LINK                      THE    LOCAL    HOST    HOS
C                     IMPLEMENT                      ORIG. INITIALIZ.     IMPLEM
C                     KNOTS                          OFF = NO KNOTS      KNOTS K
C                     MACRO (CREATE)                 OFF                 MACRO
C                     MAIL                           N/A                 MAIL JO
C                     MAXIMUM RECORD LENGTH          N/A                 MAIL JO
C                     NAME                           N/A                 NAME Y
C                     NEWS                           N/A                 NEWS
C                     OPERATOR                       N/A                 OPERAT
C                     POLYNOMIAL DEGREE              1 = LINEAR          POLYNOM
C                     PRECISION                      SINGLE              PRECISI
C                     PRE-ERASE                      ON = PRE-ERASE      PRE-ERA
C                     PRINTING                       ON = PRINTING       PRINTIN
C                     PROBE                          N/A                 PROBE N
C                     QUADRUPLE PRECISION            OFF = SING. PREC.   QUADRUP
C                     QUERY                          N/A                 QUERY H
C                     READ                           N/A                 READ CA
C                     RESET                          N/A                 RESET
C                     RESTORE                        N/A                 RESTORE
C                     RETAIN                         N/A                 RETAIN
C                     ROW LIMITS                     1 INFINITY          ROW LIM
C                     SAVE                           N/A                 SAVE SC
C                     SEED                           20867350019         SEED
C                     TERMIANATOR CHARACTER          ;                   SEPAR
C                     SERIAL READ                    N/A                 SERIAL
C                     SET                            OFF                 SET IBU
C                     SINGLE PRECISION               ON                  SINGLE
C                     SKIP                           0 = NO LINES        SKIP 5
C                     STATUS                         N/A                 STATUS
C                     TIME                           N/A                 TIME
C                     TRIPLE PRECISION               OFF = SING. PREC.   TRIPLE
C                     WEIGHTS                        OFF = EQUI-WEIGHTED WEIGHTS
C                     WRITE                          N/A                 WRITE C
C                     .                              N/A                 . CARRY
C                     CONTINUE CHARACTER             ...                 CONTI
C                     PRINTER FORMAT ASCII/POSTSCRIPT
C                     FILE FORMAT ASCII/POSTSCRIPT
C
C                     VECTOR FORMAT <ANGLE/POINT/DELTA>
C                     VECTOR ARROW  <FIXED/VARIABLE>
C                     VECTOR ARROW  <OPEN/CLOSED>
C                     ANDREWS INCREMENT
C                     OPTIMIZATION METHOD
C                     WEB HELP
C                     RECIPE SATTERWAITE APPROXIMATION
C                     RECIPE OUTPUT
C                     RECIPE PROBABILITY CONTENT (OR RECIPE CONTENT)
C                     RECIPE CONFIDENCE
C                     RECIPE FIT DEGREE (OR RECIPE DEGREE)
C                     RECIPE ANOVA FACTORS (OR RECIPE FACTORS)
C                     RECIPE CORRELATION
C                     RECIPE SIMCOV REPLICATES
C                     RECIPE SIMPVT REPLICATES
C
C                     GUI WRITE/PRINT
C                     GUI STATUS
C                     GUI PLOT CONTROL <N>
C
C                     VARIABLE LABEL
C
C                     ORTHOGONAL DISTANCE ERROR 
C                     ORTHOGONAL DISTANCE DELTA 
C
C                     KERNEL DENSITY WIDTH
C                     KERNEL DENSITY POINTS
C
C                     AUTO TEXT
C
C                     SYSTEM
C                     PROCES ID (OR PID)
C                     CPU TIME
C                     PWD       (OR GETCWD, CURRENT DIRECTORY)
C                     CLIPBOARD CLEAR (OR CLEAR CLIPBOARD)
C                     CLIPBOARD
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 1983.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --OCTOBER   1987. (ISUBRO FOR DPAPPE AND DPEXTE)
C     UPDATED         --AUGUST    1988. EQUATE PROPORTION LIMITS WITH ANOP LIM
C     UPDATED         --DECEMBER  1988. ADJUST RESET FOR RESET2
C     UPDATED         --DECEMBER  1988. RESET DATA, IO, PC, ETC.
C     UPDATED         --DECEMBER  1988. SET WRITE FORMAT
C     UPDATED         --DECEMBER  1988.  SET READ  REWIND
C     UPDATED         --DECEMBER  1988.  SET WRITE REWIND
C     UPDATED         --DECEMBER  1988.  LOWESS FRACTION
C     UPDATED         --DECEMBER  1988.  READ/WRITE DECI, FORMAT, REWIND
C     UPDATED         --JANUARY   1989.  BOOTSTRAP SAMPLE SIZE
C     UPDATED         --FEBRUARY  1989.  CONTINUE CHARACTER (ALAN)
C     UPDATED         --FEBRUARY  1989.  SOFT-CODED LIMITS FOR IANSSV (ALAN)
C     UPDATED         --FEBRUARY  1989.  SYSTEM COMMAND (ALAN)
C     UPDATED         --JUNE      1989.  REPLACEMENT/SUBSTITUTION CHARACTER
C     UPDATED         --JUNE      1989.  CAPTURE (TEXT OUTPUT)
C     UPDATED         --JULY      1989.  MORE/PAUSE TO LIST
C     UPDATED         --NOVEMBER  1989.  COLUMN RULER
C     UPDATED         --NOVEMBER  1989.  NLIST
C     UPDATED         --NOVEMBER  1989.  ADD ARG TO CALL TO DPSYST
C     UPDATED         --MARCH     1990.  ADD ARGUMENT TO SYSTEM COMMAND (ALAN)
C     UPDATED         --MAY       1990.  ADD ARGUMENTS TO DPREAD, DPREAL
C     UPDATED         --MAY       1990.  COMMENT CHARACTER COMMAND
C     UPDATED         --JUNE      1990.  IBUGD2 TO IBUGS2 IN CALL TO DPSYST
C     UPDATED         --JULY      1990.  ICOMFL RENAMED AS ICOMSW
C     UPDATED         --SEPTEMBER 1990.  DOS, UNIX, ETC. FOR SYSTEM
C     UPDATED         --SEPTEMBER 1990.  DATE SYNONYM FOR TIME
C     UPDATED         --MARCH     1992.  PRINTER FORMAT ASCI/POST
C     UPDATED         --MARCH     1992.  FILE FORMAT ASCI/POST
C     UPDATED         --APRIL     1992.  ADD NPLOTP TO ARGS
C     UPDATED         --AUGUST    1992.  VECTOR FORMAT, VECTOR ARROW
C     UPDATED         --SEPTEMBER 1992.  LIST SYNONYMS: VIEW/PREVIEW
C     UPDATED         --NOVEMBER  1992.  ANDREWS INCREMENT
C     UPDATED         --JULY      1993.  FRACTAL ITERATIONS
C     UPDATED         --JULY      1993.  FRACTAL TYPE
C     UPDATED         --JULY      1993.  PRINCIPLE COMPONENT TYPE
C     UPDATED         --JULY      1993.  ADD ARGS TO DPLICO: MORE
C     UPDATED         --SEPTEMBER 1993.  REWRITE CODE AROUND DPLICO
C     UPDATED         --DECEMBER  1993.  CHECK FOR "SAVE" AND "S CHART"
C                                        CONFLICT.
C     UPDATED         --JANUARY   1994.  SEARCH1
C     UPDATED         --MAY       1994.  COPY FILE => COPY
C     UPDATED         --JUNE      1994.  OPTIMIZATION TOLERANCE
C     UPDATED         --AUGUST    1994.  EXECUTE SUBSET OF MACRO
C     UPDATED         --SEPTEMBER 1994.  CHECK FOR NAME CONFLICT
C     UPDATED         --NOVEMBER  1994.  DECLARE NEWNAM (BOMB ON VAX)
C     UPDATED         --FEBRUARY  1995.  OPTIMIZATION METHOD
C     UPDATED         --APRIL     1995.  IUNFOF, IUNFNR, IUNFMC
C     UPDATED         --AUGUST    1995.  ADD IFTORD
C     UPDATED         --SEPTEMBER 1995.  ISUBRO ADDED TO CALL DPDELE
C     UPDATED         --SEPTEMBER 1995.  INIT COMMAND (FOR DEBUGGING)
C     UPDATED         --OCTOBER   1995.  NAME CONFLICT WITH DOUBLE
C     UPDATED         --MARCH     1997.  SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --APRIL     1997.  WEB HELP COMMAND (ALAN)
C     UPDATED         --APRIL     1997.  LIST GRAPH (ALAN)
C     UPDATED         --APRIL     1997.  SAVE GRAPH (ALAN)
C     UPDATED         --APRIL     1997.  REPEAT GRAPH (ALAN)
C     UPDATED         --APRIL     1997.  CYCLE GRAPH (ALAN)
C     UPDATED         --AUGUST    1997.  SLEEP (= PAUSE <n>)
C     UPDATED         --AUGUST    1997.  CD COMMAND
C     UPDATED         --AUGUST    1997.  6 RECIPE COMMANDS
C     UPDATED         --NOVEMBER  1997.  GUI PRINT/WRITE
C     UPDATED         --NOVEMBER  1997.  GUI STATUS
C     UPDATED         --NOVEMBER  1997.  GUI SAVE PLOT CONTROL
C     UPDATED         --JANUARY   1998.  CALL TO DPDIME
C     UPDATED         --NOVEMBER  1998.  CALL LIST TO DPSET, DPPROB
C     UPDATED         --MARCH     1998.  NAME CONFLICT WITH CP AND CP PLOT
C     UPDATED         --APRIL     1997.  RECIPE FIT FACTORS COMMANDS
C     UPDATED         --MARCH     1999.  NAME CONFLICT FOR SINGLE
C     UPDATED         --NOVEMBER  1999.  VARIABLE LABEL
C     UPDATED         --APRIL     2001.  ORTHOGONAL DISTANCE ERROR
C     UPDATED         --APRIL     2001.  ORTHOGONAL DISTANCE DELTA
C     UPDATED         --AUGUST    2001.  KERNEL DENSITY WIDTH/POINTS
C     UPDATED         --JUNE      2002.  ICAPTY IN DPCAPT CALL
C     UPDATED         --FEBRUARY  2003.  CALL TO DPREAD, DPSERI
C     UPDATED         --FEBRUARY  2003.  CALL TO DPCOLL
C     UPDATED         --FEBRUARY  2003.  ADD: MAXIMUM RECORD LENGTH
C     UPDATED         --FEBRUARY  2003.  CALL LIST TO DPSEAR
C     UPDATED         --SEPTEMBER 2003.  CALL LIST TO DPWRIT
C     UPDATED         --SEPTEMBER 2005.  CALL LIST TO DPMACR
C     UPDATED         --SEPTEMBER 2005.  MACRO SUBSTITUTION CHARACTER
C     UPDATED         --JANUARY   2006.  ARGUMENT LIST TO DPCAPT
C     UPDATED         --MARCH     2006.  PROCESS ID
C     UPDATED         --AUGUST    2007.  USER-DEFINED ACTION ON
C                                        ERROR
C     UPDATED         --SEPTEMBER 2007.  IERRST
C     UPDATED         --MAY       2008.  GUI FEEDBACK SWITCH
C     UPDATED         --APRIL     2009.  TABLE WIDTH COMMAND
C     UPDATED         --APRIL     2009.  CALL LIST TO DPWRIT
C     UPDATED         --MAY       2009.  ADD CPU TIME COMMAND
C     UPDATED         --MAY       2010.  REMOVE "MAIL" AND "QUERY"
C                                        COMMANDS
C     UPDATED         --JANUARY   2011.  ADD PWD COMMAND
C     UPDATED         --NOVEMBER  2014.  CLIPBOARD CLEAR
C     UPDATED         --NOVEMBER  2014.  CLIPBOARD RUN
C     UPDATED         --JANUARY   2015. LOOP ARGUMENTS TO DPREAD
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C     UPDATED         --MARCH     2015. CALL LIST TO UPDATF
C     UPDATED         --NOVEMBER  2015. CALL LIST TO DPMACR
C     UPDATED         --DECEMBER  2015. CALL LIST TO MAININ
C     UPDATED         --JULY      2016. STREAM READ
C     UPDATED         --JULY      2017. CALL LIST TO DPMACR,
C                                       CALL LIST TO MAINSU
C     UPDATED         --JULY      2017. INSERT CALL ARGUMENTS COMMAND
C     UPDATED         --JULY      2017. ISSUE WITH COMMAND LINE
C                                       ARGUMENTS IN LOOP STORE MODE,
C                                       DO COMMAND LINE SUBSTITUTION
C                                       FOR FILE NAME, BUT NOT ARGUMENTS
C     UPDATED         --APRIL     2018. CALL LISTS TO DPHELW, DPHANW,
C                                       DPWEB
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
C     UPDATED         --FEBRUARY  2019. SUPPORT FOR "CALL CLIPBOARD"
C     UPDATED         --SEPTEMBER 2019. SUPPORT FOR "GREP" AS SEARCH
C                                       OPTION
C     UPDATED         --SEPTEMBER 2019. ADD "RM" AND "RMDIR" COMMANDS
C     UPDATED         --SEPTEMBER 2019. ADD "MKDIR" COMMAND
C     UPDATED         --SEPTEMBER 2019. ADD "CAT" COMMAND
C     UPDATED         --SEPTEMBER 2019. ADD "DIR" COMMAND
C     UPDATED         --SEPTEMBER 2019. FOR LIST AND SAVE, CHECK IF
C                                       FIRST ARGUMENT IS "="
C     UPDATED         --OCTOBER   2019. "HEAD" AND "TAIL" OPTIONS FOR
C                                       WRITE COMMAND
C     UPDATED         --NOVEMBER  2019. "RSCRIPT" AND "PYTHON"
C                                       COMMANDS
C     UPDATED         --FEBRUARY  2020. FOR "CLIPBOARD" COMMANDS,
C                                       CHECK IF "CLIPBOARD" ARGUMENT
C                                       IS ACTUALLY A FILE NAME.
C     UPDATED         --AUGUST    2020. FOR CYCLE GRAPH COMMAND, CHECK
C                                       FOR CONFLICT WITH:
C                                           CP CUMULATIVE STATISTIC PLOT
C                                           CP MOVING     STATISTIC PLOT
C     UPDATED         --DECEMBER  2020. ADDED SAVE VARIABLE COMMAND
C     UPDATED         --DECEMBER  2020. ADDED RESTORE VARIABLE COMMAND
C     UPDATED         --APRIL     2021. ADD "X" AS SYNOMYM FOR "CALL
C                                       CLIPBOARD"
C     UPDATED         --APRIL     2021. ADD ALIAS COMMAND
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IMPSW
      CHARACTER*4 ILOOST
      CHARACTER*4 IIFSW
C
      CHARACTER*4 ISQUAR
      CHARACTER*4 ITOPIC
      CHARACTER*4 IPROSW
C
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 IOFILE
C
      CHARACTER*4 IPROGR
      CHARACTER*4 ICONCL
C
      CHARACTER*4 ICOM3
      CHARACTER*4 ICOM4
      CHARACTER*40 ICOM5
C
      CHARACTER*30 ICTRA1
      CHARACTER*30 ICTRA2
C
      CHARACTER*1 IBASLC
      CHARACTER*1 IREPCH
      CHARACTER*4 IOSW
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
      CHARACTER*4 IBUGEX
      CHARACTER*4 IBUGE2
      CHARACTER*4 IBUGHE
      CHARACTER*4 IBUGH2
      CHARACTER*4 IBUGLO
C
      CHARACTER*40 ICPREH
      CHARACTER*40 ICPOSH
C
CCCCC THE FOLLOWING 2 LINES WERE ADDED    MARCH 1992
      CHARACTER*4 IPRITY
      CHARACTER*4 IOUTTY
C
      CHARACTER*4 IFTEXP
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IFTORD
C
      CHARACTER*4 IFORSW
      CHARACTER*80 ICREAF
      CHARACTER*80 ICWRIF
C
      CHARACTER*4 IREARW
      CHARACTER*4 IWRIRW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IDEFHL
      CHARACTER*4 IHOSLI
C
      CHARACTER*1 IANSSV
CCCCC CHARACTER*80 ISACNC
      CHARACTER (LEN=MAXFNC) :: ISACNC
C
      CHARACTER*4 IAUTSW
      CHARACTER*4 IAUTEX
      CHARACTER*4 IBELSJ
      CHARACTER*4 IERASJ
      CHARACTER*4 IBACCJ
      CHARACTER*4 ICOPSJ
C
      CHARACTER*4 ISEART
C
CCCCC THE FOLLOWING LINE WAS ADDED JUNE 1989
      CHARACTER*4 ICAPSW
C
CCCCC THE FOLLOWING 5 LINES WERE ADDED FEBRUARY 1993
      CHARACTER*24 CURRTI
      CHARACTER*24 CURRDA
      CHARACTER*4 IC4
      CHARACTER*4 IFOUNN
      CHARACTER*4 IERRON
C
CCCCC THE FOLLOWING LINE NOVEMBER 1994
      CHARACTER*4 NEWNAM
CCCCC THE FOLLOWING 2 LINES WERE ADDED SEPTEMBER 1995
      CHARACTER*4 ICOMHO
      CHARACTER*4 ICOMH2
CCCCC THE FOLLOWING 2 LINES WERE ADDED OCTOBER 1996
      CHARACTER*4 IRHSTG
CCCCC THE FOLLOWING 2 LINES WERE ADDED SEPTEMBER 2003
      CHARACTER*4 ITABBR
      CHARACTER*80 ITABTI
C
      CHARACTER*4 ICASOD
C
      CHARACTER*4 IERRFA
      CHARACTER*4 IGUIFL
      CHARACTER*4 IGUIFB
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASE2
C
      CHARACTER*4 IFUTMP(100)
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
C
      CHARACTER*1 IQUOTE
CCCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
C
      DIMENSION IDEFHL(10)
      DIMENSION IHOSLI(10)
C
CCCCC DIMENSION IANSSV(50,80)
      DIMENSION IANSSV(MAXLIS,MAXCIS)
      CHARACTER*4 IANSLO(MAXLIL,MAXCIL)
      DIMENSION IWIDLL(MAXLIL)
C
      DIMENSION ICOM3(*)
      DIMENSION ICOM4(*)
      DIMENSION ICOM5(*)
      DIMENSION NCOM5(*)
C
      DIMENSION ICTRA1(*)
      DIMENSION NCTRA1(*)
      DIMENSION ICTRA2(*)
      DIMENSION NCTRA2(*)
C
      CHARACTER*4 IFEESV
C
      CHARACTER*255 CURDIR
      CHARACTER*4   IFUNC9(255)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCODG.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOGR.INC'
CCCCC THE FOLLOWING LINE WAS INSERTED NOVEMBER 1989
      INCLUDE 'DPCODE.INC'
C
CCCCC TO AVOID NAME CONFLICTS, ONLY BRING IN THE SPECIFIC
CCCCC COMMON BLOCK
C
      CHARACTER*4  IERRST
      COMMON/CSETG/IERRST
C
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGSU.EQ.'ON'.OR.ISUBRO.EQ.'INSU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MAINSU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IBUGSU,IBUGS2,IBUGCO,IBUGEV,IBUGQ
   55   FORMAT('IBUGSU,IBUGS2,IBUGCO,IBUGEV,IBUGQ = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IANGLU,ISQUAR,IFENSW,IBOOSS,IDEBOO
   58   FORMAT('IANGLU,ISQUAR,IFENSW,IBOOSS,IDEBOO = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IMACRO,IMACNU,IMACCS,IOFILE
   59   FORMAT('IMACRO,IMACNU,IMACCS,IOFILE = ',A4,I8,2X,A12,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IFOUND,IERROR,ICOM,ICOM2,NUMARG
   62   FORMAT('IFOUND,IERROR,ICOM,ICOM2,NUMARG = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,73)(IA(I),I=1,100)
   73   FORMAT('(IA(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)IMACRO,IPROGR,ICONCL,NUMCHA
   75   FORMAT('IMACRO,IPROGR,ICONCL,NUMCHA = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)ISACNC
   81   FORMAT('ISACNC = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)IAUTSW,IAUTEX,ITOPIC,MAXNXT
   82   FORMAT('IAUTSW,IAUTEX,ITOPIC,MAXNXT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)IHELMX,IFTEXP,IFORSW,ALOWFR
   83   FORMAT('IHELMX,IFTEXP,IFORSW,ALOWFR = ',I8,2(2X,A4),G15.7)
        CALL DPWRST('XXX','BUG ')
CCCCC   THE FOLLOWING 2 LINES WERE INSERTED NOVEMBER 1989
        WRITE(ICOUT,86)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
   86   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,
     1         2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)IRHSTG,IPRITY,IOUTTY,ALOWDG
   87   FORMAT('IRHSTG,IPRITY,IOUTTY,ALOWDG = ',3(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,88)ITABBR,ITABSP,ITABWD,ITABHT,NCTABT
   88   FORMAT('ITABBR,ITABSP,ITABWD,ITABHT,NCTABT = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,89)ITABTI
   89   FORMAT('ITABTI = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IERROR='NO'
C
C               ******************************
C               **  TREAT THE ADD     CASE  **
C               **  TREAT THE CALL    CASE  **
C               **  TREAT THE EXECUTE CASE  **
C               **  TREAT THE RUN     CASE  **
C               ******************************
C
C     2015/03: CALL EXIT AND CALL EXIT ALL CASES SUPPORTED
C
C     2021/04: ADD "X" AS A SYNONYM FOR "CALL CLIPBOARD"
C
      IF(ICOM.EQ.'X   ' .AND. NUMARG.LE.0)THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IHARG(1)='CLIP'
        IHARG2(1)='BOAR'
        ICOM='CALL'
        ICOM2='    '
        IANS(1)='C'
        IANS(2)='A'
        IANS(3)='L'
        IANS(4)='L'
        IANS(5)=' '
        IANS(6)='C'
        IANS(7)='L'
        IANS(8)='I'
        IANS(9)='P'
        IANS(10)='B'
        IANS(11)='O'
        IANS(12)='A'
        IANS(13)='R'
        IANS(14)='D'
        IWIDTH=14
        DO2031II=1,IWIDTH
          IANSLC(II)=IANS(II)
 2031   CONTINUE
      ENDIF
C
      IF(ICOM.EQ.'ADD' .OR. ICOM.EQ.'CALL' .OR.
     1   ICOM.EQ.'EXEC' .OR. ICOM.EQ.'RUN ')THEN
C
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'CLIP' .AND.
     1     IHARG2(1).EQ.'BOAR')THEN
C
C         CHECK IF ARGUMENT IS A FILE NAME STARTING WITH
C         "CLIPBOARD.
C
          IWORD=2
          IOFILE='NO'
          CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1                IOFILE,IBUGS2,ISUBRO,IERROR)
          IF(IOFILE.EQ.'NO')GOTO13390
        ENDIF
C
        IF(NUMARG.EQ.1 .AND.
     1    IHARG(1).EQ.'EXIT')THEN
          IMACCS='CLO2        '
          IMACRO='EOF'
          IFOUND='YES'
        ELSE
C
C         2015/11: COMMAND LINE SUBSTITUTION (ISSUE FOR LOOPS)
C
          IF(ILOOST.NE.'STOR')THEN
            CALL DPREP2(IANSLC,IWIDTH,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVARLB,IROWLB,MAXOBV,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1                  IBUGS2,ISUBRO,IERROR)
          ELSE
C
C           IN PROCESSING THE CALL COMMAND IN STORE MODE, WE ACTUALLY WANT
C           TO PERFORM COMMAND LINE SUBSTITION FOR THE FILE NAME, BUT NOT
C           FOR THE COMMAND LINE ARGUMENTS.  IF THERE ARE NO COMMAND LINE
C           ARGUMENTS, THEN NO SPECIAL PROCESSING NEEDED.
C
            ICANS=' '
            DO13301II=1,IWIDTH
              ICANS(II:II)=IANSLC(II)(1:1)
13301       CONTINUE
C
C           NOW SEARCH FOR THE LOCATION OF THE FIRST TWO WORDS.
C
            IQUOTE='"'
            ISTART=0
            DO13303II=1,IWIDTH
              IF(ICANS(II:II).NE.' ')THEN
                ISTART=1
                GOTO13309
              ENDIF
13303       CONTINUE
            GOTO13399
13309       CONTINUE
C
            IQFLAG=0
            IF(ICANS(ISTART:ISTART).EQ.IQUOTE)IQFLAG=1
            DO13310KK=1,2
              DO13311II=ISTART,IWIDTH
                IF(IQFLAG.EQ.0)THEN
                  IF(ICANS(II:II).EQ.' ')THEN
                    ISTOP=II-1
                    GOTO13319
                  ENDIF
                ELSE
                  IF(ICANS(II:II).EQ.IQUOTE)THEN
                    ISTOP=II
                    GOTO13319
                  ENDIF
                ENDIF
13311         CONTINUE
              CALL DPREP2(IANSLC,IWIDTH,
     1                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                    IVARLB,IROWLB,MAXOBV,
     1                    IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1                    IBUGS2,ISUBRO,IERROR)
              GOTO13399
13319         CONTINUE
              IF(KK.EQ.1)THEN
                ISTART=ISTOP+2
              ENDIF
13310       CONTINUE
C
C           NOW DO COMMAND LINE SUBSTITUTION FOR THE FIRST ISTOP
C           CHARACTERS.
C
            ISTOP2=ISTOP
            CALL DPREP2(IANSLC,ISTOP,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVARLB,IROWLB,MAXOBV,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1                  IBUGS2,ISUBRO,IERROR)
            ICNT=ISTOP
            DO13321II=ISTOP2+1,IWIDTH
              ICNT=ICNT+1
              IANSLC(ICNT)(1:1)=ICANS(II:II)
13321       CONTINUE
            IWIDTH=ICNT
          ENDIF
        ENDIF
C
13399   CONTINUE
        CALL DPMACR(ICOM,ICOM2,
CCCCC   THE FOLOWING LINE WAS AUGMENTED   AUGUST 1994
CCCCC1              IMACRO,IMACNU,IMACCS,
     1              IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,IMALEV,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              IANSLC,IANS,IWIDTH,
     1              IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IOFILE,
     1              ILOOST,ILOOLI,NUMLIL,NUMLOS,
     1              IANSLO,IWIDLL,MAXCIL,MAXLIL,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
C
C       IF "CALL EXIT" OR "CALL EXIT ALL" ENTERED, THEN DEPRECATE
C       CURRENT IF SWITCH SETTING.
C
        IF(ICOM.EQ.'CALL' .AND. IHARG(1).EQ.'EXIT')THEN
          IF(IIFSW.EQ.'TRUE' .AND. NUMIF.GT.0)NUMIF=NUMIF-1
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
13390 CONTINUE
C               **********************************************
C               **  TREAT THE INSERT CALL ARGUMENTS  CASE  **
C               **********************************************
C
      IF(ICOM.EQ.'INSE' .AND. IHARG(1).EQ.'CALL' .AND.
     1   IHARG(2).EQ.'ARGU')THEN
        CALL DPICLA(ICOM,ICOM2,
     1              IMACRO,IMACNU,IMACCS,
     1              IMACL1,IMACL2,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              IANSLC,IWIDTH,
     1              IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE DEFINE  CASE  **
C               ******************************
C
      IF(ICOM.EQ.'DEFI')THEN
        CALL DPDEFI(IHARG,IHARG2,IHARLC,NUMARG,
     1              ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,
     1              ICPREP,NCPREP,ICPOST,NCPOST,
     1              ICPREH,NCPREH,ICPOSH,NCPOSH,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE ALIAS   CASE  **
C               ******************************
C
      IF(ICOM.EQ.'ALIA')THEN
        CALL DPALIA(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE TRANSLATE CASE  **
C               ********************************
C
      IF(ICOM.EQ.'TRAN')THEN
        CALL DPTRAN(IHARG,IHARG2,NUMARG,
     1              ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE BAUD CASE  **
C               ***************************
C
      IF(ICOM.EQ.'BAUD')THEN
        CALL DPBAUD(IHARG,IARGT,IARG,NUMARG,IDEFBA,
     1              IBAUD,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IGBAUD=IBAUD
          DO415I=1,MAXDEV
            IDBAUD(I)=IBAUD
  415     CONTINUE
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE COLUMN LIMITS CASE  **
C               ************************************
C
CCCCC IF(ICOM.EQ.'COLU')GOTO500
C  DECEMBER, 1989.  CHECK FOR CONFLICT WWITH COLUMN RULER COMMAND.
      IF(ICOM.EQ.'COLU'.AND.IHARG(1).NE.'RULE')GOTO500
      GOTO599
C
  500 CONTINUE
      CALL DPCOLL(IDEFC1,IDEFC2,IFCOL1,IFCOL2,
CCCCC FEBRUARY 2003: ADD FOLLOWING LINE
     1NUMRCM,
     1IFCOLL,IFCOLU,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
  599 CONTINUE
C
C               ************************************
C               **  TREAT THE TABLE WIDTH   CASE  **
C               ************************************
C
      IF(ICOM.EQ.'TABL'.AND.IHARG(1).EQ.'WIDT')THEN
        CALL DPTAWI(IFORWI,IFORWR,MAXNWI,
     1              ISUBRO,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************************
C               **  TREAT THE MAXIMUM RECORD LENGTH CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'MAXI'.AND.IHARG(1).EQ.'RECO'.AND.IHARG(2).EQ.'LENG')
     1  THEN
        CALL DPMXRL(IHARG,IARGT,IARG,NUMARG,IDEFRL,NUMRCM,MAXRCL,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE DEGREES CASE  **
C               ******************************
C
C     (THE FOLLOWING IS COMMENTED OUT
C     (THE FOLLOWING IS COMMENTED OUT
C     (THE FOLLOWING IS COMMENTED OUT
C     IN THE SUBROUTINE MAINDG)
C
CCCCC IF(ICOM.EQ.'DEGR'.AND.ICOM2.EQ.'EES ')GOTO700
CCCCC GOTO799
C
CC700 CONTINUE
CCCCC CALL DPDEGS(IHARG,NUMARG,IDEFAU,
CCCCC1IANGLU,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
CC799 CONTINUE
C
C               *****************************
C               **  TREAT THE DELETE CASE  **
C               *****************************
C
      IF(ICOM.EQ.'DELE')THEN
CCCCC   THE FOLLOWING LINE WAS FIXED     SEPTEMBER 1995
CCCCC   CALL DPDELE(IBUGS2,IBUGQ,IFOUND,IERROR)
        CALL DPDELE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE DEMODULATION FREQUENCY CASE  **
C               *********************************************
C
      IF(ICOM.EQ.'DEMO')THEN
        CALL DPDEFR(IHARG,IARGT,ARG,NUMARG,DEFDMF,
     1              DEMOFR,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE DIMENSION CASE    **
C               **  TREAT THE REDIMENSION CASE  **
C               **********************************
C
      IF(ICOM.EQ.'DIME' .OR. ICOM.EQ.'REDI' .OR.
     1  (ICOM.EQ.'MATR' .AND. IHARG(1).EQ.'DIME'))THEN
        CALL DPDIME(IANS,IHARG,IARGT,IARG,NUMARG,IDEMXN,IDEMXC,
     1              IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1              IVALUE,VALUE,NUMNAM,MAXNAM,
     1              V,MAXNK,NUMN,MAXN,MAXNXT,
CCCCC               JANUARY 1998.  ADD FOLLOWING LINE
     1              MAXTOM,MAXROM,MAXCOM,MAXOBV,
     1              NUMCOL,MAXCOL,IFOUND,IERROR,IBUGS2,ISUBRO)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE DOUBLE PRECISION CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'DOUB')THEN
CCCCC   CHECK FOR CONFLICT WITH DOUBLY NON-CENTRAL F PROB PLOT.
CCCCC   SEPTEMBER 1994
CCCCC   CHECK FOR CONFLICT WITH DOUBLE EXPONENTIAL PROB PLOT.
CCCCC   OCTOBER 1995
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NONC')GOTO9000
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NON-')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WEIB')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GAMM')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SAMP')GOTO9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PARE')GOTO9000
        CALL DPDOUB(IHARG,NUMARG,IDEFPR,IHMXPR,
     1              IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE ECHO CASE  **
C               ***************************
C
      IF(ICOM.EQ.'ECHO')THEN
        CALL DPECSW(IHARG,NUMARG,
     1              IECHO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE EXIT  CASE  **
C               **  TREAT THE END   CASE  **
C               **  TREAT THE HALT  CASE  **
C               **  TREAT THE STOP  CASE  **
C               ****************************
C
      IF((ICOM.EQ.'END ' .AND. NUMARG.LE.0) .OR.
     1  ICOM.EQ.'EXIT' .OR.
     1  ICOM.EQ.'HALT' .OR. ICOM.EQ.'STOP' .OR.
     1  ICOM.EQ.'BYE ' .OR. ICOM.EQ.'QUIT')THEN
        CALL DPEXIT(ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE ERASE DELAY CASE  **
C               **********************************
C
      IF(ICOM.EQ.'ERAS')THEN
        CALL DPERDE(IHARG,IARGT,ARG,NUMARG,DEFERD,
     1              ERASDE,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')AGERDE=ERASDE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE FIT CONSTRAINT  CASE  **
C               **************************************
C
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'CONS')THEN
        CALL DPFICN(ICOM,IHARG,IHARG2,IARGT,ARG,NUMARG,
     1              IPARNC,IPANC2,IPAROP,
     1              PARLIM,PARLLM,PARULM,
     1              NUMCON,MAXCON,IFOUND,IERROR,IBUGS2)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  TREAT THE FIT ITERATIONS CASE  **
C               *************************************
C
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'ITER')THEN
        CALL DPFIIT(IHARG,IARGT,IARG,NUMARG,IDEFNI,
     1              IFITIT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE FIT POWER CASE  **
C               ********************************
C
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'POWE')THEN
        CALL DPFIPW(IHARG,IARGT,ARG,NUMARG,DEFFPW,
     1              FITPOW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE FIT STANDARD DEVIATION CASE  **
C               *********************************************
C
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'STAN')THEN
        CALL DPFISD(IHARG,IARGT,ARG,NUMARG,DEFFSD,
     1              FITSD,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE GRADS CASE  **
C               ****************************
C
C     (THE FOLLOWING IS COMMENTED OUT
C     BECAUSE THE ANGLE COMMAND IS NOW DONE
C     IN THE SUBROUTINE MAINDG)
C
CCCCC IF(ICOM.EQ.'GRAD')GOTO2100
CCCCC GOTO2199
C
C2100 CONTINUE
CCCCC CALL DPGRAD(IHARG,NUMARG,IDEFAU,
CCCCC1IANGLU,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C2199 CONTINUE
C
C               **************************************
C               **  TREAT THE HARDCOPY DELAY CASE   **
C               **************************************
C
      IF(ICOM.EQ.'HARD')THEN
        CALL DPHADE(IHARG,IARGT,ARG,NUMARG,DEFHAD,
     1              HARDDE,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')AGCODE=HARDDE
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE HELP CASE  **
C               ***************************
C
      IF(ICOM.EQ.'HELP')THEN
C
CCCCC THE FOLLOWING CALL WAS CHANGED JULY 1990
CCCCC CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,
CCCCC1IHELMX,
CCCCC1ICPREH,NCPREH,ICPOSH,NCPOSH,
CCCCC1IBUGS2,ISUBRO,IFOUND,IERROR)
C
CCCCC THE FOLLOWING CALL WAS INSERTED JULY 1990
CCCCC AND THEN COMMENTED OUT NOVEMBER 1991
CCCCC CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,
CCCCC1IHE1CO,IHE1AL,
CCCCC1IHE2CO,IHE2AL,
CCCCC1IHE3CO,IHE3AL,
CCCCC1IHE4CO,IHE4AL,
CCCCC1IHE5CO,IHE5AL,
CCCCC1IHE6CO,IHE6AL,
CCCCC1IHE7CO,IHE7AL,
CCCCC1IHE8CO,IHE8AL,
CCCCC1IHE9CO,IHE9AL,
CCCCC1IHELMX,
CCCCC1ICPREH,NCPREH,ICPOSH,NCPOSH,
CCCCC1IBUGS2,ISUBRO,IFOUND,IERROR)
C
CCCCC THE FOLLOWING CALL WAS CHANGED BACK NOVEMBER 1991
        CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,
     1              IHELMX,
     1              ICPREH,NCPREH,ICPOSH,NCPOSH,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE HOST CASE  **
C               ***************************
C
      IF(ICOM.EQ.'HOST'.AND.IHARG(1).NE.'LINK')THEN
        CALL DPHOST(IHARG,NUMARG,IDEFHO,
     1              IHOST,IHOST1,IHOST2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE HOST LINK CASE  **
C               ********************************
C
      IF(ICOM.EQ.'HOST'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LINK')GOTO2500
      IF(ICOM.EQ.'COMM'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LINK')GOTO2500
      IF(ICOM.EQ.'LINK')GOTO2500
      GOTO2599
C
 2500 CONTINUE
      IF(IHARG(1).EQ.'LINK')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IHARG(1)='LINK'
        IHARG2(1)='    '
      ENDIF
      CALL DPHOSL(IHARG,NUMARG,IDEFHL,
     1IHOSLI,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2599 CONTINUE
C
C               ****************************
C               **  TREAT THE KNOTS CASE  **
C               ****************************
C
      IF(ICOM.EQ.'KNOT')THEN
        CALL DPKNOT(IHARG,IHARG2,NUMARG,IDEFK1,IDEFK2,
     1              IKNOT1,IKNOT2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE MACRO CASE          **
C               **  TREAT THE END MACRO CASE      **
C               **  TREAT THE END OF MACRO CASE   **
C               **  TREAT THE CREATE CASE         **
C               **  TREAT THE END CREATE CASE     **
C               **  TREAT THE END OF CREATE CASE  **
C               ************************************
C
      IF(ICOM.EQ.'MACR' .OR. ICOM.EQ.'CREA')GOTO2700
      IF(ICOM.EQ.'END ' .AND. NUMARG.GE.1 .AND.
     1  (IHARG(1).EQ.'MACR' .OR. IHARG(1).EQ.'CREA'))GOTO2700
      IF(ICOM.EQ.'END ' .AND. NUMARG.GE.2 .AND. IHARG(1).EQ.'OF  ' .AND.
     1  (IHARG(2).EQ.'MACR' .OR. IHARG(2).EQ.'CREA'))GOTO2700
      GOTO2799
C
 2700 CONTINUE
      CALL DPMACR(ICOM,ICOM2,
CCCCC             THE FOLOWING LINE WAS AUGMENTED   AUGUST 1994
CCCCC1            IMACRO,IMACNU,IMACCS,
     1            IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,IMALEV,
     1            IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            IANSLC,IANS,IWIDTH,
     1            IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1            IOFILE,
     1            ILOOST,ILOOLI,NUMLIL,NUMLOS,
     1            IANSLO,IWIDLL,MAXCIL,MAXLIL,
     1            IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2799 CONTINUE
C
C               *******************************
C               **  TREAT THE OPERATOR CASE  **
C               **  TREAT THE CONSOLE  CASE  **
C               *******************************
C
      IF((ICOM.EQ.'CONS'.AND.ICOM2.EQ.'OLE ') .OR.
     1   ICOM.EQ.'OPER')THEN
        CALL DPOPMS(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE NAME CASE  **
C               ***************************
C
      IF(ICOM.EQ.'NAME' .OR. ICOM.EQ.'RENA')THEN
        CALL DPNAME(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1              IVALUE,VALUE,NUMNAM,MAXNAM,
     1              IVARLB,
     1              NUMCOL,MAXCOL,MAXN,IANS,IWIDTH,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  TREAT THE VARIABLE LABEL CASE  **
C               **  NAME CONFLICTS WITH "VARIANCE" **
C               **  COMMANDS.                      **
C               *************************************
C
      IF(ICOM.EQ.'VARI'.AND.ICOM2.EQ.'ABLE')THEN
        CALL DPVLAB(IHARG,IHARG2,IARG,NUMARG,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1              NUMNAM,MAXNAM,IVARLB,
     1              IANS,IANSLC,IWIDTH,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE POLYNOMIAL DEGREE CASE  **
C               **  TREAT THE DEGREE CASE             **
C               ****************************************
C
      IF((ICOM.EQ.'DEGR'.AND.ICOM2.EQ.'EE  ') .OR.
     1   (ICOM.EQ.'POLY'.AND.IHARG(1).NE.'AEPP'))THEN
C
C       CHECK FOR NAME CONFLICTS
C
        IF(NUMARG.GE.2. AND. IHARG(1).EQ.'MLE')GOTO3199
        IF(NUMARG.GE.3. AND. IHARG(1).EQ.'MAXI' .AND.
     1     IHARG(2).EQ.'LIKE')GOTO3199
        IF(IHARG(1).EQ.'FIT' .OR. IHARG(2).EQ.'FIT' .OR.
     1     IHARG(3).EQ.'FIT')GOTO3199
C
        CALL DPDEGR(IHARG,IARGT,IARG,NUMARG,IDEFDG,IDEG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
 3199 CONTINUE
C
C               ********************************
C               **  TREAT THE PRECISION CASE  **
C               ********************************
C
      IF(ICOM.EQ.'PREC' .AND. IHARG(1).NE.'PLOT')THEN
        CALL DPPREC(IHARG,NUMARG,IDEFPR,IHMXPR,
     1              IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE PRE-ERASE CASE  **
C               ********************************
C
      IF(ICOM.EQ.'PRE')THEN
        CALL DPPREE(IHARG,NUMARG,
     1              IERASW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *******************************
C               **  TREAT THE PRINTING CASE  **
C               *******************************
C
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'TING')THEN
         CALL DPPRSW(IHARG,NUMARG,
     1               IPRIN2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE PROBE CASE  **
C               ****************************
C
      IF(ICOM.EQ.'PROB' .OR. ICOM.EQ.'DUMP')THEN
        CALL DPPROB(ILISMX,IREPCH,IOSW,
     1              IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1              IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,
     1              IHELMX,IFTEXP,IFTORD,
     1              IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,
     1              IREARW,IWRIRW,NPLOTP,IPRITY,
     1              IUNFOF,IUNFNR,IUNFMC,IMACRO,IMALEV,
     1              IANSLO,ILOOST,ILOOLI,
     1              NUMIF,ISEED,
     1              IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************************
C               **  TREAT THE QUADRUPLE PRECISION CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'QUAD'.AND.ICOM2.EQ.'RUPL')THEN
        CALL DPQUAD(IHARG,NUMARG,IDEFPR,IHMXPR,
     1              IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE RADIANS CASE  **
C               ******************************
C
C     (THE FOLLOWING IS COMMENTED OUT
C     BECAUSE THE ANGLE COMMAND IS NOW DONE
C     IN THE SUBROUTINE MAINDG)
C
CCCCC IF(ICOM.EQ.'RADI')GOTO3700
CCCCC GOTO3799
C
C3700 CONTINUE
CCCCC CALL DPRADI(IHARG,NUMARG,IDEFAU,
CCCCC1IANGLU,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C3799 CONTINUE
C
C               ***************************
C               **  TREAT THE READ CASE  **
C               ***************************
C
      IF(ICOM.EQ.'READ')THEN
        INTINF=I1MACH(9)
        CALL DPREAD(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,
     1              IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,
     1              IREARW,ICOMCH,ICOMSW,
     1              IUNFOF,IUNFNR,IUNFMC,NUMRCM,
     1              IFCOLL,IFCOLU,
     1              IANSLO,ILOOST,ILOOLI,IREPCH,
     1              IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE STREAM READ CASE  **
C               **********************************
C
      IF(ICOM.EQ.'STRE' .AND. IHARG(1).EQ.'READ')THEN
        INTINF=I1MACH(9)
        CALL DPSTRE(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,
     1              IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,
     1              ICWRIF,NCWRIF,IREARW,ICOMCH,ICOMSW,
     1              IUNFOF,IUNFNR,IUNFMC,NUMRCM,
     1              IFCOLL,IFCOLU,
     1              IANSLO,ILOOST,ILOOLI,IREPCH,
     1              IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE RESET CASE  **
C               ****************************
C
      IF(ICOM.EQ.'CLEA' .AND. ICOM2.NE.'N   ' .AND.
     1   IHARG(1).NE.'CLIP')THEN
        ICOM='RESE'
        ICOM2='T   '
      ENDIF
C
      IF(ICOM.EQ.'RESE')THEN
        CALL DPRESE(IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE RESTORE           CASE  **
C               **  TREAT THE RESTORE VARIABLE  CASE  **
C               ****************************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REST')THEN
        IF(IHARG(1).EQ.'MEMO'.AND.IHARG2(1).EQ.'RY  ')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'ALL '.AND.IHARG2(1).EQ.'    ')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'VARI'.AND.IHARG2(1).EQ.'ABLE')THEN
          CALL DPREVA(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(ICOM.EQ.'REST')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************************
C               **  TREAT THE RETAIN CASE  **
C               **  TREAT THE KEEP   CASE  **
C               **  TREAT THE PACK   CASE  **
C               *****************************
C
      IF(ICOM.EQ.'RETA' .OR. ICOM.EQ.'KEEP' .OR.
     1   ICOM.EQ.'PACK')THEN
        CALL DPRETA(IBUGS2,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *********************************
C               **  TREAT THE ROW LIMITS CASE  **
C               *********************************
C
      IF(ICOM.EQ.'ROW')THEN
        CALL DPROWL(IHARG,IARGT,IARG,NUMARG,IDEFR1,IDEFR2,
     1              IFROW1,IFROW2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************************
C               **  TREAT THE TERMINATOR CHARACTOR CASE **
C               **  TREAT THE SEPARATOR CHARACTOR CASE  **
C               ******************************************
C
      IF((ICOM.EQ.'TERM'.AND.ICOM2.EQ.'INAT') .OR.
     1   (ICOM.EQ.'SEPA'.AND.ICOM2.EQ.'RATO'))THEN
        CALL DPTECH(IHARG,NUMARG,
     1              IDEFTC,ITERCH,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************************
C               **  TREAT THE CONTINUE   CHARACTOR CASE **
C               ******************************************
C
      IF(ICOM.EQ.'CONT' .AND. ICOM2.EQ.'INUE' .AND.
     1   IHARG(1).NE.'LOOP')THEN
        CALL DPCONC(IHARG,NUMARG,
     1              IDEFCC,
     1              ICONCH,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED JUNE 1989
C               ********************************************
C               **  TREAT THE REPLACEMENT  CHARACTOR CASE **
C               **  TREAT THE SUBSTITUTION CHARACTOR CASE **
C               ********************************************
C
      IF((ICOM.EQ.'REPL'.AND.ICOM2.EQ.'ACEM') .OR.
     1   (ICOM.EQ.'SUBS'.AND.ICOM2.EQ.'TITU'))THEN
        CALL DPRECH(IHARG,NUMARG,
     1              IBASLC,IREPCH,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED SEPTEMBER 2005
C               **************************************************
C               **  TREAT THE MACRO SUBSTITUTION CHARACTOR CASE **
C               **************************************************
C
      IF(ICOM.EQ.'MACR'.AND.IHARG(1).EQ.'SUBS'.AND.
     1   IHARG(2).EQ.'CHAR')THEN
        CALL DPREMA(IHARG,NUMARG,
     1              IMACSC,IDEFMS,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE SERIAL READ CASE  **
C               **********************************
C
      IF(ICOM.EQ.'SERI')THEN
C
CCCCC   MAY, 1990.  ADD ICOMCH, ICOMSW TO CALL LIST
CCCCC   MARCH, 1996.  ADD IMALEV TO CALL LIST
        INTINF=I1MACH(9)
        CALL DPSERI(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,
     1              IMACRO,IMACNU,IMACCS,IOSW,IMALEV,
     1              IREARW,ICOMCH,ICOMSW,
CCCCC FEBRAURY 2003.  ADD FOLLOWING LINE
     1              NUMRCM,
     1              IFCOLL,IFCOLU,
     1              IANSLO,ILOOST,ILOOLI,IREPCH,
     1              IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE SINGLE PRECISION CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'SING' .AND. IHARG(1).NE.'SAMP')THEN
        CALL DPSING(IHARG,NUMARG,IDEFPR,IHMXPR,
     1              IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE SKIP CASE  **
C               ***************************
C
      IF(ICOM.EQ.'SKIP')THEN
        CALL DPSKIP(IHARG,IARGT,IARG,NUMARG,IDEFSK,
     1              ISKIP,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE STATUS CASE  **
C               *****************************
C
CCCCC NOVEMBER 1997.  GUI STATUS (DON'T STORE IN SAVED COMMAND
CCCCC LIST)
CCCCC SEPTEMBER 2010. MAKE LS A SYNONYM FOR STATUS
C
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI ' .AND.
     1  (IHARG(1).EQ.'STAT' .OR. IHARG(1).EQ.'LS  '))THEN
        IF(NUMARG.GE.1.AND.
     1     (IHARG(1).EQ.'STAT' .OR. IHARG(1).EQ.'LS  '))THEN
             ISHIFT=1
             CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                   IBUGA2,IERROR)
             ICOM='STAT'
             ICOM2='US  '
             IFEEDB=IGUIFB
        ENDIF
      ENDIF
      IF(ICOM.EQ.'STAT' .OR. ICOM.EQ.'LS  ')THEN
        CALL DPSTAT(ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IFEEDB=IFEESV
C
C               ****************************
C               **  TREAT THE TIME  CASE  **
C               **  TREAT THE CLOCK CASE  **
C               ****************************
C
      IF(ICOM.EQ.'TIME' .OR. ICOM.EQ.'CLOC' .OR.
     1   ICOM.EQ.'DATE')THEN
        CALL DPTIME(CURRTI,NCURRT,CURRDA,NCURRD,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
           DO5110I=1,NCURRT
              IC4(1:4)='    '
              IC4(1:1)=CURRTI(I:I)
              IFUTMP(I)=IC4(1:4)
5110       CONTINUE
           CALL UPDATF('CURR','TIME',IFUTMP,NCURRT,'CHAD','NO  ',
     1                 IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1                 NUMNAM,MAXNAM,IANS,IWIDTH,ILISTL,NEWNAM,MAXNAM,
     1                 IFUNC,NUMCHF,MAXCHF,IBUGS2,ILOCN,IFOUNN,IERRON)
           DO5120I=1,NCURRD
              IC4(1:4)='    '
              IC4(1:1)=CURRDA(I:I)
              IFUTMP(I)=IC4(1:4)
5120       CONTINUE
           CALL UPDATF('CURR','DATE',IFUTMP,NCURRD,'CHAD','NO  ',
     1                 IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1                 NUMNAM,MAXNAM,IANS,IWIDTH,ILISTL,NEWNAM,MAXNAM,
     1                 IFUNC,NUMCHF,MAXCHF,IBUGS2,ILOCN,IFOUNN,IERRON)
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE CPU TIME  CASE  **
C               ********************************
C
      IF(ICOM.EQ.'CPU ')THEN
        CALL DPCPU(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1             ATIME,
     1             IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IH='CPUT'
          IH2='IME '
          VALUE0=ATIME
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGS2,IERROR)
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE PROCESS-ID  CASE  **
C               **********************************
C
      IFOUND='NO'
      IF(ICOM.EQ.'PID ' .OR.
     1   (ICOM.EQ.'PROC' .AND. IHARG(1).EQ.'ID'))THEN
        CALL DPPID(IPID,IBUGS2,ISUBRO,IFOUND,IERROR)
    
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO'.AND.IPID.GT.0)THEN
          IH='PID '
          IH2='    '
          VALUE0=REAL(IPID)
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGS2,IERROR)
        ENDIF
      ENDIF
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ********************************
C               **  TREAT THE PWD       CASE  **
C               ********************************
C
      IF(ICOM.EQ.'PWD ' .OR.
     1  (ICOM.EQ.'GETC' .AND. ICOM2.EQ.'WD  ') .OR.
     1  (ICOM.EQ.'CURR' .AND. IHARG(1).EQ.'DIRE'))THEN
        MAXTMP=255
        CALL DPPWD(CURDIR,MAXTMP,ICNT,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IH='CURD'
          IH2='IR  '
          NEWNAM='YES'
          DO5130I=1,NUMNAM 
            I2=I
            IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
              IF(IUSE(I2).EQ.'F')THEN
                NEWNAM='NO'
                GOTO5139
              ELSE
                NEWNAM='NULL'
              ENDIF
            ENDIF
 5130     CONTINUE
 5139     CONTINUE
C
          IF(NEWNAM.NE.'NULL')THEN
            ILISTL=NUMNAM+1
            DO5140I=1,ICNT
              IFUNC9(I)=' '
              IFUNC9(I)(1:1)=CURDIR(I:I)
 5140       CONTINUE
            CALL DPINFU(IFUNC9,ICNT,IHNAME,IHNAM2,IUSE,IN,
     1                  IVSTAR,IVSTOP,
     1                  NUMNAM,IANS,IWIDTH,IH,IH2,ILISTL,
     1                  NEWNAM,MAXNAM,
     1                  IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
          ENDIF
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD CLEAR  CASE  **
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'CLEA') .OR.
     1   (ICOM.EQ.'CLEA' .AND. IHARG(1).EQ.'CLIP'))THEN
        CALL DPCLI3(IBUGS2,ISUBRO,IERROR)
        IFOUND='YES'
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD LOOP   CASE  **
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'LOOP') .OR.
     1   (ICOM.EQ.'LOOP' .AND. IHARG(1).EQ.'CLIP'))THEN
        IF(NUMARG.EQ.1)THEN
          ICLILO='ON'
          ICLIL2=0
          IFOUND='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD LOOP END CASE**
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'LOOP' .AND.
     1    IHARG(2).EQ.'END') .OR.
     1   (ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'END ' .AND.
     1    IHARG(2).EQ.'LOOP') .OR.
     1   (ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'END ' .AND.
     1    IHARG(2).EQ.'OF  ' .AND. IHARG(3).EQ.'LOOP'))THEN
        ICLILO='OFF'
        ICLIL2=0
        ICLIFL='OFF'
        ICLILN=0
        IFOUND='YES'
        CALL DPCLI3(IBUGS2,ISUBRO,IERROR)
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,5150)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD PAUSE  CASE  **
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'PAUS') .OR.
     1   (ICOM.EQ.'PAUS' .AND. IHARG(1).EQ.'CLIP'))THEN
        ICLIFL='PAUS'
        IF(ICLILO.EQ.'ON')ICLILO='PAUS'
        IFOUND='YES'
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,5150)
 5150     FORMAT('COMMANDS WILL NOW BE ENTERED FROM KEYBOARD')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD RESUME CASE  **
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'RESU') .OR.
     1   (ICOM.EQ.'RESU' .AND. IHARG(1).EQ.'CLIP'))THEN
        ICLIFL='ON'
        IF(ICLILO.EQ.'PAUS')ICLILO='ON'
        IFOUND='YES'
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE CLIPBOARD RUN    CASE  **
C               ***************************************
C
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'RUN ') .OR.
     1   (ICOM.EQ.'RUN ' .AND. IHARG(1).EQ.'CLIP') .OR.
     1     (ICOM.EQ.'CALL' .AND. IHARG(1).EQ.'CLIP') .OR.
     1      ICOM.EQ.'CB' .OR.
     1   (NUMARG.EQ.0 .AND. ICOM.EQ.'CLIP'))THEN
        ICLIFL='ON'
        ICLILN=0
        IFOUND='YES'
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE TRIPLE PRECISION CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'TRIP')THEN
        CALL DPTRIP(IHARG,NUMARG,IDEFPR,IHMXPR,
     1              IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE WEIGHTS CASE  **
C               ******************************
C
      IF(ICOM.EQ.'WEIG')THEN
        CALL DPWEIG(IHARG,IHARG2,NUMARG,IDEFW1,IDEFW2,
     1              IWEIG1,IWEIG2,IWEIGH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************************
C               **  TREAT THE ORTHOGONAL DISTANCE ERROR CASE  **
C               ************************************************
C
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'ERRO')
     1    THEN
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPORER(IHARG,IHARG2,NUMARG,
     1                IODRE1,IODRE2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               ************************************************
C               **  TREAT THE ORTHOGONAL DISTANCE DELTA CASE  **
C               ************************************************
C
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'DELT')
     1    THEN
          ICASOD='DELT'
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPORDE(IHARG,IHARG2,NUMARG,
     1                IODRD1,IODRD2,IODRD3,IODRD4,
     1                IWEIN1,IWEIN2,
     1                ICASOD,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'Y   ')
     1    THEN
          ICASOD='Y   '
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPORDE(IHARG,IHARG2,NUMARG,
     1                IODRD1,IODRD2,IODRD3,IODRD4,
     1                IWEIN1,IWEIN2,
     1                ICASOD,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               **************************************
C               **  TREAT THE CLASS ... LOWER CASE  **
C               **  TREAT THE CLASS ... UPPER CASE  **
C               **  TREAT THE CLASS ... WIDTH CASE  **
C               **************************************
C
      IF(ICOM.EQ.'CLAS')THEN
        CALL DPCLLO(IHARG,IARGT,ARG,NUMARG,
     1              CLLIMI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPCLUP(IHARG,IARGT,ARG,NUMARG,
     1              CLLIMI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
        CALL DPCLWI(IHARG,IARGT,ARG,NUMARG,
     1              CLWIDT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE WRITE CASE  **
C               **  TREAT THE PRINT CASE  **
C               ****************************
C
CCCCC NOVEMBER 1997.  GUI PRINT/WRITE (DON'T STORE IN SAVED COMMAND
CCCCC LIST)
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PRIN'.OR.
     1     IHARG(1).EQ.'WRIT')THEN
             ISHIFT=1
             CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                   IBUGA2,IERROR)
             ICOM='WRIT'
             ICOM2='E   '
             IFEEDB=IGUIFB
        ENDIF
      ENDIF
      IF(ICOM.EQ.'WRIT')GOTO5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T   ')GOTO5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T1  ')GOTO5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T2  ')GOTO5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T3  ')GOTO5800
      IF(ICOM.EQ.'HEAD'.AND.ICOM2.EQ.'    ')GOTO5800
      IF(ICOM.EQ.'TAIL'.AND.ICOM2.EQ.'    '.AND.
     1   IHARG(1).NE.'AREA')GOTO5800
      GOTO5899
C
 5800 CONTINUE
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
CCCCC1IHARG2(1).EQ.'MALS')GOTO5899
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
CCCCC1IHARG2(1).EQ.'MAL')GOTO5899
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FORM'.AND.
CCCCC1IHARG2(1).EQ.'AT')GOTO5899
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REWI'.AND.
CCCCC1IHARG2(1).EQ.'ND')GOTO5899
C
      CALL DPWRIT(IMACRO,IMACNU,IMACCS,
     1            IFORSW,ICWRIF,NCWRIF,
     1            IWRIRW,
     1            IFORWI,IFORWR,MAXNWI,
     1            IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
C
      IFEEDB=IFEESV
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5899 CONTINUE
C
C               ******************************
C               **  TREAT THE COMMENT CASE  **
C               **  TREAT THE    .    CASE  **
C               ******************************
C
C  MAY, 1990.  SOFT-CODE THE COMMENT CHARACTER.  ALSO, A COMMENT
C  CHARACTER AND A COMMENT CHECK COMMAND WERE ADDED.  ALWAYS TREAT
C  PERIOD AS COMMENT ON COMMAND LINE.
C
      IF(ICOM.EQ.'.' .OR. ICOM.EQ.ICOMCH .OR. ICOM.EQ.'COMM')THEN
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'CHAR')GOTO5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'CHEC')GOTO5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'COEF')GOTO5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(2).EQ.'COEF')GOTO5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'WEIB' .AND.
     1    IHARG(2).EQ.'SHAP')GOTO5999
        CALL DPDOT(IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
 5999 CONTINUE
C
C               *******************************
C               **  TREAT THE FEEDBACK CASE  **
C               *******************************
C
      IF(ICOM.EQ.'FEED')THEN
        CALL DPFEED(IHARG,NUMARG,
     1              IFEED2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***********************************
C               **  TREAT THE FILTER WIDTH CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'FILT')THEN
        CALL DPFIWI(IHARG,IARGT,ARG,NUMARG,DEFFW,
     1              FILWID,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE DEFAULT COMMAND CASE  **
C               **************************************
C
      IF(ICOM.EQ.'DEFA')THEN
        CALL DPDECO(IANS,IWIDTH,IHARG,NUMARG,
     1              IDEFCM,IWIDDC,IDEFC,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE BUGS CASE  **
C               ***************************
C
      IF(ICOM.EQ.'BUGS' .OR. ICOM.EQ.'BUG ')THEN
        CALL DPBUGS(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************
C               **  TREAT THE MAIL CASE  **
C               ***************************
C
CCCCC IF(ICOM.EQ.'MAIL')GOTO6700
CCCCC GOTO6799
C
C6700 CONTINUE
CCCCC CALL DPMAIL(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6799 CONTINUE
C
C               ***************************
C               **  TREAT THE NEWS CASE  **
C               ***************************
C
      IF(ICOM.EQ.'NEWS')THEN
        CALL DPNEWS(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE QUERY CASE  **
C               ****************************
C
CCCCC IF(ICOM.EQ.'QUER')GOTO6900
CCCCC IF(ICOM.EQ.'QUES')GOTO6900
CCCCC IF(ICOM.EQ.'MESS')GOTO6900
CCCCC GOTO6999
C
C6900 CONTINUE
CCCCC CALL DPQUER(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C6999 CONTINUE
C
C               ****************************
C               **  TREAT THE SET   CASE  **
C               ****************************
C
      IF(ICOM.EQ.'SET ')GOTO7110
C
CCCCC IF(ICOM.EQ.'READ')GOTO7105
CCCCC IF(ICOM.EQ.'WRIT')GOTO7105
CCCCC IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T   ')GOTO7105
CCCCC GOTO7199
C
C7105 CONTINUE
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
CCCCC1IHARG2(1).EQ.'MALS')GOTO7110
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
CCCCC1IHARG2(1).EQ.'MAL')GOTO7110
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FORM'.AND.
CCCCC1IHARG2(1).EQ.'AT')GOTO7110
CCCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REWI'.AND.
CCCCC1IHARG2(1).EQ.'ND')GOTO7110
      GOTO7199
C
 7110 CONTINUE
      CALL DPSET(ILISMX,IREPCH,IOSW,
     1IPPDE1,IPPDE2,
     1IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,
CCCCC AUGUST 1995.  ADD IFTORD
CCCCC1IHELMX,IFTEXP,
     1IHELMX,IFTEXP,IFTORD,
     1IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,
     1IREARW,IWRIRW,
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
     1NPLOTP,
CCCCC THE FOLLOWING LINE WAS ADDED   FEBRUARY 1993
     1IPRITY,
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1995
     1IUNFOF,IUNFNR,IUNFMC,
CCCCC FOLLOWING LINE ADD MARCH 1996
CCCCC1IRHSTG,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7199 CONTINUE
C
C               ********************************
C               **  TREAT THE IMPLEMENT CASE  **
C               ********************************
C
      IF(ICOM.EQ.'IMPL')THEN
        CALL DPIMPL(IHARG,IARGT,IARG,NUMARG,
     1              IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,
     1              ISQUAR,
     1              PXMIN,PYMIN,PXMAX,PYMAX,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE REWIND CASE  **
C               *****************************
C
CCCCC IF(ICOM.EQ.'REWI')GOTO7300
CCCCC GOTO7399
CCCCC
C7300 CONTINUE
CCCCC CALL DPREWI(IBUGS2,IBUGQ,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C7399 CONTINUE
C
C               ******************************
C               **  TREAT THE ENDFILE CASE  **
C               ******************************
C
CCCCC IF(ICOM.EQ.'ENDF')GOTO7400
CCCCC GOTO7499
CCCCC
C7400 CONTINUE
CCCCC CALL DPENDF(IBUGS2,IBUGQ,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C7499 CONTINUE
C
C               *****************************
C               **  TREAT THE RELEASE CASE **
C               *****************************
C
CCCCC IF(ICOM.EQ.'RELE')GOTO7500
CCCCC IF(ICOM.EQ.'CLOS')GOTO7500
CCCCC IF(ICOM.EQ.'FREE')GOTO7500
CCCCC GOTO7599
CCCCC
C7500 CONTINUE
CCCCC CALL DPREWI(IBUGS2,IBUGQ,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C7599 CONTINUE
C
C               ***************************
C               **  TREAT THE SEED CASE  **
C               ***************************
C
      IF(ICOM.EQ.'SEED')THEN
        CALL DPSEED(IHARG,IARGT,IARG,NUMARG,IDEFSE,
     1              ISEED,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE THE PROPORTION LIMITS   CASE  **
C               **  = THE ANOP LIMITS     CASE  **
C               **************************************
C
      IF(ICOM.EQ.'PROP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LIMI'.AND.IHARG2(1).EQ.'TS  ')GOTO8100
      IF(ICOM.EQ.'PROP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'REGI'.AND.IHARG2(1).EQ.'ON  ')GOTO8100
      IF(ICOM.EQ.'ANOP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LIMI'.AND.IHARG2(1).EQ.'TS  ')GOTO8100
      IF(ICOM.EQ.'ANOP'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'REGI'.AND.IHARG2(1).EQ.'ON  ')GOTO8100
      GOTO8199
C
 8100 CONTINUE
      CALL DPANOL(IHARG,IARGT,ARG,NUMARG,DEFAL1,DEFAL2,
     1ANOPL1,ANOPL2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8199 CONTINUE
C
C               ****************************
C               **  TREAT THE FENCE CASE  **
C               ****************************
C
      IF(ICOM.EQ.'FENC')THEN
        CALL DPFENC(IHARG,NUMARG,
     1              IFENSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE PAUSE CASE  **
C               ****************************
C
      IF(ICOM.EQ.'PAUS' .AND. NUMARG.EQ.0)THEN
        CALL DPPAUS(IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE SLEEP CASE  **
C               ****************************
C
      IF(ICOM.EQ.'SLEE')THEN
        CALL DPSLEE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1              IBUGD2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE APPEND  CASE  **
C               ******************************
C
      IF(ICOM.EQ.'APPE' .OR. ICOM.EQ.'AUGM')THEN
        CALL DPAPPE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE EXTEND  CASE  **
C               ******************************
C
      IF(ICOM.EQ.'EXTE')THEN
        CALL DPEXTE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE SUGGESTION  CASE      **
C               **  TREAT THE RECOMMENDATION  CASE  **
C               **  TREAT THE PROGRAM  CASE         **
C               **  TREAT THE CODE  CASE            **
C               **  TREAT THE EXPERT  CASE          **
C               **************************************
C
CCCCC IF(ICOM.EQ.'SUGG')GOTO8600
CCCCC IF(ICOM.EQ.'RECO')GOTO8600
CCCCC IF(ICOM.EQ.'PROG')GOTO8600
CCCCC IF(ICOM.EQ.'CODE')GOTO8600
CCCCC IF(ICOM.EQ.'EXPE')GOTO8600
CCCCC GOTO8699
CCCCC
C8600 CONTINUE
CCCCC CALL DPSUPR(IHARG,IHARG2,NUMARG,
CCCCC1ITOPIC,
CCCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C8699 CONTINUE
CCCCC
CCCCC           **************************************
CCCCC           **  TREAT THE GO          CASE      **
CCCCC           **************************************
CCCCC
CCCCC IF(ICOM.EQ.'GO')GOTO8700
CCCCC GOTO8799
CCCCC
C8700 CONTINUE
CCCCC CALL DPWRPF(IPRONU,IPROFS,IPROST,
CCCCC1ITOPIC,
CCCCC1IHARG,IHARG2,NUMARG,
CCCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C8799 CONTINUE
CCCCC
CCCCC           **************************************
CCCCC           **  TREAT THE CONCLUSIONS CASE      **
CCCCC           **************************************
CCCCC
CCCCC IF(ICOM.EQ.'CONC')GOTO8800
CCCCC GOTO8899
CCCCC
C8800 CONTINUE
CCCCC CALL DPLICF(ICONNU,ICONFS,ICONST,
CCCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C8899 CONTINUE
CCCCC
CCCCC
C               **************************************
C               **  TREAT THE ROOT ACCURACY  CASE   **
C               **************************************
C
      IF(ICOM.EQ.'ROOT'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'ACCU')THEN
        CALL DPROAC(IHARG,IARGT,ARG,NUMARG,DEFRAC,
     1              ROOTAC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC           ***************************
CCCCC           **  TREAT THE MENU CASE  **
CCCCC           ***************************
CCCCC
CCCCC IF(ICOM.EQ.'MENU')GOTO9100
CCCCC GOTO9199
CCCCC
C9100 CONTINUE
CCCCC CALL DPMENU(IMENNU,IMENFS,IMENST,
CCCCC1IHARG,NUMARG,IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C9199 CONTINUE
CCCCC
C               *****************************
C               **  TREAT THE PROMPT CASE  **
C               *****************************
C
      IF(ICOM.EQ.'PROM')THEN
        CALL DPPROM(IHARG,NUMARG,IPROSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************
C               **  TREAT THE LIST (COMMANDS) CASE  **
C               **  (SAME AS THE RECALL CASE)       **
C               **************************************
C
CCCCC THE FOLLOWING PARAGRAPH WAS REWRITTEN    SEPTEMBER 1993
C
      IF(NUMARG.LE.0)THEN
        IF(ICOM.EQ.'LIST' .OR. ICOM.EQ.'TYPE' .OR.
     1     ICOM.EQ.'L'    .OR. ICOM.EQ.'RECA' .OR.
     1     ICOM.EQ.'V'    .OR. ICOM.EQ.'PREV' .OR.
     1    (ICOM.EQ.'VIEW' .AND. IHARG(1).NE.'PLOT'))THEN
          CALL DPLICO(IHARG,NUMARG,IANSSV,IREPMX,ILISMX,IPOINT,
     1                IHELMX,
     1                ICPREH,NCPREH,ICPOSH,NCPOSH,
     1                IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               ****************************
C               **  TREAT THE LIST  CASE  **
C               ****************************
C
CCCCC APRIL 1997: CHECK FOR CONFLICT WITH LIST GRAPH, LIST PLOT,
CCCCC             VIEW PLOTS, AND VIEW GRAPHS.
C
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.
     1IHARG2(1).EQ.'    ')GOTO9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.
     1IHARG2(1).EQ.'S  ')GOTO9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.
     1IHARG2(1).EQ.'H  ')GOTO9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.
     1IHARG2(1).EQ.'HS ')GOTO9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.
     1IHARG2(1).EQ.'    ')GOTO9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.
     1IHARG2(1).EQ.'S  ')GOTO9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.
     1IHARG2(1).EQ.'H  ')GOTO9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.
     1IHARG2(1).EQ.'HS ')GOTO9499
C
      IF((ICOM.EQ.'LIST' .OR. ICOM.EQ.'L   ' .OR. ICOM.EQ.'VIEW' .OR.
     1   ICOM.EQ.'PREV' .OR. ICOM.EQ.'NLIS' .OR. ICOM.EQ.'NTYP' .OR.
     1   ICOM.EQ.'NVIE' .OR. ICOM.EQ.'NPRE') .AND.
     1   IHARG(1).NE.'=   ')THEN
CCCCC    2 LINES OF ARGS (IHELMX THROUGH NCPOSH) WERE ADDED JULY 1989
CCCCC    THE FOLLOWING LINE WAS CHANGED NOVEMBER 1989
CCCCC   CALL DPLIST(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,IARGT,
CCCCC1              IARG,ARG,NUMARG,
        CALL DPLIST(ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,
CCCCC1              IHELMX,
     1              ILISMX,
     1              ICPREH,NCPREH,ICPOSH,NCPOSH,
     1              ILOOST,ILOOLI,NUMLIL,NUMLOS,
     1              IANSLO,IWIDLL,
     1              IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
 9499 CONTINUE
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               **********************************
C               **  TREAT THE SAVE PLOT   CASE  **
C               **********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GOTO9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GOTO9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GOTO9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GOTO9500
      IF(ICOM.EQ.'SG  ')GOTO9500
      IF(ICOM.EQ.'SP  ')GOTO9500
      GOTO9509
C
 9500 CONTINUE
      CALL DPSAPL(IANSLC,IWIDTH,IHARG,NUMARG,
CCCCC1            IANSSV,IREPMX,IPOINT,
     1            IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO9509
C
C
C               ************************************
C               **  TREAT THE SAVE MEMORY CASE    **
C               **  TREAT THE SAVE VARIABLE CASE  **
C               ************************************
C
 9509 CONTINUE
      IF(NUMARG.GE.1 .AND. ICOM.EQ.'SAVE')THEN
        IF((IHARG(1).EQ.'MEMO' .AND. IHARG2(1).EQ.'RY  ') .OR.
     1     (IHARG(1).EQ.'ALL ' .AND. IHARG2(1).EQ.'    '))THEN
          CALL DPSAVE(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'VARI' .AND. IHARG2(1).EQ.'ABLE')THEN
          CALL DPSAVA(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               **************************************
C               **  TREAT THE GUI SAVE PLOT CONTROL **
C               **************************************
C
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI')THEN
        IFEEDB=IGUIFB
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SAVE'.AND.
     1     IHARG(2).EQ.'PLOT'.AND.IHARG(3).EQ.'CONT')THEN
           CALL DPSAPC(IBUGS2,ISUBRO,IFOUND,IERROR)
           IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
      IFEEDB=IFEESV
C
C               **************************************
C               **  TREAT THE SAVE (COMMANDS) CASE  **
C               **************************************
C
CCCCC DECEMBER 1993.  CHECK FOR CONFLICT WITH S CHART COMMAND
C
      IF(ICOM.EQ.'SAVE' .OR. ICOM.EQ.'S   ' .AND.
     1   IHARG(1).NE.'CONT' .AND. IHARG(1).NE.'CHAR' .AND.
     1   IHARG(1).NE.'=   ')THEN
        CALL DPSACO(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1              IANSSV,IREPMX,IPOINT,ISACNC,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE AUTOPLOT (SWITCH) CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'AUTO'.AND.ICOM2.EQ.'PLOT')THEN
        CALL DPAUPL(IHARG,NUMARG,
     1              IAUTSW,IAUTEX,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **********************************
C               **  TREAT THE CURSOR SIZE CASE  **
C               **********************************
C
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SIZE')GOTO10100
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'HEIG')GOTO10100
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SIZE')GOTO10100
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'HEIG')GOTO10100
      GOTO10199
C
10100 CONTINUE
      CALL DPCUSZ(IHARG,IARGT,ARG,NUMARG,DEFCSZ,
     1ACURSZ,IFOUND,IERROR)
      PDIAHE=ACURSZ
      PDIAWI=PDIAHE/2.0
      IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')GOTO10110
      GOTO10119
C
10110 CONTINUE
CCCCC ICOPSW='OFF'
CCCCC NUMCOP=0
CCCCC CALL DPCLPL(ICOPSW,NUMCOP,
CCCCC1PGRAXF,PGRAYF,
CCCCC1IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
CCCCC1PDIAHE,PDIAWI,PDIAVG,PDIAHG)
CCCCC CALL DPCLDE
C0119 CONTINUE
      IF(NUMDEV.LE.0)GOTO10119
      DO10112IDEVIC=1,NUMDEV
      IF(IDPOWE(IDEVIC).EQ.'OFF')GOTO10112
      IMANUF=IDMANU(IDEVIC)
      IMODEL=IDMODE(IDEVIC)
      IMODE2=IDMOD2(IDEVIC)
      IMODE3=IDMOD3(IDEVIC)
      IGCONT=IDCONT(IDEVIC)
      IGCOLO=IDCOLO(IDEVIC)
      IGFONT=IDFONT(IDEVIC)
      NUMVPP=IDNVPP(IDEVIC)
      NUMHPP=IDNHPP(IDEVIC)
      ANUMVP=NUMVPP
      ANUMHP=NUMHPP
      IGUNIT=IDUNIT(IDEVIC)
      PCHSCA=PDSCAL(IDEVIC)
C
      CALL DPOPDE
      IBELSJ='OFF'
      NUMRIJ=0
      IERASJ='OFF'
      IBACCJ=IBACCO
      CALL DPOPPL(IGRASW,IBELSJ,NUMRIJ,IERASJ,IBACCJ,IBACC2)
      ICOPSJ='OFF'
      NUMCOJ=0
      CALL DPCLPL(ICOPSJ,NUMCOJ,
     1            PGRAXF,PGRAYF,
     1            IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1            PDIAHE,PDIAWI,PDIAVG,PDIAHG)
      CALL DPCLDE
10112 CONTINUE
10119 CONTINUE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
10199 CONTINUE
C
C               *************************************
C               **  TREAT THE CURSOR SPACING CASE  **
C               *************************************
C
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SPAC')GOTO10200
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'GAP')GOTO10200
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SPAC')GOTO10200
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'GAP')GOTO10200
      GOTO10299
C
10200 CONTINUE
      DEFCSP=0.0
      CALL DPCUSP(IHARG,IARGT,ARG,NUMARG,DEFCSP,
     1PDIAVG,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
10299 CONTINUE
C
C               *****************************************
C               **  TREAT THE CURSOR COORDINATES CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'COOR')GOTO10300
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LOCA')GOTO10300
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'COOR')GOTO10300
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'LOCA')GOTO10300
      GOTO10399
C
10300 CONTINUE
      CALL DPCUCO(IHARG,IARGT,ARG,NUMARG,PDIAYC,
     1PDIAY2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')GOTO10310
      GOTO10319
C
10310 CONTINUE
      IF(NUMDEV.LE.0)GOTO10319
      DO10312IDEVIC=1,NUMDEV
      IF(IDPOWE(IDEVIC).EQ.'OFF')GOTO10312
      IMANUF=IDMANU(IDEVIC)
      IMODEL=IDMODE(IDEVIC)
      IMODE2=IDMOD2(IDEVIC)
      IMODE3=IDMOD3(IDEVIC)
      IGCONT=IDCONT(IDEVIC)
      IGCOLO=IDCOLO(IDEVIC)
      IGFONT=IDFONT(IDEVIC)
      NUMVPP=IDNVPP(IDEVIC)
      NUMHPP=IDNHPP(IDEVIC)
      ANUMVP=NUMVPP
      ANUMHP=NUMHPP
      IGUNIT=IDUNIT(IDEVIC)
      PCHSCA=PDSCAL(IDEVIC)
C
      CALL DPOPDE
      IBELSJ='OFF'
      NUMRIJ=0
      IERASJ='OFF'
      IBACCJ='JUNK'
      CALL DPOPPL(IGRASW,IBELSJ,NUMRIJ,IERASJ,IBACCJ,IBACC2)
      ICOPSJ='OFF'
      NUMCOJ=0
      CALL DPCLPL(ICOPSJ,NUMCOJ,
     1            PGRAXF,PGRAYF,
     1            IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1            PDIAHE,PDIAWI,PDIAVG,PDIAHG)
      CALL DPCLDE
10312 CONTINUE
10319 CONTINUE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
10399 CONTINUE
C
C               **************************************
C               **  TREAT THE PREPOST  DEVICE CASE  **
C               **************************************
C
      IF(ICOM.EQ.'PREP'.AND.ICOM2.EQ.'OST')THEN
        CALL DPPRPO(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IPPDE1,IPPDE2,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE SEARCH CASE **
C               ****************************
C
      IF(ICOM.EQ.'SEAR' .OR. ICOM.EQ.'?   ' .OR.
     1   ICOM.EQ.'??? ' .OR. ICOM.EQ.'GREP' .OR.
     1  (ICOM.EQ.'FIND' .AND. ICOM2.EQ.'STR '))THEN
        ISEART='1LIN'
        IF(ICOM2.EQ.'CHB')ISEART='BLAN'
        IF(ICOM2.EQ.'CHBL')ISEART='BLAN'
        IF(ICOM2.EQ.'CHD')ISEART='----'
        IF(ICOM2.EQ.'CHDA')ISEART='----'
        IF(ICOM.EQ.'GREP')ISEART='GREP'
        IF(ICOM.EQ.'FIND' .AND. ICOM2.EQ.'STR ')ISEART='FIND'
CCCCC   THE FOLLOWING LINE WAS ADDED      JANUARY 1994
        IF(ICOM2.EQ.'CH1 ')ISEART='FIRS'
        CALL DPSEAR(IANS,IANSLC,IWIDTH,ICOM,IHARG,IHARG2,NUMARG,ISEART,
CCCCC   FEBRUARY 2003: ADD FOLLOWING LINE
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE LOWESS FRACTION  CASE   **
C               ****************************************
C
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'FRAC')GOTO10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'DECI')GOTO10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'PROP')GOTO10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'PERC')GOTO10600
      GOTO10699
C
10600 CONTINUE
C
      CALL DPLOFR(IHARG,IARGT,ARG,NUMARG,
     1ALOWFR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
10699 CONTINUE
C
C               *********************************************
C               **  TREAT THE KERNEL DENSITY WIDTH  CASE   **
C               *********************************************
C
      IF(ICOM.EQ.'KERN')THEN
        IF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'WIDT')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPKDWI(IHARG,IARGT,ARG,NUMARG,
     1                PKDEWI,DEFKWI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'WIDT')THEN
          CALL DPKDWI(IHARG,IARGT,ARG,NUMARG,
     1                PKDEWI,DEFKWI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
        IF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'POIN')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,
     1                IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'POIN')THEN
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,
     1                IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'NUMB'.AND.
     1         IHARG(3).EQ.'OF  '.AND.IHARG(4).EQ.'POIN')THEN
          ISHIFT=3
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,
     1                IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSEIF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'NUMB'.AND.
     1         IHARG(3).EQ.'POIN')THEN
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,
     1                IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *********************************************
C               **  TREAT THE BOOSTRAP SAMPLE SIZE  CASE   **
C               *********************************************
C
      IF(ICOM.EQ.'BOOT'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SAMP')GOTO10700
      IF(ICOM.EQ.'BOOT'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'SIZE')GOTO10700
      GOTO10799
C
10700 CONTINUE
C
      CALL DPBOSS(IHARG,IARGT,IARG,NUMARG,
     1IBOOSS,IDEBOO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
10799 CONTINUE
C
C               ***************************
C               **  TREAT THE SYSTEM CASE**
C               ***************************
C
      IF(ICOM.EQ.'SYST' .OR. ICOM.EQ.'DOS' .OR.
     1   ICOM.EQ.'UNIX' .OR. ICOM.EQ.'VMS' .OR.
     1   ICOM.EQ.'OS')THEN
        CALL DPSYST(IANS,IANSLC,IWIDTH,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  TREAT THE RSCRIPT/PYTHON CASE  **
C               *************************************
C
      IF(ICOM.EQ.'RSCR' .OR. ICOM.EQ.'PYTH')THEN
        CALL DPEXRP(IANS,IANSLC,IWIDTH,ICOM,ICOM2,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED JUNE 1989
C               *************************************
C               **  TREAT THE CAPTURE CASE         **
C               **  TREAT THE END CAPTURE CASE     **
C               **  TREAT THE END OF CAPTURE CASE  **
C               **  TREAT THE REDIRECT CASE        **
C               **  TREAT THE END REDIRECT CASE    **
C               **  TREAT THE END OF REDIRECT CASE **
C               *************************************
C
      IF(ICOM.EQ.'CAPT')GOTO11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CAPT')GOTO11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'OF  '.AND.
     1IHARG(2).EQ.'CAPT')GOTO11100
      IF(ICOM.EQ.'REDI')GOTO11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'REDI')GOTO11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'OF  '.AND.
     1IHARG(2).EQ.'REDI')GOTO11100
      GOTO11199
C
11100 CONTINUE
      CALL DPCAPT(ICOM,ICOM2,
CCCCC JUNE 2002.  ADD ICAPTY
CCCCC JANUARY 2006.  ADD ICAPSC
     1ICAPSW,ICAPTY,ICAPSC,IPRDEF,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,IANSLC,IANS,IWIDTH,
     1IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1IOFILE,
CCCCC JUNE 2002.  ADD FOLLOWING ARGUMENTS TO ALLOW "CALL DPERAS".
     1IBACCO,IBACC2,IGRASW,IDIASW,
     1PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1IREPCH,IMPSW,
     1IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11199 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
C               **************************************************
C               **  TREAT THE YATES COEF/T/RESSD CUTOFF CASE    **
C               **************************************************
C
      IF(ICOM.EQ.'YATE')GOTO11210
      GOTO11299
C
11210 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CUTO'.AND.
     1IHARG2(2).EQ.'FF')GOTO11220
      GOTO11299
11220 CONTINUE
      CALL DPYACU(IHARG,IARGT,ARG,NUMARG,
     1YATCCU,YATTCU,YATRCU,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11299 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
C               **************************************************
C               **  TREAT THE YATES OUTPUT CASE                 **
C               **************************************************
C
      IF(ICOM.EQ.'YATE')GOTO11310
      GOTO11399
C
11310 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OUTP'.AND.
     1IHARG2(1).EQ.'UT')GOTO11320
      GOTO11399
11320 CONTINUE
      CALL DPYAOU(IHARG,NUMARG,
     1IYATOS,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11399 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
C               **************************************************
C               **  TREAT THE COLUMN RULER CASE                 **
C               **  TREAT THE        RULER CASE                 **
C               **************************************************
C
      IF(ICOM.EQ.'COLU'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'RULE')GOTO11410
      IF(ICOM.EQ.'RULE')GOTO11410
      IF(ICOM.EQ.'COLU'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'NRUL')GOTO11410
      IF(ICOM.EQ.'NRUL')GOTO11410
      GOTO11499
C
11410 CONTINUE
      CALL DPCORU(ICOM,IHARG,NUMARG,
     1IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11499 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   MAY 1990
C               ******************************************
C               **  TREAT THE COMMENT    CHARACTER CASE **
C               ******************************************
C
      IF(ICOM.EQ.'COMM')THEN
        CALL DPCOMM(IHARG,NUMARG,
     1              IDEFCZ,
     1              ICOMCH,
     1              ICOMSW,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED   MARCH 1992
C               ******************************************
C               **  TREAT THE PRINTER TYPE/FORMAT CASE  **
C               ******************************************
C
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'TER ')GOTO11610
      IF(ICOM.EQ.'LP  ' .AND. IHARG(1).NE.'LOCA')GOTO11610
      GOTO11699
C
11610 CONTINUE
CCCCC CALL DPPRFO(IHARG,NUMARG,IPRITY,IBUGS2,IERROR)
      CALL DPPRFO(IHARG,NUMARG,IPRITY,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
11699 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED   MARCH 1992
C               ******************************************
C               **  TREAT THE FILE TYPE/FORMAT CASE     **
C               ******************************************
C
      IF(ICOM.EQ.'FILE')THEN
        CALL DPFIFO(IHARG,NUMARG,IOUTTY,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   AUGUST 1992
C               ******************************************
C               **  TREAT THE VECTOR FORMAT        CASE **
C               ******************************************
C
      IF(ICOM.EQ.'VECT'.AND.IHARG(1).EQ.'FORM')THEN
        CALL DPVCFM(IHARG,NUMARG,
     1              IDEFVF,
     1              IVCFMT,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   AUGUST 1992
C               ******************************************
C               **  TREAT THE VECTOR ARROW         CASE **
C               ******************************************
C
      IF(ICOM.EQ.'VECT'.AND.IHARG(1).EQ.'ARRO')THEN
        CALL DPVCAR(IHARG,NUMARG,
     1              IDEFVA,IDEFVO,
     1              IVCARR,IVCOPN,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   NOVEMBER 1992
C               ******************************************
C               **  TREAT THE ANDREWS INCREMENT    CASE **
C               ******************************************
C
      IF(ICOM.EQ.'ANDR'.AND.IHARG(1).EQ.'INCR')THEN
        CALL DPANIN(IHARG,IARGT,ARG,NUMARG,DEFAIN,
     1              ANDINC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
C               ******************************************
C               **  TREAT THE FRACTAL ITERATIONS   CASE **
C               ******************************************
C
      IF(ICOM.EQ.'FRAC'.AND.IHARG(1).EQ.'ITER')THEN
        CALL DPFRIT(IHARG,IARGT,ARG,NUMARG,MAXPOP,
     1              IFRAIT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
C               ******************************************
C               **  TREAT THE FRACTAL TYPE         CASE **
C               ******************************************
C
      IF(ICOM.EQ.'FRAC'.AND.IHARG(1).EQ.'TYPE')THEN
        CALL DPFRTY(IHARG,NUMARG,
     1              IDEFFT,
     1              IFRATY,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
C               **********************************************
C               **  TREAT THE PRINCIPLE COMPONENT TYPE CASE **
C               **********************************************
C
      IF(ICOM.EQ.'PRIN'.AND.IHARG(1).EQ.'COMP'.AND.
     1   IHARG(2).EQ.'TYPE')THEN
        CALL DPPCTY(IHARG,NUMARG,
     1              IDEFPT,
     1              IPCMTY,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************************
C               **  TREAT THE LOWESS DEGREE  CASE     **
C               ****************************************
C
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'DEGR')THEN
        CALL DPLODG(IHARG,IARGT,ARG,NUMARG,
     1              ALOWDG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC FOLLOWING SECTION ADDED JUNE 1994.
C               ***********************************************
C               **  TREAT THE OPTIMIZATION TOLERANCE  CASE   **
C               ***********************************************
C
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'TOLE')GOTO12600
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.
     1IHARG(1).EQ.'ACCU')GOTO12600
      GOTO12699
C
12600 CONTINUE
C
      CALL DPOPAC(IHARG,IARGT,ARG,NUMARG,DEFOAC,
     1OPTACC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12699 CONTINUE
C
CCCCC THE FOLLOWING SECTION  WAS ADDED   MAY 1994  (JJF)
C               *****************************************
C               **  TREAT THE COPY (= COPY FILE) CASE  **
C               *****************************************
C
      IF(NUMARG.GE.1)THEN
         IF(ICOM.EQ.'COPY')THEN
            CALL DPCOFI(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
     1                  IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
            IFOUND='YES'
            GOTO9000
         ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION  WAS ADDED   MARCH 2019
C               *****************************************
C               **  TREAT THE PRINTFILE          CASE  **
C               *****************************************
C
      IF(NUMARG.GE.1)THEN
         IF(ICOM.EQ.'PRIN' .AND. ICOM2.EQ.'TFIL')THEN
            CALL DPPRFI(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
            IFOUND='YES'
            GOTO9000
         ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   FEBRUARY 1995
C               **********************************************
C               **  TREAT THE OPTIMIZATION METHOD      CASE **
C               **********************************************
C
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.
     1   IHARG(1).EQ.'METH')THEN
        CALL DPOPME(IHARG,NUMARG,
     1              IDEFOM,IDEFHS,
     1              IOPTME,IOPTHE,
     1              IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED BY JIM   SEPTEMBER 1995
C               **********************************************
C               **  TREAT THE   INIT   CASE                 **
C               **  (USEFUL FOR SIGN-ON DEBUGGING)          **
C               **********************************************
C
      IF(ICOM.EQ.'INIT')THEN
         IBUGIN='ON'
C
         ICOMHO=ICOM
         ICOMH2=ICOM2
C
         WRITE(ICOUT,10811)
10811    FORMAT('FROM MAINSU--BEFORE CALL TO MAININ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,10812)IBUGMA,IBUGIN,ICOM,ICOM2,ICOMHO,ICOMH2,NUMDEV
10812    FORMAT('IBUGMA,IBUGIN,ICOM,ICOM2,ICOMHO,ICOMH2,NUMDEV = ',
     1   A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,I8)
         CALL DPWRST('XXX','BUG ')
C
         IRSCNT=1
         CALL MAININ(IBUGIN,ICOMHO,ICOMH2,IRSCNT)
         IBUGIN='OFF'
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               *******************************
C               **  TREAT THE WEB HELP CASE  **
C               *******************************
C
 1201 CONTINUE
C
      IF((ICOM.EQ.'WEB'.AND.IHARG(1).EQ.'HELP') .OR.
     1    ICOM.EQ.'??  ')THEN
        CALL DPHELW(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IANS,IWIDTH,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC FOLLOWING SECTION ADDED MARCH 1999.
C               ***********************************
C               **  TREAT THE WEB HANDBOOK CASE  **
C               ***********************************
C
      IF(ICOM.EQ.'HAND' .OR. ICOM.EQ.'HB  ' .OR.
     1   ICOM.EQ.'WHB ' .OR.
     1  (ICOM.EQ.'????' .AND. ICOM2.EQ.'    ') .OR.
     1   (ICOM.EQ.'WEB'.AND.IHARG(1).EQ.'HAND'))THEN
        CALL DPHANW(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,IANS,
     1              IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               *******************************
C               **  TREAT THE WEB      CASE  **
C               **  NOTE: SET "HANDBOOK" =   **
C               **        "WEB HANDBOOK      **
C               *******************************
C
CCCCC 2019/11: IF THE FIRST ARGUMENT DOES NOT START WITH ONE OF THE
C              FOLLOWING
C
C                  http
C                  file:
C                  www.
C                  A PERIOD "." WITHIN THE FIRST 8 CHARACTERS, THEN
C
C              THEN ASSUME THIS IS NOT A URL ADDRESS AND CONVERT THE
C              "WEB" COMMAND TO A "WEB HELP" COMMAND.
C
      IF(ICOM.EQ.'WEB'  .OR. ICOM.EQ.'W   ' .OR.
     1   ICOM.EQ.'WS  ' .OR.
     1  (ICOM.EQ.'????' .AND. ICOM2.EQ.'?   '))THEN
C
        IFLAG=0
        IF(ICOM.EQ.'WS  ')IFLAG=1
        IF(ICOM.EQ.'????' .AND. ICOM2.EQ.'?   ')IFLAG=1
        IF(IHARG(1).EQ.'SEAR' .AND. IHARG2(1).EQ.'CH  ')IFLAG=1
        IF(IHARG(1).EQ.'HTTP' .AND. IHARG2(1)(1:1).EQ.':')IFLAG=1
        IF(IHARG(1).EQ.'HTTP' .AND. IHARG2(1)(1:2).EQ.'S:')IFLAG=1
        IF(IHARG(1).EQ.'FILE' .AND. IHARG2(1)(1:1).EQ.':')IFLAG=1
        IF(IHARG(1).EQ.'"HTT' .AND. IHARG2(1)(1:2).EQ.'P:')IFLAG=1
        IF(IHARG(1).EQ.'"HTT' .AND. IHARG2(1)(1:3).EQ.'PS:')IFLAG=1
        IF(IHARG(1).EQ.'"FIL' .AND. IHARG2(1)(1:2).EQ.'E:')IFLAG=1
        IF(IHARG(1).EQ.'WWW.')IFLAG=1
        IF(IHARG(1)(1:1).EQ.'.')IFLAG=1
        IF(IHARG(1)(2:2).EQ.'.')IFLAG=1
        IF(IHARG(1)(3:3).EQ.'.')IFLAG=1
        IF(IHARG(1)(4:4).EQ.'.')IFLAG=1
        IF(IHARG2(1)(1:1).EQ.'.')IFLAG=1
        IF(IHARG2(1)(2:2).EQ.'.')IFLAG=1
        IF(IHARG2(1)(3:3).EQ.'.')IFLAG=1
        IF(IHARG2(1)(4:4).EQ.'.')IFLAG=1
C
        IF(IFLAG.EQ.1)THEN
          CALL DPWEB(ICOM,ICOM2,IHARG,IHARG2,NUMARG,
     1               IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGS2,IERROR)
          ISTRT=-1
          DO1210II=1,30
            IF(IANS(II)(1:1).EQ.IHARG(1)(1:1)   .AND.
     1         IANS(II+1)(1:1).EQ.IHARG(1)(2:2) .AND.
     1         IANS(II+2)(1:1).EQ.IHARG(1)(3:3) .AND.
     1         IANS(II+3)(1:1).EQ.IHARG(1)(4:4))THEN
              ISTRT=II
              GOTO1219
            ENDIF
 1210     CONTINUE
 1219     CONTINUE
          IF(ISTRT.GE.1)THEN
            DO1220II=ISTRT,IWIDTH
              IF(II+5.LE.MAXSTR)THEN
                IANS(II+5)=IANS(II)
                IANSLC(II+5)=IANSLC(II)
              ENDIF
 1220       CONTINUE
            IANS(ISTRT)='H   '
            IANS(ISTRT+1)='E   '
            IANS(ISTRT+2)='L   '
            IANS(ISTRT+3)='P   '
            IANS(ISTRT+4)='    '
            IANSLC(ISTRT)='H   '
            IANSLC(ISTRT+1)='E   '
            IANSLC(ISTRT+2)='L   '
            IANSLC(ISTRT+3)='P   '
            IANSLC(ISTRT+4)='    '
          ENDIF
          IHARG(1)='HELP'
          IHARG2(1)='    '
          GOTO1201
        ENDIF
      ENDIF
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               **********************************
C               **  TREAT THE REPEAT GRAPH CASE **
C               **********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GOTO12900
      IF(ICOM.EQ.'RG  ')GOTO12900
      IF(ICOM.EQ.'RP  ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GOTO12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GOTO12900
      IF(ICOM.EQ.'VG  ')GOTO12900
      IF(ICOM.EQ.'VP  ')GOTO12900
      GOTO12990
C
12900 CONTINUE
      CALL DPREGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
12990 CONTINUE
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               **********************************
C               **  TREAT THE LIST   GRAPH CASE **
C               **********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GOTO13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GOTO13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GOTO13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GOTO13000
      IF(ICOM.EQ.'LG  ')GOTO13000
      IF(ICOM.EQ.'LP  ' .AND. IHARG(1).NE.'LOCA')GOTO13000
      GOTO13090
C
13000 CONTINUE
      CALL DPLIGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
13090 CONTINUE
C
CCCCC FOLLOWING SECTION ADDED APRIL 1997.
C               **********************************
C               **  TREAT THE CYCLE  GRAPH CASE **
C               **********************************
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GOTO13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.
     1IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GOTO13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GOTO13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.
     1IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GOTO13100
      IF(ICOM.EQ.'CG  ')GOTO13100
CCCCC MARCH  1998.  CONFLICT WITH CP PLOT COMMAND.
CCCCC AUGUST 1998.  CONFLICT WITH CP CUMULATIVE STATISTIC PLOT COMMAND.
CCCCC AUGUST 1998.  CONFLICT WITH CP MOVING     STATISTIC PLOT COMMAND.
      IF(ICOM.EQ.'CP  ')THEN
        IF(IHARG(1).EQ.'PLOT')GOTO13190
        IF(IHARG(1).EQ.'CUMU' .AND. IHARG(2).EQ.'STAT')GOTO13190
        IF(IHARG(1).EQ.'MOVI' .AND. IHARG(2).EQ.'STAT')GOTO13190
        GOTO13100
      ENDIF
      GOTO13190
C
13100 CONTINUE
      CALL DPCYGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
13190 CONTINUE
C
C               ***************************
C               **  TREAT THE CD     CASE**
C               ***************************
C
      IF(ICOM.EQ.'CD  ')THEN
        CALL DPCDIR(IANS,IANSLC,IWIDTH,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE RM     CASE     **
C               ********************************
C
      IF(ICOM.EQ.'RM  ' .OR.
     1  (ICOM.EQ.'RMDI' .AND. ICOM2.EQ.'R   '))THEN
        CALL DPRM(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE MKDIR  CASE     **
C               ********************************
C
      IF(ICOM.EQ.'MKDI' .AND. ICOM2.EQ.'R   ')THEN
        CALL DPMKDR(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE CAT    CASE     **
C               ********************************
C
      IF(ICOM.EQ.'CAT ' .OR.
     1  (ICOM.EQ.'TYPE' .AND. ICOM2.EQ.'    '))THEN
        CALL DPCAT(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE DIR    CASE     **
C               ********************************
C
      IF(ICOM.EQ.'DIR ' .OR. ICOM.EQ.'LS  ')THEN
        CALL DPDIR(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************************************
C               **  TREAT THE RECIPE SATTERWAITE APPROXIMATION CASE **
C               ******************************************************
C
      IF(ICOM.EQ.'RECI')THEN
        IF((NUMARG.GE.2.AND.IHARG(1).EQ.'SATT'.AND.
     1     IHARG(2).EQ.'APPR') .OR.
     1     (NUMARG.GE.1.AND.IHARG(1).EQ.'SATT') .OR.
     1     (NUMARG.GE.1.AND.IHARG(1).EQ.'APPR'))THEN
          CALL DPRESA(IHARG,NUMARG,IDEFSA,IRECSA,IBUGS2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE PROBABILITY CONTENT       CASE **
C               ******************************************************
C
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.
     1         IHARG(2).EQ.'PLOT') .OR.
     1         (NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.
     1         IHARG(2).EQ.'CONT') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'CONT') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'PROB'))THEN
          CALL DPREPC(IHARG,IARGT,ARG,NUMARG,DEFRPC,RECIPC,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE CONFIDENCE                CASE **
C               ******************************************************
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')THEN
          CALL DPRECO(IHARG,IARGT,ARG,NUMARG,DEFRCO,RECICO,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE FIT DEGREE                CASE **
C               ******************************************************
C
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.
     1         IHARG(2).EQ.'DEGR') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'DEGR'))THEN
          CALL DPREDG(IHARG,IARGT,ARG,NUMARG,DEFRDG,RECIDG,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE ANOVA FACTORS             CASE **
C               ******************************************************
C
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'ANOV'.AND.
     1         IHARG(2).EQ.'FACT') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'FACT'))THEN
          CALL DPREFA(IHARG,IARGT,ARG,NUMARG,DEFRFA,RECIFA,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE OUTPUT                    CASE **
C               ******************************************************
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'OUTP')THEN
          CALL DPRETN(IHARG,NUMARG,IDEFTN,IRECTN,
     1                IBUGS2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE CORRELATION               CASE **
C               ******************************************************
C
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORR')THEN
          CALL DPRECR(IHARG,IARGT,IARG,NUMARG,IDEFR9,IRECC1,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE SIMCOV REPLICATES         CASE **
C               ******************************************************
C
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'SIMC'.AND.
     1         IHARG(2).EQ.'REPL') .OR.
     1         (NUMARG.GE.2.AND.IHARG(1).EQ.'REPL'.AND.
     1         IHARG(2).EQ.'SIMC') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'SIMC') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'REPL'))THEN
          CALL DPRES1(IHARG,IARGT,IARG,NUMARG,IDEFR7,IRECR1,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  TREAT THE RECIPE SIMPVT REPLICATES         CASE **
C               ******************************************************
C
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'SIMP'.AND.
     1         IHARG(2).EQ.'REPL') .OR.
     1         (NUMARG.GE.2.AND.IHARG(1).EQ.'REPL'.AND.
     1         IHARG(2).EQ.'SIMP') .OR.
     1         (NUMARG.GE.1.AND.IHARG(1).EQ.'SIMP'))THEN
          CALL DPRESZ(IHARG,IARGT,IARG,NUMARG,IDEFR8,IRECR2,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
CCCCC FOLLOWING SECTION ADDED APRIL 1998.
C               ******************************************************
C               **  TREAT THE RECIPE FIT   FACTORS             CASE **
C               ******************************************************
C
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.
     1         IHARG(2).EQ.'FACT')THEN
          CALL DPREFF(IHARG,IARGT,ARG,NUMARG,DEFRFF,RECIFF,
     1                IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
        ENDIF
C
      ENDIF
C
C               ********************************
C               **  TREAT THE AUTO TEXT CASE  **
C               ********************************
C
      IF(ICOM.EQ.'AUTO'.AND.
     1   NUMARG.GE.1.AND.IHARG(1).EQ.'TEXT')THEN
        CALL DPAUTX(IHARG,NUMARG,IATXSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IERRST=IERROR
C
C     AUGUST 2007.  CHECK FOR FATAL ERROR
C
      IF(IERROR.EQ.'YES')THEN
        ISUBN1='MAIN'
        ISUBN2='SU  '
        ICASE2='SUPP'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1              ISUBN1,ISUBN2,ICASE2,
     1              IBUGS2,ISUBRO,IERROR)
      ENDIF
C
C
      IF(IBUGSU.EQ.'ON'.OR.ISUBRO.EQ.'INSU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MAINSU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFOUND,IERROR
 9022   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAKCDF(X,XI,LAMBDA,THETA,CDF)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
C     DISTRIBUTION FUNCTION. IT HAS THE FOLLOWING CDF:
C         F(X,XI,LAMBDA,THETA) = 1 - 
C                EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
C                X > 0; LAMBDA, XI > 0, THETA >= 0
C     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
C     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
C     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
C     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
C
C         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
C         LAMBDA(DLMF) = K(MEEKER)
C         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL LAMBDA
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CDF=0.0
      IF(X.LE.0.0)GOTO9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKCDF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKCDF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKCDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
C
      DTERM1=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
C
      IF(DTERM1.LE.-80.D0)THEN
        CDF=1.0
        GOTO9999
      ELSEIF(DTERM1.GE.80.D0)THEN
        CDF=0.0
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSE
        DCDF=1.0D0 - DEXP(DTERM1)
        CDF=REAL(DCDF)
      ENDIF
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKCDF.  THE COMPUTED ',
     1'CDF VALUE EXCEEDS MACHINE PRECISION.')
C
 9999 CONTINUE
      RETURN
      END
      REAL FUNCTION MAKFU2(X)
C
C     PURPOSE--MAKPPF CALLS FZERO TO FIND A ROOT FOR THE PERCENT
C              POINT FUNCTION.  MAKFU2 IS THE FUNCTION FOR WHICH
C              THE ZERO IS FOUND.  IT IS:
C                 P - MAKCDF(X,XI,LAMBDA,THETA)
C              WHERE P IS THE DESIRED PERCENT POINT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE MAKFU2.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--MAKCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003.12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL P
      COMMON/MA2COM/P
C
      REAL XI
      REAL LAMBDA
      REAL THETA
      COMMON/MAKCOM/XI,LAMBDA,THETA
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CALL MAKCDF(X,XI,LAMBDA,THETA,CDF)
      MAKFU2=P - CDF
      RETURN
      END
      SUBROUTINE MAKCHA(X,XI,LAMBDA,THETA,HAZ)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
C     HAZARD FUNCTION WHICH HAS THE FOLLOWING FORMULA:
C         H(X,XI,LAMBDA,THETA) = 
C                      -[-XI*(EXP(LAMBDA*X) - 1) - XI*THETA*LAMBDA*X]
C                X > 0; LAMBDA, XI > 0, THETA >= 0
C     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
C     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
C     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
C     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
C
C         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
C         LAMBDA(DLMF) = K(MEEKER)
C         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL LAMBDA
C
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      HAZ=0.0
      IF(X.LE.0.0)GOTO9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKCHAZ')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKCHAZ IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKCHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
C
      DHAZ=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
      HAZ=-REAL(DHAZ)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE MAKHAZ(X,XI,LAMBDA,THETA,HAZ)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM
C     HAZARD FUNCTION WHICH HAS THE FOLLOWING FORMULA:
C         h(X,XI,LAMBDA,THETA) = f(X,XI,LAMBDA,THETA)/
C                                -LOG[1 - F(x,XI,LAMBDA,THETA)]
C                              = XI*THETA*LAMBDA + XI*LAMBDA*
C                                EXP(LAMBDA*X)
C                X > 0; LAMBDA, XI > 0, THETA >= 0
C         WHERE f IS THE PROBABILITY DENSITY AND F IS THE
C         CUMULATIVE DISTRIBUTION FUNCTION.
C
C     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
C     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
C     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
C     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
C
C         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
C         LAMBDA(DLMF) = K(MEEKER)
C         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL LAMBDA
C
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      HAZ=0.0
      IF(X.LE.0.0)GOTO9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKHAZZ')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKHAZZ IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKHAZZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
C
      DTERM1=DXI*DTHETA*DLMBDA
      DTERM2=DXI*DLMBDA*DEXP(DLMBDA*DX)
      DHAZ=DTERM1 + DTERM2
      HAZ=REAL(DHAZ)
      HAZ=REAL(DHAZ)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE MAKPDF(X,XI,LAMBDA,THETA,PDF)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM PROBABILITY
C     DENSITY FUNCTION. VALUE DISTRIBUTION.  IT HAS THE FOLLOWING
C     PDF:
C         F(X,XI,LAMBDA,THETA) = XI*LAMBDA*(THETA + EXP(LAMBDA*X))*
C                EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
C                X > 0; LAMBDA, XI > 0, THETA >= 0
C     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
C     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
C     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
C     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
C
C         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
C         LAMBDA(DLMF) = K(MEEKER)
C         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C         LAMBDA = THETA*LAMBDA*XI
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL LAMBDA
C
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      PDF=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKPDF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKPDF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKPDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
  301 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO MAKPDF IS')
  302 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
C
      DTERM1=DLOG(DXI) + DLOG(DLMBDA)
      DTERM2=DLOG(DTHETA + DEXP(DLMBDA*DX))
      DTERM3=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
      DTERM4=DTERM1 + DTERM2 + DTERM3
C
      IF(DTERM4.LE.-80.D0)THEN
        PDF=0.0
        GOTO9999
      ELSEIF(DTERM4.GE.80.D0)THEN
        PDF=0.0
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKPDF.  THE COMPUTED ',
     1'PDF VALUE EXCEEDS MACHINE PRECISION.')
C
      DPDF=DEXP(DTERM4)
      PDF=REAL(DPDF)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE MAKPPF(P,XI,LAMBDA,THETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GOMPERTZ-MAKEHAM DISTRIBUTION
C              WITH SHAPE PARAMETERS XI, LAMBDA, AND THETA.
C              THIS DISTRIBUTION IS DEFINED FOR POSITIVE X AND THE
C              PERCENT POINT FUNCTION IS COMPUTED BY
C              NUMERICALLY INVERTING THE CDF FUNCTION.
C
C     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
C     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
C     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
C     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
C
C         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
C         LAMBDA(DLMF) = K(MEEKER)
C         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
C
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --XI     = THE FIRST SHAPE PARAMETER
C                     --LAMBDA = THE SECOND SHAPE PARAMETER
C                     --THETA  = THE THIRD SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--FZERO.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003.12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL LAMBDA
      REAL PPF
C
      REAL MAKFU2
      EXTERNAL MAKFU2
C
      REAL P2
      COMMON/MA2COM/P2
C
      REAL XI2
      REAL LAMBD2
      REAL THETA2
      COMMON/MAKCOM/XI2,LAMBD2,THETA2
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      PPF=0.0
C
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
         WRITE(ICOUT,61)
   61    FORMAT('***** ERROR--THE FIRST  INPUT ARGUMENT ',
     1          'TO THE MAKPPF SUBROUTINE ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,62)
   62    FORMAT('      IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL ***')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,63)P
   63    FORMAT('      VALUE OF ARGUMENT = ',G15.7)
         CALL DPWRST('XXX','BUG ')
         PPF=0.0
         GOTO9000
      ENDIF
C
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKPPF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKPPF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKPPF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      IF(P.EQ.0.0)THEN
        PPF=0.0
        GOTO9000
      ENDIF
C
C  STEP 1: FIND BRACKETING INTERVAL.  LOWER BOUND IS ZERO.  START WITH
C          10 AS GUESS FOR UPPER BOUND.  MULTIPLY BY 10 UNTIL
C          BRACKETING INTERVAL FOUND.
C
      XLOW=0.0000001
      XUP2=10.0
  200 CONTINUE
        CALL MAKCDF(XUP2,XI,LAMBDA,THETA,PTEMP)
        IF(PTEMP.GT.P)THEN
          XUP=XUP2
        ELSE
          XUP2=XUP2*10.0
          IF(XUP2.GT.CPUMAX/100.)THEN
            WRITE(ICOUT,201)
  201       FORMAT('***** ERROR FROM MAKPPF--UNABLE TO FIND A ',
     1             'BRACKETING INTERVAL')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
          GOTO200
        ENDIF
C
      AE=1.E-6
      RE=1.E-6
      P2=P
      XI2=XI
      LAMBD2=LAMBDA
      THETA2=THETA
      CALL FZERO(MAKFU2,XLOW,XUP,XUP,RE,AE,IFLAG)
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
CC111   FORMAT('***** WARNING FROM MAKPPF--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,113)
CC113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
CCCCC1         'TOLERANCE.')
CCCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      PPF VALUE MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
  141   FORMAT('***** WARNING FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,143)
  143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MAKRAN(N,XI,LAMBDA,THETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE THE GOMPERTZ-MAKEHAM DISTIBUTION WITH
C              LOCATION = 0 AND SCALE = 1.  THIS DISTRIBUTION IS
C              DEFINED FOR POSITIVE X AND HAS THE PROBABILITY DENSITY
C              FUNCTION:
C              F(X,XI,LAMBDA,THETA) = XI*LAMBDA*(THETA + EXP(LAMBDA*X))
C                *EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
C                X > 0; LAMBDA, XI > 0, THETA >= 0
C              XI, LAMBDA, AND THETA ARE SHAPE PARAMETERS.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C                     --XI     = A SINGLE PRECISON SCALAR THAT DEFINES
C                                THE FIRST SHAPE PARAMETER.
C                     --LAMBDA = A SINGLE PRECISON SCALAR THAT DEFINES
C                                THE SECOND SHAPE PARAMETER.
C                     --THETA  = A SINGLE PRECISON SCALAR THAT DEFINES
C                                THE THIRD SHAPE PARAMETER.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE COMPERTZ-MAKEHAM
C             DISTRIBUTION WITH LOCATION = 0 AND SCALE = 1.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, MAKPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     METHOD--TRANSFORM NORMAL RANDOM NUMBERS
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--2003.12
C     ORIGINAL VERSION--DECEMBER  2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      REAL XI
      REAL THETA
      REAL LAMBDA
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
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
        GOTO9999
      ELSEIF(XI.LE.0.0)THEN
        WRITE(ICOUT, 6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)XI
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT, 7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(THETA.LT.0.0)THEN
        WRITE(ICOUT,8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE FIRST (N) INPUT ARGUMENT TO THE ',
     1'MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    6 FORMAT('***** FATAL ERROR--THE SECOND (XI) INPUT ARGUMENT TO ',
     1'THE MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    7 FORMAT('***** FATAL ERROR--THE THIRD (LAMBDA) INPUT ARGUMENT ',
     1'TO THE MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    8 FORMAT('***** FATAL ERROR--THE FOURTH (THETA) INPUT ARGUMENT ',
     1'TO THE MAKRAN SUBROUTINE IS NEGATIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F15.7,' *****')
C
C     GENERATE N UNIFORM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N GOMPERTZ-MAKEHAM RANDON NUMBERS USING THE
C     PERCENT POINT FUNCTION TRANSFORMATION.
C
      DO100I=1,N
        XTEMP=X(I)
        CALL MAKPPF(XTEMP,XI,LAMBDA,THETA,PPF)
        X(I)=PPF
  100 CONTINUE
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE MA2CDF(X,ZETA,ETA,CDF)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
C     DISTRIBUTION FUNCTION. THIS USES THE MEEKER AND ESCOBAR
C     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
C     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
C     PARAMETER.  IT HAS THE FOLLOWING CDF:
C         F(X,ZETA,ETA) = 1 - EXP[C1 - EXP(C2) - C3]
C                X,  > 0; ETA >= 0
C     WITH
C         C1 = EXP(-ZETA)
C         C2 = EXP(LOG(X)) - ZETA
C            = X - ZETA
C         C3 = ETA*EXP(LOG(X))
C            = ETA*X
C
C     PUTTING THIS TOGETHER GIVES
C         F(X,ZETA,ETA) = 1 - EXP[EXP(-ZETA) - EXP(X-ZETA) - ETA*X]
C                X,  > 0; ETA >= 0
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL ZETA
      REAL ETA
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CDF=0.0
      IF(X.LE.0.0)GOTO9000
CCCCC IF(ETA.LE.0.0)THEN
CCCCC   WRITE(ICOUT,101)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,102)ETA
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   GOTO9000
CCCCC ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ZETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
CC101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (ETA) TO MA2CDF')
CC102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ZETA) TO')
  107 FORMAT('      MAKCDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
C
      DTERM1=DEXP(-DZETA)
      DTERM2=DEXP(DX - DZETA)
      DTERM3=DETA*DX
      DTERM4=DTERM1 - DTERM2 - DTERM3
C
      IF(DTERM4.LE.-80.D0)THEN
        CDF=1.0
      ELSEIF(DTERM4.GE.80.D0)THEN
        CDF=0.0
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
      ELSE
        DCDF=1.0D0 - DEXP(DTERM4)
        CDF=REAL(DCDF)
      ENDIF
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKCDF.  THE ',
     1'COMPUTED CDF VALUE EXCEEDS MACHINE PRECISION.')
C
 9000 CONTINUE
      RETURN
      END
      REAL FUNCTION MA2FU2(X)
C
C     PURPOSE--MA2PPF CALLS FZERO TO FIND A ROOT FOR THE PERCENT
C              POINT FUNCTION.  MA2FU2 IS THE FUNCTION FOR WHICH
C              THE ZERO IS FOUND.  IT IS:
C                 P - MA2CDF(X,ZETA,ETA)
C              WHERE P IS THE DESIRED PERCENT POINT.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE MA2FU2.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--MA2CDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL P
      COMMON/MA4COM/P
C
      REAL ETA
      REAL ZETA
      COMMON/MA3COM/ETA,ZETA
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CALL MA2CDF(X,ZETA,ETA,CDF)
      MA2FU2=P - CDF
      RETURN
      END
      SUBROUTINE MA2CHA(X,ZETA,ETA,CHAZ)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
C     HAZARD FUNCTION. THIS USES THE MEEKER AND ESCOBAR
C     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
C     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
C     PARAMETER.  IT HAS THE FOLLOWING CDF:
C         F(X,ZETA,ETA) = 1 - 
C                         EXP[EXP(-ZETA) - EXP(X - ZETA) - ETA*X]
C                X,  > 0; ETA >= 0
C     THE CUMULATIVE HAZARD IS:
C         H(X,ZETA,ETA) = -LOG(1 - F(X,ZETA,ETA))
C                        = -EXP(-ZETA) + EXP(X-ZETA) + ETA*X
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL ZETA
      REAL ETA
C
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CHAZ=0.0
      IF(X.LE.0.0)GOTO9000
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKCHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
C
      DTERM1=DEXP(-DZETA)
      DTERM2=DEXP(DX - DZETA)
      DTERM3=DETA*DX
      DTERM4=DTERM1 - DTERM2 - DTERM3
      CHAZ=-REAL(DTERM4)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MA2HAZ(X,ZETA,ETA,HAZ)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM HAZARD
C     FUNCTION. THIS USES THE MEEKER AND ESCOBAR
C     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
C     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
C     PARAMETER.  IT HAS THE FOLLOWING HAZARD FUNCTION:
C         h(X,ZETA,ETA) = ETA + EXP(-ZETA)*EXP(X)
C                         X, ETA >= 0
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL ZETA
      REAL ETA
C
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      HAZ=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  103 FORMAT('***** ERROR--THE INPUT ARGUMENT  TO MA2HAZ IS')
  104 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
C
      DHAZ=DETA + DEXP(-DZETA)*DEXP(DX)
      HAZ=REAL(DHAZ)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MA2PDF(X,ZETA,ETA,PDF)
C
C     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM PROBABILITY
C     DENSITY FUNCTION. THIS USES THE MEEKER AND ESCOBAR
C     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
C     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
C     PARAMETER.  IT HAS THE FOLLOWING PROBABILITY DENSITY FUNCTION:
C         f(X,ZETA,ETA) = (ETA + EXP(X-ZETA))*
C                         EXP[EXP(-ZETA)-EXP(X-ZETA)-ETA*X]
C                         X, ETA > 0
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL ZETA
      REAL ETA
C
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      PDF=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
CCCCC IF(ZETA.LE.0.0)THEN
CCCCC   WRITE(ICOUT,101)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,102)ZETA
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   GOTO9000
CCCCC ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  103 FORMAT('***** ERROR--THE INPUT ARGUMENT  TO MA2PDF IS')
  104 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKPDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
      DX=DBLE(X)
      DZETA=DBLE(ZETA)
      DETA=DBLE(ETA)
C
      DTERM1=DETA + EXP(DX-DZETA)
      DTERM2=DEXP(-DZETA) - DEXP(DX-DZETA) - DETA*DX
      DPDF=DTERM1*DEXP(DTERM2)
      PDF=REAL(DPDF)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MA2PPF(P,ZETA,ETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GOMPERTZ-MAKEHAM DISTRIBUTION
C              WITH SHAPE PARAMETERS ETA AND ZETA.
C              THIS DISTRIBUTION IS DEFINED FOR POSITIVE X AND THE
C              PERCENT POINT FUNCTION IS COMPUTED BY
C              NUMERICALLY INVERTING THE CDF FUNCTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ZETA l = THE FIRST SHAPE PARAMETER
C                     --ETA    = THE SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--FZERO.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL ETA
      REAL ZETA
      REAL PPF
C
      REAL MA2FU2
      EXTERNAL MA2FU2
C
      REAL P2
      COMMON/MA4COM/P2
C
      REAL ETA2
      REAL ZETA2
      COMMON/MA3COM/ETA2,ZETA2
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      PPF=0.0
C
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
         WRITE(ICOUT,61)
   61    FORMAT('***** ERROR--THE FIRST  INPUT ARGUMENT ',
     1          'TO THE MA2PPF SUBROUTINE ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,62)
   62    FORMAT('      IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL ***')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,63)P
   63    FORMAT('      VALUE OF ARGUMENT = ',G15.7)
         CALL DPWRST('XXX','BUG ')
         PPF=0.0
         GOTO9000
      ENDIF
C
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKPPF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
C
C  STEP 1: FIND BRACKETING INTERVAL.  LOWER BOUND IS ZERO.  START WITH
C          10 AS GUESS FOR UPPER BOUND.  MULTIPLY BY 10 UNTIL
C          BRACKETING INTERVAL FOUND.
C
      XLOW=0.0000001
      XUP2=10.0
  200 CONTINUE
        CALL MA2CDF(XUP2,ZETA,ETA,PTEMP)
        IF(PTEMP.GT.P)THEN
          XUP=XUP2
        ELSE
          XUP2=XUP2*10.0
          IF(XUP2.GT.CPUMAX/100.)THEN
            WRITE(ICOUT,201)
  201       FORMAT('***** ERROR FROM MA2PPF--UNABLE TO FIND A ',
     1             'BRACKETING INTERVAL')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
          GOTO200
        ENDIF
C
      AE=1.E-6
      RE=1.E-6
      P2=P
      ETA2=ETA
      ZETA2=ZETA
      CALL FZERO(MA2FU2,XLOW,XUP,XUP,RE,AE,IFLAG)
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
CC111   FORMAT('***** WARNING FROM MA2PPF--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,113)
CC113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
CCCCC1         'TOLERANCE.')
CCCCC   CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)
  121   FORMAT('***** WARNING FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      PPF VALUE MAY BE NEAR A SINGULAR POINT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      APPROPRIATE BRACKETING INTERVAL NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFLAG.EQ.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)
  141   FORMAT('***** WARNING FROM MAKPPF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,143)
  143   FORMAT('      MAXIMUM ITERATIONS EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MA2RAN(N,ZETA,ETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE THE GOMPERTZ-MAKEHAM DISTIBUTION WITH
C              LOCATION = 0 AND SCALE = 1.  THIS DISTRIBUTION IS
C              DEFINED FOR POSITIVE X AND HAS THE PROBABILITY DENSITY
C              FUNCTION:
C              f(X,ETA,ZETA) = (1/X)*EXP(LOG(X))*
C                              [ZETA + EXP[EXP(LOG(X)) - ETA]*
C                              [1 - MA2CDF(X,ETA,ZETA)]
C                              X, ZETA > 0
C             WHERE MA2CDF IS:
C             F(X,ETA,ZETA) = 1 - EXP[C1 - EXP(C2) - C3]
C                             X, ZETA > 0
C             WITH
C                 C1 = EXP(-ETA)
C                 C2 = EXP(LOG(X) - ETA)
C                 C3 = ZETA*EXP(LOG(X))
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C                     --ETA    = A SINGLE PRECISON SCALAR THAT DEFINES
C                                THE FIRST SHAPE PARAMETER.
C                     --ZETA   = A SINGLE PRECISON SCALAR THAT DEFINES
C                                THE SECOND SHAPE PARAMETER.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE COMPERTZ-MAKEHAM
C             DISTRIBUTION WITH LOCATION = 0 AND SCALE = 1.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EETASTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAETAMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, MA2PPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     METHOD--TRANSFORM NORMAL RANDOM NUMBERS
C     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
C                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.7
C     ORIGINAL VERSION--JULY      2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      REAL ETA
      REAL ZETA
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT, 6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(ZETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)ZETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ZETA) TO THE')
  107 FORMAT('      GOMPERTZ MAKEHAM RANDOM NUMBERS ROUTINE IS ',
     1       'NON-POSITIVE.')
    5 FORMAT('***** THE REQUESTED NUMBER OF RANDOM NUMBERS FOR THE')
    6 FORMAT('      GOMPERTZ-MAKEHAM DISTRIBUTION IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F15.7)
C
C     GENERATE N UNIFORM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N GOMPERTZ-MAKEHAM RANDON NUMBERS USING THE
C     PERCENT POINT FUNCTION TRANSFORMATION.
C
      DO100I=1,N
        XTEMP=X(I)
        CALL MA2PPF(XTEMP,ZETA,ETA,PPF)
        X(I)=PPF
  100 CONTINUE
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE MANDIS(X,Y,N,IWRITE,STATVA,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE MANHATTAN DISTANCE BETWEEN THE
C              TWO SETS OF DATA IN THE INPUT VECTORS X AND Y.  THE
C              SAMPLE MANHATTAN DISTANCE WILL BE A SINGLE PRECISION VALUE
C              CALCULATED AS:
C
C                 DISTANCE = SUM[i=1 to n][|X(i) - Y(i)|]
C
C              THIS IS EQUIVALENT TO A MINKOWSKY DISTANCE WITH
C              P = 1 AND IS ALSO KNOWN CITY BLOCK OR TAXI-CAB
C              DISTANCE.
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
C             SAMPLE MANHATTAN DISTANCE BETWEEN THE 2 SETS
C             OF DATA IN THE INPUT VECTORS X AND Y.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--ABS.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--XXX
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='MAND'
      ISUBN2='IS  '
      IERROR='NO'
      STATVA=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NDIS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MANDIS--')
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
  111   FORMAT('***** ERROR IN MANHATTAN DISTANCE--')
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
C               ************************************************
C               **  STEP 2--                                  **
C               **  COMPUTE THE MANHATTAN DISTANCE.           **
C               ************************************************
C
      STATVA=0.0
      DO200I=1,N
        STATVA=STATVA + ABS(X(I) - Y(I))
  200 CONTINUE
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
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE MANHATTAN DISTANCE OF THE ',I8,
     1           ' OBSERVATIONS = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'NDIS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MANDIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,STATVA
 9012   FORMAT('IERROR,STATVA = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE MATARI(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,MAXROM,MAXCOM,
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC SUBROUTINE MATARI(YM1,NR1,NC1,YM2,NR2,NC2,YM3,NR3,NC3,
     1Y1,N1,Y2,N2,Y3,N3,Y4,N4,
     1INDEX,IZROV,IPOSV,
     1DMEAN,DSSQD,P1,P2,BETA,
     1YS1,YS2,YS3,YS4,
     1IMCASE,IUPFLG,IMSUBC,ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,
     1YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,
CCCCC AUGUST 1993.
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC1YMJUNK,YMJUN2,
     1IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
C              OF THE REAL DATA IN MATRICES YM1 AND YM2.
C
C     OPERATIONS--ADDITION
C                 SUBTRACTION
C                 MULTIPLICATION
C                 TRUNCATION
C
C                 NUMBER OF ROWS
C                 NUMBER OF COLUMNS
C                 ROW
C                 ELEMENT
C                 REPLACE ROW
C                 REPLACE ELEMENT
C                 DIAGONAL
C
C                 SOLUTION
C                 ITERATIVE SOLUTION
C                 TRIDIAGONAL SOLVE
C                 TRIANGULAR SOLVE
C                 SIMPLEX SOLUTION
C                 RANK
C
C                 CONDITION NUMBER
C                 RECIPROCAL CONDITION NUMBER
C                 INVERSE
C                 TRIANGULAR INVERSE
C                 DETERMINANT
C                 TRACE
C                 PERMANENT
C                 ADJOINT
C                 SUBMATRIX
C                 MINOR
C                 COFACTOR
C
C                 DEFINITION
C                 AUGMENT
C                 TRANSPOSE
C
C                 CHARACTERISTIC EQUATION      (NOT YET IMPLEMENED)
C
C                 EIGENVALUES
C                 EIGENVECTORS
C                 SINGULAR VALUE
C                 SINGULAR VALUE DECOMPOSITION
C                 CHOLESKY DECOMPOSITION
C                 SPECTRAL NORM 
C                 SPECTRAL RADIUS
C                 EUCLIDEAN NORM 
C
C                 VARIANCE-COVARIANCE MATRIX
C                 CORRELATION MATRIX
C                 PRINCIPLE COMPONENTS ...
C                 ... PRINCIPLE COMPONENT ...
C                 COMOVEMENT MATRIX
C
C     EXAMPLES--LET M3 = MATRIX ADDITION M1 M2
C               LET M3 = MATRIX ADDITION M1 P1
C             --LET M3 = MATRIX SUBTRACTION M1 M2
C               LET M3 = MATRIX SUBTRACTION M1 P1
C             --LET M3 = MATRIX MULTIPLICATION M1 M2
C               LET M3 = MATRIX MULTIPLICATION M1 V1
C               LET M3 = MATRIX MULTIPLICATION M1 P1
C             --LET V3 = MATRIX SOLUTION M1 V2
C             --LET V3 = MATRIX ITERATIVE SOLUTION M1 V2
C             --LET M3 = MATRIX INVERSE M1
C             --LET P3 = MATRIX CONDITION NUMBER M1
C             --LET P3 = MATRIX RECIPROCAL CONDITION NUMBER M1
C             --LET M3 = MATRIX TRANSPOSE M1
C             --LET M3 = MATRIX ADJOINT M1
C             --LET V3 = MATRIX CHARACTERISTIC EQUATION M1
C             --LET V3 = MATRIX EIGENVALUES M1
C             --LET P3 = MATRIX EIGENVECTORS M1
C             --LET P3 = MATRIX RANK M1
C             --LET P3 = MATRIX DETERMINANT M1
C             --LET P3 = MATRIX PERMANENT M1
C             --LET P3 = MATRIX SPECTRAL NORM M1
C             --LET P3 = MATRIX SPECTRAL RADIUS M1
C             --LET P3 = MATRIX NUMBER OF ROWS M1
C             --LET P3 = MATRIX NUMBER OF COLUMNS M1
C             --LET V4 = MATRIX SIMPLEX SOLUTION V1 M1 V2 V3
C             --LET P3 = MATRIX TRACE M1
C             --LET M3 = MATRIX SUBMATRIX M1 P1 P2
C             --LET P3 = MATRIX MINOR M1 P1 P2
C             --LET P3 = MATRIX COFACTOR M1 P1 P2
C             --LET M3 = MATRIX DEFINITION V1 P1 P2
C             --LET M3 = MATRIX DEFINITION V1 P1 P2 P3
C             --LET P3 = MATRIX EUCLIDEAN NORM M1
C             --LET V3 = MATRIX ROW M1 P1
C             --LET P3 = MATRIX ELEMENT M1 P1 P2
C             --LET M3 = MATRIX REPLACE ROW M1 V1 P1
C             --LET M3 = MATRIX REPLACE ELEMENT M1 P1 P2
C             --LET M3 = MATRIX AUGMENT M1
C             --LET V3 = MATRIX DIAGONAL M1
C             --LET M3 = DIAGONAL MATRIX V1
C             --LET M3 = VARIANCE-COVARIANCE MATRIX M1
C             --LET M3 = CORRELATION MATRIX M1
C             --LET M3 = PRINCIPLE COMPONENTS M1
C             --LET M3 = PRINCIPLE COMPONENTS EIGENVECTORS M1
C             --LET V3 = PRINCIPLE COMPONENTS EIGENVALUES M1
C             --LET V3 = ... PRINCIPLE COMPONENT M1
C             --LET V3 = ... PRINCIPLE COMPONENT EIGENVECTOR M1
C             --LET P3 = ... PRINCIPLE COMPONENT EIGENVALUE M1
C             --LET V3 = MATRIX SINGULAR VALUES M1
C             --LET M3 V3 M2 = MATRIX SINGULAR VALUE DECOMP M1
C             --LET M3 V3 M2 = MATRIX SINGULAR VALUE FACTOR M1
C             --LET M3 = CHOLESKY DECOMP M1
C             --LET V4 = TRIDIAGONAL SOLVE V1 V2 V3
C             --LET V4 = TRIANGULAR SOLVE M1 V2
C             --LET M3 = TRIANGULAR INVERSE M2
C             --LET M3 = MATRIX TRUNCATION M1 P1
C             --LET M3 = MATRIX UPPPER TRUNCATION M1 P1
C
C     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
C                     --NR1
C                     --NC1
C                     --YM2 (REAL MATRIX)
C                     --NR2
C                     --NC2
C                     --YM3 (REAL MATRIX)
C                     --NR3
C                     --NC3
C                     --Y1  (REAL VECTOR)
C                     --N1
C                     --Y2  (REAL VECTOR)
C                     --N2
C                     --Y3  (REAL VECTOR)
C                     --N3
C                     --Y4  (REAL VECTOR)
C                     --N4
C     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
C                     --NR9
C                     --NC9
C                     --VECT9 (REAL VECTOR)
C                     --NVECT9
C                     --SCAL9 (REAL SCALAR)
C                     --ITYP9
C
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
C           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/10
C     ORIGINAL VERSION--SEPTEMBER 1987
C     UPDATED         --AUGUST    1988  (VARIANCE-COVARIANCE MATRIX)
C     UPDATED         --AUGUST    1988  (CORRELATION MATRIX)
C     UPDATED         --AUGUST    1988  (PRINCIPLE COMPONENTS)
C     UPDATED         --AUGUST    1988  (... PRINCIPLE COMPONENTS)
C     UPDATED         --APRIL     1992  DEFINE D999
C     UPDATED         --JULY      1993  FOR MATRIX SOLUTION,
C                                       DETERMINANT, INVERSE, REPLACE
C                                       NUMERICAL RECIPES CODE WITH
C                                       LINPACK CODE
C     UPDATED         --JULY      1993  EIGENVALUES AND EIGENVECTORS 
C                                       EXTENDED TO NON-SYMMETRIC CASE
C     UPDATED         --JULY      1993  IMPLEMENT RANK, ADJOINT,
C                                       SINGULAR VALUES, SINGULAR VALUE
C                                       DECOMP
C     UPDATED         --SEPT      1993  ROW, ELEMENT CASES
C     UPDATED         --OCTOBER   1993  CHOLESKY DECOMPOSITION, REPLACE 
C                                       ROW, REPLACE ELEMENT, AUGMENT,
C                                       DIAGONAL, ADD ARGUMENT TO
C                                       MATRIX DEFINITION, TRIDIAGONAL
C                                       SOLVE.
C     UPDATED         --OCTOBER   1993  MOVE SOME OPERATIONS TO MATAR2
C     UPDATED         --DECEMBER  1994 MATRIX SUBMATRIX FOR NON-SQUARE
C                                      MATRICES
C     UPDATED         --JUNE      1995 EXTEND SPECTRAL RADIUS TO 
C                                      NON-SYMMETRIC CASE
C     UPDATED         --JANUARY   1998 RECODE TO USE FEWER MATRICES
C     UPDATED         --JULY      2002 SUPPORT FOR DIFFERENT TYPES OF
C                                      COVARIANCE AND CORRELATION MATRIX
C     UPDATED         --NOVEMBER  2004 SUPPORT FOR DIFFERENT TYPES OF
C     UPDATED         --MARCH     2006 MATRIX <LOWER/UPPER> TRUNCATE
C     UPDATED         --NOVEMBER  2007 COMOVEMENT MATRIX
C     UPDATED         --SEPTEMBER 2011 MATRIX CONDITION NUMBER
C     UPDATED         --SEPTEMBER 2011 MATRIX RECIPROCAL CONDITION
C                                             NUMBER
C     UPDATED         --JUNE      2012 PARTIAL CORRELATION MATRIX
C     UPDATED         --JUNE      2012 PARTIAL CORRELATION CDF MATRIX
C     UPDATED         --JUNE      2012 PARTIAL CORRELATION PVALUE MATRIX
C     UPDATED         --JUNE      2012 CORRELATION CDF MATRIX
C     UPDATED         --JUNE      2012 CORRELATION PVALUE MATRIX
C     UPDATED         --SEPTEMBER 2016 CORRELATION ABSOLUTE VALUE
C     UPDATED         --SEPTEMBER 2016 CORRELATION PERCENTAGE VALUE
C     UPDATED         --SEPTEMBER 2016 CORRELATION DIGITS
C     UPDATED         --AUGUST    2019 CALL LIST TO KENTAU
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IMCASE
      CHARACTER*4 IUPFLG
      CHARACTER*4 IMSUBC
      CHARACTER*4 PCCASE
      CHARACTER*4 ITYPA1
      CHARACTER*4 ITYPA2
      CHARACTER*4 ITYPA3
      CHARACTER*4 ITYPA4
      CHARACTER*4 IWRITE
      CHARACTER*4 ITYP9
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASZZ
C
C-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DYM2
      DOUBLE PRECISION DYM9
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DDEL
C
      DOUBLE PRECISION DNR1
      DOUBLE PRECISION DNC1
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSSQD
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DCOV
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      DOUBLE PRECISION D999
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION YM1(MAXROM,MAXCOM)
      DIMENSION YM2(MAXROM,MAXCOM)
CCCCC DIMENSION YM3(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION YM9(MAXROM,MAXCOM)
      DIMENSION VECT9(*)
C
CCCCC DIMENSION YMJUNK(MAXROM,MAXCOM)
CCCCC DIMENSION YMJUN2(MAXROM,MAXCOM)
CCCCC JANUARY 1998.  FOLLOWINF DIMENSIONS TO MAXOBV.
CCCCC DIMENSION INDEX(MAXROM)
CCCCC DIMENSION VJUNK(MAXROM)
CCCCC DIMENSION VJUNK2(MAXROM)
CCCCC DIMENSION AINDE2(MAXROM)
CCCCC DIMENSION AINDE3(MAXROM)
C
CCCCC DIMENSION IZROV(MAXROM)
CCCCC DIMENSION IPOSV(MAXROM)
C
CCCCC DIMENSION DMEAN(MAXROM)
CCCCC DIMENSION DSSQD(MAXROM)
C
CCCCC DIMENSION INDEX(MAXOBV)
      DIMENSION INDEX(*)
CCCCC REPLACE VJUNK, VJUNK2 WITH Y3 AND Y4 BELOW (TO SAVE SPACE)
CCCCC DIMENSION VJUNK(MAXOBV)
CCCCC DIMENSION VJUNK2(MAXOBV)
CCCCC REPLACE AINDE2, AINDE3 WITH Y1 AND Y2 BELOW (TO SAVE SPACE)
CCCCC DIMENSION AINDE2(MAXOBV)
CCCCC DIMENSION AINDE3(MAXOBV)
C
CCCCC DIMENSION IZROV(MAXOBV)
CCCCC DIMENSION IPOSV(MAXOBV)
      DIMENSION IZROV(*)
      DIMENSION IPOSV(*)
C
CCCCC DIMENSION DMEAN(MAXOBV)
CCCCC DIMENSION DSSQD(MAXOBV)
      DIMENSION DMEAN(*)
      DIMENSION DSSQD(*)
C
C---------------------------------------------------------------------
C
CCCCC JULY 1993.  ADD FOLLOWING COMMON BLOCK FOR PRINCIPLE COMPONENTS
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC JULY 1993.  FOLLOWING LINE ADDED FOR RANK.
      DATA RMXINT /134217727. /
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='MATA'
      ISUBN2='RI  '
      IERROR='NO'
C
CCCCC JULY 1993.  
CCCCC PCCASE='DACR'
      PCCASE=IPCMTY
      AMINOR=CPUMIN
      SCAL9=CPUMIN
      COFACT=CPUMIN
      DET=CPUMIN
C
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
C
      NRJ=(-999)
      NCJ=(-999)
C
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      D999=(-999.0D0)
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GOTO190
C
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52 FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1        (A4,2X),A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE
   53 FORMAT('IMCASE,IMSUBC,IWRITE = ',2(A4,2X),A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NUMVAR,YS1,YS2,YS3,YS4,DSSQD(1)
   55 FORMAT('NUMVAR,YS1,YS2,YS3,YS4,DSSQD(1) = ',I8,5G15.7)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)NR1,NC1
   61 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GOTO69
      IF(NC1.LE.0)GOTO69
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO62I=1,NR1
      WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   69 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NR2,NC2
   71 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO79
      IF(NC2.LE.0)GOTO79
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO72I=1,NR2
      WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   79 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)NR3,NC3
   81 FORMAT('NR3,NC3 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR3.LE.0)GOTO89
      IF(NC3.LE.0)GOTO89
      JMAX=NC3
      IF(JMAX.GT.10)JMAX=10
      DO82I=1,NR3
      WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   82 CONTINUE
   89 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)N1
  111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GOTO119
      DO112I=1,N1
      WRITE(ICOUT,113)I,Y1(I)
  113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  119 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)N2
  121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GOTO129
      DO122I=1,N2
      WRITE(ICOUT,123)I,Y2(I)
  123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  122 CONTINUE
  129 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)N3
  131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GOTO139
      DO132I=1,N3
      WRITE(ICOUT,133)I,Y3(I)
  133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  132 CONTINUE
  139 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,141)N4
  141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GOTO149
      DO142I=1,N4
      WRITE(ICOUT,143)I,Y4(I)
  143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  142 CONTINUE
  149 CONTINUE
C
  190 CONTINUE
C
C               **************************************************
C               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
C               **************************************************
C
      DNR1=NR1
      DNC1=NC1
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
C               ********************************************
C
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GOTO1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GOTO1100
C
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GOTO1100
C
      GOTO1190
C
 1100 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('      THE INPUT NUMBER OF ROWS AND/OR COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('      IN THE MATRIX AND/OR VECTOR FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAAD')WRITE(ICOUT,1121)
 1121 FORMAT('      THE MATRIX     ADDITION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAAD')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASU')WRITE(ICOUT,1122)
 1122 FORMAT('      THE MATRIX     SUBTRACTION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MASU')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAMU')WRITE(ICOUT,1123)
 1123 FORMAT('      THE MATRIX     MULTIPLICATION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAMU')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASO')WRITE(ICOUT,1124)
 1124 FORMAT('      THE MATRIX     SOLUTION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MASO')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAIN')WRITE(ICOUT,1125)
 1125 FORMAT('      THE MATRIX     INVERSE IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAIN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MATR')WRITE(ICOUT,1126)
 1126 FORMAT('      THE MATRIX     TRANSPOSE IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MATR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAAJ')WRITE(ICOUT,1127)
 1127 FORMAT('      THE MATRIX     ADJOINT IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAAJ')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACE')WRITE(ICOUT,1128)
 1128 FORMAT('      THE MATRIX CHARACTERISTIC EQUATION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MACE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEA')WRITE(ICOUT,1129)
 1129 FORMAT('      THE MATRIX     EIGENVALUES ARE TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAEA')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEE')WRITE(ICOUT,1130)
 1130 FORMAT('      THE MATRIX     EIGENVECTORS ARE TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAEE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MARA')WRITE(ICOUT,1131)
 1131 FORMAT('      THE MATRIX     RANK IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MARA')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MADE')WRITE(ICOUT,1132)
 1132 FORMAT('      THE MATRIX     DETERMINANT IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MADE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAPE')WRITE(ICOUT,1133)
 1133 FORMAT('      THE MATRIX     PERMANENT IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAPE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASN')WRITE(ICOUT,1134)
 1134 FORMAT('      THE MATRIX     SPECTRAL NORM IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MASN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASR')WRITE(ICOUT,1135)
 1135 FORMAT('      THE MATRIX     SPECTRAL RADIUS IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MASR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANR')WRITE(ICOUT,1136)
 1136 FORMAT('      THE MATRIX     NUMBER OF ROWS IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MANR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANC')WRITE(ICOUT,1137)
 1137 FORMAT('      THE MATRIX     NUMBER OF COLUMNS IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MANC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANC')WRITE(ICOUT,1138)
 1138 FORMAT('      THE MATRIX     SIMPLEX SOLUTION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MANC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MATC')WRITE(ICOUT,1141)
 1141 FORMAT('      THE MATRIX     TRACE IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MATC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASM')WRITE(ICOUT,1142)
 1142 FORMAT('      THE MATRIX     SUBMATRIX IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MASM')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAMI')WRITE(ICOUT,1143)
 1143 FORMAT('      THE MATRIX     MINOR IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAMI')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACF')WRITE(ICOUT,1144)
 1144 FORMAT('      THE MATRIX     COFACTOR IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MACF')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MADF')WRITE(ICOUT,1145)
 1145 FORMAT('      THE MATRIX     DEFINITION IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MADF')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEN')WRITE(ICOUT,1146)
 1146 FORMAT('      THE MATRIX     EUCLIDEAN NORM IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAEN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAVC')WRITE(ICOUT,1151)
 1151 FORMAT('      THE VARIANCE-COVARIANCE MATRIX IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAVC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACO')WRITE(ICOUT,1152)
 1152 FORMAT('      THE CORRELATION MATRIX IS TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MACO')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAPC')WRITE(ICOUT,1153)
 1153 FORMAT('      THE PRINCIPLE COMPONENTS ARE TO BE ',
     1'COMPUTED')
      IF(IMCASE.EQ.'MAPC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE(1:3).EQ.'MAP'.AND.IMCASE(4:4).NE.'C')THEN
         WRITE(ICOUT,1154)
 1154    FORMAT('      THE ... PRINCIPLE COMPONENT TO BE ',
     1   'COMPUTED')
         CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,1181)
 1181 FORMAT('      MUST BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1)WRITE(ICOUT,1183)NR1,NC1
 1183 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1)CALL DPWRST('XXX','BUG ')
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2)WRITE(ICOUT,1184)NR2,NC2
 1184 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2)CALL DPWRST('XXX','BUG ')
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3)WRITE(ICOUT,1185)NR3,NC3
 1185 FORMAT('            MATRIX 3--',I8,' ROWS BY ',I8,' COLUMNS')
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3)CALL DPWRST('XXX','BUG ')
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1)WRITE(ICOUT,1186)N1
 1186 FORMAT('            VECTOR 1--',I8,' ROWS')
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1)CALL DPWRST('XXX','BUG ')
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2)WRITE(ICOUT,1187)N2
 1187 FORMAT('            VECTOR 2--',I8,' ROWS')
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2)CALL DPWRST('XXX','BUG ')
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3)WRITE(ICOUT,1188)N3
 1188 FORMAT('            VECTOR 3--',I8,' ROWS')
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3)CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1190 CONTINUE
C
C               *********************************
C               **  STEP 12--                  **
C               **  BRANCH TO THE PROPER CASE  **
C               *********************************
C
      IF(IMCASE.EQ.'MAAD')GOTO2100
      IF(IMCASE.EQ.'MASU')GOTO2200
      IF(IMCASE.EQ.'MAMU')GOTO2300
      IF(IMCASE.EQ.'MASO')GOTO2400
      IF(IMCASE.EQ.'MAIN')GOTO2500
      IF(IMCASE.EQ.'MACN')GOTO2560
      IF(IMCASE.EQ.'MARC')GOTO2560
      IF(IMCASE.EQ.'MATR')GOTO2600
      IF(IMCASE.EQ.'MAAJ')GOTO2700
      IF(IMCASE.EQ.'MACE')GOTO2800
      IF(IMCASE.EQ.'MAEA')GOTO2900
      IF(IMCASE.EQ.'MAEE')GOTO3000
      IF(IMCASE.EQ.'MARA')GOTO3100
      IF(IMCASE.EQ.'MADE')GOTO3200
      IF(IMCASE.EQ.'MAPE')GOTO3300
      IF(IMCASE.EQ.'MASN')GOTO3400
      IF(IMCASE.EQ.'MASR')GOTO3500
      IF(IMCASE.EQ.'MANR')GOTO3600
      IF(IMCASE.EQ.'MANC')GOTO3700
      IF(IMCASE.EQ.'MASS')GOTO3800
      IF(IMCASE.EQ.'MATC')GOTO4100
      IF(IMCASE.EQ.'MASM')GOTO4200
      IF(IMCASE.EQ.'MAMI')GOTO4300
      IF(IMCASE.EQ.'MACF')GOTO4400
      IF(IMCASE.EQ.'MADF')GOTO4500
      IF(IMCASE.EQ.'MAEN')GOTO4600
      IF(IMCASE.EQ.'MAVC')GOTO5100
      IF(IMCASE.EQ.'MACO')GOTO5200
      IF(IMCASE.EQ.'MACC')GOTO5200
      IF(IMCASE.EQ.'MACP')GOTO5200
C
      IF(IMCASE.EQ.'MAPC')GOTO5300
      IF(IMCASE.EQ.'MAP1')GOTO5300
      IF(IMCASE.EQ.'MAP2')GOTO5300
      IF(IMCASE.EQ.'MAP3')GOTO5300
      IF(IMCASE.EQ.'MAP4')GOTO5300
      IF(IMCASE.EQ.'MAP5')GOTO5300
      IF(IMCASE.EQ.'MAP6')GOTO5300
      IF(IMCASE.EQ.'MAP7')GOTO5300
      IF(IMCASE.EQ.'MAP8')GOTO5300
      IF(IMCASE.EQ.'MAP9')GOTO5300
      IF(IMCASE.EQ.'MA10')GOTO5300
CCCCCC OCTOBER 1993.  FOLLOWING OPERATIONS MOVED TO MATAR2
CCCCC JULY 1993.  ADD FOLLOWING 3 LINES
CCCCC IF(IMCASE.EQ.'MASV')GOTO5800
CCCCC IF(IMCASE.EQ.'MASD')GOTO5900
CCCCC IF(IMCASE.EQ.'MASF')GOTO6000
CCCCC SEPTEMBER 1993.  ADD FOLLOWING 2 LINES
CCCCC IF(IMCASE.EQ.'MARW')GOTO6100
CCCCC IF(IMCASE.EQ.'MAEL')GOTO6200
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
CCCCC IF(IMCASE.EQ.'MACH')GOTO6300
CCCCC IF(IMCASE.EQ.'MAAU')GOTO6400
CCCCC IF(IMCASE.EQ.'MADI')GOTO6500
CCCCC IF(IMCASE.EQ.'DIMA')GOTO6600
CCCCC IF(IMCASE.EQ.'MARR')GOTO6700
CCCCC IF(IMCASE.EQ.'MARE')GOTO6800
CCCCC IF(IMCASE.EQ.'MATD')GOTO6900
CCCCC IF(IMCASE.EQ.'MATS')GOTO7000
CCCCC IF(IMCASE.EQ.'MATI')GOTO7100
CCCCC IF(IMCASE.EQ.'MAIS')GOTO7200
C
      IF(IMCASE.EQ.'MATZ')GOTO6100
      IF(IMCASE.EQ.'MAUZ')GOTO6200
      IF(IMCASE.EQ.'MACM')GOTO6300
      IF(IMCASE.EQ.'MPCO')GOTO6400
      IF(IMCASE.EQ.'MPCC')GOTO6400
      IF(IMCASE.EQ.'MPCP')GOTO6400
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** INTERNAL ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      IMCASE NOT EQUAL TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      MAAD, MASU, MAMU, MASO, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      MAIN, MATR, MAAJ, MACE, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      MAEA, MAEE, MARA, MADE, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      MAPE, MASN, MASR, MANR, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1217)
 1217 FORMAT('      MANC, MASS,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1221)
 1221 FORMAT('      MAVC, MACO, MAPC, OR MAPX ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1228)IMCASE
 1228 FORMAT('      IMCASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *********************************************
C               **  STEP 21--                              **
C               **  TREAT THE MATRIX     ADDITION    CASE  **
C               *********************************************
C
 2100 CONTINUE
C
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GOTO2110
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GOTO2130
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GOTO2150
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GOTO2170
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GOTO2180
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX ADDITION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2103)ITYPA1
 2103 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2104)ITYPA2
 2104 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 2110 CONTINUE
      IF(NR1.EQ.NR2.AND.NC1.EQ.NC2)GOTO2119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2111)
 2111 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2112)
 2112 FORMAT('      FOR MATRIX ADDITION OF MATRIX 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2113)
 2113 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2114)
 2114 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2115)
 2115 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2116)
 2116 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2117)NR1,NC1
 2117 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2118)NR2,NC2
 2118 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2119 CONTINUE
C
      DO2121I=1,NR1
      DO2122J=1,NC1
      DYM1=YM1(I,J)
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2122 CONTINUE
 2121 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      GOTO9000
C
 2130 CONTINUE
      IF(NR1.EQ.N2)GOTO2139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2131)
 2131 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2132)
 2132 FORMAT('      FOR MATRIX ADDITION OF MATRIX 1 & VECTOR 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2133)
 2133 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2134)
 2134 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2135)
 2135 FORMAT('      THE NUMBER OF ROWS IN VECTOR 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2136)
 2136 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2137)NR1,NC1
 2137 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2138)N2
 2138 FORMAT('            VECTOR 2--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2139 CONTINUE
C
      DO2141I=1,NR1
      DYM2=Y2(I)
      DO2142J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2142 CONTINUE
 2141 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 2150 CONTINUE
      IF(N1.EQ.NR2)GOTO2159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2151)
 2151 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2152)
 2152 FORMAT('      FOR MATRIX ADDITION OF VECTOR 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2153)
 2153 FORMAT('      THE NUMBER OF ROWS IN VECTOR 1;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2154)
 2154 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2155)
 2155 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2156)
 2156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2157)N1
 2157 FORMAT('            VECTOR 1--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2158)NR2,NC2
 2158 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2159 CONTINUE
C
      DO2161I=1,NR2
      DYM1=Y1(I)
      DO2162J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2162 CONTINUE
 2161 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      GOTO9000
C
 2170 CONTINUE
      DYM2=YS2
      DO2171I=1,NR1
      DO2172J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2172 CONTINUE
 2171 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 2180 CONTINUE
      DYM1=YS1
      DO2181I=1,NR2
      DO2182J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2182 CONTINUE
 2181 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GOTO9000
C
C               *********************************************
C               **  STEP 22--                              **
C               **  TREAT THE MATRIX     SUBTRACTION CASE  **
C               *********************************************
C
 2200 CONTINUE
C
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GOTO2210
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GOTO2230
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GOTO2250
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GOTO2270
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GOTO2280
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
 2201 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2202)
 2202 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX SUBTRACTION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2203)ITYPA1
 2203 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2204)ITYPA2
 2204 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 2210 CONTINUE
      IF(NR1.EQ.NR2.AND.NC1.EQ.NC2)GOTO2219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2211)
 2211 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2212)
 2212 FORMAT('      FOR MATRIX SUBTRACTION OF MATRIX 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2213)
 2213 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2214)
 2214 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2215)
 2215 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2216)
 2216 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2217)NR1,NC1
 2217 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2218)NR2,NC2
 2218 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2219 CONTINUE
C
C
      DO2221I=1,NR1
      DO2222J=1,NC1
      DYM1=YM1(I,J)
      DYM2=YM2(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2222 CONTINUE
 2221 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 2230 CONTINUE
      IF(NR1.EQ.N2)GOTO2239
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2231)
 2231 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2232)
 2232 FORMAT('      FOR MATRIX SUBTRACTION OF MATRIX 1 & VECTOR 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2233)
 2233 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2234)
 2234 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2235)
 2235 FORMAT('      THE NUMBER OF ROWS IN VECTOR 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2236)
 2236 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2237)NR1,NC1
 2237 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2238)N2
 2238 FORMAT('            VECTOR 2--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2239 CONTINUE
C
      DO2241I=1,NR1
      DYM2=Y2(I)
      DO2242J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2242 CONTINUE
 2241 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 2250 CONTINUE
      IF(N1.EQ.NR2)GOTO2259
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2251)
 2251 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2252)
 2252 FORMAT('      FOR MATRIX SUBTRACTION OF VECTOR 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2253)
 2253 FORMAT('      THE NUMBER OF ROWS IN VECTOR 1;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2254)
 2254 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2255)
 2255 FORMAT('      THE NUMBER OF ROWS AND COLUMNS IN MATRIX 2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2256)
 2256 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2257)N1
 2257 FORMAT('            VECTOR 1--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2258)NR2,NC2
 2258 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2259 CONTINUE
C
      DO2261I=1,NR2
      DYM1=Y1(I)
      DO2262J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2262 CONTINUE
 2261 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GOTO9000
C
 2270 CONTINUE
      DYM2=YS2
      DO2271I=1,NR1
      DO2272J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2272 CONTINUE
 2271 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 2280 CONTINUE
      DYM1=YS1
      DO2281I=1,NR2
      DO2282J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2282 CONTINUE
 2281 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GOTO9000
C
C               *********************************************
C               **  STEP 23--                              **
C               **  TREAT THE MATRIX  MULTIPLICATION CASE  **
C               *********************************************
C
 2300 CONTINUE
C
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GOTO2310
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GOTO2330
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GOTO2350
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GOTO2370
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GOTO2380
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2301)
 2301 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2302)
 2302 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX MULTIPLIC.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2303)ITYPA1
 2303 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2304)ITYPA2
 2304 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 2310 CONTINUE
      IF(NC1.EQ.NR2)GOTO2319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2311)
 2311 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2312)
 2312 FORMAT('      FOR MATRIX MULTIPLIC. OF MATRIX 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2313)
 2313 FORMAT('      THE NUMBER OF COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2314)
 2314 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2315)
 2315 FORMAT('      THE NUMBER OF ROWS    IN MATRIX 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2316)
 2316 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2317)NR1,NC1
 2317 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2318)NR2,NC2
 2318 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2319 CONTINUE
C
      DO2321I=1,NR1
      DO2322J=1,NC2
      DSUM=0.0D0
      DO2323K=1,NC1
      DYM1=YM1(I,K)
      DYM2=YM2(K,J)
      DYM9=DYM1*DYM2
      DSUM=DSUM+DYM9
 2323 CONTINUE
      YM9(I,J)=DSUM
 2322 CONTINUE
 2321 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC2
      IUPFLG='FULL'
      GOTO9000
C
 2330 CONTINUE
      IF(NC1.EQ.N2)GOTO2339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2331)
 2331 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2332)
 2332 FORMAT('      FOR MATRIX MULTIPLIC. OF MATRIX 1 & VECTOR 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2333)
 2333 FORMAT('      THE NUMBER OF COLUMNS IN MATRIX 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2334)
 2334 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2335)
 2335 FORMAT('      THE NUMBER OF ROWS IN VECTOR 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2336)
 2336 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2337)NR1,NC1
 2337 FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2338)N2
 2338 FORMAT('            VECTOR 2--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2339 CONTINUE
C
      DO2341I=1,NR1
        J=1
        DSUM=0.0D0
        DO2343K=1,NC1
          DYM1=YM1(I,K)
          DYM2=Y2(K)
          DYM9=DYM1*DYM2
          DSUM=DSUM+DYM9
 2343   CONTINUE
        VECT9(I)=DSUM
 2341 CONTINUE
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
 2350 CONTINUE
      IF(1.EQ.NR2)GOTO2359
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2351)
 2351 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2352)
 2352 FORMAT('      FOR MATRIX MULTIPLIC. OF VECTOR 1 & MATRIX 2,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2355)
 2355 FORMAT('      THE NUMBER OF ROWS IN MATRIX 2 MUST = 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2356)
 2356 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2358)NR2,NC2
 2358 FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2359 CONTINUE
C
      DO2361I=1,NR1
        DO2362J=1,NC2
          DSUM=0.0D0
          K=1
          DYM1=Y1(I)
          DYM2=YM2(K,J)
          DYM9=DYM1*DYM2
          DSUM=DSUM+DYM9
          YM9(I,J)=DSUM
 2362   CONTINUE
 2361 CONTINUE
      ITYP9='MATR'
      NR9=N1
      NC9=NC2
      IUPFLG='FULL'
      GOTO9000
C
 2370 CONTINUE
      DYM2=YS2
      DO2371I=1,NR1
      DO2372J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1*DYM2
      YM9(I,J)=DYM9
 2372 CONTINUE
 2371 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 2380 CONTINUE
      DYM1=YS1
      DO2381I=1,NR2
      DO2382J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1*DYM2
      YM9(I,J)=DYM9
 2382 CONTINUE
 2381 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 24--                              **
C               **  TREAT THE MATRIX     SOLUTION    CASE  **
C               **  REFERENCE--PRESS ET AL, PAGE 37        **
C               *********************************************
C
 2400 CONTINUE
C
      IF(NR1.EQ.N2)GOTO2409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2401)
 2401 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2402)
 2402 FORMAT('      FOR SOLVING A MATRIX EQUATION SUCH AS A*X = B,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2403)
 2403 FORMAT('      THE NUMBER OF ROWS IN THE LEFT-SIDE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2404)
 2404 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2405)
 2405 FORMAT('      THE NUMBER OF ROWS IN THE RIGHT-SIDE VECTOR;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2406)
 2406 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2407)NR1
 2407 FORMAT('              NUMBER OF ROWS IN THE MATRIX = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2408)N2
 2408 FORMAT('              NUMBER OF ROWS IN THE VECTOR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2409 CONTINUE
C
      DO2451I=1,N2
CCCCC VECT9(I)=YM2(1,I)
CCCCC VECT9(I)=YM2(I,1)
      VECT9(I)=Y2(I)
 2451 CONTINUE
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK 
CCCCC ALGORITHM.  
CCCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
CCCCC CALL LUBKSB(YMJUNK,NR1,MAXROM,INDEX,VECT9)
      CALL SGECO(YM1,MAXROM,NR1,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2461)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
 2461 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE MATRIX = ',E15.7)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2471)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,2472)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
      ELSE
        IJOB=0
        CALL SGESL(YM1,MAXROM,NR1,INDEX,VECT9,IJOB)
      END IF
 2471 FORMAT('****** ERROR IN MATARI ********')
 2472 FORMAT('       THE INPUT MATRIX IS SINGULAR')
CCCCC END CHANGE
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 25--                              **
C               **  TREAT THE MATRIX     INVERSE     CASE  **
C               **  REFERENCE--PRESS ET AL, PAGE 38        **
C               *********************************************
C
 2500 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2501)
 2501   FORMAT('***** ERROR IN MATRIX INVERSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2502)
 2502   FORMAT('      FOR MATRIX INVERSE, THE NUMBER OF ROWS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2503)
 2503   FORMAT('      MATRIX MUST EQUAL THE NUMBER OF COLUMNS IN THE ',
     1         'MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2504)NR1
 2504   FORMAT('            NUMBER OF ROWS    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2505)NC1
 2505   FORMAT('            NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO2511I=1,NR1
        DO2512J=1,NC1
          YM9(I,J)=0.0
 2512   CONTINUE
        YM9(I,I)=1.0
 2511 CONTINUE
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK
CCCCC ALGORITHM.
C
CCCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
C
CCCCC DO2521J=1,NR1
CCCCC CALL LUBKSB(YMJUNK,NR1,MAXROM,INDEX,YM9(1,J))
C2521 CONTINUE
      CALL SGECO(YM1,MAXROM,NR1,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2521)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
 2521 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE MATRIX = ',G15.7)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2561)
        CALL DPWRST('XXX','ERRO')
        WRITE(ICOUT,2523)
 2523   FORMAT('       THE INPUT MATRIX IS SINGULAR.')
        CALL DPWRST('XXX','ERRO')
        IERROR='YES'
      ELSE
        IJOB=1
        CALL SGEDI(YM1,MAXROM,NR1,INDEX,Y3,Y4,IJOB)
        DO2531J=1,NC1
          DO2532I=1,NR1
            YM9(I,J)=YM1(I,J)
 2532     CONTINUE
 2531   CONTINUE
      END IF
CCCCC END CHANGE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 25B-                              **
C               **  TREAT THE MATRIX CONDITION NUMBER CASE **
C               *********************************************
C
 2560 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2561)
 2561   FORMAT('***** ERROR IN MATRIX CONDITION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2562)
 2562   FORMAT('      FOR MATRIX CONDITION NUMBER, THE NUMBER OF ',
     1         'ROWS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2563)
 2563   FORMAT('      MATRIX MUST EQUAL THE NUMBER OF COLUMNS IN THE ',
     1         'MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2564)NR1
 2564   FORMAT('            NUMBER OF ROWS    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2565)NC1
 2565   FORMAT('            NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      CALL SGECO(YM1,MAXROM,NR1,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2521)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
C
      ITYP9='SCAL'
      SCAL9=RCOND
      IF(IMCASE.EQ.'MACN')SCAL9=1.0/RCOND
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 26--                              **
C               **  TREAT THE MATRIX     TRANSPOSE   CASE  **
C               *********************************************
C
 2600 CONTINUE
C
      IF(NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2601)
 2601   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2603)NR1
 2603   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX,',I5,
     1         'EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2605)MAXCOM
 2605   FORMAT('      NUMBER OF COLUMNS FOR A MATRIX,',I5,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2607)
 2607   FORMAT('      THE MATRIX TRANSPOSE WAS NOT COMPUTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO2611I=1,NR1
      DO2612J=1,NC1
      YM9(J,I)=YM1(I,J)
 2612 CONTINUE
 2611 CONTINUE
C
      ITYP9='MATR'
      NR9=NC1
      NC9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 27--                              **
C               **  TREAT THE MATRIX     ADJOINT     CASE  **
C               *********************************************
CCCCC JULY 1993.  IMPLENENT THIS COMMAND.  NOTE THAT THE CLASSICAL
CCCCC ADJOINT IS ESSENTIALLY THE MATRIX CONTAINING THE COFACTORS 
CCCCC FOR EACH ELEMENT.  THIS CALCULATES THE DETERMINANT AT 
CCCCC EACH MATRIX SUB-ELEMENT, SO CAN GET TIME-CONSUMING FOR LARGE
CCCCC MATRICES.
C
 2700 CONTINUE
C
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2711)
C2711 FORMAT('***** ERROR IN MATARI--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2712)
C2712 FORMAT('      THE MATRIX ADJOINT COMMAND')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2713)
C2713 FORMAT('      IS NOT YET IMPLEMENTED.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
C
      IF(NR1.EQ.NC1)GOTO2709
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2701)
 2701 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2702)
 2702 FORMAT('      FOR MATRIX ADJOINT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2703)
 2703 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2704)
 2704 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2705)
 2705 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2706)
 2706 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2707)NR1
 2707 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2708)NC1
 2708 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2709 CONTINUE
C
      DO2790IROWID=1,NR1
      IYS2=IROWID
      DO2780ICOLID=1,NC1
      IYS3=ICOLID
      I2=0
      J2=0
      DO2711I=1,NR1
      IF(I.EQ.IYS2)GOTO2711
      I2=I2+1
      NRJ=I2
      J2=0
      DO2712J=1,NC1
      IF(J.EQ.IYS3)GOTO2712
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 2712 CONTINUE
 2711 CONTINUE
C
      IF(NRJ.GE.1.AND.NCJ.GE.1)GOTO2729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2721)
 2721 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2722)
 2722 FORMAT('      FOR MATRIX COFACTOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2723)
 2723 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2724)
 2724 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2725)
 2725 FORMAT('      MUST BOTH BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2726)
 2726 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2727)NRJ
 2727 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2728)NCJ
 2728 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2729 CONTINUE
C
      IF(NRJ.EQ.NCJ)GOTO2739
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2731)
 2731 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2732)
 2732 FORMAT('      FOR MATRIX ADJOINT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2733)
 2733 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2734)
 2734 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2735)
 2735 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2736)
 2736 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2737)NRJ
 2737 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2738)NCJ
 2738 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2739 CONTINUE
C
      CALL SGECO(YM2,MAXROM,NRJ,INDEX,RCOND,Y3)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2771)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,2772)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,2773)IROWID,ICOLID
        CALL DPWRST('XXX','ERRO ')
        COFACT=0.0
        IERROR='YES'
      ELSE
        IJOB=10
        CALL SGEDI(YM2,MAXROM,NRJ,INDEX,Y3,Y4,IJOB)
        DET=Y3(1)*10.0**Y3(2)
        COFACT=DET
        IYS23=IYS2+IYS3
        IREM=IYS23-2*(IYS23/2)
        IF(IREM.EQ.1)COFACT=(-COFACT)
      END IF
 2771 FORMAT('****** ERROR IN MATARI ********')
 2772 FORMAT('       UNABLE TO COMPUTE THE DETERMINANT FOR')
 2773 FORMAT('       ROW ',I4,' AND COLUMN ',I4)
CCCCC END CHANGE
C
      YM9(IROWID,ICOLID)=COFACT
 2780 CONTINUE
 2790 CONTINUE
C
      ITYP9='MATR'
      NC9=NR1
      NR9=NR1
      SCAL9=COFACT
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 28--                                        **
C               **  TREAT THE MATRIX CHARACTERISTIC EQUATION   CASE  **
C               *******************************************************
C
 2800 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2811)
 2811 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2812)
 2812 FORMAT('      THE MATRIX CHARACTERISTIC EQUATION COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2813)
 2813 FORMAT('      IS NOT YET IMPLEMENTED.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C
C               *********************************************
C               **  STEP 29--                              **
C               **  TREAT THE MATRIX     EIGENVALUES CASE  **
C               *********************************************
C
 2900 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2901)
 2901   FORMAT('***** ERROR IN MATRIX EIGENVALUES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2903)
 2903   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX MUST EQUAL THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2905)
 2905   FORMAT('      NUMBER OF COLUMNS IN THE MATRIX;  SUCH WAS NOT ',
     1         'THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2907)NR1
 2907   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2908)NC1
 2908   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO2911I=1,NR1
        I2=I
        DO2912J=I,NC1
          J2=J
          YM1IJ=YM1(I,J)
          YM1JI=YM1(J,I)
          IF(YM1IJ.NE.YM1JI)GOTO2930
 2912   CONTINUE
 2911 CONTINUE
      GOTO2939
C
CCCCC JULY 1993.  ADD SUPPORT FOR NON-SYMMETRIC CASE.  THIS CASE
CCCCC CAN HAVE COMPLEX EIGENVALUES.  ROWS 1 THROUGH N OF THE OUTPUT
CCCCC VECTOR WILL CONTAIN THE REAL COMPONENT, ROWS N+1 THROUGH 2*N 
CCCCC WILL CONTAIN THE COMPLEX COMPONENT.
 2930 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2931)
C2931 FORMAT('***** ERROR IN MATARI--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2932)
C2932 FORMAT('      FOR MATRIX EIGENVALUES,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2933)
C2933 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2935)
C2935 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2936)
C2936 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2937)I2,J2,YM1IJ
C2937 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2938)J2,I2,YM1JI
C2938 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
C
      IERR2=0
      IJOB=0
      CALL SGEEV(YM1,MAXROM,NR1,VECT9,YM2,MAXROM,Y3,
     1IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,2941)
        WRITE(ICOUT,2942)
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,2941)
        WRITE(ICOUT,2947)
        WRITE(ICOUT,2948)IERR2
        WRITE(ICOUT,2949)IERR2-1
      END IF
 2941 FORMAT('******** ERROR FROM MATRIX EIGENVALUES--')
 2942 FORMAT('         PROBLEM WITH MATRIX DIMENSIONS')
 2947 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 2948 FORMAT('         FOR EIGENVALUE ',I4)
 2949 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGE
C
      ITYP9='VECT'
      NVECT9=2*NR1
      GOTO9000
CCCCC END CHANGES
 2939 CONTINUE
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
CCCCC ALGORITHM.
CCCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VECT9,YMJUN2,NJACIT)
C
      IERR2=0
      IJOB=0
      CALL SSIEV(YM1,MAXROM,NR1,VECT9,Y3,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        WRITE(ICOUT,2962)
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        WRITE(ICOUT,2963)
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        WRITE(ICOUT,2967)
        WRITE(ICOUT,2968)IERR2
        WRITE(ICOUT,2969)IERR2-1
      END IF
 2961 FORMAT('******** ERROR FROM MATARI ************')
 2962 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 2963 FORMAT('         LESS THAN 1 ROW')
 2967 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 2968 FORMAT('         FOR EIGENVALUE ',I4)
 2969 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGE
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 30--                              **
C               **  TREAT THE MATRIX    EIGENVECTORS CASE  **
C               *********************************************
C
 3000 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3001)
 3001   FORMAT('***** ERROR IN MATRIX EIGENVECTORS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2903)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2905)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2907)NR1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2908)NC1
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO3011I=1,NR1
        I2=I
        DO3012J=I,NC1
          J2=J
          YM1IJ=YM1(I,J)
          YM1JI=YM1(J,I)
          IF(YM1IJ.NE.YM1JI)GOTO3030
 3012   CONTINUE
 3011 CONTINUE
      GOTO3039
C
 3030 CONTINUE
CCCCC JULY 1993.  ADD SUPPORT FOR NON-SYMMETRIC CASE.  THIS CASE
CCCCC CAN HAVE COMPLEX EIGENVECTORS.  ROWS 1 THROUGH N OF THE OUTPUT
CCCCC MATRIX WILL CONTAIN THE REAL COMPONENT, ROWS N+1 THROUGH 2*N 
CCCCC WILL CONTAIN THE COMPLEX COMPONENT.
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3031)
C3031 FORMAT('***** ERROR IN MATARI--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3032)
C3032 FORMAT('      FOR MATRIX EIGENVECTORS,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3033)
C3033 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3035)
C3035 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3036)
C3036 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3037)I2,J2,YM1IJ
C3037 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3038)J2,I2,YM1JI
C3038 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
C
      IERR2=0
      IJOB=1
      DO3021J=1,MAXCOM
        DO3022I=1,MAXROM
          IF(J.GT.NR1 .OR. I.GT.NR1)YM1(I,J)=0.0
          YM2(I,J)=0.0
          YM9(I,J)=0.0
 3022   CONTINUE
 3021 CONTINUE
      DO3023I=1,MAXOBV
        VECT9(I)=0.0
        Y3(I)=0.0
 3023 CONTINUE
C
      CALL SGEEV(YM1,MAXROM,NR1,VECT9,YM2,MAXROM,Y3,
     1IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        WRITE(ICOUT,3042)
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        WRITE(ICOUT,3047)
        WRITE(ICOUT,3048)IERR2
        WRITE(ICOUT,3049)IERR2-1
      ELSE
        DO3045J=1,NR1
        DO3044I=1,2*NR1
        YM9(I,J)=YM2(I,J)
 3044   CONTINUE
 3045   CONTINUE
      END IF
 3042 FORMAT('         PROBLEM WITH MATRIX DIMENSIONS')
 3047 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3048 FORMAT('         FOR EIGENVALUE ',I4)
 3049 FORMAT('         EIGENVECTORS 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGE
C
      ITYP9='MATR'
      NR9=2*NR1
      NC9=NC1
      IUPFLG='FULL'
CCCCC END CHANGES
      GOTO9000
 3039 CONTINUE
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
CCCCC ALGORITHM.
CCCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VJUNK,YM9,NJACIT)
C
      IERR2=0
      IJOB=1
      DO3071I=1,MAXOBV
        VECT9(I)=0.0
        Y3(I)=0.0
 3071 CONTINUE
      CALL SSIEV(YM1,MAXROM,NR1,VECT9,Y3,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        WRITE(ICOUT,3062)
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        WRITE(ICOUT,3063)
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        WRITE(ICOUT,3067)
        WRITE(ICOUT,3068)IERR2
        WRITE(ICOUT,3069)IERR2-1
      ELSE
        DO3080J=1,NR1
        DO3082I=1,NR1
        YM9(I,J)=YM1(I,J)
 3082   CONTINUE
 3080   CONTINUE
      END IF
 3062 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3063 FORMAT('         LESS THAN 1 ROW')
 3067 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3068 FORMAT('         FOR EIGENVALUE ',I4)
 3069 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 31--                                 **
C               **  TREAT THE MATRIX     RANK        CASE     **
C               **  COMPUTE FROM SINGULAR VALUE DECOMPOSITION **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 3100 CONTINUE
C
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y3,YM1,MAXROM,
     1YM1,MAXROM,Y4,IJOB,IERR2)
      ARANK=0.
      IF(ITYPA2.EQ.'PARA')THEN
        ATOL=YS2
      ELSE
CCCCC   ATOL=0.0000001
        CALL SPDIV(RMXINT,2.0,IND,RESULT)
        ETA=RESULT+1.0
        CALL SPDIV(1.0,ETA,IND,ETA)
        ATOL=REAL(MAX(NR1,NC1))*VECT9(1)*ETA
      ENDIF
      NLAST=MIN(NR1,NC1)
      DO3120I=1,NLAST
      IF(VECT9(I).LE.ATOL)THEN
        ARANK=REAL(I-1)
        GOTO3129
      ENDIF
 3120 CONTINUE
      ARANK=REAL(NLAST)
 3129 CONTINUE 
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GOTO3190
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3151)
 3151 FORMAT('***** COMPUTING RANK--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3152)EPS,VECT9(1),ATOL
 3152 FORMAT('EPS,VECT((1),ATOL = ',
     1E15.7,2X,E15.7,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      NLAST=MIN(NR1+1,NC1)
      DO3180I=1,NLAST
      WRITE(ICOUT,3183)I,VECT9(I)
 3183 FORMAT('I,VECT9(I) = ',I4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 3180 CONTINUE
C
 3190 CONTINUE
C
CCCCC END CHANGE
C
      ITYP9='SCAL'
      SCAL9=ARANK
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 32--                              **
C               **  TREAT THE MATRIX     DETERMINANT CASE  **
C               **  REFERENCE--PRESS ET AL, PAGE 39        **
C               *********************************************
C
 3200 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO3209
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3201)
 3201 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3202)
 3202 FORMAT('      FOR MATRIX DETERMINANT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3203)
 3203 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3204)
 3204 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3205)
 3205 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3206)
 3206 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3207)NR1
 3207 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3208)NC1
 3208 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3209 CONTINUE
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK
CCCCC ALGORITHM.
CCCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
C
CCCCC DET=DP1M1
CCCCC DO3221I=1,NR1
CCCCC DET=DET*YMJUNK(I,I)
C3221 CONTINUE
      CALL SGECO(YM1,MAXROM,NR1,INDEX,RCOND,Y3)
      WRITE(ICOUT,3261)RCOND
      CALL DPWRST('XXX','TEXT ')
 3261 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE MATRIX = ',E15.7)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,3271)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,3272)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
      ELSE
        IJOB=10
        CALL SGEDI(YM1,MAXROM,NR1,INDEX,Y3,Y4,IJOB)
        DET=Y3(1)*10.0**Y3(2)
      END IF
 3271 FORMAT('****** ERROR IN MATARI ********')
 3272 FORMAT('       THE INPUT MATRIX IS SINGULAR')
CCCCC END CHANGE
C
      ITYP9='SCAL'
      SCAL9=DET
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 33--                              **
C               **  TREAT THE MATRIX     PERMANENT   CASE  **
C               *********************************************
C
 3300 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3301)
 3301   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3302)
 3302   FORMAT('      FOR MATRIX PERMANENT, THE NUMBER OF ROWS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3305)
 3305   FORMAT('      MATRIX MUST EQUAL THE NUMBER OF COLUMNS IN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3306)
 3306   FORMAT('      THE MATRIX;  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3307)NR1
 3307   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3308)NC1
 3308   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NR1.GT.50)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3311)
 3311   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3312)
 3312   FORMAT('      FOR MATRIX PERMANENT, THE NUMBER OF ROWS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3315)
 3315   FORMAT('      MATRIX IS CURRENTLY RESTRICTED TO 50 OR LESS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3317)NR1
 3317   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      CALL PERMAN(YM1,MAXROM,NR1,INDEX,Y3,APERM)
C
      ITYP9='SCAL'
      SCAL9=APERM
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 34--                                        **
C               **  TREAT THE MATRIX     SPECTRAL NORM    CASE       **
C               **  SPECTRAL NORM = COMPUTE MATRIX TIMES ITS         **
C               **                  TRANSPOSE, THEN FIND THE SQUARE  **
C               **                  ROOT OF THE EIGENVALUE WITH THE  **
C               **                  LARGEST ABSOLUTE VALUE.          **
C               **  REFERENCE--RALSTON                               **
C               *******************************************************
C
 3400 CONTINUE
C
      IF(NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3411)
 3411   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3413)NR1
 3413   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX,',I5,
     1         'EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3415)MAXCOM
 3415   FORMAT('      NUMBER OF COLUMNS FOR A MATRIX,',I5,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3417)
 3417   FORMAT('      THE MATRIX TRANSPOSE WAS NOT COMPUTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO3421I=1,NR1
      DO3422J=1,NR1
      DSUM=0.0D0
      DO3423K=1,NC1
      DYM1=YM1(I,K)
      DYM2=YM1(J,K)
      DYM9=DYM1*DYM2
      DSUM=DSUM+DYM9
 3423 CONTINUE
      YM2(I,J)=DSUM
 3422 CONTINUE
 3421 CONTINUE
      NRJ=NR1
      NCJ=NR1
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH THE EISPACK
CCCCC ALGORITHM.  NOTE THAT MATRIX TIMES IT TRANSPOSE IS SYMMETRIC, SO
CCCCC USE SYMMERIC VERSION.
CCCCC CALL JACOBI(YMJUNK,NRJ,MAXROM,VJUNK,YMJUN2,NJACIT)
C
      IERR2=0
      IJOB=0
      CALL SSIEV(YM2,MAXROM,NR1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3452)
        GOTO9000
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3453)
        GOTO9000
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3457)
        WRITE(ICOUT,3458)IERR2
        WRITE(ICOUT,3459)IERR2-1
        GOTO9000
      END IF
 3451 FORMAT('******** ERROR FROM MATARI ************')
 3452 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3453 FORMAT('         LESS THAN 1 ROW')
 3457 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3458 FORMAT('         FOR EIGENVALUE ',I4)
 3459 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGES
      AMAX=ABS(Y3(1))
      DO3461I=1,NR1
      IF(ABS(Y3(I)).GT.AMAX)AMAX=ABS(Y3(I))
 3461 CONTINUE
      AMAX2=0.0
      IF(AMAX.GT.0.0)AMAX2=SQRT(AMAX)
C
      ITYP9='SCAL'
      SCAL9=AMAX2
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 35--                                        **
C               **  TREAT THE MATRIX     SPECTRAL RADIUS    CASE     **
C               **  SPECTRAL RADIUS = LARGEST ABS(EIGENVALUE) OF A   **
C               **  REFERENCE--RALSTON                               **
C               *******************************************************
C
 3500 CONTINUE
C
CCCCC JUNE 1995.  EISPACK WILL HANDLE NON-SYMMETRIC MATRICES (FOR
CCCCC EIGENVALUES).  NO NEED TO RESTRICT TO SYMMETRIC MATRICES).
CCCCC IF(NR1.EQ.NC1)GOTO3509
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3501)
C3501 FORMAT('***** ERROR IN MATARI--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3502)
C3502 FORMAT('      FOR MATRIX SPECTRAL RADIUS,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3503)
C3503 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3504)
C3504 FORMAT('      MUST EQUAL')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3505)
C3505 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3506)
C3506 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3507)NR1
C3507 FORMAT('            NUMBER OF ROWS    =',I8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3508)NC1
C3508 FORMAT('            NUMBER OF COLUMNS =',I8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C
      DO3511I=1,NR1
      I2=I
      DO3512J=I,NC1
      J2=J
      YM1IJ=YM1(I,J)
      YM1JI=YM1(J,I)
      IF(YM1IJ.EQ.YM1JI)GOTO3512
      GOTO3530
 3512 CONTINUE
 3511 CONTINUE
      GOTO3539
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
CCCCC EISPACK CAN HANDLE NON-SYMMETRIC MATRICES.
 3530 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3531)
C3531 FORMAT('***** ERROR IN MATARI--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3532)
C3532 FORMAT('      FOR MATRIX SPECTRAL RADIUS,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3533)
C3533 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3535)
C3535 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3536)
C3536 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3537)I2,J2,YM1IJ
C3537 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,3538)J2,I2,YM1JI
C3538 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C
      IERR2=0
      IJOB=0
      CALL SGEEV(YM1,MAXROM,NR1,Y3,YM2,MAXROM,Y4,
     1IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3541)
        WRITE(ICOUT,3542)
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3541)
        WRITE(ICOUT,3547)
        WRITE(ICOUT,3548)IERR2
        WRITE(ICOUT,3549)IERR2-1
      END IF
 3541 FORMAT('******** ERROR FROM MATARI ************')
 3542 FORMAT('         PROBLEM WITH MATRIX DIMENSIONS')
 3547 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3548 FORMAT('         FOR EIGENVALUE ',I4)
 3549 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
C
C  COMPLEX ABSOLUTE VALUE IS DEFINED TO BE: SQRT(REAL**2+COMPLEX**2)
C
      AMAX=0.0
      ATEMP1=Y3(1)**2 + Y3(1+NR1)**2
      IF(ATEMP1.GE.0.0)AMAX=SQRT(ATEMP1)
      DO3538I=1,NR1
      ATEMP1=0.0
      ATEMP2=Y3(I)**2 + Y3(I+NR1)**2
      IF(ATEMP2.GE.0.0)ATEMP1=SQRT(ATEMP2)
      IF(ATEMP1.GT.AMAX)AMAX=ATEMP1
 3538 CONTINUE
      GOTO3599
C
CCCCC END CHANGE
 3539 CONTINUE
C
CCCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
CCCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VJUNK,YMJUN2,NJACIT)
C
      IERR2=0
      IJOB=0
      CALL SSIEV(YM1,MAXROM,NR1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3562)
        GOTO9000
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3563)
        GOTO9000
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3567)
        WRITE(ICOUT,3568)IERR2
        WRITE(ICOUT,3569)IERR2-1
        GOTO9000
      END IF
 3561 FORMAT('******** ERROR FROM MATARI ************')
 3562 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3563 FORMAT('         LESS THAN 1 ROW')
 3567 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3568 FORMAT('         FOR EIGENVALUE ',I4)
 3569 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
CCCCC END CHANGES
C
      AMAX=ABS(Y3(1))
      DO3591I=1,NR1
      IF(ABS(Y3(I)).GT.AMAX)AMAX=ABS(Y3(I))
 3591 CONTINUE
C
 3599 CONTINUE
      ITYP9='SCAL'
      SCAL9=AMAX
      IUPFLG='FULL'
      GOTO9000
C
C               ***************************************************
C               **  STEP 36--                                    **
C               **  TREAT THE MATRIX     NUMBER OF ROWS    CASE  **
C               ***************************************************
C
 3600 CONTINUE
C
      SCAL9=NR1
C
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               ***************************************************
C               **  STEP 37--                                    **
C               **  TREAT THE MATRIX     NUMBER OF COLUMNS CASE  **
C               ***************************************************
C
 3700 CONTINUE
C
      SCAL9=NC1
C
      ITYP9='SCAL'
C
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 38--                                      **
C               **  TREAT THE MATRIX     SIMPLEX SOLUTION    CASE  **
C               **  REFERENCE--PRESS ET AL, PAGE 322               **
C               *****************************************************
C
 3800 CONTINUE
C
      NC2M2=NC2-2
C
      IF(N1.EQ.NC2M2)GOTO3809
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3801)
 3801 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3802)
 3802 FORMAT('      FOR MATRIX SIMPLEX SOLUTION OF OBJ. FUNCT. F.X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3803)
 3803 FORMAT('      SUBJECT TO THE CONSTRAINTS IN MATRIX C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3804)
 3804 FORMAT('      VIA   LET V = MATRIX SIMPLEX SOLUTION F C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3805)
 3805 FORMAT('      NUMBER OF ROWS IN OBJ. FUNCTION VECTOR F MUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3806)
 3806 FORMAT('      BE EXACTLY 2 LESS THAN NUMBER OF COLUMNS IN C;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3807)N1
 3807 FORMAT('            VECTOR--',I8,' ROWS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3808)NR2,NC2
 3808 FORMAT('            MATRIX--',I8,' ROWS BY ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3809 CONTINUE
C
      IF(1.LE.N1.AND.N1.LE.MAXCOM)GOTO3819
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3811)
 3811 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3812)
 3812 FORMAT('      FOR MATRIX SIMPLEX SOLUTION OF OBJ. FUNCT. F.X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3813)
 3813 FORMAT('      SUBJECT TO THE CONSTRAINTS IN MATRIX C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3814)
 3814 FORMAT('      VIA   LET V = MATRIX SIMPLEX SOLUTION F C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3815)
 3815 FORMAT('      THE NUMBER OF ROWS IN OBJ. FUNCTION VECTOR F')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3816)MAXCOM
 3816 FORMAT('      MUST BE AT LEAST 1, AND AT MOST ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3817)N1
 3817 FORMAT('            NUMBER OF ROWS = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3819 CONTINUE
C
      IF(1.LE.NR2.AND.NR2.LE.MAXROM)GOTO3829
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3821)
 3821 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3822)
 3822 FORMAT('      FOR MATRIX SIMPLEX SOLUTION OF OBJ. FUNCT. F.X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3823)
 3823 FORMAT('      SUBJECT TO THE CONSTRAINTS IN MATRIX C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3824)
 3824 FORMAT('      VIA   LET V = MATRIX SIMPLEX SOLUTION F C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3825)
 3825 FORMAT('      THE NUMBER OF CONSTRAINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3826)
 3826 FORMAT('      (THAT IS, THE NUMBER OF ROWS IN THE MATRIX C)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3827)MAXROM
 3827 FORMAT('      MUST BE AT LEAST 1, AND AT MOST ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3828)NR2
 3828 FORMAT('            NUMBER OF CONSTRAINTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3829 CONTINUE
C
      IF(3.LE.NC2.AND.NC2.LE.MAXCOM)GOTO3839
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3831)
 3831 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3832)
 3832 FORMAT('      FOR MATRIX SIMPLEX SOLUTION OF OBJ. FUNCT. F.X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3833)
 3833 FORMAT('      SUBJECT TO THE CONSTRAINTS IN MATRIX C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3834)
 3834 FORMAT('      VIA   LET V = MATRIX SIMPLEX SOLUTION F C')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3835)
 3835 FORMAT('      THE NUMBER OF COLUMNS IN THE CONSTRAINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3836)MAXCOM
 3836 FORMAT('      MATRIX C MUST BE AT LEAST 3, AND AT MOST ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3837)NC2
 3837 FORMAT('            NUMBER OF COLUMNS = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3839 CONTINUE
C
      EPS=0.000001
      NR2P1=NR2+1
      NC2P=NC2-2
      NC2PP1=NC2P+1
      NC2M1=NC2-1
C
      DO3850J=1,NC2PP1
      YM9(1,J)=0.0
 3850 CONTINUE
C
      N1P1=N1+1
      DO3860J=2,N1P1
      JM1=J-1
      YM9(1,J)=Y1(JM1)
 3860 CONTINUE
C
      K=1
      DO3871ILOOP=1,3
C
      DO3872I=2,NR2P1
      IM1=I-1
      YTARG=YM2(IM1,NC2M1)
      IF(ILOOP.EQ.1.AND.YTARG.LT.-EPS)GOTO3873
      IF(ILOOP.EQ.1)GOTO3872
      IF(ILOOP.EQ.2.AND.EPS.LT.YTARG)GOTO3873
      IF(ILOOP.EQ.2)GOTO3872
      IF(ILOOP.EQ.3.AND.-EPS.LE.YTARG.AND.
     1                  YTARG.LE.EPS)GOTO3873
      IF(ILOOP.EQ.3)GOTO3872
 3873 CONTINUE
      K=K+1
C
      YM9(K,1)=YM2(IM1,NC2)
      DO3874J=2,NC2PP1
      JM1=J-1
      YM9(K,J)=(-YM2(IM1,JM1))
 3874 CONTINUE
C
 3872 CONTINUE
C
 3871 CONTINUE
C
      NLTZ=0
      NGTZ=0
      NEQZ=0
      DO3877I=1,NR2
      YTARG=YM2(I,NC2M1)
      IF(YTARG.LT.-EPS)NLTZ=NLTZ+1
      IF(EPS.LT.YTARG)NGTZ=NGTZ+1
      IF(-EPS.LE.YTARG.AND.YTARG.LE.EPS)NEQZ=NEQZ+1
 3877 CONTINUE
C
      CALL SIMPLX(YM9,NR2,NC2P,MAXROM,MAXCOM,NLTZ,NGTZ,NEQZ,
     1ICASE,IZROV,IPOSV,IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      DO3881I=1,N1
      VECT9(I)=0.0
 3881 CONTINUE
C
      DO3882I=1,NR2
      INDEX2=IPOSV(I)
      IP1=I+1
      IF(INDEX2.LE.N1)VECT9(INDEX2)=YM9(IP1,1)
 3882 CONTINUE
C
      ITYP9='VECT'
      NVECT9=N1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 41--                                      **
C               **  TREAT THE MATRIX     TRACE               CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
 4100 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO4109
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4101)
 4101 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4102)
 4102 FORMAT('      FOR MATRIX TRACE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4103)
 4103 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4104)
 4104 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4105)
 4105 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4106)
 4106 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4107)NR1
 4107 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4108)NC1
 4108 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4109 CONTINUE
C
      DSUM1=0.0D0
      DO4111I=1,NR1
      DYM1=YM1(I,I)
      DSUM1=DSUM1+DYM1
 4111 CONTINUE
C
      ITYP9='SCAL'
      SCAL9=DSUM1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 42--                                      **
C               **  TREAT THE MATRIX     SUBMATRIX           CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
 4200 CONTINUE
C
CCCCC  NO REASON FOR RESTRICTION ON SQUARE MATRICES FOR THIS 
CCCCC  COMMAND.  COMMENT OUT FOLLOWING SECTION.   DECEMBER 1994.
CCCCC  IF(NR1.EQ.NC1)GOTO4209
CCCCC  WRITE(ICOUT,999)
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4201)
C4201 FORMAT('***** ERROR IN MATARI--')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4202)
C4202 FORMAT('      FOR MATRIX SUBMATRIX,')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4203)
C4203 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4204)
C4204 FORMAT('      MUST EQUAL')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4205)
C4205 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4206)
C4206 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4207)NR1
C4207 FORMAT('            NUMBER OF ROWS    =',I8)
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  WRITE(ICOUT,4208)NC1
C4208 FORMAT('            NUMBER OF COLUMNS =',I8)
CCCCC  CALL DPWRST('XXX','BUG ')
CCCCC  IERROR='YES'
CCCCC  GOTO9000
C4209 CONTINUE
C
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO4211I=1,NR1
      IF(I.EQ.IYS2)GOTO4211
      I2=I2+1
      NRJ=I2
      J2=0
      DO4212J=1,NC1
      IF(J.EQ.IYS3)GOTO4212
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4212 CONTINUE
 4211 CONTINUE
C
      IF(NRJ.GE.1.AND.NCJ.GE.1)GOTO4229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4221)
 4221 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4222)
 4222 FORMAT('      FOR MATRIX SUBMATRIX,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4223)
 4223 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4224)
 4224 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4225)
 4225 FORMAT('      MUST BOTH BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4226)
 4226 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4227)NRJ
 4227 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4228)NCJ
 4228 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4229 CONTINUE
C
      DO4231I=1,NRJ
      DO4232J=1,NCJ
      YM9(I,J)=YM2(I,J)
 4232 CONTINUE
 4231 CONTINUE
C
      ITYP9='MATR'
CCCCC DECEMBER 1994.  FOLLOWING IS BACKWARDS.
CCCCC NR9=NCJ
CCCCC NC9=NRJ
      NR9=NRJ
      NC9=NCJ
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 43--                                      **
C               **  TREAT THE MATRIX     MINOR               CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
 4300 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO4309
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4301)
 4301 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4302)
 4302 FORMAT('      FOR MATRIX MINOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4303)
 4303 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4304)
 4304 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4305)
 4305 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4306)
 4306 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4307)NR1
 4307 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4308)NC1
 4308 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4309 CONTINUE
C
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO4311I=1,NR1
      IF(I.EQ.IYS2)GOTO4311
      I2=I2+1
      NRJ=I2
      J2=0
      DO4312J=1,NC1
      IF(J.EQ.IYS3)GOTO4312
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4312 CONTINUE
 4311 CONTINUE
C
      IF(NRJ.GE.1.AND.NCJ.GE.1)GOTO4329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4321)
 4321 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4322)
 4322 FORMAT('      FOR MATRIX MINOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4323)
 4323 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4324)
 4324 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4325)
 4325 FORMAT('      MUST BOTH BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4326)
 4326 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4327)NRJ
 4327 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4328)NCJ
 4328 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4329 CONTINUE
C
      IF(NRJ.EQ.NCJ)GOTO4339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4331)
 4331 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4332)
 4332 FORMAT('      FOR MATRIX MINOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4333)
 4333 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4334)
 4334 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4335)
 4335 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4336)
 4336 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4337)NRJ
 4337 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4338)NCJ
 4338 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4339 CONTINUE
C
CCCCC JULY 1993.  USE LINPACK ROUTINE TO COMPUTE THE DETERMINANT.
CCCCC CALL LUDCMP(YMJUNK,NRJ,MAXROM,INDEX,DP1M1)
C
CCCCC DPROD=DP1M1
CCCCC DO4341I=1,NRJ
CCCCC DYM9=YMJUNK(I,I)
CCCCC DPROD=DPROD*DYM9
C4341 CONTINUE
CCCCC DET=DPROD
CCCCC AMINOR=DET
C
      CALL SGECO(YM2,MAXROM,NRJ,INDEX,RCOND,Y3)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,4371)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,4372)
        CALL DPWRST('XXX','ERRO ')
        COFACT=0.0
        IERROR='YES'
      ELSE
        IJOB=10
        CALL SGEDI(YM2,MAXROM,NRJ,INDEX,Y3,Y4,IJOB)
        DET=Y3(1)*10.0**Y3(2)
        AMINOR=DET
      END IF
 4371 FORMAT('****** ERROR IN MATARI ********')
 4372 FORMAT('       UNABLE TO COMPUTE THE DETERMINANT')
CCCCC END CHANGE
C
      ITYP9='SCAL'
      SCAL9=AMINOR
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 44--                                      **
C               **  TREAT THE MATRIX     COFACTOR            CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
 4400 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO4409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4401)
 4401 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4402)
 4402 FORMAT('      FOR MATRIX COFACTOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4403)
 4403 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4404)
 4404 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4405)
 4405 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4406)
 4406 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4407)NR1
 4407 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4408)NC1
 4408 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4409 CONTINUE
C
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO4411I=1,NR1
      IF(I.EQ.IYS2)GOTO4411
      I2=I2+1
      NRJ=I2
      J2=0
      DO4412J=1,NC1
      IF(J.EQ.IYS3)GOTO4412
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4412 CONTINUE
 4411 CONTINUE
C
      IF(NRJ.GE.1.AND.NCJ.GE.1)GOTO4429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4421)
 4421 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4422)
 4422 FORMAT('      FOR MATRIX COFACTOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4423)
 4423 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4424)
 4424 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4425)
 4425 FORMAT('      MUST BOTH BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4426)
 4426 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4427)NRJ
 4427 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4428)NCJ
 4428 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4429 CONTINUE
C
      IF(NRJ.EQ.NCJ)GOTO4439
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4431)
 4431 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4432)
 4432 FORMAT('      FOR MATRIX COFACTOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4433)
 4433 FORMAT('      THE NUMBER OF ROWS IN THE SUBMATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4434)
 4434 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4435)
 4435 FORMAT('      THE NUMBER OF COLUMNS IN THE SUBMATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4436)
 4436 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4437)NRJ
 4437 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4438)NCJ
 4438 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4439 CONTINUE
C
CCCCC JULY 1993.  USE LINPACK ROUTINE TO COMPUTE THE DETERMINANT.
CCCCC CALL LUDCMP(YMJUNK,NRJ,MAXROM,INDEX,DP1M1)
C
CCCCC DPROD=DP1M1
CCCCC DO4441I=1,NRJ
CCCCC DYM9=YMJUNK(I,I)
CCCCC DPROD=DPROD*DYM9
C4441 CONTINUE
CCCCC DET=DPROD
C
      CALL SGECO(YM2,MAXROM,NRJ,INDEX,RCOND,Y3)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,4471)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,4472)
        CALL DPWRST('XXX','ERRO ')
        COFACT=0.0
        IERROR='YES'
      ELSE
        IJOB=10
        CALL SGEDI(YM2,MAXROM,NRJ,INDEX,Y3,Y4,IJOB)
        DET=Y3(1)*10.0**Y3(2)
        COFACT=DET
        IYS23=IYS2+IYS3
        IREM=IYS23-2*(IYS23/2)
        IF(IREM.EQ.1)COFACT=(-COFACT)
      END IF
 4471 FORMAT('****** ERROR IN MATARI ********')
 4472 FORMAT('       UNABLE TO COMPUTE THE DETERMINANT')
CCCCC END CHANGE
C
      ITYP9='SCAL'
      SCAL9=COFACT
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 45--                                      **
C               **  TREAT THE MATRIX     DEFINITION          CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
CCCCC OCTOBER 1993.  ADD OPTIONAL SYNTAX.  IF FOURTH PARAMETER 
CCCCC SPECIFIED, LET IT BE THE STARTING ROW NUMBER.
 4500 CONTINUE
C
      IF(ITYPA4.EQ.'PARA')GOTO4560
      DO4511I=1,NR1
      DO4512J=1,NC1
      YM9(I,J)=YM1(I,J)
 4512 CONTINUE
 4511 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      GOTO9000
CCCCC OCTOBER 1993.  ADD FOLLOWING SECTION
 4560 CONTINUE
      IROWID=INT(YS4+0.5)
      IF(IROWID.LT.1.OR.IROWID.GT.NR1)IROWID=1
      ICOUNT=0
CCCCC NLAST=IROWID+NR1-1
      NLAST=NR1
      IF(NLAST.GT.MAXROM)NLAST=MAXROM
      DO4561I=IROWID,NLAST
      ICOUNT=ICOUNT+1
      DO4562J=1,NC1
      YM9(ICOUNT,J)=YM1(I,J)
 4562 CONTINUE
 4561 CONTINUE
C
      ITYP9='MATR'
      NR9=ICOUNT
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 46--                                      **
C               **  TREAT THE MATRIX     EUCLIDEAN NORM      CASE  **
C               **  REFERENCE--RALSTON, PAGE XXX                   **
C               *****************************************************
C
 4600 CONTINUE
C
      DSUM1=0.0D0
      DO4621I=1,NR1
      DO4622J=1,NC1
      DYM1=YM1(I,J)
      DSUM1=DSUM1+DYM1*DYM1
 4622 CONTINUE
 4621 CONTINUE
      DSUM2=0.0D0
      IF(DSUM1.GT.0.0D0)DSUM2=SQRT(DSUM1)
C
      ITYP9='SCAL'
      SCAL9=DSUM2
      IUPFLG='FULL'
      GOTO9000
C
C               **************************************************************
C               **  STEP 51--                                               **
C               **  TREAT THE VARIANCE-COVARIANCE CASE                      **
C               **************************************************************
C
 5100 CONTINUE
C
CCCCC JULY 2002.  SUPPORT FOR WINSORIZED CORRELATION.
C
CCCCC NOVEMBER 2004.  SUPPORT FOR ROW BASED (AS OPPOSSED TO COLUMN
CCCCC                 BASED COVARIANCES.
C
      NTRIM1=-1
      NTRIM2=-1
      IF(ICOVDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO5151J=1,NC1
          DO5161K=1,NC1
            DO5155I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 5155       CONTINUE
            IF(ICOVTY.EQ.'RANK')THEN
              CALL RANKCV(Y3,Y4,NR1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5181I=1,NR1
                Y3(I)=Y2(I)
 5181         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5186I=1,NR1
                Y4(I)=Y2(I)
 5186         CONTINUE
              CALL COV(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSE
              CALL COV(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 5161     CONTINUE
 5151   CONTINUE
        NR9=NC1
        NC9=NC1
      ELSE
        IWRITE='OFF'
        DO5121J=1,NR1
          DO5131K=1,NR1
            DO5125I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 5125       CONTINUE
            IF(ICOVTY.EQ.'RANK')THEN
              CALL RANKCV(Y3,Y4,NC1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5141I=1,NC1
                Y3(I)=Y2(I)
 5141         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5146I=1,NC1
                Y4(I)=Y2(I)
 5146         CONTINUE
              CALL COV(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSE
              CALL COV(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 5131     CONTINUE
 5121   CONTINUE
        NR9=NC1
        NC9=NC1
      ENDIF
C
C
      ITYP9='MATR'
      IUPFLG='FULL'
      GOTO9000
C
C               ******************************************************
C               **  STEP 52--                                       **
C               **  TREAT THE CORRELATION CASE                      **
C               ******************************************************
C
 5200 CONTINUE
C
CCCCC JULY 2002.       SUPPORT FOR WINSORIZED CORRELATION, RANK CORRELATION,
CCCCC                  BIWEIGHT MID CORRELATION.
CCCCC NOVEMBER  2004.  SUPPORT FOR ROW BASED (AS OPPOSSED TO COLUMN
CCCCC                  BASED CORRELATIONS.  ALSO, ADD SUPPORT FOR
CCCCC                  KENDELL'S TAU CORRELATION.
CCCCC JUNE      2012.  SUPPORT FOR CORRELATION CDF MATRIX AND
CCCCC                              CORRELATION PVALUE MATRIX
CCCCC SEPTEMBER 2016.  SUPPORT FOR:
CCCCC                          SET CORRELATION ABSOLUTE VALUE <ON/OFF>
CCCCC                          SET CORRELATION PERCENTAGE VALUE <ON/OFF>
CCCCC                          SET CORRELATION DIGITS <VALUE>
C
      NTRIM1=-1
      NTRIM2=-1
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO5251J=1,NC1
          DO5261K=1,NC1
            DO5255I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 5255       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NR1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,
     1                    RIGHT,STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5281I=1,NR1
                Y3(I)=Y2(I)
 5281         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5286I=1,NR1
                Y4(I)=Y2(I)
 5286         CONTINUE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH1,
     1                    IBUGA3,IERROR)
              CALL BIWMDV(Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH2,
     1                    IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH3,
     1                    IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NR1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,
     1                    RIGHT,AKTAUA,AKTAUB,AKTAUC,
     1                    STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            IF(ICORAV.EQ.'ON')RIGHT=ABS(RIGHT)
            IF(ICORPV.EQ.'ON')RIGHT=100.*RIGHT
            IF(ICORDG.EQ.0)THEN
               RIGHT=INT(RIGHT+0.5)
            ELSEIF(ICORDG.GE.1 .AND. ICORDG.LE.6)THEN
              IPOWER=INT(AINT(ICORDG+0.5))
              RIGHT=REAL(INT(RIGHT*10**IPOWER + 0.5))/10**IPOWER
            ENDIF
            YM9(J,K)=RIGHT
 5261     CONTINUE
 5251   CONTINUE
        NR9=NC1
        NC9=NC1
      ELSE
        IWRITE='OFF'
        DO5221J=1,NR1
          DO5231K=1,NR1
            DO5225I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 5225       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NC1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,
     1                    RIGHT,STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5241I=1,NC1
                Y3(I)=Y2(I)
 5241         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO5246I=1,NC1
                Y4(I)=Y2(I)
 5246         CONTINUE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH1,
     1                    IBUGA3,IERROR)
              CALL BIWMDV(Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH2,
     1                    IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH3,
     1                    IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NC1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,
     1                    RIGHT,AKTAUA,AKTAUB,AKTAUC,
     1                    STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            IF(ICORAV.EQ.'ON')RIGHT=ABS(RIGHT)
            IF(ICORPV.EQ.'ON')RIGHT=100.*RIGHT
            IF(ICORDG.EQ.0)THEN
               RIGHT=INT(RIGHT+0.5)
            ELSEIF(ICORDG.GE.1 .AND. ICORDG.LE.6)THEN
              IPOWER=INT(AINT(ICORDG+0.5))
              RIGHT=REAL(INT(RIGHT*10**IPOWER + 0.5))/10**IPOWER
            ENDIF
            YM9(J,K)=RIGHT
 5231     CONTINUE
 5221   CONTINUE
        NR9=NR1
        NC9=NR1
      ENDIF
C
C       SAVE EITHER THE CDF VALUES OR THE P-VALUES.
C
      IF(IMCASE.EQ.'MACC')THEN
        IDF1=1
        IDF2=NR1 - 2
        DO5291J=1,NC9
          DO5292I=1,NR9
            IF(I.EQ.J)THEN
              YM9(I,J)=0.0
            ELSE
              ANUM=REAL(NR1 - 2)*YM9(I,J)**2
              DENOM=1.0 - YM9(I,J)**2
              CDF=0.0
              IF(DENOM.NE.0.0)THEN
                AVAL=ABS(ANUM/DENOM)
                CALL FCDF(AVAL,IDF1,IDF2,CDF)
              ENDIF
              YM9(I,J)=CDF
            ENDIF
 5292     CONTINUE
 5291   CONTINUE
      ELSEIF(IMCASE.EQ.'MACP')THEN
        IDF1=1
        IDF2=NR1 - 2
        DO5296J=1,NC9
          DO5297I=1,NR9
            IF(I.EQ.J)THEN
              YM9(I,J)=1.0
            ELSE
              ANUM=REAL(NR1 - 2)*YM9(I,J)**2
              DENOM=1.0 - YM9(I,J)**2
              CDF=0.0
              IF(DENOM.NE.0.0)THEN
                AVAL=ABS(ANUM/DENOM)
                CALL FCDF(AVAL,IDF1,IDF2,CDF)
              ENDIF
              YM9(I,J)=1.0 - CDF
            ENDIF
 5297     CONTINUE
 5296   CONTINUE
      ENDIF
C
      ITYP9='MATR'
      IUPFLG='FULL'
      GOTO9000
C
C               **************************************************************
C               **  STEP 53--                                               **
C               **  TREAT THE PRINCIPLE COMPONENTS CASE                     **
C               **  TREAT THE PRINCIPLE COMPONENTS EIGENVECTORS CASE        **
C               **  TREAT THE PRINCIPLE COMPONENTS EIGENVALUES CASE         **
C               **  REFERENCE--JACKSON, J. E. (1980, 1981)                  **
C               **             PRINCIPLE COMPONENTS AND FACTOR ANALYSIS:    **
C               **             PART 1--PRINCIPLE COMPONENTS,                **
C               **             JQT OCT 1980, PAGES 201-213.                 **
C               **             PART 2--ADDITIONAL TOPICS RELATED            **
C               **             TO PRINCIPLE COMPONENTS,                     **
C               **             JQT JAN 1981, PAGES 46-58.                   **
C               **             PART 3--WHAT IS FACTOR ANALYSIS?,            **
C               **             JQT APR 1981, PAGES 125-130.                 **
C               **  REFERENCE--LAWTON, W. H., SYLVESTRE, E. A.,             **
C               **             AND MAGGIA, M. S. (1972).                    **
C               **             SELF MODELING NONLINEAR REGRESSION.          **
C               **             TECHNOMETRICS, AUGUST, 1972,                 **
C               **             PAGES 513-532.                               **
C               **************************************************************
C
 5300 CONTINUE
C
      IF(PCCASE.EQ.'DACV')GOTO5310
      IF(PCCASE.EQ.'DACR')GOTO5310
      GOTO5329
C
 5310 CONTINUE
      DO5311J=1,NC1
      DSUM1=0.0D0
      DO5312I=1,NR1
      DYM1=YM1(I,J)
      DSUM1=DSUM1+DYM1
 5312 CONTINUE
      DMEAN(J)=D999
      DDENOM=DNR1
      IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 5311 CONTINUE
C
      DO5321J=1,NC1
      DO5322K=J,NC1
      DSUM1=0.0D0
      DO5323I=1,NR1
      DYM1=YM1(I,J)
      DYM2=YM1(I,K)
      DDEL1=DYM1-DMEAN(J)
      DDEL2=DYM2-DMEAN(K)
      DSUM1=DSUM1+DDEL1*DDEL2
 5323 CONTINUE
      DCOV=D999
      DDENOM=DNR1-1.0D0
      IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
      YM2(J,K)=DCOV
      YM2(K,J)=DCOV
 5322 CONTINUE
 5321 CONTINUE
 5329 CONTINUE
C
      IF(PCCASE.EQ.'DACV')GOTO5360
      IF(PCCASE.EQ.'DACR')GOTO5340
      IF(PCCASE.EQ.'CVCV')GOTO5330
      IF(PCCASE.EQ.'CVCR')GOTO5330
      IF(PCCASE.EQ.'CRCV')GOTO5350
      IF(PCCASE.EQ.'CRCR')GOTO5330
      GOTO5360
C
 5330 CONTINUE
      DO5331I=1,NR1
      DO5332J=1,NC1
      YM2(I,J)=YM1(I,J)
 5332 CONTINUE
 5331 CONTINUE
      IF(PCCASE.EQ.'CVCR')GOTO5340
      GOTO5360
C
 5340 CONTINUE
      DO5341I=1,NC1
      S1=YM2(I,I)
      S1=SQRT(S1)
      DO5342J=1,NC1
      S2=YM2(J,J)
      S2=SQRT(S2)
      IF(I.EQ.J)GOTO5342
      S1S2=S1*S2
      IF(S1S2.LE.0.0)YM2(I,J)=(-999.99)
      IF(S1S2.GT.0.0)YM2(I,J)=YM2(I,J)/S1S2
 5342 CONTINUE
 5341 CONTINUE
      DO5343I=1,NC1
      YM2(I,I)=1.0
 5343 CONTINUE
      GOTO5360
C
 5350 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5351)
 5351 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5352)
 5352 FORMAT('       ILLEGAL PRINCIPLE COMPONENTS TYPE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5353)
 5353 FORMAT('      YOU CANNOT SPECIFY THAT THE STARTING MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5354)
 5354 FORMAT('      IS THE CORRELATION MATRIX,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5355)
 5355 FORMAT('      AND THEN SPECIFY THAT THE INTERMEDIATE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5356)
 5356 FORMAT('      IS THE COVARIANCE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5357)
 5357 FORMAT('      (SINCE LATTER CANNOT BE DERIVED FROM FORMER).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5358)
 5358 FORMAT('      FIX BY USING THE    PRINCIPLE COMPONENTS TYPE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5359)
 5359 FORMAT('      COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 5360 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TARI')GOTO5361
      GOTO5369
 5361 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5362)
 5362 FORMAT('***** FROM THE MIDDLE OF MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5363)NC1,MAXCOM
 5363 FORMAT('NC1,MAXCOM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO5364I=1,NC1
      DO5365J=1,NC1
      WRITE(ICOUT,5366)I,J,YM2(I,J)
 5366 FORMAT('I,J,YM2(I,J) = ',2I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 5365 CONTINUE
 5364 CONTINUE
 5369 CONTINUE
C
CCCCC JULY 1993.  USE EISPACK ROUTINES (NOTE THAT CORRELATION OR 
CCCCC COVARIANCE MATRIX IS SYMMETRIC).
CCCCC ALSO, SINCE MAXROM AND MAXCOM NO LONGER EQUAL, BE SURE TO SEND
CCCCC MAXROM AS MATRIX LEADING DIMENSION.
CCCCC CALL JACOBI(YMJUNK,NC1,MAXCOM,VJUNK,YM9,NJACIT)
C
      IERR2=0
      IJOB=1
      DO5650J=1,NC1
      DO5651I=1,NC1
      YM9(I,J)=YM2(I,J)
 5651 CONTINUE
 5650 CONTINUE
      CALL SSIEV(YM9,MAXROM,NC1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.NE.0)THEN
        IERROR='YES'
        WRITE(ICOUT,5661)
        WRITE(ICOUT,5662)
        WRITE(ICOUT,5663)
        GOTO9000
      END IF
 5661 FORMAT('******** ERROR FROM MATARI ************')
 5662 FORMAT('         UNABLE TO CALCULATE EIGENVALUES CORRECTLY.')
 5663 FORMAT('         PRINCIPLE COMPONENTS WERE NOT COMPUTED.')
CCCCC END CHANGE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TARI')GOTO5371
      GOTO5379
 5371 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5372)
 5372 FORMAT('***** FROM THE MIDDLE OF MATARI--')
      CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,5373)NC1,MAXCOM,NJACIT
C5373 FORMAT('NC1,MAXCOM,NJACIT = ',3I8)
      WRITE(ICOUT,5373)NC1,MAXCOM
 5373 FORMAT('NC1,MAXCOM= ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO5374I=1,NC1
      DO5375J=1,NC1
      WRITE(ICOUT,5376)I,J,YM9(I,J),Y3(I)
 5376 FORMAT('I,J,YM9(I,J),Y3(I) = ',2I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 5375 CONTINUE
 5374 CONTINUE
 5379 CONTINUE
C
      DO5380I=1,NC1
CCCCC AINDE2(I)=I
      Y1(I)=I
 5380 CONTINUE
C
CCCCC CALL SORTC(Y3,AINDE2,NC1,Y4,AINDE3)
      CALL SORTC(Y3,Y1,NC1,Y4,Y2)
C
      DO5390J=1,NC1
      JREV=NC1-J+1
CCCCC INDEX3=AINDE3(JREV)+0.5
      INDEX3=INT(Y2(JREV)+0.5)
      VECT9(J)=Y3(INDEX3)
 5390 CONTINUE
C
      DO5411J=1,NC1
      JREV=NC1-J+1
CCCCC INDEX3=AINDE3(JREV)+0.5
      INDEX3=INT(Y2(JREV)+0.5)
      DO5412I=1,NC1
      YM2(I,J)=YM9(I,INDEX3)
 5412 CONTINUE
 5411 CONTINUE
C
      DO5416I=1,NC1
      DO5417J=1,NC1
      YM9(I,J)=YM2(I,J)
 5417 CONTINUE
 5416 CONTINUE
C
      IF(IMCASE.EQ.'MAPC')GOTO5430
      GOTO5500
C
 5430 CONTINUE
      IF(IMSUBC.EQ.'EVEC')GOTO5440
      IF(IMSUBC.EQ.'EVAL')GOTO5450
      GOTO5460
C
 5440 CONTINUE
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 5450 CONTINUE
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 5460 CONTINUE
      DO5461I=1,NR1
      DO5462J=1,NC1
      DSUM=0.0D0
      DO5463K=1,NC1
      DYM1=YM1(I,K)
      DDEL=DYM1-DMEAN(K)
      DYM2=YM9(K,J)
      DYM9=DDEL*DYM2
      DSUM=DSUM+DYM9
 5463 CONTINUE
      YM2(I,J)=DSUM
 5462 CONTINUE
 5461 CONTINUE
      DO5465I=1,NR1
      DO5466J=1,NC1
      YM9(I,J)=YM2(I,J)
 5466 CONTINUE
 5465 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 5500 CONTINUE
      L=1
      IF(IMCASE.EQ.'MAP2')L=2
      IF(IMCASE.EQ.'MAP3')L=3
      IF(IMCASE.EQ.'MAP4')L=4
      IF(IMCASE.EQ.'MAP5')L=5
      IF(IMCASE.EQ.'MAP6')L=6
      IF(IMCASE.EQ.'MAP7')L=7
      IF(IMCASE.EQ.'MAP8')L=8
      IF(IMCASE.EQ.'MAP9')L=9
      IF(IMCASE.EQ.'MA10')L=10
C
      IF(IMSUBC.EQ.'EVEC')GOTO5530
      IF(IMSUBC.EQ.'EVAL')GOTO5540
      GOTO5550
C
 5530 CONTINUE
      DO5531I=1,NC1
      VECT9(I)=YM9(I,L)
 5531 CONTINUE
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 5540 CONTINUE
      ITYP9='SCAL'
      SCAL9=VECT9(L)
      IUPFLG='FULL'
      GOTO9000
C
 5550 CONTINUE
      DO5551I=1,NR1
      DSUM=0.0D0
      DO5553K=1,NC1
      DYM1=YM1(I,K)
      DDEL=DYM1-DMEAN(K)
      DYM2=YM9(K,L)
      DYM9=DDEL*DYM2
      DSUM=DSUM+DYM9
 5553 CONTINUE
      VECT9(I)=DSUM
 5551 CONTINUE
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               **************************************************
C               **  STEP 54--                                   **
C               **  TREAT THE MATRIX TRUNCATION      CASE       **
C               **  THIS COMMAND SETS ANY VALUE BELOW THE       **
C               **  TRUNCATION VALUE TO THAT TRUNCATION         **
C               **  VALUE.  A COMMON USE OF THIS COMMAND        **
C               **  MIGHT BE TO REMOVE BACKGROUND (USE          **
C               **  MATRIX SUBTRACTION TO REMOVE THE            **
C               **  BACKGROUND AND THEN USE MATRIX TRUNCATION   **
C               **  TO SET ANY RESULTING NEGATIVE VALUES (I.E., **
C               **  POINTS BELOW THE BACKGROUND LEVEL) TO ZERO. **
C               **************************************************
C
 6100 CONTINUE
C
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GOTO6170
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GOTO6180
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6101)
 6101 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6102)
 6102 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX TRUNCATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6103)ITYPA1
 6103 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6104)ITYPA2
 6104 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 6170 CONTINUE
      DO6171I=1,NR1
      DO6172J=1,NC1
        YM9(I,J)=MAX(YM1(I,J),YS2)
 6172 CONTINUE
 6171 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 6180 CONTINUE
      DO6181I=1,NR1
      DO6182J=1,NC1
        YM9(I,J)=MAX(YM2(I,J),YS1)
 6182 CONTINUE
 6181 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
C               **************************************************
C               **  STEP 55--                                   **
C               **  TREAT THE MATRIX UPPER TRUNCATION   CASE    **
C               **  THIS COMMAND SETS ANY VALUE ABOVE THE       **
C               **  TRUNCATION VALUE TO THAT TRUNCATION         **
C               **  VALUE.                                      **
C               **************************************************
C
 6200 CONTINUE
C
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GOTO6270
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GOTO6280
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6201)
 6201 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6202)
 6202 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX UPPER ',
     1       'TRUNCATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6203)ITYPA1
 6203 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6204)ITYPA2
 6204 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 6270 CONTINUE
      DO6271I=1,NR1
      DO6272J=1,NC1
        YM9(I,J)=MIN(YM1(I,J),YS2)
 6272 CONTINUE
 6271 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
 6280 CONTINUE
      DO6281I=1,NR1
      DO6282J=1,NC1
        YM9(I,J)=MIN(YM2(I,J),YS1)
 6282 CONTINUE
 6281 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
C               ******************************************************
C               **  STEP 63--                                       **
C               **  TREAT THE COMOVEMENT CASE                       **
C               ******************************************************
C
 6300 CONTINUE
C
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO6351J=1,NC1
          DO6361K=1,NC1
            DO6355I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 6355       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCM(Y3,Y4,NR1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSE
              CALL COMOVE(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6361     CONTINUE
 6351   CONTINUE
        NR9=NC1
        NC9=NC1
      ELSE
        IWRITE='OFF'
        DO6321J=1,NR1
          DO6331K=1,NR1
            DO6325I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 6325       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCM(Y3,Y4,NC1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,
     1                    IBUGA3,IERROR)
            ELSE
              CALL COMOVE(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6331     CONTINUE
 6321   CONTINUE
        NR9=NR1
        NC9=NR1
      ENDIF
C
      ITYP9='MATR'
      IUPFLG='FULL'
      GOTO9000
C
C               ******************************************************
C               **  STEP 64--                                       **
C               **  TREAT THE PARTIAL CORRELATION CASE              **
C               ******************************************************
C
 6400 CONTINUE
C
CCCCC COMPUTE THE PARTIAL CORRELATION MATRIX.  AS WITH THE REGULAR
CCCCC CORRELATION MATRIX, SUPPORT FOR:
CCCCC
CCCCC   1. EITHER COLUMN (DEFAULT) OR ROW BASED CORRELATIONS
CCCCC   2. SUPPORT FOR PEARSON CORRELATION, WINSORIZED CORRELATION,
CCCCC      BIWEIGHT MID-CORRELATION, RANK CORRELATION, OR KENDALL TAU
CCCCC      CORRELATION.
CCCCC
CCCCC ALGORITHM IS:
CCCCC
CCCCC   1. COMPUTE THE STANDARD CORRELATION MATRIX
CCCCC   2. INVERT THE CORELATION MATRIX
CCCCC   3. R(ij.) = -R(ij)/SQRT(R(ii)*R(jj))
CCCCC      WHERE R(IJ) IS THE IJ-TH ELEMENT OF THE INVERTED CORRELATION
CCCCC      MATRIX.
CCCCC
CCCCC   AUTOMATICALLY SET THE DIAGONAL ELEMENTS TO +1.
C
C       NUMBER OF ROWS (N) MUST BE GREATER THAN NUMBER OF COLUMNS
C
      IF(ICORDI.EQ.'COLU')THEN
        IF(NR1-NC1.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6423)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6491)
 6491     FORMAT('      THE NUMBER OF ROWS IN THE MATRIX MUST BE ',
     1           'GREATER THAN THE NUMBER OF COLUMNS.')
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6493)NR1
 6493     FORMAT('      THE NUMBER OF ROWS     = ',I8)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6495)NC1
 6495     FORMAT('      THE NUMBER OF COLUMNS  = ',I8)
          CALL DPWRST('XXX','ERRO')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        IF(NC1-NR1.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6423)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6492)
 6492     FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX MUST BE ',
     1           'GREATER THAN THE NUMBER OF ROWS.')
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6493)NR1
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6495)NC1
          CALL DPWRST('XXX','ERRO')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      NTRIM1=-1
      NTRIM2=-1
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO6401J=1,NC1
          DO6402K=1,NC1
            DO6403I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 6403       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NR1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,
     1                    RIGHT,STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO6406I=1,NR1
                Y3(I)=Y2(I)
 6406         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO6407I=1,NR1
                Y4(I)=Y2(I)
 6407         CONTINUE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH1,
     1                    IBUGA3,IERROR)
              CALL BIWMDV(Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH2,
     1                    IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH3,
     1                    IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NR1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,
     1                    RIGHT,AKTAUA,AKTAUB,AKTAUC,
     1                    STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6402     CONTINUE
 6401   CONTINUE
        NR9=NC1
        NC9=NC1
      ELSE
        IWRITE='OFF'
        DO6411J=1,NR1
          DO6412K=1,NR1
            DO6413I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 6413       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NC1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,
     1                    RIGHT,STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO6414I=1,NC1
                Y3(I)=Y2(I)
 6414         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,
     1                    Y1,MAXOBV,Y2,
     1                    IBUGA3,ISUBRO,IERROR)
              DO6415I=1,NC1
                Y4(I)=Y2(I)
 6415         CONTINUE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,
     1                    IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH1,
     1                    IBUGA3,IERROR)
              CALL BIWMDV(Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH2,
     1                    IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH3,
     1                    IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NC1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,
     1                    RIGHT,AKTAUA,AKTAUB,AKTAUC,
     1                    STATCD,PVAL,PVALLT,PVALUT,
     1                    CUTU90,CUTU95,CTU975,CUTU99,CTU995,
     1                    CUTL90,CUTL95,CTL975,CUTL99,CTL995,
     1                    IBUGA3,ISUBRO,IERROR)
            ELSE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6412     CONTINUE
 6411   CONTINUE
        NR9=NR1
        NC9=NR1
      ENDIF
C
C     NOW INVERT THE CORRELATION MATRIX
C
      CALL SGECO(YM9,MAXROM,NR9,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6421)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
 6421 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE CORRELATION ',
     1       'MATRIX = ',G15.7)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6423)
 6423   FORMAT('***** ERROR IN PARTIAL CORRELATION MATRIX--')
        CALL DPWRST('XXX','ERRO')
        WRITE(ICOUT,6425)
 6425   FORMAT('       THE CORRELATION MATRIX IS SINGULAR.')
        CALL DPWRST('XXX','ERRO')
        IERROR='YES'
      ELSE
        IJOB=1
        CALL SGEDI(YM9,MAXROM,NR9,INDEX,Y3,Y4,IJOB)
C
        DO6431J=1,NC9
          DO6432I=1,NR9
            YM1(I,J)=PSTAMV
            IF(I.EQ.J)THEN
              YM1(I,J)=1.0
            ELSE
              DENOM=YM9(I,I)*YM9(J,J)
              IF(DENOM.GT.0.0)YM1(I,J)=-YM9(I,J)/SQRT(DENOM)
            ENDIF
 6432     CONTINUE
 6431   CONTINUE
C
C       SAVE EITHER THE PARTIAL CORRELATION MATRIX, THE CDF
C       VALUES, OR THE P-VALUES.
C
        IF(IMCASE.EQ.'MPCO')THEN
          DO6441J=1,NC9
            DO6442I=1,NR9
              YM9(I,J)=YM1(I,J)
 6442       CONTINUE
 6441     CONTINUE
        ELSEIF(IMCASE.EQ.'MPCC')THEN
          IF(ICORDI.EQ.'COLU')THEN
            IDF1=1
            IDF2=NR1 - NC9
          ELSE
            IDF1=1
            IDF2=NC1 - NR9
          ENDIF
          DO6451J=1,NC9
            DO6452I=1,NR9
              YM9(I,J)=YM1(I,J)
              IF(I.EQ.J)THEN
                YM9(I,J)=0.0
              ELSE
                ANUM=REAL(NR1 - NC9)*YM1(I,J)**2
                DENOM=1.0 - YM1(I,J)**2
                CDF=0.0
                IF(DENOM.NE.0.0)THEN
                  AVAL=ABS(ANUM/DENOM)
                  CALL FCDF(AVAL,IDF1,IDF2,CDF)
                ENDIF
                YM9(I,J)=CDF
              ENDIF
 6452       CONTINUE
 6451     CONTINUE
        ELSEIF(IMCASE.EQ.'MPCP')THEN
          IF(ICORDI.EQ.'COLU')THEN
            IDF1=1
            IDF2=NR1 - NC9
          ELSE
            IDF1=1
            IDF2=NC1 - NR9
          ENDIF
          DO6461J=1,NC9
            DO6462I=1,NR9
              YM9(I,J)=YM1(I,J)
              IF(I.EQ.J)THEN
                YM9(I,J)=1.0
              ELSE
                ANUM=REAL(NR1 - NC9)*YM1(I,J)**2
                DENOM=1.0 - YM1(I,J)**2
                CDF=0.0
                IF(DENOM.NE.0.0)THEN
                  AVAL=ABS(ANUM/DENOM)
                  CALL FCDF(AVAL,IDF1,IDF2,CDF)
                ENDIF
                YM9(I,J)=1.0 - CDF
              ENDIF
 6462       CONTINUE
 6461     CONTINUE
        ENDIF
C
      END IF
C
      ITYP9='MATR'
      IUPFLG='FULL'
      GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GOTO9090
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IMCASE,IMSUBC
 9013 FORMAT('IMCASE,IMSUBC = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMVAR,IWRITE
 9014 FORMAT('NUMVAR,IWRITE = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)YS1,YS2,YS3,YS4
 9015 FORMAT('YS1,YS2,YS3,YS4 = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IERROR
 9016 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IYS2,IYS3,IYS23,NRJ,NCJ
 9017 FORMAT('IYS2,IYS3,IYS23,NRJ,NCJ = ',5I8)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GOTO9039
      IF(NC1.LE.0)GOTO9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO9032I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9049
      IF(NC2.LE.0)GOTO9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO9042I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9059
      IF(NC9.LE.0)GOTO9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9055I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GOTO9119
      DO9112I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GOTO9129
      DO9122I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GOTO9139
      DO9132I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GOTO9149
      DO9142I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GOTO9169
      DO9162I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9179
      IF(NC9.LE.0)GOTO9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9172I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
C
      IF(IMCASE.NE.'MASS')GOTO9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9189
      IF(NC2.LE.0)GOTO9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO9182I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
      DO9185I=1,N3
      WRITE(ICOUT,9186)ICASE,I,IZROV(I),IPOSV(I)
 9186 FORMAT('ICASE,I,IZROV(I),IPOSV(I) = ',4I8)
      CALL DPWRST('XXX','BUG ')
 9185 CONTINUE
      WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
 9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
C
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE MATAR2(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,MAXROM,MAXCOM,
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC SUBROUTINE MATAR2(YM1,NR1,NC1,YM2,NR2,NC2,YM3,NR3,NC3,
     1Y1,N1,Y2,N2,Y3,N3,Y4,N4,
     1INDEX,
     1YS1,YS2,YS3,YS4,
     1IMCASE,IUPFLG,IMSUBC,ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,
     1IBPLSC,PBPLCO,
     1YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC1YMJUNK,YMJUN2,
     1IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
C              OF THE REAL DATA IN MATRICES YM1 AND YM2.
C
C     OPERATIONS--ADDITION
C                 SUBTRACTION
C                 MULTIPLICATION
C                 SOLUTION
C                 ITERATIVE SOLUTION
C                 INVERSE
C                 TRANSPOSE
C                 ADJOINT         
C                 CHARACTERISTIC EQUATION      (NOT YET IMPLEMENED)
C                 EIGENVALUES
C                 EIGENVECTORS
C                 RANK
C                 DETERMINANT
C                 PERMANENT
C                 SPECTRAL NORM 
C                 SPECTRAL RADIUS
C                 NUMBER OF ROWS
C                 NUMBER OF COLUMNS
C                 SIMPLEX SOLUTION
C                 TRACE       
C                 SUBMATRIX  
C                 MINOR     
C                 COFACTOR 
C                 DEFINITION  
C                 EUCLIDEAN NORM 
C                 SINGULAR VALUE
C                 SINGULAR VALUE DECOMPOSITION
C                 SINGULAR VALUE FACTORIZATION
C                 ROW
C                 ELEMENT
C                 REPLACE ROW
C                 REPLACE ELEMENT
C                 AUGMENT
C                 DIAGONAL
C                 CHOLESKY DECOMPOSITION
C                 TRIDIAGONAL SOLVE
C                 TRIANGULAR SOLVE
C                 TRIANGULAR INVERSE
C
C                 VARIANCE-COVARIANCE MATRIX
C                 CORRELATION MATRIX
C                 PRINCIPLE COMPONENTS ...
C                 ... PRINCIPLE COMPONENT ...
C                 BIPLOT
C
C     EXAMPLES--LET M3 = MATRIX ADDITION M1 M2
C               LET M3 = MATRIX ADDITION M1 P1
C             --LET M3 = MATRIX SUBTRACTION M1 M2
C               LET M3 = MATRIX SUBTRACTION M1 P1
C             --LET M3 = MATRIX MULTIPLICATION M1 M2
C               LET M3 = MATRIX MULTIPLICATION M1 V1
C               LET M3 = MATRIX MULTIPLICATION M1 P1
C             --LET V3 = MATRIX SOLUTION M1 V2
C             --LET M3 = MATRIX INVERSE M1
C             --LET A  = MATRIX CONDITION NUMBER M1
C             --LET M3 = MATRIX TRANSPOSE M1
C             --LET M3 = MATRIX ADJOINT M1
C             --LET V3 = MATRIX CHARACTERISTIC EQUATION M1
C             --LET V3 = MATRIX EIGENVALUES M1
C             --LET P3 = MATRIX EIGENVECTORS M1
C             --LET P3 = MATRIX RANK M1
C             --LET P3 = MATRIX DETERMINANT M1
C             --LET P3 = MATRIX PERMANENT M1
C             --LET P3 = MATRIX SPECTRAL NORM M1
C             --LET P3 = MATRIX SPECTRAL RADIUS M1
C             --LET P3 = MATRIX NUMBER OF ROWS M1
C             --LET P3 = MATRIX NUMBER OF COLUMNS M1
C             --LET V4 = MATRIX SIMPLEX SOLUTION V1 M1 V2 V3
C             --LET P3 = MATRIX TRACE M1
C             --LET M3 = MATRIX SUBMATRIX M1 P1 P2
C             --LET P3 = MATRIX MINOR M1 P1 P2
C             --LET P3 = MATRIX COFACTOR M1 P1 P2
C             --LET M3 = MATRIX DEFINITION V1 P1 P2
C             --LET P3 = MATRIX EUCLIDEAN NORM M1
C             --LET V3 = MATRIX ROW M1 P1
C             --LET P3 = MATRIX ELEMENT M1 P1 P2
C             --LET M3 = MATRIX REPLACE ROW M1 V1 P1
C             --LET M3 = MATRIX REPLACE ELEMENT M1 P1 P2
C             --LET M3 = MATRIX AUGMENT M1
C             --LET V3 = MATRIX DIAGONAL M1
C             --LET M3 = DIAGONAL MATRIX V1
C             --LET M3 = VARIANCE-COVARIANCE MATRIX M1
C             --LET M3 = CORRELATION MATRIX M1
C             --LET M3 = PRINCIPLE COMPONENTS M1
C             --LET M3 = PRINCIPLE COMPONENTS EIGENVECTORS M1
C             --LET V3 = PRINCIPLE COMPONENTS EIGENVALUES M1
C             --LET V3 = ... PRINCIPLE COMPONENT M1
C             --LET V3 = ... PRINCIPLE COMPONENT EIGENVECTOR M1
C             --LET P3 = ... PRINCIPLE COMPONENT EIGENVALUE M1
C             --LET V3 = MATRIX SINGULAR VALUES M1
C             --LET M3 V3 M2 = MATRIX SINGULAR VALUE DECOMP M1
C             --LET M3 V3 M2 = MATRIX SINGULAR VALUE FACTOR M1
C             --LET M3 = CHOLESKY DECOMP M1
C             --LET V4 = TRIDIAGONAL SOLVE V1 V2 V3
C
C     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
C                     --NR1
C                     --NC1
C                     --YM2 (REAL MATRIX)
C                     --NR2
C                     --NC2
C                     --YM3 (REAL MATRIX)
C                     --NR3
C                     --NC3
C                     --Y1  (REAL VECTOR)
C                     --N1
C                     --Y2  (REAL VECTOR)
C                     --N2
C                     --Y3  (REAL VECTOR)
C                     --N3
C                     --Y4  (REAL VECTOR)
C                     --N4
C     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
C                     --NR9
C                     --NC9
C                     --VECT9 (REAL VECTOR)
C                     --NVECT9
C                     --SCAL9 (REAL SCALAR)
C                     --ITYP9
C
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
C           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/10
C     ORIGINAL VERSION--SEPTEMBER 1987.
C     UPDATED         --AUGUST    1988  (VARIANCE-COVARIANCE MATRIX)
C     UPDATED         --AUGUST    1988  (CORRELATION MATRIX)
C     UPDATED         --AUGUST    1988  (PRINCIPLE COMPONENTS)
C     UPDATED         --AUGUST    1988  (... PRINCIPLE COMPONENTS)
C     UPDATED         --APRIL     1992  DEFINE D999
C     UPDATED         --JULY      1993  FOR MATRIX SOLUTION,
C                                       DETERMINANT, INVERSE, REPLACE
C                                       NUMERICAL RECIPES CODE WITH
C                                       LINPACK CODE
C     UPDATED         --JULY      1993  EIGENVALUES AND EIGENVECTORS
C                                       EXTENDED TO NON-SYMMETRIC CASE
C     UPDATED         --JULY      1993  IMPLEMENT RANK, ADJOINT,
C                                       SINGULAR VALUES, SINGULAR VALUE
C                                       DECOMP.
C     UPDATED         --SEPT      1993  ROW, ELEMENT CASES
C     UPDATED         --OCT       1993  CHOLESKY DECOMPOSITION, REPLACE
C                                       ROW, REPLACE ELEMENT, AUGMENT,
C                                       DIAGONAL, ADD ARGUMENT TO
C                                       MATRIX DEFINITION, TRIDIAGONAL
C                                       SOLVE.
C     UPDATED         --JANUARY   1998 RECODE TO MINIMIZE NUMBER OF
C                                      MATRICES NEEDED.
C     UPDATED         --APRIL     2009 ADD BIPLOT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IMCASE
      CHARACTER*4 IUPFLG
      CHARACTER*4 IMSUBC
      CHARACTER*4 ITYPA1
      CHARACTER*4 ITYPA2
      CHARACTER*4 ITYPA3
      CHARACTER*4 ITYPA4
      CHARACTER*4 IWRITE
      CHARACTER*4 ITYP9
      CHARACTER*4 IBPLSC
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DNR1
      DOUBLE PRECISION DNC1
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      DOUBLE PRECISION D999
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION YM1(MAXROM,MAXCOM)
      DIMENSION YM2(MAXROM,MAXCOM)
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC DIMENSION YM3(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION YM9(MAXROM,MAXCOM)
CCCCC DIMENSION VECT9(MAXROM)
      DIMENSION VECT9(*)
C
CCCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
CCCCC DIMENSION YMJUNK(MAXROM,MAXCOM)
CCCCC DIMENSION YMJUN2(MAXROM,MAXCOM)
CCCCC DIMENSION INDEX(MAXROM)
CCCCC DIMENSION VJUNK(MAXROM)
CCCCC DIMENSION VJUNK2(MAXROM)
      DIMENSION INDEX(*)
CCCCC DIMENSION VJUNK(MAXOBV)
CCCCC DIMENSION VJUNK2(MAXOBV)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='MATA'
      ISUBN2='R2  '
      IERROR='NO'
C
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
C
      NRJ=(-999)
      NCJ=(-999)
      D999=(-999.0D0)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
C
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATAR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52   FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE
   53   FORMAT('IMCASE,IMSUBC,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)YS1,YS2,YS3,YS4
   55   FORMAT('YS1,YS2,YS3,YS4 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)NR1,NC1,NR2,NC2,NR3,NC3
   61   FORMAT('NR1,NC1,NR2,NC2,NR3,NC3 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR1.GE.1 .AND. NC1.GE.1)THEN
          JMAX=NC1
          IF(JMAX.GT.10)JMAX=10
          DO62I=1,NR1
            WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63       FORMAT('I,YM1(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NR2.GE.1 .AND. NC2.GE.1)THEN
          JMAX=NC2
          IF(JMAX.GT.10)JMAX=10
          DO72I=1,NR2
            WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73       FORMAT('I,YM2(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   72     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NR3.GE.1 .AND. NC3.GE.1)THEN
          JMAX=NC3
          IF(JMAX.GT.10)JMAX=10
          DO82I=1,NR3
            WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83       FORMAT('I,YM9(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)N1,N2,N3,N4
  111   FORMAT('N1,N2,N3,N4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GE.1)THEN
          DO112I=1,N1
            WRITE(ICOUT,113)I,Y1(I)
  113       FORMAT('I,Y1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  112     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(N2.GE.1)THEN
          DO122I=1,N2
            WRITE(ICOUT,123)I,Y2(I)
  123       FORMAT('I,Y2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  122     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(N3.GE.1)THEN
          DO132I=1,N3
            WRITE(ICOUT,133)I,Y3(I)
  133       FORMAT('I,Y3(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  132     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(N4.GE.1)THEN
          DO142I=1,N4
            WRITE(ICOUT,143)I,Y4(I)
  143       FORMAT('I,Y4(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  142     CONTINUE
        ENDIF
C
      ENDIF
C
C               **************************************************
C               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
C               **************************************************
C
      DNR1=NR1
      DNC1=NC1
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
C               ********************************************
C
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GOTO1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GOTO1100
C
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GOTO1100
C
      GOTO1190
C
 1100 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('      THE INPUT NUMBER OF ROWS AND/OR COLUMNS IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('      MATRIX AND/OR VECTOR FOR WHICH THE MATRIX ',
     1       'OPERATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('      IS TO BE COMPUTED MUST BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
C
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1)THEN
        WRITE(ICOUT,1183)NR1,NC1
 1183   FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2)THEN
        WRITE(ICOUT,1184)NR2,NC2
 1184   FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3)THEN
        WRITE(ICOUT,1185)NR3,NC3
 1185   FORMAT('            MATRIX 3--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1)THEN
        WRITE(ICOUT,1186)N1
 1186   FORMAT('            VECTOR 1--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2)THEN
        WRITE(ICOUT,1187)N2
 1187   FORMAT('            VECTOR 2--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3)THEN
        WRITE(ICOUT,1188)N3
 1188   FORMAT('            VECTOR 3--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 1190 CONTINUE
C
C               *********************************
C               **  STEP 12--                  **
C               **  BRANCH TO THE PROPER CASE  **
C               *********************************
C
CCCCC JULY 1993.  ADD FOLLOWING 3 LINES
      IF(IMCASE.EQ.'MASV')GOTO5800
      IF(IMCASE.EQ.'MASD')GOTO5900
      IF(IMCASE.EQ.'MASF')GOTO6000
CCCCC SEPTEMBER 1993.  ADD FOLLOWING 2 LINES
      IF(IMCASE.EQ.'MARW')GOTO6100
      IF(IMCASE.EQ.'MAEL')GOTO6200
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      IF(IMCASE.EQ.'MACH')GOTO6300
      IF(IMCASE.EQ.'MAAU')GOTO6400
      IF(IMCASE.EQ.'MADI')GOTO6500
      IF(IMCASE.EQ.'DIMA')GOTO6600
      IF(IMCASE.EQ.'MARR')GOTO6700
      IF(IMCASE.EQ.'MARE')GOTO6800
      IF(IMCASE.EQ.'MATD')GOTO6900
      IF(IMCASE.EQ.'MATS')GOTO7000
      IF(IMCASE.EQ.'MATI')GOTO7100
      IF(IMCASE.EQ.'MAIS')GOTO7200
      IF(IMCASE.EQ.'BIPL')GOTO7300
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** INTERNAL ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      IMCASE NOT EQUAL TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      MASV, MASD, MASF, MARW, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      MAEL, MACH, MAAU, MADI, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      DIMA, MARR, MARE, MATD, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      MATS, MATI, MAIS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1228)IMCASE
 1228 FORMAT('      IMCASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               ************************************************
C               **  STEP 58--                                 **
C               **  TREAT THE MATRIX SINGULAR VALUES CASE     **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 5800 CONTINUE
C
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y1,YM1,MAXROM,
     1YM1,MAXROM,Y2,IJOB,IERR2)
C
      ITYP9='VECT'
      NVECT9=MIN(NR1,NC1)
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 59--                                 **
C               **  TREAT THE MATRIX SINGULAR VALUES          **
C               **  DECOMPOSITION CASE                        **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 5900 CONTINUE
C
      IF(NR1.LE.MAXCOM)GOTO5909
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5901)
 5901 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5902)
 5902 FORMAT('      FOR MATRIX SINGULAR VALUE DECOMPOSITION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5903)
 5903 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5904)
 5904 FORMAT('      CAN NOT EXCEED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5905)
 5905 FORMAT('      THE MAXIMUM NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5906)
 5906 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5907)NR1
 5907 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5908)MAXCOM
 5908 FORMAT('            MAXIMUM NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 5909 CONTINUE
      DO5922J=1,MAXCOM
      DO5921I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 5921 CONTINUE
 5922 CONTINUE
C
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,
     1YM2,MAXROM,Y2,IJOB,IERR2)
C
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NR1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 60--                                 **
C               **  TREAT THE MATRIX SINGULAR VALUES          **
C               **  FACTORIZATION CASE                        **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 6000 CONTINUE
C
      DO6022J=1,MAXCOM
      DO6021I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 6021 CONTINUE
 6022 CONTINUE
C
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,
     1YM2,MAXROM,Y2,IJOB,IERR2)
C
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NC1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 61--                                      **
C               **  TREAT THE MATRIX     ROW                 CASE  **
C               *****************************************************
C
 6100 CONTINUE
      IROWID=INT(YS2+0.5)
      IF(IROWID.LT.1 .OR. IROWID.GT.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6102)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6103)
        WRITE(ICOUT,6104)NR1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6105)IROWID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6101 FORMAT('***** ERROR IN MATAR2--')
 6102 FORMAT('      FOR MATRIX ROW,')
 6103 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6104 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6105 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
C
      DO6120J=1,NC1
      VECT9(J)=YM1(IROWID,J)
 6120 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 62--                                      **
C               **  TREAT THE MATRIX     ELEMENT             CASE  **
C               *****************************************************
C
 6200 CONTINUE
      IROWID=INT(YS2+0.5)
      ICOLID=INT(YS3+0.5)
      IF(IROWID.LT.1 .OR. IROWID.GT.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6202)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6203)
        WRITE(ICOUT,6204)NR1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6205)IROWID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6201 FORMAT('***** ERROR IN MATAR2--')
 6202 FORMAT('      FOR MATRIX ELEMENT,')
 6203 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6204 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6205 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
C
      IF(ICOLID.LT.1 .OR. ICOLID.GT.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6212)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6213)
        WRITE(ICOUT,6214)NC1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6215)ICOLID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6211 FORMAT('***** ERROR IN MATAR2--')
 6212 FORMAT('      FOR MATRIX ELEMENT,')
 6213 FORMAT('      THE REQUESTED COLUMN IN THE MATRIX MUST BE')
 6214 FORMAT('      BETWEEN 1 AND ',I8,'.  SUCH WAS NOT THE CASE')
 6215 FORMAT('      HERE.  THE REQUESTED COLUMN NUMBER = ',I8)
C
      ITYP9='SCAL'
      SCAL9=YM1(IROWID,ICOLID)
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 63--                              **
C               **  TREAT THE MATRIX CHOLESKY DECOMP CASE  **
C               **  REFERENCE--LINPACK USER'S GUIDE        **
C               *********************************************
C
 6300 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO6309
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6301)
 6301 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6302)
 6302 FORMAT('      FOR MATRIX CHOLESKY DECOMPOSITION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6303)
 6303 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6304)
 6304 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6305)
 6305 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6306)
 6306 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6307)NR1
 6307 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6308)NC1
 6308 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6309 CONTINUE
C
      CALL SPOCO(YM1,MAXROM,NR1,RCOND,Y1,INFO)
C
      IF(INFO.NE.0)THEN
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6351)
 6351 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6352)
 6352 FORMAT('      FOR MATRIX CHOLESKY DECOMPOSITION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6353)
 6353 FORMAT('      THE INPUT MATRIX IS NOT SINGULAR.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      ENDIF
C
      WRITE(ICOUT,6361)RCOND
      CALL DPWRST('XXX','TEXT ')
 6361 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE MATRIX = ',E15.7)
      IF(1.0+RCOND.EQ.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6371)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,6372)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
      END IF
 6371 FORMAT('****** ERROR IN MATAR2 ********')
 6372 FORMAT('       THE INPUT MATRIX IS SINGULAR')
C
      DO6380I=1,NR1
      DO6382J=I,NR1
        YM9(J,I)=0.
        YM9(I,J)=YM1(I,J)
 6382 CONTINUE
 6380 CONTINUE
C
      ITYP9='MATR'
      NVECT9=NR1
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C
C               ******************************************************
C               **  STEP 64--                                       **
C               **  TREAT THE MATRIX AUGMENT CASE                   **
C               ******************************************************
C
 6400 CONTINUE
C
      IF(NR1.EQ.NR2)GOTO6409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6401)
 6401 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6402)
 6402 FORMAT('      FOR MATRIX AUGMENT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6403)
 6403 FORMAT('      THE NUMBER OF ROWS IN THE TWO MATRICES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6404)
 6404 FORMAT('      MUST BE EQUAL.  SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6407)NR1
 6407 FORMAT('            NUMBER OF ROWS FOR MATRIX 1 =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6408)NR2
 6408 FORMAT('            NUMBER OF ROWS FOR MATRIX 2 =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6409 CONTINUE
C
      IF(NC1+NC2.LE.MAXCOM)GOTO6419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6411)
 6411 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6412)
 6412 FORMAT('      FOR MATRIX AUGMENT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6413)
 6413 FORMAT('      THE NUMBER OF COLUMNS IN THE NEW MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6414)
 6414 FORMAT('      WOULD EXCEED THE ALLOWABLE MAXIMUM.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6417)NC1
 6417 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6418)NC2
 6418 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 2 =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6419 CONTINUE
C
      DO6430J=1,NC1
      DO6435I=1,NR1
      YM9(I,J)=YM1(I,J)
 6435 CONTINUE
 6430 CONTINUE
C
      DO6440J=1,NC2
      DO6445I=1,NR2
      J2=J+NC1
      YM9(I,J2)=YM2(I,J)
 6445 CONTINUE
 6440 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1+NC2
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 65--                                      **
C               **  TREAT THE MATRIX DIAGONAL                CASE  **
C               *****************************************************
C
 6500 CONTINUE
      IF(NR1.EQ.NC1)GOTO6509
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6501)
 6501 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6502)
 6502 FORMAT('      FOR MATRIX DIAGONAL,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6503)
 6503 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6504)
 6504 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6505)
 6505 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6506)
 6506 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6507)NR1
 6507 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6508)NC1
 6508 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6509 CONTINUE
C
      DO6520I=1,NC1
      VECT9(I)=YM1(I,I)
 6520 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 66--                                      **
C               **  TREAT THE DIAGONAL MATRIX                CASE  **
C               *****************************************************
C
 6600 CONTINUE
C
      IF(N1.LE.MAXCOM)GOTO6609
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6601)
 6601 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6602)
 6602 FORMAT('      FOR DIAGONAL MATRIX,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6603)
 6603 FORMAT('      THE NUMBER OF ROWS IN THE VECTOR MUST BE LESS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6604)
 6604 FORMAT('      THAN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6606)
 6606 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6607)N1
 6607 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 6609 CONTINUE
      DO6610J=1,N1
      DO6615I=1,N1
      YM9(I,J)=0.0
 6615 CONTINUE
 6610 CONTINUE
      DO6620I=1,N1
      YM9(I,I)=Y1(I)
 6620 CONTINUE
C
      ITYP9='MATR'
      NR9=N1
      NC9=N1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 67--                                      **
C               **  TREAT THE MATRIX REPLACE ROW             CASE  **
C               *****************************************************
C
 6700 CONTINUE
      IROWID=INT(YS3+0.5)
      IF(IROWID.LT.1 .OR. IROWID.GT.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6701)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6702)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6703)
        WRITE(ICOUT,6704)NR1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6705)IROWID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6701 FORMAT('***** ERROR IN MATAR2--')
 6702 FORMAT('      FOR MATRIX REPLACE ROW,')
 6703 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6704 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6705 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
C
      IF(N2.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6711)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6712)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6713)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6714)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6715)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6716)NC1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6717)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6711 FORMAT('***** ERROR IN MATAR2--')
 6712 FORMAT('      FOR MATRIX REPLACE ROW,')
 6713 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX MUST EQUAL')
 6714 FORMAT('      THE NUMBER OF COLUMNS IN THE VECTOR.  SUCH WAS')
 6715 FORMAT('      NOT THE CASE HERE.')
 6716 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX = ',I8)
 6717 FORMAT('      THE NUMBER OF COLUMNS IN THE VECTOR = ',I8)
C
      DO6720J=1,NC1
      DO6725I=1,NR1
      YM9(I,J)=YM1(I,J)
 6725 CONTINUE
 6720 CONTINUE
      DO6730J=1,N2
      YM9(IROWID,J)=Y2(J)
 6730 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 68--                                      **
C               **  TREAT THE MATRIX REPLACE ELEMENT         CASE  **
C               *****************************************************
C
 6800 CONTINUE
      IROWID=INT(YS2+0.5)
      ICOLID=INT(YS3+0.5)
      IF(IROWID.LT.1 .OR. IROWID.GT.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6801)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6802)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6803)
        WRITE(ICOUT,6804)NR1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6805)IROWID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6801 FORMAT('***** ERROR IN MATAR2--')
 6802 FORMAT('      FOR MATRIX REPLACE ELEMENT,')
 6803 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6804 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6805 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
C
      IF(ICOLID.LT.1 .OR. ICOLID.GT.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6811)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6812)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6813)
        WRITE(ICOUT,6814)NC1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6815)ICOLID
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 6811 FORMAT('***** ERROR IN MATAR2--')
 6812 FORMAT('      FOR MATRIX REPLACE ELEMENT,')
 6813 FORMAT('      THE REQUESTED COLUMN IN THE MATRIX MUST BE')
 6814 FORMAT('      BETWEEN 1 AND ',I8,'.  SUCH WAS NOT THE CASE')
 6815 FORMAT('      HERE.  THE REQUESTED COLUMN NUMBER = ',I8)
C
      DO6820J=1,NC1
      DO6825I=1,NR1
      YM9(I,J)=YM1(I,J)
 6825 CONTINUE
 6820 CONTINUE
      YM9(IROWID,ICOLID)=YS4
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
C               *********************************************
C               **  STEP 69--                              **
C               **  TREAT THE TRIDIAGONAL SOLUTION   CASE  **
C               **  REFERENCE--LINPACK (CHAPTER 7)         **
C               *********************************************
C
 6900 CONTINUE
C
      IF((N1.EQ.N2).AND.(N2.EQ.N3).AND.(N3.EQ.N4))GOTO6909
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6901)
 6901 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6902)
 6902 FORMAT('      FOR SOLVING A TRIDIAGONAL EQUATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6903)
 6903 FORMAT('      THE NUMBER OF ROWS IN THE FOUR INPUT VECTORS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6904)
 6904 FORMAT('      MUST BE EQUAL.  SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6907)N1,N2,N3,N4
 6907 FORMAT('              NUMBER OF ROWS IN THE VECTORS = ',4(I8,1X))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6909 CONTINUE
C
      CALL SGTSL(N1,Y1,Y2,Y3,Y4,INFO)
      IF(INFO.EQ.0)GOTO6919
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6911)
 6911 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6912)
 6912 FORMAT('      IN SOLVING A TRIDIAGONAL EQUATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6913)
 6913 FORMAT('      A ZERO PIVOT ELEMENT WAS DETECTED.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 6919 CONTINUE
C
      DO6920I=1,N1
      VECT9(I)=Y4(I)
 6920 CONTINUE
C
      ITYP9='VECT'
      NVECT9=N1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 70--                              **
C               **  TREAT THE TRIANGULAR SOLVE       CASE  **
C               **  REFERENCE--LINPACK (CHAPTER 6)         **
C               *********************************************
C
 7000 CONTINUE
C
      IF(NR1.EQ.N2)GOTO7009
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7001)
 7001 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7002)
 7002 FORMAT('      FOR SOLVING A MATRIX EQUATION SUCH AS A*X = B,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7003)
 7003 FORMAT('      THE NUMBER OF ROWS IN THE LEFT-SIDE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7004)
 7004 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7005)
 7005 FORMAT('      THE NUMBER OF ROWS IN THE RIGHT-SIDE VECTOR;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7006)
 7006 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7007)NR1
 7007 FORMAT('              NUMBER OF ROWS IN THE MATRIX = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7008)N2
 7008 FORMAT('              NUMBER OF ROWS IN THE VECTOR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7009 CONTINUE
C
      IJOB=1
      DO7046I=1,NR1
      DO7047J=I+1,NC1
      IF(YM1(I,J).NE.0.0)GOTO7049
 7047 CONTINUE
 7046 CONTINUE
      IJOB=0
 7049 CONTINUE
C
      DO7051I=1,N2
      VECT9(I)=Y2(I)
 7051 CONTINUE
C
      CALL STRSL(YM1,MAXROM,NR1,VECT9,IJOB,INFO)
      IF(INFO.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7071)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,7072)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
      END IF
 7071 FORMAT('****** ERROR IN MATAR2 ********')
 7072 FORMAT('       THE INPUT MATRIX IS SINGULAR')
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 71--                              **
C               **  TREAT THE TRIANGULAR INVERSE     CASE  **
C               **  REFERENCE--LINPACK (CHAPTER 6)         **
C               *********************************************
C
 7100 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO7109
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7101)
 7101 FORMAT('***** ERROR IN MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7102)
 7102 FORMAT('      FOR TRIANGULAR INVERSE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7103)
 7103 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7104)
 7104 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7105)
 7105 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7106)
 7106 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7107)NR1
 7107 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7108)NC1
 7108 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7109 CONTINUE
C
      IJOB=11
      DO7126I=1,NR1
      DO7127J=I+1,NC1
      IF(YM1(I,J).NE.0.0)GOTO7129
 7127 CONTINUE
 7126 CONTINUE
      IJOB=10
 7129 CONTINUE
      CALL STRDI(YM1,MAXROM,NR1,Y1,IJOB,INFO)
      IF(INFO.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7171)
        CALL DPWRST('XXX','ERRO')
        WRITE(ICOUT,7172)
        CALL DPWRST('XXX','ERRO')
        IERROR='YES'
        GOTO9000
      END IF
 7171 FORMAT('****** ERROR IN MATAR2 ********')
 7172 FORMAT('       THE INPUT MATRIX IS SINGULAR')
C
      DO7181J=1,NC1
      DO7182I=1,NR1
      YM9(I,J)=YM1(I,J)
 7182 CONTINUE
 7181 CONTINUE
CCCCC END CHANGE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *********************************************
C               **  STEP 72--                              **
C               **  TREAT THE MATRIX ITERATIVE SOLUTION CASE*
C               **  REFERENCE--LINPACk (PAGE 1.9)          **
C               *********************************************
C
 7200 CONTINUE
C
      IF(NR1.EQ.N2)GOTO7209
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7201)
 7201 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7202)
 7202 FORMAT('      FOR SOLVING A MATRIX EQUATION SUCH AS A*X = B,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7203)
 7203 FORMAT('      THE NUMBER OF ROWS IN THE LEFT-SIDE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7204)
 7204 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7205)
 7205 FORMAT('      THE NUMBER OF ROWS IN THE RIGHT-SIDE VECTOR;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7206)
 7206 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7207)NR1
 7207 FORMAT('              NUMBER OF ROWS IN THE MATRIX = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7208)N2
 7208 FORMAT('              NUMBER OF ROWS IN THE VECTOR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7209 CONTINUE
C
      DO7241J=1,NC1
      DO7242I=1,NR1
      YM2(I,J)=YM1(I,J)
 7242 CONTINUE
      VECT9(J)=Y2(J)
 7241 CONTINUE
C
      CALL SGEFA(YM2,MAXROM,NR1,INDEX,INFO)
      IF(INFO.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7271)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,7272)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
        GOTO9000
      END IF
 7271 FORMAT('****** ERROR IN MATAR2 ********')
 7272 FORMAT('       THE INPUT MATRIX IS SINGULAR')
C
      IJOB=0
      CALL SGESL(YM2,MAXROM,NR1,INDEX,VECT9,IJOB)
      XNORM=SASUM(NR1,VECT9,1)
      RELERR=0.0
      IF(XNORM.EQ.0.0)GOTO7295
      DO7280ITER=1,20
        DO7285I=1,NR1
          Y3(I)=SDSDOT(NR1,YM1(I,1),MAXROM,VECT9(1),1,-Y2(I))
 7285   CONTINUE
        CALL SGESL(YM2,MAXROM,NR1,INDEX,Y3,IJOB)
        DO7290I=1,NR1
          VECT9(I)=VECT9(I)-Y3(I)
 7290   CONTINUE
        RNORM=SASUM(NR1,Y3,1)
        IF(ITER.EQ.1)RELERR=RNORM/XNORM
        YS1=XNORM+RNORM
        IF(YS1.EQ.XNORM)GOTO7295
 7280 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7281)
      CALL DPWRST('XXX','ERRO ')
      WRITE(ICOUT,7282)
      CALL DPWRST('XXX','ERRO ')
      GOTO9000
 7281 FORMAT('****** ERROR IN MATARI ********')
 7282 FORMAT('       SOLUTION FAILED TO CONVERGE.')
C
 7295 CONTINUE
      ITYP9='VECT'
      NVECT9=NR1
      IF(IFEEDB.EQ.'OFF')GOTO7299
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7296)RCOND
      CALL DPWRST('XXX','TEXT ')
 7296 FORMAT('THE RELATIVE ERROR = ',E15.7)
 7299 CONTINUE
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 73--                                 **
C               **  TREAT THE BIPLOT CASE                     **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 7300 CONTINUE
C
      DO7322J=1,MAXCOM
      DO7321I=1,MAXROM
        YM9(I,J)=0.0
        YM2(I,J)=0.0
 7321 CONTINUE
 7322 CONTINUE
C
C     STEP 1: SCALE MATRIX (BASED ON IBPLSC)
C
C             1) GMEA  - SUBTRACT GRAND MEAN (DEFAULT)
C             2) CMEA  - SUBTRACT COLUMN MEAN
C             3) NONE  - NO SCALING
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7301)IBPLSC,NR1,NC1
 7301   FORMAT('AT BIPLOT: IBPLSC,NR1,NC1 = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO7305I=1,NR1
          DO7306J=1,NC1
            WRITE(ICOUT,7308)I,J,YM1(I,J)
 7308       FORMAT('I,J,YM1(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 7306     CONTINUE
 7305   CONTINUE
      ENDIF
C
      IF(IBPLSC.EQ.'CMEA')THEN
        DO7330J=1,NC1
          DSUM1=0.0D0
          DO7335I=1,NR1
            DSUM1=DSUM1 + DBLE(YM1(I,J))
 7335     CONTINUE
          AMEAN=REAL(DSUM1/DBLE(NR1))
          DO7338I=1,NR1
            YM1(I,J)=YM1(I,J) - AMEAN
 7338     CONTINUE
 7330   CONTINUE
      ELSEIF(IBPLSC.EQ.'GMEA')THEN
        DSUM1=0.0D0
        DO7340J=1,NC1
          DO7345I=1,NR1
            DSUM1=DSUM1 + DBLE(YM1(I,J))
 7345     CONTINUE
 7340   CONTINUE
        AMEAN=REAL(DSUM1/DBLE(NR1*NC1))
        DO7349J=1,NC1
          DO7348I=1,NR1
            YM1(I,J)=YM1(I,J) - AMEAN
 7348     CONTINUE
 7349   CONTINUE
      ENDIF
C
C     STEP 2: COMPUTE EUCLIDEAN NORM
C
      DSUM1=0.0D0
      DO7361I=1,NR1
      DO7362J=1,NC1
        DYM1=YM1(I,J)
        DSUM1=DSUM1+DYM1*DYM1
 7362 CONTINUE
 7361 CONTINUE
      DYM1=0.0D0
      IF(DSUM1.GT.0.0D0)DYM1=DSQRT(DSUM1)
      SCAL9=REAL(DYM1)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7369)DYM1
 7369   FORMAT('DYM1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 3: COMPUTE SINGULAR VALUE FACTORIZATION
C
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,
     1YM2,MAXROM,Y2,IJOB,IERR2)
C
      S1=VECT9(1)
      S2=VECT9(2)
      AFACT1=S1**PBPLCO
      AFACT2=S2**PBPLCO
      SCAL9=(S1**2 + S2**2)/SCAL9**2
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7366)PBPLCO,S1,S2,SCAL9
 7366   FORMAT('PBPLCO,S1,S2,SCAL9 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO7670I=1,NTEMP1
        VECT9(I)=YM9(I,1)*AFACT1
        Y2(I)=YM9(I,2)*AFACT2
        Y3(I)=1.0
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
          WRITE(ICOUT,7371)I,VECT9(I),Y2(I),Y3(I)
 7371     FORMAT('I,VECT9(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 7670 CONTINUE
      ICNT=NTEMP1
      AFACT1=S1**(1.0-PBPLCO)
      AFACT2=S2**(1.0-PBPLCO)
      DO7680I=1,NTEMP2
        ICNT=ICNT+1
        VECT9(ICNT)=YM2(1,I)*AFACT1
        Y2(ICNT)=YM2(2,I)*AFACT2
        Y3(ICNT)=2.0
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
          WRITE(ICOUT,7381)I,ICNT,VECT9(I),Y2(I),Y3(I)
 7381     FORMAT('I,ICNT,VECT9(I),Y2(I),Y3(I) = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 7680 CONTINUE
C
      ITYP9='VECT'
      NVECT9=ICNT
      IUPFLG='FULL'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TAR2')GOTO9090
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IMCASE,IMSUBC
 9013 FORMAT('IMCASE,IMSUBC = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMVAR,IWRITE
 9014 FORMAT('NUMVAR,IWRITE = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)YS1,YS2,YS3,YS4
 9015 FORMAT('YS1,YS2,YS3,YS4 = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IERROR
 9016 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IYS2,IYS3,IYS23,NRJ,NCJ
 9017 FORMAT('IYS2,IYS3,IYS23,NRJ,NCJ = ',5I8)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GOTO9039
      IF(NC1.LE.0)GOTO9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO9032I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9049
      IF(NC2.LE.0)GOTO9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO9042I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9059
      IF(NC9.LE.0)GOTO9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9055I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GOTO9119
      DO9112I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GOTO9129
      DO9122I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GOTO9139
      DO9132I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GOTO9149
      DO9142I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GOTO9169
      DO9162I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9179
      IF(NC9.LE.0)GOTO9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9172I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
C
      IF(IMCASE.NE.'MASS')GOTO9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9189
      IF(NC2.LE.0)GOTO9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO9182I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
CCCCC WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
C9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      WRITE(ICOUT,9187)NR2
 9187 FORMAT('NR2 = ',I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
C
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE MATAR3(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,
     1                  MAXROM,MAXCOM,MAXOBV,
     1                  Y1,N1,Y2,N2,Y3,N3,
     1                  Y4,N4,Y5,Y6,
     1                  INDEX,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  P,ABSE,RELE,AERROR,
     1                  YS1,YS2,YS3,YS4,
     1                  ASIG90,ASIG95,ASIG99,ASG995,
     1                  IMCASE,IUPFLG,IMSUBC,
     1                  ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,
     1                  ITEMP5,ITEMP6,ITEMP7,
     1                  YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,
     1                  ICASS7,
     1                  IRELAT,CLWID,XSTART,XSTOP,
     1                  STME,STMEC,ST2T,ST2TC,STC,STT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
C              OF THE REAL DATA IN MATRICES YM1 AND YM2.
C              ADD SOME ADDITIONAL FUNCTIONALITY
C
C     OPERATIONS--QUADRATIC FORM (X'MX)
C                 1-SAMPLE HOTELLING T-SQUARE
C                 2-SAMPLE HOTELLING T-SQUARE
C                 POOLED SAMPLE VARIANCE-COVARIANCE MATRIX
C                 MATRIX <ROW/COLUMN> SCALE
C                 <ROW/COLUMN> <STATISTIC>
C                 PARTITION    <STATISTIC>
C                 MATRIX    <STATISTIC>
C                 MATRIX  BIN
C                 EUCLIDEAN <ROW/COLUMN> DISTANCE
C                 CHEBYCHEV <ROW/COLUMN> DISTANCE
C                 L1 NORM <ROW/COLUMN> DISTANCE
C                 MINKOWSKY <ROW/COLUMN> DISTANCE
C                 MAHALANOBIS <ROW/COLUMN> DISTANCE
C                 MATRIX MEAN (I.E., GRAND MEAN)
C                 MATRIX SUM
C                 MATRIX ADD ROW
C                 MATRIX DELETE ROW
C                 LINEAR COMBINATION
C                 VECTOR TIMES TRANSPOSE
C                 MATRIX GROUP MEAN
C                 MATRIX GROUP STANDARD DEVIATION
C                 CATCHER MATRIX
C                 MULTIVARIATE NORMAL RANDOM NUMBERS
C                 MULTINOMIAL RANDOM NUMBERS
C                 MULTINOMIAL PDF
C                 XTXINV MATRIX
C                 VARIANCE INFLATION FACTORS
C                 CONDITION INDICES
C                 CREATE MATRIX
C                 QR DECOMPOSITION (NOT DONE)
C                 PSEUDO INVERSE
C                 WISHART RANDOM NUMBERS
C                 INDEPENDENT UNIFORM RANDOM NUMBERS
C                 CORRELATED UNIFORM RANDOM NUMBERS
C                 MULTIVARIATE NORMAL CDF
C                 DIRICHLET RANDOM NUMBERS
C                 MATRIX BIN
C                 MATRIX PARTITION <STAT>
C                 MATRIX <STAT>
C                 MINIMAL SPANNING TREE
C                 MATRIX RENUMBER
C                 EDGES TO ADJACENCY MATRIX
C                 MATRIX <ROW/COLUMN> FIT
C                 VARIABLE TO MATRIX
C                 MATRIX TO VARIABLE
C                 MATRIX COMBINE ROWS
C                 MATRIX COMBINE COLUMNS
C                 GENERATE MATRIX <STAT>
C                 DEX CORE
C                 DEX CONFOUND
C                 DEX CHECK CLASSIC
C                 DEX CHECK CENTER POINT
C
C     EXAMPLES--LET A1 = QUADRATIC FORM M X
C             --LET A1 = HOTELLING T-SQUARE M U0
C             --LET Y1 = MATRIX ROW MEAN M
C               LET Y1 = MATRIX COLUMN MEAN M
C
C     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
C                     --NR1
C                     --NC1
C                     --YM2 (REAL MATRIX)
C                     --NR2
C                     --NC2
C                     --YM3 (REAL MATRIX)
C                     --NR3
C                     --NC3
C                     --Y1  (REAL VECTOR)
C                     --N1
C                     --Y2  (REAL VECTOR)
C                     --N2
C                     --Y3  (REAL VECTOR)
C                     --N3
C                     --Y4  (REAL VECTOR)
C                     --N4
C     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
C                     --NR9
C                     --NC9
C                     --VECT9 (REAL VECTOR)
C                     --NVECT9
C                     --SCAL9 (REAL SCALAR)
C                     --ITYP9
C
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
C           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/06
C     ORIGINAL VERSION--JUNE      1998.
C     UPDATED         --MAY       2002. MULTIVARIATE NORM RAND NUMB
C     UPDATED         --MAY       2002. MULTINOMIAL RAND NUMB
C     UPDATED         --MAY       2002. WISHART RAND NUMB
C     UPDATED         --JUNE      2002. CATCHER MATRIX
C     UPDATED         --JUNE      2002. XTXINV MATRIX
C     UPDATED         --JUNE      2002. VARIANCE INFLATION FACTORS
C     UPDATED         --JUNE      2002. CONDITION NUMBERS
C     UPDATED         --JUNE      2002. CREATE MATRIX
C     UPDATED         --AUGUST    2002. USE "CMPSTA" TO COMPUTE
C                                       STATISTIC FOR
C                                       MATRIX <ROW/COLU> <STAT>
C     UPDATED         --APRIL     2003. FIX WISHART RANDOM NUMBERS
C     UPDATED         --APRIL     2003. MULTIVARIATE T RANDOM NUMBERS
C     UPDATED         --APRIL     2003. INDPENDENT UNIFORM RANDOM NUMB
C     UPDATED         --APRIL     2003. MULTIVARIATE NORMAL CDF
C     UPDATED         --APRIL     2003. MULTIVARIATE T CDF
C     UPDATED         --APRIL     2003. ARGUMENT LIST TO CMPSTA
C     UPDATED         --SEPTEMBER 2003. CORRELATED UNIFORM RANDOM NUMB
C     UPDATED         --JUNE      2005. MATRIX PARTITION <STAT>
C     UPDATED         --JUNE      2005. MATRIX <STAT>
C     UPDATED         --JULY      2005. MATRIX PARTITION <STAT>
C                                       EXTENDED TO UNEQUAL PARTITION
C                                       CASE
C     UPDATED         --MARCH     2006. MATRIX BIN
C     UPDATED         --MAY       2008. MATRIX RENUMBER
C     UPDATED         --JUNE      2008. EDGES TO ADJACENCY MATRIX
C     UPDATED         --SEPTEMBER 2008. ACTIVATE PSEUDO INVERSE COMMAND
C                                       (ACTUALLY RETURNS TRANSPOSE OF
C                                       PSEUDO INVERSE)
C     UPDATED         --JANUARY   2009. DISTINCTION BETWEEN DIRECTED AND
C                                       UNDIRECTED ADJACENCY MATRIX
C     UPDATED         --FEBRUARY  2010. MATRIX <ROW/COLUMN> FIT
C     UPDATED         --JUNE      2010. CALL LIST TO CMPSTA
C     UPDATED         --NOVEMBER  2010. VARIABLE TO MATRIX
C     UPDATED         --NOVEMBER  2010. MATRIX TO VARIABLE
C     UPDATED         --JANUARY   2011. MATRIX COMBINE ROWS
C     UPDATED         --JANUARY   2011. MATRIX COMBINE COLUMNS
C     UPDATED         --AUGUST    2017. GENERATE MATRIX <STAT>
C     UPDATED         --JANUARY   2018. DEX CORE
C     UPDATED         --JANUARY   2018. DEX CONFOUND
C     UPDATED         --AUGUST    2018. HAVE ALL DISTANCE MATRIX
C                                       (EUCLIDEAN, MINKOWSKY, BLOCK,
C                                       CHEBYCHEV) GO THROUGH A SINGLE
C                                       ROUTINE
C     UPDATED         --AUGUST    2018. ADDED ADDITIONAL DISTANCE
C                                       MATRIX OPTIONS
C     UPDATED         --SEPTEMBER 2018. DEX CHECK CENTER POINTS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      CHARACTER*4 IMCASE
      CHARACTER*4 ICASS7
      CHARACTER*4 IUPFLG
      CHARACTER*4 IMSUBC
      CHARACTER*4 ITYPA1
      CHARACTER*4 ITYPA2
      CHARACTER*4 ITYPA3
      CHARACTER*4 ITYPA4
      CHARACTER*4 IWRITE
      CHARACTER*4 ITYP9
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IRELAT
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASE2
C
CCCCC MAY 2002. ADD FOLLOWING LINE
      LOGICAL LTF
C
C-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DNR1
      DOUBLE PRECISION DNC1
      DOUBLE PRECISION D999
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION ABSEPS
      DOUBLE PRECISION RELEPS
      DOUBLE PRECISION VALS
      DOUBLE PRECISION ERRS
      DOUBLE PRECISION DN
      DOUBLE PRECISION DNORM
      DOUBLE PRECISION DLNPDF
      DOUBLE PRECISION DLNGAM
C
C---------------------------------------------------------------------
C
      DIMENSION YM1(MAXROM,MAXCOM)
      DIMENSION YM2(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION Y5(*)
      DIMENSION Y6(*)
      DIMENSION YM9(MAXROM,MAXCOM)
      DIMENSION VECT9(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
      INTEGER INDEX(*)
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
      INTEGER ITEMP7(*)
C
C     2021/01: UP FROM MAX OF 40 CHARACTERS TO MAX OF 80 CHARACTERS
C
      CHARACTER*80 STME(500)
      CHARACTER*80 STMEC(500)
      CHARACTER*80 ST2T(500)
      CHARACTER*80 ST2TC(500)
      CHARACTER*80 STC(500)
      CHARACTER*80 STT(500)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='MATA'
      ISUBN2='R3  '
      IERROR='NO'
C
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
      NRJ=(-999)
      NCJ=(-999)
      D999=(-999.0D0)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ATR3')THEN
C
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52   FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE,NUMVAR
   53   FORMAT('IMCASE,IMSUBC,IWRITE,NUMVAR = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)YS1,YS2,YS3,YS4,Y6(1)
   55   FORMAT('YS1,YS2,YS3,YS4,Y6(1) = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)NR1,NC1
   61   FORMAT('NR1,NC1 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR1.GE.1 .AND. NC1.GE.1)THEN
          JMAX=NC1
          IF(JMAX.GT.10)JMAX=10
          DO62I=1,NR1
            WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63       FORMAT('I,YM1(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)NR2,NC2
   71   FORMAT('NR2,NC2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR2.GE.1 .AND. NC2.GE.1)THEN
          JMAX=NC2
          IF(JMAX.GT.10)JMAX=10
          DO72I=1,NR2
            WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73       FORMAT('I,YM2(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   72     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)NR3,NC3
   81   FORMAT('NR3,NC3 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR3.GE.1 .AND. NC3.GE.1)THEN
          JMAX=NC3
          IF(JMAX.GT.10)JMAX=10
          DO82I=1,NR3
            WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83       FORMAT('I,YM9(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)N1
  111   FORMAT('N1 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GE.1)THEN
          DO112I=1,N1
            WRITE(ICOUT,113)I,Y1(I)
  113       FORMAT('I,Y1(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  112     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)N2
  121   FORMAT('N2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N2.GE.1)THEN
          DO122I=1,N2
            WRITE(ICOUT,123)I,Y2(I)
  123       FORMAT('I,Y2(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  122     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)N3
  131   FORMAT('N3 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N3.GE.1)THEN
          DO132I=1,N3
            WRITE(ICOUT,133)I,Y3(I)
  133       FORMAT('I,Y3(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  132     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)N4
  141   FORMAT('N4 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N4.GE.1)THEN
          DO142I=1,N4
            WRITE(ICOUT,143)I,Y4(I)
  143       FORMAT('I,Y4(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  142     CONTINUE
        ENDIF
C
      ENDIF
C
C               **************************************************
C               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
C               **************************************************
C
      DNR1=NR1
      DNC1=NC1
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
C               ********************************************
C
      IF(IMCASE.EQ.'CRMA')GOTO8500
      IF(IMCASE.EQ.'GMST')GOTO8550
      IF(IMCASE.EQ.'CORE')GOTO10800
      IF(IMCASE.EQ.'CONF')GOTO10900
      IF(IMCASE.EQ.'CKCL')GOTO11000
      IF(IMCASE.EQ.'CKCP')GOTO11100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GOTO1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GOTO1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GOTO1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GOTO1100
C
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GOTO1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GOTO1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GOTO1100
C
      GOTO1190
C
 1100 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('      THE INPUT NUMBER OF ROWS AND/OR COLUMNS IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('      MATRIX AND/OR VECTOR FOR WHICH THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1121)
 1121 FORMAT('      OPERATION IS TO BE COMPUTED MUST BE 1 OR')
      WRITE(ICOUT,1182)
 1182 FORMAT('      LARGER;  SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
C
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1)THEN
        WRITE(ICOUT,1183)NR1,NC1
 1183   FORMAT('            MATRIX 1--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2)THEN
        WRITE(ICOUT,1184)NR2,NC2
 1184   FORMAT('            MATRIX 2--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3)THEN
        WRITE(ICOUT,1185)NR3,NC3
 1185   FORMAT('            MATRIX 3--',I8,' ROWS BY ',I8,' COLUMNS')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1)THEN
        WRITE(ICOUT,1186)N1
 1186   FORMAT('            VECTOR 1--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2)THEN
        WRITE(ICOUT,1187)N2
 1187   FORMAT('            VECTOR 2--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3)THEN
        WRITE(ICOUT,1188)N3
 1188   FORMAT('            VECTOR 3--',I8,' ROWS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 1190 CONTINUE
C
C               *********************************
C               **  STEP 12--                  **
C               **  BRANCH TO THE PROPER CASE  **
C               *********************************
C
      IF(IMCASE.EQ.'MPVC')GOTO5600
      IF(IMCASE.EQ.'MQFO')GOTO5800
      IF(IMCASE.EQ.'MHT1')GOTO5900
      IF(IMCASE.EQ.'MHT2')GOTO5700
      IF(IMCASE.EQ.'MROW')GOTO6000
      IF(IMCASE.EQ.'MCOL')GOTO6100
C
      IF(IMCASE.EQ.'MDER')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MDEC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MDKR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MDKC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MDBR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MDBC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MDCR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MDCC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MCSR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MCSC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MCDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MCDC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MZSR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MASC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MZDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MADC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MJSR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MJSC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MJDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MJDC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MPDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MPDC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MHDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MHDC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MXDR')THEN
        ICASE='ROW '
        GOTO6200
      ENDIF
      IF(IMCASE.EQ.'MXDC')THEN
        ICASE='COLU'
        GOTO6200
      ENDIF
C
      IF(IMCASE.EQ.'MRSC')THEN
        ICASE='ROW '
        GOTO6500
      ENDIF
      IF(IMCASE.EQ.'MCSC')THEN
        ICASE='COLU'
        GOTO6500
      ENDIF
C
      IF(IMCASE.EQ.'MDMR')THEN
        ICASE='ROW '
        GOTO6600
      ENDIF
      IF(IMCASE.EQ.'MDMC')THEN
        ICASE='COLU'
        GOTO6600
      ENDIF
C
      IF(IMCASE.EQ.'MQRD')GOTO6300
      IF(IMCASE.EQ.'MPIN')GOTO6400
      IF(IMCASE.EQ.'MAMM')GOTO7000
      IF(IMCASE.EQ.'MSUM')GOTO7030
      IF(IMCASE.EQ.'MAAR')GOTO7100
      IF(IMCASE.EQ.'MADR')GOTO7200
      IF(IMCASE.EQ.'MADM')GOTO7300
      IF(IMCASE.EQ.'MALC')GOTO7400
      IF(IMCASE.EQ.'MAVT')GOTO7500
      IF(IMCASE.EQ.'MAGM')GOTO7600
      IF(IMCASE.EQ.'MAGS')GOTO7700
      IF(IMCASE.EQ.'MVRN')GOTO7800
      IF(IMCASE.EQ.'MURN')GOTO7900
      IF(IMCASE.EQ.'MPDF')GOTO7950
      IF(IMCASE.EQ.'WIRN')GOTO8000
      IF(IMCASE.EQ.'MACA')GOTO8100
      IF(IMCASE.EQ.'XTXI')GOTO8200
      IF(IMCASE.EQ.'VINF')GOTO8300
      IF(IMCASE.EQ.'CIND')GOTO8400
      IF(IMCASE.EQ.'CRMA')GOTO8500
      IF(IMCASE.EQ.'GMST')GOTO8550
      IF(IMCASE.EQ.'IURN')GOTO8600
      IF(IMCASE.EQ.'NCDF')GOTO8700
      IF(IMCASE.EQ.'TCDF')GOTO8800
      IF(IMCASE.EQ.'TCDF')GOTO8800
      IF(IMCASE.EQ.'MTRN')GOTO8900
      IF(IMCASE.EQ.'DIRN')GOTO8950
      IF(IMCASE.EQ.'DPDF')GOTO9300
      IF(IMCASE.EQ.'DLPD')GOTO9300
      IF(IMCASE.EQ.'INRN')GOTO9400
      IF(IMCASE.EQ.'MPAR')GOTO9500
      IF(IMCASE.EQ.'MGRA')GOTO9600
      IF(IMCASE.EQ.'MATB')GOTO9700
      IF(IMCASE.EQ.'MARB')GOTO9700
      IF(IMCASE.EQ.'MSPT')GOTO9800
      IF(IMCASE.EQ.'MSP2')GOTO9900
      IF(IMCASE.EQ.'MARN')GOTO10000
      IF(IMCASE.EQ.'ADMA')GOTO10100
      IF(IMCASE.EQ.'ADMD')GOTO10100
      IF(IMCASE.EQ.'MFTR')GOTO10200
      IF(IMCASE.EQ.'MFTC')GOTO10300
      IF(IMCASE.EQ.'VMAT')GOTO10400
      IF(IMCASE.EQ.'MVAR')GOTO10500
      IF(IMCASE.EQ.'MCRO')GOTO10600
      IF(IMCASE.EQ.'MCCO')GOTO10700
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** INTERNAL ERROR IN MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)IMCASE
 1212 FORMAT('      IMCASE NOT MATCHED.  IMCASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *******************************************************
C               **  STEP 56--                                        **
C               **  TREAT THE POOLED VARIANCE-COVARIANCE MATRIX  CASE**
C               *******************************************************
C
 5600 CONTINUE
C
      IF(ITYPA2.EQ.'VARI')GOTO5650
C
      IF(NC1.EQ.NC2)GOTO5609
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5601)
 5601 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5602)
 5602 FORMAT('      FOR THE POOLED VARIANCE-COVARIANCE COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5603)
 5603 FORMAT('      THE NUMBER OF COLUMNS FOR THE TWO MATRICES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5604)
 5604 FORMAT('      MUST BE EQUAL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5606)
 5606 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5607)NC1
 5607 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 1  =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5608)NC2
 5608 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 2  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5609 CONTINUE
C
      CALL VARPOO(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,NR2,
     1DTEMP1,IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
 5650 CONTINUE
C
      IF(NR1.EQ.N2)GOTO5659
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5651)
 5651 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5652)
 5652 FORMAT('      FOR THE POOLED VARIANCE-COVARIANCE COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5653)
 5653 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5654)
 5654 FORMAT('      MUST EQUAL THE NUMBER OF ROWS IN THE GROUP-ID ',
     1'VARIABLE..')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5656)
 5656 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5657)NC1
 5657 FORMAT('            NUMBER OF ROWS FOR MATRIX             =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5658)NC2
 5658 FORMAT('            NUMBER OF ROWS FOR GROUP ID VARIABLE  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5659 CONTINUE
C
      CALL VARPO2(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,MAXROM,
     1Y2,Y3,INDEX,NK,DTEMP1,IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 57--                                        **
C               **  TREAT THE MATRIX 2-SAMPLE HOTELLING T-SQUARE CASE**
C               *******************************************************
C
 5700 CONTINUE
C
      IF(NC1.EQ.NC2)GOTO5709
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5701)
 5701 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5702)
 5702 FORMAT('      FOR THE 2-SAMPLE HOTELLING T-SQUARE TEST,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5703)
 5703 FORMAT('      THE NUMBER OF COLUMNS FOR THE TWO MATRICES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5704)
 5704 FORMAT('      MUST BE EQUAL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5706)
 5706 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5707)NC1
 5707 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 1  =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5708)NC2
 5708 FORMAT('            NUMBER OF COLUMNS FOR MATRIX 2  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5709 CONTINUE
C
      CALL HTTSQ2(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NR2,NC1,
     1TSTAT,ASIG90,ASIG95,ASIG99,ASG995,
     1DTEMP1,Y1,Y2,Y3,INDEX,
     1IBUGA3,IERROR)
C
      SCAL9=TSTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 58--                                 **
C               **  TREAT THE MATRIX QUADRATIC FORM  CASE     **
C               **  QUADRATIC FORM = x'Mx                     **
C               **  x IS A VECTOR AND M IS A MATRIX           **
C               ************************************************
C
 5800 CONTINUE
C
      IF(NR1.EQ.NC1)GOTO5809
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5801)
 5801 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5802)
 5802 FORMAT('      FOR QUADRATIC FORM,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5803)
 5803 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5804)
 5804 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5805)
 5805 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5806)
 5806 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5807)NR1
 5807 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5808)NC1
 5808 FORMAT('            NUMBER OF COLUMNS =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5809 CONTINUE
C
      IF(N2.EQ.NR1)GOTO5859
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5851)
 5851 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5852)
 5852 FORMAT('      FOR QUADRATIC FORM, THE NUMBER OF ROWS IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5855)
 5855 FORMAT('      MATRIX MUST = NUMBER OF ROWS IN THE VECTOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5856)
 5856 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5858)NR1,N1
 5858 FORMAT('          MATRIX --',I8,' ROWS, VECTOR  ',I8,' COLUMNS')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5859 CONTINUE
C
      CALL QUAFRM(YM1,MAXROM,MAXCOM,NR1,NC1,Y2,IWRITE,SCAL9,
     1IBUGA3,IERROR)
C
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 59--                                        **
C               **  TREAT THE MATRIX 1-SAMPLE HOTELLING T-SQUARE CASE**
C               **  H0: U=U0                                         **
C               **  T-SQUARE = N*(XBAR-U0)'*SINV*(XBAR-U0)           **
C               **  WHERE SINV = SAMPLE VARIANCE-COVARIANCE MATRIX   **
C               *******************************************************
C
 5900 CONTINUE
C
      IF(NC1.EQ.N2)GOTO5909
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5901)
 5901 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5902)
 5902 FORMAT('      FOR THE 1-SAMPLE HOTELLING T-SQUARE TEST,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5903)
 5903 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5904)
 5904 FORMAT('      MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5905)
 5905 FORMAT('      THE NUMBER OF ROWS IN THE MEAN VECTOR;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5906)
 5906 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5907)NC1
 5907 FORMAT('            NUMBER OF COLUMNS FOR MATRIX    =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5908)N2
 5908 FORMAT('            NUMBER OF ROWS FOR MEAN VECTOR  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5909 CONTINUE
C
      CALL HTTSQ1(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,
     1TSTAT,ASIG90,ASIG95,ASIG99,ASG995,
     1DTEMP1,Y2,Y1,Y3,INDEX,
     1IBUGA3,IERROR)
C
      SCAL9=TSTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 60--                                 **
C               **  TREAT THE MATRIX ROW STATISTIC CASE       **
C               ************************************************
C
CCCCC IMPLEMENTED JULY 1993.
 6000 CONTINUE
C
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
C
      DO6010I=1,NR1
        DO6015J=1,NC1
          Y1(J)=YM1(I,J)
 6015   CONTINUE
        ASTAT=0.0
        CALL CMPSTA(
     1  Y1,Y2,Y2,Y3,Y4,Y5,MAXNXT,NC1,NC1,NC1,NUMV2,ICASS7,
     1  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1  DTEMP1,DTEMP2,DTEMP3,
CCCCC1  IQUAME,IQUASE,PSTAMV,
     1  ASTAT,
     1  ISUBRO,IBUGA3,IERROR)
        VECT9(I)=ASTAT
 6010 CONTINUE
C
      ITYP9='VECT'
      NR9=1
      NC9=1
      NVECT9=NR1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 61--                                      **
C               **  TREAT THE MATRIX COLUMN STATISTIC        CASE  **
C               *****************************************************
C
 6100 CONTINUE
C
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
C
      DO6110I=1,NC1
        DO6115J=1,NR1
          Y1(J)=YM1(J,I)
 6115   CONTINUE
        ASTAT=0.0
        CALL CMPSTA(
     1  Y1,Y2,Y2,Y3,Y4,Y5,MAXNXT,NR1,NR1,NR1,NUMV2,ICASS7,
     1  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1  DTEMP1,DTEMP2,DTEMP3,
CCCCC1  IQUAME,IQUASE,PSTAMV,
     1  ASTAT,
     1  ISUBRO,IBUGA3,IERROR)
        VECT9(I)=ASTAT
 6110 CONTINUE
C
      ITYP9='VECT'
      NR9=1
      NC9=1
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 62--                                      **
C               **  TREAT THE MATRIX EUCLIDEAN DISTANCE      CASE  **
C               **            MATRIX CHEBYCHEV DISTANCE      CASE  **
C               **            MATRIX MINKOWSKY DISTANCE      CASE  **
C               **            MATRIX BLOCK     DISTANCE      CASE  **
C               **            MATRIX COSINE    DISTANCE      CASE  **
C               **            MATRIX COSINE    SIMILARITY    CASE  **
C               **            MATRIX JACCARD   DISTANCE      CASE  **
C               **            MATRIX JACCARD   SIMILARITY    CASE  **
C               *****************************************************
C
 6200 CONTINUE
C
      IF(ICASE.EQ.'ROW '.AND.NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6211)
 6211   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6213)
 6213   FORMAT('      FOR MATRIX ROW DISTANCES, THE NUMBER OF ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6215)NR1
 6215   FORMAT('      CREATED COLUMNS, ',I8,', WOULD EXCEED THE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6217)MAXCOM
 6217   FORMAT('      MAXIMUM NUMBER OF ALLOWED COLUMNS,  ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IWRITE='OFF'
      ICASE2='EUCL'
      IF(IMCASE(1:3).EQ.'MDK')ICASE2='MINK'
      IF(IMCASE(1:3).EQ.'MDB')ICASE2='BLOC'
      IF(IMCASE(1:3).EQ.'MDC')ICASE2='CHEB'
      IF(IMCASE(1:3).EQ.'MCS')ICASE2='COSS'
      IF(IMCASE(1:3).EQ.'MCD')ICASE2='COSD'
      IF(IMCASE(1:3).EQ.'MJS')ICASE2='JACS'
      IF(IMCASE(1:3).EQ.'MJD')ICASE2='JACD'
      IF(IMCASE(1:4).EQ.'MZSR')ICASE2='ACSS'
      IF(IMCASE(1:4).EQ.'MASC')ICASE2='ACSS'
      IF(IMCASE(1:4).EQ.'MZDR')ICASE2='ACSD'
      IF(IMCASE(1:4).EQ.'MADC')ICASE2='ACSD'
      IF(IMCASE(1:3).EQ.'MPD')ICASE2='PDIS'
      IF(IMCASE(1:3).EQ.'MPS')ICASE2='PSIM'
      IF(IMCASE(1:3).EQ.'MHD')ICASE2='HAMM'
      IF(IMCASE(1:3).EQ.'MXD')ICASE2='CANB'
C
      CALL EUCDIS(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,
     1            ICASE,ICASE2,P,IWRITE,
     1            Y1,Y2,
     1            IBUGA3,ISUBRO,IERROR)
C
      ITYP9='MATR'
      IF(ICASE.EQ.'ROW')THEN
        NR9=NR1
        NC9=NR1
        IUPFLG='SUBS'
      ELSEIF(ICASE.EQ.'COLU')THEN
        NR9=NC1
        NC9=NC1
        IUPFLG='FULL'
      ELSE
        NR9=NR1
        NC9=NR1
        IUPFLG='SUBS'
      ENDIF
      GOTO9000
C
C               *********************************************
C               **  STEP 63--                              **
C               **  TREAT THE MATRIX QR       DECOMP CASE  **
C               **  REFERENCE--LINPACK USER'S GUIDE        **
C               *********************************************
C
 6300 CONTINUE
C
CCCCC IF(NR1.LE.MAXCOM)GOTO6309
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6301)
C6301 FORMAT('***** ERROR IN MATAR2--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6302)
C6302 FORMAT('      FOR MATRIX SINGULAR VALUE DECOMPOSITION,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6303)
C6303 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6304)
C6304 FORMAT('      CAN NOT EXCEED ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6305)
C6305 FORMAT('      THE MAXIMUM NUMBER OF COLUMNS IN THE MATRIX;')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6306)
C6306 FORMAT('      SUCH WAS NOT THE CASE HERE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6307)NR1
C6307 FORMAT('            NUMBER OF ROWS    =',I8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6308)MAXCOM
C6308 FORMAT('            MAXIMUM NUMBER OF COLUMNS =',I8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C
C6309 CONTINUE
      DO6322J=1,MAXCOM
      DO6321I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 6321 CONTINUE
 6322 CONTINUE
C
      IERR2=0
      IJOB=11
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,
     1YM2,MAXROM,Y2,IJOB,IERR2)
C
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NR1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GOTO9000
C
C               ******************************************************
C               **  STEP 64--                                       **
C               **  TREAT THE MATRIX PSEUDO INVERSE         CASE    **
C               ******************************************************
C
 6400 CONTINUE
C
      IF(NR1.LT.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6401)
 6401   FORMAT('***** ERROR IN PSEUDO INVERSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6402)
 6402   FORMAT('      FOR THE MATRIX PSEUDO INVERSE, THE NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6403)
 6403   FORMAT('      ROWS IN THE MATRIX MUST BE GREATER THAN OR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6404)
 6404   FORMAT('      EQUAL TO THE NUMBER OF COLUMNS IN THE MATRIX;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6406)
 6406   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6407)NR1
 6407   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6408)NC1
 6408   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NR1.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6411)
 6411   FORMAT('***** ERROR IN PSEUDO INVERSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6412)
 6412   FORMAT('      FOR THE MATRIX PSEUDO INVERSE, THE NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6413)
 6413   FORMAT('      ROWS IN THE MATRIX EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6414)
 6414   FORMAT('      ALLOWABLE NUMBER OF ROWS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6417)NR1
 6417   FORMAT('            NUMBER OF ROWS         = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6418)MAXROM
 6418   FORMAT('            MAXIMUM NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NC1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6421)
 6421   FORMAT('***** ERROR IN PSEUDO INVERSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6422)
 6422   FORMAT('      FOR THE MATRIX PSEUDO INVERSE, THE NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6423)
 6423   FORMAT('      COLUMNS IN THE MATRIX EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6424)
 6424   FORMAT('      ALLOWABLE NUMBER OF COLUMNS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6427)NR1
 6427   FORMAT('            NUMBER OF COLUMNS         = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6428)MAXROM
 6428   FORMAT('            MAXIMUM NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      KTEMP=0
      CALL MATMPI(YM1,Y1,Y2,Y3,YM2,NR1,NC1,MAXROM,MAXROM,KTEMP,IFLAG)
C
      IF(IFLAG.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6431)
 6431   FORMAT('***** ERROR IN PSEUDO INVERSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6432)
 6432   FORMAT('      UNABLE TO COMPUTE THE SINGULAR VALUE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6433)
 6433   FORMAT('      DECOMPOSITION, SO UNABLE TO COMPUTE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6434)
 6434   FORMAT('      PSEUDO INVERSE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        DO6450J=1,NC1
          DO6460I=1,NR1
            YM9(I,J)=YM1(I,J)
 6460     CONTINUE
 6450   CONTINUE
      ENDIF
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 65--                                      **
C               **  TREAT THE MATRIX SCALE                   CASE  **
C               *****************************************************
C
 6500 CONTINUE
C
      IWRITE='OFF'
      CALL MATSCA(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,Y1,Y2,Y3,
     1IMATSC,ICASE,IWRITE,
     1IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 66--                                      **
C               **  TREAT THE MATRIX MAHALONOBIS DISTANCE    CASE  **
C               *****************************************************
C
 6600 CONTINUE
C
      IF(ICASE.EQ.'ROW '.AND.NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6611)
 6611   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6613)
 6613   FORMAT('      FOR MAHALANOBIS ROW DISTANCES, THE NUMBER OF ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6615)NR1
 6615   FORMAT('      CREATED COLUMNS, ',I8,', WOULD EXCEED THE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6617)MAXCOM
 6617   FORMAT('      MAXIMUM NUMBER OF ALLOWED COLUMNS,  ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IWRITE='OFF'
      CALL MAHDIS(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,
     1Y1,Y2,INDEX,DTEMP1,
     1ICASE,IWRITE,IBUGA3,IERROR)
C
      ITYP9='MATR'
      IF(ICASE.EQ.'ROW')THEN
        NR9=NR1
        NC9=NR1
        IUPFLG='SUBS'
      ELSEIF(ICASE.EQ.'COLU')THEN
        NR9=NC1
        NC9=NC1
        IUPFLG='FULL'
      ELSE
        NR9=NR1
        NC9=NR1
        IUPFLG='SUBS'
      ENDIF
      GOTO9000
C
C               *****************************************************
C               **  STEP 70--                                      **
C               **  TREAT THE MATRIX MEAN                    CASE  **
C               *****************************************************
C
 7000 CONTINUE
C
      ITYP9='SCAL'
      D999=0.0D0
      DO7010J=1,NC1
        DO7020I=1,NR1
          D999=D999+DBLE(YM1(I,J))
 7020   CONTINUE
 7010 CONTINUE
      D999=D999/DBLE(NR1*NC1)
      SCAL9=REAL(D999)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 70.B--                                    **
C               **  TREAT THE MATRIX SUM                     CASE  **
C               *****************************************************
C
 7030 CONTINUE
C
      ITYP9='SCAL'
      D999=0.0D0
      DO7040J=1,NC1
        DO7050I=1,NR1
          D999=D999+DBLE(YM1(I,J))
 7050   CONTINUE
 7040 CONTINUE
      SCAL9=REAL(D999)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 71--                                      **
C               **  TREAT THE MATRIX ADD ROW                 CASE  **
C               *****************************************************
C
 7100 CONTINUE
C
      IF(NC1.NE.N2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7111)
 7111   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7113)
 7113   FORMAT('      FOR MATRIX ADD ROW, THE NUMBER OF COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7115)NC1
 7115   FORMAT('      IN THE MATRIX, ',I8,', DOES NOT EQUAL THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7117)N2
 7117   FORMAT('      NUMBER OF ROWS IN THE VARIABLE,  ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO7110J=1,NC1
         DO7120I=1,NR1
           YM9(I,J)=YM1(I,J)
 7120    CONTINUE
         YM9(NR1+1,J)=Y2(J)
 7110 CONTINUE
C
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1+1
      IUPFLG='SUBS'
      GOTO9000
C               *****************************************************
C               **  STEP 72--                                      **
C               **  TREAT THE MATRIX DELETE ROW              CASE  **
C               *****************************************************
C
 7200 CONTINUE
C
      IYS2=INT(YS2+0.5)
      IF(IYS2.LT.1.OR.IYS2.GT.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7211)
 7211   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7213)
 7213   FORMAT('      FOR MATRIX DELETE ROW, THE ROW TO BE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7215)IYS2
 7215   FORMAT('      DELETED IN THE MATRIX, ',I8,', MUST BE >=1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7217)NR1
 7217   FORMAT('      AND <= ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO7210J=1,NC1
         ICOUNT=0
         DO7220I=1,NR1
           IF(IYS2.NE.I)THEN
             ICOUNT=ICOUNT+1
             YM9(ICOUNT,J)=YM1(I,J)
           ENDIF
 7220    CONTINUE
 7210 CONTINUE
C
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1-1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 73--                                      **
C               **  TREAT THE DISTANCE FROM MEAN             CASE  **
C               *****************************************************
C
 7300 CONTINUE
C
      ICASE='COLU'
      CALL VARCOV(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,DTEMP1,
     1            ICASE,IBUGA3,IERROR)
C
      CALL SGECO(YM2,MAXROM,NC1,INDEX,RCOND,Y1)
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7371)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,7372)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,7373)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
        GOTO9000
      ENDIF
 7371 FORMAT('*** ERROR FROM MATAR3: UNABLE TO COMPUTE THE INVERSE OF ',
     1       'THE COVARIANCE MATRIX.')
 7372 FORMAT('    PROBLEM: SOME COLUMNS ARE LINEARLY DEPDENDENT ON ',
     1       ' OTHER COLUMNS.')
 7373 FORMAT('    SUGGESTED SOLUTION: WORK WITH A SUBSET OF THE ',
     1       'ORIGINAL COLUMNS.')
C
      IJOB=1
      CALL SGEDI(YM2,MAXROM,NC1,INDEX,Y1,Y2,IJOB)
C
      IWRITE='OFF'
      DO7320I=1,NR1
        DO7330J=1,NC1
          Y3(J)=YM1(I,J)-REAL(DTEMP1(J))
 7330   CONTINUE
        CALL QUAFRM(YM2,MAXROM,MAXCOM,NC1,NC1,Y3,IWRITE,SCAL9,
     1              IBUGA3,IERROR)
        VECT9(I)=SCAL9
 7320 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 74--                                      **
C               **  TREAT THE LINEAR COMBINATION             CASE  **
C               *****************************************************
C
 7400 CONTINUE
C
      IF(N2.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7411)
 7411   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7413)
 7413   FORMAT('      FOR lINEAR COMBINATION, THE NUMER OF ROWS ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7415)N2
 7415   FORMAT('      IN THE VECTOR, ',I8,' DOES NOT EQUAL THE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7417)NC1
 7417   FORMAT('      NUMBER OF COLUMNS IN THE MATRIX, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ICASE='COLU'
      CALL VARCOV(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,DTEMP1,
     1            ICASE,IBUGA3,IERROR)
C
      DO7430J=1,NR1
        DSUM1=0.0D0
        DO7440L=1,NC1
          DSUM1=DSUM1 + DBLE(Y2(L))*DBLE(YM1(J,L))
 7440     CONTINUE
          VECT9(J)=REAL(DSUM1)
 7430   CONTINUE
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='SUBS'
      GOTO9000
C
C               *****************************************************
C               **  STEP 75--                                      **
C               **  TREAT THE VECTOR TIMES TRANSPOSE         CASE  **
C               *****************************************************
C
 7500 CONTINUE
C
      IF(N1.GT.MAXCOM)THEN
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7501)
 7501 FORMAT('***** ERROR IN MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7502)
 7502 FORMAT('      FOR VECTOR TIMES TRANSPOSE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7503)
 7503 FORMAT('      THE NUMBER OF ROWS IN THE VECTOR MUST BE LESS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7504)
 7504 FORMAT('      THAN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7506)
 7506 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7507)N1
 7507 FORMAT('            NUMBER OF ROWS    =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      ENDIF
C
      DO7520I=1,N1
        DO7530J=1,N1
          YM9(I,J)=Y1(I)*Y1(J)
 7530   CONTINUE
 7520 CONTINUE
C
      ITYP9='MATR'
      NR9=N1
      NC9=N1
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 76--                                        **
C               **  TREAT THE MATRIX GROUP MEANS                CASE **
C               *******************************************************
C
 7600 CONTINUE
C
      IF(NR1.EQ.N2)GOTO7609
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7601)
 7601 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7602)
 7602 FORMAT('      FOR THE MATRIX GROUP MEANS CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7603)
 7603 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7605)
 7605 FORMAT('      THE NUMBER OF ROWS IN THE GROUP ID VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7606)
 7606 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7607)NR1
 7607 FORMAT('            NUMBER OF ROWS FOR MATRIX             =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7608)N2
 7608 FORMAT('            NUMBER OF ROWS FOR GROUP ID VARIABLE  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7609 CONTINUE
C
      CALL GRPMEA(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,
     1Y2,Y3,INDEX,N2,NK,Y4,IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NK
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *******************************************************
C               **  STEP 77--                                        **
C               **  TREAT THE MATRIX GROUP STANDARD DEVIATIONS  CASE **
C               *******************************************************
C
 7700 CONTINUE
C
      IF(NR1.EQ.N2)GOTO7709
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7701)
 7701 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7702)
 7702 FORMAT('      FOR THE MATRIX GROUP STANDARD DEVIATIONS CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7703)
 7703 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7705)
 7705 FORMAT('      THE NUMBER OF ROWS IN THE GROUP ID VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7706)
 7706 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7707)NR1
 7707 FORMAT('            NUMBER OF ROWS FOR MATRIX             =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7708)N2
 7708 FORMAT('            NUMBER OF ROWS FOR GROUP ID VARIABLE  =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7709 CONTINUE
C
      CALL GRPSD(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,
     1Y2,Y3,INDEX,N2,NK,Y4,IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NK
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 78--                                        **
C     **  TREAT THE MULTIVARIATE NORM RANDOM NUMBERS  CASE **
C     *******************************************************
C
 7800 CONTINUE
C
      IF(N1.EQ.NR2)GOTO7809
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7801)
 7801 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7802)
 7802 FORMAT('      FOR THE MULTIVARIATE NORMAL RANDOM NUMBERS CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7803)
 7803 FORMAT('      THE NUMBER OF ROWS IN THE SIGMA MATRIX MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7805)
 7805 FORMAT('      THE NUMBER OF ROWS IN THE MEAN VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7806)
 7806 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7807)NR1
 7807 FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX       =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7808)N2
 7808 FORMAT('            NUMBER OF ROWS FOR MEAN VARIABLE      =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 7809 CONTINUE
C
      NTEMP=INT(YS3)
      LDSIG=MAXROM
      LTF=.TRUE.
      IFLAG=0
C
      DO7820I=1,NTEMP
        CALL RDMNOR(Y1,YM2,LDSIG,NR2,LTF,Y4,IFLAG,ISEED)
        IF(IFLAG.EQ.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7821)
 7821     FORMAT('***** ERROR IN MATARI--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7822)
 7822     FORMAT('      FOR THE MULTIVARIATE NORMAL RANDOM NUMBERS ',
     1           'CASE,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7823)
 7823     FORMAT('      UNABLE TO COMPUTE THE CHOLESKY DECOMPOSITION ',
     1           'OF THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7824)
 7824     FORMAT('      SIGMA MATRIX.  THIS IMPLIES SIGMA IS NOT ',
     1           'POSITIVE DEFINITE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7825)
 7825     FORMAT('      THE MULTIVARIATE RANDOM NUMBERS WERE NOT ',
     1           'GENERATED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        DO7830J=1,NR2
          YM9(I,J)=Y4(J)
 7830   CONTINUE
 7820 CONTINUE
C
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR2
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 79--                                        **
C     **  TREAT THE MULTINOMIAL  RANDOM NUMBERS       CASE **
C     **  LET M = MULTINOMIAL RANDOM NUMBERS P N NEVENTS   **
C     *******************************************************
C
 7900 CONTINUE
C
      DSUM1=0.0D0
      DO7909I=1,N1
        DSUM1=DSUM1 + DBLE(Y1(I))
        IF(Y1(I).LE.0.0 .OR. Y1(I).GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7911)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7901)
 7901     FORMAT('      THE SPECIFIED PROBABILITIES MUST BE IN ',
     1           'THE INTERVAL (0,1).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7903)I,Y1(I)
 7903     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(DSUM1.GT.1.000001D0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7911)
 7911     FORMAT('***** ERROR IN MULTINOMIAL RANDOM NUMBERS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7013)
 7013     FORMAT('      THE SUM OF THE SPECIFIED PROBABILITIES ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7015)
 7015     FORMAT('      HAS JUST EXCEEDED 1.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 7909 CONTINUE
C
      NTRIAL=INT(YS2+0.5)
      NEVENT=INT(YS3+0.5)
C
      IF(NTRIAL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7911)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7916)
 7916   FORMAT('      THE NUMBER OF TRIALS IS LESS THAN 1.  ',
     1         'NTRIALS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
      IF(NEVENT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7911)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7918)
 7918   FORMAT('      THE NUMBER OF EVENTS IS LESS THAN 1.  ',
     1         'NEVENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NCAT=N1
      IERROR='NO'
C
      DO7920I=1,NEVENT
        CALL MULRAN(NTRIAL,Y1,NCAT,ITEMP1,ISEED,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        DO7930J=1,NCAT
          YM9(I,J)=REAL(ITEMP1(J))
 7930   CONTINUE
 7920 CONTINUE
C
      ITYP9='MATR'
      NR9=NEVENT
      NC9=NCAT
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 79.5--                                      **
C     **  TREAT THE MULTINOMIAL PDF                   CASE **
C     **  LET M = MULTINOMIAL PDF X P                      **
C     *******************************************************
C
 7950 CONTINUE
C
      IERROR='NO'
      IF(N1.NE.N2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7951)
 7951   FORMAT('***** ERROR IN MULTINOMIAL PDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7953)
 7953   FORMAT('      THE NUMBER OF ROWS IN THE NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7955)
 7955   FORMAT('      VECTOR AND THE PROBABILITY OF SUCCESS VECTORS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7956)
 7956   FORMAT('      ARE NOT EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7957)N1
 7957   FORMAT('            NUMBER OF ROWS FOR NUMBER OF SUCCESSES = '
     1        ,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7958)N2
 7958   FORMAT('            NUMBER OF ROWS FOR PROBABILITY OF ',
     1         'SUCCESS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO7960I=1,N1
        IF(Y1(I).GE.0.0)THEN
          Y1(I)=REAL(INT(Y1(I)+0.1))
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7951)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7961)
 7961     FORMAT('      THE NUMBER OF SUCCESSES MUST BE A ',
     1           'NON-NEGATIVE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7963)I,Y1(I)
 7963     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 7960 CONTINUE
C
      DSUM1=0.0D0
      DO7970I=1,N1
        DSUM1=DSUM1 + DBLE(Y2(I))
        IF(Y2(I).LE.0.0 .OR. Y2(I).GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7951)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7971)
 7971     FORMAT('      THE SPECIFIED PROBABILITIES MUST BE IN ',
     1           'THE INTERVAL (0,1).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7973)I,Y2(I)
 7973     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(DSUM1.GT.1.000001D0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7951)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7981)
 7981     FORMAT('      THE SUM OF THE SPECIFIED PROBABILITIES ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7983)
 7983     FORMAT('      HAS JUST EXCEEDED 1.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 7970 CONTINUE
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO7990I=1,N1
        DSUM1=DSUM1+DBLE(Y1(I))
        DSUM2=DSUM2+DBLE(Y2(I))
 7990 CONTINUE
      DN=DSUM1
      DNORM=DSUM2
C
      NTRIAL=INT(DN)
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DLNPDF=DLNGAM(DN+1.0D0)
C
      DO7992I=1,N1
        DLNPDF=DLNPDF - DLNGAM(DBLE(Y1(I) + 1.0D0))
 7992 CONTINUE
      DO7995I=1,N1
        DLNPDF=DLNPDF + DLOG(DBLE(Y2(I))/DNORM)*DBLE(Y1(I))
 7995 CONTINUE
C
      IF(DLNPDF.LT.LOG(CPUMAX))THEN
        DLNPDF=DEXP(DLNPDF)
      ELSE
        WRITE(ICOUT,7998)
 7998   FORMAT('***** WARNING: LOGARITHM OF MULTINOMIAL PDF ',
     1         'RETURNED TO AVOID OVERFLOW.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      SCAL9=REAL(DLNPDF)
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 80--                                        **
C     **  TREAT THE WISHART      RANDOM NUMBERS       CASE **
C     **  LET M = WISHART RANDOM NUMBERS MU SIGMA N        **
C     *******************************************************
C
 8000 CONTINUE
C
      IF(N1.EQ.NR2)GOTO8009
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8001)
 8001 FORMAT('***** ERROR IN MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8002)
 8002 FORMAT('      FOR THE WISHART RANDOM NUMBERS CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8003)
 8003 FORMAT('      THE NUMBER OF ROWS IN THE SIGMA MATRIX MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8005)
 8005 FORMAT('      THE NUMBER OF ROWS IN THE MEAN VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8006)
 8006 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8007)NR2
 8007 FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX       =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8008)N1
 8008 FORMAT('            NUMBER OF ROWS FOR MEAN VARIABLE      =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 8009 CONTINUE
C
      IF(NR2.NE.NC2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8012)
 8012   FORMAT('      FOR WISHART RANDOM NUMBERS,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      THE NUMBER OF ROWS IN THE SIGMA MATRIX MUST ',
     1         'EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      THE NUMBER OF COLUMNS; SUCH WAS NOT THE CASE ',
     1         'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8017)NR1
 8017   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8018)NC1
 8018   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      CALL SPOCO(YM2,MAXROM,NR2,RCOND,Y4,INFO)
C
      IF(INFO.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)
 8021   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8022)
 8022   FORMAT('      FOR MATRIX CHOLESKY DECOMPOSITION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8023)
 8023   FORMAT('      THE INPUT MATRIX IS NOT SINGULAR.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
      WRITE(ICOUT,8061)RCOND
      CALL DPWRST('XXX','TEXT ')
 8061 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE SIGMA MATRIX = ',
     1       E15.7)
      IF(1.0+RCOND.EQ.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8071)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,8072)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
      END IF
 8071 FORMAT('****** ERROR FOR WISHART RANDOM NUMBERS ********')
 8072 FORMAT('       THE SIGMA MATRIX IS SINGULAR')
C
      ICOUNT=0
      DO8080I=1,NR2
      DO8082J=I,NC2
        IF(J.GE.I)THEN
          ICOUNT=ICOUNT+1
          Y2(ICOUNT)=YM2(I,J)
        ENDIF
 8082 CONTINUE
 8080 CONTINUE
C
C
      NTEMP=INT(YS3)
      NP=NR2
      NNP=NP*(NP+1)/2
C
      CALL WSHRT(Y2,NTEMP,NP,NNP,Y3,Y4,ISEED)
C
      ICOUNT=0
      DO8090J=1,NP
        DO8092I=1,NP
          IF(I.LE.J)THEN
            ICOUNT=ICOUNT+1
            YM9(I,J)=Y4(ICOUNT)
            IF(I.NE.J)YM9(J,I)=YM9(I,J)
          ENDIF
 8092   CONTINUE
 8090 CONTINUE
C
      ITYP9='MATR'
      NR9=NP
      NC9=NP
      IUPFLG='FULL'
      GOTO9000
C
C               ***********************************************
C               **  STEP 81--                                **
C               **  TREAT THE CATCHER MATRIX CASE            **
C               **  C = X(X'X)**(-1)                         **
C               ***********************************************
C
 8100 CONTINUE
C
      CALL CATCHR(YM1,YM2,YM9,Y1,Y2,INDEX,
     1MAXROM,MAXCOM,NR1,NC1,
     1IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ***********************************************
C               **  STEP 82--                                **
C               **  TREAT THE (X'X)**(-1) MATRIX CASE        **
C               **  C = X(X'X)**(-1)                         **
C               ***********************************************
C
 8200 CONTINUE
C
      CALL XTXINV(YM1,YM9,Y1,Y2,INDEX,
     1MAXROM,MAXCOM,NR1,NC1,
     1IBUGA3,IERROR)
C
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 83--                                 **
C               **  TREAT THE VARIANCE INFLATION FACTORS CASE **
C               ************************************************
C
 8300 CONTINUE
C
      CALL CATCHR(YM1,YM2,YM9,Y1,Y2,INDEX,
     1MAXROM,MAXCOM,NR1,NC1,
     1IBUGA3,IERROR)
C
      DO8310J=1,NC1
        DSUM1=0.0D0
        DSUM2=0.0D0
        DO8320I=1,NR1
          DSUM1=DSUM1 + DBLE(YM9(I,J))**2
          DSUM2=DSUM2 + DBLE(YM1(I,J))
 8320   CONTINUE
        DMEAN=DSUM2/DBLE(NR1)
        DSUM2=0.0D0
        DO8330I=1,NR1
          DSUM2=DSUM2 + (DBLE(YM1(I,J)) - DMEAN)**2
 8330   CONTINUE
        VECT9(J)=REAL(DSUM1*DSUM2)
 8310 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ***********************************************
C               **  STEP 84--                                **
C               **  TREAT THE CONDITION INDICES CASE         **
C               **  (USEFUL FOR REGRESSION DIAGNOSTICS)      **
C               ***********************************************
C
 8400 CONTINUE
C
C  SCALE DESIGN MATRIX
C
      DO8410J=1,NC1
        DSUM1=0.0D0
        DO8420I=1,NR1
          DSUM1=DSUM1 + DBLE(YM1(I,J))*DBLE(YM1(I,J))
 8420   CONTINUE
        DSUM1=DSQRT(DSUM1)
        DO8430I=1,NR1
          YM1(I,J)=YM1(I,J)/REAL(DSUM1)
 8430   CONTINUE
 8410 CONTINUE
C
C  COMPUTE SINGULAR VALUES OF SCALED MATRIX
C
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y1,YM1,MAXROM,
     1YM1,MAXROM,Y2,IJOB,IERR2)
C
      DO8440I=1,NC1
        VECT9(I)=VECT9(I)*VECT9(I)
 8440 CONTINUE
C
      CALL MAXIM(VECT9,NC1,IWRITE,XMAX,IBUGA3,IERROR)
      DO8450I=1,NC1
        IF(VECT9(I).NE.0.0)THEN
          VECT9(I)=XMAX/VECT9(I)
        ELSE
          VECT9(I)=0.0
        ENDIF
 8450 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ***********************************************
C               **  STEP 85--                                **
C               **  TREAT THE CREATE MATRIX  CASE            **
C               **  LET M = CREATE MATRIX V1 V2 ... VK       **
C               **  NOTE: MOST OF THE REAL WORK OF THIS      **
C               **  FUNCTION ACTUALLY DONE IN DPMAT2, HERE   **
C               **  SIMPLY DOING A MATRIX COPY.              **
C               ***********************************************
C
 8500 CONTINUE
C
      DO8510J=1,NC1
        DO8520I=1,NR1
          YM9(I,J)=YM1(I,J)
 8520   CONTINUE
 8510 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               ****************************************************
C               **  STEP 85B-                                     **
C               **  TREAT THE GENERATE MATRIX  CASE               **
C               **  LET M = GENERATE MATRIX <STAT> V1 V2 ... VK   **
C               **  NOTE: MOST OF THE REAL WORK OF THIS           **
C               **  FUNCTION ACTUALLY DONE IN DPMAT2, HERE        **
C               **  SIMPLY DOING A MATRIX COPY.                   **
C               ****************************************************
C
 8550 CONTINUE
C
      DO8560J=1,NC1
        DO8570I=1,NR1
          YM9(I,J)=YM1(I,J)
 8570   CONTINUE
 8560 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C     *********************************************************************
C     **  STEP 86--                                                      **
C     **  TREAT THE INDEPENDENT UNIFORM RANDOM NUMBERS  CASE             **
C     **    LET M = INDEPENDENT UNIFORM RANDOM NUMBER LOWLIM UPPLIM NP   **
C     *********************************************************************
C
 8600 CONTINUE
C
      NROW=INT(YS3 + 0.1)
      NCOL=N1
C
      DO8620J=1,NCOL
        ATEMP1=AMIN1(Y1(J),Y2(J))
        ATEMP2=ABS(Y2(J)-Y1(J))
        CALL UNIRAN(NROW,ISEED,Y4)
        DO8630I=1,NROW
          YM9(I,J)=ATEMP1 + ATEMP2*Y4(I)
 8630   CONTINUE
 8620 CONTINUE
C
      ITYP9='MATR'
      NR9=NROW
      NC9=NCOL
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 87--                                        **
C     **  TREAT THE MULTIVARIATE NORMAL CDF           CASE **
C     *******************************************************
C
 8700 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8701)
 8701   FORMAT('***** ERROR IN MULTIVARIATE NORMAL CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8702)
 8702   FORMAT('      FOR THE MULTIVARIATE NORMAL CDF CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8703)
 8703   FORMAT('      CORRELATION MATRIX MUST BE SQUARE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8706)
 8706   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8707)NR1
 8707   FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX       =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8708)NC1
 8708   FORMAT('            NUMBER OF COLUMNS FOR SIGMA MATRIX    =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        N=NR1
      ENDIF
C
      IF(N3.EQ.0)THEN
        IF(N2.NE.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8711)
 8711     FORMAT('***** ERROR IN MULTIVARIATE NORMAL CDF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8712)
 8712     FORMAT('      FOR THE MULTIVARIATE NORMAL CDF CASE, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8713)
 8713     FORMAT('      NUMBER OF ROWS FOR THE UPPER LIMIT VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8714)
 8714     FORMAT('      NUMBER OF ROWS/COLUMNS FOR THE SIGMA MATRRIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8716)
 8716     FORMAT('      SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8717)NR1
 8717     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',
     1           '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8718)N2
 8718     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        IF(N2.NE.N .OR. N3.NE.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8721)
 8721     FORMAT('***** ERROR IN MULTIVARIATE NORMAL CDF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8722)
 8722     FORMAT('      FOR THE MULTIVARIATE NORMAL CDF CASE, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8723)
 8723     FORMAT('      NUMBER OF ROWS FOR THE UPPER LIMIT VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8724)
 8724     FORMAT('      NUMBER OF ROWS/COLUMNS FOR THE SIGMA MATRRIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8726)
 8726     FORMAT('      SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8727)NR1
 8727     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',
     1           '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8728)N2
 8728     FORMAT('            NUMBER OF ROWS FOR THE LOWER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8729)N3
 8729     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(N.LT.1 .OR. N .GT.20)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8731)
 8731   FORMAT('***** ERROR IN MULTIVARIATE NORMAL CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8732)
 8732   FORMAT('      CORRELATION MATRIX HAS LESS THAN ONE OR MORE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8733)N
 8733   FORMAT('     THAN 20 VARIABLES.   NUMBER OF VARIABLES = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO8741I=1,N
        DTEMP1(I)=0.0D0
        DTEMP2(I)=0.0D0
        DTEMP3(I)=0.0D0
 8741 CONTINUE
      ICNT=0
      DO8760J=1,N
        DO8765I=1,N
          IF(J.LT.I)THEN
            ICNT=ICNT+1
            INDX=J + ((I-2)*(I-1))/2
            DTEMP1(INDX)=DBLE(YM1(I,J))
          ENDIF
 8765   CONTINUE
 8760 CONTINUE
C
      IF(N3.EQ.0)THEN
        DO8770I=1,N
          ITEMP1(I)=0
          DTEMP3(I)=DBLE(Y2(I))
          DTEMP2(I)=DBLE(Y2(I))
 8770   CONTINUE
      ELSE
        DO8775I=1,N
          ITEMP1(I)=2
          DTEMP2(I)=DBLE(Y2(I))
          DTEMP3(I)=DBLE(Y3(I))
          IF(Y2(I).EQ.CPUMIN.AND.Y3(I).EQ.CPUMAX)THEN
            ITEMP1(I)=-1
            DTEMP2(I)=0.0D0
            DTEMP3(I)=0.0D0
          ELSEIF(Y2(I).EQ.CPUMIN)THEN
            ITEMP1(I)=0
            DTEMP2(I)=DBLE(Y3(I))
            DTEMP3(I)=DBLE(Y3(I))
          ELSEIF(Y3(I).EQ.CPUMAX)THEN
            ITEMP1(I)=1
            DTEMP3(I)=DBLE(Y2(I))
            DTEMP2(I)=DBLE(Y2(I))
          ENDIF
 8775   CONTINUE
      ENDIF
C
      MAXPTS=5000*N*N*N
CCCCC ABSEPS=0.00005D0
CCCCC RELEPS=0.0D0
      ABSEPS=DBLE(ABSE)
      RELEPS=DBLE(RELE)
      VALS=0.0D0
      ERRS=0.0D0
      IFTS=0
C
      IF(IMVNTY.EQ.'SADM')THEN
        CALL SADMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'RANM')THEN
        CALL RANMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'KROM')THEN
        CALL KROMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'SPHM')THEN
        CALL SPHMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSE
        CALL SADMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ENDIF
C
      IF(IFTS.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8791)
 8791   FORMAT('***** WARNING IN MULTIVARIATE NORMAL CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8792)ABSEPS
 8792   FORMAT('      ERROR IS GREATER THAN REQUESTED VALUE OF ',
     1         E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ITYP9='SCAL'
      SCAL9=REAL(VALS)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      AERROR=ERRS
      GOTO9000
C
C     *******************************************************
C     **  STEP 88--                                        **
C     **  TREAT THE MULTIVARIATE T      CDF           CASE **
C     *******************************************************
C
 8800 CONTINUE
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8801)
 8801   FORMAT('***** ERROR IN MULTIVARIATE T CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8802)
 8802   FORMAT('      FOR THE MULTIVARIATE T CDF CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8803)
 8803   FORMAT('      CORRELATION MATRIX MUST BE SQUARE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8806)
 8806   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8807)NR1
 8807   FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX       =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8808)NC1
 8808   FORMAT('            NUMBER OF COLUMNS FOR SIGMA MATRIX    =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        N=NR1
      ENDIF
C
      IF(N4.EQ.0)THEN
        IF(N3.NE.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8811)
 8811     FORMAT('***** ERROR IN MULTIVARIATE T CDF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8812)
 8812     FORMAT('      FOR THE MULTIVARIATE T CDF CASE, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8813)
 8813     FORMAT('      NUMBER OF ROWS FOR THE UPPER LIMIT VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8814)
 8814     FORMAT('      NUMBER OF ROWS/COLUMNS FOR THE SIGMA MATRRIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8816)
 8816     FORMAT('      SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8817)NR1
 8817     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',
     1           '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8818)N3
 8818     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        IF(N3.NE.N .OR. N4.NE.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8821)
 8821     FORMAT('***** ERROR IN MULTIVARIATE T CDF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8822)
 8822     FORMAT('      FOR THE MULTIVARIATE T CDF CASE, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8823)
 8823     FORMAT('      NUMBER OF ROWS FOR THE UPPER LIMIT VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8824)
 8824     FORMAT('      NUMBER OF ROWS/COLUMNS FOR THE SIGMA MATRRIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8826)
 8826     FORMAT('      SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8827)NR1
 8827     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',
     1           '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8828)N3
 8828     FORMAT('            NUMBER OF ROWS FOR THE LOWER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8829)N4
 8829     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',
     1           'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(N.LT.1 .OR. N .GT.20)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8831)
 8831   FORMAT('***** ERROR IN MULTIVARIATE T CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8832)
 8832   FORMAT('      CORRELATION MATRIX HAS LESS THAN ONE OR MORE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8833)N
 8833   FORMAT('     THAN 20 VARIABLES.   NUMBER OF VARIABLES = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NU=INT(YS2+0.1)
C
      DO8841I=1,N
        DTEMP1(I)=0.0D0
        DTEMP2(I)=0.0D0
        DTEMP3(I)=0.0D0
 8841 CONTINUE
      ICNT=0
      DO8860J=1,N
        DO8865I=1,N
          IF(J.LT.I)THEN
            ICNT=ICNT+1
            INDX=J + ((I-2)*(I-1))/2
            DTEMP1(INDX)=DBLE(YM1(I,J))
          ENDIF
 8865   CONTINUE
 8860 CONTINUE
C
      IF(N4.EQ.0)THEN
        DO8870I=1,N
          ITEMP1(I)=0
          DTEMP3(I)=DBLE(Y3(I))
          DTEMP2(I)=DBLE(Y3(I))
 8870   CONTINUE
      ELSE
        DO8875I=1,N
          ITEMP1(I)=2
          DTEMP2(I)=DBLE(Y3(I))
          DTEMP3(I)=DBLE(Y4(I))
          IF(Y3(I).EQ.CPUMIN.AND.Y4(I).EQ.CPUMAX)THEN
            ITEMP1(I)=-1
            DTEMP2(I)=0.0D0
            DTEMP3(I)=0.0D0
          ELSEIF(Y3(I).EQ.CPUMIN)THEN
            ITEMP1(I)=0
            DTEMP2(I)=DBLE(Y4(I))
            DTEMP3(I)=DBLE(Y4(I))
          ELSEIF(Y3(I).EQ.CPUMAX)THEN
            ITEMP1(I)=1
            DTEMP3(I)=DBLE(Y3(I))
            DTEMP2(I)=DBLE(Y3(I))
          ENDIF
 8875   CONTINUE
      ENDIF
C
      MAXPTS=5000*N*N*N
CCCCC ABSEPS=0.00005D0
CCCCC RELEPS=0.0D0
      ABSEPS=DBLE(ABSE)
      RELEPS=DBLE(RELE)
      VALS=0.0D0
      ERRS=0.0D0
      IFTS=0
C
      IF(IMVNTY.EQ.'SADM')THEN
        CALL SADMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'RANM')THEN
        CALL RANMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'KROM')THEN
        CALL KROMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSE
        CALL SADMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,
     1              MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ENDIF
C
      IF(IFTS.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8891)
 8891   FORMAT('***** WARNING IN MULTIVARIATE T CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8892)ABSEPS
 8892   FORMAT('      ERROR IS GREATER THAN REQUESTED VALUE OF ',
     1         E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ITYP9='SCAL'
      SCAL9=REAL(VALS)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C     *******************************************************
C     **  STEP 89--                                        **
C     **  TREAT THE MULTIVARIATE T    RANDOM NUMBERS  CASE **
C     *******************************************************
C
 8900 CONTINUE
C
      IF(N1.EQ.NR2)GOTO8909
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8901)
 8901 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8902)
 8902 FORMAT('      FOR THE MULTIVARIATE T RANDOM NUMBERS CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8903)
 8903 FORMAT('      THE NUMBER OF ROWS IN THE SIGMA MATRIX MUST EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8905)
 8905 FORMAT('      THE NUMBER OF ROWS IN THE MEAN VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8906)
 8906 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8907)NR1
 8907 FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX       =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8908)N2
 8908 FORMAT('            NUMBER OF ROWS FOR MEAN VARIABLE      =',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 8909 CONTINUE
C
      NTEMP=INT(YS4)
      LDSIG=MAXROM
      LTF=.TRUE.
C
      DO8920I=1,NTEMP
        CALL RDMNOR(Y1,YM2,LDSIG,NR2,LTF,Y4,IFLAG,ISEED)
        DO8930J=1,NR2
          YM9(I,J)=Y4(J)
 8930   CONTINUE
 8920 CONTINUE
C
C  NOW DIVIDE BY SQRT(CHIRAN(NU)/NU)
C
      NU=INT(YS3+0.1)
      DO8940J=1,NR2
        CALL CHSRAN(NTEMP,REAL(NU),ISEED,Y4)
        DO8945I=1,NTEMP
          YM9(I,J)=YM9(I,J)/SQRT(Y4(I)/REAL(NU))
 8945   CONTINUE
 8940 CONTINUE
C
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR2
      IUPFLG='FULL'
      GOTO9000
C
C     *****************************************************************
C     **  STEP 89.5--                                                **
C     **  TREAT THE DIRICHLET RANDOM NUMBERS  CASE                   **
C     **    LET M = DIRICHLET RANDOM NUMBER ALPHA N                  **
C     *****************************************************************
C
 8950 CONTINUE
C
      NTEMP=INT(YS2 + 0.1)
      NRAN=1
C
      DO8959J=1,N1
        IF(Y1(J).LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8951)
 8951     FORMAT('***** ERROR FOR DIRICHLET RANDOM NUMBERS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8953)
 8953     FORMAT('      THE SHAPE PARAMETERS FOR THE DIRICHLET')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8954)
 8954     FORMAT('      MUST BE POSITIVE.  AT LEAST ONE OF THE SHAPE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8955)
 8955     FORMAT('      PARAMETERS IS NOT POSITIVE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 8959 CONTINUE
C
      IF(NTEMP.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8961)
 8961   FORMAT('***** ERROR FOR DIRICHLET RANDOM NUMBERS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8963)
 8963   FORMAT('      THE REQUESTEND NUMBER OF ROWS MUST BE AT LEAST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8965)
 8965   FORMAT('      ONE.  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NRAN=1
      DO8970I=1,NTEMP
        DSUM=0.0D0
        DO8980J=1,N1
          CALL GAMRAN(NRAN,Y1(J),ISEED,Y4(J))
          DSUM=DSUM+DBLE(Y4(J))
 8980   CONTINUE
        DO8985J=1,N1
          YM9(I,J)=REAL(DBLE(Y4(J))/DSUM)
 8985   CONTINUE
 8970 CONTINUE
C
      ITYP9='MATR'
      NR9=NTEMP
      NC9=N1
      IUPFLG='FULL'
      GOTO9000
C
C     *****************************************************************
C     **  STEP 93-- -                                                **
C     **  TREAT THE DIRICHLET PDF             CASE                   **
C     **    LET M = DIRICHLET PDF X THETA                            **
C     **    LET M = DIRICHLET LOG PDF X THETA                        **
C     *****************************************************************
C
 9300 CONTINUE
C
      IERROR='NO'
      IF(N1.NE.N2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9351)
 9351   FORMAT('***** ERROR IN DIRICHELET PDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9353)
 9353   FORMAT('      THE NUMBER OF ROWS IN THE X VECTOR AND THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9355)
 9355   FORMAT('      ALPHA VECTOR ARE NOT EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9357)N1
 9357   FORMAT('            NUMBER OF ROWS FOR THE X VECTOR     = ',
     1        I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9358)N2
 9358   FORMAT('            NUMBER OF ROWS FOR THE ALPHA VECTOR = ',
     1        I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO9360I=1,N1
        DSUM1=DSUM1+DBLE(Y2(I)-1.0)*DBLE(LOG(Y1(I)))
 9360 CONTINUE
      DLNPDF=DSUM1
C
      DO9370I=1,N1
        DSUM2=DSUM2 + DBLE(Y2(I))
 9370 CONTINUE
      DLNPDF=DLNPDF + DLNGAM(DSUM2)
      DO9380I=1,N1
        DLNPDF=DLNPDF - DLNGAM(DBLE(Y2(I)))
 9380 CONTINUE
C
      SCAL9=REAL(DLNPDF)
      IF(IMCASE.EQ.'DPDF')THEN
        SCAL9=EXP(SCAL9)
      ENDIF
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C     ***********************************************************
C     **  STEP 94--                                            **
C     **  TREAT THE UNIFORM           RANDOM NUMBERS  CASE     **
C     **  (CORRELATED CASE)                                    **
C     **  LET M = MULTIVARIATE UNIFORM RANDOM NUMBERS SIGMA N  **
C     **  ALGORITHM FROM GENTLE (2003), 'RANDOM NUMBER         **
C     **  GENERATION AND MONTE CARLO METHODS', 2ND. ED., P. 207**
C     **  GENERATE NORMAL RANDOM NUMBERS AND THEN TAKE NORCDF  **
C     **  OF THOSE NUMBERS.  NOTE THAT THE LOCATION PARAMETER  **
C     **  IS ASSUMED TO BE ZERO.                               **
C     ***********************************************************
C
 9400 CONTINUE
C
      NTEMP=INT(YS2)
      LDSIG=MAXROM
      LTF=.TRUE.
C
      DO9410I=1,NR1
        Y1(I)=0.0
 9410 CONTINUE
C
      DO9420I=1,NTEMP
        CALL RDMNOR(Y1,YM1,LDSIG,NR1,LTF,Y4,IFLAG,ISEED)
        DO9430J=1,NR1
          CALL NORCDF(Y4(J),YM9(I,J))
 9430   CONTINUE
 9420 CONTINUE
C
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 95--                                      **
C               **  TREAT THE MATRIX PARTITION STATISTIC     CASE  **
C               *****************************************************
C
C  THIS COMMAND SPLITS THE FULL MATRIX INTO SUB-PARTITIONS
C  (DETERMINED BY NROWPA AND NCOLPA) AND CREATE A NEW MATRIX
C  CONTAINING THE COMPUTED STATISTIC FOR EACH OF THESE SUB-MATRICES.
C
C  TWO CASES ARE SUPPORTED:
C
C  1) IF THE SECOND AND THIRD ARGUMENTS ARE BOTH SCALAR, THEN
C     EXTRACT EQUI-SIZED PARTITIONS.
C
C  2) IF EITHER THE SECOND OR THIRD ARGUMENT IS A VECTOR, THEN
C     EXTRACT UNEQUAL PARTITIONS.  THE VECTOR IS TREATED AS A
C     TAG VARIABLE WHICH IDENTIFIES THE SUB-MATRICES.  WITH THIS
C     APPROACH, THE SUB-MATRICES DO NOT NEED TO BE OF EQUAL SIZE
C     AND DO NOT NEED TO DEFINE CONTIGUOUS SUBSETS.
C
 9500 CONTINUE
C
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
C
      NROWPA=INT(ABS(YS2+0.5))
      NCOLPA=INT(ABS(YS3+0.5))
      IF(N2.LE.0 .AND. N3.LE.0)THEN
C
        IF(NROWPA.EQ.0)NROWPA=2
        IF(NCOLPA.EQ.0)NCOLPA=2
C
        IROW=0
        ICOL=0
        DO9510I=1,NC1,NCOLPA
          ICOL=ICOL+1
          ICOL1=I
          ICOL2=I+NCOLPA-1
          IF(ICOL2.GT.NC1)ICOL2=NC1
          IROW=0
          DO9515J=1,NR1,NROWPA
            IROW=IROW+1
            IROW1=J
            IROW2=J+NROWPA-1
            IF(IROW2.GT.NR1)IROW2=NR1
            III=0
            DO9520II=ICOL1,ICOL2
              DO9530JJ=IROW1,IROW2
                III=III+1
                NTEMP=III
                Y1(III)=YM1(JJ,II)
 9530         CONTINUE
 9520       CONTINUE
            ASTAT=0.0
            CALL CMPSTA(
     1    Y1,Y2,Y2,Y3,Y4,Y5,MAXNXT,NTEMP,NTEMP,NTEMP,NUMV2,ICASS7,
     1    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1    DTEMP1,DTEMP2,DTEMP3,
CCCCC1    IQUAME,IQUASE,PSTAMV,
     1    ASTAT,
     1    ISUBRO,IBUGA3,IERROR)
            YM9(IROW,ICOL)=ASTAT
 9515     CONTINUE
 9510   CONTINUE
C
        ITYP9='MATR'
        NR9=IROW
        NC9=ICOL
        IUPFLG='FULL'
        GOTO9000
C
      ELSE
C
        IF(N2.GE.1)THEN
          IF(N2.NE.NR1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9551)
 9551       FORMAT('***** ERROR IN MATRIX PARTITION <STATISTIC>--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9552)N2
 9552       FORMAT('      THE NUMBER OF ELEMENTS IN THE ROW VECTOR ',
     1             '= ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9553)NR1
 9553       FORMAT('      WHILE THE NUMBER OF ROWS IN THE MATRIX = ',
     1             I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          CALL DISTIN(Y2,N2,IWRITE,Y5,NROWPA,IBUGA3,IERROR)
          DO9557I=1,NROWPA
            DTEMP1(I)=DBLE(Y5(I))
 9557     CONTINUE
        ELSE
          NROWPA=1
          DTEMP1(1)=1.0D0
          DO9558I=1,NR1
            Y2(I)=1.0
 9558     CONTINUE
        ENDIF
C
        IF(N3.GE.1)THEN
          IF(N3.NE.NC1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9561)
 9561       FORMAT('***** ERROR IN MATRIX PARTITION <STATISTIC>--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9562)N2
 9562       FORMAT('      THE NUMBER OF ELEMENTS IN THE COLUMN ',
     1             'VECTOR = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9563)NC1
 9563       FORMAT('      WHILE THE NUMBER OF COLUMNS IN THE ',
     1             'MATRIX = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          CALL DISTIN(Y3,N3,IWRITE,Y5,NCOLPA,IBUGA3,IERROR)
          DO9567I=1,NCOLPA
            DTEMP2(I)=DBLE(Y5(I))
 9567     CONTINUE
        ELSE
          NCOLPA=1
          DTEMP2(1)=1.0D0
          DO9568I=1,NC1
            Y3(I)=1.0
 9568     CONTINUE
        ENDIF
C
        DO9571IROW=1,NROWPA
          AROW=REAL(DTEMP1(IROW))
          DO9572ICOL=1,NCOLPA
            ACOL=REAL(DTEMP2(ICOL))
C
            NTEMP=0
            DO9580JJ=1,NC1
              DO9590II=1,NR1
                IF(AROW.EQ.Y2(II) .AND. ACOL.EQ.Y3(JJ))THEN
                  NTEMP=NTEMP+1
                  Y1(NTEMP)=YM1(II,JJ)
                ENDIF
 9590         CONTINUE
 9580       CONTINUE
            IF(NTEMP.GE.1)THEN
              ASTAT=0.0
              CALL CMPSTA(
     1        Y1,Y5,Y5,YM2(1,1),YM2(1,2),YM2(1,3),MAXNXT,
     1        NTEMP,NTEMP,NTEMP,NUMV2,ICASS7,
     1        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1        DTEMP1,DTEMP2,DTEMP3,
CCCCC1        IQUAME,IQUASE,PSTAMV,
     1        ASTAT,
     1        ISUBRO,IBUGA3,IERROR)
              YM9(IROW,ICOL)=ASTAT
            ELSE
              YM9(IROW,ICOL)=0.0
            ENDIF
 9572     CONTINUE
 9571   CONTINUE
C
        ITYP9='MATR'
        NR9=NROWPA
        NC9=NCOLPA
        IUPFLG='FULL'
        GOTO9000
C
      ENDIF
C
C               *****************************************************
C               **  STEP 96--                                      **
C               **  TREAT THE MATRIX STATISTIC               CASE  **
C               *****************************************************
C
C  THIS COMMAND COMPUTES A SPECIFIED STATISTIC FOR THE ENTIRE MATRIX.
C
C  NOTE 3/2007: ADD CRAMER CONTINGENCY COEFFICIENT AND
C               PEARSON CONTINGENCY COEFFICIENT.  THESE WORK
C               DIFFERENTLY THAN THE OTHER STATISTICS IN THAT
C               THEY ARE INTERPRETED AS RXC TABLES RATHER THAN
C               ONE ARRAY CONTAINING ALL THE MATRIX OBSERVATIONS.
C
 9600 CONTINUE
C
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
C
      IF(ICASS7.EQ.'CRAM')THEN
         CALL CRAME2(YM1,MAXROM,NR1,NC1,IWRITE,Y1,ASTAT,
     1               IBUGA3,IERROR)
         GOTO9699
      ELSEIF(ICASS7.EQ.'PEAR')THEN
         CALL PEARC2(YM1,MAXROM,NR1,NC1,IWRITE,Y1,ASTAT,
     1               IBUGA3,IERROR)
         GOTO9699
      ENDIF
C
      ICNT=0
      DO9610I=1,NC1
        DO9620J=1,NR1
          ICNT=ICNT+1
          IF(ICNT.GT.MAXOBV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9611)
 9611       FORMAT('***** ERROR FROM MATRIX STATISTIC--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9613)MAXOBV
 9613       FORMAT('      THE NUMBER OF ELEMENTS IS GREATER THAN ',
     1             I10)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          Y1(ICNT)=YM1(I,J)
 9620   CONTINUE
 9610 CONTINUE
      ASTAT=0.0
      CALL CMPSTA(
     1    Y1,Y2,Y2,Y3,Y4,Y5,MAXNXT,ICNT,ICNT,ICNT,NUMV2,ICASS7,
     1    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1    DTEMP1,DTEMP2,DTEMP3,
CCCCC1    IQUAME,IQUASE,PSTAMV,
     1    ASTAT,
     1    ISUBRO,IBUGA3,IERROR)
C
 9699 CONTINUE
      SCAL9=ASTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 97--                                      **
C               **  TREAT THE MATRIX BIN                     CASE  **
C               *****************************************************
C
C  THIS COMMAND BINS THE DATA IN A MATRIX (I.E., USEFUL FOR
C  GENERATING A HISTOGRAM OF ALL THE POINTS IN THE MATRIX.
C
 9700 CONTINUE
C
      IWRITE='OFF'
      MAXNXT=MAXOBV
C
      ICNT=0
      DO9710I=1,NC1
        DO9720J=1,NR1
          ICNT=ICNT+1
          IF(ICNT.GT.MAXOBV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9711)
 9711       FORMAT('***** ERROR FROM MATRIX STATISTIC--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9713)MAXOBV
 9713       FORMAT('      THE NUMBER OF ELEMENTS IS GREATER THAN ',
     1             I10)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          Y1(ICNT)=YM1(J,I)
 9720   CONTINUE
 9710 CONTINUE
C
      CALL DPBIN(Y1,ICNT,IRELAT,CLWID,XSTART,XSTOP,IRHSTG,
     1           Y4,MAXNXT,IHSTCW,IHSTOU,
     1           Y2,Y3,N2,IBUGA3,IERROR)
C
      ITYP9='VECT'
      NVECT9=N2
      IUPFLG='FULL'
      DO9760I=1,NVECT9
        VECT9(I)=Y2(I)
        Y2(I)=Y3(I)
 9760 CONTINUE
      GOTO9000
C
C               *****************************************************
C               **  STEP 98--                                      **
C               **  TREAT THE MINIMAL SPANNING TREE          CASE  **
C               **  STEP 1:  CREATE A DISTANCE MATRIX FROM THE     **
C               **           TWO INPUT VECTORS (THE (X,Y)          **
C               **           COORDINATES)                          **
C               **  STEP 2:  CALL MINSPT TO COMPUTE THE EDGES OF   **
C               **           THE MINIMAL SPANNING TREE             **
C               **  STEP 3:  CONVERT THESE EDGES TO A LIST OF      **
C               **           VERTICES THAT CAN BE EASILY PLOTTED   **
C               *****************************************************
C
 9800 CONTINUE
C
C     STEP 1: COMPUTE A DISTANCE MATRIX
C
      IF(N1.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9801)
 9801   FORMAT('***** ERROR IN MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9803)
 9803   FORMAT('      FOR THE MINIMAL SPANNING TREE, UNABLE TO COMPUTE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9805)
 9805   FORMAT('      DISTANCE MATRIX (TOO MANY POINTS).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9806)N1
 9806   FORMAT('      THE NUMBER OF VERTICES          = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9807)MAXROM
 9807   FORMAT('      MAXIMUM NUMBER OF ALLOWED ROWS  =  ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO9810I=1,N1
        YM1(I,I)=0.0
        IF(I.EQ.N1)GOTO9810
        AY1=Y1(I)
        AX1=Y2(I)
        DO9820J=I+1,N1
          AY2=Y1(J)
          AX2=Y2(J)
          ADIST=SQRT((AX1 - AX2)**2 + (AY1 - AY2)**2)
          IF(ADIST.LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9801)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9823)I,J
 9823       FORMAT('      FOR ROW ',I8,' AND COLUMN ',I8,' THE ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9825)
 9825       FORMAT('      COMPUTED DISTANCE IS ZERO.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          YM1(I,J)=ADIST
          YM1(J,I)=ADIST
 9820   CONTINUE
 9810   CONTINUE
        NR1=N1
C
C     STEP 2: COMPUTE THE EDGES OF THE MINIMAL SPANNING TREE
C
      CALL MINSPT(YM1,MAXROM,NR1,ITEMP1,ITEMP2,ITEMP3,Y3)
C
C     STEP 3: CONVERT TO A LIST OF VERTICES.  NOTE THAT THERE ARE
C             N-1 EDGES.  AN EDGE ESSENTIALLY DEFINES TWO VERTICES.
C             WE WILL ALSO DEFINED A "TAG" VARIABLE (THIS SIMPLIFIES
C             PLOTTING).
C
      ICNT1=0
      DO9830I=1,NR1-1
        IINDX1=ITEMP1(I)
        IINDX2=ITEMP2(I)
        ICNT1=ICNT1+1
        Y3(ICNT1)=Y1(IINDX1)
        Y4(ICNT1)=Y2(IINDX1)
        ICNT1=ICNT1+1
        Y3(ICNT1)=Y1(IINDX2)
        Y4(ICNT1)=Y2(IINDX2)
 9830 CONTINUE
      NVECT9=ICNT1
C
       DO9840I=1,NVECT9
         VECT9(I)=Y3(I)
         Y2(I)=Y4(I)
 9840  CONTINUE
C
       NTAG=NVECT9/2
       ICNT1=0
       ICNT2=0
       DO9850I=1,NTAG
         ICNT2=ICNT2+1
         ICNT1=ICNT1+1
         Y3(ICNT1)=REAL(ICNT2)
         ICNT1=ICNT1+1
         Y3(ICNT1)=REAL(ICNT2)
 9850  CONTINUE
C
      ITYP9='VECT'
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 99--                                      **
C               **  TREAT THE MINIMAL SPANNING TREE          CASE  **
C               **  FOR THIS VARIANT, WE START WITH A DISTANCE     **
C               **  MATRIX (RATHER THAN THE VERTICES).  THE        **
C               **  DISTANCES MAY IN FACT REFLECT "COSTS" OR       **
C               **  "WEIGHTINGS" AS OPPOSSED TO ACTUAL DISTANCES.  **
C               **  IN THIS CASE, THE RETURNED OUTPUT IS THE       **
C               **  LIST OF EDGES (I.E., WE DO NOT CONVERT BACK    **
C               **  TO ORIGINAL VERTICES).                         **
C               *****************************************************
C
 9900 CONTINUE
C
C     STEP 1: CHECK FOR A SQUARE MATRIX
C
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9901)
 9901   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9902)
 9902   FORMAT('      FOR MINIMUM SPANNING TREE, THE NUMBER OF ROWS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9903)
 9903   FORMAT('      IN THE MATRIX MUST EQUAL THE NUMBER OF COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9905)
 9905   FORMAT('      IN THE MATRIX;  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9907)NR1
 9907   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9908)NC1
 9908   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     STEP 2: CHECK FOR A VALID DISTANCE MATRIX
C
C             A) DIAGONAL ELEMENTS SHOULD BE ZERO
C             B) ALL NON-DIAGONAL ELEMENTS SHOULD BE NON-ZERO
C             C) DIST(I,J) = DIST(J,I)
C
C
      DO9910I=1,N1
C
        IF(ABS(YM1(I,I)).GT.0.1E-12)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9901)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9913)
 9913     FORMAT('      FOR THE MINIMAL SPANNING TREE, A DIAGONAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9915)
 9915     FORMAT('      ELEMENT OF THE DISTANCE MATRIX IS NON-ZERO.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9917)I,YM1(I,I)
 9917     FORMAT('      THE VALUE OF ROW ',I8,'  = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO9920J=I+1,N1
          IF(YM1(I,J).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9901)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9921)I,J
 9921       FORMAT('      ROW ',I8,' AND COLUMN ',I8,' OF THE ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9923)
 9923       FORMAT('      DISTANCE MATRIX IS NON-POSITIVE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9925)
 9925       FORMAT('      THE VALUE IS ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSEIF(YM1(I,J).NE.YM1(J,I))THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9901)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9931)
 9931       FORMAT('      THE DISTANCE MATRIX IS NOT SYMMETRIC.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9933)I,J,YM1(I,J)
 9933       FORMAT('      ROW ',I8,' COLUMN ',I8,'  = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9933)J,I,YM1(J,I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
 9920   CONTINUE
 9910   CONTINUE
        NR1=N1
C
C     STEP 3: COMPUTE THE EDGES OF THE MINIMAL SPANNING TREE
C
      CALL MINSPT(YM1,MAXROM,NR1,ITEMP1,ITEMP2,ITEMP3,Y3)
C
C     STEP 3: CONVERT TO A LIST OF VERTICES.  NOTE THAT THERE ARE
C             N-1 EDGES.  AN EDGE ESSENTIALLY DEFINES TWO VERTICES.
C             WE WILL ALSO DEFINED A "TAG" VARIABLE (THIS SIMPLIFIES
C             PLOTTING).
C
      NVECT9=NR1-1
      DO9950I=1,NVECT9
        VECT9(I)=REAL(ITEMP1(I))
        Y2(I)=REAL(ITEMP2(I))
 9950 CONTINUE
C
      ITYP9='VECT'
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10000--                                   **
C               **  TREAT THE MATRIX RENUMBER                CASE  **
C               *****************************************************
C
C  THIS COMMAND REORDERS THE ROWS (BASED ON Y2) AND COLUMNS
C  (BASED ON Y3) OF A MATRIX.
C
10000 CONTINUE
C
      IWRITE='OFF'
C
C     STEP 1: CHECK Y2 AND Y3
C
      IF(N2.NE.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
10001   FORMAT('***** ERROR FROM MATRIX RENUMBER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10003)
10003   FORMAT('      THE NUMBER OF ELEMENTS IN THE ROW ',
     1         'PERMUATION VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10005)
10005   FORMAT('      IS NOT EQUAL TO THE NUMBER OF ROWS IN ',
     1         'THE MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10007)N2
10007   FORMAT('      NUMBER OF ELEMENTS IN THE ROW PERMUATION ',
     1         'VECTOR    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10009)NR1
10009   FORMAT('      NUMBER OF ROWS IN THE MATRIX             ',
     1         '          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10010I=1,N2
        ITEMP1(I)=INT(Y2(I)+0.1)
        Y4(I)=REAL(ITEMP1(I))
10010 CONTINUE
      CALL DISTIN(Y4,N2,IWRITE,Y5,NDIST,IBUGA3,IERROR)
      CALL MINIM(Y4,N2,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y4,N2,IWRITE,XMAX,IBUGA3,IERROR)
C
      IF(N2.NE.NDIST)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,10001)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,10013)
C10013   FORMAT('      THE VALUES IN THE ROW PERMUTATION ',
CCCCC1         'VECTOR ARE NOT ALL UNIQUE.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
      ELSEIF(XMIN.LT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10023)
10023   FORMAT('      THE MINIMUM VALUE IN THE ROW PERMUTATION ',
     1         'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10025)
10025   FORMAT('      IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10027)XMIN
10027   FORMAT('      THE MINIMUM VALUE IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(XMAX.GT.REAL(N2))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10033)
10033   FORMAT('      THE MAXIMUM VALUE IN THE ROW PERMUTATION ',
     1         'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10035)
10035   FORMAT('      IS GREATER THAN THE NUMBER OF ELEMENTS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10037)XMAX
10037   FORMAT('      THE MAXIMUM VALUE IS      ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10038)N2
10038   FORMAT('      THE NUMBER OF ELEMENTS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N3.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10043)
10043   FORMAT('      THE NUMBER OF ELEMENTS IN THE COLUMN ',
     1         'PERMUATION VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10045)
10045   FORMAT('      IS NOT EQUAL TO THE NUMBER OF COLUMNS IN ',
     1         'THE MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10047)N3
10047   FORMAT('      NUMBER OF ELEMENTS IN THE COLUMN PERMUATION ',
     1         'VECTOR    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10049)NC1
10049   FORMAT('      NUMBER OF COLUMNS IN THE MATRIX             ',
     1         '          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10050I=1,N3
        ITEMP2(I)=INT(Y3(I)+0.1)
        Y4(I)=REAL(ITEMP2(I))
10050 CONTINUE
      CALL DISTIN(Y4,N3,IWRITE,Y5,NDIST,IBUGA3,IERROR)
      CALL MINIM(Y4,N3,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y4,N3,IWRITE,XMAX,IBUGA3,IERROR)
C
      IF(N3.NE.NDIST)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,10001)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,10053)
C10053   FORMAT('      THE VALUES IN THE COLUMN PERMUTATION ',
CCCCC1         'VECTOR ARE NOT ALL UNIQUE.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
      ELSEIF(XMIN.LT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10063)
10063   FORMAT('      THE MINIMUM VALUE IN THE COLUMN PERMUTATION ',
     1         'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10065)
10065   FORMAT('      IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10067)XMIN
10067   FORMAT('      THE MINIMUM VALUE IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(XMAX.GT.REAL(N3))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10073)
10073   FORMAT('      THE MAXIMUM VALUE IN THE ROW PERMUTATION ',
     1         'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10075)
10075   FORMAT('      IS GREATER THAN THE NUMBER OF ELEMENTS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10077)XMAX
10077   FORMAT('      THE MAXIMUM VALUE IS      ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10078)N2
10078   FORMAT('      THE NUMBER OF ELEMENTS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10081I=1,NR1
        DO10082J=1,NC1
          IROW=ITEMP1(I)
          ICOL=ITEMP2(J)
          YM9(IROW,ICOL)=YM1(I,J)
10082   CONTINUE
10081 CONTINUE
C
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10100--                                   **
C               **  TREAT THE ADJACENCY MATRIX               CASE  **
C               *****************************************************
C
C  THIS COMMAND CREATES AN ADJACENCY MATRIX FROM A LIST OF EDGES.
C
10100 CONTINUE
C
      IWRITE='OFF'
      NVERT=INT(YS3+0.1)
      NVERT=MAX(NVERT,N1)
      NVERT=MAX(NVERT,N2)
C
C     STEP 1: CHECK TO SEE IF THE MATRIX WILL FIT
C
      IF(NVERT.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10101)
10101   FORMAT('***** ERROR FROM ADJACENCY MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10103)
10103   FORMAT('      THE NUMBER OF VERTICES EXCEEDS THE ',
     1         'MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10105)
10105   FORMAT('      NUMBER OF ROWS FOR A MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10107)NVERT
10107   FORMAT('      THE NUMBER OF VERTICES                 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10109)MAXROM
10109   FORMAT('      THE MAXIMUM NUMBER OF ROWS IN A MATRIX = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NVERT.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10113)
10113   FORMAT('      THE NUMBER OF VERTICES EXCEEDS THE ',
     1         'MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10115)
10115   FORMAT('      NUMBER OF COLUMNS FOR A MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10117)NVERT
10117   FORMAT('      THE NUMBER OF VERTICES                    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10119)MAXROM
10119   FORMAT('      THE MAXIMUM NUMBER OF COLUMNS IN A MATRIX = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     STEP 2: NOW CREATE THE ADJACENCY MATRIX
C
C
      DO10120J=1,N1
        DO10130I=1,N1
          YM9(I,J)=0.0
10130   CONTINUE
10120 CONTINUE
C
      DO10140I=1,N1
        IROW=INT(Y1(I)+0.1)
C
        IF(IROW.LT.1 .OR. IROW.GT.NVERT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10143)I
10143     FORMAT('      FOR EDGE ',I8,' THE ROW INDEX IS OUT OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10145)IROW
10145     FORMAT('      ROW INDEX = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        ICOL=INT(Y2(I)+0.1)
C
        IF(ICOL.LT.1 .OR. ICOL.GT.NVERT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10153)I
10153     FORMAT('      FOR EDGE ',I8,' THE COLUMN INDEX IS OUT OF ',
     1           'RANGE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10155)ICOL
10155     FORMAT('      COLUMN INDEX = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        YM9(IROW,ICOL)=1.0
        IF(IMCASE.EQ.'ADMA')YM9(ICOL,IROW)=1.0
10140 CONTINUE
C
      ITYP9='MATR'
      NR9=NVERT
      NC9=NVERT
      IUPFLG='FULL'
      GOTO9000
C
C               ************************************************
C               **  STEP 10200--                              **
C               **  TREAT THE MATRIX ROW FIT       CASE       **
C               **  PERFORM A FIT OF EACH ROW OF THE MATRIX   **
C               **  AGAINST A COMMON X VARIABLE.  RIGHT NOW,  **
C               **  LIMIT TO LINEAR FIT (BUT MAYBE ADD        **
C               **  QUADRATIC FIT IN FUTURE).                 **
C               ************************************************
C
CCCCC IMPLEMENTED FEBRUARY 2010.
10200 CONTINUE
C
      IF(N2.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10201)
10201   FORMAT('****** ERROR IN MATRIX ROW FIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10202)NC1
10202   FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX (',I8,') ',
     1         'IS NOT EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10203)N1
10203   FORMAT('      THE NUMBER OF ROWS IN THE X VARIABLE (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10210I=1,NR1
        DO10220J=1,NC1
          Y3(J)=YM1(I,J)
10220   CONTINUE
        ICNT=0
        DO10230J=1,NC1
          IF(Y3(J).EQ.PSTAMV)GOTO10230
          ICNT=ICNT+1
          Y4(ICNT)=Y2(J)
          Y3(ICNT)=Y3(J)
10230   CONTINUE
        NPTS=ICNT
        IF(NPTS.LE.0)THEN
          PPA0=PSTAMV
          PPA1=PSTAMV
          PPA0SD=PSTAMV
          PPA1SD=PSTAMV
        ELSE
          CALL LINFIT(Y3,Y4,NPTS,
     1                PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
        YM9(I,1)=PPA0
        YM9(I,2)=PPA1
        YM9(I,3)=SDPPA0
        YM9(I,4)=SDPPA1
10210 CONTINUE
C
      ITYP9='VECT'
      NR9=1
      NC9=1
      DO10240I=1,NR1
        VECT9(I)=YM9(I,1)
        Y2(I)=YM9(I,2)
        Y3(I)=YM9(I,3)
        Y4(I)=YM9(I,4)
10240 CONTINUE
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *************************************************
C               **  STEP 10300--                               **
C               **  TREAT THE MATRIX COLUMN FIT       CASE     **
C               **  PERFORM A FIT OF EACH COLUMN OF THE MATRIX **
C               **  AGAINST A COMMON X VARIABLE.  RIGHT NOW,   **
C               **  LIMIT TO LINEAR FIT (BUT MAYBE ADD         **
C               **  QUADRATIC FIT IN FUTURE).                  **
C               *************************************************
C
CCCCC IMPLEMENTED FEBRUARY 2010.
10300 CONTINUE
C
      IF(N2.NE.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10301)
10301   FORMAT('****** ERROR IN MATRIX COLUMN FIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10302)NR1
10302   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX (',I8,') ',
     1         'IS NOT EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10303)N1
10303   FORMAT('      THE NUMBER OF ROWS IN THE X VARIABLE (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10310I=1,NC1
        DO10320J=1,NR1
          Y3(J)=YM1(J,I)
10320   CONTINUE
        ICNT=0
        DO10330J=1,NR1
          IF(Y3(J).EQ.PSTAMV)GOTO10330
          ICNT=ICNT+1
          Y4(ICNT)=Y2(J)
          Y3(ICNT)=Y3(J)
10330   CONTINUE
        NPTS=ICNT
        IF(NPTS.LE.0)THEN
          PPA0=PSTAMV
          PPA1=PSTAMV
          PPA0SD=PSTAMV
          PPA1SD=PSTAMV
        ELSE
          CALL LINFIT(Y3,Y4,NPTS,
     1                PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
        YM9(I,1)=PPA0
        YM9(I,2)=PPA1
        YM9(I,3)=SDPPA0
        YM9(I,4)=SDPPA1
10310 CONTINUE
C
      ITYP9='VECT'
      NR9=1
      NC9=1
      DO10340I=1,NC1
        VECT9(I)=YM9(I,1)
        Y2(I)=YM9(I,2)
        Y3(I)=YM9(I,3)
        Y4(I)=YM9(I,4)
10340 CONTINUE
      NVECT9=NC1
      IUPFLG='FULL'
      GOTO9000
C
C               *************************************************
C               **  STEP 10400--                               **
C               **  TREAT THE VARIABLE TO MATRIX      CASE     **
C               *************************************************
C
CCCCC IMPLEMENTED NOVEMBER 2010.
10400 CONTINUE
C
      NROW=INT(YS2+0.1)
      NCOL=N1/NROW
      NREM=N1 - (NROW*NCOL)
      IF(NREM.GT.0)NCOL=NCOL+1
C
      IF(NROW.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10401)
10401   FORMAT('****** ERROR IN VARIABLE TO MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10402)NROW
10402   FORMAT('      THE REQUESTED NUMBER OF ROWS FOR THE MATRIX (',
     1         I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10403)MAXROM
10403   FORMAT('      THE MAXIMUM NUMBER  OF ROWS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCOL.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10411)
10411   FORMAT('****** ERROR IN VARIABLE TO MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10412)NCOL
10412   FORMAT('      THE REQUESTED NUMBER OF COLUMNS FOR THE MATRIX (',
     1         I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10413)MAXCOM
10413   FORMAT('      THE MAXIMUM NUMBER  OF COLUMNS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IVARMA.EQ.'COLU')THEN
        ICNT=0
        DO10420J=1,NCOL
          DO10430I=1,NROW
            ICNT=ICNT+1
            IF(ICNT.LE.N1)THEN
              YM9(I,J)=Y1(ICNT)
            ELSE
              YM9(I,J)=PSTAMV
            ENDIF
10430     CONTINUE
10420   CONTINUE
      ELSE
        ICNT=0
        DO10470I=1,NROW
          DO10480J=1,NCOL
            ICNT=ICNT+1
            IF(ICNT.LE.N1)THEN
              YM9(I,J)=Y1(ICNT)
            ELSE
              YM9(I,J)=PSTAMV
            ENDIF
10480     CONTINUE
10470   CONTINUE
      ENDIF
C
      ITYP9='MATR'
      NR9=NROW
      NC9=NCOL
      IUPFLG='FULL'
      GOTO9000
C
C               *************************************************
C               **  STEP 10500--                               **
C               **  TREAT THE MATRIX TO VARIABLE CASE          **
C               *************************************************
C
CCCCC IMPLEMENTED NOVEMBER 2010.
10500 CONTINUE
C
      IF(IMATVA.EQ.'COLU')THEN
        ICNT=0
        DO10520J=1,NC1
          DO10530I=1,NR1
            ICNT=ICNT+1
            IF(ICNT.LE.MAXOBV)THEN
              VECT9(ICNT)=YM1(I,J)
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,10501)
10501         FORMAT('****** ERROR IN MATRIX TO VARIABLE--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,10502)MAXOBV
10502         FORMAT('      MAXIMUM NUMBER OF ROWS IN VARIABLE (',
     1               I8,') EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
10530     CONTINUE
10520   CONTINUE
      ELSE
        ICNT=0
        DO10570I=1,NR1
          DO10580J=1,NC1
            ICNT=ICNT+1
            IF(ICNT.LE.MAXOBV)THEN
              VECT9(ICNT)=YM1(I,J)
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,10501)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,10502)MAXOBV
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
10580     CONTINUE
10570   CONTINUE
      ENDIF
C
      ITYP9='VECT'
      NVECT9=ICNT
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10600-                                    **
C               **  TREAT THE MATRIX COMBINE ROW             CASE  **
C               *****************************************************
C
10600 CONTINUE
C
      IF(NC1.NE.NC2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10611)
10611   FORMAT('***** ERROR IN MATRIX COMBINE ROW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10613)
10613   FORMAT('      THE NUMBER OF COLUMNS IN THE TWO MATRICES IS ',
     1         'NOT EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10615)NC1
10615   FORMAT('      THE NUMBER OF COLUMNS FOR THE FIRST  MATRIX: ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10617)NC2
10617   FORMAT('      THE NUMBER OF COLUMNS FOR THE SECOND MATRIX: ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10610J=1,NC1
         DO10620I=1,NR1
           YM9(I,J)=YM1(I,J)
10620    CONTINUE
         DO10630I=1,NR2
           IINDX=I+NR1
           IF(IINDX.GT.MAXROM)THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10611)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10633)
10633        FORMAT('      THE MAXIMUM NUMBER OF ROWS FOR THE ',
     1              'OUTPUT MATRIX HAS BEEN EXCEEDED.')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10635)MAXROM
10635        FORMAT('      THE MAXIMUM NUMBER OF ROWS  = ',I8)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10637)NR1+NR2
10637        FORMAT('      THE REQUIRED NUMBER OF ROWS = ',I8)
             CALL DPWRST('XXX','BUG ')
             IERROR='YES'
             GOTO9000
           ENDIF
           YM9(I+NR1,J)=YM2(I,J)
10630    CONTINUE
10610 CONTINUE
C
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1+NR2
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10700-                                    **
C               **  TREAT THE MATRIX COMBINE COLUMN          CASE  **
C               *****************************************************
C
10700 CONTINUE
C
      IF(NR1.NE.NR2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10711)
10711   FORMAT('***** ERROR IN MATRIX COMBINE COLUMN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10713)
10713   FORMAT('      THE NUMBER OF ROWS IN THE TWO MATRICES IS ',
     1         'NOT EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10715)NR1
10715   FORMAT('      THE NUMBER OF ROWS FOR THE FIRST  MATRIX: ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10717)NR2
10717   FORMAT('      THE NUMBER OF ROWS FOR THE SECOND MATRIX: ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO10710I=1,NR1
         DO10720J=1,NC1
           YM9(I,J)=YM1(I,J)
10720    CONTINUE
         DO10730J=1,NC2
           IINDX=I+NC1
           IF(IINDX.GT.MAXCOM)THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10711)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10733)
10733        FORMAT('      THE MAXIMUM NUMBER OF COLUMNS FOR THE ',
     1              'OUTPUT MATRIX HAS BEEN EXCEEDED.')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10735)MAXCOM
10735        FORMAT('      THE MAXIMUM NUMBER OF COLUMNS  = ',I8)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10737)NC1+NC2
10737        FORMAT('      THE REQUIRED NUMBER OF COLUMNS = ',I8)
             CALL DPWRST('XXX','BUG ')
             IERROR='YES'
             GOTO9000
           ENDIF
           YM9(I,J+NC1)=YM2(I,J)
10730    CONTINUE
10710 CONTINUE
C
      ITYP9='MATR'
      NC9=NC1+NC2
      NR9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10800-                                    **
C               **  TREAT THE DEX CORE                       CASE  **
C               *****************************************************
C
10800 CONTINUE
C
      MAXK=25
      NROW=10000
      CALL DPCORE(YM1,NC1,NR1,MAXROM,MAXK,
     1            YM9,ITEMP1,NROW,NUMCOR,
     1            ITEMP2,Y1,Y2,
     1            IBUGA3,ISUBRO,IERROR)
C
      ITYP9='MATR'
      NC9=5
      NR9=NUMCOR
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 10900-                                    **
C               **  TREAT THE DEX CONFOUND                   CASE  **
C               *****************************************************
C
10900 CONTINUE
C
      MAXK=25
      MAX2T=500
      IF(NR1*MAX2T.GT.46*MAXOBV/3)THEN
        WRITE(ICOUT,10901)
10901   FORMAT('***** ERROR IN DEX CONFOUND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10903)
10903   FORMAT('      INSUFFICIENT SPACE TO GENERATE CONFOUNDING ',
     1         'STRUCTURE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
      CALL DPDCF2(YM1,NC1,NR1,MAXK,MAXROM,
     1            YM2,MAX2T,
     1            Y1,Y3,Y4,VECT9,Y2,
     1            INDEX,ITEMP1,
     1            ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,ITEMP7,
     1            STME,STMEC,ST2T,ST2TC,STC,STT,
     1            NUMCON,
     1            IBUGA3,ISUBRO,IERROR)
C
      ITYP9='VECT'
      NVECT9=NUMCON
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 11000-                                    **
C               **  TREAT THE DEX CHECK CLASSIC              CASE  **
C               *****************************************************
C
11000 CONTINUE
C
C     CHECK IF MATRIX IS IN "CLASSIC" FORM FOR 2-LEVEL FACTORIAL DESIGN.
C     THAT IS, IF A VALUE OTHER THAN -1, 0, OR 1 IS DETECTED, THEN THE
C     MATRIX IS NOT IN CLASSIC FORM.  SET OUTPUT TO 1 FOR THE CLASSIC
C     CASE AND 0 OTHERWISE.
C
      SCAL9=1.0
      DO11010J=1,NC1
        DO11020I=1,NR1
          IF(YM1(I,J).EQ.-1.0 .OR. YM1(I,J).EQ.0.0 .OR.
     1       YM1(I,J).EQ.1.0)GOTO11020
          SCAL9=0.0
          GOTO11030
11020   CONTINUE
11010 CONTINUE
C
11030 CONTINUE
      ITYP9='SCAL'
      IUPFLG='FULL'
      GOTO9000
C
C               *****************************************************
C               **  STEP 11000-                                    **
C               **  TREAT THE DEX CHECK CENTER POINT         CASE  **
C               *****************************************************
C
11100 CONTINUE
C
C     CHECK FOR CENTER POINTS IN A LIST OF FACTOR VARIABLES.  CREATE
C     A TAG VARIABLE THAT WILL BE 1 FOR ROWS THAT ARE NOT CENTER POINTS
C     0 FOR ROWS THAT ARE CENTER POINTS.
C
C     2018/10: UPDATE SO THAT WE DO NOT ASSUME THE FACTORS ARE IN
C              CLASSIC UNITS (I.E., CENTER POINT EQUAL 0).  INSTEAD,
C              CHECK IF EQUAL TO THE MEDIAN VALUE OF THE DISTINCT
C              VALUES (FOR EVEN NUMBER OF DISTINCT LEVELS NO CENTER
C              POINT WILL BE DETECTED, FOR ODD NUMBER OF DISTINCT
C              LEVELS, CENTER POINT EQUALS THE MEDIAN).
C
      SCAL9=1.0
      DO11110I=1,NR1
        IFLAG=0
        DO11120J=1,NC1
C
C         FOR COLUMN J, DETERMINE THE CENTER POINT (= THE MEDIAN
C         OF THE DISTINCT VALUES)
C
          DO11130K=1,NR1
            Y1(K)=YM1(K,J)
11130     CONTINUE
          CALL DISTIN(Y1,NR1,IWRITE,Y2,NDIST,IBUGA3,IERROR)
          CALL MEDIAN(Y2,NDIST,IWRITE,Y3,MAXOBV,YMED,IBUGA3,IERROR)
C
C         NOW CHECK WHETHER THE CURRENT ROW IS EQUAL TO THE MEDIAN
C
          IF(YM1(I,J).NE.YMED)THEN
            IFLAG=1
            GOTO11129
          ENDIF
11120   CONTINUE
11129   CONTINUE
        VECT9(I)=REAL(IFLAG)
11110 CONTINUE
C
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'ATR3')GOTO9090
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',
     1A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IMCASE,IMSUBC
 9013 FORMAT('IMCASE,IMSUBC = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMVAR,IWRITE
 9014 FORMAT('NUMVAR,IWRITE = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)YS1,YS2,YS3,YS4
 9015 FORMAT('YS1,YS2,YS3,YS4 = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IERROR
 9016 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IYS2,IYS3,IYS23,NRJ,NCJ
 9017 FORMAT('IYS2,IYS3,IYS23,NRJ,NCJ = ',5I8)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GOTO9039
      IF(NC1.LE.0)GOTO9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO9032I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9049
      IF(NC2.LE.0)GOTO9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO9042I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9059
      IF(NC9.LE.0)GOTO9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9055I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GOTO9119
      DO9112I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GOTO9129
      DO9122I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GOTO9139
      DO9132I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GOTO9149
      DO9142I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GOTO9169
      DO9162I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GOTO9179
      IF(NC9.LE.0)GOTO9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO9172I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
C
      IF(IMCASE.NE.'MASS')GOTO9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GOTO9189
      IF(NC2.LE.0)GOTO9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO9182I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
CCCCC WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
C9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      WRITE(ICOUT,9187)NR2
 9187 FORMAT('NR2 = ',I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
C
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE MATCH(X,Z,NX,VAL,NVAL,IWRITE,Y,ICASE,
     1                 IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--MATCH EACH VALUE IN THE VALUE ARRAY TO THE
C              CLOSEST VALUE IN THE X ARRAY.  THE RETURNED
C              Y ARRAY WILL CONTAIN THE CORRESPONDING INDEX
C              VALUES OF THE X ARRAY (I.E., DON'T RETURN
C              THE MATCHING VALUE, JUST THE INDEX OF THE
C              MATCHING VALUE).
C              IF ICASE IS TRAN, THEN RETURN THE VALUE OF THE
C              ARRAY Z CORRESPONDING TO INDEX.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001/10
C     ORIGINAL VERSION--OCTOBER   2001.
C     UPDATED         --DECEMBER  2019. ADD ISUBRO
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
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
C
C------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION VAL(*)
C
C-----COMMON----------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT--------------------------------------------------
C
      ISUBN1='MATC'
      ISUBN2='H   '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ATCH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IWRITE,NX,NVAL
   52   FORMAT('IBUGA3,ISUBRO,IWRITE,NX,NVAL = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NX
          WRITE(ICOUT,56)I,X(I),Z(I)
   56     FORMAT('I,X(I),Z(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ****************************************
C               **  COMPUTE INDICES OF MATCHING VALUES *
C               ****************************************
C
      DO100I=1,NVAL
        VALTMP=VAL(I)
        INDTMP=1
        YDIFF=CPUMAX
        DO200J=1,NX
          APROD=X(J)*VALTMP
          TERM1=MAX(X(J),VALTMP)
          TERM2=MIN(X(J),VALTMP)
          IF(APROD.GT.0.0)THEN
            ADIFF=ABS(ABS(TERM1) - ABS(TERM2))
          ELSEIF(APROD.LT.0.0)THEN
            ADIFF=TERM1+ABS(TERM2)
          ELSE
            ADIFF=ABS(TERM1-TERM2)
          ENDIF
          IF(ADIFF.LT.YDIFF)THEN
            INDTMP=J
            YDIFF=ADIFF
          ENDIF
  200   CONTINUE
        IF(ICASE.EQ.'INDE')THEN
          Y(I)=REAL(INDTMP)
        ELSE
          Y(I)=Z(INDTMP)
        ENDIF
  100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ATCH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MATCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NX
 9012   FORMAT('IERROR,NX = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NVAL
          WRITE(ICOUT,9016)I,VAL(I),Y(I)
 9016     FORMAT('I,VAL(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE MATCH2(X,NX,VAL,NVAL,Y,IWRITE,ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--SORT THE VALUES IN X.  FIND THE INDEX, IVAL,
C              SUCH THAT
C
C                    X(I) <= VAL < X(I+1)
C
C              IF VAL < X(1), RETURN A 0 AND IF VAL > X(NX) RETURN
C              NX + 1.
C
C              DO THIS FOR EACH ROW OF THE VAL VECTOR.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2018/08
C     ORIGINAL VERSION--AUGUST    2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION VAL(*)
      DIMENSION Y(*)
C
C---------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT--------------------------------------------------
C
      ISUBN1='MATC'
      ISUBN2='H2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TCH2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATCH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IWRITE,NX
   52   FORMAT('IBUGA3,ISUBRO,IWRITE,NX = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I), VAL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ****************************************
C               **  ERROR CHECKING                    **
C               ****************************************
C
      IF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN MATCH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)NX
  103   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE (',I5,') IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NVAL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NVAL
  105   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE MATCH ',
     1         'VARIABLE (',I5,') IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  COMPUTE INDICES OF MATCHING VALUES *
C               ****************************************
C
      CALL SORT(X,NX,X)
C
      DO100I=1,NVAL
        VALTMP=VAL(I)
        IF(VALTMP.LT.X(1))THEN
          Y(I)=0.
        ELSEIF(VALTMP.GT.X(NX))THEN
          Y(I)=REAL(NX+1)
        ELSE
          DO200J=1,NX-1
            IF(VALTMP.GE.X(J) .AND. VALTMP.LT.X(J+1))THEN
              Y(I)=REAL(J)
              GOTO100
            ENDIF
  200     CONTINUE
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ATC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MATCH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NVAL
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE MATCDF(X,K,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE CLASSICAL MATCHING
C              DISTRIBUTION ON THE INTERVAL (0,K).
C              THIS DISTRIBUTION HAS MEAN = 1
C              AND STANDARD DEVIATION = 1
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              MASS FUNCTION:
C
C                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
C                         X = 0, 1, ..., K
C
C              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
C              ARRANGED IN A RANDOM ORDER.  THE MATCHING
C              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
C              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
C              ORDER.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                       K      = THE INTEGER VALUE THAT SPECIFIES
C                                THE MAXIMUM VALUE
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             DISTRIBUTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP, POIPDF.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
C                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
C                 PAGES 409-414.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2006. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DK
      DOUBLE PRECISION DI
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DGAMMA
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      CDF=0.0
C
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',
     1       'MATCDF SUBROUTINE IS LESS THAN 0.')
C
      IX=INT(X+0.5)
      IF(IX.LT.0 .OR. IX.GT.K)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)IX
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',
     1       'MATCDF SUBROUTINE IS OUTSIDE THE (0,N) INTERVAL')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C-----START POINT-----------------------------------------------------
C
C     FOR K SUFFICENTLY LARGE, USE POISSON (WITH LAMBDA = 1)
C     APPROXIMATION
C
      IF(K.GE.20)THEN
        ALAMB=1.0
        CALL POICDF(X,ALAMB,CDF)
      ELSE
        DK=DBLE(K)
        DCDF=0.0D0
        DO200J=0,IX
          IX2=J
          DX=DBLE(J)
          DTERM1=1.0D0/DGAMMA(DX+1.0D0)
          DSUM1=0.0D0
          DO100I=0,K-IX2
            DI=DBLE(I)
            DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100     CONTINUE
          DPDF=DTERM1*DSUM1
          DCDF=DCDF + DPDF
  200   CONTINUE
        CDF=REAL(DCDF)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE MATPDF(X,K,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE CLASSICAL MATCHING
C              DISTRIBUTION ON THE INTERVAL (0,K).
C              THIS DISTRIBUTION HAS MEAN = 1
C              AND STANDARD DEVIATION = 1
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              MASS FUNCTION:
C
C                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
C                         X = 0, 1, ..., K
C
C              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
C              ARRANGED IN A RANDOM ORDER.  THE MATCHING
C              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
C              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
C              ORDER.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                       K      = THE INTEGER VALUE THAT SPECIFIES
C                                THE MAXIMUM VALUE
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA, DLNGAM.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
C                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
C                 PAGES 409-414.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2006. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DK
      DOUBLE PRECISION DI
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DGAMMA
      DOUBLE PRECISION DLNGAM
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0
C
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',
     1       'MATPDF SUBROUTINE IS LESS THAN 0.')
C
      IX=INT(X+0.5)
      IF(IX.LT.0 .OR. IX.GT.K)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)IX
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',
     1       'MATPDF SUBROUTINE IS OUTSIDE THE (0,N) INTERVAL')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C-----START POINT-----------------------------------------------------
C
      DX=DBLE(IX)
      DK=DBLE(K)
C
C     FOR K SUFFICENTLY LARGE, USE APPROXIMATION EXP(-1)/X!
C
      IF(K.GE.20)THEN
        DPDF=DEXP(-1.0D0 - DLNGAM(DX+1.0D0))
      ELSE
        DTERM1=1.0D0/DGAMMA(DX+1.0D0)
        DSUM1=0.0D0
        DO100I=0,K-IX
          DI=DBLE(I)
          DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100   CONTINUE
        DPDF=DTERM1*DSUM1
      ENDIF
      PDF=REAL(DPDF)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE MATPPF(P,K,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE CLASSICAL MATCHING
C              DISTRIBUTION ON THE INTERVAL (0,K).
C              THIS DISTRIBUTION HAS MEAN = 1
C              AND STANDARD DEVIATION = 1
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              MASS FUNCTION:
C
C                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
C                         X = 0, 1, ..., K
C
C              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
C              ARRANGED IN A RANDOM ORDER.  THE MATCHING
C              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
C              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
C              ORDER.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                       K      = THE INTEGER VALUE THAT SPECIFIES
C                                THE MAXIMUM VALUE
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             DISTRIBUTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP, POIPPF.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
C                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
C                 PAGES 409-414.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2006. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DP
      DOUBLE PRECISION DK
      DOUBLE PRECISION DI
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DGAMMA
      DOUBLE PRECISION DEPS
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0
C
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',
     1       'MATCDF SUBROUTINE IS LESS THAN 0.')
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)P
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO ',
     1       'MATPPF IS OUTSIDE THE (0,1) INTERVAL')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
C     P = 0 AND P = 1 CASES
C
      IF(P.LE.0.0)THEN
        PPF=0.0
        GOTO9000
      ELSEIF(P.GE.1.0)THEN
        PPF=REAL(K)
        GOTO9000
      ENDIF
C
C     FOR K SUFFICENTLY LARGE, USE POISSON (WITH LAMBDA = 1)
C     APPROXIMATION
C
      IF(K.GE.20)THEN
        ALAMB=1.0
        CALL POIPPF(P,ALAMB,PPF)
        GOTO9000
      ELSE
        DK=DBLE(K)
        DP=DBLE(P)
        DCDF=0.0D0
        DEPS=1.0D-7
        DO200J=0,K
          IX2=J
          DX=DBLE(J)
          DTERM1=1.0D0/DGAMMA(DX+1.0D0)
          DSUM1=0.0D0
          DO100I=0,K-IX2
            DI=DBLE(I)
            DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100     CONTINUE
          DPDF=DTERM1*DSUM1
          DCDF=DCDF + DPDF
          IF(DCDF.GE.DP-DEPS)THEN
            PPF=REAL(J)
            GOTO9000
          ENDIF
C
  200   CONTINUE
        PPF=1.0
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE MATRAN(N,K,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE MATCHING DISTRIBUTION
C              WITH SHAPE PARAMETER K.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              MASS FUNCTION:
C
C                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
C                         X = 0, 1, ..., K
C
C              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
C              ARRANGED IN A RANDOM ORDER.  THE MATCHING
C              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
C              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
C              ORDER.
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --NPAR   = THE INTEGER VALUE
C                                OF THE SHAPE PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE MATCHING DISTRIBUTION
C             WITH SHAPE PARAMETERS N AND NPAR.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --NPAR > 0
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, LCTPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
C                 DISCRETE DISTRIBUTIONS", SECOND EDITION, 
C                 WILEY, PP. 242-244.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/6
C     ORIGINAL VERSION--JUNE      2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INTEGER N
      INTEGER K
      DIMENSION X(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
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
        GOTO9999
      ENDIF
C
      IF(K.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)K
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',
     1'MATCHING RANDOM NUMBERS IS NON-POSITIVE')
   12 FORMAT('***** ERROR--THE K PARAMETER FOR THE ',
     1'MATCHING RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C 100 CONTINUE
C
      IF(K.LT.20)THEN
        CALL UNIRAN(N,ISEED,X)
        DO100I=1,N
          XTEMP=X(I)
          CALL MATPPF(XTEMP,K,PPF)
          X(I)=PPF
  100   CONTINUE
      ELSE
        ALAMB=1.0
        CALL POIRAN(N,ALAMB,ISEED,X)
      ENDIF
C
 9999 CONTINUE
C
      RETURN
      END

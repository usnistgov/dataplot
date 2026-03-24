      SUBROUTINE MAINGR(ANOPL1,ANOPL2,NPLOTV,NPLOTP,NS,ICASPL,    &
                        MAXNPP,ISEED,IBOOSS,                      &
                        IX1TSV,IX2TSV,IY1TSV,IY2TSV,              &
                        IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,              &
                        BARHEF,BARWEF,                            &
                        IRHSTG,IHSTCW,IHSTEB,IHSTOU,IASHWT,       &
                        IHSTMC,IHSTOP,                            &
                        ICAPSW,IFORSW,IGUIFL,IERRFA,IFRALI,       &
                        IAND1,IAND2,ICONT,NUMHPP,NUMVPP,MAXNXT,   &
                        ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAINGR.
!              (THE   GR    AT THE END OF    MAINGR   STANDS FOR   GRAPHICS)
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES GRAPHICS COMMANDS.
!              THE GRAPHICS COMMANDS SEARCHED FOR BY MAINGR ARE AS FOLLOWS--
!
!                     ANOP PLOT (= PROPORTION PLOT)
!                     ... BOX PLOT
!                     BOX-COX NORMALITY PLOT
!                     BOX-COX HOMOSCEDASTICITY PLOT
!                     BOX-COX SYMMETRY PLOT (NOT DONE)
!                     BOX-COX LINEARITY PLOT
!                     BOX-COX STANDARDIZED EFFECTS PLOT (NOT DONE)
!                     COMPLEX DEMODULATION ... PLOT
!                     CONTOUR PLOT
!                     ... CONTROL CHART
!                     ... CORRELATION PLOT
!                     ... FFT PLOT             (NOT DONE)
!                     ... FREQUENCY PLOT
!                     ... HISTOGRAM
!                     ... HOMOSCEDASTICITY PLOT
!                     ... I PLOT
!                     INTERACTION PLOT
!                     LAG ... PLOT
!                     ... NORMALITY PLOT
!                     PERCENT POINT PLOT
!                     ... PERIODOGRAM
!                     PIE CHART
!                     PLOT
!                     ... PROBABILITY PLOT
!                     ... PPCC (PROBABILITY PLOT CORRELATION
!                              COEFFICIENT) PLOT
!                     ... ROOTOGRAM
!                     RUN SEQUENCE PLOT
!                     RUNS ... PLOT
!                     ... SPECTRAL PLOT
!                     3-D PLOT
!                     3-D ... FREQUENCY  PLOT   (NOT DONE)
!                     3-D ... HISTOGRAM         (NOT DONE)
!                     4-PLOT ... ANALYSIS (DONE IN MAIN)
!                     BAR CHART
!                     STEM AND LEAF DIAGRAM
!                     ... STATISTIC PLOT
!                     YOUDEN PLOT
!                     ... BIHISTOGRAM
!                     ERROR BAR PLOT    OCTOBER 1988
!                     FRACTAL PLOT      DECEMBER 1988
!                     POINCARE PLOT     DECEMBER 1988
!                     (REPLACED BY PHASE PLANE DIAGRAM  JULY 1989)
!                     JACKNIFE ... STATISTIC PLOT   JANUARY 1989
!                     BOOTSTRAP ... STATISTIC PLOT  JANUARY 1989
!                     DEX/DOE EXP DESIGN ... PLOT MAY       1989
!                     TAIL AREA PLOT                        1989
!                     NORMAL PLOT                 MAY       1990
!                     PHD PLOT (KER-CHAU LIE)     OCTOBER   1991
!                         (NOT IMPLEMENTED YET)
!                     BLOCK  PLOT                 APRIL     1992.
!                     <STAT> BLOCK                JUNE      1992.
!                     SYMBOL PLOT                 AUGUST    1992.
!                     VECTOR PLOT                 AUGUST    1992
!                     ANDREWS PLOT                NOVEMBER  1992
!                     PARTIAL AUTOCORR. PLOT      FEBRUARY  1993
!                     Q ... CONTROL CHART         DECEMBER  1993
!                     CME (CONT. MEAN EXCEEDANCE) PLOT DECEMBER 1993
!                     CONDITIONAL ... PLOT        DECEMBER  1993
!                     ... COMOVEMENT  PLOT        OCTOBER   1997
!                     KAPLAN MEIER    PLOT        MAY       1998
!                     DUANE           PLOT        MAY       1998
!                     EMPIRICAL CDF   PLOT        MAY       1998
!                     EXPONENTIAL HAZARD PLOT     MAY       1998
!                     NORMAL      HAZARD PLOT     MAY       1998
!                     LOGNORMAL   HAZARD PLOT     MAY       1998
!                     WEIBULL     HAZARD PLOT     MAY       1998
!                     HOTELLING CONTROL CHART     MAY       1998
!                     SEASONAL SUBSERIES PLOT     FEBRUARY  1999
!                     SPREAD-LOCATION PLOT        AUGUST    1999
!                     TUKEY MEAN-DIFFERENCE PLOT  SEPTEMBER 1999
!                     INTERACTION   PLOT          OCTOBER   1999
!                     ... INTERACTION STAT PLOT   OCTOBER   1999
!                     CROSS TABULATE <STAT> PLOT  DECEMBER  1999
!                     DEX CONTOUR PLOT            JANUARY   2000
!                     YATES CUBE  PLOT            JANUARY   2000
!                     BAG PLOT                    JANUARY   2001
!                         (NOT IMPLEMENTED YET)
!                     KERNEL DENSITY PLOT         AUGUST    2001
!                     CONSENSUS MEAN PLOT         AUGUST    2001
!                     PARTIAL RESIDUAL PLOT       JUNE      2002
!                     PARTIAL REGRESSION PLOT     JUNE      2002
!                     PARTIAL LEVERAGE PLOT       JUNE      2002
!                     CCPR PLOT                   JUNE      2002
!                     INFLUENCE CURVE <STAT> PLOT JULY      2002
!                     SHIFT PLOT                  FEBRUARY  2003
!                     VIOLIN PLOT                 FEBRUARY  2003
!                     PARALLEL COORDINATES PLOT   MARCH     2003
!                     PEAKS OVER THRESHOLD PLOT   APRIL     2005
!                     REPAIR PLOT                 OCTOBER   2006
!                     MEAN REPAIR FUNCTION PLOT   OCTOBER   2006
!                     TRILINEAR PLOT              DECEMBER  2006
!                     ROC CURVE                   APRIL     2007
!                     ROSE PLOT                   APRIL     2007
!                     BIVARIATE NORMAL TOLERANCE
!                         REGION PLOT             MAY       2007
!                     BIVARIATE NORMAL CONFIDENCE
!                         REGION PLOT             NOVEMBER  2013
!                     BINARY <TYPE> PLOT          MAY       2007
!                     ORD PLOT                    MAY       2007
!                     POISSON PLOT                MAY       2007
!                     BINOMIAL PLOT               MAY       2007
!                     NEGATIVE BINOMIAL PLOT      MAY       2007
!                     GEOMETRIC PLOT              MAY       2007
!                     LOGARITHMIC SERIES PLOT     MAY       2007
!                     ASSOCIATION PLOT            JUNE      2007
!                     SIEVE PLOT                  JUNE      2007
!                     PSUEDO ROC CURVE            JULY      2007
!                     LEVEL PLOT                  MARCH     2008
!                     (DISCRETE CONTOUR PLOT)
!                     IMAGE PLOT                  MARCH     2008
!                     SPATIAL DISTRIBUTION PLOT   APRIL     2008
!                     (UNDER DEVELOPMENT)
!                     FLUCUATION PLOT             MAY       2008
!                     STRIP PLOT                  OCTOBER   2008
!                     DETECTIION LIMIT PLOT       DECEMBER  2008
!                     (UNDER DEVELOPMENT)
!                     TABULATION PLOT             SEPTEMBER 2009
!                     ISO 13528 PLOT              FEBRUARY  2012
!                     ISO 13528 ZSCORE PLOT       FEBRUARY  2012
!                     ISO 13528 JSCORE PLOT       FEBRUARY  2012
!                     ISO 13528 RLP PLOT          FEBRUARY  2012
!                     FRECHET PLOT                OCTOBER   2013
!                     DISTRIBUTIONAL FIT PLOT     AUGUST    2014
!                     LORENZ CURVE                FEBRUARY  2015
!                     H CONSISTENCY PLOT          MAY       2015
!                     K CONSISTENCY PLOT          MAY       2015
!                     COCHRAN VARIANCE PLOT       MAY       2015
!                     MOVING STATISTIC PLOT       MAY       2015
!                     CUMULATIVE STATISTIC PLOT   MAY       2015
!                     TWO-WAY <ROW/COLUMN> PLOT   JUNE      2015
!                     TWO FACTOR PLOT             JUNE      2015
!                     EMPIRICAL QUANTILE PLOT     FEBRUARY  2017
!                     TIQ PLOT                    MARCH     2017
!                     QUANTILE BOX PLOT           MARCH     2017
!                     BLAND ALTMAN PLOT           JULY      2017
!                     NORM KERN DENSITY MIXT PLOT JULY      2017
!                     DEX ORDER PLOT              FEBRUARY  2018
!                     CLASSIFICATION ... PLOT     FEBRUARY  2019
!                     TOTAL TIME ON TEST PLOT     JUNE      2020
!                     FISHER DISCRIMINATION PLOT  JULY      2024
!                     K NEAREST NEIGHBORS
!                       DISCRIMINATION PLOT       JULY      2024
!                     COMMUTABILITY DIFF PLOT     APRIL     2025
!                     COMMUTABILITY PLOT          APRIL     2025
!                     DUMBBELL PLOT               MAY       2025
!                     WATERFALL CHART             JUNE      2025
!                     BULLET PLOT                 JUNE      2025
!                     VARIABLE WIDTH BAR PLOT     JUNE      2025
!                     RIDGELINE PLOT              JUNE      2025
!                     CANDLESTICK PLOT            JULY      2025
!                     OPEN CLOSE LOW HIGH PLOT    JULY      2025
!                     DOT MATRIX PLOT             JULY      2025
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --MARCH     1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!        ETC.
!     UPDATED         --AUGUST    1987. BOX-COX STANDARDIZED EFFECTS PLOT
!     UPDATED         --JANUARY   1988. (... STATISTIC PLOTS)
!     UPDATED         --JANUARY   1988. (... CHARTS)
!     UPDATED         --FEBRUARY  1988. PROFILE PLOT
!     UPDATED         --FEBRUARY  1988. STAR PLOT
!     UPDATED         --AUGUST    1988. CONTOUR PLOT
!     UPDATED         --AUGUST    1988. PARETO PLOT
!     UPDATED         --SEPTEMBER 1988. EQUATE PROPROTION PLOT TO ANOP PLOT
!     UPDATED         --SEPTEMBER 1988. YOUDEN PLOT (= PLOT WITH 3 ARGS)
!     UPDATED         --SEPTEMBER 1988. BIHISTOGRAM
!     UPDATED         --NOVEMBER  1988. ERROR BAR PLOT
!     UPDATED         --DECEMBER  1988. ISEED ARGUMENT--FRACTAL PLOT
!     UPDATED         --DECEMBER  1988. POINCARE PLOT
!     UPDATED         --JANUARY   1989. JACKNIFE ... STAT PLOTS
!     UPDATED         --JANUARY   1989. BOOTSTRAP ... STAT PLOTS
!     UPDATED         --FEBRUARY  1989. CONTINUE CHARACTER CONFLICT (ALAN)
!     UPDATED         --APRIL     1989. SCATTER PLOT (= SYNONYM FOR PLOT)
!     UPDATED         --MAY       1989. DEX/DOE ... PLOT
!     UPDATED         --MAY       1989. TAIL AREA PLOT
!     UPDATED         --JULY      1989. POINCARE PLOT TO PHASE PLANE DIAG
!     UPDATED         --MAY       1990. NORMAL PLOT
!     UPDATED         --OCTOBER   1991. PHD PLOT (NOT DONE YET)
!     UPDATED         --APRIL     1992. BLOCK PLOT
!     UPDATED         --JUNE      1992. <STAT> BLOCK PLOT
!     UPDATED         --AUGUST    1992. VECTOR PLOT, SYMBOL PLOT
!     UPDATED         --NOVEMBER  1992. ANDREWS PLOT
!     UPDATED         --FEBRUARY  1993. PARTIAL AUTOCORRELATION PLOT
!     UPDATED         --JULY      1993. ARGUMENTS TO FRACTAL PLOT
!     UPDATED         --AUGUST    1993. CONFLICT WITH MEDIAN POLISH
!     UPDATED         --DECEMBER  1993. ADD ARG IN CALL DPPP()
!     UPDATED         --DECEMBER  1993. Q ... CONTROL CHART
!     UPDATED         --DECEMBER  1993. CME PLOT
!     UPDATED         --DECEMBER  1993. COND. ... EXCEEDANCE PLOT
!     UPDATED         --DECEMBER  1994. AUGMENT DPPARE() ARG. LIST
!     UPDATED         --MARCH     1995. ADD MAXNXT TO DPBLOC
!     UPDATED         --MARCH     1996. ADD IRHSTG TO DPHIST
!     UPDATED         --OCTOBER   1997. COMOVEMENT PLOT
!     UPDATED         --OCTOBER   1997. AUTO COMOVEMENT PLOT
!     UPDATED         --MAY       1998. KAPLAN MEIER PLOT
!     UPDATED         --MAY       1998. DUANE PLOT
!     UPDATED         --MAY       1998. EMPIRICAL CDF PLOT
!     UPDATED         --SEPTEMBER 1998. HOTELLING CONTROL CHART
!     UPDATED         --FEBRUARY  1999. SEASONAL SUBSERIES PLOT
!     UPDATED         --AUGUST    1999. SPREAD-LOCATION PLOT
!     UPDATED         --SEPTEMBER 1999. TUKEY MEAN-DIFFERENCE PLOT
!     UPDATED         --OCTOBER   1999. INTERACTION PLOT
!     UPDATED         --OCTOBER   1999. INTERACTION STATISTIC PLOT
!     UPDATED         --DECEMBER  1999. IMPLEMENT SUB-REGIONS
!     UPDATED         --DECEMBER  1999. SAVE SOME INTERNAL PARAMETERS
!                                       FOR ALL PLOTS
!     UPDATED         --DECEMBER  1999. CROSS TABULATE PLOT
!     UPDATED         --JANUARY   2000. DEX CONTOUR PLOT
!     UPDATED         --JANUARY   2001. BAG PLOT (NOT WORKING)
!     UPDATED         --AUGUST    2001. KERNEL DENSITY PLOT
!     UPDATED         --AUGUST    2001. CONSENSUS MEAN PLOT
!     UPDATED         --MARCH     2002. ROBUSTNESS PLOT SYNONUM
!                                       FOR BLOCK PLOT
!     UPDATED         --JULY      2002. INFLUENCE CURVE
!     UPDATED         --OCTOBER   2002. CALL LIST TO CONSENUSE MEAN
!                                       PLOT
!     UPDATED         --FEBRUARY  2003. SHIFT PLOT
!     UPDATED         --FEBRUARY  2003. VIOLIN PLOT
!     UPDATED         --MARCH     2003. PARALLEL COORDINATES PLOT
!     UPDATED         --SEPTEMBER 2003. BCA <BOOTSTRAP/JACKINFE>
!     UPDATED         --MAY       2004. KOLMOGOROV SMIRNOV PLOT AS
!                                       VARIANT OF PPCC PLOT
!     UPDATED         --SEPTEMBER 2004. CALL LIST TO DPHIST
!     UPDATED         --APRIL     2005. PEAKS OVER THRESHOLD PLOT
!     UPDATED         --MARCH     2006. ADD IFORSW TO CONSENSUS MEAN
!                                       PLOT
!     UPDATED         --OCTOBER   2006. REPAIR PLOT
!     UPDATED         --OCTOBER   2006. MEAN REPAIR FUNCTION PLOT
!     UPDATED         --DECEMBER  2006. TRILINEAR PLOT
!     UPDATED         --APRIL     2007. ROC CURVE
!     UPDATED         --APRIL     2007. ROSE PLOT
!     UPDATED         --MAY       2007. BIVARIATE NORMAL TOLERANCE
!                                       REGION PLOT
!     UPDATED         --MAY       2007. BINARY PLOT
!     UPDATED         --MAY       2007. ORD PLOT
!     UPDATED         --JUNE      2007. ASSOCIATION PLOT
!     UPDATED         --JUNE      2007. SIEVE PLOT
!     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO
!                                       COMMON
!     UPDATED         --JANUARY   2008. ADJUST USE OF DPCOZ3.INC
!                                       STORAGE
!     UPDATED         --MARCH     2008. LEVEL (DISCRETE CONTOUR) PLOT
!     UPDATED         --MARCH     2008. IMAGE PLOT
!     UPDATED         --APRIL     2008. SPATIAL DISTRIBUTION PLOT
!                                       (STILL UNDER DEVELOPMENT)
!     UPDATED         --MAY       2008. FLUCUATION PLOT
!     UPDATED         --OCTOBER   2008. STRIP PLOT
!     UPDATED         --SEPTEMBER 2009. TABLE <STAT> PLOT
!     UPDATED         --OCTOBER   2009. "BATCH MULTIPLE" OPTION
!                                       FOR STRIP PLOT
!     UPDATED         --JANUARY   2010. CALL LIST TO DPHIST
!     UPDATED         --FEBRUARY  2012. ISO 13528 PLOT
!     UPDATED         --FEBRUARY  2012. ISO 13528 ZSCORE PLOT
!     UPDATED         --FEBRUARY  2012. ISO 13528 JSCORE PLOT
!     UPDATED         --FEBRUARY  2012. ISO 13528 RLP PLOT
!     UPDATED         --OCTOBER   2013. FRECHET PLOT
!     UPDATED         --NOVEMBER  2013. BIVARIATE NORMAL CONFIDENCE
!                                       REGION PLOT
!     UPDATED         --AUGUST    2014. DISTRIBUTIONAL FIT PLOT
!     UPDATED         --FEBRUARY  2015. LORENZ CURVE
!     UPDATED         --MAY       2015. H CONSISTENCY PLOT
!     UPDATED         --MAY       2015. K CONSISTENCY PLOT
!     UPDATED         --MAY       2015. COCHRAN VARIANCE PLOT
!     UPDATED         --MAY       2015. <stat> CUMULATIVE STATISTIC PLOT
!     UPDATED         --MAY       2015. <stat> MOVING STATISTIC PLOT
!     UPDATED         --JUNE      2015. TWO WAY <ROW/COLUMN> PLOT
!     UPDATED         --JUNE      2015. TWO FACTOR PLOT
!     UPDATED         --JUNE      2016. <stat> WINDOW STATISTIC PLOT
!     UPDATED         --FEBRUARY  2017. EMPIRICAL QUANTILE PLOT
!     UPDATED         --MARCH     2017. TIQ PLOT
!     UPDATED         --JULY      2017. BLAND ALTMAN PLOT
!     UPDATED         --JULY      2017. NORMAL KERNEL DENSITY MIXTURE PLOT
!     UPDATED         --FEBRUARY  2018. DEX ORDER PLOT
!     UPDATED         --FEBRUARY  2019. CLASSIFICATION ... PLOT
!     UPDATED         --MARCH     2019. CALL LIST TO DPBLOC
!     UPDATED         --JUNE      2020. TOTAL TIME ON TEST PLOT
!     UPDATED         --APRIL     2021. IMPROVE ALGORITHM FOR
!                                       DEFAULT SUBREGIONS
!     UPDATED         --JULY      2024. FISHER DISCRIMINATION PLOT
!     UPDATED         --JULY      2024. K NEAREST NEIGHBORS
!                                       DISCRIMINATION PLOT
!     UPDATED         --DECEMBER  2024. COMMUTABILITY PLOT
!     UPDATED         --MAY       2025. DUMBBELL PLOT
!     UPDATED         --JUNE      2025. WATERFALL CHART
!     UPDATED         --JUNE      2025. BULLET PLOT
!     UPDATED         --JUNE      2025. VARIABLE WIDTH BAR PLOT
!     UPDATED         --JUNE      2025. RIDGELINE PLOT
!     UPDATED         --JULY      2025. CANDLESTICK PLOT
!     UPDATED         --JULY      2025. OPEN CLOSE LOW HIGH PLOT
!     UPDATED         --JULY      2025. DOT MATRIX PLOT
!     UPDATED         --NOVEMBER  2025. CALL LIST TO DPFRLI
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
!CCCC CHARACTER*4 ICASSW
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
!
      DIMENSION TEMPZZ(2)
!CCCC DIMENSION TEMP(*)
!CCCC DIMENSION TEMP2(*)
!CCCC DIMENSION TEMP3(*)
!CCCC DIMENSION XTEMP1(*)
!CCCC DIMENSION XTEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOCO.INC'
      INCLUDE 'DPCOHO.INC'
!
!CCCC TO AVOID NAME CONFLICTS, ONLY BRING IN THE SPECIFIC
!CCCC COMMON BLOCK (NOT ALL OF DPCOST.INC)
!
      CHARACTER*4  IERRST
      COMMON/CSETG/IERRST
!
!
      INCLUDE 'DPCOZ3.INC'
!
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      EQUIVALENCE (G3RBAG(KGARB5),TEMP(1))
      EQUIVALENCE (G3RBAG(KGARB6),TEMP2(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!CCCC ICONT=IDEVCN(1)
!CCCC ICOLOR=IDEVCL(1)
!CCCC NUMHPP=IDEVPP(1,1)
!CCCC NUMVPP=IDEVPP(1,2)
      ISUBN1='MAIN'
      ISUBN2='GR  '
!
      NACC=0
      NREJ=0
      NTOT=0
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,81)IX1TSC,IX2TSC,IY1TSC,IY2TSC
   81   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)IX1TSV,IX2TSV,IY1TSV,IY2TSV
   82   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFOUND='NO'
      IERROR='NO'
      IF(ICOM.EQ.'LET ')GO TO 9000
      IBCABT='OFF'
!
!               ***********************************************
!               **  TREAT THE EMPIRICAL QUANTILE PLOT  CASE  **
!               **            QUANTILE BOX PLOT        CASE  **
!               ***********************************************
!
      IF((ICOM.EQ.'EMPI' .AND. IHARG(1).EQ.'QUAN') .OR.   &
         (IHARG(1).EQ.'EMPI' .AND. IHARG(2).EQ.'QUAN'))THEN
        CALL DPEQFU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'QUAN' .AND. IHARG(1).EQ.'BOX ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
        CALL DPEQFU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************
!               **  TREAT THE BOX PLOT CASE  **
!               *******************************
!
      IF(   &
        ICOM.EQ.'BOX' .OR. IHARG(1).EQ.'BOX' .OR.   &
        IHARG(2).EQ.'BOX')THEN
        CALL DPBOX(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   ICONT,IFENSW,IBUGG2,IBUGG3,IBUGQ,ISUBRO,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************************
!               **  TREAT THE DISTRIBUTIONAL FIT PLOT CASE  **
!               **********************************************
!
      IF(ICOM.EQ.'DIST' .AND. IHARG(1).EQ.'FIT ' .AND.   &
        IHARG(2).EQ.'PLOT')THEN
        CALL DPDFPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(IHARG(1).EQ.'DIST' .AND. IHARG(2).EQ.'FIT ' .AND.   &
        IHARG(3).EQ.'PLOT')THEN
        CALL DPDFPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE VIOLIN PLOT CASE  **
!               **********************************
!
      IF(   &
        ICOM.EQ.'VIOL' .OR. IHARG(1).EQ.'VIOL' .OR.   &
        IHARG(2).EQ.'VIOL')THEN
        CALL DPVIOL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ICONT,IFENSW,IKDETY,IKDENP,PKDEWI,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  TREAT THE COMPLEX DEMODULATION ... PLOT CASE  **
!               ****************************************************
!
!CCCC IF(ICOM.EQ.'COMP')GO TO 200
      IF(ICOM.EQ.'COMP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'DEMO')THEN
        CALL DPCD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  IANGLU,DEMOFR,DEMODF,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE ... CONTROL CHART CASE  **
!               ****************************************
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT                FEBRUARY 1989
!CCCC AND REPLACED BY THE SUCCEEDING LINE                 FEBRUARY 1989
!CCCC TO AVOID A CONFLICT WITH THE CONTINUE CHARACTER     FEBRUARY 1989
!CCCC IF(ICOM.EQ.'CONT')GO TO 300
!CCCC ADD HOTELLING CONTROL CHART (= MULTIVARIATE CONTROL
!CCCC CHART)                                             SEPTEMBER 1998
!CCCC SUPPORT FOUR DISTINCT CASES FOR HOTELLING CONTROL  FEBRUARY 2003
!CCCC CHART:
!CCCC   1) PHASE I HOTELLING CONTROL CHART
!CCCC   2) PHASE I HOTELLING INDIVIDUAL CONTROL CHART
!CCCC   3) PHASE II HOTELLING CONTROL CHART
!CCCC   4) PHASE II HOTELLING INDIVIDUAL CONTROL CHART
!CCCC IF PHASE <I/II> OMITTED, ASSUME A PHASE I CHART.
!
      IF(ICOM.EQ.'CONT'.AND.ICOM2.NE.'INUE')GO TO 300
      IF(ICOM.EQ.'CONT'.AND.ICOM2.NE.'OUR ')GO TO 300
      IF(ICOM.EQ.'CONT'.AND.IHARG(1).NE.'LOOP')GO TO 300
!
      IF(ICOM.EQ.'PHAS')THEN
        IF(IHARG(1).EQ.'I'.OR.IHARG(1).EQ.'ONE'.OR.IHARG(1).EQ.'1')THEN
          CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'II'.OR.IHARG(1).EQ.'TWO'.OR.   &
               IHARG(1).EQ.'2')THEN
          CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'HOTE'.OR.   &
        (ICOM.EQ.'MULT'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CONT'))THEN
        CALL DPHTCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
        ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!     2015/09: CHECK FOR CONFLICT WITH CONTOUR OR DEX CONTOUR
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT'.AND.   &
         IHARG2(1).NE.'OUR ')GO TO 300
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')GO TO 300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CONT'.AND.   &
         IHARG2(2).NE.'OUR ')GO TO 300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHAR')GO TO 300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CONT'.AND.   &
         IHARG2(3).NE.'OUR ')GO TO 300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHAR')GO TO 300
      GO TO 399
!
  300 CONTINUE
      CALL DPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
  399 CONTINUE
!
!               *******************************************
!               **  TREAT THE ... CORRELATION PLOT CASE  **
!               *******************************************
!
! 2012/1: FOLD IN COMOVEMENT PLOT IN WITH CORRELATION PLOT.
!
      IF(ICOM.EQ.'AUTO' .OR. ICOM.EQ.'CROS' .OR.   &
         ICOM.EQ.'PART' .OR. ICOM.EQ.'COMO' .OR.   &
         IHARG(1).EQ.'AUTO' .OR. IHARG(2).EQ.'CROS' .OR.   &
         IHARG(1).EQ.'PART' .OR. IHARG(1).EQ.'COMO')THEN
        CALL DPCORR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE ... FREQUENCY PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'FREQ' .OR. IHARG(1).EQ.'FREQ' .OR.   &
         IHARG(2).EQ.'FREQ' .OR. IHARG(3).EQ.'FREQ' .OR.   &
         IHARG(4).EQ.'FREQ')THEN
        CALL DPFREQ(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    CLLIMI,CLWIDT,   &
                    IRHSTG,IHSTCW,IHSTEB,IHSTOU,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE ... HISTOGRAM CASE  **
!               ************************************
!
      IF(ICOM.EQ.'HIST' .OR. ICOM.EQ.'ASH ')GO TO 600
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HIST')GO TO 600
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'HIST')GO TO 600
      GO TO 699
!
  600 CONTINUE
      CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      CLLIMI,CLWIDT,   &
      IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,IHSTMC,IHSTOP,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
  699 CONTINUE
!
!               *****************************
!               **  TREAT THE I PLOT CASE  **
!               *****************************
!
!     10/18/2013: THERE ARE A NUMBER OF NEW VARIANTS TO THIS
!                 COMMAND.  SO CALL THIS ROUTINE AND LET DPI
!                 DETERMINE IF A VALID I PLOT COMMAND HAS BEEN
!                 ENTERED.
!
!CCCC IF(
!CCCC1  ICOM.EQ.'I' .OR. IHARG(1).EQ.'I' .OR.
!CCCC1  IHARG(2).EQ.'I' .OR. IHARG(3).EQ.'I')THEN
        CALL DPI(NPLOTV,NPLOTP,NS,ICASPL,ISEED,IAND1,IAND2,   &
                 ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
!CCCC ENDIF
!
!               ************************************
!               **  TREAT THE DUMBBELL PLOT CASE  **
!               ************************************
!
      IF(ICOM.EQ.'DUMB' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPDBPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE WATERFALL CHART CASE  **
!               **************************************
!
      IF(ICOM.EQ.'WATE' .AND. IHARG(1).EQ.'CHAR')THEN
        CALL DPWAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE BULLET PLOT     CASE  **
!               **************************************
!
      IF(ICOM.EQ.'BULL' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPBUPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  TREAT THE VARIABLE WIDTH BAR PLOT  CASE  **
!               ***********************************************
!
      IF(ICOM.EQ.'VARI' .AND. IHARG(1).EQ.'WIDT' .AND.          &
         IHARG(2).EQ.'BAR ')THEN
        CALL DPVWBP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,  &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE CANDLESTICK PLOT CASE **
!               **************************************
!
      IF(ICOM.EQ.'CAND' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPCSPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************************
!               **  TREAT THE OPEN CLOSE LOW HIGH PLOT CASE **
!               **********************************************
!
      IF(ICOM.EQ.'OCLH' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPCSPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'OPEN'     .AND. IHARG(1).EQ.'CLOS' .AND.  &
             IHARG(2).EQ.'LOW ' .AND. IHARG(3).EQ.'HIGH' .AND.  &
             IHARG(4).EQ.'PLOT')THEN
        CALL DPCSPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,      &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************
!               **  TREAT THE LAG ... PLOT CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'LAG' .OR. IHARG(1).EQ.'LAG')THEN
        CALL DPLAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE PERCENT POINT PLOT CASE  **
!               *****************************************
!
      IMAX=NUMARG-1
      IF(IMAX.LE.0)GO TO 1099
      IFLAG1=0
      IFLAG2=0
      IFLAG3=0
      IF(ICOM.EQ.'PERC' .AND. ICOM2.NE.'ENTI')IFLAG1=1
      DO 1010 I=1,NUMARG
        IF(IHARG(I).EQ.'PERC' .AND. IHARG2(I).NE.'ENTI')IFLAG1=1
        IF(IHARG(I).EQ.'POIN')IFLAG2=1
        IF(IHARG(I).EQ.'PLOT')IFLAG3=1
 1010 CONTINUE
      IF(IFLAG1*IFLAG2*IFLAG3.EQ.1)THEN
        CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    CLLIMI,CLWIDT,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
 1099 CONTINUE
!
!               **************************************
!               **  TREAT THE ... PERIODOGRAM CASE  **
!               **************************************
!
!CCCC 2012/1: HANDLE WITH SPECTRAL PLOT
!
!CCCC IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ASD')GO TO 9399
!CCCC IF(ICOM.EQ.'PERI')GO TO 1100
!CCCC IF(ICOM2.EQ.'PERI')GO TO 1100
!CCCC IF(ICOM2.EQ.'SPER')GO TO 1100
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PERI')GO TO 1100
!CCCC GO TO 1199
!
!1100 CONTINUE
!CCCC CALL DPPERI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES')GO TO 9000
!
!1199 CONTINUE
!
!               ********************************
!               **  TREAT THE PIE CHART CASE  **
!               ********************************
!
      IF(ICOM.EQ.'PIE')THEN
        CALL DPPIE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE PLOT CASE.          **
!               **  TREAT THE YOUDEN PLOT         **
!               **  AS A SPECIAL CASE OF PLOT     **
!               **  (PLOT WITH 3 ARGUMENTS).      **
!               **  TREAT THE SCATTER PLOT        **
!               **  AS A SYNONYM FOR PLOT         **
!               ************************************
!
      IF((ICOM.EQ.'YOUD' .OR. ICOM.EQ.'SCAT') .AND.   &
         IHARG(1).NE.'INDE')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGA2,IERROR)
      ELSEIF(ICOM.NE.'PLOT')THEN
        GO TO 1399
      ENDIF
!
      IAND1=IAND2
      CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  IANGLU,MAXNPP,   &
                  IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,   &
                  IFOUND,IERROR)
!
      IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
        WRITE(ICOUT,333)IFOUND,IERROR,IAND1,IAND2
  333   FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFOUND.EQ.'YES')GO TO 9000
!CCCC IF(IAND2.EQ.'YES')GO TO 100
!CCCC IF(IAND2.EQ.'NO')GO TO 9000
!
 1399 CONTINUE
!
!               ****************************************************
!               **  TREAT THE ... MOVING     STATISTIC PLOT CASE **
!               **  TREAT THE ... CUMULATIVE STATISTIC PLOT CASE **
!               **  TREAT THE ... WINDOW     STATISTIC PLOT CASE **
!               ***************************************************
!
      IF(ICOM.EQ.'FLUC')GO TO 6399
      DO 6302 I=1,NUMARG
        IF(IHARG(I).EQ.'INTE'.AND.IHARG2(I).EQ.'RACT')GO TO 6399
        IF(IHARG(I).EQ.'INFL'.AND.IHARG2(I).EQ.'UENC')GO TO 6399
        IF(IHARG(I).EQ.'BLOC')GO TO 6399
 6302 CONTINUE
      DO 6303 I=1,NUMARG-1
        IF(IHARG(I).EQ.'PROB' .AND. IHARG(I+1).EQ.'PLOT')GO TO 6399
        IF(IHARG(I).EQ.'PPCC' .AND. IHARG(I+1).EQ.'PLOT')GO TO 6399
 6303 CONTINUE
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GO TO 6300
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GO TO 6300
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GO TO 6300
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GO TO 6300
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GO TO 6300
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GO TO 6300
      GO TO 6399
!
 6300 CONTINUE
      CALL DPMOSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  MAXNXT,ISEED,FILWID,   &
                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6399 CONTINUE
!
!               ***********************************************
!               **  TREAT THE <DIST> TIQP        PLOT  CASE  **
!               ***********************************************
!
      IF(ICOM.EQ.'TIQ ' .OR. IHARG(1).EQ.'TIQ ' .OR.   &
         IHARG(2).EQ.'TIQ ' .OR. IHARG(3).EQ.'TIQ ')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'TRUN' .AND. IHARG(1).EQ.'INFO' .AND.   &
         IHARG(2).EQ.'QUAN')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(IHARG(1).EQ.'TRUN' .OR. IHARG(2).EQ.'TRUN' .OR.   &
         IHARG(3).EQ.'TRUN' .OR. IHARG(4).EQ.'TRUN')THEN
        CALL DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE ... STATISTIC PLOT CASE **
!               ****************************************
!
      IF(ICOM.EQ.'FLUC')GO TO 6699
      DO 6602 I=1,NUMARG
        IF(IHARG(I).EQ.'INTE'.AND.IHARG2(I).EQ.'RACT')GO TO 6699
        IF(IHARG(I).EQ.'INFL'.AND.IHARG2(I).EQ.'UENC')GO TO 6699
        IF(IHARG(I).EQ.'BLOC')GO TO 6699
 6602 CONTINUE
      DO 6603 I=1,NUMARG-1
        IF(IHARG(I).EQ.'PROB' .AND. IHARG(I+1).EQ.'PLOT')GO TO 6699
        IF(IHARG(I).EQ.'PPCC' .AND. IHARG(I+1).EQ.'PLOT')GO TO 6699
 6603 CONTINUE
      DO 6604 I=1,NUMARG-2
        IF(IHARG(I).EQ.'CUMU' .AND. IHARG(I+1).EQ.'STAT' .AND.   &
           IHARG(I+2).EQ.'PLOT')GO TO 6699
        IF(IHARG(I).EQ.'MOVI' .AND. IHARG(I+1).EQ.'STAT' .AND.   &
           IHARG(I+2).EQ.'PLOT')GO TO 6699
 6604 CONTINUE
      IF(ICOM.EQ.'VARI' .AND. IHARG(1).EQ.'WIDT' .AND.           &
         IHARG(2).EQ.'BAR ')GO TO 6699
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GO TO 6600
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GO TO 6600
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GO TO 6600
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GO TO 6600
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GO TO 6600
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GO TO 6600
      GO TO 6699
!
 6600 CONTINUE
      CALL DPSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                MAXNXT,ISEED,ICONT,   &
                ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6699 CONTINUE
!
!               *******************************************
!               **  TREAT THE ... PROBABILITY PLOT CASE  **
!               *******************************************
!
      IMAX=NUMARG-1
      IF(IMAX.GT.1)THEN
        DO 1410 I=1,NUMARG
          IF(IHARG(I).EQ.'PROB')THEN
            CALL DPPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES')GO TO 9000
          ENDIF
 1410   CONTINUE
      ENDIF
!
!               ************************************
!               **  TREAT THE ... PPCC PLOT CASE  **
!               ************************************
!
!     SINCE A NUMBER OF GOODNESS-OF-FIT STATISTICS ARE NOW
!     SUPPORTED, JUST CALL THIS COMMAND AND SEE IF DPPPCC
!     RECOGNIZES ONE OF THE SUPPORTED STATISTICS.  NO NEED TO
!     DUPLICATE HERE.
!
      CALL DPPPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICASP2,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
!               ****************************************
!               **  TREAT THE RUN SEQUENCE PLOT CASE  **
!               ****************************************
!
      IF((ICOM.EQ.'RUN'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'SEQU') .OR.   &
        (IHARG(1).EQ.'RUN'.AND.IHARG(2).EQ.'SEQU') .OR.   &
        (IHARG(2).EQ.'RUN'.AND.IHARG(3).EQ.'SEQU'))THEN
        CALL DPRUNS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE RUNS ... PLOT CASE  **
!               ************************************
!
      IF(ICOM.EQ.'RUNS'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPRUPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE ... SPECTRAL PLOT CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'AUTO')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'PLAN')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'I   ')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'1   ')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'ONE ')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'II  ')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'2   ')GO TO 1899
      IF(ICOM.EQ.'PHAS' .AND. IHARG(1).EQ.'TWO ')GO TO 1899
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'SPLIN')GO TO 1899
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'FIT')GO TO 1899
!
      IF(ICOM.EQ.'AUTO' .OR. IHARG(1).EQ.'AUTO')GO TO 1800
      IF(ICOM.EQ.'SPEC' .OR. IHARG(1).EQ.'SPEC')GO TO 1800
      IF(ICOM.EQ.'PERI' .OR. IHARG(1).EQ.'PERI')GO TO 1800
      IF(ICOM.EQ.'COSP' .OR. IHARG(1).EQ.'COSP')GO TO 1800
      IF(ICOM.EQ.'QUAD' .AND. IHARG(1).EQ.'SPEC')GO TO 1800
      IF(IHARG(1).EQ.'QUAD' .AND. IHARG(2).EQ.'SPEC')GO TO 1800
      IF(ICOM.EQ.'CROS'.AND.IHARG(1).EQ.'SPEC')GO TO 1800
      IF(IHARG(1).EQ.'CROS'.AND.IHARG(2).EQ.'SPEC')GO TO 1800
      IF(ICOM.EQ.'COHE' .OR. IHARG(1).EQ.'COHE')GO TO 1800
      IF(ICOM.EQ.'AMPL' .OR. IHARG(1).EQ.'AMPL')GO TO 1800
      IF(ICOM.EQ.'PHAS' .OR. IHARG(1).EQ.'PHAS')GO TO 1800
      IF(ICOM.EQ.'GAIN' .OR. IHARG(1).EQ.'GAIN')GO TO 1800
      IF(ICOM.EQ.'ARGA' .OR. IHARG(1).EQ.'ARGA')GO TO 1800
      GO TO 1899
!
 1800 CONTINUE
      CALL DPSPEC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 1899 CONTINUE
!
!               *********************************************
!               **  TREAT THE 3-D ... FREQUENCY PLOT CASE  **
!               *********************************************
!
!  NOTE: THIS COMMAND IS NOT IMPLEMENTED YET.
!
!CCCC IF(ICOM.EQ.'3D' .AND. IHARG(1).EQ.'FREQ')THEN
!
!CCCC   CALL DP3DFR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1              IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!CCCC ENDIF
!CCCC IF(IFOUND.EQ.'YES')GO TO 9000
!
!2099 CONTINUE
!
!               ****************************************
!               **  TREAT THE 3-D ... HISTOGRAM CASE  **
!               ****************************************
!
!  NOTE: THIS COMMAND IS NOT IMPLEMENTED YET.
!
!CCCC IF(ICOM.EQ.'3D' .AND. IHARG(1).EQ.'HIST')THEN
!CCCC   CALL DP3DHI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1              IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!CCCC ENDIF
!CCCC IF(IFOUND.EQ.'YES')GO TO 9000
!
!2199 CONTINUE
!
!               *******************************
!               **  TREAT THE 3-D PLOT CASE  **
!               *******************************
!
      IF(ICOM.EQ.'3D' .OR. ICOM.EQ.'3DPL' .OR.   &
        (ICOM.EQ.'3' .AND. IHARG(1).NE.'PARA'))THEN
        CALL DP3DPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,IFORSW,MAXNPP,   &
                    IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,   &
                    IFOUND,IERROR)
!
        IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
          WRITE(ICOUT,1933)IFOUND,IERROR,IAND1,IAND2
 1933     FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      IF(IFOUND.EQ.'YES')GO TO 9000
!CCCC IF(IAND2.EQ.'YES')GO TO 100
!CCCC IF(IAND2.EQ.'NO')GO TO 9000
!
!               ***********************************************
!               **  TREAT THE BOX-COX NORMALITY        PLOT  **
!               **  TREAT THE BOX-COX LINEARITY        PLOT  **
!               **  TREAT THE BOX-COX HOMOSCEDASTICITY PLOT  **
!               ***********************************************
!
      IF(   &
        (ICOM.EQ.'BOX' .AND. IHARG(1).EQ.'COX') .OR.   &
         (IHARG(1).EQ.'BOX' .AND. IHARG(2).EQ.'COX'))THEN
         CALL DPBCNP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                     IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE PROPORTION PLOT CASE  **
!               **  = THE ANOP PLOT CASE            **
!               **************************************
!
      IF(   &
        (ICOM.EQ.'PROP'.AND.IHARG(1).EQ.'PLOT') .OR.   &
        (ICOM.EQ.'ANOP'.AND.IHARG(1).EQ.'PLOT') .OR.   &
        (ICOM.EQ.'ANAL'.AND.IHARG(1).EQ.'OF  ' .AND.   &
         IHARG(2).EQ.'PROP'.AND.IHARG(3).EQ.'PLOT') .OR.   &
        ICOM.EQ.'MULT')THEN
!
        CALL DPANPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,MAXNPP,   &
                    ANOPL1,ANOPL2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               ************************************
!               **  TREAT THE BAR PLOT CASE       **
!               ************************************
!
      IF(ICOM.EQ.'BAR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')   &
      GO TO 2600
      IF(ICOM.EQ.'BAR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 2600
      GO TO 2699
!
 2600 CONTINUE
      CALL DPBARP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 2699 CONTINUE
!
!               *******************************
!               **  TREAT THE FFT PLOT CASE  **
!               *******************************
!
!CCCC IF(ICOM.EQ.'FFT'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')
!CCCC1GO TO 2700
!CCCC GO TO 2799
!
!2700 CONTINUE
!CCCC CALL DPFFTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES')GO TO 9000
!
!2799 CONTINUE
!
!               ************************************
!               **  TREAT THE ... ROOTOGRAM CASE  **
!               ************************************
!
      IF(ICOM.EQ.'ROOT')GO TO 2800
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ROOT')GO TO 2800
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ROOT')GO TO 2800
      GO TO 2899
!
 2800 CONTINUE
!CCCC CALL DPROGR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1CLLIMI,CLWIDT,
!CCCC1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      CLLIMI,CLWIDT,   &
      IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,IHSTMC,IHSTOP,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 2899 CONTINUE
!
!               ********************************************
!               **  TREAT THE STEM AND LEAF DIAGRAM CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'STEM')THEN
        CALL DPSTEM(IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************************
!               **  TREAT THE ALLAN VARIANCE PLOT CASE             **
!               **  TREAT THE ALLAN STANDARD DEVIATION PLOT CASE   **
!               *****************************************************
!
      IF(ICOM.EQ.'ALLA' .OR. ICOM.EQ.'AV' .OR. ICOM.EQ.'ASD' .OR.   &
         ICOM.EQ.'AS  ' .OR.   &
         IHARG(1).EQ.'ALLA' .OR. IHARG(1).EQ.'AV  ' .OR.   &
         IHARG(1).EQ.'ASD ' .OR. IHARG(1).EQ.'AS  ')THEN
        CALL DPALLA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  TREAT THE COMPLEX REMODULATION PLOT CASE      **
!               ****************************************************
!
      IF(ICOM.EQ.'REMO')GO TO 3300
      IF(ICOM.EQ.'COMP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'REMO')GO TO 3300
      GO TO 3399
!
 3300 CONTINUE
      CALL DPREMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IANGLU,DEMOFR,DEMODF,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 3399 CONTINUE
!
!               ************************************
!               **  TREAT THE SYMMETRY PLOT CASE  **
!               ************************************
!
      IF(ICOM.EQ.'SYMM' .OR. IHARG(1).EQ.'SYMM')THEN
        CALL DPSYMM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************************
!               **  TREAT THE BOX-COX SYMMETRY PLOT CASE  **
!               ********************************************
!
!NNNN IF(NUMARG.GE.2.AND.ICOM.EQ.'BOX'.AND.
!NNNN1IHARG(1).EQ.'COX'.AND.IHARG(2).EQ.'SYMM')GO TO 4200
!NNNN GO TO 4299
!
!4200 CONTINUE
!NNNN CALL DPBCSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!NNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!NNNN IF(IFOUND.EQ.'YES')GO TO 9000
!
!4299 CONTINUE
!
!               *********************************************
!               **  TREAT THE QUANTILE-QUANTILE PLOT CASE  **
!               *********************************************
!
      IF(ICOM.EQ.'QUAN' .OR.   &
        ((ICOM.EQ.'HIGH'.OR.ICOM.EQ.'SUBS') .AND.   &
          IHARG(1).EQ.'QUAN'))THEN
        CALL DPQUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,MAXNPP,IBOOSS,ISEED,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE BAG               PLOT CASE  **
!               *********************************************
!
!     THIS IS NOT YET IMPLEMENTED.
!
      IF(ICOM.EQ.'BAG ')THEN
!CCCC   CALL DPBAGP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1              ISEED,MAXNPP,
!CCCC1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!CCCC   IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************************
!               **  TREAT THE HOMOSCEDASTICITY PLOT CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'REPL'.AND.IHARG(1).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'MULT'.AND.IHARG(1).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'SUMM'.AND.   &
         IHARG(2).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'HOMO'.AND.   &
         IHARG(2).EQ.'SUMM')GO TO 4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'SUBS'.AND.   &
         IHARG(2).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'SUMM'.AND.   &
         IHARG(2).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'HOMO'.AND.   &
         IHARG(2).EQ.'SUMM')GO TO 4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HIGH'.AND.   &
         IHARG(2).EQ.'HOMO')GO TO 4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO'.AND.   &
         IHARG(2).EQ.'HIGH')GO TO 4400
      IF(ICOM.EQ.'SUMM'.AND.IHARG(1).EQ.'HOMO'.AND.   &
         IHARG(2).EQ.'SUBS')GO TO 4400
      GO TO 4499
!
 4400 CONTINUE
      CALL DPHOMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      ISEED,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 4499 CONTINUE
!
!               ***************************************
!               **  TREAT THE BIHISTOGRAM PLOT CASE  **
!               ***************************************
!
      IF(ICOM.EQ.'BIHI' .OR. IHARG(1).EQ.'BIHI' .OR.   &
         IHARG(2).EQ.'BIHI' .OR. IHARG(3).EQ.'BIHI' .OR.   &
         ICOM.EQ.'BIRO' .OR. IHARG(1).EQ.'BIRO' .OR.   &
         IHARG(2).EQ.'BIRO' .OR. IHARG(3).EQ.'BIRO' .OR.   &
         (ICOM.EQ.'BIAS' .AND. ICOM2.EQ.'H   ') .OR.   &
         IHARG(1).EQ.'BIAS' .OR. IHARG(2).EQ.'BIAS')THEN
        CALL DPBIHI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    CLLIMI,CLWIDT,   &
                    IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE YOUDEN PLOT CASE    **
!               ************************************
!
!NNNN IF(ICOM.EQ.'YOUDEN')GO TO 4700
!NNNN GO TO 4799
!
!4700 CONTINUE
!NNNN CALL DPYOUD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!NNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!NNNN IF(IFOUND.EQ.'YES')GO TO 9000
!
!4799 CONTINUE
!
!               ************************************
!               **  TREAT THE GANOVA PLOT CASE    **
!               ************************************
!
!NNNN IF(ICOM.EQ.'GANO'.AND.ICOM2.EQ.'VA  ')GO TO 4800
!NNNN GO TO 4899
!
!4800 CONTINUE
!NNNN CALL DPGANO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!NNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!NNNN IF(IFOUND.EQ.'YES')GO TO 9000
!
!4899 CONTINUE
!
!               *************************************
!               **  TREAT THE DRAFTSMAN PLOT CASE  **
!               *************************************
!
!NNNN IF(ICOM.EQ.'DRSF')GO TO 6100
!NNNN GO TO 6199
!
!6100 CONTINUE
!NNNN CALL DPDRAF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!NNNN1IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!NNNN IF(IFOUND.EQ.'YES')GO TO 9000
!
!6199 CONTINUE
!
!               ***********************************
!               **  TREAT THE CONTOUR PLOT CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'CONT'.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPCOPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,MAXNPP,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
        IF(IBUGGR.EQ.'ON'.OR.ISUBRO.EQ.'INGR')THEN
           WRITE(ICOUT,6233)IFOUND,IERROR,IAND1,IAND2
 6233      FORMAT('IFOUND,IERROR,IAND1,IAND2 = ',3(A4,2X),A4)
           CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************************
!               **  TREAT THE BOX-COX  STANDARDIZED EFFECTS PLOT CASE  **
!               *********************************************************
!
      IF(NUMARG.GE.3.AND.ICOM.EQ.'BOX'.AND.   &
      IHARG(1).EQ.'COX'.AND.IHARG(2).EQ.'STAN'.AND.   &
      IHARG(3).EQ.'EFFE')GO TO 6400
      GO TO 6499
!
 6400 CONTINUE
!CCCC CALL DPBCSE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES')GO TO 9000
!
 6499 CONTINUE
!
!               ************************************
!               **  TREAT THE WEIBULL  PLOT CASE  **
!               ************************************
!
!     OCTOBER 2013: ADD FRECHET PLOT
!
      IF((ICOM.EQ.'WEIB' .OR. ICOM.EQ.'FREC') .AND.   &
         IHARG(1).EQ.'PLOT')GO TO 6510
      IF(ICOM.EQ.'HIGH' .AND.   &
         (IHARG(1).EQ.'WEIB' .OR. IHARG(1).EQ.'FREC') .AND.   &
         IHARG(2).EQ.'PLOT')GO TO 6510
      IF(ICOM.EQ.'SUBS' .AND.   &
         (IHARG(1).EQ.'WEIB' .OR. IHARG(1).EQ.'FREC') .AND.   &
         IHARG(2).EQ.'PLOT')GO TO 6510
      GO TO 6599
!
 6510 CONTINUE
      CALL DPWEIB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  IANGLU,MAXNPP,   &
                  IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                  IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6599 CONTINUE
!
!CCCCC ADD FOLLOWING COMMAND DECEMBER 1999.
!               ****************************************************
!               **  TREAT THE CROSS TABULATE <STATISTIC> PLOT CASE**
!               ****************************************************
!
      IF(NUMARG.GE.2.AND.ICOM.EQ.'CROS'.AND.IHARG(1).EQ.'TABU')THEN
        DO 16602 I=2,NUMARG
          IF(IHARG(I).EQ.'PLOT')GO TO 16600
16602   CONTINUE
        GO TO 16699
!
16600   CONTINUE
        CALL DPCRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
          MAXNXT,   &
          ISEED,   &
          ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
16699 CONTINUE
!
!               ***********************************
!               **  TREAT THE PROFILE PLOT CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'PROF')GO TO 6700
      GO TO 6799
!
 6700 CONTINUE
      CALL DPPROF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6799 CONTINUE
!
!               ***********************************
!               **  TREAT THE STAR    PLOT CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'STAR')GO TO 6800
      GO TO 6899
!
 6800 CONTINUE
      CALL DPSTAR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6899 CONTINUE
!
!               **********************************
!               **  TREAT THE PARETO PLOT CASE  **
!               **********************************
!
      IF(ICOM.EQ.'PARE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'PLOT')GO TO 6900
      GO TO 6999
!
 6900 CONTINUE
      IDIREC='DECR'
!CCCC THE FOLLOWING ARGUMENT LIST WAS AUGMENTED   DECEMBER 1994
      CALL DPPARE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      ICONT,IDIREC,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 6999 CONTINUE
!
!               *************************************
!               **  TREAT THE ERROR BAR PLOT CASE  **
!               *************************************
!
      IF(ICOM.EQ.'ERRO')GO TO 7100
      GO TO 7199
!
 7100 CONTINUE
      IF(IHARG(1).EQ.'PROB' .AND. IHARG(2).EQ.'PLOT')GO TO 7199
      IF(IHARG(1).EQ.'PPCC' .AND. IHARG(2).EQ.'PLOT')GO TO 7199
      IF(IHARG(1).EQ.'KOLM' .AND. IHARG(2).EQ.'SMIR')GO TO 7199
      IF(IHARG(1).EQ.'CHI ' .AND. IHARG(2).EQ.'SQUA')GO TO 7199
      IF(IHARG(1).EQ.'CHIS' .AND. IHARG(2).EQ.'GOOD')GO TO 7199
      CALL DPERBA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 7199 CONTINUE
!
!               *************************************
!               **  TREAT THE FRACTAL PLOT CASE    **
!               *************************************
!
      IF(ICOM.EQ.'FRAC' .AND.   &
        (IHARG(1).EQ.'ITER' .OR. IHARG(1).EQ.'TYPE'))THEN
        CALL DPFRAC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,   &
                    IANGLU,ISEED,   &
                    IFRAIT,IFRATY,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS CHANGED FROM POINCARE PLOT   JULY 1989
!CCCC TO PHASE PLANE DIAGRAM                                 JULY 1989
!               ******************************************
!               **  TREAT THE PHASE PLANE DIAGRAM CASE  **
!               ******************************************
!
      IF(NUMARG.GE.2.AND.ICOM.EQ.'PHAS'.AND. IHARG(1).EQ.'PLAN' .AND.   &
        (IHARG(2).EQ.'DIAG' .OR. IHARG(2).EQ.'PLOT'))THEN
        CALL DPPPD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************************
!               **  TREAT THE JACKNIFE  ... STATISTIC PLOT CASE **
!               **  AND   THE BOOTSTRAP ... STATISTIC PLOT CASE **
!               **************************************************
!
!CCCC SEPTEMBER 2003: ADD BCA BOOTSTRAP/JACKNIFE
!
      IF(ICOM.EQ.'JACK')GO TO 7400
      IF(ICOM.EQ.'BOOT')GO TO 7400
      IF(ICOM.EQ.'BCA'.AND.   &
        (IHARG(1).EQ.'BOOT'.OR.IHARG(1).EQ.'JACK'))GO TO 7400
      GO TO 7499
!
 7400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FIT')GO TO 7499
!
      IF(ICOM.EQ.'BCA')THEN
        ICOM=IHARG(1)
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IBCABT='ON'
      ENDIF
!
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.5.AND.IHARG(5).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.6.AND.IHARG(6).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.7.AND.IHARG(7).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.8.AND.IHARG(8).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.9.AND.IHARG(9).EQ.'PLOT')GO TO 7410
      IF(NUMARG.GE.10.AND.IHARG(10).EQ.'PLOT')GO TO 7410
      GO TO 7499
 7410 CONTINUE
      CALL DPJBSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IBOOSS,ISEED,IBCABT,   &
      MAXNXT,   &
      ICAPSW,ICAPTY,IFORSW,   &
      CLLIMI,CLWIDT,   &
      ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 7499 CONTINUE
!
!               ****************************************
!               **  TREAT THE DEX CONTOUR PLOT CASE   **
!               ****************************************
!
      IF(ICOM.EQ.'DEX'.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'CONT'.AND.   &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPDCNT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE YATES CUBE  PLOT CASE   **
!               ****************************************
!
      IF((ICOM.EQ.'DEX'.OR.ICOM.EQ.'YATE').AND.NUMARG.GE.2.AND.   &
         IHARG(1).EQ.'CUBE'.AND.IHARG(2).EQ.'PLOT')THEN
        CALL DPYACB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE DEX/DOE ... PLOT CASE   **
!               ****************************************
!
      IF(ICOM.EQ.'DEX ' .OR. ICOM.EQ.'DEXP' .OR. ICOM.EQ.'DOE ' .OR.   &
         ICOM.EQ.'DOX ' .OR. ICOM.EQ.'CLAS')THEN
        CALL DPDEXP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    MAXNXT,ISEED,ICONT,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE TAIL AREA PLOT CASE     **
!               **  (A SYNONYM IS SURVIVAL PLOT)      **
!               **  (MAY 1989)                        **
!               ****************************************
!
      IF(ICOM.EQ.'TAIL' .OR. ICOM.EQ.'SURV' .OR.   &
         IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV')THEN
        CALL DPTAIL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED JULY 2017
!               **************************************************
!               **  TREAT THE NORMAL KERNEL DENSITY MIXTURE     **
!               **  PLOT CASE                                   **
!               **************************************************
!
      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'KERN' .AND.   &
         IHARG(2).EQ.'DENS' .AND. IHARG(3).EQ.'MIXT' .AND.   &
         IHARG(4).EQ.'PLOT')THEN
          CALL DPNMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED MAY 1998
!               **************************************************
!               **  TREAT THE <NORMAL/LOGNORMAL/WEIBULL/HAZARD> **
!               **  PLOT CASE                                   **
!               **************************************************
!
      IF(ICOM.EQ.'NORM'.OR.ICOM.EQ.'LOGN'.OR.ICOM.EQ.'EXPO'.OR.   &
        ICOM.EQ.'WEIB'.OR.ICOM.EQ.'GUMB')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HAZA'.AND.   &
           IHARG(2).EQ.'PLOT')THEN
          CALL DPHAZA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IANGLU,MAXNPP,   &
                      IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                      IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                      IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,   &
                      IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICOM.EQ.'EXTR'.AND.IHARG(1).EQ.'VALU')THEN
        IF(NUMARG.GE.3.AND.IHARG(2).EQ.'HAZA'.AND.   &
           IHARG(3).EQ.'PLOT')THEN
          CALL DPHAZA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IANGLU,MAXNPP,   &
                      IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                      IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                      IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,   &
                      IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED MAY 1990
!               ************************************
!               **  TREAT THE NORMAL   PLOT CASE  **
!               ************************************
!
      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'PLOT')GO TO 7710
      IF(ICOM.EQ.'HIGH' .AND. IHARG(1).EQ.'NORM' .AND.   &
         IHARG(2).EQ.'PLOT')GO TO 7710
      IF(ICOM.EQ.'SUBS' .AND. IHARG(1).EQ.'NORM' .AND.   &
         IHARG(2).EQ.'PLOT')GO TO 7710
      GO TO 7799
!
 7710 CONTINUE
      CALL DPNORM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  IANGLU,MAXNPP,   &
                  IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                  IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 7799 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED    APRIL 1992 (JJF)
!               *********************************
!               **  TREAT THE BLOCK PLOT CASE  **
!               *********************************
!
!CCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT   JUNE 1992  JJF
!CCCC TO ACCOMODATE THE <STAT> BLOCK PLOTS   JUNE 1992   JJF
!CCCC IF(NUMARG.GE.1.AND.ICOM.EQ.'BLOC'.AND.
!CCCC1IHARG(1).EQ.'PLOT')GO TO 7800
!CCCC GO TO 7899
!
!CCCC THE FOLLOWING 10 LINES WERE ADDED TO AVOID     AUGUST 1993
!CCCC A CONFLICT WITH   MEIDAN POLISH   COMMAND      AUGUST 1993
      IF(ICOM.EQ.'ROBU'.AND.IHARG(1).EQ.'SMOO')GO TO 7899
      IF(NUMARG.GE.1)THEN
         IF((ICOM.EQ.'BLOC'.OR.ICOM.EQ.'ROBU').AND.   &
            IHARG(1).EQ.'PLOT')GO TO 7800
      ENDIF
      IF(NUMARG.GE.2)THEN
         IF((IHARG(1).EQ.'BLOC'.OR.IHARG(1).EQ.'ROBU').AND.   &
           IHARG(2).EQ.'PLOT')GO TO 7800
      ENDIF
      IF(NUMARG.GE.3)THEN
         IF((IHARG(2).EQ.'BLOC'.OR.IHARG(2).EQ.'ROBU').AND.   &
           IHARG(3).EQ.'PLOT')GO TO 7800
      ENDIF
!CCCC FOLLOWING 3 LINES ADDED MARCH 1995.
      IF(NUMARG.GE.4)THEN
         IF((IHARG(3).EQ.'BLOC'.OR.IHARG(3).EQ.'ROBU').AND.   &
            IHARG(4).EQ.'PLOT')GO TO 7800
      ENDIF
      GO TO 7899
!
 7800 CONTINUE
!CCCC MARCH 1995.  ADD MAXNXT TO ARGUMENT LIST.
!CCCC MARCH 2019.  ADD ICHMAP TO ARGUMENT LIST.
      CALL DPBLOC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  BARHEF,BARWEF,MAXNXT,ISEED,ICHMAP,ICONT,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 7899 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED OCTOBER 1991 (JJF)
!               *********************************
!               **  TREAT THE PHD PLOT CASE    **
!               *********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'PHD'.AND.   &
      IHARG(1).EQ.'PLOT')THEN
!CCCC   CALL DPPHDP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1              TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
!CCCC1              ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED    AUGUST 1992 (ALAN)
!               *********************************
!               **  TREAT THE VECTOR PLOT CASE **
!               *********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VECT'.AND.   &
         IHARG(1).EQ.'PLOT')THEN
        CALL DPVECT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IVCFMT,IVCARR,IANGLU,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED    AUGUST 1992 (ALAN)
!               *********************************
!               **  TREAT THE SYMBOL PLOT CASE **
!               *********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SYMB'.AND.   &
         IHARG(1).EQ.'PLOT')THEN
        CALL DPPLSY(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED    NOVEMBER 1992 (ALAN)
!               **********************************
!               **  TREAT THE ANDREWS PLOT CASE **
!               **********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'ANDR'.AND.   &
         IHARG(1).EQ.'PLOT')THEN
!CCCC   PANINC=0.1
        CALL DPANDR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ANDINC,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED    MARCH 2003 (ALAN)
!               ***********************************************
!               **  TREAT THE PARALLEL COORDINATES PLOT CASE **
!               ***********************************************
!
      IF(NUMARG.GE.2.AND.ICOM.EQ.'PARA'.AND.   &
      IHARG(1).EQ.'COOR'.AND.IHARG(2).EQ.'PLOT')THEN
        CALL DPPCPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(NUMARG.GE.3.AND.ICOM.EQ.'GROU'.AND.   &
             IHARG(1).EQ.'PARA'.AND. IHARG(2).EQ.'COOR'.AND.   &
             IHARG(3).EQ.'PLOT')THEN
        CALL DPPCPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED      DECEMBER 1993
!               ******************************************
!               **  TREAT THE Q ... CONTROL CHART CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'Q' .AND. IHARG(1).NE.'QUAN')THEN
        CALL DPQCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED      DECEMBER 1993
!               ************************************************
!               **  TREAT THE CME PLOT CASE                   **
!               **  TREAT THE COND. ... EXCEEDANCE PLOT CASE  **
!               ************************************************
!
!  MAY 1998.  CHECK FOR CONFLICT WITH "CME ESTIMATE" OR
!             "CME GENERALIZED PARETO".
      IF(ICOM.EQ.'CME')GO TO 8500
      IF(ICOM.EQ.'COND')GO TO 8500
      IF(ICOM.EQ.'YANG')GO TO 8500
      IF(ICOM.EQ.'LIFE')GO TO 8500
      IF(ICOM.EQ.'MEAN')GO TO 8500
      GO TO 8599
!
 8500 CONTINUE
      IF(NUMARG.GE.1.AND.(IHARG(1).EQ.'ESTI'.OR.IHARG(1).EQ.'GENE'))   &
      GO TO 8599
      CALL DPCME(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 8599 CONTINUE
!
!               *******************************************
!               **  TREAT THE ... COMOVEMENT  PLOT CASE  **
!               *******************************************
!
! NOTE: FOLD COMOVEMENT PLOT IN WITH CORRELATION PLOT.
!
!CCCC IF(ICOM.EQ.'AUTO' .OR. ICOM.EQ.'CROS' .OR. ICOM.EQ.'COMO' .OR.
!CCCC1   IHARG(1).EQ.'AUTO' .OR. IHARG(1).EQ.'CROS' .OR.
!CCCC1   IHARG(1).EQ.'COMO')THEN
!CCCC   CALL DPCOMV(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
!CCCC1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!CCCC   IF(IFOUND.EQ.'YES')GO TO 9000
!CCCC ENDIF
!
!               ****************************************
!               **  TREAT THE KAPLAN MEIER PLOT CASE  **
!               **  (MAY 1998)                        **
!               ****************************************
!
      IF(ICOM.EQ.'KAPL' .OR. ICOM.EQ.'MODI')THEN
        CALL DPKAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE TOTAL TIME ON TEST PLOT **
!               **  CASE (JUNE 2020)                  **
!               ****************************************
!
      IF(ICOM.EQ.'TOTA' .OR. ICOM.EQ.'TTT ')THEN
        CALL DPTTTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE FISHER DISCRIMINATION PLOT **
!               **  CASE (JULY 2024)                     **
!               *******************************************
!
      IF(ICOM.EQ.'FISH')THEN
        CALL DPDISC(NPLOTV,NPLOTP,ICASPL,ICAPSW,IFORSW,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE K NEAREST NEIGHBORS        **
!               **  DISCRIMINATION PLOT CASE (JULY 2024) **
!               *******************************************
!
      IF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'NEAR')THEN
        CALL DPKNND(NPLOTV,NPLOTP,ICASPL,ICAPSW,IFORSW,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE DUANE        PLOT CASE  **
!               **  (MAY 1998)                        **
!               ****************************************
!
      IF(ICOM.EQ.'DUAN')THEN
        CALL DPDUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE EMPIRICAL CDF PLOT CASE **
!               **  (MAY 1998)                        **
!               ****************************************
!
      IF(ICOM.EQ.'EMPI' .OR. ICOM.EQ.'ECDF')THEN
        CALL DPECDF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE SEASONAL SUBSERIES PLOT CASE **
!               **  (FEBRUARY 1999)                        **
!               *********************************************
!
      IF(ICOM.EQ.'SEAS' .OR. IHARG(1).EQ.'SEAS')THEN
        CALL DPSESB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE SPREAD-LOCATION    PLOT CASE **
!               **  (AUGUST   1999)                        **
!               *********************************************
!
      IF(ICOM.EQ.'SPRE' .OR. IHARG(1).EQ.'SPRE')THEN
        CALL DPSLOC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE TUKEY MEAN-DIFFERENCE PLOT CASE **
!               ************************************************
!
      IF((ICOM.EQ.'TUKE'.AND.IHARG(1).NE.'LAMB') .OR.   &
         (ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'TUKE') .OR.   &
         (ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'TUKE'))THEN
        CALL DPTUMD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,MAXNPP,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE SHIFT                 PLOT CASE **
!               ************************************************
!
      IF(ICOM.EQ.'SHIF' .OR.   &
        (ICOM.EQ.'HIGH' .AND. IHARG(1).EQ.'SHIF') .OR.   &
        (ICOM.EQ.'SUBS' .AND. IHARG(1).EQ.'SHIF'))THEN
        CALL DPSHPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,MAXNPP,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES' .OR. IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE BLAND ALTMAN          PLOT CASE **
!               ************************************************
!
      IF((ICOM.EQ.'BLAN'.AND.IHARG(1).EQ.'ALTM') .OR.   &
         (ICOM.EQ.'HIGH'.AND.IHARG(1).EQ.'BLAN') .OR.   &
         (ICOM.EQ.'SUBS'.AND.IHARG(1).EQ.'BLAN'))THEN
        CALL DPBAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IANGLU,ISEED,IBOOSS,MAXNPP,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE COMMUTABILITY         PLOT CASE **
!               ************************************************
!
      IF(ICOM.EQ.'COMM'.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPCPLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ICAPSW,ICAPTY,IFORSW,                  &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'COMM'.AND.IHARG(1).EQ.'DIFF'.AND.    &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPCPLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ICAPSW,ICAPTY,IFORSW,                  &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE INTERACTION           PLOT CASE **
!               ************************************************
!
      IF(ICOM.EQ.'INTE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'PLOT')GO TO 9500
      GO TO 9599
!
 9500 CONTINUE
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGG2,IERROR)
      CALL DPINPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      MAXNPP,   &
      ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 9599 CONTINUE
!
!               ****************************************************
!               **  TREAT THE ... STATISTIC INTERACTION PLOT CASE **
!               ****************************************************
!
      IF(NUMARG.LT.2)GO TO 9699
      DO 9602 I=1,NUMARG-1
        IF(IHARG(I).EQ.'INTE'.AND.IHARG(I+1).EQ.'PLOT')GO TO 9600
 9602 CONTINUE
      IF(NUMARG.LT.3)GO TO 9699
      DO 9604 I=1,NUMARG-2
        IF(IHARG(I).EQ.'INTE'.AND.IHARG(I+1).EQ.'STAT'.AND.   &
           IHARG(I+2).EQ.'PLOT')GO TO 9600
 9604 CONTINUE
      GO TO 9699
!
 9600 CONTINUE
      CALL DPISP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      MAXNXT,   &
      ISEED,   &
      ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')GO TO 9000
!
 9699 CONTINUE
!
!               *******************************************
!               **  TREAT THE KERNEL DENSITY PLOT  CASE  **
!               *******************************************
!
      IF((ICOM.EQ.'KERN' .OR. ICOM.EQ.'DENS') .OR.   &
         IHARG(1).EQ.'KERN' .OR. IHARG(2).EQ.'KERN' .OR.   &
         IHARG(3).EQ.'KERN')THEN
        CALL DPKDEN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IKDENP,PKDEWI,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE RIDGELINE      PLOT  CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'RIDG' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPRIPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,         &
                    IKDENP,PKDEWI,ISEED,                         &
                    CLLIMI,CLWIDT,                               &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'RIDG' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
        CALL DPRIPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,         &
                    IKDENP,PKDEWI,ISEED,                         &
                    CLLIMI,CLWIDT,                               &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE LORENZ CURVE         CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'LORE' .OR.   &
         IHARG(1).EQ.'LORE' .OR. IHARG(2).EQ.'LORE' .OR.   &
         IHARG(3).EQ.'LORE')THEN
        CALL DPLORE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE H CONSISTENCY PLOT   CASE  **
!               *******************************************
!
      IF(   &
        (ICOM.EQ.'H   ' .AND. IHARG(1).EQ.'CONS' .AND.   &
         IHARG(2).EQ.'PLOT') .OR.   &
        (ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'CONS' .AND.   &
         IHARG(2).EQ.'PLOT') .OR.   &
        (ICOM.EQ.'COCH' .AND. IHARG(1).EQ.'VARI' .AND.   &
         IHARG(2).EQ.'PLOT')   &
        )THEN
        CALL DPHKCP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE TWO FACTOR    PLOT   CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'FACT' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPTWFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE DOT MATRIX    PLOT   CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'DOT ' .AND. IHARG(1).EQ.'MATR' .AND.       &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPDMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,     &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'DOT ' .AND. IHARG(1).EQ.'MATR' .AND.   &
             IHARG(2).EQ.'PROP' .AND. IHARG(3).EQ.'PLOT')THEN
        CALL DPDMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,     &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'WAFF' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPDMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,     &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE CONSENSUS MEAN PLOT  CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'CONS')THEN
        IF(NUMARG.GE.2.AND.   &
          IHARG(1).EQ.'MEAN'.AND.IHARG(2).EQ.'PLOT')THEN
          CALL DPCMPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      ICAPSW,ICAPTY,   &
                      IFORSW,ISEED,IBOOSS,   &
                      ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *********************************************
!               **  TREAT THE PARTIAL REGRESSION PLOT CASE **
!               **  TREAT THE PARTIAL RESIDUAL   PLOT CASE **
!               **  TREAT THE PARTIAL LEVERAGE   PLOT CASE **
!               *********************************************
!
      IF(ICOM.EQ.'PART')THEN
        IF(NUMARG.GE.2.AND.   &
          (IHARG(1).EQ.'REGR'.AND.IHARG(2).EQ.'PLOT') .OR.   &
          (IHARG(1).EQ.'RESI'.AND.IHARG(2).EQ.'PLOT') .OR.   &
          (IHARG(1).EQ.'LEVE'.AND.IHARG(2).EQ.'PLOT'))THEN
          ICASPL='PREG'
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'ADDE')THEN
        ICASPL='PREG'
        IF(NUMARG.GE.2.AND.   &
          IHARG(1).EQ.'VARI'.AND.IHARG(2).EQ.'PLOT')THEN
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'COMP')THEN
        ICASPL='PREG'
        IF(NUMARG.GE.3.AND.   &
          IHARG(1).EQ.'PLUS'.AND.IHARG(2).EQ.'RESI'.AND.   &
          IHARG(3).EQ.'PLOT')THEN
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'CCPR')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
          ICASPL='CCPR'
          CALL DPPREG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *****************************************
!               **  TREAT THE ... INFLUENCE CURVE CASE **
!               *****************************************
!
      IF(NUMARG.GE.2)THEN
        DO 9710 I=1,NUMARG-1
          IF(IHARG(I).EQ.'INFL' .AND. IHARG(I+1).EQ.'CURV')THEN
            CALL DPINCU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                 MAXNXT,   &
                 ISEED,   &
                 ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES')GO TO 9000
            GO TO 9719
          ENDIF
 9710   CONTINUE
      ENDIF
 9719 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED      APRIL    2005
!               ************************************************
!               **  TREAT THE PEAKS OVER THRESHOLD PLOT  CASE **
!               **            POT                  PLOT       **
!               ************************************************
!
      IF(ICOM.EQ.'PEAK')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'OVER'.AND.   &
           IHARG(2).EQ.'THRE'.AND.IHARG(3).EQ.'PLOT')THEN
          CALL DPPOTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBOOSS,ISEED,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'POT ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
          CALL DPPOTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IBOOSS,ISEED,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *************************************************
!               **  TREAT THE REPAIR PLOT CASE                 **
!               **  (OCTOBER  2006)                            **
!               *************************************************
!
      IF(ICOM.EQ.'REPA')THEN
        CALL DPRPLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************************
!               **  TREAT THE MEAN REPAIR  FUNCTION PLOT CASE  **
!               **  (OCTOBER  2006)                            **
!               *************************************************
!
      IF(ICOM.EQ.'MEAN' .OR. ICOM.EQ.'AVER')THEN
        CALL DPMRFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE TRILINEAR PLOT CASE.  **
!               **************************************
!
      IF(ICOM.EQ.'TRIL' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPTRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE ROC         PLOT CASE.  **
!               **  TREAT THE PSUEDO ROC  PLOT CASE.  **
!               ****************************************
!
      IF(ICOM.EQ.'ROC ')THEN
        CALL DPROC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'PSUE' .AND. IHARG(1).EQ.'ROC ')THEN
        CALL DPROC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE ROSE      PLOT CASE.  **
!               **************************************
!
      IF(ICOM.EQ.'ROSE')THEN
        CALL DPROSE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE BIVARIATE NORMAL TOLERANCE **
!               **  REGION   PLOT CASE.                  **
!               *******************************************
!
      IF(ICOM.EQ.'BIVA' .OR. ICOM.EQ.'POIN')THEN
        CALL DPBNTR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE BINARY    PLOT CASE.  **
!               **************************************
!
      IF(ICOM.EQ.'BINA' .AND. IHARG(1).NE.'TABU')THEN
        CALL DPBIPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE ORD           PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'ORD ')THEN
        CALL DPORD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE POISSON       PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'POIS' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'GEOM' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'BINO' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'LOGA' .AND. IHARG(1).EQ.'SERI' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'NEGA' .AND. IHARG(1).EQ.'BINO' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
        CALL DPPOIS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   CLLIMI,CLWIDT,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE ASSOCIATION   PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'ASSO' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPASSO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE SIEVE         PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'SIEV' .AND. IHARG(1).EQ.'PLOT')THEN
        CALL DPSIEV(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE LEVEL         PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'LEVE' .AND. IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        CALL DPLEPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'DISC' .AND. IHARG(1).EQ.'CONT' .AND.   &
        IHARG(2).EQ.'PLOT')THEN
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        CALL DPLEPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE IMAGE         PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'IMAG' .AND. IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        CALL DPIMAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************************
!               **  TREAT THE SPATIAL DISTRIBUTION  PLOT CASE  **
!               *************************************************
!
      IF(ICOM.EQ.'SPAT' .AND. IHARG(1).EQ.'DIST' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        CALL DPSDPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE FLUCUATION    PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'FLUC')THEN
        CALL DPFLUC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE STRIP         PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'STRI'.AND.IHARG(1).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'STRI'.AND.   &
             IHARG(2).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
!
!     FOLLOWING SECTION ADDED TO SUPPORT "BATCH MULTIPLE"
!     OPTION FOR STRIP PLOT--10/2009
!
      ELSEIF(ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'MULT'.AND.   &
             IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'MULT'.AND.IHARG(1).EQ.'BATC'.AND.   &
             IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPSTRI(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE DETECTION LIMIT PLOT CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'DETE'.AND.IHARG(1).EQ.'LIMI'.AND.   &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPDLPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'NORM'.AND.IHARG(1).EQ.'DETE'.AND.   &
             IHARG(2).EQ.'LIMI'.AND.IHARG(3).EQ.'PLOT')THEN
        CALL DPDLPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,   &
                    ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE TABULATION    PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'TABU' .OR.   &
         (ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'TABU'))THEN
        CALL DPTAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                   IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************************
!               **  TREAT THE ISO 13528 ZSCORE PLOT CASE  **
!               **            ISO 13528 JSCORE PLOT CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.   &
        (IHARG(2).EQ.'ZSCO' .OR. IHARG(2).EQ.'JSCO').AND.   &
         IHARG(3).EQ.'PLOT')THEN
        CALL DPZSCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE ISO 13528     PLOT CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        CALL DPISOP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  TREAT THE ISO 13528 RLP PLOT  CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'ISO ' .AND. IHARG(1).EQ.'1352' .AND.   &
         IHARG(2).EQ.'RLP' .AND. IHARG(3).EQ.'PLOT')THEN
        CALL DPRLPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************************
!               **  TREAT THE TWO-WAY <ROW/COLUMN> PLOT  CASE  **
!               *************************************************
!
      IF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'WAY ')THEN
        CALL DPTWPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                    ICAPSW,ICAPTY,IFORSW,   &
                    IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  END OF SEARCH FOR GRAPHICS COMMANDS  **
!               *******************************************
      IFOUND='NO'
      IERROR='NO'
      GO TO 9001
!
!               *******************************************
!               **  STEP 90A--                           **
!               **  DO THE FOLLOWING FOR ALL PLOTS:      **
!               **  1) SAVE SOME INTERNAL PARAMETERS     **
!               **  2) IMPLEMENT SUB-REGIONS             **
!               *******************************************
!
 9000 CONTINUE
      IF(IFOUND.EQ.'NO')GO TO 9001
      IF(IERROR.EQ.'YES')GO TO 9001
      IF(NPLOTP.LT.1)GO TO 9001
      IF(ICASPL(1:2).EQ.'3D')GO TO 9001
!
!  FIND PLOT MIN AND MAX AND CORRESPONDING INDEX AND SAVE AS
!  INTERNAL PARAMETERS.
!
      AYMIN=CPUMAX
      AYMAX=CPUMIN
      AXMIN=CPUMAX
      AXMAX=CPUMIN
      IYMIN=0
      IYMAX=0
      IXMIN=0
      IXMAX=0
      DO 10001 I=1,NPLOTP
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
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='YMAX'
      IH2='INDE'
      VALUE0=REAL(IYMAX)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='YMIN'
      VALUE0=AYMIN
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='YMIN'
      IH2='INDE'
      VALUE0=REAL(IYMIN)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='XMAX'
      VALUE0=AXMAX
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='XMAX'
      IH2='INDE'
      VALUE0=REAL(IXMAX)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='PLOT'
      IH2='XMIN'
      VALUE0=AXMIN
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='XMIN'
      IH2='INDE'
      VALUE0=REAL(IXMIN)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
!
!  FIND CORRELATION OF PLOT POINTS.  FIND 2 CORRELATIIONS:
!  ONE WITH ALL POINTS, ONE WITH TAGPLO=1.
!
      IWRITE='OFF'
      CALL CORR(Y,X,NPLOTP,IWRITE,ACORR,IBUGG3,IERROR)
      IH='PLOT'
      IH2='CORR'
      VALUE0=ACORR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      J=0
      DO 10101 I=1,NPLOTP
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
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
!
!  IMPLEMENT SUB-REGIONS
!
      NUMSBR=0
      DO 10200 I=MAXSUB,1,-1
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
10212       FORMAT('      THE NUMBER OF PLOT POINTS WOULD EXCEED ',   &
                   'MAXIMUM OF ',I8,'.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,10214)NPLOTP
10214       FORMAT('      THE CURRENT NUMBER OF PLOT POINTS = ',I8)
            CALL DPWRST('XXX','BUG ')
            GO TO 10299
          ELSE
            DO 10220 II=NPLOTP,1,-1
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
!
            TEMPZZ(1)=AXMIN
            TEMPZZ(2)=AXMAX
            NZTEMP=2
            ICASZZ='X'
            CALL DPFRLI(ICASZZ,IFRALI,TEMPZZ,NZTEMP,                 &
                        GX1MIN,GX1MAX,GY1MIN,GY1MAX,                 &
                        IX1TSC,IX1TSW,IY1TSC,IY1TSW,                 &
                        IX1JSW,IY1JSW,                               &
                        NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW,   &
                        PX1COO,X1COOR,NX1COO,                        &
                        PY1COO,Y1COOR,NY1COO,                        &
                        PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,          &
                        PY1CMN,Y1COMN,NY1CMN,PY1TOB,PY1TOT,          &
                        ITICX1,ITICX2,ITICY1,ITICY2,                 &
                        PXMIN,PXMAX,PYMIN,PYMAX,                     &
                        AVALXL,AVALXU,                               &
                        IBUGG3,ISUBRO,IERROR)
!
            IF(ASUBXL(I).EQ.CPUMIN)THEN
!CCCC         X(1)=AXMIN
              X(1)=AVALXL
            ELSE
              X(1)=ASUBXL(I)
            ENDIF
!
            IF(ASUBXU(I).EQ.CPUMAX)THEN
!CCCC         X(2)=AXMAX
              X(2)=AVALXU
            ELSE
              X(2)=ASUBXU(I)
            ENDIF
!
            IF(ASUBXU(I).EQ.CPUMAX)THEN
!CCCC         X(3)=AXMAX
              X(3)=AVALXU
            ELSE
              X(3)=ASUBXU(I)
            ENDIF
!
            IF(ASUBXL(I).EQ.CPUMIN)THEN
!CCCC         X(4)=AXMIN
              X(4)=AVALXL
            ELSE
              X(4)=ASUBXL(I)
            ENDIF
!
            TEMPZZ(1)=AYMIN
            TEMPZZ(2)=AYMAX
            NZTEMP=2
            ICASZZ='Y'
            CALL DPFRLI(ICASZZ,IFRALI,TEMPZZ,NZTEMP,               &
                        GX1MIN,GX1MAX,GY1MIN,GY1MAX,               &
                        IX1TSC,IX1TSW,IY1TSC,IY1TSW,               &
                        IX1JSW,IY1JSW,                             &
                        NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW, &
                        PX1COO,X1COOR,NX1COO,                      &
                        PY1COO,Y1COOR,NY1COO,                      &
                        PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,        &
                        PY1CMN,Y1COMN,NY1CMN,PY1TOB,PY1TOT,        &
                        ITICX1,ITICX2,ITICY1,ITICY2,               &
                        PXMIN,PXMAX,PYMIN,PYMAX,                   &
                        AVALYL,AVALYU,                             &
                        IBUGG3,ISUBRO,IERROR)
!
            IF(ASUBYL(I).EQ.CPUMIN)THEN
!CCCC         Y(1)=AYMIN
              Y(1)=AVALYL
            ELSE
              Y(1)=ASUBYL(I)
            ENDIF
!
            IF(ASUBYL(I).EQ.CPUMIN)THEN
!CCCC         Y(2)=AYMIN
              Y(2)=AVALYL
            ELSE
              Y(2)=ASUBYL(I)
            ENDIF
!
            IF(ASUBYU(I).EQ.CPUMAX)THEN
!CCCC         Y(3)=AYMAX
              Y(3)=AVALYU
            ELSE
              Y(3)=ASUBYU(I)
            ENDIF
!
            IF(ASUBYU(I).EQ.CPUMAX)THEN
!CCCC         Y(4)=AYMAX
              Y(4)=AVALYU
            ELSE
              Y(4)=ASUBYU(I)
            ENDIF
!
            X(5)=X(1)
            Y(5)=Y(1)
            DO 10225 JJ=1,5
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
        IF(NSTRT.GT.NPLOTP)GO TO 10299
        NTOT=0
        NACC=0
        NREJ=0
        XLOW=X(1)
        XHIGH=X(2)
        YLOW=Y(1)
        YHIGH=Y(4)
        DO 10260 I=NSTRT,NPLOTP
          NTOT=NTOT+1
          XPNT=X(I)
          YPNT=Y(I)
          IF(   &
            (XPNT.LT.XLOW.OR. XPNT.GT.XHIGH) .OR.   &
            (YPNT.LT.YLOW.OR.YPNT.GT.YHIGH)   &
             )THEN
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
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='NREJ'
      IH2='ECT '
      VALUE0=REAL(NREJ)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
      IH='NTOT'
      IH2='AL  '
      VALUE0=REAL(NTOT)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9001 CONTINUE
!
!     APRIL 2007.  CHECK FOR FATAL ERROR
!
      IERRST=IERROR
!
      IF(IERROR.EQ.'YES')THEN
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,   &
                    ISUBN1,ISUBN2,ICASPL,   &
                    IBUGG2,ISUBRO,IERROR)
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE MAINGR
      SUBROUTINE MAININ(IBUGIN,ICOMHO,ICOMH2,IRSCNT)
!
!     PURPOSE--THIS IS SUBROUTINE MAININ.
!              (THE   IN    AT THE END OF    MAINPC   STANDS FOR   INITIAL
!              THIS SUBROUTINE INITIALIZES ALL NEEDED CONSTANTS
!              FOR THE   AREAS--MC = MACHINE CONSTANTS
!                             --DB = DEBUGGING
!                             --HK = HOUSEKEEPING
!                             --PC = PLOT CONTROL
!                             --OD = OUTPUT DEVICES
!                             --SU = SUPPORT
!                             --GR = GRAPHICS
!                             --AN = ANALYSIS
!                             --DA = DATA
!                             --DG = DIAGRAMMATIC GRAPHICS
!                             --H2 = HOUSEKEEPING (PART 2)
!                             --3D = 3-DIMENSIONAL
!     THIS ROUTINE IS TYPICALLY CALLED ONLY ONCE PER DATAPLOT RUN
!     (IMMEDIATELY AFTER SIGN-ON).
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
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --MAY       1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1986.
!     UPDATED         --SEPTEMBER 1988. GENERAL 3-D
!     UPDATED         --DECEMBER  1988. RESET2
!     UPDATED         --MAY       1989. INITIALIZE DES. OF EXP. COMMON
!     UPDATED         --AUGUST    1990. INITIALIZE WINDOW SYSTEM
!     UPDATED         --DECEMBER  2015. ADD "IRSCNT".  IF IRSCNT > 0,
!                                       DO NOT RESET DEVICE 1 UNDER
!                                       WINDOWS (THIS CAUSES A CRASH
!                                       WITH THE QWIN DEVICE).
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGIN
      CHARACTER*4 ICOMHO
      CHARACTER*4 ICOMH2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
!               ****************************************************************
!               **  STEP 1--
!               **  INITIALIZE VARIABLES AND PARAMETERS.
!               **  11 INITIALIZATION SUBROUTINES ARE CALLED--
!               **        INITMC--INITIALIZE MACHINE CONSTANTS
!               **        INITFO--INITIALIZE FILE OPERATIONS
!               **        INITHK--INITIALIZE HOUSEKEEPING           VARIABLES AN
!               **        INITDA--INITIALIZE DATA                   VARIABLES.
!               **        INITPC--INITIALIZE PLOT CONTROL  COMMANDS VARIABLES AN
!               **        INITDG--INITIALIZE DIAGRAMMATIC GRAPHICS  COMMANDS VAR
!               **        INITOD--INITIALIZE OUTPUT DEVICE COMMANDS VARIABLES AN
!               **        INITSU--INITIALIZE SUPPORT       COMMANDS VARIABLES AN
!               **        INITH2--INITIALIZE HOUSEKEEPING (PART 2)  VARIABLES AN
!               **        INITDB--INITIALIZE DEBUGGING              VARIABLES.
!               **        INIT3D--INITIALIZE 3-DIMENSIONAL          VARIABLES.
!               ****************************************************************
!
      IBUGIN='OFF'
      IFLAG=0
      IF(ICOMHO.EQ.'RESE'.AND.ICOMH2.EQ.'T2  ')IFLAG=1
      IF(IFLAG.EQ.0)THEN
        CALL INITMC(IBUGIN)
        CALL INITFO(IBUGIN)
      ENDIF
!
      CALL INITHK(IBUGIN)
      CALL INITDA(IBUGIN)
      CALL INITPC(IBUGIN)
!CCCC CALL INITDG(IBUGIN)
!     DIAGRAMMATIC GRAPHICS INITIALIZATION  IS NOW DONE (NOV 1983)
!     IN INITPC
!
      IF(IFLAG.EQ.0 .AND. IRSCNT.EQ.0)THEN
        CALL INITOD(IBUGIN)
      ENDIF
!
      CALL INITSU(IBUGIN)
!CCCC THE FOLLOWING DES. OF EXP. LINE WAS ADDED MAY 1989
      CALL INITDE(IBUGIN)
      CALL INIT3D(IBUGIN)
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1990
!CCCC CALL INITWI(IBUGIN)
!
      CALL INITH2(IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
      IVALUE,VALUE,NUMNAM,MAXN,MAXCOL,IBUGIN)
      CALL INITDB
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE MAININ
      SUBROUTINE MAINOD(IBUGOD,IBUGO2,ISUBRO,   &
                        ICAPSW,   &
                        IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAINOD.
!              (THE   OD    AT THE END OF    MAINOD   STANDS FOR   OUTPUT DEVICE
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES OUTPUT DEVICE COMMANDS.
!              THE OUTPUT DEVICE COMMANDS SEARCHED FOR BY MAINOD ARE AS FOLLOWS-
!
!                   1) DEVICE ... POWER                  ON/OFF
!                   2) DEVICE ... MANUFACTURER           A MANUFACTURER AND MODE
!                   3) DEVICE ... CONTINUOUS             ON/OFF
!                   4) DEVICE ... COLOR                  ON/OFF
!                   5) DEVICE ... PICTURE POINTS         2 NUMBERS
!                   6) DEVICE ... UNIT NUMBER            A NUMBER
!
!                   7) TERMINAL   POWER                  ON/OFF
!                   8) TERMINAL   MANUFACTURER           A MANUFACTURER AND MODE
!                   8) TERMINAL   CONTINUOUS             ON/OFF
!                   9) TERMINAL   COLOR                  ON/OFF
!                  10) TERMINAL   PICTURE POINTS         2 NUMBERS
!                   6) TERMINAL UNIT NUMBER            A NUMBER
!
!                  11) POWER                             ON/OFF
!                  12) MANUFACTURER                      A MANUFACTURER AND MODE
!                  13) CONTINUOUS                        ON/OFF
!                  14) COLOR                             ON/OFF
!                  15) PICTURE POINTS                    2 NUMBERS
!                   16) UNIT NUMBER            A NUMBER
!
!                  16) DISCRETE                          ON/OFF
!                  17) DISCRETE NARROW-WIDTH             ON/OFF
!                  18) DISCRETE WIDE-CARRIAGE            ON/OFF
!                  19) BATCH                             ON/OFF
!
!                  20) FILE                              ON/OFF
!                  20) CALCOMP                           ON/OFF
!                  21) VERSATEC                          ON/OFF
!                  22) ZETA                              ON/OFF
!
!                  22) METAFILE                              ON/OFF
!
!                  23) HARDCOPY                          ON/OFF AND OPTIONALLY A
!                  24) PENPLOTTER                        ON/OFF AND OPTIONALLY A
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --FEBRUARY  1989.  2 OFFSET ARGUMENTS IN CALLS TO DPDEMN
!     UPDATED         --FEBRUARY  1989.  ADD CHECKS FOR NEW DEVICES (ALAN)--
!                                           GENERAL CGM (OR CGM)
!                                           QUIC (OR QMS)
!                                           POSTSCRIPT
!                                           PCL (OR LASERJET)
!                                           DICOMED
!     UPDATED         --MARCH     1990.   ADD CHECK FOR X11 DEVICE
!     UPDATED         --MAY       1990.   CHECK FOR [HPGL/ZETA/CALC] PEN MAP,
!                                         DISTINGUISH BETWEEN ON/OFF AND
!                                         OPEN/CLOSE
!     UPDATED         --JANUARY   1991.   ADD REGIS TO PEN MAP COMMAND
!     UPDATED         --MAY       1991.   ADD TURBO-C/VGA (JJF)
!     UPDATED         --JUNE      1991.   ADD X11 TO PEN MAP COMMAND
!     UPDATED         --OCTOBER   1991.   ADD "POSTSCRIPT SHOW FONT" COMMAND
!     UPDATED         --APRIL     1992.   PRINT PLOT, P, PP
!     UPDATED         --MAY       1992.   POSTSCRIPT BLANK PAGE SWITCH
!     UPDATED         --JUNE      1992.   ARGUMENT LIST TO DPDEMN
!     UPDATED         --AUGUST    1992.   ADD "SHOW COLORS" COMMAND.
!     UPDATED         --APRIL     1993.   CHECK FOR CONFLICT WITH
!                                         P CONTROL CHART (ALAN)
!     UPDATED         --OCTOBER   1993.   BUG FOR DISCRETE ON
!     UPDATED         --DECEMBER   1993.  COMMENT OUT   GENERAL
!     UPDATED         --MAY        1994.  CHECK CONFLICT BETWEEN REGIS
!                                         AND REGION
!     UPDATED         --SEPTEMBER  1994.  CHECK CONFLICT BETWEEN DISCR
!                                         AND DISCR UNIFORM PROB PLOT
!     UPDATED         --APRIL      1995.  CHECK CONFLICT BETWEEN POWER
!                                         AND POWER NORMAL AND POWER
!                                         LOGNORMAL (PROB PLOT, PPCC
!                                         PLOT)
!     UPDATED         --OCTOBER    1995.  CHECK CONFLICT BETWEEN GENERAL
!                                         AND GENERALIZED EXTREME VALUE
!                                         AND GENERALIZED HALF LOGISTIC
!                                         (PROB AND PPCC PLOTS)
!     UPDATED         --DECEMBER   1995.  CHECK CONFLICT BETWEEN GENERAL
!                                         AND GENERALIZED LOGISTIC
!     UPDATED         --FEBRUARY   1996.  CHECK CONFLICT BETWEEN GENERAL
!                                         AND GENERALIZED EXPONENTIAL
!     UPDATED         --JULY       1996.  DEVICE ... FONT COMMAND
!     UPDATED         --OCTOBER    1996.  ADD CHECKS FOR NEW DEVICES (ALAN)--
!                                           MICROSOFT QUICKWIN
!                                           PBM (PORTABLE BIT MAP)
!     UPDATED         --JUNE       1998.  NAME CONFLICT WITH POWER MLE
!     UPDATED         --JUNE       2000.  ADD CHECKS FOR NEW DEVICES (ALAN)--
!                                           OPEN-GL
!                                           GD JPEG
!                                           GD PNG
!                                           GD WBMP
!                                           WINDOWS BITMAP
!     UPDATED         --MARCH      2002.  ADD CHECKS FOR NEW DEVICES (ALAN)--
!                                           SVG
!     UPDATED         --SEPTEMBER  2002.  ICAPSW FOR DPDEMN, DPDEPW
!     UPDATED         --SEPTEMBER  2007.  IERRST
!     UPDATED         --SEPTEMBER  2011.  VIEW PLOT COMMAND
!     UPDATED         --OCTOBER    2016.  UPDATES TO VIEW PLOT COMMAND
!     UPDATED         --DECEMBER   2018.  DEVICE SCALE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IOP
      CHARACTER*4 ICAPSW
!
      CHARACTER*4 IBUGOD
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 IFTYPE
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE3
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASEZ
!
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXFNC) :: ICMDTI
      CHARACTER (LEN=MAXFNC) :: IFILEZ
      CHARACTER (LEN=MAXFNC) :: ITEMP
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOF2.INC'
!CCCC THE FOLLOWING LINE WAS ADDED   MAY 1992 (JJF)
      INCLUDE 'DPCODV.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      I=1
      IOP='-999'
      ISUBN1='MAIN'
      ISUBN2='OD  '
      IFOUND='NO'
      IERROR='NO'
!
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
   60   FORMAT('IFOUND,IERROR,ICOM,ICOM2,IPSTBP,NUMARG = ',   &
               5(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  TREAT THE GENERAL (= DEVICE-INDEPENDENT) CASE  **
!               *****************************************************
!
!     CHECK FOR NAME CONFLICTS WITH "GENERAL"
!
      IF(NUMARG.GE.2)THEN
        IF(IHARG(2).EQ.'PROB')GO TO 9000
        IF(IHARG(2).EQ.'PPCC')GO TO 9000
      ELSEIF(NUMARG.GE.1)THEN
        IF(IHARG(1).EQ.'JACC')GO TO 9000
        IF(IHARG(1).EQ.'PARE')GO TO 9000
        IF(IHARG(1).EQ.'LOGI')GO TO 9000
        IF(IHARG(1).EQ.'PPCC')GO TO 9000
        IF(IHARG(1).EQ.'PROB')GO TO 9000
        IF(IHARG(1).EQ.'GAMM')GO TO 9000
        IF(IHARG(1).EQ.'EXTR')GO TO 9000
        IF(IHARG(1).EQ.'HALF')GO TO 9000
        IF(IHARG(1).EQ.'LOGI')GO TO 9000
        IF(IHARG(1).EQ.'EXPO')GO TO 9000
        IF(IHARG(1).EQ.'LAMB')GO TO 9000
        IF(IHARG(1).EQ.'TRAP')GO TO 9000
        IF(IHARG(1).EQ.'MCLE')GO TO 9000
        IF(IHARG(1).EQ.'INVE'.AND.IHARG(2).EQ.'GAUS')GO TO 9000
        IF(IHARG(1).EQ.'ASYM'.AND.IHARG(2).EQ.'LAPL')GO TO 9000
        IF(IHARG(1).EQ.'ASYM'.AND.IHARG(2).EQ.'DOUB')GO TO 9000
        IF(IHARG(1).EQ.'TUKE'.AND.IHARG(2).EQ.'LAMB')GO TO 9000
        IF(IHARG(1).EQ.'LOGA'.AND.IHARG(2).EQ.'SERI')GO TO 9000
        IF(IHARG(1).EQ.'NEGA'.AND.IHARG(2).EQ.'BINO')GO TO 9000
        IF(IHARG(1).EQ.'LOST'.AND.IHARG(2).EQ.'GAME')GO TO 9000
        IF(IHARG(1).EQ.'TOPP'.AND.IHARG(2).EQ.'LEON')GO TO 9000
        IF(IHARG(1).EQ.'TOPP'.AND.IHARG(2).EQ.'AND '.AND.   &
             IHARG(3).EQ.'LEON')GO TO 9000
      ENDIF
!
!     DEVICE PEN MAP CASE
!
      IF((ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'MAP')  .OR.   &
         (ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'PEN')  .OR.   &
         (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'PEN')  .OR.   &
         (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'MAP')  .OR.   &
         (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PEN')  .OR.   &
         (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MAP')  .OR.   &
         (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'PEN')  .OR.   &
         (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'MAP')  .OR.   &
         (ICOM.EQ.'HPGL'.AND.IHARG(1).EQ.'COLO') .OR.   &
         (ICOM.EQ.'HP-G'.AND.IHARG(1).EQ.'COLO') .OR.   &
         (ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'COLO') .OR.   &
         (ICOM.EQ.'CALC'.AND.IHARG(1).EQ.'COLO') .OR.   &
         (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.   &
          IHARG(1).EQ.'MAP')                     .OR.   &
         (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.   &
          IHARG(1).EQ.'PEN')                     .OR.   &
         (ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   '.AND.   &
          IHARG(1).EQ.'COLO')                    .OR.   &
         (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'MAP')  .OR.   &
         (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'PEN')  .OR.   &
         (ICOM.EQ.'X11 '.AND.IHARG(1).EQ.'COLO'))THEN
        CALL DPDEPM(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IBUGO2,ISUBRO,IFOUND,IERROR)
        GO TO 9000
!
!     FOLLOWING LINES ADDED OCTOBER, 1991.  ADD "POSTSCRIPT SHOW FONTS" COMMAND
!
      ELSEIF((ICOM.EQ.'POST'.AND.IHARG(1).EQ.'SHOW') .OR.   &
             (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'LIST') .OR.   &
             (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'PRIN') .OR.   &
             (ICOM.EQ.'POST'.AND.IHARG(1).EQ.'FONT') .OR.   &
             (ICOM.EQ.'SHOW' .AND. IHARG(1).EQ.'FONT'))THEN
        CALL DPDEFN(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IBUGO2,ISUBRO,IFOUND,IERROR)
        GO TO 9000
!
!     SHOW COLORS CASE
!
      ELSEIF(ICOM.EQ.'SHOW' .AND. IHARG(1).EQ.'COLO')THEN
        CALL DPDEPM(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IBUGO2,ISUBRO,IFOUND,IERROR)
      GO TO 9000
!
!     GENERAL DEVICE (METAFILE)
!
      ELSEIF((ICOM.EQ.'GENE' .AND. NUMARG.LT.1) .OR.   &
              ICOM.EQ.'CGM ' .OR.   &
             (ICOM.EQ.'DEVI'.AND.NUMARG.GE.1.AND.   &
              IHARG(1).EQ.'GENE') .OR.   &
             (ICOM.EQ.'DEVI'.AND.NUMARG.GE.1.AND.   &
              IHARG(1).EQ.'INDE'))THEN
        IOP='ON'
        IF(NUMARG.GE.1.AND.IHARG(NUMARG).EQ.'OFF')IOP='OFF'
        ICOM='DEVI'
        ICOM2='CE  '
!
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
!
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
          CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                      IPL1NU,IPL1NA,IPL2NU,IPL2NA,   &
                      IPL1CS,IPL2CS,   &
                      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
                      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
                      NUMDEV,MAXDEV,   &
                      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
                      IDNVOF,IDNHOF,   &
                      ICAPSW,ICAPNU,   &
                      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSE
          IHARG(2)='POWE'
          IHARG2(2)='R   '
          IARGT(2)='WORD'
          IHARG(3)='OFF '
          IHARG2(3)='    '
          IARGT(3)='WORD'
          NUMARG=3
          CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                      IPL1NU,IPL1NA,IPL2NU,IPL2NA,   &
                      IDEFPO,   &
                      NUMDEV,MAXDEV,   &
                      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                      IDNVOF,IDNHOF,   &
                      ICAPSW,ICAPNU,   &
                      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
        GO TO 1099
!
      ELSE
        GO TO 1099
      ENDIF
!
 1099 CONTINUE
!
!CCCC THE FOLLOWING SECTION  WAS ADDED   APRIL 1992  (JJF)
!               ****************************************
!               **  TREAT THE P CASE                  **
!               **  TREAT THE PP CASE                 **
!               **  TREAT THE PRINT PLOT CASE         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGOD.EQ.'ON'.OR.ISUBRO.EQ.'INOD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC APRIL 1993    CHECK FOR CONFLICT WITH P CHART
!CCCC APRIL 1993    AND P CONTROL CHART (ALAN)
      IF(ICOM.EQ.'P'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GO TO 9000
      IF(ICOM.EQ.'P'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CHAR')GO TO 9000
!
      IF(ICOM.EQ.'P' .OR. ICOM.EQ.'PP' .OR.   &
        (NUMARG.GE.1 .AND. ICOM.EQ.'PRIN' .AND.   &
         IHARG(1).EQ.'PLOT' .AND. IHARG2(1).EQ.'    '))THEN
!
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
              GO TO 9000
           ENDIF
        ENDIF
        IFTYPE='POST'
        CALL PRINFI(IPL2NA,IFTYPE,IBUGO2,ISUBRO,IERROR)
        GO TO 9000
!
      ENDIF
!
!CCCC THE FOLLOWING SECTION  WAS ADDED   SEPTEMBER 2011
!               ****************************************
!               **  TREAT THE PSVIEW CASE             **
!               **  (VIEWS DPPL2F.DAT FILE)           **
!               ****************************************
!
      IF(ICOM.EQ.'PSVI' .OR.   &
        (NUMARG.EQ.0 .AND. ICOM.EQ.'SHOW'))THEN
        IFOUND='YES'
!
!       VIEW DEVICE 3 OUTPUT
!
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
               GO TO 9000
             ENDIF
          ENDIF
          ICASE3='IPL2'
          CALL VIEWFI(IPL2NA,ICASE3,IBUGO2,ISUBRO,IERROR)
          GO TO 9000
        ELSEIF((IHARG(1).LE.'DEVI' .AND. IHARG(2).EQ.'2') .OR.   &
               (IHARG(1).EQ.'DPPL' .AND. IHARG2(1)(1:2).EQ.'1F') .OR.   &
               (IHARG(1).EQ.'IPL1' .AND. IHARG2(1).EQ.'NA  '))THEN
!
!       VIEW DEVICE 2 OUTPUT.  SET PSVIEW CLOSE FILE COMMAND SPECIFIES
!       WHETHER USER WANTS TO CLOSE FILE OR NOT.
!
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
                GO TO 9000
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
 7018         FORMAT('      DEVICE 2 OUTPUT FILE HAS NOT BEEN ',   &
                     'CLOSED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7019)
 7019         FORMAT('      THE LAST PLOT MAY NOT BE COMPLETE.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
          ICASE3='IPL1'
          CALL VIEWFI(IPL1NA,ICASE3,IBUGO2,ISUBRO,IERROR)
          GO TO 9000
        ELSEIF(NUMARG.GE.1)THEN
!
!         ARBITRARY FILE NAME
!
          IWORD=2
          MAXTMP=80
          ICASEZ='NULL'
          ICMDTI='THE POSTSCRIPT FILE NAME FOR THE PSVIEW COMMAND = '
          CALL DPEXFN(IANS,IANSLC,ICANS,MAXTMP,IWIDTH,NUMARG,   &
                      ISTRIN,IWORD,ICMDTI,ITEMP,   &
                      ICASEZ,IFILEZ,NCFILE,   &
                      IBUGO2,ISUBRO,IFOUND,IERROR)
          IF(NCFILE.LE.80)THEN
            ICASE3='FILE'
            CALL VIEWFI(ISTRIN,ICASE3,IBUGO2,ISUBRO,IERROR)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7013)
 7013       FORMAT('      THE SPECIFIED FILE NAME HAS MORE THAN 80 ',   &
                   'CHARACTERS.')
            IERROR='YES'
          ENDIF
          GO TO 9000
        ENDIF
      ENDIF
!
!               ***********************************
!               **  PRE-TREAT THE TERMINAL CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'TERM'.AND.IHARG(1).EQ.'CHAR')GO TO 9000
      IF(ICOM.EQ.'TERM')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
      ENDIF
!
!               **************************************************
!               **  TREAT THE DEVICE ... POWER CASE             **
!               **  TREAT THE DEVICE ... CONTINUOUS CASE        **
!               **  TREAT THE DEVICE ... COLOR      CASE        **
!               **  TREAT THE DEVICE ... PICTURE POINTS CASE    **
!               **  TREAT THE DEVICE ... UNIT        CASE       **
!               **  TREAT THE DEVICE ... FONT       CASE        **
!               **  TREAT THE DEVICE ... SCALE      CASE        **
!               **  TREAT THE DEVICE ... HARDWARE               **
!               **                       CHARACTER OFFSET  CASE **
!               **  TREAT THE DEVICE ...  (MANUFACTURER) CASE   **
!               **************************************************
!
      IF(ICOM.EQ.'DEVI' .OR. ICOM.EQ.'TERM')THEN
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IPL1NU,IPL1NA,   &
                    IPL2NU,IPL2NA,   &
                    IDEFPO,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IDNVOF,IDNHOF,   &
                    ICAPSW,ICAPNU,   &
                    IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDECN(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFCN,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDECL(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFDC,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDEPP(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFVP,IDEFHP,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDEUN(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFUN,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDEFT(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFFN,NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,   &
                    IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDESC(IHARG,IARGT,IARG,ARG,NUMARG,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,   &
                    IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
                    PDSCAL,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDEOS(IHARG,IARGT,IARG,ARG,NUMARG,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,   &
                    IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
                    PCHOSH,PCHOSV,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IPL1NU,IPL1NA,   &
                    IPL2NU,IPL2NA,   &
                    IPL1CS,IPL2CS,   &
                    IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
                    IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
                    IDNVOF,IDNHOF,   &
                    ICAPSW,ICAPNU,   &
                    IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS INSERTED BY ALAN.  FEBRUARY 1989
!CCCC MAY, 1990. DISTINGUISH BETWEEN ON/OFF AND OPEN/CLOSE
!               *****************************************************
!               **  TREAT THE DEVICE ... ON/OFF (OR OPEN/CLOSE) CASE*
!               *****************************************************
!
      IF(NUMARG.GE.1)THEN
        IF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.   &
           IHARG(NUMARG).EQ.'OFF')THEN
          IOP='OFF'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.   &
           IHARG(NUMARG).EQ.'CLOS')THEN
          IOP='CLOS'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.   &
           IHARG(NUMARG).EQ.'ON')THEN
          IOP='ON'
        ELSEIF((ICOM.EQ.'DEVI'.OR.ICOM.EQ.'TERM').AND.NUMARG.GE.1.AND.   &
           IHARG(NUMARG).EQ.'OPEN')THEN
          IOP='OPEN'
        ELSE
          GO TO 1799
        ENDIF
!
        IF(NUMARG.LE.1)THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGO2,IERROR)
          IHARG(1)='1   '
          IHARG2(1)='    '
          IARGT(1)='NUMB'
          IARG(1)=1
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGO2,IERROR)
          IHARG(1)=IHARG(2)
          IHARG2(1)=IHARG2(2)
          IARGT(1)=IARGT(2)
          IARG(1)=IARG(2)
        ENDIF
!
        IHARG(2)='POWE'
        IHARG2(2)='ER  '
        IARGT(2)='WORD'
        IHARG(3)=IOP
        IHARG2(3)='    '
        IARGT(3)='WORD'
        NUMARG=3
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IPL1NU,IPL1NA,   &
                    IPL2NU,IPL2NA,   &
                    IDEFPO,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IDNVOF,IDNHOF,   &
                    ICAPSW,ICAPNU,   &
                    IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
 1799 CONTINUE
!
!               ****************************
!               **  TREAT THE POWER CASE  **
!               ****************************
!
!CCCC MAY 1995.  CHECK NAME CONFLICTS
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'NORM')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LOGN')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'EXPO')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LOG ')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'FUNC')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'PROB')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'PPCC')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'MAXI')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'MLE ')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'KS  ')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'KOLM')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'LAW ')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'CHI ')GO TO 9000
      IF(ICOM.EQ.'POWE'.AND.IHARG(1).EQ.'CHIS')GO TO 9000
!
      IF(ICOM.EQ.'POWE')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                    IPL1NU,IPL1NA,IPL2NU,IPL2NA,   &
                    IDEFPO,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IDNVOF,IDNHOF,   &
                    ICAPSW,ICAPNU,   &
                    IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************
!               **  TREAT THE CONTINUITY CASE  **
!               *********************************
!
      IF((ICOM.EQ.'CONT'.AND.ICOM2.EQ.'INUO') .OR.   &
         (ICOM.EQ.'CONT'.AND.ICOM2.EQ.'INUI'))THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDECN(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFCN,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE COLOR CASE  **
!               ****************************
!
      IF(ICOM.EQ.'COLO')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDECL(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFDC,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  TREAT THE PICTURE POINTS CASE  **
!               *************************************
!
      IF(ICOM.EQ.'PICT' .OR. ICOM.EQ.'PP')THEN
        ISHIFT=2
        IF(ICOM.EQ.'PP')ISHIFT=3
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
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
        CALL DPDEPP(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFVP,IDEFHP,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  TREAT THE UNIT NUMBER CASE  **
!               *************************************
!
      IF(ICOM.EQ.'UNIT')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGO2,IERROR)
        IHARG(1)='1   '
        IHARG2(1)='    '
        IARGT(1)='NUMB'
        IARG(1)=1
        IHARG(2)=ICOM
        IHARG2(2)=ICOM2
        IARGT(2)='WORD'
        CALL DPDEUN(IHARG,IARGT,IARG,NUMARG,   &
                    IDEFUN,   &
                    NUMDEV,MAXDEV,   &
                    IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE EXPLICIT MANUFACTURER CASE   **
!               **  (FOR A SUBSET OF AVAILABLE TERMINALS)  **
!               *********************************************
!
      IF(ICOM.EQ.'TEKT'.AND.IHARG(1).NE.'META')GO TO 3600
      IF(ICOM.EQ.'HEWL')GO TO 3600
      IF(ICOM.EQ.'HP')GO TO 3600
      IF(ICOM.EQ.'HPGL')GO TO 3600
      IF(ICOM.EQ.'RAMT')GO TO 3600
      IF(ICOM.EQ.'TELE')GO TO 3600
      IF(ICOM.EQ.'VT')GO TO 3600
      IF(ICOM.EQ.'DEC')GO TO 3600
!CCCC MAY, 1994.  CHECK FOR CONFLICT WITH REGION COMMAND.
!CCCC IF(ICOM.EQ.'REGI')GO TO 3600
      IF(ICOM.EQ.'REGI'.AND.ICOM2.EQ.'S   ')GO TO 3600
      IF(ICOM.EQ.'RAMT')GO TO 3600
!CCCC THE FOLLOWING 5 LINES WERE ADDED BY ALAN.  FEBRUARY 1989
      IF(ICOM.EQ.'SUN')GO TO 3600
      IF(ICOM.EQ.'PCL')GO TO 3600
      IF(ICOM.EQ.'POST')GO TO 3600
!CCCC MARCH 1995.  ADD FOLLOWING 3 LINES
      IF(ICOM.EQ.'ENCA')THEN
        IF(IHARG(1).EQ.'POST'.OR.IHARG(1).EQ.'PS')THEN
          ICOM='POST'
          IHARG(1)='ENCA'
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
          IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GO TO 3600
      ENDIF
!CCCC OCTOBER 1996.  ADD FOLLOWING LINES
      IF(ICOM.EQ.'DISP')THEN
        IF(IHARG(1).EQ.'POST'.OR.IHARG(1).EQ.'PS')THEN
          ICOM='POST'
          IHARG(1)='DISP'
          GO TO 3600
        ENDIF
      ENDIF
!
      IF(ICOM.EQ.'PS  ')THEN
        ICOM='POST'
        GO TO 3600
      ENDIF
      IF(ICOM.EQ.'EPS ')THEN
        IF(IHARG(1).EQ.'POST')THEN
          ICOM='POST'
          IHARG(1)='ENCA'
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
          IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GO TO 3600
      ENDIF
!
      IF(ICOM.EQ.'DICO')GO TO 3600
      IF((ICOM.EQ.'QUIC'.AND.ICOM2.EQ.'KWIN').OR.   &
       (ICOM.EQ.'QUIC'.AND.ICOM2.EQ.'K-WI').OR.   &
       (ICOM.EQ.'MS'.AND.IHARG(1).EQ.'WIND').OR.   &
       (ICOM.EQ.'MICR'.AND.IHARG(1).EQ.'WIND'))THEN
        ICOM='QWIN'
        IHARG(1)='    '
        IARGT(1)='WORD'
        GO TO 3600
      ENDIF
      IF(ICOM.EQ.'QUIC')GO TO 3600
!CCCC FOLLOWING LINE ADDED MARCH 1990 BY ALAN.
      IF(ICOM.EQ.'X11 ')GO TO 3600
!CCCC FOLLOWING 2 LINES ADDED FOR CONFLICT WITH DISCRET UNIFORM
!CCCC PROBABILITY PLOT.   SEPTEMBER 1994.
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'UNIF')GO TO 9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'PROB')GO TO 9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'ARCS')GO TO 9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'WEIB')GO TO 9000
      IF(NUMARG.GE.2.AND.ICOM.EQ.'DISC'.AND.IHARG(1).EQ.'CONT'.AND.   &
         IHARG(2).EQ.'PLOT')GO TO 9000
      IF(ICOM.EQ.'DISC')GO TO 3600
!CCCC NOVEMBER 2008: CHECK FOR CONFLICT WITH "BATCH STRIP PLOT"
      IF(NUMARG.GE.2.AND.ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'STRI'.AND.   &
         IHARG(2).EQ.'PLOT')GO TO 9000
      IF(NUMARG.GE.3.AND.ICOM.EQ.'BATC'.AND.IHARG(1).EQ.'MULT'.AND.   &
         IHARG(2).EQ.'STRI'.AND.IHARG(3).EQ.'PLOT')GO TO 9000
      IF(ICOM.EQ.'BATC')GO TO 3600
!CCCC SEPTEMBER 1997.  CHECK FOR CONFLICT WITH ANDERSON DARLING TEST
!CCCC IF(ICOM.EQ.'ANDE')GO TO 3600
      IF(ICOM.EQ.'ANDE')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DARL')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TEST')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NORM')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WEIB')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LOGI')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GO TO 9000
        GO TO 3600
      ENDIF
      IF(ICOM.EQ.'AJ')GO TO 3600
      IF(ICOM.EQ.'HAZE')GO TO 3600
      IF(ICOM.EQ.'OMRO')GO TO 3600
      IF(ICOM.EQ.'TERM'.AND.ICOM2.EQ.'INET')GO TO 3600
      IF(ICOM.EQ.'TEXA')GO TO 3600
      IF(ICOM.EQ.'TI')GO TO 3600
!CCCC THE FOLLOWING 4 LINES WERE ADDED MAY 1991 (JJF)
                                                                                                                                  
      IF(ICOM.EQ.'TURB')GO TO 3600
      IF(ICOM.EQ.'TC')GO TO 3600
      IF(ICOM.EQ.'VGA')GO TO 3600
      IF(ICOM.EQ.'EGA')GO TO 3600
      IF(ICOM.EQ.'LAHE ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'INTE')THEN
          ICOM='INTE'
          IHARG(1)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'WINT')THEN
          ICOM='WINT'
          IHARG(1)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIN '.AND.   &
               IHARG(2).EQ.'INTE')THEN
          ICOM='WINT'
          IHARG(1)='    '
          IHARG(2)='    '
          NUMARG=0
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIND'.AND.   &
               IHARG(2).EQ.'INTE')THEN
          ICOM='WINT'
          IHARG(1)='    '
          IHARG(2)='    '
          NUMARG=0
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
          IBUGO2,IERROR)
          ICOM='POST'
          IHARG(1)='ENCA'
          IHARG2(1)='    '
          IARGT(1)='WORD'
        ENDIF
        GO TO 3600
      ENDIF
!
      IF(ICOM.EQ.'GKS ')GO TO 3600
      IF(ICOM.EQ.'GD  ')GO TO 3600
      IF(ICOM.EQ.'SVG ')GO TO 3600
      IF(ICOM.EQ.'OPEN'.AND.ICOM2.EQ.'GL  ')THEN
          ICOM='OPGL'
          GO TO 3600
      ENDIF
      IF(ICOM.EQ.'OPEN'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GL  ')THEN
          ICOM='OPGL'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGO2,IERROR)
          GO TO 3600
      ENDIF
!
      GO TO 3699
!
 3600 CONTINUE
!
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGO2,IERROR)
      IHARG(1)='1   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=1
      IHARG(2)=ICOM
      IHARG2(2)=ICOM2
      IARGT(2)='WORD'
!CCCC OCTOBER 1993.  FIX BUG WHERE DISCRETE ON, BATCH ON ACT
!CCCC LIKE DISCRETE OFF, ETC.  STRIP OFF ON ARGUMENT.
      IF(IHARG(2).EQ.'DISC'.OR.IHARG(2).EQ.'BATC')THEN
        IF(NUMARG.GE.3.AND.IHARG(NUMARG).EQ.'ON')THEN
          IHARG(NUMARG)='    '
          NUMARG=NUMARG-1
        ENDIF
      ENDIF
!CCCC END CHANGE
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IPL1CS,IPL2CS,   &
      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3699 CONTINUE
!
!               *********************************************
!               **  TREAT THE DISCRETE CASE                **
!               **  TREAT THE DISCRETE NARROW-WIDTH CASE   **
!               **  TREAT THE DISCRETE WIDE-CARRIAGE CASE  **
!               **  TREAT THE BATCH    CASE                **
!               *********************************************
!
      IF(ICOM.EQ.'DISC')GO TO 4100
      IF(ICOM.EQ.'BATC')GO TO 4100
      GO TO 4199
!
 4100 CONTINUE
      ISHIFT=3
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGO2,IERROR)
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
!CCCC OCTOBER 1993.  FIX BUG WHERE DISCRETE ON, BATCH ON ACT
!CCCC LIKE DISCRETE OFF, ETC.  STRIP OFF ON ARGUMENT.
      IF(IHARG(2).EQ.'DISC'.OR.IHARG(2).EQ.'BATC')THEN
        IF(NUMARG.GE.3.AND.IHARG(NUMARG).EQ.'ON')THEN
          IHARG(NUMARG)='    '
          NUMARG=NUMARG-1
        ENDIF
      ENDIF
!CCCC END CHANGE
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IPL1CS,IPL2CS,   &
      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4199 CONTINUE
!
!
!               *********************************
!               **  TREAT THE PENPLOTTER CASE  **
!               *********************************
!
      IF(ICOM.EQ.'PENP')GO TO 4200
      GO TO 4299
!
 4200 CONTINUE
      IF(NUMARG.LE.0)IOP='ON'
      IF(NUMARG.GE.1)IOP=IHARG(1)
      IF(IOP.EQ.'OPEN')IOP='ON'
      IF(IOP.EQ.'AUTO')IOP='ON'
      IF(IOP.EQ.'DEFA')IOP='ON'
      IF(IOP.EQ.'CLOS')IOP='OFF'
!
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGO2,IERROR)
      IHARG(1)='1   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=1
!
      IF(IOP.EQ.'ON')GO TO 4210
      GO TO 4220
!
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
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IPL1CS,IPL2CS,   &
      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 4299
!
 4220 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IDEFPO,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 4299
!
 4299 CONTINUE
!
!               *******************************
!               **  TREAT THE HARDCOPY CASE  **
!               *******************************
!
      IF(ICOM.EQ.'HARD')GO TO 4300
      GO TO 4399
!
 4300 CONTINUE
      CALL DPHAPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
                  ICOPSW,NUMCOP,   &
                  IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4399 CONTINUE
!
!               ******************************
!               **  TREAT THE FILE   CASE  **
!               **  TREAT THE CALCOMP CASE  **
!               **  TREAT THE VERSATEC CASE  **
!               **  TREAT THE ZETA     CASE  **
!               ******************************
!
      IF(ICOM.EQ.'TEKT'.AND.IHARG(1).EQ.'META')GO TO 5100
      IF(ICOM.EQ.'CALC')GO TO 5100
      IF(ICOM.EQ.'VERS')GO TO 5100
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'CHI ')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'CHIS')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'KS  ')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'KOLM')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PROB')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'PPCC')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MLE ')GO TO 9000
      IF(ICOM.EQ.'ZETA'.AND.IHARG(1).EQ.'MAXI')GO TO 9000
      IF(ICOM.EQ.'ZETA')GO TO 5100
      GO TO 5199
!
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
!
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGO2,IERROR)
      IHARG(1)='2   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=2
!
      IF(IOP.EQ.'ON')GO TO 5110
      GO TO 5120
!
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
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IPL1CS,IPL2CS,   &
      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 5199
!
 5120 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IDEFPO,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 5199
!
 5199 CONTINUE
!
!               ******************************
!               **  TREAT THE GENERAL METAFILE   CASE  **
!               ******************************
!
      IF(ICOM.EQ.'META')GO TO 5200
      IF(ICOM.EQ.'GENE'.AND.IHARG(1).EQ.'META')GO TO 5200
      GO TO 5299
!
 5200 CONTINUE
      IF(NUMARG.LE.0)IOP='ON'
      IF(NUMARG.GE.1)IOP=IHARG(1)
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'META')IOP='ON'
      IF(IOP.EQ.'OPEN')IOP='ON'
      IF(IOP.EQ.'AUTO')IOP='ON'
      IF(IOP.EQ.'DEFA')IOP='ON'
      IF(IOP.EQ.'CLOS')IOP='OFF'
!
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGO2,IERROR)
      IHARG(1)='3   '
      IHARG2(1)='    '
      IARGT(1)='NUMB'
      IARG(1)=3
!
      IF(IOP.EQ.'ON')GO TO 5210
      GO TO 5220
!
 5210 CONTINUE
      IHARG(2)='MANU'
      IHARG2(2)='FACT'
      IARGT(2)='WORD'
      IHARG(3)='META'
      IHARG2(3)='FILE'
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IPL1CS,IPL2CS,   &
      IDEFMA,IDEFMO,IDEFM2,IDEFM3,   &
      IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 5299
!
 5220 CONTINUE
      IHARG(2)='POWE'
      IHARG2(2)='R   '
      IARGT(2)='WORD'
      IHARG(3)='OFF '
      IHARG2(3)='    '
      IARGT(3)='WORD'
      NUMARG=3
      CALL DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,   &
      IPL1NU,IPL1NA,   &
      IPL2NU,IPL2NA,   &
      IDEFPO,   &
      NUMDEV,MAXDEV,   &
      IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,   &
      ICAPSW,ICAPNU,   &
      IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 5299
!
 5299 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED   MAY 1992 (JJF)
!               *********************************
!               **  TREAT THE BLANK PAGE CASE  **
!               *********************************
!
      IF(ICOM.EQ.'BLAN' .AND. IHARG(1).NE.'ALTM')THEN
        CALL DPBLPA(IHARG,NUMARG,   &
                    IPSTBP,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  OUTPUT DEVICE COMMAND NOT FOUND--  **
!               **  BRANCH TO EXIT.                    **
!               *****************************************
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IERRST=IERROR
!
!     SEPTEMBER 2012.  CHECK FOR FATAL ERROR
!
      IF(IERROR.EQ.'YES')THEN
        ICASE2='DEVI'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,   &
                    ISUBN1,ISUBN2,ICASE2,   &
                    IBUGO2,ISUBRO,IERROR)
      ENDIF
!
!
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
!
      RETURN
      END SUBROUTINE MAINOD
      SUBROUTINE MAINPC(IBUGPC,IBUGP2,IBUGQ,ISUBRO,   &
                        IVGMSW,IHGMSW,   &
                        IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
                        IMPARG,   &
                        PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                        IERASV,   &
                        PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                        BARHEF,BARWEF,   &
                        ITIAUT,IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,   &
                        IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAINPC.
!              (THE   PC    AT THE END OF    MAINPC   STANDS FOR   PLOT
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES PLOT CONTROL CO
!              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAINPC ARE AS F
!
!                      ARROW ... COLOR                   A COLOR
!                      ARROW ... COORDINATES             2 NUMBERS
!                      BACKGROUND COLOR                  A COLOR
!                      BELL                              ON/OFF
!                      BOX ... COLOR                     A COLOR
!                      BOX ... CORNER COORDINATES        4 NUMBERS
!                      CHARACTERS                        A LIST OF CHARA
!                      CHARACTER COLORS                  A LIST OF COLOR
!                      CHARACTER SIZES                   A LIST OF NUMBE
!                      CHARACTER FILL                    A LIST OF ON/OF
!                      EYE COORDINATES                   3 NUMBERS
!                      ...FRAME                          ON/OFF
!                      ...FRAME COLOR                    A COLOR
!                      FRAME CORNER COORDINATES          4 NUMBERS
!                      WINDOW CORNER COORDINATES         4 NUMBERS
!                      ...GRID                           ON/OFF
!                      GRID COLOR                        A COLOR
!                      GRID PATTERN                      PATTERN
!                      ...LABEL                          A STRING OF CHA
!                      LABEL COLOR                       A COLOR
!                      LABEL SIZE                        A NUMBER
!                      LEGEND ...                        A STRING OF CHA
!                      LEGEND ... COLOR                  A COLOR
!                      LEGEND ... COORDINATES            2 NUMBERS
!                      LEGEND ... SIZE                   A NUMBER
!                      ...LIMITS                         2 NUMBERS
!                      LINES                             A LIST OF LINE
!                      LINE  COLORS                      A LIST OF COLOR
!                      LINE  THICKNESSES                 A LIST OF THICK
!                      ...LOG                            ON/OFF
!                      MARGIN COLOR                      A COLOR
!                      ...MAXIMUM                        A NUMBER
!                      ...MINIMUM                        A NUMBER
!                      NEGATE                            ON/OFF
!                      ORIGIN COORDINATES                3 NUMBERS
!                      PEDESTAL                          ON/OFF
!                      PEDESTAL COLOR                    A COLOR
!                      PEDESTAL HEIGHT                   A NUMBER
!                      PRE-SORT                          ON/OFF
!                      SEGMENT ... COLOR                 A COLOR
!                      SEGMENT ... COORDINATES           2 NUMBERS
!                      SEQUENCE                          ON/OFF
!                      ...TIC                            ON/OFF
!CCCCC                 ...TIC COLOR                      A COLOR
!CCCCC                 ...TIC DECIMALS                   A NUMBER
!CCCCC                 ...TIC COORDINATES                A LIST OF NUMBE
!                      ...TIC POSITION (JUSTIFICATION)   INSIDE/OUTSIDE/
!                      ...TIC SIZE                       A NUMBER
!                      ...TIC LABELS                     ON/OFF
!                      ...TIC LABEL COLOR                A COLOR
!                      ...TIC LABEL SIZE                 A NUMBER
!                      TITLE                             A STRING OF CHA
!                      TITLE COLOR                       A COLOR
!                      TITLE SIZE                        A NUMBER
!                      VISIBLE                           ON/OFF
!
!                      BAR SWITCH                        A SERIES OF ON/
!                      BAR WIDTH                         A SERIES OF NUM
!                      BAR BASE                          A SERIES OF NUM
!                      BAR BORDER COLOR                  A SERIES OF COL
!                      BAR BORDER THICKNESS              A SERIES OF NUM
!                      BAR BORDER LINE                   A SERIES OF LIN
!                      BAR FILL SWITCH                   A SERIES OF ON/
!                      BAR FILL COLOR                    A SERIES OF COL
!                      BAR PATTERN TYPE                  A SERIES OF PAT
!                      BAR PATTERN COLOR                 A SERIES OF COL
!                      BAR PATTERN SPACING               A SERIES OF NUM
!                      BAR PATTERN THICKNESS             A SERIES OF NUM
!                      BAR PATTERN LINE                  A SERIES OF LIN
!                      BAR TYPES                         A SERIES OF NUMBERS
!
!                      BAR EXPANSION FACTORS             2 NUMBERS
!
!                      REGION BASE                       A SERIES OF NUM
!                      REGION BORDER COLOR               A SERIES OF COL
!                      REGION BORDER THICKNESS           A SERIES OF NUM
!                      REGION BORDER LINE                A SERIES OF LIN
!                      REGION FILL SWITCH                A SERIES OF ON/
!                      REGION FILL COLOR                 A SERIES OF COL
!                      REGION PATTERN TYPE               A SERIES OF PAT
!                      REGION PATTERN COLOR              A SERIES OF COL
!                      REGION PATTERN SPACING            A SERIES OF NUM
!                      REGION PATTERN THICKNESS          A SERIES OF NUM
!                      REGION PATTERN LINE               A SERIES OF LIN
!
!                      TEXT BORDER COLOR                 A SERIES OF COL
!                      TEXT BORDER THICKNESS             A SERIES OF NUM
!                      TEXT BORDER LINE                  A SERIES OF LIN
!                      TEXT FILL SWITCH                  A SERIES OF ON/
!                      TEXT FILL COLOR                   A SERIES OF COL
!                      TEXT PATTERN TYPE                 A SERIES OF PAT
!                      TEXT PATTERN COLOR                A SERIES OF COL
!                      TEXT PATTERN SPACING              A SERIES OF NUM
!                      TEXT PATTERN THICKNESS            A SERIES OF NUM
!                      TEXT PATTERN LINE                 A SERIES OF LIN
!
!                      MAJOR ...TIC MARK NUMBER          A NUMBER
!                      MINOR ...TIC MARK NUMBER          A NUMBER
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --APRIL     1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JULY      1986.
!     UPDATED         --SEPTEMBER 1988. 3D PROJECTION (ORTHOGRAP./PERSPECT.)
!     UPDATED         --SEPTEMBER 1988. INCLUDE DPCO3D.INC
!     UPDATED         --APRIL     1992. BAR EXPANSION FACTORS ... ...
!     UPDATED         --AUGUST    1992. ADD SWITCHES FOR AUTOMATIC
!     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR AUTOMATIC SWITCHES
!     UPDATED         --AUGUST    1999. ARGUMENT LIST TO MAIPC2
!     UPDATED         --SEPTEMBER 2007. IERRST
!     UPDATED         --SEPTEMBER 2012. SET FATAL ERROR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
!
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
!
!CCCC THE FOLLOWING 6 LINES WERE ADDED    SEPTEMBER 1993
      CHARACTER*4 ITIAUT
      CHARACTER*4 IX1AUT
      CHARACTER*4 IX2AUT
      CHARACTER*4 IX3AUT
      CHARACTER*4 IY1AUT
      CHARACTER*4 IY2AUT
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
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
!
      IFOUND='NO'
      IERROR='NO'
!
      CALL MAIPC1(IBUGPC,IBUGP2,IBUGQ,ISUBRO,   &
                  IVGMSW,IHGMSW,   &
                  IMPSW,IMPNR,IMPNC,IMPCO,   &
                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                  IERASV,ICHAOF,ICHADY,ICHAVN,   &
                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                  IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,IRGBMX,   &
                  IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      CALL MAIPC2(IBUGPC,IBUGP2,ISUBRO,   &
                  IVGMSW,IHGMSW,   &
                  IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
                  IMPARG,   &
                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                  IERASV,   &
                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                  ITIAUT,IRGBMX,   &
                  IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      CALL MAIPC3(IBUGPC,IBUGP2,ISUBRO,   &
                  IVGMSW,IHGMSW,   &
                  IMPSW,IMPNR,IMPNC,IMPCO,   &
                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                  IERASV,   &
                  PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                  BARHEF,BARWEF,IRGBMX,   &
                  IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      CALL MAIPC4(IBUGPC,IBUGP2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IERRST=IERROR
!
!     SEPTEMBER 2012.  CHECK FOR FATAL ERROR
!
      IF(IERROR.EQ.'YES')THEN
        ISUBN1='MAIN'
        ISUBN2='IN  '
        ICASE2='INPC'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,   &
                    ISUBN1,ISUBN2,ICASE2,   &
                    IBUGP2,ISUBRO,IERROR)
      ENDIF
!
!
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
!CCCC   THE FOLLOWING 2 LINES WERE ADDED    APRIL 1992
        WRITE(ICOUT,9048)BARHEF,BARWEF
 9048   FORMAT('BARHEF,BARWEF = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END SUBROUTINE MAINPC
      SUBROUTINE MAIPC1(IBUGPC,IBUGP2,IBUGQ,ISUBRO,   &
                        IVGMSW,IHGMSW,   &
                        IMPSW,IMPNR,IMPNC,IMPCO,   &
                        PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                        IERASV,ICHAOF,ICHADY,ICHAVN,   &
                        PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                        IX1AUT,IX2AUT,IX3AUT,IY1AUT,IY2AUT,IRGBMX,   &
                        IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAIPC1.
!              (THE   PC    AT THE END OF    MAIPC1   STANDS FOR PLOT CONTROL
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
!              PLOT CONTROL COMMANDS (PART 1).
!              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAIPC1 ARE AS F
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
!     UPDATED--JULY 1987        LEGEND HW
!     UPDATED--FEBRUARY      1988 FURTHER RESOLVE CONFLICT--MIN VS MIN PLOT
!                                                           AND MAX VS MAX PLO
!     UPDATED--MARCH     1988.  FURTHER RESOLVE CONFLICT--MIN VS MIN PLOT
!                                                         AND MAX VS MAX PLO
!     UPDATED--SEPTEMBER 1988.  MOVE EYE/ORIGIN/PEDESTAL COMMANDS
!                               TO MAIPC4 FOR GENERAL 3-D.
!     UPDATED         --SEPTEMBER 1988.  CHANGE 'BACK' TO 'BACKGROU'
!     UPDATED         --DECEMBER  1988.  LABEL AND LEGEND DEFAULT WIDTH
!     UPDATED         --FEBRUARY  1989.  ADD MANY ATTRIBUTE COMMANDS (ALAN)
!     UPDATED         --MAY       1989.  DES. OF EXP. WIDTH/DEPTH/HOR. AXIS
!     UPDATED         --JULY      1989.  ...LABEL DISPLACEMENT
!     UPDATED         --FEBRUARY  1992. FIX LEGEND DIRECTION CONFLICT
!     UPDATED         --APRIL     1992. IDEXHO TO IDEXHA
!     UPDATED         --AUGUST    1992. ADD SWITCHES FOR AUTOMATIC
!     UPDATED         --AUGUST    1992. BOX SHADOW HEIGHT/WIDTH
!     UPDATED         --AUGUST    1992. BOX FILL COLOR
!     UPDATED         --AUGUST    1992. BOX FILL PATTERN
!     UPDATED         --AUGUST    1992. BOX FILL THICK
!     UPDATED         --AUGUST    1992. BOX FILL GAP
!     UPDATED         --MARCH     1993. BUG IN CALL TO DPBOTH
!     UPDATED         --SEPTEMBER 1993. LOWER CASE LABELS
!     UPDATED         --SEPTEMBER 1993. LOWER CASE LEGENDS
!     UPDATED         --SEPTEMBER 1993. 3-D FRAME SWITCH
!     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR AUTOMATIC SWITCHES
!     UPDATED         --OCTOBER   1993. BACKGROUND COLOR SETS THE
!                                       MARGIN COLOR AS WELL
!     UPDATED         --DECEMBER  1994. EXACT CHARACTER MAPPING
!     UPDATED         --JANUARY   1995. FIX DEFAULT CHAR SIZE
!     UPDATED         --APRIL     1995. CHECK FOR COMMAND CONFLICT
!     UPDATED         --AUGUST    1995. SEGMENT PATTERN, FRAME PATTERN,
!                                       BUG (DASH2, ETC)
!     UPDATED         --NOVEMBER  1997. CALL TO DPLIM
!     UPDATED         --JANUARY   1998. NAME CONFLICTS FOR MAXI, MINI
!     UPDATED         --FEBRUARY  1998. LINE/CHAR <SAVE/RESTORE>
!     UPDATED         --OCTOBER   1999. LABEL JUSTIFICIATION
!     UPDATED         --OCTOBER   1999. LABEL OFFSET
!     UPDATED         --DECEMBER  1999. LEGEND UNITS
!     UPDATED         --OCTOBER   2018. LABEL COORDINATES
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR (MODIFY
!                                       CALL LISTS FOR A NUMBER OF
!                                       ROUTINES)
!     UPDATED         --MARCH     2021. CALL LIST TO DPLIM
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHADY
      CHARACTER*8 ICHAVN
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 ICASCL
!
!CCCC THE FOLLOWING 5 LINES WERE ADDED   SEPTEMBER 1993
      CHARACTER*4 IX1AUT
      CHARACTER*4 IX2AUT
      CHARACTER*4 IX3AUT
      CHARACTER*4 IY1AUT
      CHARACTER*4 IY2AUT
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
!CCCC THE FOLLOWING DES. OF EXP. LINE WAS ADDED MAY 1989
      INCLUDE 'DPCODE.INC'
!CCCC THE FOLLOWING 3D LINE WAS ADDED SEPTEMBER 1993
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
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
!
      IFOUND='NO'
      IERROR='NO'
!
!               ********************************************
!               **  TREAT THE ARROW ... COLOR CASE        **
!               **            ARROW ... PATTERN CASE      **
!               **            ARROW ... THICKNESS CASE    **
!               **            ARROW ... COORDINATES CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'ARRO')THEN
        CALL DPARCL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IDEFCO,MAXARR,IARRCO,IARRC2,IRGBMX,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPARPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                    MAXARR,IARRPA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPARTH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                    MAXARR,PARRTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPARCO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
                    MAXNAM,IANS,IWIDTH,   &
                    MAXARR,PARRXC,PARRYC,NUMARR,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               ***************************************
!               **  TREAT THE BACKGROUND COLOR CASE  **
!               ***************************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'GROU')THEN
        CALL DPBACL(IHARG,IARG,NUMARG,IDEFBK,IRGBMX,IBACCO,IBACC2,   &
                    IFOUND,IERROR)
        IF(IERROR.EQ.'NO')IMARCO=IBACCO
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE BELL CASE  **
!               ***************************
!
      IF(ICOM.EQ.'BELL')THEN
        CALL DPBELL(IHARG,NUMARG,IBELSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************************
!               **  TREAT THE BOX ... CORNER COORDINATES CASE  **
!               **            BOX ... COLOR CASE               **
!               **            BOX ... PATTERN CASE             **
!               **            BOX ... THICKNESS CASE           **
!               **            BOX ... FILL COLOR CASE          **
!               **            BOX ... FILL PATTTERN CASE       **
!               **            BOX ... FILL LINE     CASE       **
!               **            BOX ... FILL THICKNESS CASE      **
!               **            BOX ... FILL GAP       CASE      **
!               **            BOX ... SHADOW HW CASE           **
!               *************************************************
!
      IF(ICOM.EQ.'BOX')THEN
!
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'COOR') .OR.   &
           (NUMARG.GE.2.AND.IHARG(2).EQ.'COOR') .OR.   &
           (NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.   &
            IHARG(2).EQ.'COOR') .OR.   &
           (NUMARG.GE.3.AND.IHARG(2).EQ.'CORN'.AND.   &
            IHARG(3).EQ.'COOR'))THEN
          CALL DPBOCC(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
                      MAXNAM,IANS,IWIDTH,   &
                      MAXBOX,PBOXXC,PBOXYC,NUMBOX,IBUGP2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'COLO') .OR.   &
           (NUMARG.GE.2.AND.IHARG(2).EQ.'COLO'.AND.   &
            IHARG(1).NE.'FILL'.AND.IHARG(1).NE.'RGB'))THEN
          ICASCL='STAN'
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,   &
                      MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.   &
            IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,   &
                      MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NUMARG.GE.3.AND.IHARG(2).EQ.'RGB '.AND.   &
           IHARG(3).EQ.'COLO'.AND.IHARG(1).NE.'FILL')THEN
          ICASCL='RGB'
          IF(IARGT(1).EQ.'NUMB')THEN
            ISHIFT=1
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,ISUBRO,IERROR)
          ELSE
            ISHIFT=2
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
          ENDIF
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,   &
                      MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NUMARG.GE.4.AND.IHARG(3).EQ.'RGB '.AND.   &
           IHARG(4).EQ.'COLO'.AND.IHARG(2).NE.'FILL')THEN
          ICASCL='RGB'
          IF(IARGT(1).EQ.'NUMB')THEN
            ISHIFT=2
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,ISUBRO,IERROR)
          ELSE
            ISHIFT=3
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
          ENDIF
          CALL DPBOCL(IHARG,IARG,IARGT,NUMARG,IDEFCO,IRGBMX,ICASCL,   &
                      MAXBOX,IBOBCO,IBOBC2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'PATT') .OR.   &
           (NUMARG.GE.2.AND.IHARG(2).EQ.'PATT'.AND.   &
            IHARG(1).NE.'FILL'))THEN
          CALL DPBOPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFFI,   &
                      MAXBOX,IBOBPA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF((NUMARG.GE.1.AND.IHARG(1).EQ.'THIC') .OR.   &
           (NUMARG.GE.2.AND.IHARG(2).EQ.'THIC'.AND.   &
            IHARG(1).NE.'FILL'))THEN
          CALL DPBOTH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                      MAXBOX,PBOPTH,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO')THEN
            ICASCL='STAN'
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,   &
                        IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'COLO')THEN
            ICASCL='STAN'
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,   &
                        IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
        IF(NUMARG.GE.3)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'RGB '.AND.   &
             IHARG(3).EQ.'COLO')THEN
            ICASCL='RGB'
            ISHIFT=1
            CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
            IHARG(1)='FILL'
            IHARG2(1)='    '
            IHARG(2)='COLO'
            IHARG2(2)='    '
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,   &
                        IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.4)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'RGB '.AND.   &
             IHARG(4).EQ.'COLO')THEN
            ICASCL='RGB'
            ISHIFT=1
            IMIN=2
            CALL SHIFL2(ISHIFT,IMIN,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,ISUBRO,IERROR)
            IHARG(2)='FILL'
            IHARG2(2)='    '
            IHARG(3)='COLO'
            IHARG2(3)='    '
            CALL DPBOFC(IHARG,IARGT,IARG,NUMARG,IDEFXC,MAXBOX,   &
                        IBOFCO,IBOFC2,ICASCL,IRGBMX,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'PATT')THEN
            CALL DPBOFP(IHARG,IARGT,IARG,NUMARG,IDEFFI,   &
                        MAXBOX,IBOFPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'PATT')THEN
            CALL DPBOFP(IHARG,IARGT,IARG,NUMARG,IDEFFI,   &
                        MAXBOX,IBOFPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
!
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'LINE')THEN
            CALL DPBOFL(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                        MAXBOX,IBOPPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'LINE')THEN
            CALL DPBOFL(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                        MAXBOX,IBOPPA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'THIC')THEN
            CALL DPBOFT(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                        MAXBOX,PBOFTH,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'THIC')THEN
            CALL DPBOFT(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                        MAXBOX,PBOFTH,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
        IF(NUMARG.GE.2)THEN
          IF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'GAP')THEN
            CALL DPBOFG(IHARG,IARGT,IARG,ARG,NUMARG,PDEFGA,   &
                        MAXBOX,PBOPGA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.3)THEN
          IF(IHARG(2).EQ.'FILL'.AND.IHARG(3).EQ.'GAP')THEN
            CALL DPBOFG(IHARG,IARGT,IARG,ARG,NUMARG,PDEFGA,   &
                        MAXBOX,PBOPGA,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
        IF(NUMARG.GE.1)THEN
          IF(IHARG(1).EQ.'SHAD')THEN
             CALL DPBSHW(IHARG,IARGT,IARG,ARG,NUMARG,PDEFSH,PDEFSW,   &
                         MAXBOX,PBOSHE,PBOSWI,IFOUND,IERROR)
             IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
        IF(NUMARG.GE.2)THEN
          IF(IHARG(2).EQ.'SHAD')THEN
            CALL DPBSHW(IHARG,IARGT,IARG,ARG,NUMARG,PDEFSH,PDEFSW,   &
                        MAXBOX,PBOSHE,PBOSWI,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
      ENDIF
!
!               *************************************************
!               **  TREAT THE FRAME (CORNER) COORDINATES CASE  **
!               *************************************************
!
      IF(ICOM.EQ.'FRAM')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.   &
           IHARG(2).EQ.'COOR')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')THEN
          CONTINUE
        ELSE
          GO TO 1299
        ENDIF
        CALL DPFRCC(IHARG,IHARG2,IARGT,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
                    MAXNAM,IANS,IWIDTH,   &
                    PXMIN,PXMAX,PYMIN,PYMAX,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
 1299 CONTINUE
!
!               **************************************
!               **  TREAT THE FRAME COLOR CASE      **
!               **            FRAME PATTERN CASE    **
!               **            FRAME THICKNESS CASE  **
!               **            FRAME CASE            **
!               **************************************
!
      IF(ICOM.EQ.'XFRA' .OR. ICOM.EQ.'X1FR' .OR. ICOM.EQ.'X2FR' .OR.   &
         ICOM.EQ.'YFRA' .OR. ICOM.EQ.'Y1FR' .OR. ICOM.EQ.'Y2FR' .OR.   &
         ICOM.EQ.'XYFR' .OR. ICOM.EQ.'YXFR' .OR. ICOM.EQ.'FRAM' .OR.   &
         ICOM.EQ.'3DFR')THEN
!
        IF(ICOM.EQ.'3DFR')GO TO 1310
!
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPFRCL(ICOM,IHARG,IARG,NUMARG,   &
                      IDEFCO,ICASCL,IRGBMX,   &
                      IX1FCO,IX2FCO,IY1FCO,IY2FCO,   &
                      IX1FC2,IX2FC2,IY1FC2,IY2FC2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.   &
               IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPFRCL(ICOM,IHARG,IARG,NUMARG,   &
                      IDEFCO,ICASCL,IRGBMX,   &
                      IX1FCO,IX2FCO,IY1FCO,IY2FCO,   &
                      IX1FC2,IX2FC2,IY1FC2,IY2FC2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')THEN
          CALL DPFRPA(ICOM,IHARG,IHARG2,NUMARG,   &
                      IDEFPA,IX1FPA,IX2FPA,IY1FPA,IY2FPA,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')THEN
          CALL DPFRTH(ICOM,IHARG,ARG,NUMARG,   &
                      PDEFTH,PFRATH,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
 1310   CONTINUE
!
        CALL DPFRAM(ICOM,IHARG,NUMARG,   &
                    IX1FSW,IX2FSW,IY1FSW,IY2FSW,FRAM3D,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE GRID  THICKNESS CASE  **
!               **            GRID  COLOR CASE      **
!               **            GRID  PATTERN CASE    **
!               **            GRID CASE             **
!               **************************************
!
      IF(ICOM.EQ.'XGRI' .OR. ICOM.EQ.'YGRI' .OR. ICOM.EQ.'XYGR' .OR.   &
         ICOM.EQ.'YXGR' .OR. ICOM.EQ.'GRID')THEN
!
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')THEN
          CALL DPGRTH(ICOM,IHARG,ARG,NUMARG,   &
                      PDEFTH,PVGRTH,PHGRTH,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPGRCL(ICOM,IHARG,IARG,NUMARG,   &
                      IDEFCO,ICASCL,IRGBMX,   &
                      IVGRCO,IHGRCO,IVGRC2,IHGRC2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.   &
               IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPGRCL(ICOM,IHARG,IARG,NUMARG,   &
                      IDEFCO,ICASCL,IRGBMX,   &
                      IVGRCO,IHGRCO,IVGRC2,IHGRC2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')THEN
          CALL DPGRPA(ICOM,IHARG,IHARG2,NUMARG,   &
                      IDEFPA,IVGRPA,IHGRPA,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSE
!
          CALL DPGRID(ICOM,IHARG,NUMARG,IVGRSW,IHGRSW,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ENDIF
!
      ENDIF
!
!               ********************************************
!               **  TREAT THE LABEL FONT            CASE  **
!               **            LABEL CASE            CASE  **
!               **            LABEL FILL            CASE  **
!               **            LABEL JUSTIFICATION   CASE  **
!               **            LABEL THICKNESS       CASE  **
!               **            LABEL DISPLACEMENT    CASE  **
!               **            LABEL OFFSET          CASE  **
!               **            LABEL ANGLE           CASE  **
!               **            LABEL DIRECTION       CASE  **
!               **            LABEL COLORS          CASE  **
!               **            LABEL SIZES           CASE  **
!               **            LABEL REFERENCE POINT CASE  **
!               **            LABEL                 CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'LABE' .OR. ICOM.EQ.'XLAB' .OR. ICOM.EQ.'X1LA' .OR.   &
         ICOM.EQ.'X2LA' .OR. ICOM.EQ.'X3LA' .OR. ICOM.EQ.'YLAB' .OR.   &
         ICOM.EQ.'Y1LA' .OR. ICOM.EQ.'Y2LA')THEN
!
        CALL DPLAFO(ICOM,IHARG,NUMARG,   &
                    IDEFFO,IX1LFO,IX2LFO,IX3LFO,IY1LFO,IY2LFO,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLACA(ICOM,IHARG,NUMARG,   &
                    IDEFCA,IX1LCA,IX2LCA,IX3LCA,IY1LCA,IY2LCA,   &
                    IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLAFI(ICOM,IHARG,NUMARG,   &
                    IDEFFI,IX1LFI,IX2LFI,IX3LFI,IY1LFI,IY2LFI,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLAJU(ICOM,IHARG,NUMARG,   &
                    IDEFJU,IX1LJU,IX2LJU,IX3LJU,IY1LJU,IY2LJU,   &
                    IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLATH(ICOM,IHARG,ARG,NUMARG,   &
                    PDEFTH,PX1LTH,PX2LTH,PX3LTH,PY1LTH,PY2LTH,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLADS(ICOM,IHARG,ARG,NUMARG,   &
                    PDEFDS,PX1LDS,PX2LDS,PX3LDS,PY1LDS,PY2LDS,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLAOF(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    PDEFOF,PX1LOF,PX2LOF,PX3LOF,PY1LOF,PY2LOF,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLAAN(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    ADEFAN,PX1LAN,PX2LAN,PX3LAN,PY1LAN,PY2LAN,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLADI(ICOM,IHARG,NUMARG,   &
                    IDEFDI,IX1LDI,IX2LDI,IX3LDI,IY1LDI,IY2LDI,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLACL(ICOM,IARG,IHARG,NUMARG,   &
                    IDEFCO,IX1LCO,IX2LCO,IX3LCO,IY1LCO,IY2LCO,   &
                    IX1LC2,IX2LC2,IX3LC2,IY1LC2,IY2LC2,IRGBMX,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLASZ(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    PDEFHE,PDEFWI,   &
                    PX1LHE,PX1LWI,PX1LVG,PX1LHG,   &
                    PX2LHE,PX2LWI,PX2LVG,PX2LHG,   &
                    PX3LHE,PX3LWI,PX3LVG,PX3LHG,   &
                    PY1LHE,PY1LWI,PY1LVG,PY1LHG,   &
                    PY2LHE,PY2LWI,PY2LVG,PY2LHG,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLACO(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    PX1LXC,PX1LYC,PX2LXC,PX2LYC,PX3LXC,PX3LYC,   &
                    PY1LXC,PY1LYC,PY2LXC,PY2LYC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLAB(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                   IX1LTE,NCX1LA,IX1AUT,   &
                   IX2LTE,NCX2LA,IX2AUT,   &
                   IX3LTE,NCX3LA,IX3AUT,   &
                   IY1LTE,NCY1LA,IY1AUT,   &
                   IY2LTE,NCY2LA,IY2AUT,   &
                   IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               ***********************************************
!               **  TREAT THE LEGEND ... FONT          CASE  **
!               **            LEGEND ... CASE          CASE  **
!               **            LEGEND ... JUSTIFICATION CASE  **
!               **            LEGEND ... DIRECTION     CASE  **
!               **            LEGEND ... UNITS         CASE  **
!               **            LEGEND ... FILL          CASE  **
!               **            LEGEND ... THICKNESS     CASE  **
!               **            LEGEND ... ANGLE         CASE  **
!               **            LEGEND ... COLORS        CASE  **
!               **            LEGEND ... COORDINATES   CASE  **
!               **            LEGEND ... SIZES         CASE  **
!               **            LEGEND ... HW            CASE  **
!               **            LEGEND ...               CASE  **
!               ***********************************************
!
      IF(ICOM.EQ.'LEGE')THEN
!
        CALL DPLEFO(IHARG,IARGT,IARG,NUMARG,IDEFFO,   &
                    MAXLEG,ILEGFO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLECA(IHARG,IARGT,IARG,NUMARG,IDEFCA,   &
                    MAXLEG,ILEGCA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEJU(IHARG,IARGT,IARG,NUMARG,IDEFJU,   &
                    MAXLEG,ILEGJU,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEDI(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFDI,   &
                    MAXLEG,ILEGDI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEUN(IHARG,IARGT,IARG,NUMARG,IDEFUZ,   &
                    MAXLEG,ILEGUN,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEFI(IHARG,IARGT,IARG,NUMARG,IDEFFI,   &
                    MAXLEG,ILEGFI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLETH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                    MAXLEG,PLEGTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEAN(IHARG,IARGT,IARG,ARG,NUMARG,ADEFAN,   &
                    MAXLEG,ALEGAN,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLECL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,IDEFCO,   &
                    MAXLEG,ILEGCO,ILEGC2,IRGBMX,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLECO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
                    MAXNAM,IANS,IWIDTH,   &
                    MAXLEG,PLEGXC,PLEGYC,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLESZ(IHARG,IARGT,IARG,ARG,NUMARG,   &
                    PDEFHE,PDEFWI,MAXLEG,   &
                    PLEGHE,PLEGWI,PLEGVG,PLEGHG,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEHW(IHARG,IARGT,IARG,ARG,NUMARG,   &
                    PDEFHE,MAXLEG,PLEGHE,PLEGWI,PLEGVG,PLEGHG,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPLEG(IHARG,IARG,ARG,IARGT,NUMARG,IANS,IANSLC,IWIDTH,   &
                   ILEGNA,ILEGST,ILEGSP,NUMLEG,MAXLEG,   &
                   ILEGTE,NCLEG,MXCLEG,IFOUND,IERROR,IBUGP2)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               ********************************
!               **  TREAT THE ...LIMITS CASE  **
!               ********************************
!
      IF(ICOM.EQ.'XLIM' .OR. ICOM.EQ.'X1LI' .OR. ICOM.EQ.'X2LI' .OR.   &
         ICOM.EQ.'YLIM' .OR. ICOM.EQ.'Y1LI' .OR. ICOM.EQ.'Y2LI' .OR.   &
         ICOM.EQ.'XYLI' .OR. ICOM.EQ.'YXLI' .OR. ICOM.EQ.'LIMI')THEN
!
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'DETE')GO TO 2499
!
        CALL DPLIM(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                   GX1MIN,GX1MAX,GY1MIN,GY1MAX,   &
                   GX2MIN,GX2MAX,GY2MIN,GY2MAX,   &
                   FX1MIN,FX1MAX,FY1MIN,FY1MAX,   &
                   FX2MIN,FX2MAX,FY2MIN,FY2MAX,   &
                   IX1MIN,IX1MAX,IY1MIN,IY1MAX,   &
                   IX2MIN,IX2MAX,IY2MIN,IY2MAX,   &
                   FX1MNZ,FX1MXZ,FX2MNZ,FX2MXZ,   &
                   FY1MNZ,FY1MXZ,FY2MNZ,FY2MXZ,   &
                   GX1MNX,GX1MXX,GY1MNX,GY1MXX,   &
                   GX2MNX,GX2MXX,GY2MNX,GY2MXX,   &
                   IX1MNX,IX1MXX,IY1MNX,IY1MXX,   &
                   IX2MNX,IX2MXX,IY2MNX,IY2MXX,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
 2499 CONTINUE
!
!               ****************************************
!               **  TREAT THE LINE COLORS       CASE  **
!               **  TREAT THE LINE THICKNESS    CASE  **
!               **  TREAT THE LINE UNITS        CASE  **
!               **  TREAT THE LINE              CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'LINE')THEN
!
        IF(IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPLICL(IHARG,IARG,NUMARG,IDEFCO,MAXLIN,ILINCO,ILINC2,   &
                      IRGBMX,ICASCL,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPLICL(IHARG,IARG,NUMARG,IDEFCO,MAXLIN,ILINCO,ILINC2,   &
                      IRGBMX,ICASCL,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'UNIT')THEN
          CALL DPLIUN(IHARG,NUMARG,MAXLIN,ILINTY,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'THIC')THEN
          CALL DPLITH(IHARG,IARGT,ARG,NUMARG,PDEFLT,MAXLIN,PLINTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          IF(ICOM2.NE.'AR  ')THEN
            CALL DPLINE(IHARG,IHARG2,NUMARG,MAXLIN,ILINPA,ILINPO,   &
                        IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
        ENDIF
!
      ENDIF
!
!               *****************************
!               **  TREAT THE ...LOG CASE  **
!               *****************************
!
      IF(ICOM.EQ.'XLOG' .OR. ICOM.EQ.'X1LO' .OR. ICOM.EQ.'X2LO' .OR.   &
         ICOM.EQ.'YLOG' .OR. ICOM.EQ.'Y1LO' .OR. ICOM.EQ.'Y2LO' .OR.   &
         ICOM.EQ.'LOG ' .OR. ICOM.EQ.'LOGL' .OR.   &
        (ICOM.EQ.'XYLO'.AND.ICOM2.EQ.'G   ') .OR.   &
        (ICOM.EQ.'YXLO'.AND.ICOM2.EQ.'G   '))THEN
!
!CCCC   APRIL 1995.  CHECK FOR LOG LOGISTIC PROB PLOT, LOG LOGISTIC PPCC
!CCCC                PLOT (ALSO ENTERED AS LOGLOGISTIC PROB PLOT)
!CCCC   SEPTEMBER 2001.  CHECK FOR LOG DOUBLE EXPO PROB PLOT,
!CCCC                    LOG DOUBLE EPXO PPCC PLOT
!
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROB')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PROB')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PROB')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PROB')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PPCC')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PPCC')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'PPCC')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'PPCC')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KOLM')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KOLM')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KOLM')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KOLM')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KS  ')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KS  ')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KS  ')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KS  ')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANDE')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ANDE')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ANDE')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ANDE')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AD  ')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AD  ')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'AD  ')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'AD  ')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHI ')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHI ')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHI ')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'CHI ')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CHIS')GO TO 2899
        IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CHIS')GO TO 2899
        IF(NUMARG.GE.3.AND.IHARG(3).EQ.'CHIS')GO TO 2899
        IF(NUMARG.GE.4.AND.IHARG(4).EQ.'CHIS')GO TO 2899
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'BETA')GO TO 2899
!
        CALL DPTISC(ICOM,IHARG,NUMARG,   &
                    IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
 2899 CONTINUE
!
!               ***********************************
!               **  TREAT THE MARGIN COLOR CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'MARG'.AND.   &
        (IHARG(1).EQ.'COLO'.OR.IHARG(1).EQ.'RGB '))THEN
        CALL DPMACL(IHARG,IARG,NUMARG,IDEFMC,IRGBMX,IMARCO,IMARC2,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************
!               **  TREAT THE ...MAXIMUM CASE  **
!               *********************************
!
      IF(ICOM.EQ.'XMAX' .OR. ICOM.EQ.'X1MA' .OR. ICOM.EQ.'X2MA' .OR.   &
         ICOM.EQ.'YMAX' .OR. ICOM.EQ.'Y1MA' .OR. ICOM.EQ.'Y2MA' .OR.   &
         ICOM.EQ.'XYMA' .OR. ICOM.EQ.'YXMA' .OR. ICOM.EQ.'MAXI' .OR.   &
         ICOM.EQ.'MAX ')THEN
        IF(NUMARG.GE.1.AND.ICOM.EQ.'MAXI')THEN
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'STAT'.AND.IHARG(2).EQ.'PLOT')   &
            GO TO 3099
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BLOC'.AND.IHARG(2).EQ.'PLOT')   &
            GO TO 3099
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIND'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'CUMU'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MOVI'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GO TO 3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GO TO 3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GO TO 3099
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIXO')GO TO 3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'GRUB')GO TO 3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'TIET')GO TO 3099
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'DIXO')GO TO 3099
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RECO'.AND.IHARG(2).EQ.'LENG')   &
            GO TO 3099
        ENDIF
!
        CALL DPMAX(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                   GX1MAX,GY1MAX,GX2MAX,GY2MAX,   &
                   IX1MAX,IY1MAX,IX2MAX,IY2MAX,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
 3099 CONTINUE
!
!               *********************************
!               **  TREAT THE ...MINIMUM CASE  **
!               *********************************
!
      IF(ICOM.EQ.'XMIN' .OR. ICOM.EQ.'X1MI' .OR. ICOM.EQ.'X2MI' .OR.   &
         ICOM.EQ.'YMIN' .OR. ICOM.EQ.'Y1MI' .OR. ICOM.EQ.'Y2MI' .OR.   &
         ICOM.EQ.'XYMI' .OR. ICOM.EQ.'YXMI' .OR. ICOM.EQ.'MINI' .OR.   &
         ICOM.EQ.'MIN ')THEN
!
        IF(NUMARG.GE.1.AND.ICOM.EQ.'MINI')THEN
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'STAT'.AND.IHARG(2).EQ.'PLOT')   &
            GO TO 3199
          IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BLOC'.AND.IHARG(2).EQ.'PLOT')   &
            GO TO 3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIND'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'CUMU'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MOVI'.AND.IHARG(2).EQ.'STAT'   &
            .AND.IHARG(3).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')GO TO 3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GO TO 3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GO TO 3199
          IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIXO')GO TO 3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'GRUB')GO TO 3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'TIET')GO TO 3199
          IF(NUMARG.GE.2.AND.IHARG(2).EQ.'DIXO')GO TO 3199
        ENDIF
!
        CALL DPMIN(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                   GX1MIN,GY1MIN,GX2MIN,GY2MIN,   &
                   IX1MIN,IY1MIN,IX2MIN,IY2MIN,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
 3199 CONTINUE
!
!               *******************************
!               **  TREAT THE PRE-SORT CASE  **
!               *******************************
!
      IF(ICOM.EQ.'PRE')THEN
        CALL DPPRES(IHARG,NUMARG,ISORSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************************
!               **  TREAT THE ...WEIB (SCALE) AXIS CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'XWEI' .OR. ICOM.EQ.'X1WE' .OR. ICOM.EQ.'X2WE' .OR.   &
         ICOM.EQ.'YWEI' .OR. ICOM.EQ.'Y1WE' .OR. ICOM.EQ.'Y2WE' .OR.   &
        (ICOM.EQ.'XYWE'.AND.ICOM2.EQ.'IB  ') .OR.   &
        (ICOM.EQ.'YXWE'.AND.ICOM2.EQ.'IB  '))THEN
!
        CALL DPTIS2(ICOM,IHARG,NUMARG,   &
                    IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               *********************************************
!               **  TREAT THE SEGMENT ... COLOR CASE       **
!               **            SEGMENT ... PATTERN CASE     **
!               **            SEGMENT ... THICKNESS CASE   **
!               **            SEGMENT ... COORDINATES CASE **
!               *********************************************
!
      IF(ICOM.EQ.'SEGM')THEN
        CALL DPSECL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IDEFCO,MAXSEG,ISEGCO,ISEGC2,IRGBMX,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPSEPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                    MAXSEG,ISEGPA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPSETH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                    MAXSEG,PSEGTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPSECO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,   &
                    MAXSEG,PSEGXC,PSEGYC,NUMSEG,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               *******************************
!               **  TREAT THE SEQUENCE CASE  **
!               *******************************
!
      IF(ICOM.EQ.'SEQU')THEN
        CALL DPSEQ(IHARG,IARGT,IARG,NUMARG,   &
                   ISEQSW,NUMSEQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************************************
!               **  TREAT THE CHARACTER COLORS CASE              **
!               **            CHARACTER FONT   CASE              **
!               **            CHARACTER CASE   CASE              **
!               **            CHARACTER MAPPING CASE             **
!               **            CHARACTER THICKNESS CASE           **
!               **            CHARACTER SIZES CASE               **
!               **            CHARACTER FILL   CASE              **
!               **            CHARACTER WIDTH CASE               **
!               **            CHARACTER JUSTIFICATION CASE       **
!               **            CHARACTER OFFSET CASE              **
!               **            CHARACTER ANGLE CASE               **
!               **            CHARACTER HW (HEIGHT & WIDTH) CASE **
!               **            CHARACTER UNIT   CASE              **
!               **            CHARACTERS CASE                    **
!               ***************************************************
!
      IF(ICOM.EQ.'CHAR')THEN
        IF(IHARG(1).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,   &
                      ICHAC2,IRGBMX,ICASCL,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='COLO'
          IHARG2(1)='    '
          CALL DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,   &
                      ICHAC2,IRGBMX,ICASCL,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'FONT')THEN
          CALL DPCHFO(IHARG,NUMARG,IDEFFO,MAXCHA,ICHAFO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'CASE')THEN
          CALL DPCHCA(IHARG,NUMARG,IDEFCA,MAXCHA,ICHACA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'MAP'.OR.IHARG(1).EQ.'MAPP')THEN
          CALL DPCMAP(IHARG,NUMARG,IDCMAP,ICHMAP,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'THIC')THEN
          CALL DPCHTH(IHARG,ARG,NUMARG,PDEFTH,MAXCHA,PCHATH,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'SIZE' .OR. IHARG(1).EQ.'HEIG')THEN
          CALL DPCHSZ(PDEFHE,MAXCHA,PCHAHE,PCHAWI,PCHAVG,PCHAHG,   &
                      IBUGP2,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          CALL DPCHFI(IHARG,NUMARG,IDEFFI,MAXCHA,ICHAFI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'WIDT'.OR.IHARG(2).EQ.'WIDT')THEN
          CALL DPCHWI(IHARG,IARGT,ARG,NUMARG,   &
                      PDEFWI,MAXCHA,PCHAWI,PCHAHG,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'JUST'.AND.IHARG2(1).EQ.'IFIC')THEN
          CALL DPCHJU(IHARG,NUMARG,MAXCHA,ICHAJU,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF((IHARG(1).EQ.'OFFS'.AND.IHARG2(1).EQ.'ET  ').OR.   &
               (IHARG(1).EQ.'DISP'.AND.IHARG2(1).EQ.'LACE'))THEN
          CALL DPCHOF(IHARG,IARGT,ARG,IARG,NUMARG,   &
                      MAXCHA,PCHAHO,PCHAVO,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'ANGL'.AND.IHARG2(1).EQ.'E   ')THEN
          CALL DPCHAN(MAXCHA,ACHAAN,IBUGP2,IBUGQ,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'HW')THEN
!CCCC     CALL DPCHHW(IHARG,IARGT,ARG,NUMARG,
          CALL DPCHHW(MAXCHA,PCHAHE,PCHAWI,PDEFHE,PDEFWI,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'UNIT')THEN
          CALL DPCHUN(IHARG,NUMARG,MAXCHA,ICHATY,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSE
          CALL DPCHAR(MAXCHA,ICHAPA,ICHAPO,   &
                      IBUGP2,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!CCCC THE FOLLOWING ANIMATION SWITCH CHUNK WAS ADDED APRIL 1989
!               **************************************************
!               **  TREAT THE ANIMATION SWITCH CASE             **
!               **************************************************
!
      IF(ICOM.EQ.'ANIM' .OR. ICOM.EQ.'UNDR')THEN
        CALL DPANIM(IHARG,NUMARG,IANISW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING 3 DES. FOP EXP. SECTIONS WERE ADDED MAY 1989
!               ******************************************
!               **  TREAT THE DEX WIDTH            CASE **
!               **            DEX DEPTH            CASE **
!               **            DEX HORIZONTAL AXIS  CASE **
!               ******************************************
!
      IF(ICOM.EQ.'DEX')THEN
!
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')THEN
          CALL DPDEWI(IHARG,ARG,NUMARG,DEFDEW,   &
                      DEXWID,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'DEPT')THEN
          CALL DPDEDE(IHARG,IARG,NUMARG,IDEDED,   &
                      IDEXDE,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'HORI'.AND.   &
               IHARG(2).EQ.'AXIS')THEN
          CALL DPDEHA(IHARG,NUMARG,IDEFHA,   &
                      IDEXHA,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE MAIPC1
      SUBROUTINE MAIPC2(IBUGPC,IBUGP2,ISUBRO,   &
                        IVGMSW,IHGMSW,   &
                        IMPSW,IMPNR,IMPNC,IMPCO,IMPCO2,   &
                        IMPARG,   &
                        PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                        IERASV,   &
                        PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                        ITIAUT,IRGBMX,   &
                        IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAIPC2.
!              (THE   PC    AT THE END OF    MAIPC2   STANDS FOR PLOT CONTROL
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
!              PLOT CONTROL COMMANDS (PART 2).
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
!     UPDATED  JANUARY    1988.  (OPTIONAL OMISSION OF WORD  MAJOR)
!     UPDATED         --SEPTEMBER 1988. 3D PROJECTION (ORTHOGRAP./PERSPECT.)
!     UPDATED         --SEPTEMBER 1988. PROJECTION (ORTHOGRAPHIC/PERSPECTIVE)
!                       MOVED TO MAIPC4 FOR GENERAL 3-D.
!     UPDATED         --SEPTEMBER 1988.  VISIBLE
!                       MOVED TO MAIPC4 FOR GENERAL 3-D.
!     UPDATED         --DECEMBER 1988.  TIC/TIC LABEL/TITLE SIZE DEFAULT WIDTH
!     UPDATED         --FEBRUARY 1989.  ADDED MANY ATTRIBUTE COMMANDS (ALAN)
!     UPDATED         --JULY     1989.  TITLE DISPLACEMENT
!     UPDATED         --MAY      1990.  TIC MARK OFFSET
!     UPDATED         --AUGUST   1990.  MP FOR MULTIPLOT
!     UPDATED         --AUGUST   1990.  WINDOW SYSTEM
!     UPDATED         --AUGUST   1990.  WINDOW POINTER
!     UPDATED         --AUGUST   1990.  WINDOW SYSTEM COMMON
!     UPDATED         --AUGUST   1991.  TIC LABEL DISPLACEMENT
!     UPDATED         --APRIL    1992.  GRID PATTERN CODE REDUNDANT
!     UPDATED         --AUGUST   1992.  ADD TITLE SWITCH FOR AUTOMATIC
!     UPDATED         --DECEMBER 1992.  FIX CALL TO DPTLDS
!     UPDATED         --SEPTEMBER 1993. LOWER CASE--TIC LABEL CONTENTS
!     UPDATED         --SEPTEMBER 1993. LOWER CASE FOR TITLE
!     UPDATED         --SEPTEMBER 1993. CHAR*4 FOR ITIAUT
!     UPDATED         --AUGUST    1995. DASH2 BUG (VARIOUS)
!     UPDATED         --APRIL     1997. PIXMAP TITLE COMMAND
!     UPDATED         --SEPTEMBER 1998. CALL TO DPMULT
!     UPDATED         --AUGUST    1999. CALL TO DPMULT
!     UPDATED         --NOVEMBER  1999. SUBREGION SWITCH
!     UPDATED         --MAY       2015. EMBED
!                                       EMBDED HW
!                                       EMBDED CORNER COORDINATES
!                                       EMBDED POSITION
!                                       EMBDED HORIZONTAL JUSTIFICATION
!                                       EMBDED VERTICAL JUSTIFICATION
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOWI.INC'
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
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
!
      IFOUND='NO'
      IERROR='NO'
!
!               *************************************************
!               **  TREAT THE ...TIC THICKNESS           CASE  **
!               **  TREAT THE ...TIC                     CASE  **
!               **  TREAT THE ...TIC COLOR               CASE  **
!               **  TREAT THE ...TIC DECIMALS            CASE  **
!               **  TREAT THE ...TIC POSITION            CASE  **
!               **  TREAT THE ...TIC SIZE                CASE  **
!               **  TREAT THE ...TIC OFFSET              CASE  **
!               **  TREAT THE ...TIC LABEL DISPLACEMENT  CASE  **
!               **  TREAT THE ...TIC LABEL FONT          CASE  **
!               **  TREAT THE ...TIC LABEL CASE          CASE  **
!               **  TREAT THE ...TIC LABEL JUSTIFICATION CASE  **
!               **  TREAT THE ...TIC LABEL DIRECTION     CASE  **
!               **  TREAT THE ...TIC LABEL FILL          CASE  **
!               **  TREAT THE ...TIC LABEL THICKNESS     CASE  **
!               **  TREAT THE ...TIC LABEL ANGLE         CASE  **
!               **  TREAT THE ...TIC LABEL               CASE  **
!               **  TREAT THE ...TIC LABEL COLOR         CASE  **
!               **  TREAT THE ...TIC LABEL SIZE          CASE  **
!               **  TREAT THE ...TIC LABEL FORMAT        CASE  **
!               **  TREAT THE ...TIC LABEL CONTENTS      CASE  **
!               **  TREAT THE ...TIC LABEL GAP           CASE  **
!               *************************************************
!
      IF(ICOM.EQ.'XTIC' .OR. ICOM.EQ.'X1TI' .OR.   &
         ICOM.EQ.'X2TI' .OR. ICOM.EQ.'YTIC' .OR.   &
         ICOM.EQ.'Y1TI' .OR. ICOM.EQ.'Y2TI' .OR.   &
         ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
         ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
!
        CALL DPTCTH(ICOM,IHARG,ARG,NUMARG,PDEFTH,PTICTH,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTCCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IDEFCO,IRGBMX,   &
                    IX1TCO,IX2TCO,IY1TCO,IY2TCO,   &
                    IX1TC2,IX2TC2,IY1TC2,IY2TC2,   &
                    IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTCDP(ICOM,IHARG,IARG,NUMARG,IDEFDP,   &
                    IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTCJU(ICOM,IHARG,NUMARG,   &
                    IX1TJU,IX2TJU,IY1TJU,IY2TJU,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTCSZ(ICOM,IHARG,IARGT,ARG,NUMARG,DEFTL,   &
                    PX1TLE,PX2TLE,PY1TLE,PY2TLE,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTCOF(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    DEFTOF,IDEFTU,                 &
                    ITICX1,ITICX2,ITICY1,ITICY2,   &
                    PX1TOL,PX2TOL,PY1TOB,PY2TOB,   &
                    PX1TOR,PX2TOR,PY1TOT,PY2TOT,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTIC(ICOM,IHARG,NUMARG,   &
                   IX1TSW,IX2TSW,IY1TSW,IY2TSW,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!CCCC   DECEMBER 1992.  FIX BUG.  PDEFHG AND PDEFVG ARE THE DEFAULT
!CCCC                   HORIZONTAL AND VERTICAL GAPS, NOT THE DEFAULT
!CCCC                   DISPLACEMENT.
!
        PJUNK=PDEFDS-0.5
        CALL DPTLDS(ICOM,IHARG,IARGT,ARG,NUMARG,PDEFDS,PJUNK,   &
                    PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLFO(ICOM,IHARG,NUMARG,IDEFFO,   &
                    IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLCA(ICOM,IHARG,NUMARG,IDEFCA,   &
                    IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLJU(ICOM,IHARG,NUMARG,IDEFJU,   &
                    IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLDI(ICOM,IHARG,NUMARG,IDEFDI,   &
                    IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLFI(ICOM,IHARG,NUMARG,IDEFFI,   &
                    IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLTH(ICOM,IHARG,ARG,NUMARG,PDEFTH,PTIZTH,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLAN(ICOM,IHARG,ARG,NUMARG,ADEFAN,   &
                    AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IDEFCO,IRGBMX,   &
                    IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO,   &
                    IX1ZC2,IX2ZC2,IY1ZC2,IY2ZC2,   &
                    IBUGPC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLSZ(ICOM,IHARG,IARGT,ARG,NUMARG,PDEFHE,PDEFWI,   &
                    PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,   &
                    PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,   &
                    PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,   &
                    PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLFM(ICOM,IHARG,NUMARG,IDETLF,   &
                    IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTLCN(ICOM,IHARG,NUMARG,IANS,IANSLC,IWIDTH,   &
                    IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPTL(ICOM,IHARG,NUMARG,   &
                  IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW,   &
                  IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!     2019/02: ADD TIC MARK LABEL GAP COMMAND
!
!CCCC   CALL DPTLGA(ICOM,IHARG,IARGT,ARG,NUMARG,
!CCCC1              PX1ZGA,PX2ZGA,PY1ZGA,PY2ZGA,
!CCCC1              IFOUND,IERROR)
!CCCC   IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               **********************************
!               **  TREAT THE TITLE FONT  CASE  **
!               **********************************
!
      IF(ICOM.EQ.'TITL')THEN
        CALL DPTIFO(IHARG,NUMARG,IDEFFO,ITITFO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               **********************************
!               **  TREAT THE TITLE CASE  CASE  **
!               **********************************
!
        CALL DPTICA(IHARG,NUMARG,IDEFCA,ITITCA,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               **************************************
!               **  TREAT THE TITLE THICKNESS CASE  **
!               **************************************
!
        CALL DPTITH(IHARG,ARG,NUMARG,PDEFTH,PTITTH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!CCCC   THE FOLLOWING SECTION WAS ADDED JULY 1989
!               **************************************
!               **  TREAT THE TITLE DISPLACEMENT CASE  **
!               **************************************
!
        CALL DPTIDS(IHARG,ARG,NUMARG,PDEFDS,PTITDS,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               **********************************
!               **  TREAT THE TITLE COLOR CASE  **
!               **********************************
!
        CALL DPTICL(IHARG,IARG,NUMARG,IDEFCO,IRGBMX,ITITCO,ITITC2,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               **********************************
!               **  TREAT THE TITLE SIZE  CASE  **
!               **********************************
!
        CALL DPTISZ(IHARG,IARGT,ARG,NUMARG,   &
                   PDEFHE,PDEFWI,   &
                   PTITHE,PTITWI,PTITVG,PTITHG,   &
                   IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ****************************
!               **  TREAT THE TITLE CASE  **
!               ****************************
!
        CALL DPTIT(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                   ITITTE,NCTITL,ITIAUT,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************
!               **  TREAT THE NEGATE CASE  **
!               *****************************
!
      IF(ICOM.EQ.'NEGA')THEN
        CALL DPNEGA(IHARG,NUMARG,INEGSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************************
!               **  TREAT THE WINDOW (CORNER) COORDINATES CASE **
!               *************************************************
!
      IF(ICOM.EQ.'WIND')GO TO 5400
      GO TO 5499
!
 5400 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.   &
      IHARG(2).EQ.'COOR')GO TO 5411
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')   &
      GO TO 5430
      GO TO 5499
 5411 CONTINUE
      ISHIFT=1
      GO TO 5420
 5420 CONTINUE
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      GO TO 5430
 5430 CONTINUE
      CALL DPWICC(IHARG,IHARG2,IARGT,ARG,NUMARG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,   &
      PWXMIN,PWXMAX,PWYMIN,PWYMAX,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5499 CONTINUE
!
!               *********************************
!               **  TREAT THE HORIZONTAL CASE  **
!               *********************************
!
      IF(ICOM.EQ.'HORI'.AND.IHARG(1).EQ.'SWIT')THEN
        CALL DPHRIZ(IHARG,NUMARG,IHORSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************************
!               **  TREAT THE MAJOR TIC MARK NUMBER   CASE  **
!               **********************************************
!
      IF(ICOM.EQ.'MAJO')GO TO 5800
!  FEBRUARY, 1988: CHECK FOR "MINOR TIC MARK NUMBER"
      IF(ICOM.EQ.'MINO')GO TO 5899
!  END CHANGE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GO TO 5800
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'NUMB')GO TO 5800
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'NUMB')GO TO 5800
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'NUMB')GO TO 5800
      GO TO 5899
!
 5800 CONTINUE
      CALL DPMATN(ICOM,IHARG,IARGT,IARG,NUMARG,   &
      IX1JSW,IX2JSW,IY1JSW,IY2JSW,   &
      NMJX1T,NMJX2T,NMJY1T,NMJY2T,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
                                                                                                                                  
 5899 CONTINUE
!
!               **********************************************
!               **  TREAT THE MINOR TIC MARK NUMBER   CASE  **
!               **********************************************
!
      IF(ICOM.EQ.'MINO')GO TO 5900
      GO TO 5999
!
 5900 CONTINUE
      CALL DPMITN(IHARG,IARGT,IARG,NUMARG,   &
      IX1NSW,IX2NSW,IY1NSW,IY2NSW,   &
      NMNX1T,NMNX2T,NMNY1T,NMNY2T,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5999 CONTINUE
!
!               *****************************************
!               **  TREAT THE ...TIC LABEL HW    CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 6000
      IF(ICOM.EQ.'X1TI')GO TO 6000
      IF(ICOM.EQ.'X2TI')GO TO 6000
      IF(ICOM.EQ.'YTIC')GO TO 6000
      IF(ICOM.EQ.'Y1TI')GO TO 6000
      IF(ICOM.EQ.'Y2TI')GO TO 6000
      IF(ICOM.EQ.'TIC')GO TO 6000
      IF(ICOM.EQ.'TICS')GO TO 6000
      IF(ICOM.EQ.'XYTI')GO TO 6000
      IF(ICOM.EQ.'YXTI')GO TO 6000
      GO TO 6099
!
 6000 CONTINUE
      CALL DPTLHW(ICOM,IHARG,IARGT,ARG,NUMARG,   &
      PDEFHE,PDEFWI,   &
      PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,   &
      PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,   &
      PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,   &
      PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 6099 CONTINUE
!
!               ********************************************
!               **  TREAT THE MAJOR TIC COORDINATES CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'MAJO')THEN
        CALL DPMJTC(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    IX1TSW,IX2TSW,IY1TSW,IY2TSW,   &
                    X1COOR,X2COOR,Y1COOR,Y2COOR,   &
                    NX1COO,NX2COO,NY1COO,NY2COO,   &
                    MAXTIC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************************
!               **  TREAT THE MINOR TIC COORDINATES CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'MINO')THEN
        CALL DPMNTC(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    X1COMN,X2COMN,Y1COMN,Y2COMN,   &
                    NX1CMN,NX2CMN,NY1CMN,NY2CMN,   &
                    MAXTIC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************
!               **  TREAT THE FILL  COLORS CASE  **
!               ***********************************
!
!CCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'COLO')GO TO 6500
!CCCC GO TO 6599
!
!6500 CONTINUE
!CCCC CALL DPFICO(IHARG,NUMARG,IDEFFC,MAXFIL,IFILCO,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6599 CONTINUE
!
!               ***********************************
!               **  TREAT THE FILL SPACING CASE  **
!               ***********************************
!
!CCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'SPAC')GO TO 6600
!CCCC GO TO 6699
!
!6600 CONTINUE
!CCCC CALL DPFISP(IHARG,IARGT,ARG,NUMARG,PDPFFG,MAXFIL,PFILSP,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6699 CONTINUE
!
!               *************************************
!               **  TREAT THE FILL THICKNESS CASE  **
!               *************************************
!
!CCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'THIC')GO TO 6700
!CCCC GO TO 6799
!
!6700 CONTINUE
!CCCC CALL DPFITH(IHARG,IARGT,ARG,NUMARG,PDEFFT,MAXFIL,PFILTH,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6799 CONTINUE
!
!               ********************************
!               **  TREAT THE FILL  BASE CASE **
!               ********************************
!
!CCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'BASE')GO TO 6800
!CCCC IF(ICOM.EQ.'FILL'.AND.IHARG(1).EQ.'REFE')GO TO 6800
!CCCC1 GO TO 6899
!
!6800 CONTINUE
!CCCC CALL DPFIBA(IHARG,IARGT,ARG,NUMARG,ADEFFB,MAXFIL,AFILBA,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6899 CONTINUE
!
!               ***********************************
!               **  TREAT THE FILL (SWITCH) CASE **
!               ***********************************
!
!CCCC IF(ICOM.EQ.'FILL')GO TO 6900
!CCCC GO TO 6999
!
!6900 CONTINUE
!CCCC IF(IHARG(1).EQ.'ON')GO TO 6910
!CCCC IF(IHARG(2).EQ.'ON')GO TO 6910
!CCCC IF(IHARG(1).EQ.'OFF')GO TO 6910
!CCCC IF(IHARG(2).EQ.'OFF')GO TO 6910
!CCCC GO TO 6999
!6910 CONTINUE
!CCCC CALL DPFISW(IHARG,NUMARG,IDEFFS,MAXFIL,IFILSW,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6999 CONTINUE
!
!               *************************************
!               **  TREAT THE FILL (PATTERN) CASE  **
!               *************************************
!
!CCCC IF(ICOM.EQ.'FILL')GO TO 7000
!CCCC GO TO 7099
!
!7000 CONTINUE
!CCCC CALL DPFIPA(IHARG,NUMARG,IDEFFP,MAXFIL,IFILPA,
!CCCC1IBUGP2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!7099 CONTINUE
!
!               *************************************
!               **  TREAT THE PATTERN LINE   CASE  **
!               *************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'LINE')GO TO 7100
      GO TO 7199
!
 7100 CONTINUE
      CALL DPPALI(IHARG,NUMARG,IDEFPL,MAXPAT,IPATLI,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7199 CONTINUE
!
!               **************************************
!               **  TREAT THE PATTERN SPACING CASE  **
!               **************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'SPAC')GO TO 7200
      GO TO 7299
!
 7200 CONTINUE
      CALL DPPASP(IHARG,IARGT,ARG,NUMARG,   &
                  PDEFPG,MAXPAT,PPATSP,   &
                  IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7299 CONTINUE
!
!               ****************************************
!               **  TREAT THE PATTERN THICKNESS CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'THIC')GO TO 7300
      GO TO 7399
!
 7300 CONTINUE
      CALL DPPATH(IHARG,IARGT,ARG,NUMARG,PDEFPT,MAXPAT,PPATTH,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7399 CONTINUE
!
!               ****************************************
!               **  TREAT THE PATTERN HEIGHT    CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'HEIG')GO TO 7400
      GO TO 7499
!
 7400 CONTINUE
      CALL DPPAHE(IHARG,IARGT,ARG,NUMARG,PDEFPH,MAXPAT,PPATHE,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7499 CONTINUE
!
!               ****************************************
!               **  TREAT THE PATTERN WIDTH     CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'WIDT')GO TO 7500
      GO TO 7599
!
 7500 CONTINUE
      CALL DPPAWI(IHARG,IARGT,ARG,NUMARG,PDEFPW,MAXPAT,PPATWI,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7599 CONTINUE
!
!               *************************************
!               **  TREAT THE PATTERN COLOR  CASE  **
!               *************************************
!
      IF(ICOM.EQ.'PATT'.AND.IHARG(1).EQ.'COLO')GO TO 7600
      GO TO 7699
!
 7600 CONTINUE
      CALL DPPACO(IHARG,NUMARG,IDEFPC,MAXPAT,IPATCO,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7699 CONTINUE
!
!               **************************************
!               **  TREAT THE PATTERN (SWITCH) CASE **
!               **************************************
!
      IF(ICOM.EQ.'PATT')GO TO 7700
      GO TO 7799
!
 7700 CONTINUE
      IF(IHARG(1).EQ.'ON')GO TO 7710
      IF(IHARG(2).EQ.'ON')GO TO 7710
      IF(IHARG(1).EQ.'OFF')GO TO 7710
      IF(IHARG(2).EQ.'OFF')GO TO 7710
      GO TO 7799
 7710 CONTINUE
      CALL DPPASW(IHARG,NUMARG,IDEFPS,MAXPAT,IPATSW,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7799 CONTINUE
!
!               ****************************************
!               **  TREAT THE PATTERN (PATTERN) CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'PATT')GO TO 7800
      GO TO 7899
!
 7800 CONTINUE
      CALL DPPAPA(IHARG,NUMARG,IDEFPP,MAXPAT,IPATPA,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7899 CONTINUE
!
!               **************************************
!               **  TREAT THE SPIKE COLORS CASE     **
!               **  TREAT THE SPIKE THICKNESS CASE  **
!               **  TREAT THE SPIKE LINE      CASE  **
!               **  TREAT THE SPIKE BASE CASE       **
!               **  TREAT THE SPIKE DIRECTION CASE  **
!               **  TREAT THE SPIKE (SWITCH) CASE   **
!               **  TREAT THE SPIKE (PATTERN) CASE  **
!               **        (SAME AS SPIKE LINES CASE)**
!               **************************************
!
      IF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'COLO')THEN
        ICASCL='STAN'
        CALL DPSPCO(IHARG,IARG,NUMARG,IDEFSC,MAXSPI,ISPICO,   &
                    ISPIC2,IRGBMX,ICASCL,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'RGB '.AND.   &
             IHARG(2).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGP2,IERROR)
        IHARG(1)='COLO'
        IHARG2(1)='    '
        CALL DPSPCO(IHARG,IARG,NUMARG,IDEFSC,MAXSPI,ISPICO,   &
                    ISPIC2,IRGBMX,ICASCL,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'THIC')THEN
        CALL DPSPTH(IHARG,IARGT,ARG,NUMARG,PDEFST,MAXSPI,PSPITH,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'LINE')THEN
        CALL DPSPLI(IHARG,IHARG2,NUMARG,IDEFSL,MAXSPI,ISPILI,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.   &
            (IHARG(1).EQ.'BASE' .OR. IHARG(1).EQ.'REFE'))THEN
        CALL DPSPBA(ADEFSB,MAXSPI,ASPIBA,IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK'.AND.IHARG(1).EQ.'DIRE')THEN
        CALL DPSPDI(IHARG,NUMARG,IDEFSD,MAXSPI,ISPIDI,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICOM.EQ.'SPIK')THEN
        IF(IHARG(1).EQ.'ON' .OR. IHARG(2).EQ.'ON' .OR.   &
           IHARG(1).EQ.'OFF' .OR. IHARG(2).EQ.'OFF')THEN
          CALL DPSPSW(IHARG,NUMARG,IDEFSS,MAXSPI,ISPISW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSE
          CALL DPSPPA(IHARG,IHARG2,NUMARG,IDEFSL,MAXSPI,ISPILI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               ***************************************
!               **  TREAT THE SUBREGION (SWITCH) CASE**
!               ***************************************
!
      IF(ICOM.EQ.'SUBR')THEN
        IF(IHARG(1).EQ.'ON'.OR.IHARG(2).EQ.'ON'.OR.   &
           IHARG(1).EQ.'OFF'.OR.IHARG(2).EQ.'OFF')THEN
          CALL DPSBSW(IHARG,NUMARG,IDEFSB,MAXSUB,ISUBSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               ***************************************
!               **  TREAT THE SUBREGION LIMITS   CASE**
!               ***************************************
!
      IF(ICOM.EQ.'SUBR')THEN
        CALL DPSBLI(ICOM,IHARG,IARGT,ARG,NUMARG,   &
                    ASUBXL,ASUBXU,ASUBYL,ASUBYU,   &
                    MAXSUB,   &
                    IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!
!               ***************************
!               **  TREAT THE MINOR GRID CASE  **
!               ***************************
!
      IF(ICOM.EQ.'XGMI')GO TO 8800
      IF(ICOM.EQ.'YGMI')GO TO 8800
      IF(ICOM.EQ.'XYGM')GO TO 8800
      IF(ICOM.EQ.'YXGM')GO TO 8800
      IF(ICOM.EQ.'GMIN')GO TO 8800
      IF(ICOM.EQ.'MINO')GO TO 8800
      GO TO 8899
!
 8800 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 8899
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GO TO 8899
      CALL DPGRMN(ICOM,IHARG,NUMARG,IVGMSW,IHGMSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 8899 CONTINUE
!
!               ****************************************************
!               **  TREAT THE MULTIPLOT (CORNER) COORDINATES CASE **
!               ****************************************************
!
      IF((ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'CORN') .OR.   &
         (ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'COOR') .OR.   &
         (ICOM.EQ.'MP  ' .AND. IHARG(1).EQ.'CORN') .OR.   &
         (ICOM.EQ.'MP  ' .AND. IHARG(1).EQ.'COOR'))THEN
!
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CORN'.AND.   &
           IHARG(2).EQ.'COOR')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
        ENDIF
!
        CALL DPMUCC(IHARG,IHARG2,IARGT,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                    NUMNAM,MAXNAM,IANS,IWIDTH,   &
                    PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!
!               ****************************************************
!               **  TREAT THE    END OF MULTIPLOT    CASE         **
!               ****************************************************
!
      IF(ICOM.EQ.'END')GO TO 9200
!CCCC THE FOLLOWING 2 LINES WERE ADDED AUGUST 1990
      IF(ICOM.EQ.'EOMP')GO TO 9210
      IF(ICOM.EQ.'EMP')GO TO 9210
      GO TO 9299
!
 9200 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'OF'.AND.   &
      IHARG(2).EQ.'MULT')GO TO 9210
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'MULT')   &
      GO TO 9210
      GO TO 9299
 9210 CONTINUE
      CALL DPENMU(IMPSW,   &
      IERASV,   &
      PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
      IERASW,   &
      PWXMIN,PWXMAX,PWYMIN,PWYMAX,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 9299 CONTINUE
!
!               ****************************************************
!               **  TREAT THE MULTIPLOT CASE                      **
!               ****************************************************
!
      IF(ICOM.EQ.'MULT' .OR. ICOM.EQ.'MP')GO TO 9300
      GO TO 9399
!
 9300 CONTINUE
      IF(ICOM2.EQ.'IPLE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRUB')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TIET')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ESD ')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FREQ')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'FREQ')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'FREQ')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'FREQ')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'KERN')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'KERN')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'KERN')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'KERN')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LORE')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'LORE')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'LORE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ADJA')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ADJA')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ADJA')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ALLA')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ALLA')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AV')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AV')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AS')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'AS')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ASD')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'ASD')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SPEC')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PERI')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AUTO')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PART')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CO  ')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COSP')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'QUAD')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CROS')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COHE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'AMPL')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PHAS')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GAIN')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ARGA')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TAIL')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SURV')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DIFF')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LEVE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COCH')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROP')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANOP')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ANAL')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'COMM' .AND.   &
         IHARG(2).EQ.'WEIB')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'EMPI' .AND.   &
         IHARG(2).EQ.'QUAN')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CUMU' .AND.   &
         IHARG(2).EQ.'SUM')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LJUN' .AND.   &
         IHARG(2).EQ.'BOX')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BOX' .AND.   &
         IHARG(2).EQ.'COX')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'BOX')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'BOX')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SYMM')GO TO 9399
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'I')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'I')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEAN'.AND.   &
         IHARG(2).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'MEAN'.AND.   &
         IHARG(3).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEDI'.AND.   &
         IHARG(2).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'MEDI'.AND.   &
         IHARG(3).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'TRIM'.AND.   &
         IHARG(2).EQ.'MEAN')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'TRIM'.AND.   &
         IHARG(3).EQ.'MEAN')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BIWE'.AND.   &
         IHARG(2).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'BIWE'.AND.   &
         IHARG(3).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'QUAN'.AND.   &
         IHARG(2).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'QUAN'.AND.   &
         IHARG(3).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE '.AND.   &
         IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'ERRO')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ONE '.AND.   &
         IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'ERRO')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TWO '.AND.   &
         IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'ERRO')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'TWO '.AND.   &
         IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'ERRO')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE '.AND.   &
         IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'DEVI')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ONE '.AND.   &
         IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'DEVI')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TWO '.AND.   &
         IHARG(2).EQ.'STAN'.AND.IHARG(3).EQ.'DEVI')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'TWO '.AND.   &
         IHARG(3).EQ.'STAN'.AND.IHARG(4).EQ.'DEVI')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM'.AND.   &
         IHARG(2).EQ.'TOLE')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'NORM'.AND.   &
         IHARG(3).EQ.'TOLE')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM'.AND.   &
         IHARG(2).EQ.'PRED')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'NORM'.AND.   &
         IHARG(3).EQ.'PRED')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'AGRE'.AND.   &
         IHARG(2).EQ.'COUL')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AGRE'.AND.   &
         IHARG(3).EQ.'COUL')GO TO 9399
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'VIOL')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'VIOL')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HOMO')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'PERC'.AND.   &
         IHARG(2).EQ.'POIN'.AND.IHARG(3).EQ.'PLOT')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'BEST' .AND.   &
         IHARG(2).EQ.'DIST'.AND.IHARG(3).EQ.'FIT')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RUN ' .AND.   &
         IHARG(2).EQ.'SEQU')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'KRUS' .AND.   &
         IHARG(2).EQ.'WALL')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SQUA' .AND.   &
         IHARG(2).EQ.'RANK')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MEDI' .AND.   &
         IHARG(2).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANDE' .AND.   &
         IHARG(2).EQ.'DARL')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'VAN ' .AND.   &
         IHARG(2).EQ.'DER '.AND.IHARG(3).EQ.'WAER')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ONE ' .AND.   &
         IHARG(2).EQ.'WAY '.AND.IHARG(3).EQ.'NORM')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WILK' .AND.   &
         IHARG(2).EQ.'SHAP')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SHAP' .AND.   &
         IHARG(2).EQ.'WILK')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NORM' .AND.   &
         IHARG(2).EQ.'TOLE')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NONP' .AND.   &
         IHARG(2).EQ.'TOLE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TOLE')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BEST' .AND.   &
         IHARG(2).EQ.'DIST')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'T   '.AND.   &
         IHARG(2).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'T   '.AND.   &
         IHARG(3).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(3).EQ.'T   '.AND.   &
         IHARG(4).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.5.AND.IHARG(4).EQ.'T   '.AND.   &
         IHARG(5).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.6.AND.IHARG(5).EQ.'T   '.AND.   &
         IHARG(6).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'BART'.AND.   &
         IHARG(2).EQ.'TEST')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DM  '.AND.   &
         IHARG(2).EQ.'BART')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIXO' .AND.   &
         IHARG(2).EQ.'MASS'.AND.IHARG(3).EQ.'BART')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'F   '.AND.   &
         IHARG(2).EQ.'LOC ')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SUMM')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CAPA')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'RUNS')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'JARQ'.AND.   &
         IHARG(2).EQ.'BERA')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PRED')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LOWE')GO TO 9399
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'UPPE')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SD  ' .AND.   &
         IHARG(2).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'STAN' .AND.   &
         IHARG(2).EQ.'DEVI'.AND.IHARG(3).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'STAN' .AND.   &
         IHARG(2).EQ.'DEVI'.AND.IHARG(3).EQ.'PRED')GO TO 9399
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ONE ' .AND.   &
         IHARG(2).EQ.'SIDE')GO TO 9399
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'DIST' .AND.   &
         IHARG(2).EQ.'FIT '.AND.IHARG(3).EQ.'PLOT')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.   &
         IHARG(2).EQ.'OF  '.AND. IHARG(3).EQ.'VARI' .AND.   &
         IHARG(4).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.   &
         IHARG(2).EQ.'OF  '.AND. IHARG(3).EQ.'DISP' .AND.   &
         IHARG(4).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.   &
         IHARG(2).EQ.'OF  '.AND.IHARG(3).EQ.'QUAR' .AND.   &
         IHARG(4).EQ.'DISP'.AND.IHARG(5).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'COEF' .AND.   &
         IHARG(2).EQ.'OF  '.AND.IHARG(3).EQ.'QUAR' .AND.   &
         IHARG(4).EQ.'VARI'.AND.IHARG(5).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'QUAR' .AND.   &
         IHARG(2).EQ.'COEF'.AND.IHARG(3).EQ.'OF  ' .AND.   &
         IHARG(4).EQ.'DISP'.AND.IHARG(5).EQ.'CONF')GO TO 9399
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'QUAR' .AND.   &
         IHARG(2).EQ.'COEF'.AND.IHARG(3).EQ.'OF  ' .AND.   &
         IHARG(4).EQ.'VARI'.AND.IHARG(5).EQ.'CONF')GO TO 9399
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.   &
      IHARG2(1).EQ.'    ')   &
      GO TO 9311
      GO TO 9330
 9311 CONTINUE
      ISHIFT=1
      GO TO 9320
 9320 CONTINUE
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      GO TO 9330
 9330 CONTINUE
      CALL DPMULT(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,   &
      IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
      IMPARG,   &
      AMPSCH,AMPSCW,   &
      PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
      IERASW,   &
      PWXMIN,PWXMAX,PWYMIN,PWYMAX,   &
      IERASV,   &
      PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 9399 CONTINUE
!
!               ****************************************************
!               **  TREAT THE EMBED     CASE                      **
!               ****************************************************
!
      IF(ICOM.EQ.'EMBE')THEN
        CALL DPEMBE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,ICOM,IERASW,   &
                    IEMBSW,IEMCNT,PEMXC1,PEMXC2,PEMYC1,PEMYC2,   &
                    PWXMIN,PWXMAX,PWYMIN,PWYMAX,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1990
!               *************************************
!               **  TREAT THE WINDOW SYSTEM   CASE **
!               *************************************
!
!CCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'SYST')GO TO 11100
!CCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'MANA')GO TO 11100
!CCCC  GO TO 11199
!
!11100 CONTINUE
!CCCC  CALL DPWISY(IHARG,NUMARG,IDEFWS,IWINSY,
!CCCC 1IBUGP2,IFOUND,IERROR)
!CCCC  IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!11199 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1990
!               *************************************
!               **  TREAT THE WINDOW POINTER CASE  **
!               *************************************
!
!CCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'POIN')GO TO 11200
!CCCC  IF(ICOM.EQ.'WIND'.AND.IHARG(1).EQ.'SELE')GO TO 11200
!CCCC  GO TO 11299
!
!11200 CONTINUE
!CCCC  CALL DPWIPO(IHARG,NUMARG,IDEFWP,IWINPO,
!CCCC 1IBUGP2,IFOUND,IERROR)
!CCCC  IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ***********************************
!               **  TREAT THE PIXMAP TITLE CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'PIXM'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'TITL')THEN
        CALL DPPMTI(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE MAIPC2
      SUBROUTINE MAIPC3(IBUGPC,IBUGP2,ISUBRO,   &
                        IVGMSW,IHGMSW,   &
                        IMPSW,IMPNR,IMPNC,IMPCO,   &
                        PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                        IERASV,   &
                        PWXMIS,PWXMAS,PWYMIS,PWYMAS,   &
                        BARHEF,BARWEF,IRGBMX,   &
                        IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAIPC3.
!              (THE   PC    AT THE END OF    MAIPC3   STANDS FOR PLOT CONTROL
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
!              PLOT CONTROL COMMANDS (PART 3).
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JULY 1986.
!     UPDATED        --APRIL  1992.  BAR EXPANSION FACTORS ... ...
!     UPDATED        --OCTOBER1993.  ARGUMENTS TO BAR BASE (DPBABA)
!     UPDATED        --OCTOBER1993.  ARGUMENTS TO REGION BASE (DPREBA)
!     UPDATED        --MARCH  1994.  ARGUMENTS TO REGION BASE (DPREBA)
!     UPDATED        --AUGUST    1995. DASH2 BUG (VARIOUS)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
!
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 ICASCL
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
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
!
      IFOUND='NO'
      IERROR='NO'
!
!               *****************************************
!               **  TREAT THE ORIENTATION SWITCH CASE  **
!               *****************************************
!
!
      IF(ICOM.EQ.'ORIE')THEN
        CALL DPORSW(IHARG,NUMARG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!     ----------BARS--------------------------------------------------
!
!               ******************************************
!               **  STEP XX--                           **
!               **  TREAT THE VARIOUS BAR ... COMMANDS  **
!               ******************************************
!
!               **********************************************
!               **  TREAT THE BAR PATTERN LINE (TYPE)  CASE **
!               **  TREAT THE BAR PATTERN COLOR        CASE **
!               **  TREAT THE BAR PATTERN THICKNESS    CASE **
!               **  TREAT THE BAR PATTERN SPACING      CASE **
!               **  TREAT THE BAR PATTERN (TYPE)       CASE **
!               **  TREAT THE BAR FILL COLOR           CASE **
!               **  TREAT THE BAR FILL (SWITCH)        CASE **
!               **  TREAT THE BAR BORDER COLOR         CASE **
!               **  TREAT THE BAR BORDER THICKNESS     CASE **
!               **  TREAT THE BAR BORDER LINE (TYPE)   CASE **
!               **  TREAT THE BAR WIDTH                CASE **
!               **  TREAT THE BAR BASE                 CASE **
!               **  TREAT THE BAR (SWITCH)             CASE **
!               **  TREAT THE BAR DIMENSION            CASE **
!               **  TREAT THE BAR DIRECTION            CASE **
!               **  TREAT THE BAR EXPANSION FACTORS   CASE **
!               **        (USED ONLY BY BLOCK PLOT COMMAND)**
!               **********************************************
!
      IF(ICOM.EQ.'BAR')THEN
        IF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE')THEN
          IF(IHARG(3).NE.'TYPE')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
            IHARG(3)='TYPE'
            IHARG2(3)='    '
          ENDIF
!
          CALL DPBPLI(IHARG,IHARG2,NUMARG,IDEBPL,MAXBAR,IBAPLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBPCO(IHARG,IARG,NUMARG,IDEBPC,ICASCL,MAXBAR,IRGBMX,   &
                      IBAPCO,IBAPC2,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'RGB '.AND.   &
               IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='PATT'
          IHARG2(1)='ERN '
          IHARG(2)='COLO'
          IHARG2(2)='R   '
          CALL DPBPCO(IHARG,IARG,NUMARG,IDEBPC,ICASCL,MAXBAR,IRGBMX,   &
                      IBAPCO,IBAPC2,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPBPTH(IHARG,IARGT,ARG,NUMARG,PDEBPT,MAXBAR,PBAPTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'SPAC')THEN
          CALL DPBPSP(IHARG,IARGT,ARG,NUMARG,PDEBPS,MAXBAR,PBAPSP,   &
                      IBUGP2,IFOUND,IERROR)
        ELSEIF(IHARG(1).EQ.'SPAC')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='PATT'
          IHARG2(2)='ERN '
          CALL DPBPSP(IHARG,IARGT,ARG,NUMARG,PDEBPS,MAXBAR,PBAPSP,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'TYPE')THEN
          CALL DPBPTY(IHARG,NUMARG,IDEBPT,MAXBAR,IBAPTY,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(ICOM.EQ.'BAR'.AND.IHARG(1).EQ.'PATT')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPBPTY(IHARG,NUMARG,IDEBPT,MAXBAR,IBAPTY,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'COLO')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          ICASCL='STAN'
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,   &
                      ICASCL,IBAFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,   &
                      ICASCL,IBAFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,   &
                      ICASCL,IBAFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'RGB '.AND.   &
               IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPBFCO(IHARG,IARG,NUMARG,IDEBFC,MAXBAR,IBAFCO,   &
                      ICASCL,IBAFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'SWIT')THEN
          CALL DPBFSW(IHARG,NUMARG,IDEBFS,MAXBAR,IBAFSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='SWIT'
          IHARG2(2)='CH  '
          CALL DPBFSW(IHARG,NUMARG,IDEBFS,MAXBAR,IBAFSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPBBCO(IHARG,IARG,NUMARG,IDEBBC,ICASCL,MAXBAR,IRGBMX,   &
                      IBABCO,IBABC2,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'RGB '.AND.   &
               IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='BORD'
          IHARG2(1)='ER  '
          IHARG(2)='COLO'
          IHARG2(2)='R   '
          CALL DPBBCO(IHARG,IARG,NUMARG,IDEBBC,ICASCL,MAXBAR,IRGBMX,   &
                      IBABCO,IBABC2,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPBBTH(IHARG,IARGT,ARG,NUMARG,PDEBBT,MAXBAR,PBABTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'LINE'.AND.   &
               IHARG(3).EQ.'TYPE')THEN
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'BORD'.AND.   &
              (IHARG(2).EQ.'TYPE' .OR. IHARG(2).EQ.'LINE'))THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'BORD')THEN
          ISHIFT=2
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPBBLI(IHARG,IHARG2,NUMARG,IDEBBL,MAXBAR,IBABLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'WIDT')THEN
          CALL DPBAWI(ADEBWI,MAXBAR,ABARWI,IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BASE'.OR.IHARG(1).EQ.'REFE')THEN
          CALL DPBABA(ADEBBA,MAXBAR,ABARBA,IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'DIME')THEN
          CALL DPBATY(IHARG,NUMARG,IDEBTY,MAXBAR,IBARTY,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'DIRE')THEN
          CALL DPBADI(IHARG,NUMARG,IDEBDI,MAXBAR,IBARDI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'EXPA' .OR. IHARG(1).EQ.'FACT')THEN
          CALL DPBAEF(IHARG,IARGT,ARG,NUMARG,BARHEF,BARWEF,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'SWIT')THEN
          CALL DPBASW(IHARG,NUMARG,IDEBSW,MAXBAR,IBARSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(ICOM.EQ.'BAR')THEN
          IF(IHARG(1).EQ.'ON'  .OR. IHARG(2).EQ.'ON' .OR.   &
             IHARG(1).EQ.'OFF' .OR. IHARG(2).EQ.'OFF')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
            IHARG(1)='SWIT'
            IHARG2(1)='CH  '
          ENDIF
          CALL DPBASW(IHARG,NUMARG,IDEBSW,MAXBAR,IBARSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ENDIF
      ENDIF
!
!               **********************************
!               **  END POINT FOR BAR COMMANDS  **
!               **********************************
!
!     ----------END OF BARS---------------------------------------
!
!     ----------REGIONS-----------------------------------------------
!
!               ********************************************
!               **  STEP XX--                             **
!               **  TREAT THE VARIOUS REGION ... COMMANDS **
!               ********************************************
!
      IF(ICOM.EQ.'REGI' .AND. ICOM2.EQ.'S   ')GO TO 21799
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'PATT')THEN
!
!               ********************************************************
!               **  TREAT THE REGION PATTERN LINE (TYPE)         CASE **
!               **  TREAT THE REGION PATTERN COLOR               CASE **
!               **  TREAT THE REGION PATTERN THICKNESS           CASE **
!               **  TREAT THE REGION PATTERN SPACING             CASE **
!               **  TREAT THE REGION PATTERN (TYPE)              CASE **
!               ********************************************************
!
        IF(IHARG(2).EQ.'LINE')THEN
          IF(IHARG(2).EQ.'LINE' .AND. IHARG(3).NE.'TYPE')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
            IHARG(3)='TYPE'
            IHARG2(3)='    '
          ENDIF
          CALL DPRPLI(IHARG,IHARG2,NUMARG,IDERPL,MAXREG,IREPLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRPCO(IHARG,IARG,NUMARG,IDERPC,MAXREG,IREPCO,   &
                      ICASCL,IREPC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'RGB ' .OR. IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='PATT'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPRPCO(IHARG,IARG,NUMARG,IDERPC,MAXREG,IREPCO,   &
                      ICASCL,IREPC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'THIC')THEN
          CALL DPRPTH(IHARG,IARGT,ARG,NUMARG,PDERPT,MAXREG,PREPTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'SPAC')THEN
          CALL DPRPSP(IHARG,IARGT,ARG,NUMARG,PDERPS,MAXREG,PREPSP,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'TYPE')THEN
          CALL DPRPTY(IHARG,NUMARG,IDERPT,MAXREG,IREPTY,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).NE.'TYPE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPRPTY(IHARG,NUMARG,IDERPT,MAXREG,IREPTY,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'COLO')THEN
        ICASCL='STAN'
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGP2,IERROR)
        IHARG(1)='FILL'
        IHARG2(1)='    '
        IHARG(2)='COLO'
        IHARG2(2)='    '
        CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,   &
                    ICASCL,IREFC2,IRGBMX,   &
                    IBUGP2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(ICOM.EQ.'REGI' .AND. IHARG(1).EQ.'FILL')THEN
!
!               *******************************************
!               **  TREAT THE REGION FILL COLOR  CASE    **
!               **  TREAT THE REGION FILL (SWITCH) CASE  **
!               *******************************************
!
        IF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,   &
                      ICASCL,IREFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(2).EQ.'RGB '.AND.IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB'
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,   &
                      ICASCL,IREFC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'SWIT')THEN
          CALL DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ENDIF
!
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGP2,IERROR)
        IHARG(1)='FILL'
        IHARG2(1)='    '
        IHARG(2)='SWIT'
        IHARG2(2)='CH  '
        CALL DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************************
!               **  TREAT THE REGION BORDER COLOR      CASE   **
!               **  TREAT THE REGION BORDER THICKNESS  CASE   **
!               **  TREAT THE REGION BORDER LINE (TYPE)  CASE **
!               ************************************************
!
      IF(ICOM.EQ.'REGI'.AND.IHARG(1).EQ.'BORD')THEN
        IF(IHARG(2).EQ.'COLO')THEN
          ICASCL='STAN'
          CALL DPRBCO(IHARG,IARG,NUMARG,IDERBC,MAXREG,IREBCO,   &
                      ICASCL,IREBC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'RGB ' .AND. IHARG(3).EQ.'COLO')THEN
          ICASCL='RGB '
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='BORD'
          IHARG2(1)='ER  '
          CALL DPRBCO(IHARG,IARG,NUMARG,IDERBC,MAXREG,IREBCO,   &
                      ICASCL,IREBC2,IRGBMX,   &
                      IBUGP2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'THIC')THEN
          CALL DPRBTH(IHARG,IARGT,ARG,NUMARG,PDERBT,MAXREG,PREBTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')THEN
          CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(2).EQ.'TYPE' .OR. IHARG(2).EQ.'LINE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGP2,IERROR)
        IHARG(2)='LINE'
        IHARG2(2)='    '
        IHARG(3)='TYPE'
        IHARG2(3)='    '
        CALL DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************
!               **  TREAT THE REGION   BASE CASE **
!               ***********************************
!
      IF(ICOM.EQ.'REGI' .AND.   &
        (IHARG(1).EQ.'BASE' .OR. IHARG(1).EQ.'REFE'))THEN
!
        CALL DPREBA(ADERBA,MAXREG,AREGBA,IREBIN,IREBPL,   &
                    IBUGP2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  END POINT FOR REGION COMMANDS  **
!               *************************************
!
21799 CONTINUE
!
!     ----------END OF REGIONS------------------------------------------
!
!     ----------MARKERS-------------------------------------------------
!
!               *********************************************
!               **  STEP XX--                              **
!               **  TREAT THE VARIOUS MARKER ... COMMANDS  **
!               *********************************************
!
      IF(ICOM.EQ.'MARK')GO TO 31000
      GO TO 32999
31000 CONTINUE
!
!               *************************************************
!               **  TREAT THE MARKER PATTERN LINE (TYPE)  CASE **
!               *************************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')GO TO 31120
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'LINE')GO TO 31100
      GO TO 31199
!
31100 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(3)='TYPE'
      IHARG2(3)='    '
31120 CONTINUE
!CCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
!CCCC CALL DPMPLI(IHARG,NUMARG,IDEMPL,MAXMAR,IMAPLI,
      CALL DPMPLI(IHARG,IHARG2,NUMARG,IDEMPL,MAXMAR,IMAPLI,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31199 CONTINUE
!
!               *******************************************
!               **  TREAT THE MARKER PATTERN COLOR  CASE **
!               *******************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'COLO')GO TO 31200
      GO TO 31299
!
31200 CONTINUE
      CALL DPMPCO(IHARG,NUMARG,IDEMPC,MAXMAR,IMAPCO,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31299 CONTINUE
!
!               ***********************************************
!               **  TREAT THE MARKER PATTERN THICKNESS  CASE **
!               ***********************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'THIC')GO TO 31300
      GO TO 31399
!
31300 CONTINUE
      CALL DPMPTH(IHARG,IARGT,ARG,NUMARG,PDEMPT,MAXMAR,PMAPTH,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31399 CONTINUE
!
!               ***********************************************
!               **  TREAT THE MARKER PATTERN SPACING    CASE **
!               ***********************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'SPAC')GO TO 31400
      GO TO 31499
!
31400 CONTINUE
      CALL DPMPSP(IHARG,IARGT,ARG,NUMARG,PDEMPS,MAXMAR,PMAPSP,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31499 CONTINUE
!
!               **********************************************
!               **  TREAT THE MARKER PATTERN (TYPE)  CASE   **
!               **********************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT'.AND.   &
      IHARG(2).EQ.'TYPE')GO TO 31520
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'PATT')GO TO 31500
      GO TO 31599
!
31500 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(2)='TYPE'
      IHARG2(2)='    '
31520 CONTINUE
      CALL DPMPTY(IHARG,NUMARG,IDEMPT,MAXMAR,IMAPTY,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31599 CONTINUE
!
!               ****************************************
!               **  TREAT THE MARKER FILL COLOR  CASE **
!               ****************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL'.AND.   &
      IHARG(2).EQ.'COLO')GO TO 31750
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'COLO')GO TO 31710
      GO TO 31799
!
31710 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(1)='FILL'
      IHARG2(1)='    '
      IHARG(2)='COLO'
      IHARG2(2)='    '
      GO TO 31750
!
31750 CONTINUE
      CALL DPMFCO(IHARG,NUMARG,IDEMFC,MAXMAR,IMAFCO,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31799 CONTINUE
!
!               *******************************************
!               **  TREAT THE MARKER FILL (SWITCH) CASE  **
!               *******************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL'.AND.   &
      IHARG(2).EQ.'SWIT')GO TO 31820
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'FILL')GO TO 31800
      GO TO 31899
!
31800 CONTINUE
      IF(IHARG(1).EQ.'ON')GO TO 31810
      IF(IHARG(2).EQ.'ON')GO TO 31810
      IF(IHARG(1).EQ.'OFF')GO TO 31810
      IF(IHARG(2).EQ.'OFF')GO TO 31810
      GO TO 31899
31810 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(2)='SWIT'
      IHARG2(2)='CH  '
31820 CONTINUE
      CALL DPMFSW(IHARG,NUMARG,IDEMFS,MAXMAR,IMAFSW,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
31899 CONTINUE
!
!               ******************************************
!               **  TREAT THE MARKER BORDER COLOR  CASE **
!               ******************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.   &
      IHARG(2).EQ.'COLO')GO TO 32100
      GO TO 32199
!
32100 CONTINUE
      CALL DPMBCO(IHARG,NUMARG,IDEMBC,MAXMAR,IMABCO,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
32199 CONTINUE
!
!               **********************************************
!               **  TREAT THE MARKER BORDER THICKNESS  CASE **
!               **********************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.   &
      IHARG(2).EQ.'THIC')GO TO 32200
      GO TO 32299
!
32200 CONTINUE
      CALL DPMBTH(IHARG,IARGT,ARG,NUMARG,PDEMBT,MAXMAR,PMABTH,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
32299 CONTINUE
!
!               **************************************************
!               **  TREAT THE MARKER BORDER LINE (TYPE)  CASE   **
!               **************************************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.   &
      IHARG(2).EQ.'LINE'.AND.IHARG(3).EQ.'TYPE')GO TO 32330
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.   &
      IHARG(2).EQ.'TYPE')GO TO 32320
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD'.AND.   &
      IHARG(2).EQ.'LINE')GO TO 32320
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BORD')GO TO 32310
      GO TO 32399
!
32310 CONTINUE
      ISHIFT=2
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(2)='LINE'
      IHARG2(2)='    '
      IHARG(3)='TYPE'
      IHARG2(3)='    '
      GO TO 32330
!
32320 CONTINUE
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGP2,IERROR)
      IHARG(2)='LINE'
      IHARG2(2)='    '
      IHARG(3)='TYPE'
      IHARG2(3)='    '
      GO TO 32330
!
32330 CONTINUE
!CCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
!CCCC CALL DPMBLI(IHARG,NUMARG,IDEMBL,MAXMAR,IMABLI,
      CALL DPMBLI(IHARG,IHARG2,NUMARG,IDEMBL,MAXMAR,IMABLI,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
32399 CONTINUE
!
!               ***********************************
!               **  TREAT THE MARKER   BASE CASE **
!               ***********************************
!
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'BASE')GO TO 32600
      IF(ICOM.EQ.'MARK'.AND.IHARG(1).EQ.'REFE')GO TO 32600
      GO TO 32699
!
32600 CONTINUE
      CALL DPMABA(IHARG,IARGT,ARG,NUMARG,ADEMBA,MAXMAR,AMARBA,   &
      IBUGP2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
32699 CONTINUE
!
!               *************************************
!               **  END POINT FOR MARKER COMMANDS  **
!               *************************************
!
32999 CONTINUE
!
!     ----------END OF MARKERS---------------------------------------
!
!     ----------TEXTS--------------------------------------------------
!
!               ***********************************************
!               **  STEP XX--                                **
!               **  TREAT THE VARIOUS TEXT ... COMMANDS      **
!               **  TREAT THE TEXT PATTERN LINE (TYPE)  CASE **
!               **  TREAT THE TEXT PATTERN COLOR        CASE **
!               **  TREAT THE TEXT PATTERN THICKNESS    CASE **
!               **  TREAT THE TEXT PATTERN SPACING      CASE **
!               **  TREAT THE TEXT PATTERN (TYPE)       CASE **
!               **  TREAT THE TEXT FILL COLOR           CASE **
!               **  TREAT THE TEXT FILL (SWITCH)        CASE **
!               **  TREAT THE TEXT BORDER COLOR         CASE **
!               **  TREAT THE TEXT BORDER THICKNESS     CASE **
!               **  TREAT THE TEXT BORDER LINE (TYPE)   CASE **
!               **  TREAT THE TEXT BASE                 CASE **
!               ***********************************************
!
      IF(ICOM.EQ.'TEXT')THEN
!
        IF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE'.AND.   &
           IHARG(3).EQ.'TYPE')THEN
          CALL DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'LINE')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'COLO'.AND.   &
               IHARG2(2).EQ.'R   ')THEN
          CALL DPTPCO(IHARG,NUMARG,IDETPC,MAXTEX,ITEPCO,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPTPTH(IHARG,IARGT,ARG,NUMARG,PDETPT,MAXTEX,PTEPTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'SPAC')THEN
          CALL DPTPSP(IHARG,IARGT,ARG,NUMARG,PDETPS,MAXTEX,PTEPSP,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'PATT'.AND.IHARG(2).EQ.'TYPE')THEN
          CALL DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'PATT')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='TYPE'
          IHARG2(2)='    '
          CALL DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'COLO'.AND.   &
               IHARG2(2).EQ.'R   ')THEN
          CALL DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'COLO'.AND.IHARG2(1).EQ.'R   ')THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(1)='FILL'
          IHARG2(1)='    '
          IHARG(2)='COLO'
          IHARG2(2)='    '
          CALL DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'FILL'.AND.IHARG(2).EQ.'SWIT')THEN
          CALL DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'FILL')THEN
          IF(IHARG(1).EQ.'ON'.OR.IHARG(2).EQ.'ON'.OR.   &
             IHARG(1).EQ.'OFF'.OR.IHARG(2).EQ.'OFF')THEN
            ISHIFT=1
            CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IBUGP2,IERROR)
            IHARG(2)='SWIT'
            IHARG2(2)='CH  '
            CALL DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,   &
                        IBUGP2,IFOUND,IERROR)
            IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
          ENDIF
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'COLO'.AND.   &
               IHARG2(2).EQ.'R   ')THEN
          CALL DPTBCO(IHARG,NUMARG,IDETBC,MAXTEX,ITEBCO,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'THIC')THEN
          CALL DPTBTH(IHARG,IARGT,ARG,NUMARG,PDETBT,MAXTEX,PTEBTH,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'BORD'.AND.IHARG(2).EQ.'LINE'.AND.   &
               IHARG(3).EQ.'TYPE')THEN
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'BORD'.AND.   &
              (IHARG(2).EQ.'LINE' .OR. IHARG(2).EQ.'TYPE'))THEN
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(ICOM.EQ.'TEXT'.AND.IHARG(1).EQ.'BORD')THEN
          ISHIFT=2
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGP2,IERROR)
          IHARG(2)='LINE'
          IHARG2(2)='    '
          IHARG(3)='TYPE'
          IHARG2(3)='    '
          CALL DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,   &
                      IBUGP2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!CCCC   ELSEIF(IHARG(1).EQ.'BASE'.OR.IHARG(1).EQ.'REFE')THEN
!CCCC     CALL DPTEBA(IHARG,IARGT,ARG,NUMARG,ADETBA,MAXTEX,ATEXBA,
!CCCC1                IBUGP2,IFOUND,IERROR)
!CCCC     IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               ***********************************
!               **  END POINT FOR TEXT COMMANDS  **
!               ***********************************
!
!
!     ----------END OF TEXTS---------------------------------------
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE MAIPC3
      SUBROUTINE MAIPC4(IBUGPC,IBUGP2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--THIS IS SUBROUTING MAIPC4.
!              (THE   PC    AT THE END OF    MAIPC4   STANDS FOR PLOT CONTROL
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES
!              PLOT CONTROL COMMANDS (PART 1).
!              THE PLOT CONTROL COMMANDS SEARCHED FOR BY MAIPC4
!              ARE THE FOLLOWING 3D-RELATED COMMANDS--
!
!                 EYE (COORDINATES)
!                 ORIGIN COORDINATES
!                 VISIBLE (HIDDENLINES, BACKLINES)
!                 PROJECTION
!
!                 PEDESTAL ON/OFF
!                 PEDESTAL BASE
!                 PEDESTAL SIZE
!                 PEDESTAL COLOR
!                 PEDESTAL GRID
!                 PEDESTAL GRID PATTERN
!                 PEDESTAL GRID COLOR
!
!                 BASEPLANE ON/OFF
!                 BASEPLANE COLOR
!                 BASEPLANE GRID
!                 BASEPLANE GRID PATTERN
!                 BASEPLANE GRID COLOR
!
!                 BACKPLANE ON/OFF
!                 BACKPLANE COLOR
!                 BACKPLANE GRID
!                 BACKPLANE GRID PATTERN
!                 BACKPLANE GRID COLOR
!
!                 SIDEFACE ON/OFF
!                 SIDEFACE COLOR
!                 SIDEFACE GRID
!                 SIDEFACE GRID PATTERN
!                 SIDEFACE GRID COLOR
!
!                 TIC PLANE
!
!                 ROTATE EYE
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
!     VERSION NUMBER  --88.10
!     ORIGINAL VERSION--SEPTEMBER 1988.
!     UPDATED         --APRIL     1992. DEPBA=DEFBA COMMENTED OUT
!     UPDATED         --SEPTEMBER 1993. ALLOW EYE FOR EYE COOR
!     UPDATED         --SEPTEMBER 1993. NEW COMMAND--ROTATE EYE
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGPC
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
!
      IFOUND='NO'
      IERROR='NO'
!
!CCCC THE FOLLOWING SECTION WAS REWRITTEN    SEPTEMBER 1993
!               ***************************************
!               **  TREAT THE EYE (COORDINATES) CASE **
!               ***************************************
!
      IF(ICOM.EQ.'EYE')THEN
         IF(NUMARG.GE.1)THEN
            IF(IHARG(1).EQ.'COOR')THEN
               ISHIFT=1
               CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,   &
               IARGT,NUMARG,IBUGPC,IERROR)
            ENDIF
         ENDIF
         CALL DPEYCO(IHARG,IARGT,ARG,NUMARG,   &
         AEYEXC,AEYEYC,AEYEZC,   &
         X3DEYE,Y3DEYE,Z3DEYE,   &
         IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************************
!               **  TREAT THE ORIGIN COORDINATES CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'ORIG')THEN
        CALL DPORCO(IHARG,IARGT,ARG,NUMARG,   &
                    AORIXC,AORIYC,AORIZC,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************
!               **  TREAT THE VISIBLE CASE   **
!               **  HIDDEN LINES, BACKLINES  **
!               *******************************
!
      IF(ICOM.EQ.'VISI')GO TO 1300
      IF(ICOM.EQ.'HIDD')GO TO 1300
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'LINE')GO TO 1300
      IF(ICOM.EQ.'BACK'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'LINE')   &
      GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      CALL DPVIS(IHARG,NUMARG,IVISSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 1399 CONTINUE
!
!               **************************************************
!               **  TREAT THE PROJECTION CASE (3D)              **
!               **************************************************
!
      IF(ICOM.EQ.'PROJ')GO TO 1400
      IF(ICOM.EQ.'ORTH')GO TO 1400
      IF(ICOM.EQ.'PERS')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      CALL DPPROJ(ICOM,IHARG,NUMARG,I3DPRO,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 1499 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED    SEPTEMBER 1993
!               **************************************
!               **  TREAT THE ROTATE EYE CASE       **
!               **************************************
!
      IF(ICOM.EQ.'ROTA')THEN
         CALL DPROEY(IHARG,IARGT,ARG,NUMARG,   &
         X3DEYE,Y3DEYE,Z3DEYE,   &
         X3DMID,Y3DMID,Z3DMID,   &
         AEYEXC,AEYEYC,AEYEZC,   &
         IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!     -----PEDESTAL-----
!
!               ******************************************
!               **  TREAT THE PEDESTAL GRID COLOR CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'PEDE')GO TO 2100
      GO TO 2199
!
 2100 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GO TO 2110
      GO TO 2199
 2110 CONTINUE
      CALL DPPEGC(IHARG,NUMARG,IDEPGC,IPEDGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 2199 CONTINUE
!
!               *********************************************
!               **  TREAT THE PEDESTAL GRID PATTERN  CASE  **
!               *********************************************
!
      IF(ICOM.EQ.'PEDE')GO TO 2200
      GO TO 2299
!
 2200 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GO TO 2210
      GO TO 2299
 2210 CONTINUE
      CALL DPPEGP(IHARG,NUMARG,IDEPGP,IPEDGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 2299 CONTINUE
!
!               *************************************
!               **  TREAT THE PEDESTAL GRID  CASE  **
!               *************************************
!
      IF(ICOM.EQ.'PEDE')GO TO 2300
      GO TO 2399
!
 2300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GO TO 2310
      GO TO 2399
 2310 CONTINUE
      CALL DPPEGR(IHARG,NUMARG,IDEPGR,IPEDGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 2399 CONTINUE
!
!               **************************************
!               **  TREAT THE PEDESTAL CASE         **
!               **  TREAT THE PEDESTAL COLOR  CASE  **
!               **  TREAT THE PEDESTAL SIZE   CASE  **
!               **  TREAT THE PEDESTAL BASE   CASE  **
!               **************************************
!
      IF(ICOM.EQ.'PEDE')THEN
        IF(NUMARG.GE.1.AND.   &
          (IHARG(1).EQ.'COLO'.OR.IHARG(1).EQ.'RGB '))THEN
          CALL DPPECL(IHARG,IARG,NUMARG,IDEPCO,IRGBMX,IPEDCO,IPEDC2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.   &
              (IHARG(1).EQ.'SIZE' .OR. IHARG(1).EQ.'HEIG'))THEN
          CALL DPPESZ(IHARG,IARGT,ARG,NUMARG,   &
                      ADEPSZ,APEDSZ,   &
                      IFOUND,IERROR)
           IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'BASE')THEN
          CALL DPPEBA(IHARG,IARGT,ARG,NUMARG,   &
                      ADEPBA,APEDBA,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        ELSE
          CALL DPPED(IHARG,NUMARG,IPEDSW,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!     -----BASEPLANE-----
!
!               ******************************************
!               **  TREAT THE BASEPLANE GRID COLOR CASE **
!               ******************************************
!
      IF(ICOM.EQ.'BASE')GO TO 3100
      GO TO 3199
!
 3100 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GO TO 3110
      GO TO 3199
 3110 CONTINUE
      CALL DPBSGC(IHARG,NUMARG,IDBSGC,IBSPGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3199 CONTINUE
!
!               *********************************************
!               **  TREAT THE BASEPLANE GRID PATTERN  CASE **
!               *********************************************
!
      IF(ICOM.EQ.'BASE')GO TO 3200
      GO TO 3299
!
 3200 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GO TO 3210
      GO TO 3299
 3210 CONTINUE
      CALL DPBSGP(IHARG,NUMARG,IDBSGP,IBSPGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3299 CONTINUE
!
!               *************************************
!               **  TREAT THE BASEPLANE GRID  CASE **
!               *************************************
!
      IF(ICOM.EQ.'BASE')GO TO 3300
      GO TO 3399
!
 3300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GO TO 3310
      GO TO 3399
 3310 CONTINUE
      CALL DPBSGR(IHARG,NUMARG,IDBSGR,IBSPGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3399 CONTINUE
!
!               *************************************
!               **  TREAT THE BASEPLANE COLOR CASE **
!               *************************************
!
      IF(ICOM.EQ.'BASE')GO TO 3400
      GO TO 3499
!
 3400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 3410
      GO TO 3499
 3410 CONTINUE
      CALL DPBSCL(IHARG,NUMARG,IDBSCO,IBSPCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3499 CONTINUE
!
!               *******************************
!               **  TREAT THE BASEPLANE CASE **
!               *******************************
!
      IF(ICOM.EQ.'BASE')GO TO 3500
      GO TO 3599
!
 3500 CONTINUE
      CALL DPBSP(IHARG,NUMARG,IBSPSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 3599 CONTINUE
!
!     -----BACKPLANE-----
!
!               ******************************************
!               **  TREAT THE BACKPLANE GRID COLOR CASE **
!               ******************************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GO TO 4100
      GO TO 4199
!
 4100 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GO TO 4110
      GO TO 4199
 4110 CONTINUE
      CALL DPBKGC(IHARG,NUMARG,IDBKGC,IBKPGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4199 CONTINUE
!
!               *********************************************
!               **  TREAT THE BACKPLANE GRID PATTERN  CASE **
!               *********************************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GO TO 4200
      GO TO 4299
!
 4200 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GO TO 4210
      GO TO 4299
 4210 CONTINUE
      CALL DPBKGP(IHARG,NUMARG,IDBKGP,IBKPGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4299 CONTINUE
!
!               *************************************
!               **  TREAT THE BACKPLANE GRID  CASE **
!               *************************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GO TO 4300
      GO TO 4399
!
 4300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GO TO 4310
      GO TO 4399
 4310 CONTINUE
      CALL DPBKGR(IHARG,NUMARG,IDBKGR,IBKPGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4399 CONTINUE
!
!               *************************************
!               **  TREAT THE BACKPLANE COLOR CASE **
!               *************************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GO TO 4400
      GO TO 4499
!
 4400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 4410
      GO TO 4499
 4410 CONTINUE
      CALL DPBKCL(IHARG,NUMARG,IDBKCO,IBKPCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4499 CONTINUE
!
!               *******************************
!               **  TREAT THE BACKPLANE CASE **
!               *******************************
!
      IF(ICOM.EQ.'BACK'.AND.ICOM2.EQ.'PLAN')GO TO 4500
      GO TO 4599
!
 4500 CONTINUE
      CALL DPBKP(IHARG,NUMARG,IBKPSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 4599 CONTINUE
!
!     -----SIDEFACE-----
!
!               ******************************************
!               **  TREAT THE SIDEFACE GRID COLOR CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'SIDE')GO TO 5100
      GO TO 5199
!
 5100 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'COLO')GO TO 5110
      GO TO 5199
 5110 CONTINUE
      CALL DPSDGC(IHARG,NUMARG,IDSDGC,ISDFGC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5199 CONTINUE
!
!               *********************************************
!               **  TREAT THE SIDEFACE GRID PATTERN  CASE  **
!               *********************************************
!
      IF(ICOM.EQ.'SIDE')GO TO 5200
      GO TO 5299
!
 5200 CONTINUE
      IF(NUMARG.GE.2.AND.   &
      IHARG(1).EQ.'GRID'.AND.IHARG(2).EQ.'PATT')GO TO 5210
      GO TO 5299
 5210 CONTINUE
      CALL DPSDGP(IHARG,NUMARG,IDSDGP,ISDFGP,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5299 CONTINUE
!
!               *************************************
!               **  TREAT THE SIDEFACE GRID  CASE  **
!               *************************************
!
      IF(ICOM.EQ.'SIDE')GO TO 5300
      GO TO 5399
!
 5300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GRID')GO TO 5310
      GO TO 5399
 5310 CONTINUE
      CALL DPSDGR(IHARG,NUMARG,IDSDGR,ISDFGR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5399 CONTINUE
!
!               *************************************
!               **  TREAT THE SIDEFACE COLOR CASE  **
!               *************************************
!
      IF(ICOM.EQ.'SIDE')GO TO 5400
      GO TO 5499
!
 5400 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 5410
      GO TO 5499
 5410 CONTINUE
      CALL DPSDCL(IHARG,NUMARG,IDSDCO,ISDFCO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5499 CONTINUE
!
!               *******************************
!               **  TREAT THE SIDEFACE CASE  **
!               *******************************
!
      IF(ICOM.EQ.'SIDE')GO TO 5500
      GO TO 5599
!
 5500 CONTINUE
      CALL DPSDF(IHARG,NUMARG,ISDFSW,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5599 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE MAIPC4
      SUBROUTINE MAINSU(IDEFSE,ISEED,ANOPL1,ANOPL2,   &
                        ISQUAR,IBOOSS,IDEBOO,   &
                        IANSSV,IREPMX,ILISMX,IPOINT,   &
                        ISACNC,IAUTSW,IAUTEX,ITOPIC,MAXNXT,IPROSW,   &
                        IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,   &
                        IOFILE,IMALEV,IPROGR,ICONCL,   &
                        ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,   &
                        ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,   &
                        IBASLC,IREPCH,IOSW,ICAPSW,IPRDEF,   &
                        IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
                        IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,   &
                        ICPREH,NCPREH,ICPOSH,NCPOSH,IOUTTY,IPRITY,   &
                        IHELMX,IFTEXP,IFTORD,ALOWFR,ALOWDG,   &
                        IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,   &
                        IREARW,IWRIRW,   &
                        IUNFOF,IUNFNR,IUNFMC,   &
                        IRHSTG,IMPSW,IERRFA,IGUIFL,IGUIFB,   &
                        ITABTI,NCTABT,ITABBR,ITABSP,ITABWD,ITABHT,   &
                        IANSLO,ILOOST,ILOOLI,NUMLIL,NUMLOS,IWIDLL,   &
                        IIFSW,NUMIF,   &
                        NPLOTP,IFOUND,IERROR)
!
!CCCC IBASLC WAS ADDED TO ABOVE INPUT ARGUMENT LIST       JUNE 1989
!CCCC ICAPSW AND IPRDEF WERE ADDED TO ABOVE ARGUMENT LIST JUNE 1989
!CCCC ADD "LOOP" ARGUMENTS (FOR READ COMMAND) JANUARY 2015.
!
!     PURPOSE--THIS IS SUBROUTING MAINSU.
!
!              (THE   SU    AT THE END OF    MAINSU   STANDS FOR   SUPPORT)
!              THIS SUBROUTINE SEARCHES FOR AND EXECUTES SUPPORT COMMANDS.
!              THE SUPPORT COMMANDS SEARCHED FOR BY MAINSU ARE AS FOLLOWS--
!
!                     ADD                            N/A                 ADD CAL
!                     ANOP LIMITS (= PROPORTION LIMITS)     +-INFINITY
!                     BAUD                           9600                BAUD 12
!                     BUGS                           N/A                 BUGS
!                     CLASS ... LOWER                AUTOMATIC           CLASS L
!                     CLASS ... UPPER                AUTOMATIC FROM DATA CLASS U
!                     CLASS ... WIDTH                AUTOMATIC FROM DATA CLASS W
!                     COLUMN LIMITS                  1 132               COLUMN
!                     COMMENT                        N/A                 COMMENT
!                     CURSOR SIZE                    1.0                 CURSOR
!                     DEFAULT COMMAND                NO COMMAND          DEFAULT
!                     DELETE                         N/A                 DELETE
!                     DEMODULATION FREQUENCY         0.25                DEMODUL
!                     DIMENSION                      1000 ROWS 10 COLS   DIMENSI
!                     DOUBLE PRECISION               OFF = SING. PREC.   DOUBLE
!                     ECHO                           OFF = NO ECHO       ECHO ON
!                     END                            N/A                 END
!                     ERASE DELAY                    1                   ERASE D
!                     FEEDBACK                       ON = FEEDBACK       FEEDBAC
!                     FILTER WIDTH                   3                   FILTER
!                     FIT CONSTRAINT                 ALL UNCONSTRAINED   FIT CON
!                     FIT ITERATIONS                 50                  FIT ITE
!                     FIT STANDARD DEVIATION         .000005             FIT STA
!                     HARDCOPY DELAY                 1                   HARDCOP
!                     HELP                           N/A                 HELP PL
!                     HOST                           THE    LOCAL    HOST    HOS
!                     HOST LINK                      THE    LOCAL    HOST    HOS
!                     IMPLEMENT                      ORIG. INITIALIZ.     IMPLEM
!                     KNOTS                          OFF = NO KNOTS      KNOTS K
!                     MACRO (CREATE)                 OFF                 MACRO
!                     MAIL                           N/A                 MAIL JO
!                     MAXIMUM RECORD LENGTH          N/A                 MAIL JO
!                     NAME                           N/A                 NAME Y
!                     NEWS                           N/A                 NEWS
!                     OPERATOR                       N/A                 OPERAT
!                     POLYNOMIAL DEGREE              1 = LINEAR          POLYNOM
!                     PRECISION                      SINGLE              PRECISI
!                     PRE-ERASE                      ON = PRE-ERASE      PRE-ERA
!                     PRINTING                       ON = PRINTING       PRINTIN
!                     PROBE                          N/A                 PROBE N
!                     QUADRUPLE PRECISION            OFF = SING. PREC.   QUADRUP
!                     QUERY                          N/A                 QUERY H
!                     READ                           N/A                 READ CA
!                     RESET                          N/A                 RESET
!                     RESTORE                        N/A                 RESTORE
!                     RETAIN                         N/A                 RETAIN
!                     ROW LIMITS                     1 INFINITY          ROW LIM
!                     SAVE                           N/A                 SAVE SC
!                     SEED                           20867350019         SEED
!                     TERMIANATOR CHARACTER          ;                   SEPAR
!                     SERIAL READ                    N/A                 SERIAL
!                     SET                            OFF                 SET IBU
!                     SINGLE PRECISION               ON                  SINGLE
!                     SKIP                           0 = NO LINES        SKIP 5
!                     STATUS                         N/A                 STATUS
!                     TIME                           N/A                 TIME
!                     TRIPLE PRECISION               OFF = SING. PREC.   TRIPLE
!                     WEIGHTS                        OFF = EQUI-WEIGHTED WEIGHTS
!                     WRITE                          N/A                 WRITE C
!                     .                              N/A                 . CARRY
!                     CONTINUE CHARACTER             ...                 CONTI
!                     PRINTER FORMAT ASCII/POSTSCRIPT
!                     FILE FORMAT ASCII/POSTSCRIPT
!
!                     VECTOR FORMAT <ANGLE/POINT/DELTA>
!                     VECTOR ARROW  <FIXED/VARIABLE>
!                     VECTOR ARROW  <OPEN/CLOSED>
!                     ANDREWS INCREMENT
!                     OPTIMIZATION METHOD
!                     WEB HELP
!                     RECIPE SATTERWAITE APPROXIMATION
!                     RECIPE OUTPUT
!                     RECIPE PROBABILITY CONTENT (OR RECIPE CONTENT)
!                     RECIPE CONFIDENCE
!                     RECIPE FIT DEGREE (OR RECIPE DEGREE)
!                     RECIPE ANOVA FACTORS (OR RECIPE FACTORS)
!                     RECIPE CORRELATION
!                     RECIPE SIMCOV REPLICATES
!                     RECIPE SIMPVT REPLICATES
!
!                     GUI WRITE/PRINT
!                     GUI STATUS
!                     GUI PLOT CONTROL <N>
!
!                     VARIABLE LABEL
!
!                     ORTHOGONAL DISTANCE ERROR
!                     ORTHOGONAL DISTANCE DELTA
!
!                     KERNEL DENSITY WIDTH
!                     KERNEL DENSITY POINTS
!
!                     AUTO TEXT
!
!                     SYSTEM
!                     PROCES ID (OR PID)
!                     CPU TIME
!                     PWD       (OR GETCWD, CURRENT DIRECTORY)
!                     CLIPBOARD CLEAR (OR CLEAR CLIPBOARD)
!                     CLIPBOARD
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
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --JANUARY   1982.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 1983.
!     UPDATED         --JANUARY   1986.
!     UPDATED         --OCTOBER   1987. (ISUBRO FOR DPAPPE AND DPEXTE)
!     UPDATED         --AUGUST    1988. EQUATE PROPORTION LIMITS WITH ANOP LIM
!     UPDATED         --DECEMBER  1988. ADJUST RESET FOR RESET2
!     UPDATED         --DECEMBER  1988. RESET DATA, IO, PC, ETC.
!     UPDATED         --DECEMBER  1988. SET WRITE FORMAT
!     UPDATED         --DECEMBER  1988.  SET READ  REWIND
!     UPDATED         --DECEMBER  1988.  SET WRITE REWIND
!     UPDATED         --DECEMBER  1988.  LOWESS FRACTION
!     UPDATED         --DECEMBER  1988.  READ/WRITE DECI, FORMAT, REWIND
!     UPDATED         --JANUARY   1989.  BOOTSTRAP SAMPLE SIZE
!     UPDATED         --FEBRUARY  1989.  CONTINUE CHARACTER (ALAN)
!     UPDATED         --FEBRUARY  1989.  SOFT-CODED LIMITS FOR IANSSV (ALAN)
!     UPDATED         --FEBRUARY  1989.  SYSTEM COMMAND (ALAN)
!     UPDATED         --JUNE      1989.  REPLACEMENT/SUBSTITUTION CHARACTER
!     UPDATED         --JUNE      1989.  CAPTURE (TEXT OUTPUT)
!     UPDATED         --JULY      1989.  MORE/PAUSE TO LIST
!     UPDATED         --NOVEMBER  1989.  COLUMN RULER
!     UPDATED         --NOVEMBER  1989.  NLIST
!     UPDATED         --NOVEMBER  1989.  ADD ARG TO CALL TO DPSYST
!     UPDATED         --MARCH     1990.  ADD ARGUMENT TO SYSTEM COMMAND (ALAN)
!     UPDATED         --MAY       1990.  ADD ARGUMENTS TO DPREAD, DPREAL
!     UPDATED         --MAY       1990.  COMMENT CHARACTER COMMAND
!     UPDATED         --JUNE      1990.  IBUGD2 TO IBUGS2 IN CALL TO DPSYST
!     UPDATED         --JULY      1990.  ICOMFL RENAMED AS ICOMSW
!     UPDATED         --SEPTEMBER 1990.  DOS, UNIX, ETC. FOR SYSTEM
!     UPDATED         --SEPTEMBER 1990.  DATE SYNONYM FOR TIME
!     UPDATED         --MARCH     1992.  PRINTER FORMAT ASCI/POST
!     UPDATED         --MARCH     1992.  FILE FORMAT ASCI/POST
!     UPDATED         --APRIL     1992.  ADD NPLOTP TO ARGS
!     UPDATED         --AUGUST    1992.  VECTOR FORMAT, VECTOR ARROW
!     UPDATED         --SEPTEMBER 1992.  LIST SYNONYMS: VIEW/PREVIEW
!     UPDATED         --NOVEMBER  1992.  ANDREWS INCREMENT
!     UPDATED         --JULY      1993.  FRACTAL ITERATIONS
!     UPDATED         --JULY      1993.  FRACTAL TYPE
!     UPDATED         --JULY      1993.  PRINCIPLE COMPONENT TYPE
!     UPDATED         --JULY      1993.  ADD ARGS TO DPLICO: MORE
!     UPDATED         --SEPTEMBER 1993.  REWRITE CODE AROUND DPLICO
!     UPDATED         --DECEMBER  1993.  CHECK FOR "SAVE" AND "S CHART"
!                                        CONFLICT.
!     UPDATED         --JANUARY   1994.  SEARCH1
!     UPDATED         --MAY       1994.  COPY FILE => COPY
!     UPDATED         --JUNE      1994.  OPTIMIZATION TOLERANCE
!     UPDATED         --AUGUST    1994.  EXECUTE SUBSET OF MACRO
!     UPDATED         --SEPTEMBER 1994.  CHECK FOR NAME CONFLICT
!     UPDATED         --NOVEMBER  1994.  DECLARE NEWNAM (BOMB ON VAX)
!     UPDATED         --FEBRUARY  1995.  OPTIMIZATION METHOD
!     UPDATED         --APRIL     1995.  IUNFOF, IUNFNR, IUNFMC
!     UPDATED         --AUGUST    1995.  ADD IFTORD
!     UPDATED         --SEPTEMBER 1995.  ISUBRO ADDED TO CALL DPDELE
!     UPDATED         --SEPTEMBER 1995.  INIT COMMAND (FOR DEBUGGING)
!     UPDATED         --OCTOBER   1995.  NAME CONFLICT WITH DOUBLE
!     UPDATED         --MARCH     1997.  SUPPORT FOR DEVICE FONT (ALAN)
!     UPDATED         --APRIL     1997.  WEB HELP COMMAND (ALAN)
!     UPDATED         --APRIL     1997.  LIST GRAPH (ALAN)
!     UPDATED         --APRIL     1997.  SAVE GRAPH (ALAN)
!     UPDATED         --APRIL     1997.  REPEAT GRAPH (ALAN)
!     UPDATED         --APRIL     1997.  CYCLE GRAPH (ALAN)
!     UPDATED         --AUGUST    1997.  SLEEP (= PAUSE <n>)
!     UPDATED         --AUGUST    1997.  CD COMMAND
!     UPDATED         --AUGUST    1997.  6 RECIPE COMMANDS
!     UPDATED         --NOVEMBER  1997.  GUI PRINT/WRITE
!     UPDATED         --NOVEMBER  1997.  GUI STATUS
!     UPDATED         --NOVEMBER  1997.  GUI SAVE PLOT CONTROL
!     UPDATED         --JANUARY   1998.  CALL TO DPDIME
!     UPDATED         --NOVEMBER  1998.  CALL LIST TO DPSET, DPPROB
!     UPDATED         --MARCH     1998.  NAME CONFLICT WITH CP AND CP PLOT
!     UPDATED         --APRIL     1997.  RECIPE FIT FACTORS COMMANDS
!     UPDATED         --MARCH     1999.  NAME CONFLICT FOR SINGLE
!     UPDATED         --NOVEMBER  1999.  VARIABLE LABEL
!     UPDATED         --APRIL     2001.  ORTHOGONAL DISTANCE ERROR
!     UPDATED         --APRIL     2001.  ORTHOGONAL DISTANCE DELTA
!     UPDATED         --AUGUST    2001.  KERNEL DENSITY WIDTH/POINTS
!     UPDATED         --JUNE      2002.  ICAPTY IN DPCAPT CALL
!     UPDATED         --FEBRUARY  2003.  CALL TO DPREAD, DPSERI
!     UPDATED         --FEBRUARY  2003.  CALL TO DPCOLL
!     UPDATED         --FEBRUARY  2003.  ADD: MAXIMUM RECORD LENGTH
!     UPDATED         --FEBRUARY  2003.  CALL LIST TO DPSEAR
!     UPDATED         --SEPTEMBER 2003.  CALL LIST TO DPWRIT
!     UPDATED         --SEPTEMBER 2005.  CALL LIST TO DPMACR
!     UPDATED         --SEPTEMBER 2005.  MACRO SUBSTITUTION CHARACTER
!     UPDATED         --JANUARY   2006.  ARGUMENT LIST TO DPCAPT
!     UPDATED         --MARCH     2006.  PROCESS ID
!     UPDATED         --AUGUST    2007.  USER-DEFINED ACTION ON
!                                        ERROR
!     UPDATED         --SEPTEMBER 2007.  IERRST
!     UPDATED         --MAY       2008.  GUI FEEDBACK SWITCH
!     UPDATED         --APRIL     2009.  TABLE WIDTH COMMAND
!     UPDATED         --APRIL     2009.  CALL LIST TO DPWRIT
!     UPDATED         --MAY       2009.  ADD CPU TIME COMMAND
!     UPDATED         --MAY       2010.  REMOVE "MAIL" AND "QUERY"
!                                        COMMANDS
!     UPDATED         --JANUARY   2011.  ADD PWD COMMAND
!     UPDATED         --NOVEMBER  2014.  CLIPBOARD CLEAR
!     UPDATED         --NOVEMBER  2014.  CLIPBOARD RUN
!     UPDATED         --JANUARY   2015. LOOP ARGUMENTS TO DPREAD
!     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
!     UPDATED         --MARCH     2015. CALL LIST TO UPDATF
!     UPDATED         --NOVEMBER  2015. CALL LIST TO DPMACR
!     UPDATED         --DECEMBER  2015. CALL LIST TO MAININ
!     UPDATED         --JULY      2016. STREAM READ
!     UPDATED         --JULY      2017. CALL LIST TO DPMACR,
!                                       CALL LIST TO MAINSU
!     UPDATED         --JULY      2017. INSERT CALL ARGUMENTS COMMAND
!     UPDATED         --JULY      2017. ISSUE WITH COMMAND LINE
!                                       ARGUMENTS IN LOOP STORE MODE,
!                                       DO COMMAND LINE SUBSTITUTION
!                                       FOR FILE NAME, BUT NOT ARGUMENTS
!     UPDATED         --APRIL     2018. CALL LISTS TO DPHELW, DPHANW,
!                                       DPWEB
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --FEBRUARY  2019. SUPPORT FOR "CALL CLIPBOARD"
!     UPDATED         --SEPTEMBER 2019. SUPPORT FOR "GREP" AS SEARCH
!                                       OPTION
!     UPDATED         --SEPTEMBER 2019. ADD "RM" AND "RMDIR" COMMANDS
!     UPDATED         --SEPTEMBER 2019. ADD "MKDIR" COMMAND
!     UPDATED         --SEPTEMBER 2019. ADD "CAT" COMMAND
!     UPDATED         --SEPTEMBER 2019. ADD "DIR" COMMAND
!     UPDATED         --SEPTEMBER 2019. FOR LIST AND SAVE, CHECK IF
!                                       FIRST ARGUMENT IS "="
!     UPDATED         --OCTOBER   2019. "HEAD" AND "TAIL" OPTIONS FOR
!                                       WRITE COMMAND
!     UPDATED         --NOVEMBER  2019. "RSCRIPT" AND "PYTHON"
!                                       COMMANDS
!     UPDATED         --FEBRUARY  2020. FOR "CLIPBOARD" COMMANDS,
!                                       CHECK IF "CLIPBOARD" ARGUMENT
!                                       IS ACTUALLY A FILE NAME.
!     UPDATED         --AUGUST    2020. FOR CYCLE GRAPH COMMAND, CHECK
!                                       FOR CONFLICT WITH:
!                                           CP CUMULATIVE STATISTIC PLOT
!                                           CP MOVING     STATISTIC PLOT
!     UPDATED         --DECEMBER  2020. ADDED SAVE VARIABLE COMMAND
!     UPDATED         --DECEMBER  2020. ADDED RESTORE VARIABLE COMMAND
!     UPDATED         --APRIL     2021. ADD "X" AS SYNOMYM FOR "CALL
!                                       CLIPBOARD"
!     UPDATED         --APRIL     2021. ADD ALIAS COMMAND
!     UPDATED         --MARCH     2025. FOR WEIGHTS COMMAND, CHECK FOR
!                                       WEIGHTED DEMING FIT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 IMPSW
      CHARACTER*4 ILOOST
      CHARACTER*4 IIFSW
!
      CHARACTER*4 ISQUAR
      CHARACTER*4 ITOPIC
      CHARACTER*4 IPROSW
!
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 IOFILE
!
      CHARACTER*4 IPROGR
      CHARACTER*4 ICONCL
!
      CHARACTER*4 ICOM3
      CHARACTER*4 ICOM4
      CHARACTER*40 ICOM5
!
      CHARACTER*30 ICTRA1
      CHARACTER*30 ICTRA2
!
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
!
      CHARACTER*40 ICPREH
      CHARACTER*40 ICPOSH
!
!CCCC THE FOLLOWING 2 LINES WERE ADDED    MARCH 1992
      CHARACTER*4 IPRITY
      CHARACTER*4 IOUTTY
!
      CHARACTER*4 IFTEXP
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IFTORD
!
      CHARACTER*4 IFORSW
      CHARACTER*80 ICREAF
      CHARACTER*80 ICWRIF
!
      CHARACTER*4 IREARW
      CHARACTER*4 IWRIRW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IDEFHL
      CHARACTER*4 IHOSLI
!
      CHARACTER*1 IANSSV
!CCCC CHARACTER*80 ISACNC
      CHARACTER (LEN=MAXFNC) :: ISACNC
!
      CHARACTER*4 IAUTSW
      CHARACTER*4 IAUTEX
      CHARACTER*4 IBELSJ
      CHARACTER*4 IERASJ
      CHARACTER*4 IBACCJ
      CHARACTER*4 ICOPSJ
!
      CHARACTER*4 ISEART
!
!CCCC THE FOLLOWING LINE WAS ADDED JUNE 1989
      CHARACTER*4 ICAPSW
!
!CCCC THE FOLLOWING 5 LINES WERE ADDED FEBRUARY 1993
      CHARACTER*24 CURRTI
      CHARACTER*24 CURRDA
      CHARACTER*4 IC4
      CHARACTER*4 IFOUNN
      CHARACTER*4 IERRON
!
!CCCC THE FOLLOWING LINE NOVEMBER 1994
      CHARACTER*4 NEWNAM
!CCCC THE FOLLOWING 2 LINES WERE ADDED SEPTEMBER 1995
      CHARACTER*4 ICOMHO
      CHARACTER*4 ICOMH2
!CCCC THE FOLLOWING 2 LINES WERE ADDED OCTOBER 1996
      CHARACTER*4 IRHSTG
!CCCC THE FOLLOWING 2 LINES WERE ADDED SEPTEMBER 2003
      CHARACTER*4 ITABBR
      CHARACTER*80 ITABTI
!
      CHARACTER*4 ICASOD
!
      CHARACTER*4 IERRFA
      CHARACTER*4 IGUIFL
      CHARACTER*4 IGUIFB
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASE2
!
      CHARACTER*4 IFUTMP(100)
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
!
      CHARACTER*1 IQUOTE
!CCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
!
      DIMENSION IDEFHL(10)
      DIMENSION IHOSLI(10)
!
!CCCC DIMENSION IANSSV(50,80)
      DIMENSION IANSSV(MAXLIS,MAXCIS)
      CHARACTER*4 IANSLO(MAXLIL,MAXCIL)
      DIMENSION IWIDLL(MAXLIL)
!
      DIMENSION ICOM3(*)
      DIMENSION ICOM4(*)
      DIMENSION ICOM5(*)
      DIMENSION NCOM5(*)
!
      DIMENSION ICTRA1(*)
      DIMENSION NCTRA1(*)
      DIMENSION ICTRA2(*)
      DIMENSION NCTRA2(*)
!
      CHARACTER*4 IFEESV
!
      CHARACTER*255 CURDIR
      CHARACTER*4   IFUNC9(255)
!
!-----COMMON----------------------------------------------------------
!
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
!CCCC THE FOLLOWING LINE WAS INSERTED NOVEMBER 1989
      INCLUDE 'DPCODE.INC'
!
!CCCC TO AVOID NAME CONFLICTS, ONLY BRING IN THE SPECIFIC
!CCCC COMMON BLOCK
!
      CHARACTER*4  IERRST
      COMMON/CSETG/IERRST
!
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 70 I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                 I8,3(2X,A4),2X,I8,G15.7)
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
!CCCC   THE FOLLOWING 2 LINES WERE INSERTED NOVEMBER 1989
        WRITE(ICOUT,86)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
   86   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,   &
               2(2X,A4))
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
!
      IFOUND='NO'
      IERROR='NO'
!
!               ******************************
!               **  TREAT THE ADD     CASE  **
!               **  TREAT THE CALL    CASE  **
!               **  TREAT THE EXECUTE CASE  **
!               **  TREAT THE RUN     CASE  **
!               ******************************
!
!     2015/03: CALL EXIT AND CALL EXIT ALL CASES SUPPORTED
!
!     2021/04: ADD "X" AS A SYNONYM FOR "CALL CLIPBOARD"
!
      IF(ICOM.EQ.'X   ' .AND. NUMARG.LE.0)THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
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
        DO 2031 II=1,IWIDTH
          IANSLC(II)=IANS(II)
 2031   CONTINUE
      ENDIF
!
      IF(ICOM.EQ.'ADD' .OR. ICOM.EQ.'CALL' .OR.   &
         ICOM.EQ.'EXEC' .OR. ICOM.EQ.'RUN ')THEN
!
!       2025/05: CHECK FOR CONFLICT WIH RUN SEQUENCE PLOT COMMAND
!
        IF(ICOM.EQ.'RUN ' .AND. IHARG(1).EQ.'SEQU' .AND.   &
           IHARG(2).EQ.'PLOT')GO TO 13390
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'CLIP' .AND.      &
           IHARG2(1).EQ.'BOAR')THEN
!
!         CHECK IF ARGUMENT IS A FILE NAME STARTING WITH
!         "CLIPBOARD.
!
          IWORD=2
          IOFILE='NO'
          CALL DPFILE(IANSLC,IWIDTH,IWORD,   &
                      IOFILE,IBUGS2,ISUBRO,IERROR)
          IF(IOFILE.EQ.'NO')GO TO 13390
        ENDIF
!
        IF(NUMARG.EQ.1 .AND.   &
          IHARG(1).EQ.'EXIT')THEN
          IMACCS='CLO2        '
          IMACRO='EOF'
          IFOUND='YES'
        ELSE
!
!         2015/11: COMMAND LINE SUBSTITUTION (ISSUE FOR LOOPS)
!
          IF(ILOOST.NE.'STOR')THEN
            CALL DPREP2(IANSLC,IWIDTH,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVARLB,IROWLB,MAXOBV,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,   &
                        IBUGS2,ISUBRO,IERROR)
          ELSE
!
!           IN PROCESSING THE CALL COMMAND IN STORE MODE, WE ACTUALLY WANT
!           TO PERFORM COMMAND LINE SUBSTITION FOR THE FILE NAME, BUT NOT
!           FOR THE COMMAND LINE ARGUMENTS.  IF THERE ARE NO COMMAND LINE
!           ARGUMENTS, THEN NO SPECIAL PROCESSING NEEDED.
!
            ICANS=' '
            DO 13301 II=1,IWIDTH
              ICANS(II:II)=IANSLC(II)(1:1)
13301       CONTINUE
!
!           NOW SEARCH FOR THE LOCATION OF THE FIRST TWO WORDS.
!
            IQUOTE='"'
            ISTART=0
            DO 13303 II=1,IWIDTH
              IF(ICANS(II:II).NE.' ')THEN
                ISTART=1
                GO TO 13309
              ENDIF
13303       CONTINUE
            GO TO 13399
13309       CONTINUE
!
            IQFLAG=0
            IF(ICANS(ISTART:ISTART).EQ.IQUOTE)IQFLAG=1
            DO 13310 KK=1,2
              DO 13311 II=ISTART,IWIDTH
                IF(IQFLAG.EQ.0)THEN
                  IF(ICANS(II:II).EQ.' ')THEN
                    ISTOP=II-1
                    GO TO 13319
                  ENDIF
                ELSE
                  IF(ICANS(II:II).EQ.IQUOTE)THEN
                    ISTOP=II
                    GO TO 13319
                  ENDIF
                ENDIF
13311         CONTINUE
              CALL DPREP2(IANSLC,IWIDTH,   &
                          IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                          IVARLB,IROWLB,MAXOBV,   &
                          IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,   &
                          IBUGS2,ISUBRO,IERROR)
              GO TO 13399
13319         CONTINUE
              IF(KK.EQ.1)THEN
                ISTART=ISTOP+2
              ENDIF
13310       CONTINUE
!
!           NOW DO COMMAND LINE SUBSTITUTION FOR THE FIRST ISTOP
!           CHARACTERS.
!
            ISTOP2=ISTOP
            CALL DPREP2(IANSLC,ISTOP,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVARLB,IROWLB,MAXOBV,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,   &
                        IBUGS2,ISUBRO,IERROR)
            ICNT=ISTOP
            DO 13321 II=ISTOP2+1,IWIDTH
              ICNT=ICNT+1
              IANSLC(ICNT)(1:1)=ICANS(II:II)
13321       CONTINUE
            IWIDTH=ICNT
          ENDIF
        ENDIF
!
13399   CONTINUE
        CALL DPMACR(ICOM,ICOM2,   &
                    IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,IMALEV,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    IANSLC,IANS,IWIDTH,   &
                    IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IOFILE,   &
                    ILOOST,ILOOLI,NUMLIL,NUMLOS,   &
                    IANSLO,IWIDLL,MAXCIL,MAXLIL,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
!
!       IF "CALL EXIT" OR "CALL EXIT ALL" ENTERED, THEN DEPRECATE
!       CURRENT IF SWITCH SETTING.
!
        IF(ICOM.EQ.'CALL' .AND. IHARG(1).EQ.'EXIT')THEN
          IF(IIFSW.EQ.'TRUE' .AND. NUMIF.GT.0)NUMIF=NUMIF-1
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
13390 CONTINUE
!               **********************************************
!               **  TREAT THE INSERT CALL ARGUMENTS  CASE  **
!               **********************************************
!
      IF(ICOM.EQ.'INSE' .AND. IHARG(1).EQ.'CALL' .AND.   &
         IHARG(2).EQ.'ARGU')THEN
        CALL DPICLA(ICOM,ICOM2,   &
                    IMACRO,IMACNU,IMACCS,   &
                    IMACL1,IMACL2,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    IANSLC,IWIDTH,   &
                    IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE DEFINE  CASE  **
!               ******************************
!
      IF(ICOM.EQ.'DEFI')THEN
        CALL DPDEFI(IHARG,IHARG2,IHARLC,NUMARG,   &
                    ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,   &
                    ICPREP,NCPREP,ICPOST,NCPOST,   &
                    ICPREH,NCPREH,ICPOSH,NCPOSH,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE ALIAS   CASE  **
!               ******************************
!
      IF(ICOM.EQ.'ALIA')THEN
        CALL DPALIA(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE TRANSLATE CASE  **
!               ********************************
!
      IF(ICOM.EQ.'TRAN')THEN
        CALL DPTRAN(IHARG,IHARG2,NUMARG,   &
                    ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE BAUD CASE  **
!               ***************************
!
      IF(ICOM.EQ.'BAUD')THEN
        CALL DPBAUD(IHARG,IARGT,IARG,NUMARG,IDEFBA,   &
                    IBAUD,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IGBAUD=IBAUD
          DO 415 I=1,MAXDEV
            IDBAUD(I)=IBAUD
  415     CONTINUE
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE COLUMN LIMITS CASE  **
!               ************************************
!
!CCCC IF(ICOM.EQ.'COLU')GO TO 500
!  DECEMBER, 1989.  CHECK FOR CONFLICT WWITH COLUMN RULER COMMAND.
      IF(ICOM.EQ.'COLU'.AND.IHARG(1).NE.'RULE')GO TO 500
      GO TO 599
!
  500 CONTINUE
      CALL DPCOLL(IDEFC1,IDEFC2,IFCOL1,IFCOL2,   &
                  NUMRCM,IFCOLL,IFCOLU,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
  599 CONTINUE
!
!               ************************************
!               **  TREAT THE TABLE WIDTH   CASE  **
!               ************************************
!
      IF(ICOM.EQ.'TABL'.AND.IHARG(1).EQ.'WIDT')THEN
        CALL DPTAWI(IFORWI,IFORWR,MAXNWI,   &
                    ISUBRO,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************************
!               **  TREAT THE MAXIMUM RECORD LENGTH CASE  **
!               ********************************************
!
      IF(ICOM.EQ.'MAXI'.AND.IHARG(1).EQ.'RECO'.AND.IHARG(2).EQ.'LENG')   &
        THEN
        CALL DPMXRL(IHARG,IARGT,IARG,NUMARG,IDEFRL,NUMRCM,MAXRCL,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE DEGREES CASE  **
!               ******************************
!
!     (THE FOLLOWING IS COMMENTED OUT
!     (THE FOLLOWING IS COMMENTED OUT
!     (THE FOLLOWING IS COMMENTED OUT
!     IN THE SUBROUTINE MAINDG)
!
!CCCC IF(ICOM.EQ.'DEGR'.AND.ICOM2.EQ.'EES ')GO TO 700
!CCCC GO TO 799
!
!C700 CONTINUE
!CCCC CALL DPDEGS(IHARG,NUMARG,IDEFAU,
!CCCC1IANGLU,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!C799 CONTINUE
!
!               *****************************
!               **  TREAT THE DELETE CASE  **
!               *****************************
!
      IF(ICOM.EQ.'DELE')THEN
!CCCC   THE FOLLOWING LINE WAS FIXED     SEPTEMBER 1995
!CCCC   CALL DPDELE(IBUGS2,IBUGQ,IFOUND,IERROR)
        CALL DPDELE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE DEMODULATION FREQUENCY CASE  **
!               *********************************************
!
      IF(ICOM.EQ.'DEMO')THEN
        CALL DPDEFR(IHARG,IARGT,ARG,NUMARG,DEFDMF,   &
                    DEMOFR,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE DIMENSION CASE    **
!               **  TREAT THE REDIMENSION CASE  **
!               **********************************
!
      IF(ICOM.EQ.'DIME' .OR. ICOM.EQ.'REDI' .OR.   &
        (ICOM.EQ.'MATR' .AND. IHARG(1).EQ.'DIME'))THEN
        CALL DPDIME(IANS,IHARG,IARGT,IARG,NUMARG,IDEMXN,IDEMXC,   &
                    IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                    IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    V,MAXNK,NUMN,MAXN,MAXNXT,   &
                    MAXTOM,MAXROM,MAXCOM,MAXOBV,   &
                    NUMCOL,MAXCOL,IFOUND,IERROR,IBUGS2,ISUBRO)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE DOUBLE PRECISION CASE  **
!               ***************************************
!
      IF(ICOM.EQ.'DOUB')THEN
!CCCC   CHECK FOR CONFLICT WITH DOUBLY NON-CENTRAL F PROB PLOT.
!CCCC   SEPTEMBER 1994
!CCCC   CHECK FOR CONFLICT WITH DOUBLE EXPONENTIAL PROB PLOT.
!CCCC   OCTOBER 1995
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NONC')GO TO 9000
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'NON-')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WEIB')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'EXPO')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'GAMM')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SAMP')GO TO 9000
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PARE')GO TO 9000
        CALL DPDOUB(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                    IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE ECHO CASE  **
!               ***************************
!
      IF(ICOM.EQ.'ECHO')THEN
        CALL DPECSW(IHARG,NUMARG,   &
                    IECHO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE EXIT  CASE  **
!               **  TREAT THE END   CASE  **
!               **  TREAT THE HALT  CASE  **
!               **  TREAT THE STOP  CASE  **
!               ****************************
!
      IF((ICOM.EQ.'END ' .AND. NUMARG.LE.0) .OR.   &
        ICOM.EQ.'EXIT' .OR.   &
        ICOM.EQ.'HALT' .OR. ICOM.EQ.'STOP' .OR.   &
        ICOM.EQ.'BYE ' .OR. ICOM.EQ.'QUIT')THEN
        CALL DPEXIT(ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE ERASE DELAY CASE  **
!               **********************************
!
      IF(ICOM.EQ.'ERAS')THEN
        CALL DPERDE(IHARG,IARGT,ARG,NUMARG,DEFERD,   &
                    ERASDE,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')AGERDE=ERASDE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE FIT CONSTRAINT  CASE  **
!               **************************************
!
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'CONS')THEN
        CALL DPFICN(ICOM,IHARG,IHARG2,IARGT,ARG,NUMARG,   &
                    IPARNC,IPANC2,IPAROP,   &
                    PARLIM,PARLLM,PARULM,   &
                    NUMCON,MAXCON,IFOUND,IERROR,IBUGS2)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  TREAT THE FIT ITERATIONS CASE  **
!               *************************************
!
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'ITER')THEN
        CALL DPFIIT(IHARG,IARGT,IARG,NUMARG,IDEFNI,   &
                    IFITIT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE FIT POWER CASE  **
!               ********************************
!
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'POWE')THEN
        CALL DPFIPW(IHARG,IARGT,ARG,NUMARG,DEFFPW,   &
                    FITPOW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************************
!               **  TREAT THE FIT STANDARD DEVIATION CASE  **
!               *********************************************
!
      IF(ICOM.EQ.'FIT'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'STAN')THEN
        CALL DPFISD(IHARG,IARGT,ARG,NUMARG,DEFFSD,   &
                    FITSD,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE GRADS CASE  **
!               ****************************
!
!     (THE FOLLOWING IS COMMENTED OUT
!     BECAUSE THE ANGLE COMMAND IS NOW DONE
!     IN THE SUBROUTINE MAINDG)
!
!CCCC IF(ICOM.EQ.'GRAD')GO TO 2100
!CCCC GO TO 2199
!
!2100 CONTINUE
!CCCC CALL DPGRAD(IHARG,NUMARG,IDEFAU,
!CCCC1IANGLU,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!2199 CONTINUE
!
!               **************************************
!               **  TREAT THE HARDCOPY DELAY CASE   **
!               **************************************
!
      IF(ICOM.EQ.'HARD')THEN
        CALL DPHADE(IHARG,IARGT,ARG,NUMARG,DEFHAD,   &
                    HARDDE,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')AGCODE=HARDDE
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE HELP CASE  **
!               ***************************
!
      IF(ICOM.EQ.'HELP')THEN
!
!CCCC THE FOLLOWING CALL WAS CHANGED JULY 1990
!CCCC CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,
!CCCC1IHELMX,
!CCCC1ICPREH,NCPREH,ICPOSH,NCPOSH,
!CCCC1IBUGS2,ISUBRO,IFOUND,IERROR)
!
!CCCC THE FOLLOWING CALL WAS INSERTED JULY 1990
!CCCC AND THEN COMMENTED OUT NOVEMBER 1991
!CCCC CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,
!CCCC1IHE1CO,IHE1AL,
!CCCC1IHE2CO,IHE2AL,
!CCCC1IHE3CO,IHE3AL,
!CCCC1IHE4CO,IHE4AL,
!CCCC1IHE5CO,IHE5AL,
!CCCC1IHE6CO,IHE6AL,
!CCCC1IHE7CO,IHE7AL,
!CCCC1IHE8CO,IHE8AL,
!CCCC1IHE9CO,IHE9AL,
!CCCC1IHELMX,
!CCCC1ICPREH,NCPREH,ICPOSH,NCPOSH,
!CCCC1IBUGS2,ISUBRO,IFOUND,IERROR)
!
!CCCC THE FOLLOWING CALL WAS CHANGED BACK NOVEMBER 1991
        CALL DPHELP(IHARG,IHARG2,NUMARG,IANS,IWIDTH,   &
                    IHELMX,   &
                    ICPREH,NCPREH,ICPOSH,NCPOSH,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE HOST CASE  **
!               ***************************
!
      IF(ICOM.EQ.'HOST'.AND.IHARG(1).NE.'LINK')THEN
        CALL DPHOST(IHARG,NUMARG,IDEFHO,   &
                    IHOST,IHOST1,IHOST2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE HOST LINK CASE  **
!               ********************************
!
      IF(ICOM.EQ.'HOST'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'LINK')GO TO 2500
      IF(ICOM.EQ.'COMM'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'LINK')GO TO 2500
      IF(ICOM.EQ.'LINK')GO TO 2500
      GO TO 2599
!
 2500 CONTINUE
      IF(IHARG(1).EQ.'LINK')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IHARG(1)='LINK'
        IHARG2(1)='    '
      ENDIF
      CALL DPHOSL(IHARG,NUMARG,IDEFHL,   &
      IHOSLI,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 2599 CONTINUE
!
!               ****************************
!               **  TREAT THE KNOTS CASE  **
!               ****************************
!
      IF(ICOM.EQ.'KNOT')THEN
        CALL DPKNOT(IHARG,IHARG2,NUMARG,IDEFK1,IDEFK2,   &
                    IKNOT1,IKNOT2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ************************************
!               **  TREAT THE MACRO CASE          **
!               **  TREAT THE END MACRO CASE      **
!               **  TREAT THE END OF MACRO CASE   **
!               **  TREAT THE CREATE CASE         **
!               **  TREAT THE END CREATE CASE     **
!               **  TREAT THE END OF CREATE CASE  **
!               ************************************
!
      IF(ICOM.EQ.'MACR' .OR. ICOM.EQ.'CREA')GO TO 2700
      IF(ICOM.EQ.'END ' .AND. NUMARG.GE.1 .AND.   &
        (IHARG(1).EQ.'MACR' .OR. IHARG(1).EQ.'CREA'))GO TO 2700
      IF(ICOM.EQ.'END ' .AND. NUMARG.GE.2 .AND. IHARG(1).EQ.'OF  ' .AND.   &
        (IHARG(2).EQ.'MACR' .OR. IHARG(2).EQ.'CREA'))GO TO 2700
      GO TO 2799
!
 2700 CONTINUE
      CALL DPMACR(ICOM,ICOM2,   &
                  IMACRO,IMACNU,IMACCS,IMACL1,IMACL2,IMACLR,IMALEV,   &
                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  IANSLC,IANS,IWIDTH,   &
                  IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                  IOFILE,   &
                  ILOOST,ILOOLI,NUMLIL,NUMLOS,   &
                  IANSLO,IWIDLL,MAXCIL,MAXLIL,   &
                  IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 2799 CONTINUE
!
!               *******************************
!               **  TREAT THE OPERATOR CASE  **
!               **  TREAT THE CONSOLE  CASE  **
!               *******************************
!
      IF((ICOM.EQ.'CONS'.AND.ICOM2.EQ.'OLE ') .OR.   &
         ICOM.EQ.'OPER')THEN
        CALL DPOPMS(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE NAME CASE  **
!               ***************************
!
      IF(ICOM.EQ.'NAME' .OR. ICOM.EQ.'RENA')THEN
        CALL DPNAME(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                    IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    IVARLB,   &
                    NUMCOL,MAXCOL,MAXN,IANS,IWIDTH,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  TREAT THE VARIABLE LABEL CASE  **
!               **  NAME CONFLICTS WITH "VARIANCE" **
!               **  COMMANDS.                      **
!               *************************************
!
      IF(ICOM.EQ.'VARI'.AND.ICOM2.EQ.'ABLE')THEN
        CALL DPVLAB(IHARG,IHARG2,IARG,NUMARG,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                    NUMNAM,MAXNAM,IVARLB,   &
                    IANS,IANSLC,IWIDTH,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE POLYNOMIAL DEGREE CASE  **
!               **  TREAT THE DEGREE CASE             **
!               ****************************************
!
      IF((ICOM.EQ.'DEGR'.AND.ICOM2.EQ.'EE  ') .OR.   &
         (ICOM.EQ.'POLY'.AND.IHARG(1).NE.'AEPP'))THEN
!
!       CHECK FOR NAME CONFLICTS
!
        IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'MLE')GO TO 3199
        IF(NUMARG.GE.3 .AND. IHARG(1).EQ.'MAXI' .AND.   &
           IHARG(2).EQ.'LIKE')GO TO 3199
        IF(IHARG(1).EQ.'FIT' .OR. IHARG(2).EQ.'FIT' .OR.   &
           IHARG(3).EQ.'FIT')GO TO 3199
!
        CALL DPDEGR(IHARG,IARGT,IARG,NUMARG,IDEFDG,IDEG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
 3199 CONTINUE
!
!               ********************************
!               **  TREAT THE PRECISION CASE  **
!               ********************************
!
      IF(ICOM.EQ.'PREC' .AND. IHARG(1).NE.'PLOT')THEN
        CALL DPPREC(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                    IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE PRE-ERASE CASE  **
!               ********************************
!
      IF(ICOM.EQ.'PRE')THEN
        CALL DPPREE(IHARG,NUMARG,   &
                    IERASW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *******************************
!               **  TREAT THE PRINTING CASE  **
!               *******************************
!
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'TING')THEN
         CALL DPPRSW(IHARG,NUMARG,   &
                     IPRIN2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE PROBE CASE  **
!               ****************************
!
      IF(ICOM.EQ.'PROB' .OR. ICOM.EQ.'DUMP')THEN
        CALL DPPROB(ILISMX,IREPCH,IOSW,   &
                    IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
                    IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,   &
                    IHELMX,IFTEXP,IFTORD,   &
                    IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,   &
                    IREARW,IWRIRW,NPLOTP,IPRITY,   &
                    IUNFOF,IUNFNR,IUNFMC,IMACRO,IMALEV,   &
                    IANSLO,ILOOST,ILOOLI,   &
                    NUMIF,ISEED,   &
                    IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  TREAT THE QUADRUPLE PRECISION CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'QUAD'.AND.ICOM2.EQ.'RUPL')THEN
        CALL DPQUAD(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                    IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE RADIANS CASE  **
!               ******************************
!
!     (THE FOLLOWING IS COMMENTED OUT
!     BECAUSE THE ANGLE COMMAND IS NOW DONE
!     IN THE SUBROUTINE MAINDG)
!
!CCCC IF(ICOM.EQ.'RADI')GO TO 3700
!CCCC GO TO 3799
!
!3700 CONTINUE
!CCCC CALL DPRADI(IHARG,NUMARG,IDEFAU,
!CCCC1IANGLU,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!3799 CONTINUE
!
!               ***************************
!               **  TREAT THE READ CASE  **
!               ***************************
!
      IF(ICOM.EQ.'READ')THEN
        INTINF=I1MACH(9)
        CALL DPREAD(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,   &
                    IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,   &
                    IREARW,ICOMCH,ICOMSW,   &
                    IUNFOF,IUNFNR,IUNFMC,NUMRCM,   &
                    IFCOLL,IFCOLU,   &
                    IANSLO,ILOOST,ILOOLI,IREPCH,   &
                    IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE STREAM READ CASE  **
!               **********************************
!
      IF(ICOM.EQ.'STRE' .AND. IHARG(1).EQ.'READ')THEN
        INTINF=I1MACH(9)
        CALL DPSTRE(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,   &
                    IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,   &
                    ICWRIF,NCWRIF,IREARW,ICOMCH,ICOMSW,   &
                    IUNFOF,IUNFNR,IUNFMC,NUMRCM,   &
                    IFCOLL,IFCOLU,   &
                    IANSLO,ILOOST,ILOOLI,IREPCH,   &
                    IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE RESET CASE  **
!               ****************************
!
      IF(ICOM.EQ.'CLEA' .AND. ICOM2.NE.'N   ' .AND.   &
         IHARG(1).NE.'CLIP')THEN
        ICOM='RESE'
        ICOM2='T   '
      ENDIF
!
      IF(ICOM.EQ.'RESE')THEN
        CALL DPRESE(IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE RESTORE           CASE  **
!               **  TREAT THE RESTORE VARIABLE  CASE  **
!               ****************************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REST')THEN
        IF(IHARG(1).EQ.'MEMO'.AND.IHARG2(1).EQ.'RY  ')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'ALL '.AND.IHARG2(1).EQ.'    ')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'VARI'.AND.IHARG2(1).EQ.'ABLE')THEN
          CALL DPREVA(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(ICOM.EQ.'REST')THEN
          CALL DPREST(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *****************************
!               **  TREAT THE RETAIN CASE  **
!               **  TREAT THE KEEP   CASE  **
!               **  TREAT THE PACK   CASE  **
!               *****************************
!
      IF(ICOM.EQ.'RETA' .OR. ICOM.EQ.'KEEP' .OR.   &
         ICOM.EQ.'PACK')THEN
        CALL DPRETA(IBUGS2,IBUGQ,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *********************************
!               **  TREAT THE ROW LIMITS CASE  **
!               *********************************
!
      IF(ICOM.EQ.'ROW')THEN
        CALL DPROWL(IHARG,IARGT,IARG,NUMARG,IDEFR1,IDEFR2,   &
                    IFROW1,IFROW2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  TREAT THE TERMINATOR CHARACTOR CASE **
!               **  TREAT THE SEPARATOR CHARACTOR CASE  **
!               ******************************************
!
      IF((ICOM.EQ.'TERM'.AND.ICOM2.EQ.'INAT') .OR.   &
         (ICOM.EQ.'SEPA'.AND.ICOM2.EQ.'RATO'))THEN
        CALL DPTECH(IHARG,NUMARG,   &
                    IDEFTC,ITERCH,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  TREAT THE CONTINUE   CHARACTOR CASE **
!               ******************************************
!
      IF(ICOM.EQ.'CONT' .AND. ICOM2.EQ.'INUE' .AND.   &
         IHARG(1).NE.'LOOP')THEN
        CALL DPCONC(IHARG,NUMARG,   &
                    IDEFCC,   &
                    ICONCH,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED JUNE 1989
!               ********************************************
!               **  TREAT THE REPLACEMENT  CHARACTOR CASE **
!               **  TREAT THE SUBSTITUTION CHARACTOR CASE **
!               ********************************************
!
      IF((ICOM.EQ.'REPL'.AND.ICOM2.EQ.'ACEM') .OR.   &
         (ICOM.EQ.'SUBS'.AND.ICOM2.EQ.'TITU'))THEN
        CALL DPRECH(IHARG,NUMARG,   &
                    IBASLC,IREPCH,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED SEPTEMBER 2005
!               **************************************************
!               **  TREAT THE MACRO SUBSTITUTION CHARACTOR CASE **
!               **************************************************
!
      IF(ICOM.EQ.'MACR'.AND.IHARG(1).EQ.'SUBS'.AND.   &
         IHARG(2).EQ.'CHAR')THEN
        CALL DPREMA(IHARG,NUMARG,   &
                    IMACSC,IDEFMS,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE SERIAL READ CASE  **
!               **********************************
!
      IF(ICOM.EQ.'SERI')THEN
!
!CCCC   MAY, 1990.  ADD ICOMCH, ICOMSW TO CALL LIST
!CCCC   MARCH, 1996.  ADD IMALEV TO CALL LIST
        INTINF=I1MACH(9)
        CALL DPSERI(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,   &
                    IMACRO,IMACNU,IMACCS,IOSW,IMALEV,   &
                    IREARW,ICOMCH,ICOMSW,   &
                    NUMRCM,   &
                    IFCOLL,IFCOLU,   &
                    IANSLO,ILOOST,ILOOLI,IREPCH,   &
                    IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE SINGLE PRECISION CASE  **
!               ***************************************
!
      IF(ICOM.EQ.'SING' .AND. IHARG(1).NE.'SAMP')THEN
        CALL DPSING(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                    IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE SKIP CASE  **
!               ***************************
!
      IF(ICOM.EQ.'SKIP')THEN
        CALL DPSKIP(IHARG,IARGT,IARG,NUMARG,IDEFSK,   &
                    ISKIP,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************
!               **  TREAT THE STATUS CASE  **
!               *****************************
!
!CCCC NOVEMBER 1997.  GUI STATUS (DON'T STORE IN SAVED COMMAND
!CCCC LIST)
!CCCC SEPTEMBER 2010. MAKE LS A SYNONYM FOR STATUS
!
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI ' .AND.   &
        (IHARG(1).EQ.'STAT' .OR. IHARG(1).EQ.'LS  '))THEN
        IF(NUMARG.GE.1.AND.   &
           (IHARG(1).EQ.'STAT' .OR. IHARG(1).EQ.'LS  '))THEN
             ISHIFT=1
             CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                         IBUGA2,IERROR)
             ICOM='STAT'
             ICOM2='US  '
             IFEEDB=IGUIFB
        ENDIF
      ENDIF
      IF(ICOM.EQ.'STAT' .OR. ICOM.EQ.'LS  ')THEN
        CALL DPSTAT(ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IFEEDB=IFEESV
!
!               ****************************
!               **  TREAT THE TIME  CASE  **
!               **  TREAT THE CLOCK CASE  **
!               ****************************
!
      IF(ICOM.EQ.'TIME' .OR. ICOM.EQ.'CLOC' .OR.   &
         ICOM.EQ.'DATE')THEN
        CALL DPTIME(CURRTI,NCURRT,CURRDA,NCURRD,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
           DO 5110 I=1,NCURRT
              IC4(1:4)='    '
              IC4(1:1)=CURRTI(I:I)
              IFUTMP(I)=IC4(1:4)
5110       CONTINUE
           CALL UPDATF('CURR','TIME',IFUTMP,NCURRT,'CHAD','NO  ',   &
                       IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                       NUMNAM,MAXNAM,IANS,IWIDTH,ILISTL,NEWNAM,MAXNAM,   &
                       IFUNC,NUMCHF,MAXCHF,IBUGS2,ILOCN,IFOUNN,IERRON)
           DO 5120 I=1,NCURRD
              IC4(1:4)='    '
              IC4(1:1)=CURRDA(I:I)
              IFUTMP(I)=IC4(1:4)
5120       CONTINUE
           CALL UPDATF('CURR','DATE',IFUTMP,NCURRD,'CHAD','NO  ',   &
                       IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                       NUMNAM,MAXNAM,IANS,IWIDTH,ILISTL,NEWNAM,MAXNAM,   &
                       IFUNC,NUMCHF,MAXCHF,IBUGS2,ILOCN,IFOUNN,IERRON)
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE CPU TIME  CASE  **
!               ********************************
!
      IF(ICOM.EQ.'CPU ')THEN
        CALL DPCPU(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                   ATIME,   &
                   IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IH='CPUT'
          IH2='IME '
          VALUE0=ATIME
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGS2,IERROR)
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE PROCESS-ID  CASE  **
!               **********************************
!
      IFOUND='NO'
      IF(ICOM.EQ.'PID ' .OR.   &
         (ICOM.EQ.'PROC' .AND. IHARG(1).EQ.'ID'))THEN
        CALL DPPID(IPID,IBUGS2,ISUBRO,IFOUND,IERROR)
                                                                                                                                  
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO'.AND.IPID.GT.0)THEN
          IH='PID '
          IH2='    '
          VALUE0=REAL(IPID)
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGS2,IERROR)
        ENDIF
      ENDIF
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ********************************
!               **  TREAT THE PWD       CASE  **
!               ********************************
!
      IF(ICOM.EQ.'PWD ' .OR.   &
        (ICOM.EQ.'GETC' .AND. ICOM2.EQ.'WD  ') .OR.   &
        (ICOM.EQ.'CURR' .AND. IHARG(1).EQ.'DIRE'))THEN
        MAXTMP=255
        CALL DPPWD(CURDIR,MAXTMP,ICNT,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')THEN
          IH='CURD'
          IH2='IR  '
          NEWNAM='YES'
          DO 5130 I=1,NUMNAM
            I2=I
            IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
              IF(IUSE(I2).EQ.'F')THEN
                NEWNAM='NO'
                GO TO 5139
              ELSE
                NEWNAM='NULL'
              ENDIF
            ENDIF
 5130     CONTINUE
 5139     CONTINUE
!
          IF(NEWNAM.NE.'NULL')THEN
            ILISTL=NUMNAM+1
            DO 5140 I=1,ICNT
              IFUNC9(I)=' '
              IFUNC9(I)(1:1)=CURDIR(I:I)
 5140       CONTINUE
            CALL DPINFU(IFUNC9,ICNT,IHNAME,IHNAM2,IUSE,IN,   &
                        IVSTAR,IVSTOP,   &
                        NUMNAM,IANS,IWIDTH,IH,IH2,ILISTL,   &
                        NEWNAM,MAXNAM,   &
                        IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
          ENDIF
        ENDIF
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD CLEAR  CASE  **
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'CLEA') .OR.   &
         (ICOM.EQ.'CLEA' .AND. IHARG(1).EQ.'CLIP'))THEN
        CALL DPCLI3(IBUGS2,ISUBRO,IERROR)
        IFOUND='YES'
        GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD LOOP   CASE  **
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'LOOP') .OR.   &
         (ICOM.EQ.'LOOP' .AND. IHARG(1).EQ.'CLIP'))THEN
        IF(NUMARG.EQ.1)THEN
          ICLILO='ON'
          ICLIL2=0
          IFOUND='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD LOOP END CASE**
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'LOOP' .AND.   &
          IHARG(2).EQ.'END') .OR.   &
         (ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'END ' .AND.   &
          IHARG(2).EQ.'LOOP') .OR.   &
         (ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'END ' .AND.   &
          IHARG(2).EQ.'OF  ' .AND. IHARG(3).EQ.'LOOP'))THEN
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
        GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD PAUSE  CASE  **
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'PAUS') .OR.   &
         (ICOM.EQ.'PAUS' .AND. IHARG(1).EQ.'CLIP'))THEN
        ICLIFL='PAUS'
        IF(ICLILO.EQ.'ON')ICLILO='PAUS'
        IFOUND='YES'
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,5150)
 5150     FORMAT('COMMANDS WILL NOW BE ENTERED FROM KEYBOARD')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD RESUME CASE  **
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'RESU') .OR.   &
         (ICOM.EQ.'RESU' .AND. IHARG(1).EQ.'CLIP'))THEN
        ICLIFL='ON'
        IF(ICLILO.EQ.'PAUS')ICLILO='ON'
        IFOUND='YES'
        GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE CLIPBOARD RUN    CASE  **
!               ***************************************
!
      IFOUND='NO'
      IF((ICOM.EQ.'CLIP' .AND. IHARG(1).EQ.'RUN ') .OR.   &
         (ICOM.EQ.'RUN ' .AND. IHARG(1).EQ.'CLIP') .OR.   &
           (ICOM.EQ.'CALL' .AND. IHARG(1).EQ.'CLIP') .OR.   &
            ICOM.EQ.'CB' .OR.   &
         (NUMARG.EQ.0 .AND. ICOM.EQ.'CLIP'))THEN
        ICLIFL='ON'
        ICLILN=0
        IFOUND='YES'
        GO TO 9000
      ENDIF
!
!               ***************************************
!               **  TREAT THE TRIPLE PRECISION CASE  **
!               ***************************************
!
      IF(ICOM.EQ.'TRIP')THEN
        CALL DPTRIP(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                    IPREC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE WEIGHTS CASE  **
!               ******************************
!
      IF(ICOM.EQ.'WEIG')THEN
        IF(IHARG(1).EQ.'DEMI' .AND.                                &
          (IHARG(2).EQ.'FIT ' .OR. IHARG(2).EQ.'REGR'))GO TO 5700
        IF(IHARG(1).EQ.'REPL' .AND. IHARG(2).EQ.'DEMI' .AND.       &
          (IHARG(3).EQ.'FIT ' .OR. IHARG(3).EQ.'REGR'))GO TO 5700
        CALL DPWEIG(IHARG,IHARG2,NUMARG,IDEFW1,IDEFW2,   &
                    IWEIG1,IWEIG2,IWEIGH,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
 5700 CONTINUE
!
!               ************************************************
!               **  TREAT THE ORTHOGONAL DISTANCE ERROR CASE  **
!               ************************************************
!
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'ERRO')   &
          THEN
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPORER(IHARG,IHARG2,NUMARG,   &
                      IODRE1,IODRE2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               ************************************************
!               **  TREAT THE ORTHOGONAL DISTANCE DELTA CASE  **
!               ************************************************
!
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'DELT')   &
          THEN
          ICASOD='DELT'
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPORDE(IHARG,IHARG2,NUMARG,   &
                      IODRD1,IODRD2,IODRD3,IODRD4,   &
                      IWEIN1,IWEIN2,   &
                      ICASOD,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICOM.EQ.'ORTH')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DIST'.AND.IHARG(2).EQ.'Y   ')   &
          THEN
          ICASOD='Y   '
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPORDE(IHARG,IHARG2,NUMARG,   &
                      IODRD1,IODRD2,IODRD3,IODRD4,   &
                      IWEIN1,IWEIN2,   &
                      ICASOD,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               **************************************
!               **  TREAT THE CLASS ... LOWER CASE  **
!               **  TREAT THE CLASS ... UPPER CASE  **
!               **  TREAT THE CLASS ... WIDTH CASE  **
!               **************************************
!
      IF(ICOM.EQ.'CLAS')THEN
        CALL DPCLLO(IHARG,IARGT,ARG,NUMARG,   &
                    CLLIMI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPCLUP(IHARG,IARGT,ARG,NUMARG,   &
                    CLLIMI,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
        CALL DPCLWI(IHARG,IARGT,ARG,NUMARG,   &
                    CLWIDT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE WRITE CASE  **
!               **  TREAT THE PRINT CASE  **
!               ****************************
!
!CCCC NOVEMBER 1997.  GUI PRINT/WRITE (DON'T STORE IN SAVED COMMAND
!CCCC LIST)
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI ')THEN
        IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PRIN'.OR.   &
           IHARG(1).EQ.'WRIT')THEN
             ISHIFT=1
             CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                         IBUGA2,IERROR)
             ICOM='WRIT'
             ICOM2='E   '
             IFEEDB=IGUIFB
        ENDIF
      ENDIF
      IF(ICOM.EQ.'WRIT')GO TO 5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T   ')GO TO 5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T1  ')GO TO 5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T2  ')GO TO 5800
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T3  ')GO TO 5800
      IF(ICOM.EQ.'HEAD'.AND.ICOM2.EQ.'    ')GO TO 5800
      IF(ICOM.EQ.'TAIL'.AND.ICOM2.EQ.'    '.AND.   &
         IHARG(1).NE.'AREA')GO TO 5800
      GO TO 5899
!
 5800 CONTINUE
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
!CCCC1IHARG2(1).EQ.'MALS')GO TO 5899
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
!CCCC1IHARG2(1).EQ.'MAL')GO TO 5899
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FORM'.AND.
!CCCC1IHARG2(1).EQ.'AT')GO TO 5899
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REWI'.AND.
!CCCC1IHARG2(1).EQ.'ND')GO TO 5899
!
      CALL DPWRIT(IMACRO,IMACNU,IMACCS,   &
                  IFORSW,ICWRIF,NCWRIF,   &
                  IWRIRW,   &
                  IFORWI,IFORWR,MAXNWI,   &
                  IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
!
      IFEEDB=IFEESV
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 5899 CONTINUE
!
!               ******************************
!               **  TREAT THE COMMENT CASE  **
!               **  TREAT THE    .    CASE  **
!               ******************************
!
!  MAY, 1990.  SOFT-CODE THE COMMENT CHARACTER.  ALSO, A COMMENT
!  CHARACTER AND A COMMENT CHECK COMMAND WERE ADDED.  ALWAYS TREAT
!  PERIOD AS COMMENT ON COMMAND LINE.
!
      IF(ICOM.EQ.'.' .OR. ICOM.EQ.ICOMCH .OR. ICOM.EQ.'COMM')THEN
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'CHAR')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'CHEC')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'COEF')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'PLOT')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'DIFF' .AND.       &
           IHARG(2).EQ.'PLOT')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(2).EQ.'COEF')GO TO 5999
        IF(ICOM.EQ.'COMM'.AND. IHARG(1).EQ.'WEIB' .AND.       &
          IHARG(2).EQ.'SHAP')GO TO 5999
        CALL DPDOT(IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
 5999 CONTINUE
!
!               *******************************
!               **  TREAT THE FEEDBACK CASE  **
!               *******************************
!
      IF(ICOM.EQ.'FEED')THEN
        CALL DPFEED(IHARG,NUMARG,   &
                    IFEED2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************
!               **  TREAT THE FILTER WIDTH CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'FILT')THEN
        CALL DPFIWI(IHARG,IARGT,ARG,NUMARG,DEFFW,   &
                    FILWID,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE DEFAULT COMMAND CASE  **
!               **************************************
!
      IF(ICOM.EQ.'DEFA')THEN
        CALL DPDECO(IANS,IWIDTH,IHARG,NUMARG,   &
                    IDEFCM,IWIDDC,IDEFC,IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE BUGS CASE  **
!               ***************************
!
      IF(ICOM.EQ.'BUGS' .OR. ICOM.EQ.'BUG ')THEN
        CALL DPBUGS(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE MAIL CASE  **
!               ***************************
!
!CCCC IF(ICOM.EQ.'MAIL')GO TO 6700
!CCCC GO TO 6799
!
!6700 CONTINUE
!CCCC CALL DPMAIL(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6799 CONTINUE
!
!               ***************************
!               **  TREAT THE NEWS CASE  **
!               ***************************
!
      IF(ICOM.EQ.'NEWS')THEN
        CALL DPNEWS(IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE QUERY CASE  **
!               ****************************
!
!CCCC IF(ICOM.EQ.'QUER')GO TO 6900
!CCCC IF(ICOM.EQ.'QUES')GO TO 6900
!CCCC IF(ICOM.EQ.'MESS')GO TO 6900
!CCCC GO TO 6999
!
!6900 CONTINUE
!CCCC CALL DPQUER(IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!6999 CONTINUE
!
!               ****************************
!               **  TREAT THE SET   CASE  **
!               ****************************
!
      IF(ICOM.EQ.'SET ')GO TO 7110
!
!CCCC IF(ICOM.EQ.'READ')GO TO 7105
!CCCC IF(ICOM.EQ.'WRIT')GO TO 7105
!CCCC IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'T   ')GO TO 7105
!CCCC GO TO 7199
!
!7105 CONTINUE
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
!CCCC1IHARG2(1).EQ.'MALS')GO TO 7110
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI'.AND.
!CCCC1IHARG2(1).EQ.'MAL')GO TO 7110
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FORM'.AND.
!CCCC1IHARG2(1).EQ.'AT')GO TO 7110
!CCCC IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REWI'.AND.
!CCCC1IHARG2(1).EQ.'ND')GO TO 7110
      GO TO 7199
!
 7110 CONTINUE
      CALL DPSET(ILISMX,IREPCH,IOSW,   &
      IPPDE1,IPPDE2,   &
      IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
      IBUGEX,IBUGE2,IBUGHE,IBUGH2,IBUGLO,   &
      IHELMX,IFTEXP,IFTORD,   &
      IFORSW,ICREAF,NCREAF,ICWRIF,NCWRIF,   &
      IREARW,IWRIRW,   &
      NPLOTP,   &
      IPRITY,   &
      IUNFOF,IUNFNR,IUNFMC,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 7199 CONTINUE
!
!               ********************************
!               **  TREAT THE IMPLEMENT CASE  **
!               ********************************
!
      IF(ICOM.EQ.'IMPL')THEN
        CALL DPIMPL(IHARG,IARGT,IARG,NUMARG,   &
                    IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,   &
                    ISQUAR,   &
                    PXMIN,PYMIN,PXMAX,PYMAX,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************************
!               **  TREAT THE REWIND CASE  **
!               *****************************
!
!CCCC IF(ICOM.EQ.'REWI')GO TO 7300
!CCCC GO TO 7399
!CCCC
!7300 CONTINUE
!CCCC CALL DPREWI(IBUGS2,IBUGQ,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!7399 CONTINUE
!
!               ******************************
!               **  TREAT THE ENDFILE CASE  **
!               ******************************
!
!CCCC IF(ICOM.EQ.'ENDF')GO TO 7400
!CCCC GO TO 7499
!CCCC
!7400 CONTINUE
!CCCC CALL DPENDF(IBUGS2,IBUGQ,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!7499 CONTINUE
!
!               *****************************
!               **  TREAT THE RELEASE CASE **
!               *****************************
!
!CCCC IF(ICOM.EQ.'RELE')GO TO 7500
!CCCC IF(ICOM.EQ.'CLOS')GO TO 7500
!CCCC IF(ICOM.EQ.'FREE')GO TO 7500
!CCCC GO TO 7599
!CCCC
!7500 CONTINUE
!CCCC CALL DPREWI(IBUGS2,IBUGQ,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!7599 CONTINUE
!
!               ***************************
!               **  TREAT THE SEED CASE  **
!               ***************************
!
      IF(ICOM.EQ.'SEED')THEN
        CALL DPSEED(IHARG,IARGT,IARG,NUMARG,IDEFSE,   &
                    ISEED,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE THE PROPORTION LIMITS   CASE  **
!               **  = THE ANOP LIMITS     CASE  **
!               **************************************
!
      IF(ICOM.EQ.'PROP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'LIMI'.AND.IHARG2(1).EQ.'TS  ')GO TO 8100
      IF(ICOM.EQ.'PROP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'REGI'.AND.IHARG2(1).EQ.'ON  ')GO TO 8100
      IF(ICOM.EQ.'ANOP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'LIMI'.AND.IHARG2(1).EQ.'TS  ')GO TO 8100
      IF(ICOM.EQ.'ANOP'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'REGI'.AND.IHARG2(1).EQ.'ON  ')GO TO 8100
      GO TO 8199
!
 8100 CONTINUE
      CALL DPANOL(IHARG,IARGT,ARG,NUMARG,DEFAL1,DEFAL2,   &
      ANOPL1,ANOPL2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
 8199 CONTINUE
!
!               ****************************
!               **  TREAT THE FENCE CASE  **
!               ****************************
!
      IF(ICOM.EQ.'FENC')THEN
        CALL DPFENC(IHARG,NUMARG,   &
                    IFENSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE PAUSE CASE  **
!               ****************************
!
      IF(ICOM.EQ.'PAUS' .AND. NUMARG.EQ.0)THEN
        CALL DPPAUS(IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE SLEEP CASE  **
!               ****************************
!
      IF(ICOM.EQ.'SLEE')THEN
        CALL DPSLEE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                    IBUGD2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE APPEND  CASE  **
!               ******************************
!
      IF(ICOM.EQ.'APPE' .OR. ICOM.EQ.'AUGM')THEN
        CALL DPAPPE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************
!               **  TREAT THE EXTEND  CASE  **
!               ******************************
!
      IF(ICOM.EQ.'EXTE')THEN
        CALL DPEXTE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE SUGGESTION  CASE      **
!               **  TREAT THE RECOMMENDATION  CASE  **
!               **  TREAT THE PROGRAM  CASE         **
!               **  TREAT THE CODE  CASE            **
!               **  TREAT THE EXPERT  CASE          **
!               **************************************
!
!CCCC IF(ICOM.EQ.'SUGG')GO TO 8600
!CCCC IF(ICOM.EQ.'RECO')GO TO 8600
!CCCC IF(ICOM.EQ.'PROG')GO TO 8600
!CCCC IF(ICOM.EQ.'CODE')GO TO 8600
!CCCC IF(ICOM.EQ.'EXPE')GO TO 8600
!CCCC GO TO 8699
!CCCC
!8600 CONTINUE
!CCCC CALL DPSUPR(IHARG,IHARG2,NUMARG,
!CCCC1ITOPIC,
!CCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!8699 CONTINUE
!CCCC
!CCCC           **************************************
!CCCC           **  TREAT THE GO          CASE      **
!CCCC           **************************************
!CCCC
!CCCC IF(ICOM.EQ.'GO')GO TO 8700
!CCCC GO TO 8799
!CCCC
!8700 CONTINUE
!CCCC CALL DPWRPF(IPRONU,IPROFS,IPROST,
!CCCC1ITOPIC,
!CCCC1IHARG,IHARG2,NUMARG,
!CCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!8799 CONTINUE
!CCCC
!CCCC           **************************************
!CCCC           **  TREAT THE CONCLUSIONS CASE      **
!CCCC           **************************************
!CCCC
!CCCC IF(ICOM.EQ.'CONC')GO TO 8800
!CCCC GO TO 8899
!CCCC
!8800 CONTINUE
!CCCC CALL DPLICF(ICONNU,ICONFS,ICONST,
!CCCC1IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!8899 CONTINUE
!CCCC
!CCCC
!               **************************************
!               **  TREAT THE ROOT ACCURACY  CASE   **
!               **************************************
!
      IF(ICOM.EQ.'ROOT'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'ACCU')THEN
        CALL DPROAC(IHARG,IARGT,ARG,NUMARG,DEFRAC,   &
                    ROOTAC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC           ***************************
!CCCC           **  TREAT THE MENU CASE  **
!CCCC           ***************************
!CCCC
!CCCC IF(ICOM.EQ.'MENU')GO TO 9100
!CCCC GO TO 9199
!CCCC
!9100 CONTINUE
!CCCC CALL DPMENU(IMENNU,IMENFS,IMENST,
!CCCC1IHARG,NUMARG,IANS,IWIDTH,IBUGS2,IFOUND,IERROR)
!CCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!CCCC
!9199 CONTINUE
!CCCC
!               *****************************
!               **  TREAT THE PROMPT CASE  **
!               *****************************
!
      IF(ICOM.EQ.'PROM')THEN
        CALL DPPROM(IHARG,NUMARG,IPROSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************
!               **  TREAT THE LIST (COMMANDS) CASE  **
!               **  (SAME AS THE RECALL CASE)       **
!               **************************************
!
!CCCC THE FOLLOWING PARAGRAPH WAS REWRITTEN    SEPTEMBER 1993
!
      IF(NUMARG.LE.0)THEN
        IF(ICOM.EQ.'LIST' .OR. ICOM.EQ.'TYPE' .OR.   &
           ICOM.EQ.'L'    .OR. ICOM.EQ.'RECA' .OR.   &
           ICOM.EQ.'V'    .OR. ICOM.EQ.'PREV' .OR.   &
          (ICOM.EQ.'VIEW' .AND. IHARG(1).NE.'PLOT'))THEN
          CALL DPLICO(IHARG,NUMARG,IANSSV,IREPMX,ILISMX,IPOINT,   &
                      IHELMX,   &
                      ICPREH,NCPREH,ICPOSH,NCPOSH,   &
                      IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               ****************************
!               **  TREAT THE LIST  CASE  **
!               ****************************
!
!CCCC APRIL 1997: CHECK FOR CONFLICT WITH LIST GRAPH, LIST PLOT,
!CCCC             VIEW PLOTS, AND VIEW GRAPHS.
!
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.   &
      IHARG2(1).EQ.'    ')GO TO 9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.   &
      IHARG2(1).EQ.'S  ')GO TO 9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.   &
      IHARG2(1).EQ.'H  ')GO TO 9499
      IF(ICOM.EQ.'VIEW'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.   &
      IHARG2(1).EQ.'HS ')GO TO 9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.   &
      IHARG2(1).EQ.'    ')GO TO 9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT'.AND.   &
      IHARG2(1).EQ.'S  ')GO TO 9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.   &
      IHARG2(1).EQ.'H  ')GO TO 9499
      IF(ICOM.EQ.'LIST'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'GRAP'.AND.   &
      IHARG2(1).EQ.'HS ')GO TO 9499
!
      IF((ICOM.EQ.'LIST' .OR. ICOM.EQ.'L   ' .OR. ICOM.EQ.'VIEW' .OR.   &
         ICOM.EQ.'PREV' .OR. ICOM.EQ.'NLIS' .OR. ICOM.EQ.'NTYP' .OR.   &
         ICOM.EQ.'NVIE' .OR. ICOM.EQ.'NPRE') .AND.   &
         IHARG(1).NE.'=   ')THEN
!CCCC    2 LINES OF ARGS (IHELMX THROUGH NCPOSH) WERE ADDED JULY 1989
!CCCC    THE FOLLOWING LINE WAS CHANGED NOVEMBER 1989
!CCCC   CALL DPLIST(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,IARGT,
!CCCC1              IARG,ARG,NUMARG,
        CALL DPLIST(ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,   &
                    ILISMX,   &
                    ICPREH,NCPREH,ICPOSH,NCPOSH,   &
                    ILOOST,ILOOLI,NUMLIL,NUMLOS,   &
                    IANSLO,IWIDLL,   &
                    IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
 9499 CONTINUE
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               **********************************
!               **  TREAT THE SAVE PLOT   CASE  **
!               **********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GO TO 9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GO TO 9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GO TO 9500
      IF(NUMARG.GE.1.AND.ICOM.EQ.'SAVE'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GO TO 9500
      IF(ICOM.EQ.'SG  ')GO TO 9500
      IF(ICOM.EQ.'SP  ')GO TO 9500
      GO TO 9509
!
 9500 CONTINUE
      CALL DPSAPL(IANSLC,IWIDTH,IHARG,NUMARG,   &
                  IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      GO TO 9509
!
!
!               ************************************
!               **  TREAT THE SAVE MEMORY CASE    **
!               **  TREAT THE SAVE VARIABLE CASE  **
!               ************************************
!
 9509 CONTINUE
      IF(NUMARG.GE.1 .AND. ICOM.EQ.'SAVE')THEN
        IF((IHARG(1).EQ.'MEMO' .AND. IHARG2(1).EQ.'RY  ') .OR.   &
           (IHARG(1).EQ.'ALL ' .AND. IHARG2(1).EQ.'    '))THEN
          CALL DPSAVE(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'VARI' .AND. IHARG2(1).EQ.'ABLE')THEN
          CALL DPSAVA(IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               **************************************
!               **  TREAT THE GUI SAVE PLOT CONTROL **
!               **************************************
!
      IFEESV=IFEEDB
      IF(ICOM.EQ.'GUI')THEN
        IFEEDB=IGUIFB
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SAVE'.AND.   &
           IHARG(2).EQ.'PLOT'.AND.IHARG(3).EQ.'CONT')THEN
           CALL DPSAPC(IBUGS2,ISUBRO,IFOUND,IERROR)
           IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
      IFEEDB=IFEESV
!
!               **************************************
!               **  TREAT THE SAVE (COMMANDS) CASE  **
!               **************************************
!
!CCCC DECEMBER 1993.  CHECK FOR CONFLICT WITH S CHART COMMAND
!
      IF(ICOM.EQ.'SAVE' .OR. ICOM.EQ.'S   ' .AND.   &
         IHARG(1).NE.'CONT' .AND. IHARG(1).NE.'CHAR' .AND.   &
         IHARG(1).NE.'=   ')THEN
        CALL DPSACO(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,   &
                    IANSSV,IREPMX,IPOINT,ISACNC,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE AUTOPLOT (SWITCH) CASE  **
!               ****************************************
!
      IF(ICOM.EQ.'AUTO'.AND.ICOM2.EQ.'PLOT')THEN
        CALL DPAUPL(IHARG,NUMARG,   &
                    IAUTSW,IAUTEX,IFOUND,IERROR)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **********************************
!               **  TREAT THE CURSOR SIZE CASE  **
!               **********************************
!
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SIZE')GO TO 10100
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'HEIG')GO TO 10100
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SIZE')GO TO 10100
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'HEIG')GO TO 10100
      GO TO 10199
!
10100 CONTINUE
      CALL DPCUSZ(IHARG,IARGT,ARG,NUMARG,DEFCSZ,   &
      ACURSZ,IFOUND,IERROR)
      PDIAHE=ACURSZ
      PDIAWI=PDIAHE/2.0
      IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')GO TO 10110
      GO TO 10119
!
10110 CONTINUE
!CCCC ICOPSW='OFF'
!CCCC NUMCOP=0
!CCCC CALL DPCLPL(ICOPSW,NUMCOP,
!CCCC1PGRAXF,PGRAYF,
!CCCC1IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
!CCCC1PDIAHE,PDIAWI,PDIAVG,PDIAHG)
!CCCC CALL DPCLDE
!0119 CONTINUE
      IF(NUMDEV.LE.0)GO TO 10119
      DO 10112 IDEVIC=1,NUMDEV
      IF(IDPOWE(IDEVIC).EQ.'OFF')GO TO 10112
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
!
      CALL DPOPDE
      IBELSJ='OFF'
      NUMRIJ=0
      IERASJ='OFF'
      IBACCJ=IBACCO
      CALL DPOPPL(IGRASW,IBELSJ,NUMRIJ,IERASJ,IBACCJ,IBACC2)
      ICOPSJ='OFF'
      NUMCOJ=0
      CALL DPCLPL(ICOPSJ,NUMCOJ,   &
                  PGRAXF,PGRAYF,   &
                  IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                  PDIAHE,PDIAWI,PDIAVG,PDIAHG)
      CALL DPCLDE
10112 CONTINUE
10119 CONTINUE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
10199 CONTINUE
!
!               *************************************
!               **  TREAT THE CURSOR SPACING CASE  **
!               *************************************
!
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SPAC')GO TO 10200
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'GAP')GO TO 10200
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SPAC')GO TO 10200
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'GAP')GO TO 10200
      GO TO 10299
!
10200 CONTINUE
      DEFCSP=0.0
      CALL DPCUSP(IHARG,IARGT,ARG,NUMARG,DEFCSP,   &
      PDIAVG,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
10299 CONTINUE
!
!               *****************************************
!               **  TREAT THE CURSOR COORDINATES CASE  **
!               *****************************************
!
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'COOR')GO TO 10300
      IF(ICOM.EQ.'CURS'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'LOCA')GO TO 10300
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'COOR')GO TO 10300
      IF(ICOM.EQ.'DIAL'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'LOCA')GO TO 10300
      GO TO 10399
!
10300 CONTINUE
      CALL DPCUCO(IHARG,IARGT,ARG,NUMARG,PDIAYC,   &
      PDIAY2,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.AND.IERROR.EQ.'NO')GO TO 10310
      GO TO 10319
!
10310 CONTINUE
      IF(NUMDEV.LE.0)GO TO 10319
      DO 10312 IDEVIC=1,NUMDEV
      IF(IDPOWE(IDEVIC).EQ.'OFF')GO TO 10312
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
!
      CALL DPOPDE
      IBELSJ='OFF'
      NUMRIJ=0
      IERASJ='OFF'
      IBACCJ='JUNK'
      CALL DPOPPL(IGRASW,IBELSJ,NUMRIJ,IERASJ,IBACCJ,IBACC2)
      ICOPSJ='OFF'
      NUMCOJ=0
      CALL DPCLPL(ICOPSJ,NUMCOJ,   &
                  PGRAXF,PGRAYF,   &
                  IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                  PDIAHE,PDIAWI,PDIAVG,PDIAHG)
      CALL DPCLDE
10312 CONTINUE
10319 CONTINUE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
10399 CONTINUE
!
!               **************************************
!               **  TREAT THE PREPOST  DEVICE CASE  **
!               **************************************
!
      IF(ICOM.EQ.'PREP'.AND.ICOM2.EQ.'OST')THEN
        CALL DPPRPO(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IPPDE1,IPPDE2,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************
!               **  TREAT THE SEARCH CASE **
!               ****************************
!
      IF(ICOM.EQ.'SEAR' .OR. ICOM.EQ.'?   ' .OR.   &
         ICOM.EQ.'??? ' .OR. ICOM.EQ.'GREP' .OR.   &
        (ICOM.EQ.'FIND' .AND. ICOM2.EQ.'STR '))THEN
        ISEART='1LIN'
        IF(ICOM2.EQ.'CHB')ISEART='BLAN'
        IF(ICOM2.EQ.'CHBL')ISEART='BLAN'
        IF(ICOM2.EQ.'CHD')ISEART='----'
        IF(ICOM2.EQ.'CHDA')ISEART='----'
        IF(ICOM.EQ.'GREP')ISEART='GREP'
        IF(ICOM.EQ.'FIND' .AND. ICOM2.EQ.'STR ')ISEART='FIND'
!CCCC   THE FOLLOWING LINE WAS ADDED      JANUARY 1994
        IF(ICOM2.EQ.'CH1 ')ISEART='FIRS'
        CALL DPSEAR(IANS,IANSLC,IWIDTH,ICOM,IHARG,IHARG2,NUMARG,ISEART,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE LOWESS FRACTION  CASE   **
!               ****************************************
!
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'FRAC')GO TO 10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'DECI')GO TO 10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'PROP')GO TO 10600
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'PERC')GO TO 10600
      GO TO 10699
!
10600 CONTINUE
!
      CALL DPLOFR(IHARG,IARGT,ARG,NUMARG,   &
      ALOWFR,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
10699 CONTINUE
!
!               *********************************************
!               **  TREAT THE KERNEL DENSITY WIDTH  CASE   **
!               *********************************************
!
      IF(ICOM.EQ.'KERN')THEN
        IF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'WIDT')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPKDWI(IHARG,IARGT,ARG,NUMARG,   &
                      PKDEWI,DEFKWI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'WIDT')THEN
          CALL DPKDWI(IHARG,IARGT,ARG,NUMARG,   &
                      PKDEWI,DEFKWI,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
        IF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'POIN')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,   &
                      IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'POIN')THEN
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,   &
                      IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'NUMB'.AND.   &
               IHARG(3).EQ.'OF  '.AND.IHARG(4).EQ.'POIN')THEN
          ISHIFT=3
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,   &
                      IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSEIF(IHARG(1).EQ.'DENS'.AND.IHARG(2).EQ.'NUMB'.AND.   &
               IHARG(3).EQ.'POIN')THEN
          ISHIFT=2
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGA2,IERROR)
          CALL DPKDNP(IHARG,IARGT,ARG,NUMARG,   &
                      IKDENP,IDEFKN,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *********************************************
!               **  TREAT THE BOOSTRAP SAMPLE SIZE  CASE   **
!               *********************************************
!
      IF(ICOM.EQ.'BOOT'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SAMP')GO TO 10700
      IF(ICOM.EQ.'BOOT'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'SIZE')GO TO 10700
      GO TO 10799
!
10700 CONTINUE
!
      CALL DPBOSS(IHARG,IARGT,IARG,NUMARG,   &
      IBOOSS,IDEBOO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
10799 CONTINUE
!
!               ***************************
!               **  TREAT THE SYSTEM CASE**
!               ***************************
!
      IF(ICOM.EQ.'SYST' .OR. ICOM.EQ.'DOS' .OR.   &
         ICOM.EQ.'UNIX' .OR. ICOM.EQ.'VMS' .OR.   &
         ICOM.EQ.'OS')THEN
        CALL DPSYST(IANS,IANSLC,IWIDTH,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                    IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  TREAT THE RSCRIPT/PYTHON CASE  **
!               *************************************
!
      IF(ICOM.EQ.'RSCR' .OR. ICOM.EQ.'PYTH')THEN
        CALL DPEXRP(IANS,IANSLC,IWIDTH,ICOM,ICOM2,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED JUNE 1989
!               *************************************
!               **  TREAT THE CAPTURE CASE         **
!               **  TREAT THE END CAPTURE CASE     **
!               **  TREAT THE END OF CAPTURE CASE  **
!               **  TREAT THE REDIRECT CASE        **
!               **  TREAT THE END REDIRECT CASE    **
!               **  TREAT THE END OF REDIRECT CASE **
!               *************************************
!
      IF(ICOM.EQ.'CAPT')GO TO 11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'CAPT')GO TO 11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'OF  '.AND.   &
      IHARG(2).EQ.'CAPT')GO TO 11100
      IF(ICOM.EQ.'REDI')GO TO 11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'REDI')GO TO 11100
      IF(ICOM.EQ.'END '.AND.NUMARG.GE.2.AND.IHARG(1).EQ.'OF  '.AND.   &
      IHARG(2).EQ.'REDI')GO TO 11100
      GO TO 11199
!
11100 CONTINUE
      CALL DPCAPT(ICOM,ICOM2,   &
      ICAPSW,ICAPTY,ICAPSC,IPRDEF,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,IANSLC,IANS,IWIDTH,   &
      IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
      IOFILE,   &
      IBACCO,IBACC2,IGRASW,IDIASW,   &
      PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
      PDIAHE,PDIAWI,PDIAVG,PDIAHG,   &
      NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
      IDNVOF,IDNHOF,IDFONT,PDSCAL,   &
      IREPCH,IMPSW,   &
      IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
11199 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
!               **************************************************
!               **  TREAT THE YATES COEF/T/RESSD CUTOFF CASE    **
!               **************************************************
!
      IF(ICOM.EQ.'YATE')GO TO 11210
      GO TO 11299
!
11210 CONTINUE
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CUTO'.AND.   &
      IHARG2(2).EQ.'FF')GO TO 11220
      GO TO 11299
11220 CONTINUE
      CALL DPYACU(IHARG,IARGT,ARG,NUMARG,   &
      YATCCU,YATTCU,YATRCU,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
11299 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
!               **************************************************
!               **  TREAT THE YATES OUTPUT CASE                 **
!               **************************************************
!
      IF(ICOM.EQ.'YATE')GO TO 11310
      GO TO 11399
!
11310 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OUTP'.AND.   &
      IHARG2(1).EQ.'UT')GO TO 11320
      GO TO 11399
11320 CONTINUE
      CALL DPYAOU(IHARG,NUMARG,   &
      IYATOS,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
11399 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS INSERTED NOVERMBER 1989
!               **************************************************
!               **  TREAT THE COLUMN RULER CASE                 **
!               **  TREAT THE        RULER CASE                 **
!               **************************************************
!
      IF(ICOM.EQ.'COLU'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'RULE')GO TO 11410
      IF(ICOM.EQ.'RULE')GO TO 11410
      IF(ICOM.EQ.'COLU'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'NRUL')GO TO 11410
      IF(ICOM.EQ.'NRUL')GO TO 11410
      GO TO 11499
!
11410 CONTINUE
      CALL DPCORU(ICOM,IHARG,NUMARG,   &
      IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
11499 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   MAY 1990
!               ******************************************
!               **  TREAT THE COMMENT    CHARACTER CASE **
!               ******************************************
!
      IF(ICOM.EQ.'COMM')THEN
        CALL DPCOMM(IHARG,NUMARG,   &
                    IDEFCZ,   &
                    ICOMCH,   &
                    ICOMSW,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED   MARCH 1992
!               ******************************************
!               **  TREAT THE PRINTER TYPE/FORMAT CASE  **
!               ******************************************
!
      IF(ICOM.EQ.'PRIN'.AND.ICOM2.EQ.'TER ')GO TO 11610
      IF(ICOM.EQ.'LP  ' .AND. IHARG(1).NE.'LOCA')GO TO 11610
      GO TO 11699
!
11610 CONTINUE
!CCCC CALL DPPRFO(IHARG,NUMARG,IPRITY,IBUGS2,IERROR)
      CALL DPPRFO(IHARG,NUMARG,IPRITY,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
11699 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED   MARCH 1992
!               ******************************************
!               **  TREAT THE FILE TYPE/FORMAT CASE     **
!               ******************************************
!
      IF(ICOM.EQ.'FILE')THEN
        CALL DPFIFO(IHARG,NUMARG,IOUTTY,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   AUGUST 1992
!               ******************************************
!               **  TREAT THE VECTOR FORMAT        CASE **
!               ******************************************
!
      IF(ICOM.EQ.'VECT'.AND.IHARG(1).EQ.'FORM')THEN
        CALL DPVCFM(IHARG,NUMARG,   &
                    IDEFVF,   &
                    IVCFMT,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   AUGUST 1992
!               ******************************************
!               **  TREAT THE VECTOR ARROW         CASE **
!               ******************************************
!
      IF(ICOM.EQ.'VECT'.AND.IHARG(1).EQ.'ARRO')THEN
        CALL DPVCAR(IHARG,NUMARG,   &
                    IDEFVA,IDEFVO,   &
                    IVCARR,IVCOPN,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   NOVEMBER 1992
!               ******************************************
!               **  TREAT THE ANDREWS INCREMENT    CASE **
!               ******************************************
!
      IF(ICOM.EQ.'ANDR'.AND.IHARG(1).EQ.'INCR')THEN
        CALL DPANIN(IHARG,IARGT,ARG,NUMARG,DEFAIN,   &
                    ANDINC,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
!               ******************************************
!               **  TREAT THE FRACTAL ITERATIONS   CASE **
!               ******************************************
!
      IF(ICOM.EQ.'FRAC'.AND.IHARG(1).EQ.'ITER')THEN
        CALL DPFRIT(IHARG,IARGT,ARG,NUMARG,MAXPOP,   &
                    IFRAIT,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
!               ******************************************
!               **  TREAT THE FRACTAL TYPE         CASE **
!               ******************************************
!
      IF(ICOM.EQ.'FRAC'.AND.IHARG(1).EQ.'TYPE')THEN
        CALL DPFRTY(IHARG,NUMARG,   &
                    IDEFFT,   &
                    IFRATY,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   JULY 1993
!               **********************************************
!               **  TREAT THE PRINCIPLE COMPONENT TYPE CASE **
!               **********************************************
!
      IF(ICOM.EQ.'PRIN'.AND.IHARG(1).EQ.'COMP'.AND.   &
         IHARG(2).EQ.'TYPE')THEN
        CALL DPPCTY(IHARG,NUMARG,   &
                    IDEFPT,   &
                    IPCMTY,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ****************************************
!               **  TREAT THE LOWESS DEGREE  CASE     **
!               ****************************************
!
      IF(ICOM.EQ.'LOWE'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'DEGR')THEN
        CALL DPLODG(IHARG,IARGT,ARG,NUMARG,   &
                    ALOWDG,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC FOLLOWING SECTION ADDED JUNE 1994.
!               ***********************************************
!               **  TREAT THE OPTIMIZATION TOLERANCE  CASE   **
!               ***********************************************
!
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'TOLE')GO TO 12600
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.   &
      IHARG(1).EQ.'ACCU')GO TO 12600
      GO TO 12699
!
12600 CONTINUE
!
      CALL DPOPAC(IHARG,IARGT,ARG,NUMARG,DEFOAC,   &
      OPTACC,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
12699 CONTINUE
!
!CCCC THE FOLLOWING SECTION  WAS ADDED   MAY 1994  (JJF)
!               *****************************************
!               **  TREAT THE COPY (= COPY FILE) CASE  **
!               *****************************************
!
      IF(NUMARG.GE.1)THEN
         IF(ICOM.EQ.'COPY')THEN
            CALL DPCOFI(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                        IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
            IFOUND='YES'
            GO TO 9000
         ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION  WAS ADDED   MARCH 2019
!               *****************************************
!               **  TREAT THE PRINTFILE          CASE  **
!               *****************************************
!
      IF(NUMARG.GE.1)THEN
         IF(ICOM.EQ.'PRIN' .AND. ICOM2.EQ.'TFIL')THEN
            CALL DPPRFI(ICOM,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                        IBUGS2,ISUBRO,IFOUND,IERROR)
            IFOUND='YES'
            GO TO 9000
         ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY ALAN   FEBRUARY 1995
!               **********************************************
!               **  TREAT THE OPTIMIZATION METHOD      CASE **
!               **********************************************
!
      IF(ICOM.EQ.'OPTI'.AND.NUMARG.GE.1.AND.   &
         IHARG(1).EQ.'METH')THEN
        CALL DPOPME(IHARG,NUMARG,   &
                    IDEFOM,IDEFHS,   &
                    IOPTME,IOPTHE,   &
                    IBUGS2,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED BY JIM   SEPTEMBER 1995
!               **********************************************
!               **  TREAT THE   INIT   CASE                 **
!               **  (USEFUL FOR SIGN-ON DEBUGGING)          **
!               **********************************************
!
      IF(ICOM.EQ.'INIT')THEN
         IBUGIN='ON'
!
         ICOMHO=ICOM
         ICOMH2=ICOM2
!
         WRITE(ICOUT,10811)
10811    FORMAT('FROM MAINSU--BEFORE CALL TO MAININ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,10812)IBUGMA,IBUGIN,ICOM,ICOM2,ICOMHO,ICOMH2,NUMDEV
10812    FORMAT('IBUGMA,IBUGIN,ICOM,ICOM2,ICOMHO,ICOMH2,NUMDEV = ',   &
         A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,I8)
         CALL DPWRST('XXX','BUG ')
!
         IRSCNT=1
         CALL MAININ(IBUGIN,ICOMHO,ICOMH2,IRSCNT)
         IBUGIN='OFF'
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               *******************************
!               **  TREAT THE WEB HELP CASE  **
!               *******************************
!
 1201 CONTINUE
!
      IF((ICOM.EQ.'WEB'.AND.IHARG(1).EQ.'HELP') .OR.   &
          ICOM.EQ.'??  ')THEN
        CALL DPHELW(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IANS,IWIDTH,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC FOLLOWING SECTION ADDED MARCH 1999.
!               ***********************************
!               **  TREAT THE WEB HANDBOOK CASE  **
!               ***********************************
!
      IF(ICOM.EQ.'HAND' .OR. ICOM.EQ.'HB  ' .OR.   &
         ICOM.EQ.'WHB ' .OR.   &
        (ICOM.EQ.'????' .AND. ICOM2.EQ.'    ') .OR.   &
         (ICOM.EQ.'WEB'.AND.IHARG(1).EQ.'HAND'))THEN
        CALL DPHANW(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,IANS,   &
                    IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               *******************************
!               **  TREAT THE WEB      CASE  **
!               **  NOTE: SET "HANDBOOK" =   **
!               **        "WEB HANDBOOK      **
!               *******************************
!
!CCCC 2019/11: IF THE FIRST ARGUMENT DOES NOT START WITH ONE OF THE
!              FOLLOWING
!
!                  http
!                  file:
!                  www.
!                  A PERIOD "." WITHIN THE FIRST 8 CHARACTERS, THEN
!
!              THEN ASSUME THIS IS NOT A URL ADDRESS AND CONVERT THE
!              "WEB" COMMAND TO A "WEB HELP" COMMAND.
!
      IF(ICOM.EQ.'WEB'  .OR. ICOM.EQ.'W   ' .OR.   &
         ICOM.EQ.'WS  ' .OR.   &
        (ICOM.EQ.'????' .AND. ICOM2.EQ.'?   '))THEN
!
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
!
        IF(IFLAG.EQ.1)THEN
          CALL DPWEB(ICOM,ICOM2,IHARG,IHARG2,NUMARG,   &
                     IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ELSE
          ISHIFT=1
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGS2,IERROR)
          ISTRT=-1
          DO 1210 II=1,30
            IF(IANS(II)(1:1).EQ.IHARG(1)(1:1)   .AND.   &
               IANS(II+1)(1:1).EQ.IHARG(1)(2:2) .AND.   &
               IANS(II+2)(1:1).EQ.IHARG(1)(3:3) .AND.   &
               IANS(II+3)(1:1).EQ.IHARG(1)(4:4))THEN
              ISTRT=II
              GO TO 1219
            ENDIF
 1210     CONTINUE
 1219     CONTINUE
          IF(ISTRT.GE.1)THEN
            DO 1220 II=ISTRT,IWIDTH
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
          GO TO 1201
        ENDIF
      ENDIF
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               **********************************
!               **  TREAT THE REPEAT GRAPH CASE **
!               **********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'REPE'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GO TO 12900
      IF(ICOM.EQ.'RG  ')GO TO 12900
      IF(ICOM.EQ.'RP  ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GO TO 12900
      IF(NUMARG.GE.1.AND.ICOM.EQ.'VIEW'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GO TO 12900
      IF(ICOM.EQ.'VG  ')GO TO 12900
      IF(ICOM.EQ.'VP  ')GO TO 12900
      GO TO 12990
!
12900 CONTINUE
      CALL DPREGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,   &
      IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
12990 CONTINUE
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               **********************************
!               **  TREAT THE LIST   GRAPH CASE **
!               **********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GO TO 13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GO TO 13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GO TO 13000
      IF(NUMARG.GE.1.AND.ICOM.EQ.'LIST'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GO TO 13000
      IF(ICOM.EQ.'LG  ')GO TO 13000
      IF(ICOM.EQ.'LP  ' .AND. IHARG(1).NE.'LOCA')GO TO 13000
      GO TO 13090
!
13000 CONTINUE
      CALL DPLIGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,   &
      IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
13090 CONTINUE
!
!CCCC FOLLOWING SECTION ADDED APRIL 1997.
!               **********************************
!               **  TREAT THE CYCLE  GRAPH CASE **
!               **********************************
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'    ')GO TO 13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.   &
      IHARG(1).EQ.'PLOT'.AND.IHARG2(1).EQ.'S   ')GO TO 13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'H   ')GO TO 13100
      IF(NUMARG.GE.1.AND.ICOM.EQ.'CYCL'.AND.   &
      IHARG(1).EQ.'GRAP'.AND.IHARG2(1).EQ.'HS  ')GO TO 13100
      IF(ICOM.EQ.'CG  ')GO TO 13100
!CCCC MARCH  1998.  CONFLICT WITH CP PLOT COMMAND.
!CCCC AUGUST 1998.  CONFLICT WITH CP CUMULATIVE STATISTIC PLOT COMMAND.
!CCCC AUGUST 1998.  CONFLICT WITH CP MOVING     STATISTIC PLOT COMMAND.
      IF(ICOM.EQ.'CP  ')THEN
        IF(IHARG(1).EQ.'PLOT')GO TO 13190
        IF(IHARG(1).EQ.'CUMU' .AND. IHARG(2).EQ.'STAT')GO TO 13190
        IF(IHARG(1).EQ.'MOVI' .AND. IHARG(2).EQ.'STAT')GO TO 13190
        GO TO 13100
      ENDIF
      GO TO 13190
!
13100 CONTINUE
      CALL DPCYGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,   &
      IBUGS2,ISUBRO,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
13190 CONTINUE
!
!               ***************************
!               **  TREAT THE CD     CASE**
!               ***************************
!
      IF(ICOM.EQ.'CD  ')THEN
        CALL DPCDIR(IANS,IANSLC,IWIDTH,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                    IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                    IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE RM     CASE     **
!               ********************************
!
      IF(ICOM.EQ.'RM  ' .OR.   &
        (ICOM.EQ.'RMDI' .AND. ICOM2.EQ.'R   '))THEN
        CALL DPRM(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE MKDIR  CASE     **
!               ********************************
!
      IF(ICOM.EQ.'MKDI' .AND. ICOM2.EQ.'R   ')THEN
        CALL DPMKDR(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE CAT    CASE     **
!               ********************************
!
      IF(ICOM.EQ.'CAT ' .OR.   &
        (ICOM.EQ.'TYPE' .AND. ICOM2.EQ.'    '))THEN
        CALL DPCAT(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ********************************
!               **  TREAT THE DIR    CASE     **
!               ********************************
!
      IF(ICOM.EQ.'DIR ' .OR. ICOM.EQ.'LS  ')THEN
        CALL DPDIR(IANS,IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  TREAT THE RECIPE SATTERWAITE APPROXIMATION CASE **
!               ******************************************************
!
      IF(ICOM.EQ.'RECI')THEN
        IF((NUMARG.GE.2.AND.IHARG(1).EQ.'SATT'.AND.   &
           IHARG(2).EQ.'APPR') .OR.   &
           (NUMARG.GE.1.AND.IHARG(1).EQ.'SATT') .OR.   &
           (NUMARG.GE.1.AND.IHARG(1).EQ.'APPR'))THEN
          CALL DPRESA(IHARG,NUMARG,IDEFSA,IRECSA,IBUGS2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE PROBABILITY CONTENT       CASE **
!               ******************************************************
!
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.   &
               IHARG(2).EQ.'PLOT') .OR.   &
               (NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.   &
               IHARG(2).EQ.'CONT') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'CONT') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'PROB'))THEN
          CALL DPREPC(IHARG,IARGT,ARG,NUMARG,DEFRPC,RECIPC,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE CONFIDENCE                CASE **
!               ******************************************************
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')THEN
          CALL DPRECO(IHARG,IARGT,ARG,NUMARG,DEFRCO,RECICO,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE FIT DEGREE                CASE **
!               ******************************************************
!
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.   &
               IHARG(2).EQ.'DEGR') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'DEGR'))THEN
          CALL DPREDG(IHARG,IARGT,ARG,NUMARG,DEFRDG,RECIDG,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE ANOVA FACTORS             CASE **
!               ******************************************************
!
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'ANOV'.AND.   &
               IHARG(2).EQ.'FACT') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'FACT'))THEN
          CALL DPREFA(IHARG,IARGT,ARG,NUMARG,DEFRFA,RECIFA,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE OUTPUT                    CASE **
!               ******************************************************
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'OUTP')THEN
          CALL DPRETN(IHARG,NUMARG,IDEFTN,IRECTN,   &
                      IBUGS2,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE CORRELATION               CASE **
!               ******************************************************
!
        ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORR')THEN
          CALL DPRECR(IHARG,IARGT,IARG,NUMARG,IDEFR9,IRECC1,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE SIMCOV REPLICATES         CASE **
!               ******************************************************
!
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'SIMC'.AND.   &
               IHARG(2).EQ.'REPL') .OR.   &
               (NUMARG.GE.2.AND.IHARG(1).EQ.'REPL'.AND.   &
               IHARG(2).EQ.'SIMC') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'SIMC') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'REPL'))THEN
          CALL DPRES1(IHARG,IARGT,IARG,NUMARG,IDEFR7,IRECR1,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  TREAT THE RECIPE SIMPVT REPLICATES         CASE **
!               ******************************************************
!
        ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'SIMP'.AND.   &
               IHARG(2).EQ.'REPL') .OR.   &
               (NUMARG.GE.2.AND.IHARG(1).EQ.'REPL'.AND.   &
               IHARG(2).EQ.'SIMP') .OR.   &
               (NUMARG.GE.1.AND.IHARG(1).EQ.'SIMP'))THEN
          CALL DPRESZ(IHARG,IARGT,IARG,NUMARG,IDEFR8,IRECR2,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
!
!CCCC FOLLOWING SECTION ADDED APRIL 1998.
!               ******************************************************
!               **  TREAT THE RECIPE FIT   FACTORS             CASE **
!               ******************************************************
!
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.   &
               IHARG(2).EQ.'FACT')THEN
          CALL DPREFF(IHARG,IARGT,ARG,NUMARG,DEFRFF,RECIFF,   &
                      IFOUND,IERROR)
          IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
      ENDIF
!
!               ********************************
!               **  TREAT THE AUTO TEXT CASE  **
!               ********************************
!
      IF(ICOM.EQ.'AUTO'.AND.   &
         NUMARG.GE.1.AND.IHARG(1).EQ.'TEXT')THEN
        CALL DPAUTX(IHARG,NUMARG,IATXSW,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IERRST=IERROR
!
!     AUGUST 2007.  CHECK FOR FATAL ERROR
!
      IF(IERROR.EQ.'YES')THEN
        ISUBN1='MAIN'
        ISUBN2='SU  '
        ICASE2='SUPP'
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,   &
                    ISUBN1,ISUBN2,ICASE2,   &
                    IBUGS2,ISUBRO,IERROR)
      ENDIF
!
!
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
!
      RETURN
      END SUBROUTINE MAINSU
      SUBROUTINE MAKCDF(X,XI,LAMBDA,THETA,CDF)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
!     DISTRIBUTION FUNCTION. IT HAS THE FOLLOWING CDF:
!         F(X,XI,LAMBDA,THETA) = 1 -
!                EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
!                X > 0; LAMBDA, XI > 0, THETA >= 0
!     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
!     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
!     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
!     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
!
!         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
!         LAMBDA(DLMF) = K(MEEKER)
!         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/12
!     ORIGINAL VERSION--DECEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL LAMBDA
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CDF=0.0
      IF(X.LE.0.0)GO TO 9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKCDF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKCDF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKCDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
!
      DTERM1=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
!
      IF(DTERM1.LE.-80.D0)THEN
        CDF=1.0
        GO TO 9999
      ELSEIF(DTERM1.GE.80.D0)THEN
        CDF=0.0
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSE
        DCDF=1.0D0 - DEXP(DTERM1)
        CDF=REAL(DCDF)
      ENDIF
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKCDF.  THE COMPUTED ',   &
      'CDF VALUE EXCEEDS MACHINE PRECISION.')
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MAKCDF
      REAL FUNCTION MAKFU2(X)
!
!     PURPOSE--MAKPPF CALLS FZERO TO FIND A ROOT FOR THE PERCENT
!              POINT FUNCTION.  MAKFU2 IS THE FUNCTION FOR WHICH
!              THE ZERO IS FOUND.  IT IS:
!                 P - MAKCDF(X,XI,LAMBDA,THETA)
!              WHERE P IS THE DESIRED PERCENT POINT.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE MAKFU2.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--MAKCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
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
      REAL P
      COMMON/MA2COM/P
!
      REAL XI
      REAL LAMBDA
      REAL THETA
      COMMON/MAKCOM/XI,LAMBDA,THETA
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CALL MAKCDF(X,XI,LAMBDA,THETA,CDF)
      MAKFU2=P - CDF
      RETURN
      END FUNCTION MAKFU2
      SUBROUTINE MAKCHA(X,XI,LAMBDA,THETA,HAZ)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
!     HAZARD FUNCTION WHICH HAS THE FOLLOWING FORMULA:
!         H(X,XI,LAMBDA,THETA) =
!                      -[-XI*(EXP(LAMBDA*X) - 1) - XI*THETA*LAMBDA*X]
!                X > 0; LAMBDA, XI > 0, THETA >= 0
!     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
!     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
!     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
!     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
!
!         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
!         LAMBDA(DLMF) = K(MEEKER)
!         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL LAMBDA
!
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      HAZ=0.0
      IF(X.LE.0.0)GO TO 9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKCHAZ')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKCHAZ IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKCHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
!
      DHAZ=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
      HAZ=-REAL(DHAZ)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MAKCHA
      SUBROUTINE MAKHAZ(X,XI,LAMBDA,THETA,HAZ)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM
!     HAZARD FUNCTION WHICH HAS THE FOLLOWING FORMULA:
!         h(X,XI,LAMBDA,THETA) = f(X,XI,LAMBDA,THETA)/
!                                -LOG[1 - F(x,XI,LAMBDA,THETA)]
!                              = XI*THETA*LAMBDA + XI*LAMBDA*
!                                EXP(LAMBDA*X)
!                X > 0; LAMBDA, XI > 0, THETA >= 0
!         WHERE f IS THE PROBABILITY DENSITY AND F IS THE
!         CUMULATIVE DISTRIBUTION FUNCTION.
!
!     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
!     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
!     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
!     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
!
!         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
!         LAMBDA(DLMF) = K(MEEKER)
!         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL LAMBDA
!
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      HAZ=0.0
      IF(X.LE.0.0)GO TO 9999
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKHAZZ')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKHAZZ IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKHAZZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
!
      DTERM1=DXI*DTHETA*DLMBDA
      DTERM2=DXI*DLMBDA*DEXP(DLMBDA*DX)
      DHAZ=DTERM1 + DTERM2
      HAZ=REAL(DHAZ)
      HAZ=REAL(DHAZ)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MAKHAZ
      SUBROUTINE MAKPDF(X,XI,LAMBDA,THETA,PDF)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM PROBABILITY
!     DENSITY FUNCTION. VALUE DISTRIBUTION.  IT HAS THE FOLLOWING
!     PDF:
!         F(X,XI,LAMBDA,THETA) = XI*LAMBDA*(THETA + EXP(LAMBDA*X))*
!                EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
!                X > 0; LAMBDA, XI > 0, THETA >= 0
!     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
!     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
!     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
!     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
!
!         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
!         LAMBDA(DLMF) = K(MEEKER)
!         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!         LAMBDA = THETA*LAMBDA*XI
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/12
!     ORIGINAL VERSION--DECEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL LAMBDA
!
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DLMBDA
      DOUBLE PRECISION DTHETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      PDF=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKPDF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKPDF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKPDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
  301 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO MAKPDF IS')
  302 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DXI=DBLE(XI)
      DLMBDA=DBLE(LAMBDA)
      DTHETA=DBLE(THETA)
!
      DTERM1=DLOG(DXI) + DLOG(DLMBDA)
      DTERM2=DLOG(DTHETA + DEXP(DLMBDA*DX))
      DTERM3=-DXI*(DEXP(DLMBDA*DX) - 1.0D0) - DXI*DLMBDA*DTHETA*DX
      DTERM4=DTERM1 + DTERM2 + DTERM3
!
      IF(DTERM4.LE.-80.D0)THEN
        PDF=0.0
        GO TO 9999
      ELSEIF(DTERM4.GE.80.D0)THEN
        PDF=0.0
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKPDF.  THE COMPUTED ',   &
      'PDF VALUE EXCEEDS MACHINE PRECISION.')
!
      DPDF=DEXP(DTERM4)
      PDF=REAL(DPDF)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MAKPDF
      SUBROUTINE MAKPPF(P,XI,LAMBDA,THETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE GOMPERTZ-MAKEHAM DISTRIBUTION
!              WITH SHAPE PARAMETERS XI, LAMBDA, AND THETA.
!              THIS DISTRIBUTION IS DEFINED FOR POSITIVE X AND THE
!              PERCENT POINT FUNCTION IS COMPUTED BY
!              NUMERICALLY INVERTING THE CDF FUNCTION.
!
!     NOTE THAT THIS IS THE PARAMETERIZATION USED BY THE DIGITAL
!     LIBRARY OF MATHEMATICAL FUNCTIONS (DLMF).  TO USE THE
!     PARAMETERIZATION GIVEN ON PAGE 108-109 OF MEEKER AND ESCOBAR,
!     DO THE FOLLOWING BEFORE CALLING THIS ROUTINE:
!
!         XI(DLMF) = GAMMA(MEEKER)/K(MEEKER)
!         LAMBDA(DLMF) = K(MEEKER)
!         THETA(DLMF) = LAMBDA(MEEKER)/GAMMA(MEEKER)
!
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --XI     = THE FIRST SHAPE PARAMETER
!                     --LAMBDA = THE SECOND SHAPE PARAMETER
!                     --THETA  = THE THIRD SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--FZERO.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
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
      REAL LAMBDA
      REAL PPF
!
      REAL MAKFU2
      EXTERNAL MAKFU2
!
      REAL P2
      COMMON/MA2COM/P2
!
      REAL XI2
      REAL LAMBD2
      REAL THETA2
      COMMON/MAKCOM/XI2,LAMBD2,THETA2
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
      PPF=0.0
!
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
         WRITE(ICOUT,61)
   61    FORMAT('***** ERROR--THE FIRST  INPUT ARGUMENT ',   &
                'TO THE MAKPPF SUBROUTINE ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,62)
   62    FORMAT('      IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL ***')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,63)P
   63    FORMAT('      VALUE OF ARGUMENT = ',G15.7)
         CALL DPWRST('XXX','BUG ')
         PPF=0.0
         GO TO 9000
      ENDIF
!
      IF(XI.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(THETA.LT.0.0)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (XI) TO MAKPPF')
  102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (LAMBDA) TO')
  107 FORMAT('      MAKPPF IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  111 FORMAT('***** ERROR--THE THIRD SHAPE PARAMETER (THETA) TO')
  112 FORMAT('      MAKPPF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      IF(P.EQ.0.0)THEN
        PPF=0.0
        GO TO 9000
      ENDIF
!
!  STEP 1: FIND BRACKETING INTERVAL.  LOWER BOUND IS ZERO.  START WITH
!          10 AS GUESS FOR UPPER BOUND.  MULTIPLY BY 10 UNTIL
!          BRACKETING INTERVAL FOUND.
!
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
  201       FORMAT('***** ERROR FROM MAKPPF--UNABLE TO FIND A ',   &
                   'BRACKETING INTERVAL')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
          GO TO 200
        ENDIF
!
      AE=1.E-6
      RE=1.E-6
      P2=P
      XI2=XI
      LAMBD2=LAMBDA
      THETA2=THETA
      CALL FZERO(MAKFU2,XLOW,XUP,XUP,RE,AE,IFLAG)
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
!C111   FORMAT('***** WARNING FROM MAKPPF--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,113)
!C113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
!CCCC1         'TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MAKPPF
      SUBROUTINE MAKRAN(N,XI,LAMBDA,THETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE THE GOMPERTZ-MAKEHAM DISTIBUTION WITH
!              LOCATION = 0 AND SCALE = 1.  THIS DISTRIBUTION IS
!              DEFINED FOR POSITIVE X AND HAS THE PROBABILITY DENSITY
!              FUNCTION:
!              F(X,XI,LAMBDA,THETA) = XI*LAMBDA*(THETA + EXP(LAMBDA*X))
!                *EXP[-XI*(EXP(LAMBDA*X) -1) - XI*THETA*LAMBDA*X)
!                X > 0; LAMBDA, XI > 0, THETA >= 0
!              XI, LAMBDA, AND THETA ARE SHAPE PARAMETERS.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!                     --XI     = A SINGLE PRECISON SCALAR THAT DEFINES
!                                THE FIRST SHAPE PARAMETER.
!                     --LAMBDA = A SINGLE PRECISON SCALAR THAT DEFINES
!                                THE SECOND SHAPE PARAMETER.
!                     --THETA  = A SINGLE PRECISON SCALAR THAT DEFINES
!                                THE THIRD SHAPE PARAMETER.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE COMPERTZ-MAKEHAM
!             DISTRIBUTION WITH LOCATION = 0 AND SCALE = 1.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, MAKPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     METHOD--TRANSFORM NORMAL RANDOM NUMBERS
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--2003.12
!     ORIGINAL VERSION--DECEMBER  2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      REAL XI
      REAL THETA
      REAL LAMBDA
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
        GO TO 9999
      ELSEIF(XI.LE.0.0)THEN
        WRITE(ICOUT, 6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)XI
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(LAMBDA.LE.0.0)THEN
        WRITE(ICOUT, 7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)LAMBDA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(THETA.LT.0.0)THEN
        WRITE(ICOUT,8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE FIRST (N) INPUT ARGUMENT TO THE ',   &
      'MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    6 FORMAT('***** FATAL ERROR--THE SECOND (XI) INPUT ARGUMENT TO ',   &
      'THE MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    7 FORMAT('***** FATAL ERROR--THE THIRD (LAMBDA) INPUT ARGUMENT ',   &
      'TO THE MAKRAN SUBROUTINE IS NON-POSITIVE *****')
    8 FORMAT('***** FATAL ERROR--THE FOURTH (THETA) INPUT ARGUMENT ',   &
      'TO THE MAKRAN SUBROUTINE IS NEGATIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F15.7,' *****')
!
!     GENERATE N UNIFORM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N GOMPERTZ-MAKEHAM RANDON NUMBERS USING THE
!     PERCENT POINT FUNCTION TRANSFORMATION.
!
      DO 100 I=1,N
        XTEMP=X(I)
        CALL MAKPPF(XTEMP,XI,LAMBDA,THETA,PPF)
        X(I)=PPF
  100 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MAKRAN
      SUBROUTINE MA2CDF(X,ZETA,ETA,CDF)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
!     DISTRIBUTION FUNCTION. THIS USES THE MEEKER AND ESCOBAR
!     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
!     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
!     PARAMETER.  IT HAS THE FOLLOWING CDF:
!         F(X,ZETA,ETA) = 1 - EXP[C1 - EXP(C2) - C3]
!                X,  > 0; ETA >= 0
!     WITH
!         C1 = EXP(-ZETA)
!         C2 = EXP(LOG(X)) - ZETA
!            = X - ZETA
!         C3 = ETA*EXP(LOG(X))
!            = ETA*X
!
!     PUTTING THIS TOGETHER GIVES
!         F(X,ZETA,ETA) = 1 - EXP[EXP(-ZETA) - EXP(X-ZETA) - ETA*X]
!                X,  > 0; ETA >= 0
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ZETA
      REAL ETA
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CDF=0.0
      IF(X.LE.0.0)GO TO 9000
!CCCC IF(ETA.LE.0.0)THEN
!CCCC   WRITE(ICOUT,101)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,102)ETA
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
!CCCC ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ZETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!C101 FORMAT('***** ERROR--THE FIRST SHAPE PARAMETER (ETA) TO MA2CDF')
!C102 FORMAT('      IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ZETA) TO')
  107 FORMAT('      MAKCDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
!
      DTERM1=DEXP(-DZETA)
      DTERM2=DEXP(DX - DZETA)
      DTERM3=DETA*DX
      DTERM4=DTERM1 - DTERM2 - DTERM3
!
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
  401 FORMAT('***** NON-FATAL DIAGNOSTIC FROM MAKCDF.  THE ',   &
      'COMPUTED CDF VALUE EXCEEDS MACHINE PRECISION.')
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MA2CDF
      REAL FUNCTION MA2FU2(X)
!
!     PURPOSE--MA2PPF CALLS FZERO TO FIND A ROOT FOR THE PERCENT
!              POINT FUNCTION.  MA2FU2 IS THE FUNCTION FOR WHICH
!              THE ZERO IS FOUND.  IT IS:
!                 P - MA2CDF(X,ZETA,ETA)
!              WHERE P IS THE DESIRED PERCENT POINT.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE MA2FU2.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--MA2CDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL P
      COMMON/MA4COM/P
!
      REAL ETA
      REAL ZETA
      COMMON/MA3COM/ETA,ZETA
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CALL MA2CDF(X,ZETA,ETA,CDF)
      MA2FU2=P - CDF
      RETURN
      END FUNCTION MA2FU2
      SUBROUTINE MA2CHA(X,ZETA,ETA,CHAZ)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM CUMULATIVE
!     HAZARD FUNCTION. THIS USES THE MEEKER AND ESCOBAR
!     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
!     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
!     PARAMETER.  IT HAS THE FOLLOWING CDF:
!         F(X,ZETA,ETA) = 1 -
!                         EXP[EXP(-ZETA) - EXP(X - ZETA) - ETA*X]
!                X,  > 0; ETA >= 0
!     THE CUMULATIVE HAZARD IS:
!         H(X,ZETA,ETA) = -LOG(1 - F(X,ZETA,ETA))
!                        = -EXP(-ZETA) + EXP(X-ZETA) + ETA*X
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ZETA
      REAL ETA
!
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CHAZ=0.0
      IF(X.LE.0.0)GO TO 9000
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKCHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
!
      DTERM1=DEXP(-DZETA)
      DTERM2=DEXP(DX - DZETA)
      DTERM3=DETA*DX
      DTERM4=DTERM1 - DTERM2 - DTERM3
      CHAZ=-REAL(DTERM4)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MA2CHA
      SUBROUTINE MA2HAZ(X,ZETA,ETA,HAZ)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM HAZARD
!     FUNCTION. THIS USES THE MEEKER AND ESCOBAR
!     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
!     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
!     PARAMETER.  IT HAS THE FOLLOWING HAZARD FUNCTION:
!         h(X,ZETA,ETA) = ETA + EXP(-ZETA)*EXP(X)
!                         X, ETA >= 0
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ZETA
      REAL ETA
!
      DOUBLE PRECISION DHAZ
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      HAZ=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  103 FORMAT('***** ERROR--THE INPUT ARGUMENT  TO MA2HAZ IS')
  104 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKHAZ IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DETA=DBLE(ETA)
      DZETA=DBLE(ZETA)
!
      DHAZ=DETA + DEXP(-DZETA)*DEXP(DX)
      HAZ=REAL(DHAZ)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MA2HAZ
      SUBROUTINE MA2PDF(X,ZETA,ETA,PDF)
!
!     THIS SUBROUTINE COMPUTES THE GOMPERTZ-MAKEHAM PROBABILITY
!     DENSITY FUNCTION. THIS USES THE MEEKER AND ESCOBAR
!     PARAMETERIZATION (THIS TAKES THE 3-SHAPE PARAMETER CASE AND
!     RE-PARAMETERRIZES IT TO 2-SHAPE PARAMETERS AND A SCALE
!     PARAMETER.  IT HAS THE FOLLOWING PROBABILITY DENSITY FUNCTION:
!         f(X,ZETA,ETA) = (ETA + EXP(X-ZETA))*
!                         EXP[EXP(-ZETA)-EXP(X-ZETA)-ETA*X]
!                         X, ETA > 0
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL ZETA
      REAL ETA
!
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DETA
      DOUBLE PRECISION DZETA
      DOUBLE PRECISION DX
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      PDF=0.0
      IF(X.LE.0.0)THEN
        WRITE(ICOUT,103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!CCCC IF(ZETA.LE.0.0)THEN
!CCCC   WRITE(ICOUT,101)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,102)ZETA
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
!CCCC ENDIF
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  103 FORMAT('***** ERROR--THE INPUT ARGUMENT  TO MA2PDF IS')
  104 FORMAT('      NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKPDF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
      DX=DBLE(X)
      DZETA=DBLE(ZETA)
      DETA=DBLE(ETA)
!
      DTERM1=DETA + EXP(DX-DZETA)
      DTERM2=DEXP(-DZETA) - DEXP(DX-DZETA) - DETA*DX
      DPDF=DTERM1*DEXP(DTERM2)
      PDF=REAL(DPDF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MA2PDF
      SUBROUTINE MA2PPF(P,ZETA,ETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE GOMPERTZ-MAKEHAM DISTRIBUTION
!              WITH SHAPE PARAMETERS ETA AND ZETA.
!              THIS DISTRIBUTION IS DEFINED FOR POSITIVE X AND THE
!              PERCENT POINT FUNCTION IS COMPUTED BY
!              NUMERICALLY INVERTING THE CDF FUNCTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --ZETA l = THE FIRST SHAPE PARAMETER
!                     --ETA    = THE SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--FZERO.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL ETA
      REAL ZETA
      REAL PPF
!
      REAL MA2FU2
      EXTERNAL MA2FU2
!
      REAL P2
      COMMON/MA4COM/P2
!
      REAL ETA2
      REAL ZETA2
      COMMON/MA3COM/ETA2,ZETA2
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
      PPF=0.0
!
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
         WRITE(ICOUT,61)
   61    FORMAT('***** ERROR--THE FIRST  INPUT ARGUMENT ',   &
                'TO THE MA2PPF SUBROUTINE ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,62)
   62    FORMAT('      IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL ***')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,63)P
   63    FORMAT('      VALUE OF ARGUMENT = ',G15.7)
         CALL DPWRST('XXX','BUG ')
         PPF=0.0
         GO TO 9000
      ENDIF
!
      IF(ETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)ETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ETA) TO')
  107 FORMAT('      MAKPPF IS NEGATIVE.  IT HAS THE VALUE ',E15.7)
!
!  STEP 1: FIND BRACKETING INTERVAL.  LOWER BOUND IS ZERO.  START WITH
!          10 AS GUESS FOR UPPER BOUND.  MULTIPLY BY 10 UNTIL
!          BRACKETING INTERVAL FOUND.
!
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
  201       FORMAT('***** ERROR FROM MA2PPF--UNABLE TO FIND A ',   &
                   'BRACKETING INTERVAL')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
          GO TO 200
        ENDIF
!
      AE=1.E-6
      RE=1.E-6
      P2=P
      ETA2=ETA
      ZETA2=ZETA
      CALL FZERO(MA2FU2,XLOW,XUP,XUP,RE,AE,IFLAG)
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
!C111   FORMAT('***** WARNING FROM MA2PPF--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,113)
!C113   FORMAT('      PPF VALUE MAY NOT BE COMPUTED TO DESIRED ',
!CCCC1         'TOLERANCE.')
!CCCC   CALL DPWRST('XXX','BUG ')
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MA2PPF
      SUBROUTINE MA2RAN(N,ZETA,ETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE THE GOMPERTZ-MAKEHAM DISTIBUTION WITH
!              LOCATION = 0 AND SCALE = 1.  THIS DISTRIBUTION IS
!              DEFINED FOR POSITIVE X AND HAS THE PROBABILITY DENSITY
!              FUNCTION:
!              f(X,ETA,ZETA) = (1/X)*EXP(LOG(X))*
!                              [ZETA + EXP[EXP(LOG(X)) - ETA]*
!                              [1 - MA2CDF(X,ETA,ZETA)]
!                              X, ZETA > 0
!             WHERE MA2CDF IS:
!             F(X,ETA,ZETA) = 1 - EXP[C1 - EXP(C2) - C3]
!                             X, ZETA > 0
!             WITH
!                 C1 = EXP(-ETA)
!                 C2 = EXP(LOG(X) - ETA)
!                 C3 = ZETA*EXP(LOG(X))
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!                     --ETA    = A SINGLE PRECISON SCALAR THAT DEFINES
!                                THE FIRST SHAPE PARAMETER.
!                     --ZETA   = A SINGLE PRECISON SCALAR THAT DEFINES
!                                THE SECOND SHAPE PARAMETER.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE COMPERTZ-MAKEHAM
!             DISTRIBUTION WITH LOCATION = 0 AND SCALE = 1.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EETASTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAETAMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, MA2PPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     METHOD--TRANSFORM NORMAL RANDOM NUMBERS
!     REFERENCE--"STATISTICAL METHODS FOR RELIABILITY DATA",
!                MEEKER AND ESCOBAR, WILEY, 1998, PP. 108-109.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.7
!     ORIGINAL VERSION--JULY      2004.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      REAL ETA
      REAL ZETA
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
        WRITE(ICOUT, 6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(ZETA.LT.0.0)THEN
        WRITE(ICOUT,106)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,107)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)ZETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
  106 FORMAT('***** ERROR--THE SECOND SHAPE PARAMETER (ZETA) TO THE')
  107 FORMAT('      GOMPERTZ MAKEHAM RANDOM NUMBERS ROUTINE IS ',   &
             'NON-POSITIVE.')
    5 FORMAT('***** THE REQUESTED NUMBER OF RANDOM NUMBERS FOR THE')
    6 FORMAT('      GOMPERTZ-MAKEHAM DISTRIBUTION IS NON-POSITIVE.')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F15.7)
!
!     GENERATE N UNIFORM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N GOMPERTZ-MAKEHAM RANDON NUMBERS USING THE
!     PERCENT POINT FUNCTION TRANSFORMATION.
!
      DO 100 I=1,N
        XTEMP=X(I)
        CALL MA2PPF(XTEMP,ZETA,ETA,PPF)
        X(I)=PPF
  100 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE MA2RAN
      SUBROUTINE MANDIS(X,Y,N,IWRITE,STATVA,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE MANHATTAN DISTANCE BETWEEN THE
!              TWO SETS OF DATA IN THE INPUT VECTORS X AND Y.  THE
!              SAMPLE MANHATTAN DISTANCE WILL BE A SINGLE PRECISION VALUE
!              CALCULATED AS:
!
!                 DISTANCE = SUM[i=1 to n][|X(i) - Y(i)|]
!
!              THIS IS EQUIVALENT TO A MINKOWSKY DISTANCE WITH
!              P = 1 AND IS ALSO KNOWN CITY BLOCK OR TAXI-CAB
!              DISTANCE.
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
!             SAMPLE MANHATTAN DISTANCE BETWEEN THE 2 SETS
!             OF DATA IN THE INPUT VECTORS X AND Y.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--ABS.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--XXX
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
      ISUBN1='MAND'
      ISUBN2='IS  '
      IERROR='NO'
      STATVA=CPUMIN
!
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
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  COMPUTE THE MANHATTAN DISTANCE.           **
!               ************************************************
!
      STATVA=0.0
      DO 200 I=1,N
        STATVA=STATVA + ABS(X(I) - Y(I))
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
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE MANHATTAN DISTANCE OF THE ',I8,   &
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
!
      RETURN
      END SUBROUTINE MANDIS
      SUBROUTINE MATARI(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,MAXROM,MAXCOM,   &
      Y1,N1,Y2,N2,Y3,N3,Y4,N4,   &
      INDEX,IZROV,IPOSV,   &
      DMEAN,DSSQD,P1,P2,BETA,   &
      YS1,YS2,YS3,YS4,   &
      IMCASE,IUPFLG,IMSUBC,ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,   &
      YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,   &
      IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
!              OF THE REAL DATA IN MATRICES YM1 AND YM2.
!
!     OPERATIONS--ADDITION
!                 SUBTRACTION
!                 MULTIPLICATION
!                 TRUNCATION
!
!                 NUMBER OF ROWS
!                 NUMBER OF COLUMNS
!                 ROW
!                 ELEMENT
!                 REPLACE ROW
!                 REPLACE ELEMENT
!                 DIAGONAL
!
!                 SOLUTION
!                 ITERATIVE SOLUTION
!                 TRIDIAGONAL SOLVE
!                 TRIANGULAR SOLVE
!                 SIMPLEX SOLUTION
!                 RANK
!
!                 CONDITION NUMBER
!                 RECIPROCAL CONDITION NUMBER
!                 INVERSE
!                 TRIANGULAR INVERSE
!                 DETERMINANT
!                 TRACE
!                 PERMANENT
!                 ADJOINT
!                 SUBMATRIX
!                 MINOR
!                 COFACTOR
!
!                 DEFINITION
!                 AUGMENT
!                 TRANSPOSE
!
!                 CHARACTERISTIC EQUATION      (NOT YET IMPLEMENED)
!
!                 EIGENVALUES
!                 EIGENVECTORS
!                 SINGULAR VALUE
!                 SINGULAR VALUE DECOMPOSITION
!                 CHOLESKY DECOMPOSITION
!                 SPECTRAL NORM
!                 SPECTRAL RADIUS
!                 EUCLIDEAN NORM
!
!                 VARIANCE-COVARIANCE MATRIX
!                 CORRELATION MATRIX
!                 PRINCIPAL COMPONENTS ...
!                 ... PRINCIPAL COMPONENT ...
!                 COMOVEMENT MATRIX
!
!     EXAMPLES--LET M3 = MATRIX ADDITION M1 M2
!               LET M3 = MATRIX ADDITION M1 P1
!             --LET M3 = MATRIX SUBTRACTION M1 M2
!               LET M3 = MATRIX SUBTRACTION M1 P1
!             --LET M3 = MATRIX MULTIPLICATION M1 M2
!               LET M3 = MATRIX MULTIPLICATION M1 V1
!               LET M3 = MATRIX MULTIPLICATION M1 P1
!             --LET V3 = MATRIX SOLUTION M1 V2
!             --LET V3 = MATRIX ITERATIVE SOLUTION M1 V2
!             --LET M3 = MATRIX INVERSE M1
!             --LET P3 = MATRIX CONDITION NUMBER M1
!             --LET P3 = MATRIX RECIPROCAL CONDITION NUMBER M1
!             --LET M3 = MATRIX TRANSPOSE M1
!             --LET M3 = MATRIX ADJOINT M1
!             --LET V3 = MATRIX CHARACTERISTIC EQUATION M1
!             --LET V3 = MATRIX EIGENVALUES M1
!             --LET P3 = MATRIX EIGENVECTORS M1
!             --LET P3 = MATRIX RANK M1
!             --LET P3 = MATRIX DETERMINANT M1
!             --LET P3 = MATRIX PERMANENT M1
!             --LET P3 = MATRIX SPECTRAL NORM M1
!             --LET P3 = MATRIX SPECTRAL RADIUS M1
!             --LET P3 = MATRIX NUMBER OF ROWS M1
!             --LET P3 = MATRIX NUMBER OF COLUMNS M1
!             --LET V4 = MATRIX SIMPLEX SOLUTION V1 M1 V2 V3
!             --LET P3 = MATRIX TRACE M1
!             --LET M3 = MATRIX SUBMATRIX M1 P1 P2
!             --LET P3 = MATRIX MINOR M1 P1 P2
!             --LET P3 = MATRIX COFACTOR M1 P1 P2
!             --LET M3 = MATRIX DEFINITION V1 P1 P2
!             --LET M3 = MATRIX DEFINITION V1 P1 P2 P3
!             --LET P3 = MATRIX EUCLIDEAN NORM M1
!             --LET V3 = MATRIX ROW M1 P1
!             --LET P3 = MATRIX ELEMENT M1 P1 P2
!             --LET M3 = MATRIX REPLACE ROW M1 V1 P1
!             --LET M3 = MATRIX REPLACE ELEMENT M1 P1 P2
!             --LET M3 = MATRIX AUGMENT M1
!             --LET V3 = MATRIX DIAGONAL M1
!             --LET M3 = DIAGONAL MATRIX V1
!             --LET M3 = VARIANCE-COVARIANCE MATRIX M1
!             --LET M3 = CORRELATION MATRIX M1
!             --LET M3 = PRINCIPAL COMPONENTS M1
!             --LET M3 = PRINCIPAL COMPONENTS EIGENVECTORS M1
!             --LET V3 = PRINCIPAL COMPONENTS EIGENVALUES M1
!             --LET V3 = ... PRINCIPAL COMPONENT M1
!             --LET V3 = ... PRINCIPAL COMPONENT EIGENVECTOR M1
!             --LET P3 = ... PRINCIPAL COMPONENT EIGENVALUE M1
!             --LET V3 = MATRIX SINGULAR VALUES M1
!             --LET M3 V3 M2 = MATRIX SINGULAR VALUE DECOMP M1
!             --LET M3 V3 M2 = MATRIX SINGULAR VALUE FACTOR M1
!             --LET M3 = CHOLESKY DECOMP M1
!             --LET V4 = TRIDIAGONAL SOLVE V1 V2 V3
!             --LET V4 = TRIANGULAR SOLVE M1 V2
!             --LET M3 = TRIANGULAR INVERSE M2
!             --LET M3 = MATRIX TRUNCATION M1 P1
!             --LET M3 = MATRIX UPPPER TRUNCATION M1 P1
!
!     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
!                     --NR1
!                     --NC1
!                     --YM2 (REAL MATRIX)
!                     --NR2
!                     --NC2
!                     --YM3 (REAL MATRIX)
!                     --NR3
!                     --NC3
!                     --Y1  (REAL VECTOR)
!                     --N1
!                     --Y2  (REAL VECTOR)
!                     --N2
!                     --Y3  (REAL VECTOR)
!                     --N3
!                     --Y4  (REAL VECTOR)
!                     --N4
!     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
!                     --NR9
!                     --NC9
!                     --VECT9 (REAL VECTOR)
!                     --NVECT9
!                     --SCAL9 (REAL SCALAR)
!                     --ITYP9
!
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
!           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/10
!     ORIGINAL VERSION--SEPTEMBER 1987
!     UPDATED         --AUGUST    1988  (VARIANCE-COVARIANCE MATRIX)
!     UPDATED         --AUGUST    1988  (CORRELATION MATRIX)
!     UPDATED         --AUGUST    1988  (PRINCIPAL COMPONENTS)
!     UPDATED         --AUGUST    1988  (... PRINCIPAL COMPONENTS)
!     UPDATED         --APRIL     1992  DEFINE D999
!     UPDATED         --JULY      1993  FOR MATRIX SOLUTION,
!                                       DETERMINANT, INVERSE, REPLACE
!                                       NUMERICAL RECIPES CODE WITH
!                                       LINPACK CODE
!     UPDATED         --JULY      1993  EIGENVALUES AND EIGENVECTORS
!                                       EXTENDED TO NON-SYMMETRIC CASE
!     UPDATED         --JULY      1993  IMPLEMENT RANK, ADJOINT,
!                                       SINGULAR VALUES, SINGULAR VALUE
!                                       DECOMP
!     UPDATED         --SEPT      1993  ROW, ELEMENT CASES
!     UPDATED         --OCTOBER   1993  CHOLESKY DECOMPOSITION, REPLACE
!                                       ROW, REPLACE ELEMENT, AUGMENT,
!                                       DIAGONAL, ADD ARGUMENT TO
!                                       MATRIX DEFINITION, TRIDIAGONAL
!                                       SOLVE.
!     UPDATED         --OCTOBER   1993  MOVE SOME OPERATIONS TO MATAR2
!     UPDATED         --DECEMBER  1994 MATRIX SUBMATRIX FOR NON-SQUARE
!                                      MATRICES
!     UPDATED         --JUNE      1995 EXTEND SPECTRAL RADIUS TO
!                                      NON-SYMMETRIC CASE
!     UPDATED         --JANUARY   1998 RECODE TO USE FEWER MATRICES
!     UPDATED         --JULY      2002 SUPPORT FOR DIFFERENT TYPES OF
!                                      COVARIANCE AND CORRELATION MATRIX
!     UPDATED         --NOVEMBER  2004 SUPPORT FOR DIFFERENT TYPES OF
!     UPDATED         --MARCH     2006 MATRIX <LOWER/UPPER> TRUNCATE
!     UPDATED         --NOVEMBER  2007 COMOVEMENT MATRIX
!     UPDATED         --SEPTEMBER 2011 MATRIX CONDITION NUMBER
!     UPDATED         --SEPTEMBER 2011 MATRIX RECIPROCAL CONDITION
!                                             NUMBER
!     UPDATED         --JUNE      2012 PARTIAL CORRELATION MATRIX
!     UPDATED         --JUNE      2012 PARTIAL CORRELATION CDF MATRIX
!     UPDATED         --JUNE      2012 PARTIAL CORRELATION PVALUE MATRIX
!     UPDATED         --JUNE      2012 CORRELATION CDF MATRIX
!     UPDATED         --JUNE      2012 CORRELATION PVALUE MATRIX
!     UPDATED         --SEPTEMBER 2016 CORRELATION ABSOLUTE VALUE
!     UPDATED         --SEPTEMBER 2016 CORRELATION PERCENTAGE VALUE
!     UPDATED         --SEPTEMBER 2016 CORRELATION DIGITS
!     UPDATED         --AUGUST    2019 CALL LIST TO KENTAU
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASZZ
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DYM1
      DOUBLE PRECISION DYM2
      DOUBLE PRECISION DYM9
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DDEL
!
      DOUBLE PRECISION DNR1
      DOUBLE PRECISION DNC1
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DSSQD
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DDEL1
      DOUBLE PRECISION DDEL2
      DOUBLE PRECISION DCOV
!CCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      DOUBLE PRECISION D999
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION YM1(MAXROM,MAXCOM)
      DIMENSION YM2(MAXROM,MAXCOM)
!CCCC DIMENSION YM3(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION YM9(MAXROM,MAXCOM)
      DIMENSION VECT9(*)
!
!CCCC DIMENSION YMJUNK(MAXROM,MAXCOM)
!CCCC DIMENSION YMJUN2(MAXROM,MAXCOM)
!CCCC JANUARY 1998.  FOLLOWINF DIMENSIONS TO MAXOBV.
!CCCC DIMENSION INDEX(MAXROM)
!CCCC DIMENSION VJUNK(MAXROM)
!CCCC DIMENSION VJUNK2(MAXROM)
!CCCC DIMENSION AINDE2(MAXROM)
!CCCC DIMENSION AINDE3(MAXROM)
!
!CCCC DIMENSION IZROV(MAXROM)
!CCCC DIMENSION IPOSV(MAXROM)
!
!CCCC DIMENSION DMEAN(MAXROM)
!CCCC DIMENSION DSSQD(MAXROM)
!
!CCCC DIMENSION INDEX(MAXOBV)
      DIMENSION INDEX(*)
!CCCC REPLACE VJUNK, VJUNK2 WITH Y3 AND Y4 BELOW (TO SAVE SPACE)
!CCCC DIMENSION VJUNK(MAXOBV)
!CCCC DIMENSION VJUNK2(MAXOBV)
!CCCC REPLACE AINDE2, AINDE3 WITH Y1 AND Y2 BELOW (TO SAVE SPACE)
!CCCC DIMENSION AINDE2(MAXOBV)
!CCCC DIMENSION AINDE3(MAXOBV)
!
!CCCC DIMENSION IZROV(MAXOBV)
!CCCC DIMENSION IPOSV(MAXOBV)
      DIMENSION IZROV(*)
      DIMENSION IPOSV(*)
!
!CCCC DIMENSION DMEAN(MAXOBV)
!CCCC DIMENSION DSSQD(MAXOBV)
      DIMENSION DMEAN(*)
      DIMENSION DSSQD(*)
!
!---------------------------------------------------------------------
!
!CCCC JULY 1993.  ADD FOLLOWING COMMON BLOCK FOR PRINCIPAL COMPONENTS
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC JULY 1993.  FOLLOWING LINE ADDED FOR RANK.
      DATA RMXINT /134217727. /
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='MATA'
      ISUBN2='RI  '
      IERROR='NO'
!
!CCCC JULY 1993.
!CCCC PCCASE='DACR'
      PCCASE=IPCMTY
      AMINOR=CPUMIN
      SCAL9=CPUMIN
      COFACT=CPUMIN
      DET=CPUMIN
!
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
!
      NRJ=(-999)
      NCJ=(-999)
!
!CCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      D999=(-999.0D0)
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GO TO 190
!
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52 FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
              (A4,2X),A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE
   53 FORMAT('IMCASE,IMSUBC,IWRITE = ',2(A4,2X),A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NUMVAR,YS1,YS2,YS3,YS4,DSSQD(1)
   55 FORMAT('NUMVAR,YS1,YS2,YS3,YS4,DSSQD(1) = ',I8,5G15.7)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)NR1,NC1
   61 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GO TO 69
      IF(NC1.LE.0)GO TO 69
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO 62 I=1,NR1
      WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   69 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NR2,NC2
   71 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 79
      IF(NC2.LE.0)GO TO 79
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO 72 I=1,NR2
      WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   79 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)NR3,NC3
   81 FORMAT('NR3,NC3 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR3.LE.0)GO TO 89
      IF(NC3.LE.0)GO TO 89
      JMAX=NC3
      IF(JMAX.GT.10)JMAX=10
      DO 82 I=1,NR3
      WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
   82 CONTINUE
   89 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)N1
  111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GO TO 119
      DO 112 I=1,N1
      WRITE(ICOUT,113)I,Y1(I)
  113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  112 CONTINUE
  119 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)N2
  121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GO TO 129
      DO 122 I=1,N2
      WRITE(ICOUT,123)I,Y2(I)
  123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  122 CONTINUE
  129 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)N3
  131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GO TO 139
      DO 132 I=1,N3
      WRITE(ICOUT,133)I,Y3(I)
  133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  132 CONTINUE
  139 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,141)N4
  141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GO TO 149
      DO 142 I=1,N4
      WRITE(ICOUT,143)I,Y4(I)
  143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
  142 CONTINUE
  149 CONTINUE
!
  190 CONTINUE
!
!               **************************************************
!               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
!               **************************************************
!
      DNR1=NR1
      DNC1=NC1
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
!               ********************************************
!
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GO TO 1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GO TO 1100
!
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GO TO 1100
!
      GO TO 1190
!
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
 1121 FORMAT('      THE MATRIX     ADDITION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAAD')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASU')WRITE(ICOUT,1122)
 1122 FORMAT('      THE MATRIX     SUBTRACTION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MASU')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAMU')WRITE(ICOUT,1123)
 1123 FORMAT('      THE MATRIX     MULTIPLICATION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAMU')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASO')WRITE(ICOUT,1124)
 1124 FORMAT('      THE MATRIX     SOLUTION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MASO')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAIN')WRITE(ICOUT,1125)
 1125 FORMAT('      THE MATRIX     INVERSE IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAIN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MATR')WRITE(ICOUT,1126)
 1126 FORMAT('      THE MATRIX     TRANSPOSE IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MATR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAAJ')WRITE(ICOUT,1127)
 1127 FORMAT('      THE MATRIX     ADJOINT IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAAJ')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACE')WRITE(ICOUT,1128)
 1128 FORMAT('      THE MATRIX CHARACTERISTIC EQUATION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MACE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEA')WRITE(ICOUT,1129)
 1129 FORMAT('      THE MATRIX     EIGENVALUES ARE TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAEA')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEE')WRITE(ICOUT,1130)
 1130 FORMAT('      THE MATRIX     EIGENVECTORS ARE TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAEE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MARA')WRITE(ICOUT,1131)
 1131 FORMAT('      THE MATRIX     RANK IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MARA')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MADE')WRITE(ICOUT,1132)
 1132 FORMAT('      THE MATRIX     DETERMINANT IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MADE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAPE')WRITE(ICOUT,1133)
 1133 FORMAT('      THE MATRIX     PERMANENT IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAPE')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASN')WRITE(ICOUT,1134)
 1134 FORMAT('      THE MATRIX     SPECTRAL NORM IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MASN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASR')WRITE(ICOUT,1135)
 1135 FORMAT('      THE MATRIX     SPECTRAL RADIUS IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MASR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANR')WRITE(ICOUT,1136)
 1136 FORMAT('      THE MATRIX     NUMBER OF ROWS IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MANR')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANC')WRITE(ICOUT,1137)
 1137 FORMAT('      THE MATRIX     NUMBER OF COLUMNS IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MANC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MANC')WRITE(ICOUT,1138)
 1138 FORMAT('      THE MATRIX     SIMPLEX SOLUTION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MANC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MATC')WRITE(ICOUT,1141)
 1141 FORMAT('      THE MATRIX     TRACE IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MATC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MASM')WRITE(ICOUT,1142)
 1142 FORMAT('      THE MATRIX     SUBMATRIX IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MASM')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAMI')WRITE(ICOUT,1143)
 1143 FORMAT('      THE MATRIX     MINOR IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAMI')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACF')WRITE(ICOUT,1144)
 1144 FORMAT('      THE MATRIX     COFACTOR IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MACF')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MADF')WRITE(ICOUT,1145)
 1145 FORMAT('      THE MATRIX     DEFINITION IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MADF')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAEN')WRITE(ICOUT,1146)
 1146 FORMAT('      THE MATRIX     EUCLIDEAN NORM IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAEN')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAVC')WRITE(ICOUT,1151)
 1151 FORMAT('      THE VARIANCE-COVARIANCE MATRIX IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAVC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MACO')WRITE(ICOUT,1152)
 1152 FORMAT('      THE CORRELATION MATRIX IS TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MACO')CALL DPWRST('XXX','BUG ')
      IF(IMCASE.EQ.'MAPC')WRITE(ICOUT,1153)
 1153 FORMAT('      THE PRINCIPAL COMPONENTS ARE TO BE ',   &
      'COMPUTED')
      IF(IMCASE.EQ.'MAPC')CALL DPWRST('XXX','BUG ')
      IF(IMCASE(1:3).EQ.'MAP'.AND.IMCASE(4:4).NE.'C')THEN
         WRITE(ICOUT,1154)
 1154    FORMAT('      THE ... PRINCIPAL COMPONENT TO BE ',   &
         'COMPUTED')
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
      GO TO 9000
!
 1190 CONTINUE
!
!               *********************************
!               **  STEP 12--                  **
!               **  BRANCH TO THE PROPER CASE  **
!               *********************************
!
      IF(IMCASE.EQ.'MAAD')GO TO 2100
      IF(IMCASE.EQ.'MASU')GO TO 2200
      IF(IMCASE.EQ.'MAMU')GO TO 2300
      IF(IMCASE.EQ.'MASO')GO TO 2400
      IF(IMCASE.EQ.'MAIN')GO TO 2500
      IF(IMCASE.EQ.'MACN')GO TO 2560
      IF(IMCASE.EQ.'MARC')GO TO 2560
      IF(IMCASE.EQ.'MATR')GO TO 2600
      IF(IMCASE.EQ.'MAAJ')GO TO 2700
      IF(IMCASE.EQ.'MACE')GO TO 2800
      IF(IMCASE.EQ.'MAEA')GO TO 2900
      IF(IMCASE.EQ.'MAEE')GO TO 3000
      IF(IMCASE.EQ.'MARA')GO TO 3100
      IF(IMCASE.EQ.'MADE')GO TO 3200
      IF(IMCASE.EQ.'MAPE')GO TO 3300
      IF(IMCASE.EQ.'MASN')GO TO 3400
      IF(IMCASE.EQ.'MASR')GO TO 3500
      IF(IMCASE.EQ.'MANR')GO TO 3600
      IF(IMCASE.EQ.'MANC')GO TO 3700
      IF(IMCASE.EQ.'MASS')GO TO 3800
      IF(IMCASE.EQ.'MATC')GO TO 4100
      IF(IMCASE.EQ.'MASM')GO TO 4200
      IF(IMCASE.EQ.'MAMI')GO TO 4300
      IF(IMCASE.EQ.'MACF')GO TO 4400
      IF(IMCASE.EQ.'MADF')GO TO 4500
      IF(IMCASE.EQ.'MAEN')GO TO 4600
      IF(IMCASE.EQ.'MAVC')GO TO 5100
      IF(IMCASE.EQ.'MACO')GO TO 5200
      IF(IMCASE.EQ.'MACC')GO TO 5200
      IF(IMCASE.EQ.'MACP')GO TO 5200
!
      IF(IMCASE.EQ.'MAPC')GO TO 5300
      IF(IMCASE.EQ.'MAP1')GO TO 5300
      IF(IMCASE.EQ.'MAP2')GO TO 5300
      IF(IMCASE.EQ.'MAP3')GO TO 5300
      IF(IMCASE.EQ.'MAP4')GO TO 5300
      IF(IMCASE.EQ.'MAP5')GO TO 5300
      IF(IMCASE.EQ.'MAP6')GO TO 5300
      IF(IMCASE.EQ.'MAP7')GO TO 5300
      IF(IMCASE.EQ.'MAP8')GO TO 5300
      IF(IMCASE.EQ.'MAP9')GO TO 5300
      IF(IMCASE.EQ.'MA10')GO TO 5300
!CCCCC OCTOBER 1993.  FOLLOWING OPERATIONS MOVED TO MATAR2
!CCCC JULY 1993.  ADD FOLLOWING 3 LINES
!CCCC IF(IMCASE.EQ.'MASV')GO TO 5800
!CCCC IF(IMCASE.EQ.'MASD')GO TO 5900
!CCCC IF(IMCASE.EQ.'MASF')GO TO 6000
!CCCC SEPTEMBER 1993.  ADD FOLLOWING 2 LINES
!CCCC IF(IMCASE.EQ.'MARW')GO TO 6100
!CCCC IF(IMCASE.EQ.'MAEL')GO TO 6200
!CCCC OCTOBER 1993.  ADD FOLLOWING LINE
!CCCC IF(IMCASE.EQ.'MACH')GO TO 6300
!CCCC IF(IMCASE.EQ.'MAAU')GO TO 6400
!CCCC IF(IMCASE.EQ.'MADI')GO TO 6500
!CCCC IF(IMCASE.EQ.'DIMA')GO TO 6600
!CCCC IF(IMCASE.EQ.'MARR')GO TO 6700
!CCCC IF(IMCASE.EQ.'MARE')GO TO 6800
!CCCC IF(IMCASE.EQ.'MATD')GO TO 6900
!CCCC IF(IMCASE.EQ.'MATS')GO TO 7000
!CCCC IF(IMCASE.EQ.'MATI')GO TO 7100
!CCCC IF(IMCASE.EQ.'MAIS')GO TO 7200
!
      IF(IMCASE.EQ.'MATZ')GO TO 6100
      IF(IMCASE.EQ.'MAUZ')GO TO 6200
      IF(IMCASE.EQ.'MACM')GO TO 6300
      IF(IMCASE.EQ.'MPCO')GO TO 6400
      IF(IMCASE.EQ.'MPCC')GO TO 6400
      IF(IMCASE.EQ.'MPCP')GO TO 6400
!
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
      GO TO 9000
!
!               *********************************************
!               **  STEP 21--                              **
!               **  TREAT THE MATRIX     ADDITION    CASE  **
!               *********************************************
!
 2100 CONTINUE
!
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GO TO 2110
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GO TO 2130
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GO TO 2150
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GO TO 2170
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GO TO 2180
!
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
      GO TO 9000
!
 2110 CONTINUE
      IF(NR1.EQ.NR2.AND.NC1.EQ.NC2)GO TO 2119
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
      GO TO 9000
 2119 CONTINUE
!
      DO 2121 I=1,NR1
      DO 2122 J=1,NC1
      DYM1=YM1(I,J)
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2122 CONTINUE
 2121 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      GO TO 9000
!
 2130 CONTINUE
      IF(NR1.EQ.N2)GO TO 2139
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
      GO TO 9000
 2139 CONTINUE
!
      DO 2141 I=1,NR1
      DYM2=Y2(I)
      DO 2142 J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2142 CONTINUE
 2141 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 2150 CONTINUE
      IF(N1.EQ.NR2)GO TO 2159
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
      GO TO 9000
 2159 CONTINUE
!
      DO 2161 I=1,NR2
      DYM1=Y1(I)
      DO 2162 J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2162 CONTINUE
 2161 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      GO TO 9000
!
 2170 CONTINUE
      DYM2=YS2
      DO 2171 I=1,NR1
      DO 2172 J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2172 CONTINUE
 2171 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 2180 CONTINUE
      DYM1=YS1
      DO 2181 I=1,NR2
      DO 2182 J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1+DYM2
      YM9(I,J)=DYM9
 2182 CONTINUE
 2181 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GO TO 9000
!
!               *********************************************
!               **  STEP 22--                              **
!               **  TREAT THE MATRIX     SUBTRACTION CASE  **
!               *********************************************
!
 2200 CONTINUE
!
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GO TO 2210
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GO TO 2230
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GO TO 2250
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GO TO 2270
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GO TO 2280
!
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
      GO TO 9000
!
 2210 CONTINUE
      IF(NR1.EQ.NR2.AND.NC1.EQ.NC2)GO TO 2219
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
      GO TO 9000
 2219 CONTINUE
!
!
      DO 2221 I=1,NR1
      DO 2222 J=1,NC1
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
      GO TO 9000
!
 2230 CONTINUE
      IF(NR1.EQ.N2)GO TO 2239
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
      GO TO 9000
 2239 CONTINUE
!
      DO 2241 I=1,NR1
      DYM2=Y2(I)
      DO 2242 J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2242 CONTINUE
 2241 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 2250 CONTINUE
      IF(N1.EQ.NR2)GO TO 2259
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
      GO TO 9000
 2259 CONTINUE
!
      DO 2261 I=1,NR2
      DYM1=Y1(I)
      DO 2262 J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2262 CONTINUE
 2261 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GO TO 9000
!
 2270 CONTINUE
      DYM2=YS2
      DO 2271 I=1,NR1
      DO 2272 J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2272 CONTINUE
 2271 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 2280 CONTINUE
      DYM1=YS1
      DO 2281 I=1,NR2
      DO 2282 J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1-DYM2
      YM9(I,J)=DYM9
 2282 CONTINUE
 2281 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='SUBS'
      GO TO 9000
!
!               *********************************************
!               **  STEP 23--                              **
!               **  TREAT THE MATRIX  MULTIPLICATION CASE  **
!               *********************************************
!
 2300 CONTINUE
!
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'MATR')GO TO 2310
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'VARI')GO TO 2330
      IF(ITYPA1.EQ.'VARI'.AND.ITYPA2.EQ.'MATR')GO TO 2350
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GO TO 2370
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GO TO 2380
!
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
      GO TO 9000
!
 2310 CONTINUE
      IF(NC1.EQ.NR2)GO TO 2319
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
      GO TO 9000
 2319 CONTINUE
!
      DO 2321 I=1,NR1
      DO 2322 J=1,NC2
      DSUM=0.0D0
      DO 2323 K=1,NC1
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
      GO TO 9000
!
 2330 CONTINUE
      IF(NC1.EQ.N2)GO TO 2339
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
      GO TO 9000
 2339 CONTINUE
!
      DO 2341 I=1,NR1
        J=1
        DSUM=0.0D0
        DO 2343 K=1,NC1
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
      GO TO 9000
!
 2350 CONTINUE
      IF(1.EQ.NR2)GO TO 2359
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
      GO TO 9000
 2359 CONTINUE
!
      DO 2361 I=1,NR1
        DO 2362 J=1,NC2
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
      GO TO 9000
!
 2370 CONTINUE
      DYM2=YS2
      DO 2371 I=1,NR1
      DO 2372 J=1,NC1
      DYM1=YM1(I,J)
      DYM9=DYM1*DYM2
      YM9(I,J)=DYM9
 2372 CONTINUE
 2371 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
 2380 CONTINUE
      DYM1=YS1
      DO 2381 I=1,NR2
      DO 2382 J=1,NC2
      DYM2=YM2(I,J)
      DYM9=DYM1*DYM2
      YM9(I,J)=DYM9
 2382 CONTINUE
 2381 CONTINUE
      ITYP9='MATR'
      NR9=NR2
      NC9=NC2
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 24--                              **
!               **  TREAT THE MATRIX     SOLUTION    CASE  **
!               **  REFERENCE--PRESS ET AL, PAGE 37        **
!               *********************************************
!
 2400 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 2409
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
      GO TO 9000
 2409 CONTINUE
!
      DO 2451 I=1,N2
!CCCC VECT9(I)=YM2(1,I)
!CCCC VECT9(I)=YM2(I,1)
      VECT9(I)=Y2(I)
 2451 CONTINUE
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK
!CCCC ALGORITHM.
!CCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
!CCCC CALL LUBKSB(YMJUNK,NR1,MAXROM,INDEX,VECT9)
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
!CCCC END CHANGE
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 25--                              **
!               **  TREAT THE MATRIX     INVERSE     CASE  **
!               **  REFERENCE--PRESS ET AL, PAGE 38        **
!               *********************************************
!
 2500 CONTINUE
!
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
 2503   FORMAT('      MATRIX MUST EQUAL THE NUMBER OF COLUMNS IN THE ',   &
               'MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2504)NR1
 2504   FORMAT('            NUMBER OF ROWS    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2505)NC1
 2505   FORMAT('            NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 2511 I=1,NR1
        DO 2512 J=1,NC1
          YM9(I,J)=0.0
 2512   CONTINUE
        YM9(I,I)=1.0
 2511 CONTINUE
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK
!CCCC ALGORITHM.
!
!CCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
!
!CCCC DO2521J=1,NR1
!CCCC CALL LUBKSB(YMJUNK,NR1,MAXROM,INDEX,YM9(1,J))
!2521 CONTINUE
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
        DO 2531 J=1,NC1
          DO 2532 I=1,NR1
            YM9(I,J)=YM1(I,J)
 2532     CONTINUE
 2531   CONTINUE
      END IF
!CCCC END CHANGE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 25B-                              **
!               **  TREAT THE MATRIX CONDITION NUMBER CASE **
!               *********************************************
!
 2560 CONTINUE
!
      IF(NR1.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2561)
 2561   FORMAT('***** ERROR IN MATRIX CONDITION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2562)
 2562   FORMAT('      FOR MATRIX CONDITION NUMBER, THE NUMBER OF ',   &
               'ROWS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2563)
 2563   FORMAT('      MATRIX MUST EQUAL THE NUMBER OF COLUMNS IN THE ',   &
               'MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2564)NR1
 2564   FORMAT('            NUMBER OF ROWS    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2565)NC1
 2565   FORMAT('            NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL SGECO(YM1,MAXROM,NR1,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2521)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
!
      ITYP9='SCAL'
      SCAL9=RCOND
      IF(IMCASE.EQ.'MACN')SCAL9=1.0/RCOND
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 26--                              **
!               **  TREAT THE MATRIX     TRANSPOSE   CASE  **
!               *********************************************
!
 2600 CONTINUE
!
      IF(NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2601)
 2601   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2603)NR1
 2603   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX,',I5,   &
               'EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2605)MAXCOM
 2605   FORMAT('      NUMBER OF COLUMNS FOR A MATRIX,',I5,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2607)
 2607   FORMAT('      THE MATRIX TRANSPOSE WAS NOT COMPUTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 2611 I=1,NR1
      DO 2612 J=1,NC1
      YM9(J,I)=YM1(I,J)
 2612 CONTINUE
 2611 CONTINUE
!
      ITYP9='MATR'
      NR9=NC1
      NC9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 27--                              **
!               **  TREAT THE MATRIX     ADJOINT     CASE  **
!               *********************************************
!CCCC JULY 1993.  IMPLENENT THIS COMMAND.  NOTE THAT THE CLASSICAL
!CCCC ADJOINT IS ESSENTIALLY THE MATRIX CONTAINING THE COFACTORS
!CCCC FOR EACH ELEMENT.  THIS CALCULATES THE DETERMINANT AT
!CCCC EACH MATRIX SUB-ELEMENT, SO CAN GET TIME-CONSUMING FOR LARGE
!CCCC MATRICES.
!
 2700 CONTINUE
!
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2711)
!2711 FORMAT('***** ERROR IN MATARI--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2712)
!2712 FORMAT('      THE MATRIX ADJOINT COMMAND')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2713)
!2713 FORMAT('      IS NOT YET IMPLEMENTED.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!
      IF(NR1.EQ.NC1)GO TO 2709
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
      GO TO 9000
 2709 CONTINUE
!
      DO 2790 IROWID=1,NR1
      IYS2=IROWID
      DO 2780 ICOLID=1,NC1
      IYS3=ICOLID
      I2=0
      J2=0
      DO 2711 I=1,NR1
      IF(I.EQ.IYS2)GO TO 2711
      I2=I2+1
      NRJ=I2
      J2=0
      DO 2712 J=1,NC1
      IF(J.EQ.IYS3)GO TO 2712
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 2712 CONTINUE
 2711 CONTINUE
!
      IF(NRJ.GE.1.AND.NCJ.GE.1)GO TO 2729
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
      GO TO 9000
 2729 CONTINUE
!
      IF(NRJ.EQ.NCJ)GO TO 2739
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
      GO TO 9000
 2739 CONTINUE
!
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
!CCCC END CHANGE
!
      YM9(IROWID,ICOLID)=COFACT
 2780 CONTINUE
 2790 CONTINUE
!
      ITYP9='MATR'
      NC9=NR1
      NR9=NR1
      SCAL9=COFACT
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 28--                                        **
!               **  TREAT THE MATRIX CHARACTERISTIC EQUATION   CASE  **
!               *******************************************************
!
 2800 CONTINUE
!
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
      GO TO 9000
!
!
!               *********************************************
!               **  STEP 29--                              **
!               **  TREAT THE MATRIX     EIGENVALUES CASE  **
!               *********************************************
!
 2900 CONTINUE
!
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
 2905   FORMAT('      NUMBER OF COLUMNS IN THE MATRIX;  SUCH WAS NOT ',   &
               'THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2907)NR1
 2907   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2908)NC1
 2908   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 2911 I=1,NR1
        I2=I
        DO 2912 J=I,NC1
          J2=J
          YM1IJ=YM1(I,J)
          YM1JI=YM1(J,I)
          IF(YM1IJ.NE.YM1JI)GO TO 2930
 2912   CONTINUE
 2911 CONTINUE
      GO TO 2939
!
!CCCC JULY 1993.  ADD SUPPORT FOR NON-SYMMETRIC CASE.  THIS CASE
!CCCC CAN HAVE COMPLEX EIGENVALUES.  ROWS 1 THROUGH N OF THE OUTPUT
!CCCC VECTOR WILL CONTAIN THE REAL COMPONENT, ROWS N+1 THROUGH 2*N
!CCCC WILL CONTAIN THE COMPLEX COMPONENT.
 2930 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2931)
!2931 FORMAT('***** ERROR IN MATARI--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2932)
!2932 FORMAT('      FOR MATRIX EIGENVALUES,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2933)
!2933 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2935)
!2935 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2936)
!2936 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2937)I2,J2,YM1IJ
!2937 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2938)J2,I2,YM1JI
!2938 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!
      IERR2=0
      IJOB=0
      CALL SGEEV(YM1,MAXROM,NR1,VECT9,YM2,MAXROM,Y3,   &
      IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,2941)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2942)
        CALL DPWRST('XXX','BUG ')
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,2941)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2947)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2948)IERR2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2949)IERR2-1
        CALL DPWRST('XXX','BUG ')
      END IF
 2941 FORMAT('******** ERROR FROM MATRIX EIGENVALUES--')
 2942 FORMAT('         PROBLEM WITH MATRIX DIMENSIONS')
 2947 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 2948 FORMAT('         FOR EIGENVALUE ',I4)
 2949 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGE
!
      ITYP9='VECT'
      NVECT9=2*NR1
      GO TO 9000
!CCCC END CHANGES
 2939 CONTINUE
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
!CCCC ALGORITHM.
!CCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VECT9,YMJUN2,NJACIT)
!
      IERR2=0
      IJOB=0
      CALL SSIEV(YM1,MAXROM,NR1,VECT9,Y3,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2962)
        CALL DPWRST('XXX','BUG ')
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2963)
        CALL DPWRST('XXX','BUG ')
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,2961)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2967)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2968)IERR2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2969)IERR2-1
        CALL DPWRST('XXX','BUG ')
      END IF
 2961 FORMAT('******** ERROR FROM MATARI ************')
 2962 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 2963 FORMAT('         LESS THAN 1 ROW')
 2967 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 2968 FORMAT('         FOR EIGENVALUE ',I4)
 2969 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGE
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 30--                              **
!               **  TREAT THE MATRIX    EIGENVECTORS CASE  **
!               *********************************************
!
 3000 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
      DO 3011 I=1,NR1
        I2=I
        DO 3012 J=I,NC1
          J2=J
          YM1IJ=YM1(I,J)
          YM1JI=YM1(J,I)
          IF(YM1IJ.NE.YM1JI)GO TO 3030
 3012   CONTINUE
 3011 CONTINUE
      GO TO 3039
!
 3030 CONTINUE
!CCCC JULY 1993.  ADD SUPPORT FOR NON-SYMMETRIC CASE.  THIS CASE
!CCCC CAN HAVE COMPLEX EIGENVECTORS.  ROWS 1 THROUGH N OF THE OUTPUT
!CCCC MATRIX WILL CONTAIN THE REAL COMPONENT, ROWS N+1 THROUGH 2*N
!CCCC WILL CONTAIN THE COMPLEX COMPONENT.
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3031)
!3031 FORMAT('***** ERROR IN MATARI--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3032)
!3032 FORMAT('      FOR MATRIX EIGENVECTORS,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3033)
!3033 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3035)
!3035 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3036)
!3036 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3037)I2,J2,YM1IJ
!3037 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3038)J2,I2,YM1JI
!3038 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!
      IERR2=0
      IJOB=1
      DO 3021 J=1,MAXCOM
        DO 3022 I=1,MAXROM
          IF(J.GT.NR1 .OR. I.GT.NR1)YM1(I,J)=0.0
          YM2(I,J)=0.0
          YM9(I,J)=0.0
 3022   CONTINUE
 3021 CONTINUE
      DO 3023 I=1,MAXOBV
        VECT9(I)=0.0
        Y3(I)=0.0
 3023 CONTINUE
!
      CALL SGEEV(YM1,MAXROM,NR1,VECT9,YM2,MAXROM,Y3,   &
                 IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3042)
        CALL DPWRST('XXX','BUG ')
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3047)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3048)IERR2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3049)IERR2-1
        CALL DPWRST('XXX','BUG ')
      ELSE
        DO 3045 J=1,NR1
          DO 3044 I=1,2*NR1
            YM9(I,J)=YM2(I,J)
 3044     CONTINUE
 3045   CONTINUE
      END IF
 3042 FORMAT('         PROBLEM WITH MATRIX DIMENSIONS')
 3047 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3048 FORMAT('         FOR EIGENVALUE ',I4)
 3049 FORMAT('         EIGENVECTORS 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGE
!
      ITYP9='MATR'
      NR9=2*NR1
      NC9=NC1
      IUPFLG='FULL'
!CCCC END CHANGES
      GO TO 9000
 3039 CONTINUE
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
!CCCC ALGORITHM.
!CCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VJUNK,YM9,NJACIT)
!
      IERR2=0
      IJOB=1
      DO 3071 I=1,MAXOBV
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
        DO 3080 J=1,NR1
        DO 3082 I=1,NR1
        YM9(I,J)=YM1(I,J)
 3082   CONTINUE
 3080   CONTINUE
      END IF
 3062 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3063 FORMAT('         LESS THAN 1 ROW')
 3067 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3068 FORMAT('         FOR EIGENVALUE ',I4)
 3069 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 31--                                 **
!               **  TREAT THE MATRIX     RANK        CASE     **
!               **  COMPUTE FROM SINGULAR VALUE DECOMPOSITION **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 3100 CONTINUE
!
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y3,YM1,MAXROM,   &
      YM1,MAXROM,Y4,IJOB,IERR2)
      ARANK=0.
      IF(ITYPA2.EQ.'PARA')THEN
        ATOL=YS2
      ELSE
!CCCC   ATOL=0.0000001
        CALL SPDIV(RMXINT,2.0,IND,RESULT)
        ETA=RESULT+1.0
        CALL SPDIV(1.0,ETA,IND,ETA)
        ATOL=REAL(MAX(NR1,NC1))*VECT9(1)*ETA
      ENDIF
      NLAST=MIN(NR1,NC1)
      DO 3120 I=1,NLAST
      IF(VECT9(I).LE.ATOL)THEN
        ARANK=REAL(I-1)
        GO TO 3129
      ENDIF
 3120 CONTINUE
      ARANK=REAL(NLAST)
 3129 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GO TO 3190
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3151)
 3151 FORMAT('***** COMPUTING RANK--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3152)EPS,VECT9(1),ATOL
 3152 FORMAT('EPS,VECT((1),ATOL = ',   &
      E15.7,2X,E15.7,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      NLAST=MIN(NR1+1,NC1)
      DO 3180 I=1,NLAST
      WRITE(ICOUT,3183)I,VECT9(I)
 3183 FORMAT('I,VECT9(I) = ',I4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 3180 CONTINUE
!
 3190 CONTINUE
!
!CCCC END CHANGE
!
      ITYP9='SCAL'
      SCAL9=ARANK
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 32--                              **
!               **  TREAT THE MATRIX     DETERMINANT CASE  **
!               **  REFERENCE--PRESS ET AL, PAGE 39        **
!               *********************************************
!
 3200 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 3209
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
      GO TO 9000
 3209 CONTINUE
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH LINPACK
!CCCC ALGORITHM.
!CCCC CALL LUDCMP(YMJUNK,NR1,MAXROM,INDEX,DP1M1)
!
!CCCC DET=DP1M1
!CCCC DO3221I=1,NR1
!CCCC DET=DET*YMJUNK(I,I)
!3221 CONTINUE
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
!CCCC END CHANGE
!
      ITYP9='SCAL'
      SCAL9=DET
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 33--                              **
!               **  TREAT THE MATRIX     PERMANENT   CASE  **
!               *********************************************
!
 3300 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      CALL PERMAN(YM1,MAXROM,NR1,INDEX,Y3,APERM)
!
      ITYP9='SCAL'
      SCAL9=APERM
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 34--                                        **
!               **  TREAT THE MATRIX     SPECTRAL NORM    CASE       **
!               **  SPECTRAL NORM = COMPUTE MATRIX TIMES ITS         **
!               **                  TRANSPOSE, THEN FIND THE SQUARE  **
!               **                  ROOT OF THE EIGENVALUE WITH THE  **
!               **                  LARGEST ABSOLUTE VALUE.          **
!               **  REFERENCE--RALSTON                               **
!               *******************************************************
!
 3400 CONTINUE
!
      IF(NR1.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3411)
 3411   FORMAT('***** ERROR IN MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3413)NR1
 3413   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX,',I5,   &
               'EXCEEDS THE MAXIMUM')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3415)MAXCOM
 3415   FORMAT('      NUMBER OF COLUMNS FOR A MATRIX,',I5,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3417)
 3417   FORMAT('      THE MATRIX TRANSPOSE WAS NOT COMPUTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 3421 I=1,NR1
      DO 3422 J=1,NR1
      DSUM=0.0D0
      DO 3423 K=1,NC1
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
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH THE EISPACK
!CCCC ALGORITHM.  NOTE THAT MATRIX TIMES IT TRANSPOSE IS SYMMETRIC, SO
!CCCC USE SYMMERIC VERSION.
!CCCC CALL JACOBI(YMJUNK,NRJ,MAXROM,VJUNK,YMJUN2,NJACIT)
!
      IERR2=0
      IJOB=0
      CALL SSIEV(YM2,MAXROM,NR1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3452)
        GO TO 9000
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3453)
        GO TO 9000
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3451)
        WRITE(ICOUT,3457)
        WRITE(ICOUT,3458)IERR2
        WRITE(ICOUT,3459)IERR2-1
        GO TO 9000
      END IF
 3451 FORMAT('******** ERROR FROM MATARI ************')
 3452 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3453 FORMAT('         LESS THAN 1 ROW')
 3457 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3458 FORMAT('         FOR EIGENVALUE ',I4)
 3459 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGES
      AMAX=ABS(Y3(1))
      DO 3461 I=1,NR1
      IF(ABS(Y3(I)).GT.AMAX)AMAX=ABS(Y3(I))
 3461 CONTINUE
      AMAX2=0.0
      IF(AMAX.GT.0.0)AMAX2=SQRT(AMAX)
!
      ITYP9='SCAL'
      SCAL9=AMAX2
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 35--                                        **
!               **  TREAT THE MATRIX     SPECTRAL RADIUS    CASE     **
!               **  SPECTRAL RADIUS = LARGEST ABS(EIGENVALUE) OF A   **
!               **  REFERENCE--RALSTON                               **
!               *******************************************************
!
 3500 CONTINUE
!
!CCCC JUNE 1995.  EISPACK WILL HANDLE NON-SYMMETRIC MATRICES (FOR
!CCCC EIGENVALUES).  NO NEED TO RESTRICT TO SYMMETRIC MATRICES).
!CCCC IF(NR1.EQ.NC1)GO TO 3509
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3501)
!3501 FORMAT('***** ERROR IN MATARI--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3502)
!3502 FORMAT('      FOR MATRIX SPECTRAL RADIUS,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3503)
!3503 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3504)
!3504 FORMAT('      MUST EQUAL')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3505)
!3505 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3506)
!3506 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3507)NR1
!3507 FORMAT('            NUMBER OF ROWS    =',I8)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3508)NC1
!3508 FORMAT('            NUMBER OF COLUMNS =',I8)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!
      DO 3511 I=1,NR1
      I2=I
      DO 3512 J=I,NC1
      J2=J
      YM1IJ=YM1(I,J)
      YM1JI=YM1(J,I)
      IF(YM1IJ.EQ.YM1JI)GO TO 3512
      GO TO 3530
 3512 CONTINUE
 3511 CONTINUE
      GO TO 3539
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
!CCCC EISPACK CAN HANDLE NON-SYMMETRIC MATRICES.
 3530 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3531)
!3531 FORMAT('***** ERROR IN MATARI--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3532)
!3532 FORMAT('      FOR MATRIX SPECTRAL RADIUS,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3533)
!3533 FORMAT('      THE MATRIX MUST BE SYMMETRIC')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3535)
!3535 FORMAT('      ( A(I,J) = A(J,I) FOR ALL I AND J ).')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3536)
!3536 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3537)I2,J2,YM1IJ
!3537 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3538)J2,I2,YM1JI
!3538 FORMAT('            ELEMENT',I8,',',I8,' = ',E15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!
      IERR2=0
      IJOB=0
      CALL SGEEV(YM1,MAXROM,NR1,Y3,YM2,MAXROM,Y4,   &
      IJOB,IERR2)
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
!
!  COMPLEX ABSOLUTE VALUE IS DEFINED TO BE: SQRT(REAL**2+COMPLEX**2)
!
      AMAX=0.0
      ATEMP1=Y3(1)**2 + Y3(1+NR1)**2
      IF(ATEMP1.GE.0.0)AMAX=SQRT(ATEMP1)
      DO 3538 I=1,NR1
      ATEMP1=0.0
      ATEMP2=Y3(I)**2 + Y3(I+NR1)**2
      IF(ATEMP2.GE.0.0)ATEMP1=SQRT(ATEMP2)
      IF(ATEMP1.GT.AMAX)AMAX=ATEMP1
 3538 CONTINUE
      GO TO 3599
!
!CCCC END CHANGE
 3539 CONTINUE
!
!CCCC JULY 1993.  REPLACE NUMERICAL RECIPES ALGORITHM WITH EISPACK
!CCCC CALL JACOBI(YMJUNK,NR1,MAXROM,VJUNK,YMJUN2,NJACIT)
!
      IERR2=0
      IJOB=0
      CALL SSIEV(YM1,MAXROM,NR1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.EQ.-1)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3562)
        GO TO 9000
      ELSE IF(IERR2.EQ.-2)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3563)
        GO TO 9000
      ELSE IF(IERR2.GT.0)THEN
        IERROR='YES'
        WRITE(ICOUT,3561)
        WRITE(ICOUT,3567)
        WRITE(ICOUT,3568)IERR2
        WRITE(ICOUT,3569)IERR2-1
        GO TO 9000
      END IF
 3561 FORMAT('******** ERROR FROM MATARI ************')
 3562 FORMAT('         THE NUMBER OF ROWS GREATER THAN MAXIMUM')
 3563 FORMAT('         LESS THAN 1 ROW')
 3567 FORMAT('         THE EIGENVALUE ALGORITHM FAILED TO CONVERGE ')
 3568 FORMAT('         FOR EIGENVALUE ',I4)
 3569 FORMAT('         EIGENVALUES 1 THRU ',I4,' ARE CORRECT')
!CCCC END CHANGES
!
      AMAX=ABS(Y3(1))
      DO 3591 I=1,NR1
      IF(ABS(Y3(I)).GT.AMAX)AMAX=ABS(Y3(I))
 3591 CONTINUE
!
 3599 CONTINUE
      ITYP9='SCAL'
      SCAL9=AMAX
      IUPFLG='FULL'
      GO TO 9000
!
!               ***************************************************
!               **  STEP 36--                                    **
!               **  TREAT THE MATRIX     NUMBER OF ROWS    CASE  **
!               ***************************************************
!
 3600 CONTINUE
!
      SCAL9=NR1
!
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               ***************************************************
!               **  STEP 37--                                    **
!               **  TREAT THE MATRIX     NUMBER OF COLUMNS CASE  **
!               ***************************************************
!
 3700 CONTINUE
!
      SCAL9=NC1
!
      ITYP9='SCAL'
!
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 38--                                      **
!               **  TREAT THE MATRIX     SIMPLEX SOLUTION    CASE  **
!               **  REFERENCE--PRESS ET AL, PAGE 322               **
!               *****************************************************
!
 3800 CONTINUE
!
      NC2M2=NC2-2
!
      IF(N1.EQ.NC2M2)GO TO 3809
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
      GO TO 9000
 3809 CONTINUE
!
      IF(1.LE.N1.AND.N1.LE.MAXCOM)GO TO 3819
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
      GO TO 9000
 3819 CONTINUE
!
      IF(1.LE.NR2.AND.NR2.LE.MAXROM)GO TO 3829
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
      GO TO 9000
 3829 CONTINUE
!
      IF(3.LE.NC2.AND.NC2.LE.MAXCOM)GO TO 3839
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
      GO TO 9000
 3839 CONTINUE
!
      EPS=0.000001
      NR2P1=NR2+1
      NC2P=NC2-2
      NC2PP1=NC2P+1
      NC2M1=NC2-1
!
      DO 3850 J=1,NC2PP1
      YM9(1,J)=0.0
 3850 CONTINUE
!
      N1P1=N1+1
      DO 3860 J=2,N1P1
      JM1=J-1
      YM9(1,J)=Y1(JM1)
 3860 CONTINUE
!
      K=1
      DO 3871 ILOOP=1,3
!
      DO 3872 I=2,NR2P1
      IM1=I-1
      YTARG=YM2(IM1,NC2M1)
      IF(ILOOP.EQ.1.AND.YTARG.LT.-EPS)GO TO 3873
      IF(ILOOP.EQ.1)GO TO 3872
      IF(ILOOP.EQ.2.AND.EPS.LT.YTARG)GO TO 3873
      IF(ILOOP.EQ.2)GO TO 3872
      IF(ILOOP.EQ.3.AND.-EPS.LE.YTARG.AND.   &
                        YTARG.LE.EPS)GO TO 3873
      IF(ILOOP.EQ.3)GO TO 3872
 3873 CONTINUE
      K=K+1
!
      YM9(K,1)=YM2(IM1,NC2)
      DO 3874 J=2,NC2PP1
      JM1=J-1
      YM9(K,J)=(-YM2(IM1,JM1))
 3874 CONTINUE
!
 3872 CONTINUE
!
 3871 CONTINUE
!
      NLTZ=0
      NGTZ=0
      NEQZ=0
      DO 3877 I=1,NR2
      YTARG=YM2(I,NC2M1)
      IF(YTARG.LT.-EPS)NLTZ=NLTZ+1
      IF(EPS.LT.YTARG)NGTZ=NGTZ+1
      IF(-EPS.LE.YTARG.AND.YTARG.LE.EPS)NEQZ=NEQZ+1
 3877 CONTINUE
!
      CALL SIMPLX(YM9,NR2,NC2P,MAXROM,MAXCOM,NLTZ,NGTZ,NEQZ,   &
      ICASE,IZROV,IPOSV,IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DO 3881 I=1,N1
      VECT9(I)=0.0
 3881 CONTINUE
!
      DO 3882 I=1,NR2
      INDEX2=IPOSV(I)
      IP1=I+1
      IF(INDEX2.LE.N1)VECT9(INDEX2)=YM9(IP1,1)
 3882 CONTINUE
!
      ITYP9='VECT'
      NVECT9=N1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 41--                                      **
!               **  TREAT THE MATRIX     TRACE               CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
 4100 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 4109
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
      GO TO 9000
 4109 CONTINUE
!
      DSUM1=0.0D0
      DO 4111 I=1,NR1
      DYM1=YM1(I,I)
      DSUM1=DSUM1+DYM1
 4111 CONTINUE
!
      ITYP9='SCAL'
      SCAL9=DSUM1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 42--                                      **
!               **  TREAT THE MATRIX     SUBMATRIX           CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
 4200 CONTINUE
!
!CCCC  NO REASON FOR RESTRICTION ON SQUARE MATRICES FOR THIS
!CCCC  COMMAND.  COMMENT OUT FOLLOWING SECTION.   DECEMBER 1994.
!CCCC  IF(NR1.EQ.NC1)GO TO 4209
!CCCC  WRITE(ICOUT,999)
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4201)
!4201 FORMAT('***** ERROR IN MATARI--')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4202)
!4202 FORMAT('      FOR MATRIX SUBMATRIX,')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4203)
!4203 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4204)
!4204 FORMAT('      MUST EQUAL')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4205)
!4205 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX;')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4206)
!4206 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4207)NR1
!4207 FORMAT('            NUMBER OF ROWS    =',I8)
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  WRITE(ICOUT,4208)NC1
!4208 FORMAT('            NUMBER OF COLUMNS =',I8)
!CCCC  CALL DPWRST('XXX','BUG ')
!CCCC  IERROR='YES'
!CCCC  GO TO 9000
!4209 CONTINUE
!
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO 4211 I=1,NR1
      IF(I.EQ.IYS2)GO TO 4211
      I2=I2+1
      NRJ=I2
      J2=0
      DO 4212 J=1,NC1
      IF(J.EQ.IYS3)GO TO 4212
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4212 CONTINUE
 4211 CONTINUE
!
      IF(NRJ.GE.1.AND.NCJ.GE.1)GO TO 4229
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
      GO TO 9000
 4229 CONTINUE
!
      DO 4231 I=1,NRJ
      DO 4232 J=1,NCJ
      YM9(I,J)=YM2(I,J)
 4232 CONTINUE
 4231 CONTINUE
!
      ITYP9='MATR'
!CCCC DECEMBER 1994.  FOLLOWING IS BACKWARDS.
!CCCC NR9=NCJ
!CCCC NC9=NRJ
      NR9=NRJ
      NC9=NCJ
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 43--                                      **
!               **  TREAT THE MATRIX     MINOR               CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
 4300 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 4309
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
      GO TO 9000
 4309 CONTINUE
!
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO 4311 I=1,NR1
      IF(I.EQ.IYS2)GO TO 4311
      I2=I2+1
      NRJ=I2
      J2=0
      DO 4312 J=1,NC1
      IF(J.EQ.IYS3)GO TO 4312
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4312 CONTINUE
 4311 CONTINUE
!
      IF(NRJ.GE.1.AND.NCJ.GE.1)GO TO 4329
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
      GO TO 9000
 4329 CONTINUE
!
      IF(NRJ.EQ.NCJ)GO TO 4339
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
      GO TO 9000
 4339 CONTINUE
!
!CCCC JULY 1993.  USE LINPACK ROUTINE TO COMPUTE THE DETERMINANT.
!CCCC CALL LUDCMP(YMJUNK,NRJ,MAXROM,INDEX,DP1M1)
!
!CCCC DPROD=DP1M1
!CCCC DO4341I=1,NRJ
!CCCC DYM9=YMJUNK(I,I)
!CCCC DPROD=DPROD*DYM9
!4341 CONTINUE
!CCCC DET=DPROD
!CCCC AMINOR=DET
!
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
!CCCC END CHANGE
!
      ITYP9='SCAL'
      SCAL9=AMINOR
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 44--                                      **
!               **  TREAT THE MATRIX     COFACTOR            CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
 4400 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 4409
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
      GO TO 9000
 4409 CONTINUE
!
      IYS2=INT(YS2+0.1)
      IYS3=INT(YS3+0.1)
      I2=0
      J2=0
      DO 4411 I=1,NR1
      IF(I.EQ.IYS2)GO TO 4411
      I2=I2+1
      NRJ=I2
      J2=0
      DO 4412 J=1,NC1
      IF(J.EQ.IYS3)GO TO 4412
      J2=J2+1
      NCJ=J2
      YM2(I2,J2)=YM1(I,J)
 4412 CONTINUE
 4411 CONTINUE
!
      IF(NRJ.GE.1.AND.NCJ.GE.1)GO TO 4429
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
      GO TO 9000
 4429 CONTINUE
!
      IF(NRJ.EQ.NCJ)GO TO 4439
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
      GO TO 9000
 4439 CONTINUE
!
!CCCC JULY 1993.  USE LINPACK ROUTINE TO COMPUTE THE DETERMINANT.
!CCCC CALL LUDCMP(YMJUNK,NRJ,MAXROM,INDEX,DP1M1)
!
!CCCC DPROD=DP1M1
!CCCC DO4441I=1,NRJ
!CCCC DYM9=YMJUNK(I,I)
!CCCC DPROD=DPROD*DYM9
!4441 CONTINUE
!CCCC DET=DPROD
!
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
!CCCC END CHANGE
!
      ITYP9='SCAL'
      SCAL9=COFACT
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 45--                                      **
!               **  TREAT THE MATRIX     DEFINITION          CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
!CCCC OCTOBER 1993.  ADD OPTIONAL SYNTAX.  IF FOURTH PARAMETER
!CCCC SPECIFIED, LET IT BE THE STARTING ROW NUMBER.
 4500 CONTINUE
!
      IF(ITYPA4.EQ.'PARA')GO TO 4560
      DO 4511 I=1,NR1
      DO 4512 J=1,NC1
      YM9(I,J)=YM1(I,J)
 4512 CONTINUE
 4511 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      GO TO 9000
!CCCC OCTOBER 1993.  ADD FOLLOWING SECTION
 4560 CONTINUE
      IROWID=INT(YS4+0.5)
      IF(IROWID.LT.1.OR.IROWID.GT.NR1)IROWID=1
      ICOUNT=0
!CCCC NLAST=IROWID+NR1-1
      NLAST=NR1
      IF(NLAST.GT.MAXROM)NLAST=MAXROM
      DO 4561 I=IROWID,NLAST
      ICOUNT=ICOUNT+1
      DO 4562 J=1,NC1
      YM9(ICOUNT,J)=YM1(I,J)
 4562 CONTINUE
 4561 CONTINUE
!
      ITYP9='MATR'
      NR9=ICOUNT
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 46--                                      **
!               **  TREAT THE MATRIX     EUCLIDEAN NORM      CASE  **
!               **  REFERENCE--RALSTON, PAGE XXX                   **
!               *****************************************************
!
 4600 CONTINUE
!
      DSUM1=0.0D0
      DO 4621 I=1,NR1
      DO 4622 J=1,NC1
      DYM1=YM1(I,J)
      DSUM1=DSUM1+DYM1*DYM1
 4622 CONTINUE
 4621 CONTINUE
      DSUM2=0.0D0
      IF(DSUM1.GT.0.0D0)DSUM2=SQRT(DSUM1)
!
      ITYP9='SCAL'
      SCAL9=DSUM2
      IUPFLG='FULL'
      GO TO 9000
!
!               **************************************************************
!               **  STEP 51--                                               **
!               **  TREAT THE VARIANCE-COVARIANCE CASE                      **
!               **************************************************************
!
 5100 CONTINUE
!
!CCCC JULY 2002.  SUPPORT FOR WINSORIZED CORRELATION.
!
!CCCC NOVEMBER 2004.  SUPPORT FOR ROW BASED (AS OPPOSSED TO COLUMN
!CCCC                 BASED COVARIANCES.
!
      NTRIM1=-1
      NTRIM2=-1
      IF(ICOVDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO 5151 J=1,NC1
          DO 5161 K=1,NC1
            DO 5155 I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 5155       CONTINUE
            IF(ICOVTY.EQ.'RANK')THEN
              CALL RANKCV(Y3,Y4,NR1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5181 I=1,NR1
                Y3(I)=Y2(I)
 5181         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5186 I=1,NR1
                Y4(I)=Y2(I)
 5186         CONTINUE
              CALL COV(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
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
        DO 5121 J=1,NR1
          DO 5131 K=1,NR1
            DO 5125 I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 5125       CONTINUE
            IF(ICOVTY.EQ.'RANK')THEN
              CALL RANKCV(Y3,Y4,NC1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5141 I=1,NC1
                Y3(I)=Y2(I)
 5141         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5146 I=1,NC1
                Y4(I)=Y2(I)
 5146         CONTINUE
              CALL COV(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
            ELSE
              CALL COV(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 5131     CONTINUE
 5121   CONTINUE
        NR9=NC1
        NC9=NC1
      ENDIF
!
!
      ITYP9='MATR'
      IUPFLG='FULL'
      GO TO 9000
!
!               ******************************************************
!               **  STEP 52--                                       **
!               **  TREAT THE CORRELATION CASE                      **
!               ******************************************************
!
 5200 CONTINUE
!
!CCCC JULY 2002.       SUPPORT FOR WINSORIZED CORRELATION, RANK CORRELATION,
!CCCC                  BIWEIGHT MID CORRELATION.
!CCCC NOVEMBER  2004.  SUPPORT FOR ROW BASED (AS OPPOSSED TO COLUMN
!CCCC                  BASED CORRELATIONS.  ALSO, ADD SUPPORT FOR
!CCCC                  KENDELL'S TAU CORRELATION.
!CCCC JUNE      2012.  SUPPORT FOR CORRELATION CDF MATRIX AND
!CCCC                              CORRELATION PVALUE MATRIX
!CCCC SEPTEMBER 2016.  SUPPORT FOR:
!CCCC                          SET CORRELATION ABSOLUTE VALUE <ON/OFF>
!CCCC                          SET CORRELATION PERCENTAGE VALUE <ON/OFF>
!CCCC                          SET CORRELATION DIGITS <VALUE>
!
      NTRIM1=-1
      NTRIM2=-1
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO 5251 J=1,NC1
          DO 5261 K=1,NC1
            DO 5255 I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 5255       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NR1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,   &
                          RIGHT,STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                          IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5281 I=1,NR1
                Y3(I)=Y2(I)
 5281         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5286 I=1,NR1
                Y4(I)=Y2(I)
 5286         CONTINUE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH1,   &
                          IBUGA3,IERROR)
              CALL BIWMDV(Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH2,   &
                          IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH3,   &
                          IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NR1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,   &
                          RIGHT,AKTAUA,AKTAUB,AKTAUC,   &
                          STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                          IBUGA3,ISUBRO,IERROR)
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
        DO 5221 J=1,NR1
          DO 5231 K=1,NR1
            DO 5225 I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 5225       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NC1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,   &
                          RIGHT,STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                          IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5241 I=1,NC1
                Y3(I)=Y2(I)
 5241         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 5246 I=1,NC1
                Y4(I)=Y2(I)
 5246         CONTINUE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH1,   &
                          IBUGA3,IERROR)
              CALL BIWMDV(Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH2,   &
                          IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH3,   &
                          IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NC1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,   &
                          RIGHT,AKTAUA,AKTAUB,AKTAUC,   &
                          STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                          IBUGA3,ISUBRO,IERROR)
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
!
!       SAVE EITHER THE CDF VALUES OR THE P-VALUES.
!
      IF(IMCASE.EQ.'MACC')THEN
        IDF1=1
        IDF2=NR1 - 2
        DO 5291 J=1,NC9
          DO 5292 I=1,NR9
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
        DO 5296 J=1,NC9
          DO 5297 I=1,NR9
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
!
      ITYP9='MATR'
      IUPFLG='FULL'
      GO TO 9000
!
!               **************************************************************
!               **  STEP 53--                                               **
!               **  TREAT THE PRINCIPAL COMPONENTS CASE                     **
!               **  TREAT THE PRINCIPAL COMPONENTS EIGENVECTORS CASE        **
!               **  TREAT THE PRINCIPAL COMPONENTS EIGENVALUES CASE         **
!               **  REFERENCE--JACKSON, J. E. (1980, 1981)                  **
!               **             PRINCIPAL COMPONENTS AND FACTOR ANALYSIS:    **
!               **             PART 1--PRINCIPAL COMPONENTS,                **
!               **             JQT OCT 1980, PAGES 201-213.                 **
!               **             PART 2--ADDITIONAL TOPICS RELATED            **
!               **             TO PRINCIPAL COMPONENTS,                     **
!               **             JQT JAN 1981, PAGES 46-58.                   **
!               **             PART 3--WHAT IS FACTOR ANALYSIS?,            **
!               **             JQT APR 1981, PAGES 125-130.                 **
!               **  REFERENCE--LAWTON, W. H., SYLVESTRE, E. A.,             **
!               **             AND MAGGIA, M. S. (1972).                    **
!               **             SELF MODELING NONLINEAR REGRESSION.          **
!               **             TECHNOMETRICS, AUGUST, 1972,                 **
!               **             PAGES 513-532.                               **
!               **************************************************************
!
 5300 CONTINUE
!
      IFLAGC=0
      IF(PCCASE.EQ.'DACV' .OR. PCCASE.EQ.'DACR')THEN
!
!       COMPUTE THE COVARIANCE OR CORRELATION MATRIX
!
        DO 5311 J=1,NC1
          DSUM1=0.0D0
          DO 5312 I=1,NR1
            DYM1=YM1(I,J)
            DSUM1=DSUM1+DYM1
 5312     CONTINUE
          DMEAN(J)=D999
          DDENOM=DNR1
          IF(DDENOM.NE.0.0D0)DMEAN(J)=DSUM1/DDENOM
 5311   CONTINUE
!
        DO 5321 J=1,NC1
          DO 5322 K=J,NC1
            DSUM1=0.0D0
            DO 5323 I=1,NR1
              DYM1=YM1(I,J)
              DYM2=YM1(I,K)
              DDEL1=DYM1-DMEAN(J)
              DDEL2=DYM2-DMEAN(K)
              DSUM1=DSUM1+DDEL1*DDEL2
 5323       CONTINUE
            DCOV=D999
            DDENOM=DNR1-1.0D0
            IF(DDENOM.NE.0.0D0)DCOV=DSUM1/DDENOM
            YM2(J,K)=DCOV
            YM2(K,J)=DCOV
 5322     CONTINUE
 5321   CONTINUE
!
        IF(PCCASE.EQ.'DACR')IFLAGC=1
      ELSEIF(PCCASE.EQ.'CVCV' .OR. PCCASE.EQ.'CVCR' .OR.   &
             PCCASE.EQ.'CRCR')THEN
        DO 5331 I=1,NR1
          DO 5332 J=1,NC1
            YM2(I,J)=YM1(I,J)
 5332     CONTINUE
 5331   CONTINUE
        IF(PCCASE.EQ.'CVCR')IFLAGC=1
      ELSEIF(PCCASE.EQ.'CRCV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5352)
 5352   FORMAT('       ILLEGAL PRINCIPAL COMPONENTS TYPE.  YOU CANNOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5353)
 5353   FORMAT('       SPECIFY THAT THE STARTING MATRIX IS THE ',   &
               'CORRELATION MATRIX')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5355)
 5355   FORMAT('      AND THEN SPECIFY THAT THE INTERMEDIATE MATRIX')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5356)
 5356   FORMAT('      IS THE COVARIANCE MATRIX (THE COVARIANCE MATRIX',   &
               'CANNOT BE DERIVED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5357)
 5357   FORMAT('      FROM THE CORRELATION MATRIX).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5358)
 5358   FORMAT('      FIX BY USING THE    PRINCIPAL COMPONENTS  TYPE ',   &
               'COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     CONVERT COVARIANCE MATRIX TO CORRELATION MATRIX
!
      IF(IFLAGC.EQ.1)THEN
        DO 5341 I=1,NC1
          S1=YM2(I,I)
          S1=SQRT(S1)
          DO 5342 J=1,NC1
            S2=YM2(J,J)
            S2=SQRT(S2)
            IF(I.EQ.J)GO TO 5342
            S1S2=S1*S2
            IF(S1S2.LE.0.0)YM2(I,J)=(-999.99)
            IF(S1S2.GT.0.0)YM2(I,J)=YM2(I,J)/S1S2
 5342     CONTINUE
 5341   CONTINUE
        DO 5343 I=1,NC1
          YM2(I,I)=1.0
 5343   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TARI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5362)
 5362   FORMAT('***** FROM THE MIDDLE OF MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5363)NC1,MAXCOM
 5363   FORMAT('NC1,MAXCOM = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 5364 I=1,NC1
          DO 5365 J=1,NC1
            WRITE(ICOUT,5366)I,J,YM2(I,J)
 5366       FORMAT('I,J,YM2(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 5365     CONTINUE
 5364   CONTINUE
      ENDIF
!
!CCCC JULY 1993.  USE EISPACK ROUTINES TO COMPUTE EIGENVALUES/EIGENVECTORS
!
      IERR2=0
      IJOB=1
      DO 5650 J=1,NC1
        DO 5651 I=1,NC1
          YM9(I,J)=YM2(I,J)
 5651   CONTINUE
 5650 CONTINUE
!
!     ON INPUT, YM9 CONTAINS THE COVARIANCE/CORRELATION MATRIX AND
!     ON OUTPUT IT CONTAINS THE EIGENVECTORS.  Y3 CONTAINS THE
!     EIGENVALUES ON OUTPUT.  Y4 IS A WORK ARRAY.
!
      CALL SSIEV(YM9,MAXROM,NC1,Y3,Y4,IJOB,IERR2)
      IF(IERR2.NE.0)THEN
        IERROR='YES'
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5662)
 5662   FORMAT('         PRINCIPAL COMPONENTS: UNABLE TO CALCULATE ',   &
               'THE EIGENVALUES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5663)
 5663   FORMAT('         OF THE COVARIANCE (OR CORRELATION) MATRIX.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      END IF
!CCCC END CHANGE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TARI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5372)
 5372   FORMAT('***** FROM THE MIDDLE OF MATARI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5373)
 5373   FORMAT('EIGENVACTOR MATRIX:')
        CALL DPWRST('XXX','BUG ')
        DO 5374 I=1,NC1
          DO 5375 J=1,NC1
            WRITE(ICOUT,5376)I,J,YM9(I,J),Y3(I)
 5376       FORMAT('I,J,YM9(I,J),Y3(I) = ',2I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
 5375     CONTINUE
 5374   CONTINUE
      ENDIF
!
!     SORT THE EIGENVALUES IN DESCENDING ORDER
!
      DO 5380 I=1,NC1
        Y1(I)=I
 5380 CONTINUE
!
      CALL SORTC(Y3,Y1,NC1,Y4,Y2)
!
      DO 5390 J=1,NC1
        JREV=NC1-J+1
        INDEX3=INT(Y2(JREV)+0.5)
        VECT9(J)=Y3(INDEX3)
 5390 CONTINUE
!
!     NOW SORT THE EIGENVECTOR MATRIX IN THE SAME ORDER
!
      DO 5411 J=1,NC1
        JREV=NC1-J+1
        INDEX3=INT(Y2(JREV)+0.5)
        DO 5412 I=1,NC1
          YM2(I,J)=YM9(I,INDEX3)
 5412   CONTINUE
 5411 CONTINUE
!
      DO 5416 I=1,NC1
        DO 5417 J=1,NC1
          YM9(I,J)=YM2(I,J)
 5417   CONTINUE
 5416 CONTINUE
!
      IF(IMCASE.EQ.'MAPC')THEN
        IF(IMSUBC.EQ.'EVEC')THEN
!
!         RETURN PRINCIPAL COMPONENT EIGENVECTORS
!
          ITYP9='MATR'
          NR9=NC1
          NC9=NC1
          IUPFLG='FULL'
          GO TO 9000
        ELSEIF(IMSUBC.EQ.'EVAL')THEN
          ITYP9='VECT'
          NVECT9=NC1
          IUPFLG='FULL'
          GO TO 9000
        ELSE
!
!         RETURN THE PRINCIPAL COMPONENTS
!
          DO 5461 I=1,NR1
            DO 5462 J=1,NC1
              DSUM=0.0D0
              DO 5463 K=1,NC1
                DYM1=YM1(I,K)
                DDEL=DYM1-DMEAN(K)
                DYM2=YM9(K,J)
                DYM9=DDEL*DYM2
                DSUM=DSUM+DYM9
 5463         CONTINUE
              YM2(I,J)=DSUM
 5462       CONTINUE
 5461     CONTINUE
          DO 5465 I=1,NR1
            DO 5466 J=1,NC1
              YM9(I,J)=YM2(I,J)
 5466       CONTINUE
 5465     CONTINUE
          ITYP9='MATR'
          NR9=NR1
          NC9=NC1
          IUPFLG='FULL'
          GO TO 9000
        ENDIF
      ELSE
!
!       RETURN A SPECIFIC PRINCIPAL COMPONENT (OR EIGENVALUE OR
!       EIGENVECTOR)
!
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
!
        IF(IMSUBC.EQ.'EVEC')THEN
!
!         L-TH EIGENVECTOR
!
          DO 5531 I=1,NC1
            VECT9(I)=YM9(I,L)
 5531     CONTINUE
          ITYP9='VECT'
          NVECT9=NC1
          IUPFLG='FULL'
          GO TO 9000
        ELSEIF(IMSUBC.EQ.'EVAL')THEN
!
!         L-TH EIGENVALUE
!
          ITYP9='SCAL'
          SCAL9=VECT9(L)
          IUPFLG='FULL'
          GO TO 9000
        ELSE
!
!         L-TH PRINCIPAL COMPONENT
!
          DO 5551 I=1,NR1
            DSUM=0.0D0
            DO 5553 K=1,NC1
              DYM1=YM1(I,K)
              DDEL=DYM1-DMEAN(K)
              DYM2=YM9(K,L)
              DYM9=DDEL*DYM2
              DSUM=DSUM+DYM9
 5553       CONTINUE
            VECT9(I)=DSUM
 5551     CONTINUE
          ITYP9='VECT'
          NVECT9=NR1
          IUPFLG='FULL'
          GO TO 9000
         ENDIF
       ENDIF
!
!               **************************************************
!               **  STEP 54--                                   **
!               **  TREAT THE MATRIX TRUNCATION      CASE       **
!               **  THIS COMMAND SETS ANY VALUE BELOW THE       **
!               **  TRUNCATION VALUE TO THAT TRUNCATION         **
!               **  VALUE.  A COMMON USE OF THIS COMMAND        **
!               **  MIGHT BE TO REMOVE BACKGROUND (USE          **
!               **  MATRIX SUBTRACTION TO REMOVE THE            **
!               **  BACKGROUND AND THEN USE MATRIX TRUNCATION   **
!               **  TO SET ANY RESULTING NEGATIVE VALUES (I.E., **
!               **  POINTS BELOW THE BACKGROUND LEVEL) TO ZERO. **
!               **************************************************
!
 6100 CONTINUE
!
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GO TO 6170
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GO TO 6180
!
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
      GO TO 9000
!
 6170 CONTINUE
      DO 6171 I=1,NR1
      DO 6172 J=1,NC1
        YM9(I,J)=MAX(YM1(I,J),YS2)
 6172 CONTINUE
 6171 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 6180 CONTINUE
      DO 6181 I=1,NR1
      DO 6182 J=1,NC1
        YM9(I,J)=MAX(YM2(I,J),YS1)
 6182 CONTINUE
 6181 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
!               **************************************************
!               **  STEP 55--                                   **
!               **  TREAT THE MATRIX UPPER TRUNCATION   CASE    **
!               **  THIS COMMAND SETS ANY VALUE ABOVE THE       **
!               **  TRUNCATION VALUE TO THAT TRUNCATION         **
!               **  VALUE.                                      **
!               **************************************************
!
 6200 CONTINUE
!
      IF(ITYPA1.EQ.'MATR'.AND.ITYPA2.EQ.'PARA')GO TO 6270
      IF(ITYPA1.EQ.'PARA'.AND.ITYPA2.EQ.'MATR')GO TO 6280
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6201)
 6201 FORMAT('***** ERROR IN MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6202)
 6202 FORMAT('      ILLEGAL ARGUMENT TYPES FOR MATRIX UPPER ',   &
             'TRUNCATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6203)ITYPA1
 6203 FORMAT('            TYPE FOR ARGUMENT 1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6204)ITYPA2
 6204 FORMAT('            TYPE FOR ARGUMENT 2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 6270 CONTINUE
      DO 6271 I=1,NR1
      DO 6272 J=1,NC1
        YM9(I,J)=MIN(YM1(I,J),YS2)
 6272 CONTINUE
 6271 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
 6280 CONTINUE
      DO 6281 I=1,NR1
      DO 6282 J=1,NC1
        YM9(I,J)=MIN(YM2(I,J),YS1)
 6282 CONTINUE
 6281 CONTINUE
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
!               ******************************************************
!               **  STEP 63--                                       **
!               **  TREAT THE COMOVEMENT CASE                       **
!               ******************************************************
!
 6300 CONTINUE
!
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO 6351 J=1,NC1
          DO 6361 K=1,NC1
            DO 6355 I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 6355       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCM(Y3,Y4,NR1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
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
        DO 6321 J=1,NR1
          DO 6331 K=1,NR1
            DO 6325 I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 6325       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCM(Y3,Y4,NC1,IWRITE,Y1,Y2,VECT9,MAXOBV,RIGHT,   &
                          IBUGA3,IERROR)
            ELSE
              CALL COMOVE(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6331     CONTINUE
 6321   CONTINUE
        NR9=NR1
        NC9=NR1
      ENDIF
!
      ITYP9='MATR'
      IUPFLG='FULL'
      GO TO 9000
!
!               ******************************************************
!               **  STEP 64--                                       **
!               **  TREAT THE PARTIAL CORRELATION CASE              **
!               ******************************************************
!
 6400 CONTINUE
!
!CCCC COMPUTE THE PARTIAL CORRELATION MATRIX.  AS WITH THE REGULAR
!CCCC CORRELATION MATRIX, SUPPORT FOR:
!CCCC
!CCCC   1. EITHER COLUMN (DEFAULT) OR ROW BASED CORRELATIONS
!CCCC   2. SUPPORT FOR PEARSON CORRELATION, WINSORIZED CORRELATION,
!CCCC      BIWEIGHT MID-CORRELATION, RANK CORRELATION, OR KENDALL TAU
!CCCC      CORRELATION.
!CCCC
!CCCC ALGORITHM IS:
!CCCC
!CCCC   1. COMPUTE THE STANDARD CORRELATION MATRIX
!CCCC   2. INVERT THE CORELATION MATRIX
!CCCC   3. R(ij.) = -R(ij)/SQRT(R(ii)*R(jj))
!CCCC      WHERE R(IJ) IS THE IJ-TH ELEMENT OF THE INVERTED CORRELATION
!CCCC      MATRIX.
!CCCC
!CCCC   AUTOMATICALLY SET THE DIAGONAL ELEMENTS TO +1.
!
!       NUMBER OF ROWS (N) MUST BE GREATER THAN NUMBER OF COLUMNS
!
      IF(ICORDI.EQ.'COLU')THEN
        IF(NR1-NC1.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6423)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6491)
 6491     FORMAT('      THE NUMBER OF ROWS IN THE MATRIX MUST BE ',   &
                 'GREATER THAN THE NUMBER OF COLUMNS.')
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6493)NR1
 6493     FORMAT('      THE NUMBER OF ROWS     = ',I8)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6495)NC1
 6495     FORMAT('      THE NUMBER OF COLUMNS  = ',I8)
          CALL DPWRST('XXX','ERRO')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        IF(NC1-NR1.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6423)
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6492)
 6492     FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX MUST BE ',   &
                 'GREATER THAN THE NUMBER OF ROWS.')
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6493)NR1
          CALL DPWRST('XXX','ERRO')
          WRITE(ICOUT,6495)NC1
          CALL DPWRST('XXX','ERRO')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      NTRIM1=-1
      NTRIM2=-1
      IF(ICORDI.EQ.'COLU')THEN
        IWRITE='OFF'
        DO 6401 J=1,NC1
          DO 6402 K=1,NC1
            DO 6403 I=1,NR1
              Y3(I)=YM1(I,J)
              Y4(I)=YM1(I,K)
 6403       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NR1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,   &
                          RIGHT,STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                          IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 6406 I=1,NR1
                Y3(I)=Y2(I)
 6406         CONTINUE
              CALL WINSOR(Y4,NR1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 6407 I=1,NR1
                Y4(I)=Y2(I)
 6407         CONTINUE
              CALL CORR(Y3,Y4,NR1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH1,   &
                          IBUGA3,IERROR)
              CALL BIWMDV(Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH2,   &
                          IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NR1,IWRITE,Y1,Y2,MAXOBV,RIGH3,   &
                          IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NR1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,   &
                          RIGHT,AKTAUA,AKTAUB,AKTAUC,   &
                          STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                          IBUGA3,ISUBRO,IERROR)
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
        DO 6411 J=1,NR1
          DO 6412 K=1,NR1
            DO 6413 I=1,NC1
              Y3(I)=YM1(J,I)
              Y4(I)=YM1(K,I)
 6413       CONTINUE
            IF(ICORTY.EQ.'RANK')THEN
              CALL RANKCR(Y3,Y4,NC1,IRCRTA,IWRITE,Y1,Y2,VECT9,MAXOBV,   &
                          RIGHT,STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                          IBUGA3,ISUBRO,IERROR)
            ELSEIF(ICORTY.EQ.'WINS')THEN
              CALL WINSOR(Y3,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 6414 I=1,NC1
                Y3(I)=Y2(I)
 6414         CONTINUE
              CALL WINSOR(Y4,NC1,P1,P2,NTRIM1,NTRIM2,IWRITE,   &
                          Y1,MAXOBV,Y2,   &
                          IBUGA3,ISUBRO,IERROR)
              DO 6415 I=1,NC1
                Y4(I)=Y2(I)
 6415         CONTINUE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ELSEIF(ICORTY.EQ.'PBCR')THEN
              CALL PBNCOR(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGHT,BETA,   &
                          IBUGA3,IERROR)
            ELSEIF(ICOVTY.EQ.'BIWE')THEN
              CALL BIWMDV(Y3,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH1,   &
                          IBUGA3,IERROR)
              CALL BIWMDV(Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH2,   &
                          IBUGA3,IERROR)
              CALL BIWMCV(Y3,Y4,NC1,IWRITE,Y1,Y2,MAXOBV,RIGH3,   &
                          IBUGA3,IERROR)
              RIGH4=RIGH1*RIGH2
              IF(RIGH4.GT.0.0)THEN
                RIGHT=RIGH3/SQRT(RIGH4)
              ELSE
                RIGHT=0.0
              ENDIF
            ELSEIF(ICORTY.EQ.'KTAU')THEN
              ICASZZ='TWOS'
              CALL KENTAU(Y3,Y4,NC1,ICASZZ,IKTATA,IWRITE,Y1,Y2,MAXOBV,   &
                          RIGHT,AKTAUA,AKTAUB,AKTAUC,   &
                          STATCD,PVAL,PVALLT,PVALUT,   &
                          CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                          CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                          IBUGA3,ISUBRO,IERROR)
            ELSE
              CALL CORR(Y3,Y4,NC1,IWRITE,RIGHT,IBUGA3,IERROR)
            ENDIF
            YM9(J,K)=RIGHT
 6412     CONTINUE
 6411   CONTINUE
        NR9=NR1
        NC9=NR1
      ENDIF
!
!     NOW INVERT THE CORRELATION MATRIX
!
      CALL SGECO(YM9,MAXROM,NR9,INDEX,RCOND,Y3)
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6421)RCOND
        CALL DPWRST('XXX','TEXT ')
      ENDIF
 6421 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE CORRELATION ',   &
             'MATRIX = ',G15.7)
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
!
        DO 6431 J=1,NC9
          DO 6432 I=1,NR9
            YM1(I,J)=PSTAMV
            IF(I.EQ.J)THEN
              YM1(I,J)=1.0
            ELSE
              DENOM=YM9(I,I)*YM9(J,J)
              IF(DENOM.GT.0.0)YM1(I,J)=-YM9(I,J)/SQRT(DENOM)
            ENDIF
 6432     CONTINUE
 6431   CONTINUE
!
!       SAVE EITHER THE PARTIAL CORRELATION MATRIX, THE CDF
!       VALUES, OR THE P-VALUES.
!
        IF(IMCASE.EQ.'MPCO')THEN
          DO 6441 J=1,NC9
            DO 6442 I=1,NR9
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
          DO 6451 J=1,NC9
            DO 6452 I=1,NR9
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
          DO 6461 J=1,NC9
            DO 6462 I=1,NR9
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
!
      END IF
!
      ITYP9='MATR'
      IUPFLG='FULL'
      GO TO 9000
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TARI')GO TO 9090
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATARI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
      A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
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
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GO TO 9039
      IF(NC1.LE.0)GO TO 9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO 9032 I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9049
      IF(NC2.LE.0)GO TO 9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO 9042 I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9059
      IF(NC9.LE.0)GO TO 9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9055 I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GO TO 9119
      DO 9112 I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GO TO 9129
      DO 9122 I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GO TO 9139
      DO 9132 I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GO TO 9149
      DO 9142 I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GO TO 9169
      DO 9162 I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9179
      IF(NC9.LE.0)GO TO 9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9172 I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
!
      IF(IMCASE.NE.'MASS')GO TO 9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9189
      IF(NC2.LE.0)GO TO 9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO 9182 I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
      DO 9185 I=1,N3
      WRITE(ICOUT,9186)ICASE,I,IZROV(I),IPOSV(I)
 9186 FORMAT('ICASE,I,IZROV(I),IPOSV(I) = ',4I8)
      CALL DPWRST('XXX','BUG ')
 9185 CONTINUE
      WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
 9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
!
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE MATARI
      SUBROUTINE MATAR2(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,MAXROM,MAXCOM,   &
      Y1,N1,Y2,N2,Y3,N3,Y4,N4,   &
      INDEX,   &
      YS1,YS2,YS3,YS4,   &
      IMCASE,IUPFLG,IMSUBC,ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,   &
      IBPLSC,PBPLCO,   &
      YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,   &
      IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
!              OF THE REAL DATA IN MATRICES YM1 AND YM2.
!
!     OPERATIONS--ADDITION
!                 SUBTRACTION
!                 MULTIPLICATION
!                 SOLUTION
!                 ITERATIVE SOLUTION
!                 INVERSE
!                 TRANSPOSE
!                 ADJOINT
!                 CHARACTERISTIC EQUATION      (NOT YET IMPLEMENED)
!                 EIGENVALUES
!                 EIGENVECTORS
!                 RANK
!                 DETERMINANT
!                 PERMANENT
!                 SPECTRAL NORM
!                 SPECTRAL RADIUS
!                 NUMBER OF ROWS
!                 NUMBER OF COLUMNS
!                 SIMPLEX SOLUTION
!                 TRACE
!                 SUBMATRIX
!                 MINOR
!                 COFACTOR
!                 DEFINITION
!                 EUCLIDEAN NORM
!                 SINGULAR VALUE
!                 SINGULAR VALUE DECOMPOSITION
!                 SINGULAR VALUE FACTORIZATION
!                 ROW
!                 ELEMENT
!                 REPLACE ROW
!                 REPLACE ELEMENT
!                 AUGMENT
!                 DIAGONAL
!                 CHOLESKY DECOMPOSITION
!                 TRIDIAGONAL SOLVE
!                 TRIANGULAR SOLVE
!                 TRIANGULAR INVERSE
!
!                 VARIANCE-COVARIANCE MATRIX
!                 CORRELATION MATRIX
!                 PRINCIPLE COMPONENTS ...
!                 ... PRINCIPLE COMPONENT ...
!                 BIPLOT
!
!     EXAMPLES--LET M3 = MATRIX ADDITION M1 M2
!               LET M3 = MATRIX ADDITION M1 P1
!             --LET M3 = MATRIX SUBTRACTION M1 M2
!               LET M3 = MATRIX SUBTRACTION M1 P1
!             --LET M3 = MATRIX MULTIPLICATION M1 M2
!               LET M3 = MATRIX MULTIPLICATION M1 V1
!               LET M3 = MATRIX MULTIPLICATION M1 P1
!             --LET V3 = MATRIX SOLUTION M1 V2
!             --LET M3 = MATRIX INVERSE M1
!             --LET A  = MATRIX CONDITION NUMBER M1
!             --LET M3 = MATRIX TRANSPOSE M1
!             --LET M3 = MATRIX ADJOINT M1
!             --LET V3 = MATRIX CHARACTERISTIC EQUATION M1
!             --LET V3 = MATRIX EIGENVALUES M1
!             --LET P3 = MATRIX EIGENVECTORS M1
!             --LET P3 = MATRIX RANK M1
!             --LET P3 = MATRIX DETERMINANT M1
!             --LET P3 = MATRIX PERMANENT M1
!             --LET P3 = MATRIX SPECTRAL NORM M1
!             --LET P3 = MATRIX SPECTRAL RADIUS M1
!             --LET P3 = MATRIX NUMBER OF ROWS M1
!             --LET P3 = MATRIX NUMBER OF COLUMNS M1
!             --LET V4 = MATRIX SIMPLEX SOLUTION V1 M1 V2 V3
!             --LET P3 = MATRIX TRACE M1
!             --LET M3 = MATRIX SUBMATRIX M1 P1 P2
!             --LET P3 = MATRIX MINOR M1 P1 P2
!             --LET P3 = MATRIX COFACTOR M1 P1 P2
!             --LET M3 = MATRIX DEFINITION V1 P1 P2
!             --LET P3 = MATRIX EUCLIDEAN NORM M1
!             --LET V3 = MATRIX ROW M1 P1
!             --LET P3 = MATRIX ELEMENT M1 P1 P2
!             --LET M3 = MATRIX REPLACE ROW M1 V1 P1
!             --LET M3 = MATRIX REPLACE ELEMENT M1 P1 P2
!             --LET M3 = MATRIX AUGMENT M1
!             --LET V3 = MATRIX DIAGONAL M1
!             --LET M3 = DIAGONAL MATRIX V1
!             --LET M3 = VARIANCE-COVARIANCE MATRIX M1
!             --LET M3 = CORRELATION MATRIX M1
!             --LET M3 = PRINCIPLE COMPONENTS M1
!             --LET M3 = PRINCIPLE COMPONENTS EIGENVECTORS M1
!             --LET V3 = PRINCIPLE COMPONENTS EIGENVALUES M1
!             --LET V3 = ... PRINCIPLE COMPONENT M1
!             --LET V3 = ... PRINCIPLE COMPONENT EIGENVECTOR M1
!             --LET P3 = ... PRINCIPLE COMPONENT EIGENVALUE M1
!             --LET V3 = MATRIX SINGULAR VALUES M1
!             --LET M3 V3 M2 = MATRIX SINGULAR VALUE DECOMP M1
!             --LET M3 V3 M2 = MATRIX SINGULAR VALUE FACTOR M1
!             --LET M3 = CHOLESKY DECOMP M1
!             --LET V4 = TRIDIAGONAL SOLVE V1 V2 V3
!
!     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
!                     --NR1
!                     --NC1
!                     --YM2 (REAL MATRIX)
!                     --NR2
!                     --NC2
!                     --YM3 (REAL MATRIX)
!                     --NR3
!                     --NC3
!                     --Y1  (REAL VECTOR)
!                     --N1
!                     --Y2  (REAL VECTOR)
!                     --N2
!                     --Y3  (REAL VECTOR)
!                     --N3
!                     --Y4  (REAL VECTOR)
!                     --N4
!     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
!                     --NR9
!                     --NC9
!                     --VECT9 (REAL VECTOR)
!                     --NVECT9
!                     --SCAL9 (REAL SCALAR)
!                     --ITYP9
!
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
!           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/10
!     ORIGINAL VERSION--SEPTEMBER 1987.
!     UPDATED         --AUGUST    1988  (VARIANCE-COVARIANCE MATRIX)
!     UPDATED         --AUGUST    1988  (CORRELATION MATRIX)
!     UPDATED         --AUGUST    1988  (PRINCIPLE COMPONENTS)
!     UPDATED         --AUGUST    1988  (... PRINCIPLE COMPONENTS)
!     UPDATED         --APRIL     1992  DEFINE D999
!     UPDATED         --JULY      1993  FOR MATRIX SOLUTION,
!                                       DETERMINANT, INVERSE, REPLACE
!                                       NUMERICAL RECIPES CODE WITH
!                                       LINPACK CODE
!     UPDATED         --JULY      1993  EIGENVALUES AND EIGENVECTORS
!                                       EXTENDED TO NON-SYMMETRIC CASE
!     UPDATED         --JULY      1993  IMPLEMENT RANK, ADJOINT,
!                                       SINGULAR VALUES, SINGULAR VALUE
!                                       DECOMP.
!     UPDATED         --SEPT      1993  ROW, ELEMENT CASES
!     UPDATED         --OCT       1993  CHOLESKY DECOMPOSITION, REPLACE
!                                       ROW, REPLACE ELEMENT, AUGMENT,
!                                       DIAGONAL, ADD ARGUMENT TO
!                                       MATRIX DEFINITION, TRIDIAGONAL
!                                       SOLVE.
!     UPDATED         --JANUARY   1998 RECODE TO MINIMIZE NUMBER OF
!                                      MATRICES NEEDED.
!     UPDATED         --APRIL     2009 ADD BIPLOT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DNR1
      DOUBLE PRECISION DNC1
!CCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992
      DOUBLE PRECISION D999
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DYM1
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION YM1(MAXROM,MAXCOM)
      DIMENSION YM2(MAXROM,MAXCOM)
!CCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
!CCCC DIMENSION YM3(MAXROM,MAXCOM)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION YM9(MAXROM,MAXCOM)
!CCCC DIMENSION VECT9(MAXROM)
      DIMENSION VECT9(*)
!
!CCCC JANUARY 1998.  RECODE TO USE LESS MATRICES.
!CCCC DIMENSION YMJUNK(MAXROM,MAXCOM)
!CCCC DIMENSION YMJUN2(MAXROM,MAXCOM)
!CCCC DIMENSION INDEX(MAXROM)
!CCCC DIMENSION VJUNK(MAXROM)
!CCCC DIMENSION VJUNK2(MAXROM)
      DIMENSION INDEX(*)
!CCCC DIMENSION VJUNK(MAXOBV)
!CCCC DIMENSION VJUNK2(MAXOBV)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='MATA'
      ISUBN2='R2  '
      IERROR='NO'
!
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
!
      NRJ=(-999)
      NCJ=(-999)
      D999=(-999.0D0)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
!
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATAR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52   FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE
   53   FORMAT('IMCASE,IMSUBC,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)YS1,YS2,YS3,YS4
   55   FORMAT('YS1,YS2,YS3,YS4 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)NR1,NC1,NR2,NC2,NR3,NC3
   61   FORMAT('NR1,NC1,NR2,NC2,NR3,NC3 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR1.GE.1 .AND. NC1.GE.1)THEN
          JMAX=NC1
          IF(JMAX.GT.10)JMAX=10
          DO 62 I=1,NR1
            WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63       FORMAT('I,YM1(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NR2.GE.1 .AND. NC2.GE.1)THEN
          JMAX=NC2
          IF(JMAX.GT.10)JMAX=10
          DO 72 I=1,NR2
            WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73       FORMAT('I,YM2(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   72     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NR3.GE.1 .AND. NC3.GE.1)THEN
          JMAX=NC3
          IF(JMAX.GT.10)JMAX=10
          DO 82 I=1,NR3
            WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83       FORMAT('I,YM9(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)N1,N2,N3,N4
  111   FORMAT('N1,N2,N3,N4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GE.1)THEN
          DO 112 I=1,N1
            WRITE(ICOUT,113)I,Y1(I)
  113       FORMAT('I,Y1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  112     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(N2.GE.1)THEN
          DO 122 I=1,N2
            WRITE(ICOUT,123)I,Y2(I)
  123       FORMAT('I,Y2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  122     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(N3.GE.1)THEN
          DO 132 I=1,N3
            WRITE(ICOUT,133)I,Y3(I)
  133       FORMAT('I,Y3(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  132     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(N4.GE.1)THEN
          DO 142 I=1,N4
            WRITE(ICOUT,143)I,Y4(I)
  143       FORMAT('I,Y4(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  142     CONTINUE
        ENDIF
!
      ENDIF
!
!               **************************************************
!               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
!               **************************************************
!
      DNR1=NR1
      DNC1=NC1
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
!               ********************************************
!
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GO TO 1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GO TO 1100
!
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GO TO 1100
!
      GO TO 1190
!
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
 1113 FORMAT('      MATRIX AND/OR VECTOR FOR WHICH THE MATRIX ',   &
             'OPERATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('      IS TO BE COMPUTED MUST BE 1 OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
!
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
      GO TO 9000
!
 1190 CONTINUE
!
!               *********************************
!               **  STEP 12--                  **
!               **  BRANCH TO THE PROPER CASE  **
!               *********************************
!
!CCCC JULY 1993.  ADD FOLLOWING 3 LINES
      IF(IMCASE.EQ.'MASV')GO TO 5800
      IF(IMCASE.EQ.'MASD')GO TO 5900
      IF(IMCASE.EQ.'MASF')GO TO 6000
!CCCC SEPTEMBER 1993.  ADD FOLLOWING 2 LINES
      IF(IMCASE.EQ.'MARW')GO TO 6100
      IF(IMCASE.EQ.'MAEL')GO TO 6200
!CCCC OCTOBER 1993.  ADD FOLLOWING LINE
      IF(IMCASE.EQ.'MACH')GO TO 6300
      IF(IMCASE.EQ.'MAAU')GO TO 6400
      IF(IMCASE.EQ.'MADI')GO TO 6500
      IF(IMCASE.EQ.'DIMA')GO TO 6600
      IF(IMCASE.EQ.'MARR')GO TO 6700
      IF(IMCASE.EQ.'MARE')GO TO 6800
      IF(IMCASE.EQ.'MATD')GO TO 6900
      IF(IMCASE.EQ.'MATS')GO TO 7000
      IF(IMCASE.EQ.'MATI')GO TO 7100
      IF(IMCASE.EQ.'MAIS')GO TO 7200
      IF(IMCASE.EQ.'BIPL')GO TO 7300
!
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
      GO TO 9000
!
!               ************************************************
!               **  STEP 58--                                 **
!               **  TREAT THE MATRIX SINGULAR VALUES CASE     **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 5800 CONTINUE
!
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y1,YM1,MAXROM,   &
      YM1,MAXROM,Y2,IJOB,IERR2)
!
      ITYP9='VECT'
      NVECT9=MIN(NR1,NC1)
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 59--                                 **
!               **  TREAT THE MATRIX SINGULAR VALUES          **
!               **  DECOMPOSITION CASE                        **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 5900 CONTINUE
!
      IF(NR1.LE.MAXCOM)GO TO 5909
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
      GO TO 9000
!
 5909 CONTINUE
      DO 5922 J=1,MAXCOM
      DO 5921 I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 5921 CONTINUE
 5922 CONTINUE
!
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,   &
      YM2,MAXROM,Y2,IJOB,IERR2)
!
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NR1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 60--                                 **
!               **  TREAT THE MATRIX SINGULAR VALUES          **
!               **  FACTORIZATION CASE                        **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 6000 CONTINUE
!
      DO 6022 J=1,MAXCOM
      DO 6021 I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 6021 CONTINUE
 6022 CONTINUE
!
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,   &
      YM2,MAXROM,Y2,IJOB,IERR2)
!
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NC1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 61--                                      **
!               **  TREAT THE MATRIX     ROW                 CASE  **
!               *****************************************************
!
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
        GO TO 9000
      ENDIF
 6101 FORMAT('***** ERROR IN MATAR2--')
 6102 FORMAT('      FOR MATRIX ROW,')
 6103 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6104 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6105 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
!
      DO 6120 J=1,NC1
      VECT9(J)=YM1(IROWID,J)
 6120 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 62--                                      **
!               **  TREAT THE MATRIX     ELEMENT             CASE  **
!               *****************************************************
!
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
        GO TO 9000
      ENDIF
 6201 FORMAT('***** ERROR IN MATAR2--')
 6202 FORMAT('      FOR MATRIX ELEMENT,')
 6203 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6204 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6205 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
!
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
        GO TO 9000
      ENDIF
 6211 FORMAT('***** ERROR IN MATAR2--')
 6212 FORMAT('      FOR MATRIX ELEMENT,')
 6213 FORMAT('      THE REQUESTED COLUMN IN THE MATRIX MUST BE')
 6214 FORMAT('      BETWEEN 1 AND ',I8,'.  SUCH WAS NOT THE CASE')
 6215 FORMAT('      HERE.  THE REQUESTED COLUMN NUMBER = ',I8)
!
      ITYP9='SCAL'
      SCAL9=YM1(IROWID,ICOLID)
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 63--                              **
!               **  TREAT THE MATRIX CHOLESKY DECOMP CASE  **
!               **  REFERENCE--LINPACK USER'S GUIDE        **
!               *********************************************
!
 6300 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 6309
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
      GO TO 9000
 6309 CONTINUE
!
      CALL SPOCO(YM1,MAXROM,NR1,RCOND,Y1,INFO)
!
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
!
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
!
      DO 6380 I=1,NR1
      DO 6382 J=I,NR1
        YM9(J,I)=0.
        YM9(I,J)=YM1(I,J)
 6382 CONTINUE
 6380 CONTINUE
!
      ITYP9='MATR'
      NVECT9=NR1
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!
!               ******************************************************
!               **  STEP 64--                                       **
!               **  TREAT THE MATRIX AUGMENT CASE                   **
!               ******************************************************
!
 6400 CONTINUE
!
      IF(NR1.EQ.NR2)GO TO 6409
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
      GO TO 9000
 6409 CONTINUE
!
      IF(NC1+NC2.LE.MAXCOM)GO TO 6419
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
      GO TO 9000
 6419 CONTINUE
!
      DO 6430 J=1,NC1
      DO 6435 I=1,NR1
      YM9(I,J)=YM1(I,J)
 6435 CONTINUE
 6430 CONTINUE
!
      DO 6440 J=1,NC2
      DO 6445 I=1,NR2
      J2=J+NC1
      YM9(I,J2)=YM2(I,J)
 6445 CONTINUE
 6440 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1+NC2
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 65--                                      **
!               **  TREAT THE MATRIX DIAGONAL                CASE  **
!               *****************************************************
!
 6500 CONTINUE
      IF(NR1.EQ.NC1)GO TO 6509
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
      GO TO 9000
 6509 CONTINUE
!
      DO 6520 I=1,NC1
      VECT9(I)=YM1(I,I)
 6520 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 66--                                      **
!               **  TREAT THE DIAGONAL MATRIX                CASE  **
!               *****************************************************
!
 6600 CONTINUE
!
      IF(N1.LE.MAXCOM)GO TO 6609
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
      GO TO 9000
!
 6609 CONTINUE
      DO 6610 J=1,N1
      DO 6615 I=1,N1
      YM9(I,J)=0.0
 6615 CONTINUE
 6610 CONTINUE
      DO 6620 I=1,N1
      YM9(I,I)=Y1(I)
 6620 CONTINUE
!
      ITYP9='MATR'
      NR9=N1
      NC9=N1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 67--                                      **
!               **  TREAT THE MATRIX REPLACE ROW             CASE  **
!               *****************************************************
!
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
        GO TO 9000
      ENDIF
 6701 FORMAT('***** ERROR IN MATAR2--')
 6702 FORMAT('      FOR MATRIX REPLACE ROW,')
 6703 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6704 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6705 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
!
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
        GO TO 9000
      ENDIF
 6711 FORMAT('***** ERROR IN MATAR2--')
 6712 FORMAT('      FOR MATRIX REPLACE ROW,')
 6713 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX MUST EQUAL')
 6714 FORMAT('      THE NUMBER OF COLUMNS IN THE VECTOR.  SUCH WAS')
 6715 FORMAT('      NOT THE CASE HERE.')
 6716 FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX = ',I8)
 6717 FORMAT('      THE NUMBER OF COLUMNS IN THE VECTOR = ',I8)
!
      DO 6720 J=1,NC1
      DO 6725 I=1,NR1
      YM9(I,J)=YM1(I,J)
 6725 CONTINUE
 6720 CONTINUE
      DO 6730 J=1,N2
      YM9(IROWID,J)=Y2(J)
 6730 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 68--                                      **
!               **  TREAT THE MATRIX REPLACE ELEMENT         CASE  **
!               *****************************************************
!
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
        GO TO 9000
      ENDIF
 6801 FORMAT('***** ERROR IN MATAR2--')
 6802 FORMAT('      FOR MATRIX REPLACE ELEMENT,')
 6803 FORMAT('      THE REQUESTED ROW IN THE MATRIX MUST BE BETWEEN')
 6804 FORMAT('      1 AND ',I8,'.  SUCH WAS NOT THE CASE HERE.')
 6805 FORMAT('      THE REQUESTED ROW NUMBER = ',I8)
!
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
        GO TO 9000
      ENDIF
 6811 FORMAT('***** ERROR IN MATAR2--')
 6812 FORMAT('      FOR MATRIX REPLACE ELEMENT,')
 6813 FORMAT('      THE REQUESTED COLUMN IN THE MATRIX MUST BE')
 6814 FORMAT('      BETWEEN 1 AND ',I8,'.  SUCH WAS NOT THE CASE')
 6815 FORMAT('      HERE.  THE REQUESTED COLUMN NUMBER = ',I8)
!
      DO 6820 J=1,NC1
      DO 6825 I=1,NR1
      YM9(I,J)=YM1(I,J)
 6825 CONTINUE
 6820 CONTINUE
      YM9(IROWID,ICOLID)=YS4
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *********************************************
!               **  STEP 69--                              **
!               **  TREAT THE TRIDIAGONAL SOLUTION   CASE  **
!               **  REFERENCE--LINPACK (CHAPTER 7)         **
!               *********************************************
!
 6900 CONTINUE
!
      IF((N1.EQ.N2).AND.(N2.EQ.N3).AND.(N3.EQ.N4))GO TO 6909
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
      GO TO 9000
 6909 CONTINUE
!
      CALL SGTSL(N1,Y1,Y2,Y3,Y4,INFO)
      IF(INFO.EQ.0)GO TO 6919
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
      GO TO 9000
 6919 CONTINUE
!
      DO 6920 I=1,N1
      VECT9(I)=Y4(I)
 6920 CONTINUE
!
      ITYP9='VECT'
      NVECT9=N1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 70--                              **
!               **  TREAT THE TRIANGULAR SOLVE       CASE  **
!               **  REFERENCE--LINPACK (CHAPTER 6)         **
!               *********************************************
!
 7000 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 7009
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
      GO TO 9000
 7009 CONTINUE
!
      IJOB=1
      DO 7046 I=1,NR1
      DO 7047 J=I+1,NC1
      IF(YM1(I,J).NE.0.0)GO TO 7049
 7047 CONTINUE
 7046 CONTINUE
      IJOB=0
 7049 CONTINUE
!
      DO 7051 I=1,N2
      VECT9(I)=Y2(I)
 7051 CONTINUE
!
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
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 71--                              **
!               **  TREAT THE TRIANGULAR INVERSE     CASE  **
!               **  REFERENCE--LINPACK (CHAPTER 6)         **
!               *********************************************
!
 7100 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 7109
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
      GO TO 9000
 7109 CONTINUE
!
      IJOB=11
      DO 7126 I=1,NR1
      DO 7127 J=I+1,NC1
      IF(YM1(I,J).NE.0.0)GO TO 7129
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
        GO TO 9000
      END IF
 7171 FORMAT('****** ERROR IN MATAR2 ********')
 7172 FORMAT('       THE INPUT MATRIX IS SINGULAR')
!
      DO 7181 J=1,NC1
      DO 7182 I=1,NR1
      YM9(I,J)=YM1(I,J)
 7182 CONTINUE
 7181 CONTINUE
!CCCC END CHANGE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *********************************************
!               **  STEP 72--                              **
!               **  TREAT THE MATRIX ITERATIVE SOLUTION CASE*
!               **  REFERENCE--LINPACk (PAGE 1.9)          **
!               *********************************************
!
 7200 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 7209
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
      GO TO 9000
 7209 CONTINUE
!
      DO 7241 J=1,NC1
      DO 7242 I=1,NR1
      YM2(I,J)=YM1(I,J)
 7242 CONTINUE
      VECT9(J)=Y2(J)
 7241 CONTINUE
!
      CALL SGEFA(YM2,MAXROM,NR1,INDEX,INFO)
      IF(INFO.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7271)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,7272)
        CALL DPWRST('XXX','ERRO ')
        IERROR='YES'
        GO TO 9000
      END IF
 7271 FORMAT('****** ERROR IN MATAR2 ********')
 7272 FORMAT('       THE INPUT MATRIX IS SINGULAR')
!
      IJOB=0
      CALL SGESL(YM2,MAXROM,NR1,INDEX,VECT9,IJOB)
      XNORM=SASUM(NR1,VECT9,1)
      RELERR=0.0
      IF(XNORM.EQ.0.0)GO TO 7295
      DO 7280 ITER=1,20
        DO 7285 I=1,NR1
          Y3(I)=SDSDOT(NR1,YM1(I,1),MAXROM,VECT9(1),1,-Y2(I))
 7285   CONTINUE
        CALL SGESL(YM2,MAXROM,NR1,INDEX,Y3,IJOB)
        DO 7290 I=1,NR1
          VECT9(I)=VECT9(I)-Y3(I)
 7290   CONTINUE
        RNORM=SASUM(NR1,Y3,1)
        IF(ITER.EQ.1)RELERR=RNORM/XNORM
        YS1=XNORM+RNORM
        IF(YS1.EQ.XNORM)GO TO 7295
 7280 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7281)
      CALL DPWRST('XXX','ERRO ')
      WRITE(ICOUT,7282)
      CALL DPWRST('XXX','ERRO ')
      GO TO 9000
 7281 FORMAT('****** ERROR IN MATARI ********')
 7282 FORMAT('       SOLUTION FAILED TO CONVERGE.')
!
 7295 CONTINUE
      ITYP9='VECT'
      NVECT9=NR1
      IF(IFEEDB.EQ.'OFF')GO TO 7299
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7296)RCOND
      CALL DPWRST('XXX','TEXT ')
 7296 FORMAT('THE RELATIVE ERROR = ',E15.7)
 7299 CONTINUE
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 73--                                 **
!               **  TREAT THE BIPLOT CASE                     **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 7300 CONTINUE
!
      DO 7322 J=1,MAXCOM
      DO 7321 I=1,MAXROM
        YM9(I,J)=0.0
        YM2(I,J)=0.0
 7321 CONTINUE
 7322 CONTINUE
!
!     STEP 1: SCALE MATRIX (BASED ON IBPLSC)
!
!             1) GMEA  - SUBTRACT GRAND MEAN (DEFAULT)
!             2) CMEA  - SUBTRACT COLUMN MEAN
!             3) NONE  - NO SCALING
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7301)IBPLSC,NR1,NC1
 7301   FORMAT('AT BIPLOT: IBPLSC,NR1,NC1 = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 7305 I=1,NR1
          DO 7306 J=1,NC1
            WRITE(ICOUT,7308)I,J,YM1(I,J)
 7308       FORMAT('I,J,YM1(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 7306     CONTINUE
 7305   CONTINUE
      ENDIF
!
      IF(IBPLSC.EQ.'CMEA')THEN
        DO 7330 J=1,NC1
          DSUM1=0.0D0
          DO 7335 I=1,NR1
            DSUM1=DSUM1 + DBLE(YM1(I,J))
 7335     CONTINUE
          AMEAN=REAL(DSUM1/DBLE(NR1))
          DO 7338 I=1,NR1
            YM1(I,J)=YM1(I,J) - AMEAN
 7338     CONTINUE
 7330   CONTINUE
      ELSEIF(IBPLSC.EQ.'GMEA')THEN
        DSUM1=0.0D0
        DO 7340 J=1,NC1
          DO 7345 I=1,NR1
            DSUM1=DSUM1 + DBLE(YM1(I,J))
 7345     CONTINUE
 7340   CONTINUE
        AMEAN=REAL(DSUM1/DBLE(NR1*NC1))
        DO 7349 J=1,NC1
          DO 7348 I=1,NR1
            YM1(I,J)=YM1(I,J) - AMEAN
 7348     CONTINUE
 7349   CONTINUE
      ENDIF
!
!     STEP 2: COMPUTE EUCLIDEAN NORM
!
      DSUM1=0.0D0
      DO 7361 I=1,NR1
      DO 7362 J=1,NC1
        DYM1=YM1(I,J)
        DSUM1=DSUM1+DYM1*DYM1
 7362 CONTINUE
 7361 CONTINUE
      DYM1=0.0D0
      IF(DSUM1.GT.0.0D0)DYM1=DSQRT(DSUM1)
      SCAL9=REAL(DYM1)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7369)DYM1
 7369   FORMAT('DYM1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     STEP 3: COMPUTE SINGULAR VALUE FACTORIZATION
!
      IERR2=0
      IJOB=22
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,   &
      YM2,MAXROM,Y2,IJOB,IERR2)
!
      S1=VECT9(1)
      S2=VECT9(2)
      AFACT1=S1**PBPLCO
      AFACT2=S2**PBPLCO
      SCAL9=(S1**2 + S2**2)/SCAL9**2
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
        WRITE(ICOUT,7366)PBPLCO,S1,S2,SCAL9
 7366   FORMAT('PBPLCO,S1,S2,SCAL9 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 7670 I=1,NTEMP1
        VECT9(I)=YM9(I,1)*AFACT1
        Y2(I)=YM9(I,2)*AFACT2
        Y3(I)=1.0
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
          WRITE(ICOUT,7371)I,VECT9(I),Y2(I),Y3(I)
 7371     FORMAT('I,VECT9(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 7670 CONTINUE
      ICNT=NTEMP1
      AFACT1=S1**(1.0-PBPLCO)
      AFACT2=S2**(1.0-PBPLCO)
      DO 7680 I=1,NTEMP2
        ICNT=ICNT+1
        VECT9(ICNT)=YM2(1,I)*AFACT1
        Y2(ICNT)=YM2(2,I)*AFACT2
        Y3(ICNT)=2.0
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TAR2')THEN
          WRITE(ICOUT,7381)I,ICNT,VECT9(I),Y2(I),Y3(I)
 7381     FORMAT('I,ICNT,VECT9(I),Y2(I),Y3(I) = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 7680 CONTINUE
!
      ITYP9='VECT'
      NVECT9=ICNT
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'TAR2')GO TO 9090
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATAR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
      A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
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
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GO TO 9039
      IF(NC1.LE.0)GO TO 9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO 9032 I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9049
      IF(NC2.LE.0)GO TO 9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO 9042 I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9059
      IF(NC9.LE.0)GO TO 9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9055 I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GO TO 9119
      DO 9112 I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GO TO 9129
      DO 9122 I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GO TO 9139
      DO 9132 I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GO TO 9149
      DO 9142 I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GO TO 9169
      DO 9162 I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9179
      IF(NC9.LE.0)GO TO 9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9172 I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
!
      IF(IMCASE.NE.'MASS')GO TO 9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9189
      IF(NC2.LE.0)GO TO 9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO 9182 I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
!CCCC WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
!9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      WRITE(ICOUT,9187)NR2
 9187 FORMAT('NR2 = ',I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
!
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE MATAR2
      SUBROUTINE MATAR3(YM1,NR1,NC1,YM2,NR2,NC2,NR3,NC3,   &
                        MAXROM,MAXCOM,MAXOBV,   &
                        Y1,N1,Y2,N2,Y3,N3,   &
                        Y4,N4,Y5,Y6,   &
                        INDEX,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        P,ABSE,RELE,AERROR,   &
                        YS1,YS2,YS3,YS4,   &
                        ASIG90,ASIG95,ASIG99,ASG995,   &
                        IMCASE,IUPFLG,IMSUBC,   &
                        ITYPA1,ITYPA2,ITYPA3,ITYPA4,NUMVAR,IWRITE,   &
                        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,   &
                        ITEMP5,ITEMP6,ITEMP7,   &
                        YM9,NR9,NC9,VECT9,NVECT9,SCAL9,ITYP9,   &
                        ICASS7,ISTARA,   &
                        IRELAT,CLWID,XSTART,XSTOP,   &
                        STME,STMEC,ST2T,ST2TC,STC,STT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT MATRIX     ARITHMETIC OPERATIONS
!              OF THE REAL DATA IN MATRICES YM1 AND YM2.
!              ADD SOME ADDITIONAL FUNCTIONALITY
!
!     OPERATIONS--QUADRATIC FORM (X'MX)
!                 1-SAMPLE HOTELLING T-SQUARE
!                 2-SAMPLE HOTELLING T-SQUARE
!                 POOLED SAMPLE VARIANCE-COVARIANCE MATRIX
!                 MATRIX <ROW/COLUMN> SCALE
!                 <ROW/COLUMN> <STATISTIC>
!                 PARTITION    <STATISTIC>
!                 MATRIX    <STATISTIC>
!                 MATRIX  BIN
!                 EUCLIDEAN <ROW/COLUMN> DISTANCE
!                 CHEBYCHEV <ROW/COLUMN> DISTANCE
!                 L1 NORM <ROW/COLUMN> DISTANCE
!                 MINKOWSKY <ROW/COLUMN> DISTANCE
!                 MAHALANOBIS <ROW/COLUMN> DISTANCE
!                 MATRIX MEAN (I.E., GRAND MEAN)
!                 MATRIX SUM
!                 MATRIX ADD ROW
!                 MATRIX DELETE ROW
!                 LINEAR COMBINATION
!                 VECTOR TIMES TRANSPOSE
!                 MATRIX GROUP MEAN
!                 MATRIX GROUP STANDARD DEVIATION
!                 CATCHER MATRIX
!                 MULTIVARIATE NORMAL RANDOM NUMBERS
!                 MULTINOMIAL RANDOM NUMBERS
!                 MULTINOMIAL PDF
!                 XTXINV MATRIX
!                 VARIANCE INFLATION FACTORS
!                 CONDITION INDICES
!                 CREATE MATRIX
!                 QR DECOMPOSITION (NOT DONE)
!                 PSEUDO INVERSE
!                 WISHART RANDOM NUMBERS
!                 INDEPENDENT UNIFORM RANDOM NUMBERS
!                 CORRELATED UNIFORM RANDOM NUMBERS
!                 MULTIVARIATE NORMAL CDF
!                 DIRICHLET RANDOM NUMBERS
!                 MATRIX BIN
!                 MATRIX PARTITION <STAT>
!                 MATRIX <STAT>
!                 MINIMAL SPANNING TREE
!                 MATRIX RENUMBER
!                 EDGES TO ADJACENCY MATRIX
!                 MATRIX <ROW/COLUMN> FIT
!                 VARIABLE TO MATRIX
!                 MATRIX TO VARIABLE
!                 MATRIX COMBINE ROWS
!                 MATRIX COMBINE COLUMNS
!                 GENERATE MATRIX <STAT>
!                 DEX CORE
!                 DEX CONFOUND
!                 DEX CHECK CLASSIC
!                 DEX CHECK CENTER POINT
!                 TWO WAY ANOVA DUMMY MATRIX
!
!     EXAMPLES--LET A1 = QUADRATIC FORM M X
!             --LET A1 = HOTELLING T-SQUARE M U0
!             --LET Y1 = MATRIX ROW MEAN M
!               LET Y1 = MATRIX COLUMN MEAN M
!
!     INPUT  ARGUMENTS--YM1 (REAL MATRIX)
!                     --NR1
!                     --NC1
!                     --YM2 (REAL MATRIX)
!                     --NR2
!                     --NC2
!                     --YM3 (REAL MATRIX)
!                     --NR3
!                     --NC3
!                     --Y1  (REAL VECTOR)
!                     --N1
!                     --Y2  (REAL VECTOR)
!                     --N2
!                     --Y3  (REAL VECTOR)
!                     --N3
!                     --Y4  (REAL VECTOR)
!                     --N4
!     OUTPUT ARGUMENTS--YM9 (REAL MATRIX)
!                     --NR9
!                     --NC9
!                     --VECT9 (REAL VECTOR)
!                     --NVECT9
!                     --SCAL9 (REAL SCALAR)
!                     --ITYP9
!
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT MATRIX YM9(.)
!           BEING IDENTICAL TO THE INPUT MATRIX YM1(.), YM2(.), OR YM3(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/06
!     ORIGINAL VERSION--JUNE      1998.
!     UPDATED         --MAY       2002. MULTIVARIATE NORM RAND NUMB
!     UPDATED         --MAY       2002. MULTINOMIAL RAND NUMB
!     UPDATED         --MAY       2002. WISHART RAND NUMB
!     UPDATED         --JUNE      2002. CATCHER MATRIX
!     UPDATED         --JUNE      2002. XTXINV MATRIX
!     UPDATED         --JUNE      2002. VARIANCE INFLATION FACTORS
!     UPDATED         --JUNE      2002. CONDITION NUMBERS
!     UPDATED         --JUNE      2002. CREATE MATRIX
!     UPDATED         --AUGUST    2002. USE "CMPSTA" TO COMPUTE
!                                       STATISTIC FOR
!                                       MATRIX <ROW/COLU> <STAT>
!     UPDATED         --APRIL     2003. FIX WISHART RANDOM NUMBERS
!     UPDATED         --APRIL     2003. MULTIVARIATE T RANDOM NUMBERS
!     UPDATED         --APRIL     2003. INDPENDENT UNIFORM RANDOM NUMB
!     UPDATED         --APRIL     2003. MULTIVARIATE NORMAL CDF
!     UPDATED         --APRIL     2003. MULTIVARIATE T CDF
!     UPDATED         --APRIL     2003. ARGUMENT LIST TO CMPSTA
!     UPDATED         --SEPTEMBER 2003. CORRELATED UNIFORM RANDOM NUMB
!     UPDATED         --JUNE      2005. MATRIX PARTITION <STAT>
!     UPDATED         --JUNE      2005. MATRIX <STAT>
!     UPDATED         --JULY      2005. MATRIX PARTITION <STAT>
!                                       EXTENDED TO UNEQUAL PARTITION
!                                       CASE
!     UPDATED         --MARCH     2006. MATRIX BIN
!     UPDATED         --MAY       2008. MATRIX RENUMBER
!     UPDATED         --JUNE      2008. EDGES TO ADJACENCY MATRIX
!     UPDATED         --SEPTEMBER 2008. ACTIVATE PSEUDO INVERSE COMMAND
!                                       (ACTUALLY RETURNS TRANSPOSE OF
!                                       PSEUDO INVERSE)
!     UPDATED         --JANUARY   2009. DISTINCTION BETWEEN DIRECTED AND
!                                       UNDIRECTED ADJACENCY MATRIX
!     UPDATED         --FEBRUARY  2010. MATRIX <ROW/COLUMN> FIT
!     UPDATED         --JUNE      2010. CALL LIST TO CMPSTA
!     UPDATED         --NOVEMBER  2010. VARIABLE TO MATRIX
!     UPDATED         --NOVEMBER  2010. MATRIX TO VARIABLE
!     UPDATED         --JANUARY   2011. MATRIX COMBINE ROWS
!     UPDATED         --JANUARY   2011. MATRIX COMBINE COLUMNS
!     UPDATED         --AUGUST    2017. GENERATE MATRIX <STAT>
!     UPDATED         --JANUARY   2018. DEX CORE
!     UPDATED         --JANUARY   2018. DEX CONFOUND
!     UPDATED         --AUGUST    2018. HAVE ALL DISTANCE MATRIX
!                                       (EUCLIDEAN, MINKOWSKY, BLOCK,
!                                       CHEBYCHEV) GO THROUGH A SINGLE
!                                       ROUTINE
!     UPDATED         --AUGUST    2018. ADDED ADDITIONAL DISTANCE
!                                       MATRIX OPTIONS
!     UPDATED         --SEPTEMBER 2018. DEX CHECK CENTER POINTS
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!     UPDATED         --NOVEMBER  2023. TWO WAY ANOVA DUMMY MATRIX
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      CHARACTER*4 IMCASE
      CHARACTER*4 ICASS7
      CHARACTER*4 ISTARA
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
!
      CHARACTER*4 IRELAT
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASE2
!
!CCCC MAY 2002. ADD FOLLOWING LINE
      LOGICAL LTF
!
!-----DOUBLE PRECISION STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
!---------------------------------------------------------------------
!
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
!
!     2021/01: UP FROM MAX OF 40 CHARACTERS TO MAX OF 80 CHARACTERS
!
      CHARACTER*80 STME(500)
      CHARACTER*80 STMEC(500)
      CHARACTER*80 ST2T(500)
      CHARACTER*80 ST2TC(500)
      CHARACTER*80 STC(500)
      CHARACTER*80 STT(500)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='MATA'
      ISUBN2='R3  '
      IERROR='NO'
!
      IYS1=(-999)
      IYS2=(-999)
      IYS3=(-999)
      IYS23=(-999)
      NRJ=(-999)
      NCJ=(-999)
      D999=(-999.0D0)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ATR3')THEN
!
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF MATAR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4
   52   FORMAT('IBUGA3,ISUBRO,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IMCASE,IMSUBC,IWRITE,NUMVAR
   53   FORMAT('IMCASE,IMSUBC,IWRITE,NUMVAR = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)YS1,YS2,YS3,YS4,Y6(1)
   55   FORMAT('YS1,YS2,YS3,YS4,Y6(1) = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)NR1,NC1
   61   FORMAT('NR1,NC1 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR1.GE.1 .AND. NC1.GE.1)THEN
          JMAX=NC1
          IF(JMAX.GT.10)JMAX=10
          DO 62 I=1,NR1
            WRITE(ICOUT,63)I,(YM1(I,J),J=1,JMAX)
   63       FORMAT('I,YM1(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)NR2,NC2
   71   FORMAT('NR2,NC2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR2.GE.1 .AND. NC2.GE.1)THEN
          JMAX=NC2
          IF(JMAX.GT.10)JMAX=10
          DO 72 I=1,NR2
            WRITE(ICOUT,73)I,(YM2(I,J),J=1,JMAX)
   73       FORMAT('I,YM2(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   72     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)NR3,NC3
   81   FORMAT('NR3,NC3 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NR3.GE.1 .AND. NC3.GE.1)THEN
          JMAX=NC3
          IF(JMAX.GT.10)JMAX=10
          DO 82 I=1,NR3
            WRITE(ICOUT,83)I,(YM9(I,J),J=1,JMAX)
   83       FORMAT('I,YM9(I,.) = ',I8,10E10.3)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)N1
  111   FORMAT('N1 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GE.1)THEN
          DO 112 I=1,N1
            WRITE(ICOUT,113)I,Y1(I)
  113       FORMAT('I,Y1(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  112     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,121)N2
  121   FORMAT('N2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N2.GE.1)THEN
          DO 122 I=1,N2
            WRITE(ICOUT,123)I,Y2(I)
  123       FORMAT('I,Y2(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  122     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)N3
  131   FORMAT('N3 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N3.GE.1)THEN
          DO 132 I=1,N3
            WRITE(ICOUT,133)I,Y3(I)
  133       FORMAT('I,Y3(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  132     CONTINUE
        ENDIF
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,141)N4
  141   FORMAT('N4 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N4.GE.1)THEN
          DO 142 I=1,N4
            WRITE(ICOUT,143)I,Y4(I)
  143       FORMAT('I,Y4(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
  142     CONTINUE
        ENDIF
!
      ENDIF
!
!               **************************************************
!               **  CARRY OUT MATRIX     ARITHMETIC OPERATIONS  **
!               **************************************************
!
      DNR1=NR1
      DNC1=NC1
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK NUMBER OF INPUT OBSERVATIONS.   **
!               ********************************************
!
      IF(IMCASE.EQ.'CRMA')GO TO 8500
      IF(IMCASE.EQ.'GMST')GO TO 8550
      IF(IMCASE.EQ.'CORE')GO TO 10800
      IF(IMCASE.EQ.'CONF')GO TO 10900
      IF(IMCASE.EQ.'CKCL')GO TO 11000
      IF(IMCASE.EQ.'CKCP')GO TO 11100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NR1.LE.0)GO TO 1100
      IF(ITYPA1.EQ.'MATR'.AND.NUMVAR.GE.1.AND.NC1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NR2.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'MATR'.AND.NUMVAR.GE.2.AND.NC2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NR3.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'MATR'.AND.NUMVAR.GE.3.AND.NC3.LE.0)GO TO 1100
!
      IF(ITYPA1.EQ.'VARI'.AND.NUMVAR.GE.1.AND.N1.LE.0)GO TO 1100
      IF(ITYPA2.EQ.'VARI'.AND.NUMVAR.GE.2.AND.N2.LE.0)GO TO 1100
      IF(ITYPA3.EQ.'VARI'.AND.NUMVAR.GE.3.AND.N3.LE.0)GO TO 1100
!
      GO TO 1190
!
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
!
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
      GO TO 9000
!
 1190 CONTINUE
!
!               *********************************
!               **  STEP 12--                  **
!               **  BRANCH TO THE PROPER CASE  **
!               *********************************
!
      IF(IMCASE.EQ.'MPVC')GO TO 5600
      IF(IMCASE.EQ.'MQFO')GO TO 5800
      IF(IMCASE.EQ.'MHT1')GO TO 5900
      IF(IMCASE.EQ.'MHT2')GO TO 5700
      IF(IMCASE.EQ.'MROW')GO TO 6000
      IF(IMCASE.EQ.'MCOL')GO TO 6100
!
      IF(IMCASE.EQ.'MDER')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MDEC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MDKR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MDKC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MDBR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MDBC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MDCR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MDCC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MCSR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MCSC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MCDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MCDC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MZSR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MASC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MZDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MADC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MJSR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MJSC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MJDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MJDC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MPDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MPDC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MHDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MHDC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MXDR')THEN
        ICASE='ROW '
        GO TO 6200
      ENDIF
      IF(IMCASE.EQ.'MXDC')THEN
        ICASE='COLU'
        GO TO 6200
      ENDIF
!
      IF(IMCASE.EQ.'MRSC')THEN
        ICASE='ROW '
        GO TO 6500
      ENDIF
      IF(IMCASE.EQ.'MCSC')THEN
        ICASE='COLU'
        GO TO 6500
      ENDIF
!
      IF(IMCASE.EQ.'MDMR')THEN
        ICASE='ROW '
        GO TO 6600
      ENDIF
      IF(IMCASE.EQ.'MDMC')THEN
        ICASE='COLU'
        GO TO 6600
      ENDIF
!
      IF(IMCASE.EQ.'MQRD')GO TO 6300
      IF(IMCASE.EQ.'MPIN')GO TO 6400
      IF(IMCASE.EQ.'MAMM')GO TO 7000
      IF(IMCASE.EQ.'MSUM')GO TO 7030
      IF(IMCASE.EQ.'MAAR')GO TO 7100
      IF(IMCASE.EQ.'MADR')GO TO 7200
      IF(IMCASE.EQ.'MADM')GO TO 7300
      IF(IMCASE.EQ.'MALC')GO TO 7400
      IF(IMCASE.EQ.'MAVT')GO TO 7500
      IF(IMCASE.EQ.'MAGM')GO TO 7600
      IF(IMCASE.EQ.'MAGS')GO TO 7700
      IF(IMCASE.EQ.'MVRN')GO TO 7800
      IF(IMCASE.EQ.'MURN')GO TO 7900
      IF(IMCASE.EQ.'MPDF')GO TO 7950
      IF(IMCASE.EQ.'WIRN')GO TO 8000
      IF(IMCASE.EQ.'MACA')GO TO 8100
      IF(IMCASE.EQ.'XTXI')GO TO 8200
      IF(IMCASE.EQ.'VINF')GO TO 8300
      IF(IMCASE.EQ.'CIND')GO TO 8400
      IF(IMCASE.EQ.'CRMA')GO TO 8500
      IF(IMCASE.EQ.'GMST')GO TO 8550
      IF(IMCASE.EQ.'IURN')GO TO 8600
      IF(IMCASE.EQ.'NCDF')GO TO 8700
      IF(IMCASE.EQ.'TCDF')GO TO 8800
      IF(IMCASE.EQ.'TCDF')GO TO 8800
      IF(IMCASE.EQ.'MTRN')GO TO 8900
      IF(IMCASE.EQ.'DIRN')GO TO 8950
      IF(IMCASE.EQ.'DPDF')GO TO 9300
      IF(IMCASE.EQ.'DLPD')GO TO 9300
      IF(IMCASE.EQ.'INRN')GO TO 9400
      IF(IMCASE.EQ.'MPAR')GO TO 9500
      IF(IMCASE.EQ.'MGRA')GO TO 9600
      IF(IMCASE.EQ.'MATB')GO TO 9700
      IF(IMCASE.EQ.'MARB')GO TO 9700
      IF(IMCASE.EQ.'MSPT')GO TO 9800
      IF(IMCASE.EQ.'MSP2')GO TO 9900
      IF(IMCASE.EQ.'MARN')GO TO 10000
      IF(IMCASE.EQ.'ADMA')GO TO 10100
      IF(IMCASE.EQ.'ADMD')GO TO 10100
      IF(IMCASE.EQ.'MFTR')GO TO 10200
      IF(IMCASE.EQ.'MFTC')GO TO 10300
      IF(IMCASE.EQ.'VMAT')GO TO 10400
      IF(IMCASE.EQ.'MVAR')GO TO 10500
      IF(IMCASE.EQ.'MCRO')GO TO 10600
      IF(IMCASE.EQ.'MCCO')GO TO 10700
      IF(IMCASE.EQ.'2WDM')GO TO 11200
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** INTERNAL ERROR IN MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)IMCASE
 1212 FORMAT('      IMCASE NOT MATCHED.  IMCASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 56--                                        **
!               **  TREAT THE POOLED VARIANCE-COVARIANCE MATRIX  CASE**
!               *******************************************************
!
 5600 CONTINUE
!
      IF(ITYPA2.EQ.'VARI')GO TO 5650
!
      IF(NC1.EQ.NC2)GO TO 5609
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
      GO TO 9000
 5609 CONTINUE
!
      CALL VARPOO(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,NR2,   &
      DTEMP1,IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
 5650 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 5659
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
 5654 FORMAT('      MUST EQUAL THE NUMBER OF ROWS IN THE GROUP-ID ',   &
      'VARIABLE..')
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
      GO TO 9000
 5659 CONTINUE
!
      CALL VARPO2(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,MAXROM,   &
      Y2,Y3,INDEX,NK,DTEMP1,IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 57--                                        **
!               **  TREAT THE MATRIX 2-SAMPLE HOTELLING T-SQUARE CASE**
!               *******************************************************
!
 5700 CONTINUE
!
      IF(NC1.EQ.NC2)GO TO 5709
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
      GO TO 9000
 5709 CONTINUE
!
      CALL HTTSQ2(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NR2,NC1,   &
      TSTAT,ASIG90,ASIG95,ASIG99,ASG995,   &
      DTEMP1,Y1,Y2,Y3,INDEX,   &
      IBUGA3,IERROR)
!
      SCAL9=TSTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 58--                                 **
!               **  TREAT THE MATRIX QUADRATIC FORM  CASE     **
!               **  QUADRATIC FORM = x'Mx                     **
!               **  x IS A VECTOR AND M IS A MATRIX           **
!               ************************************************
!
 5800 CONTINUE
!
      IF(NR1.EQ.NC1)GO TO 5809
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
      GO TO 9000
 5809 CONTINUE
!
      IF(N2.EQ.NR1)GO TO 5859
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
      GO TO 9000
 5859 CONTINUE
!
      CALL QUAFRM(YM1,MAXROM,MAXCOM,NR1,NC1,Y2,IWRITE,SCAL9,   &
                  IBUGA3,IERROR)
!
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 59--                                        **
!               **  TREAT THE MATRIX 1-SAMPLE HOTELLING T-SQUARE CASE**
!               **  H0: U=U0                                         **
!               **  T-SQUARE = N*(XBAR-U0)'*SINV*(XBAR-U0)           **
!               **  WHERE SINV = SAMPLE VARIANCE-COVARIANCE MATRIX   **
!               *******************************************************
!
 5900 CONTINUE
!
      IF(NC1.EQ.N2)GO TO 5909
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
      GO TO 9000
 5909 CONTINUE
!
      CALL HTTSQ1(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,   &
      TSTAT,ASIG90,ASIG95,ASIG99,ASG995,   &
      DTEMP1,Y2,Y1,Y3,INDEX,   &
      IBUGA3,IERROR)
!
      SCAL9=TSTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 60--                                 **
!               **  TREAT THE MATRIX ROW STATISTIC CASE       **
!               ************************************************
!
!CCCC IMPLEMENTED JULY 1993.
 6000 CONTINUE
!
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
!
      DO 6010 I=1,NR1
        DO 6015 J=1,NC1
          Y1(J)=YM1(I,J)
 6015   CONTINUE
        ASTAT=0.0
        CALL CMPSTA(   &
        Y1,Y2,Y2,Y3,Y4,Y5,Y6,Y6,MAXNXT,NC1,NC1,NC1,NUMV2,ICASS7,ISTARA,   &
        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
        DTEMP1,DTEMP2,DTEMP3,   &
        ASTAT,   &
        ISUBRO,IBUGA3,IERROR)
        VECT9(I)=ASTAT
 6010 CONTINUE
!
      ITYP9='VECT'
      NR9=1
      NC9=1
      NVECT9=NR1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 61--                                      **
!               **  TREAT THE MATRIX COLUMN STATISTIC        CASE  **
!               *****************************************************
!
 6100 CONTINUE
!
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
!
      DO 6110 I=1,NC1
        DO 6115 J=1,NR1
          Y1(J)=YM1(J,I)
 6115   CONTINUE
        ASTAT=0.0
        CALL CMPSTA(   &
        Y1,Y2,Y2,Y3,Y4,Y5,Y6,Y6,MAXNXT,NR1,NR1,NR1,NUMV2,ICASS7,ISTARA,   &
        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
        DTEMP1,DTEMP2,DTEMP3,   &
        ASTAT,   &
        ISUBRO,IBUGA3,IERROR)
        VECT9(I)=ASTAT
 6110 CONTINUE
!
      ITYP9='VECT'
      NR9=1
      NC9=1
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 62--                                      **
!               **  TREAT THE MATRIX EUCLIDEAN DISTANCE      CASE  **
!               **            MATRIX CHEBYCHEV DISTANCE      CASE  **
!               **            MATRIX MINKOWSKY DISTANCE      CASE  **
!               **            MATRIX BLOCK     DISTANCE      CASE  **
!               **            MATRIX COSINE    DISTANCE      CASE  **
!               **            MATRIX COSINE    SIMILARITY    CASE  **
!               **            MATRIX JACCARD   DISTANCE      CASE  **
!               **            MATRIX JACCARD   SIMILARITY    CASE  **
!               *****************************************************
!
 6200 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
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
!
      CALL EUCDIS(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,   &
                  ICASE,ICASE2,P,IWRITE,   &
                  Y1,Y2,   &
                  IBUGA3,ISUBRO,IERROR)
!
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
      GO TO 9000
!
!               *********************************************
!               **  STEP 63--                              **
!               **  TREAT THE MATRIX QR       DECOMP CASE  **
!               **  REFERENCE--LINPACK USER'S GUIDE        **
!               *********************************************
!
 6300 CONTINUE
!
!CCCC IF(NR1.LE.MAXCOM)GO TO 6309
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6301)
!6301 FORMAT('***** ERROR IN MATAR2--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6302)
!6302 FORMAT('      FOR MATRIX SINGULAR VALUE DECOMPOSITION,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6303)
!6303 FORMAT('      THE NUMBER OF ROWS IN THE MATRIX')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6304)
!6304 FORMAT('      CAN NOT EXCEED ')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6305)
!6305 FORMAT('      THE MAXIMUM NUMBER OF COLUMNS IN THE MATRIX;')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6306)
!6306 FORMAT('      SUCH WAS NOT THE CASE HERE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6307)NR1
!6307 FORMAT('            NUMBER OF ROWS    =',I8)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,6308)MAXCOM
!6308 FORMAT('            MAXIMUM NUMBER OF COLUMNS =',I8)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!
!6309 CONTINUE
      DO 6322 J=1,MAXCOM
      DO 6321 I=1,MAXROM
      YM9(I,J)=0.0
      YM2(I,J)=0.0
 6321 CONTINUE
 6322 CONTINUE
!
      IERR2=0
      IJOB=11
      NTEMP1=NR1
      NTEMP2=NC1
      CALL SSVDC(YM1,MAXROM,NTEMP1,NTEMP2,VECT9,Y1,YM9,MAXROM,   &
      YM2,MAXROM,Y2,IJOB,IERR2)
!
      ITYP9='MATR'
      MM=NR1
      IF(MM.GT.NC1)MM=NC1
      NR9=NR1
      NC9=NR1
      NR2=NC1
      NC2=NC1
      NVECT9=MM
      IUPFLG='FULL'
      GO TO 9000
!
!               ******************************************************
!               **  STEP 64--                                       **
!               **  TREAT THE MATRIX PSEUDO INVERSE         CASE    **
!               ******************************************************
!
 6400 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      KTEMP=0
      CALL MATMPI(YM1,Y1,Y2,Y3,YM2,NR1,NC1,MAXROM,MAXROM,KTEMP,IFLAG)
!
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
        GO TO 9000
      ELSE
        DO 6450 J=1,NC1
          DO 6460 I=1,NR1
            YM9(I,J)=YM1(I,J)
 6460     CONTINUE
 6450   CONTINUE
      ENDIF
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 65--                                      **
!               **  TREAT THE MATRIX SCALE                   CASE  **
!               *****************************************************
!
 6500 CONTINUE
!
      IWRITE='OFF'
      CALL MATSCA(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,Y1,Y2,Y3,   &
      IMATSC,ICASE,IWRITE,   &
      IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 66--                                      **
!               **  TREAT THE MATRIX MAHALONOBIS DISTANCE    CASE  **
!               *****************************************************
!
 6600 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
      IWRITE='OFF'
      CALL MAHDIS(YM1,YM2,YM9,MAXROM,MAXCOM,NR1,NC1,   &
      Y1,Y2,INDEX,DTEMP1,   &
      ICASE,IWRITE,IBUGA3,IERROR)
!
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
      GO TO 9000
!
!               *****************************************************
!               **  STEP 70--                                      **
!               **  TREAT THE MATRIX MEAN                    CASE  **
!               *****************************************************
!
 7000 CONTINUE
!
      ITYP9='SCAL'
      D999=0.0D0
      DO 7010 J=1,NC1
        DO 7020 I=1,NR1
          D999=D999+DBLE(YM1(I,J))
 7020   CONTINUE
 7010 CONTINUE
      D999=D999/DBLE(NR1*NC1)
      SCAL9=REAL(D999)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 70.B--                                    **
!               **  TREAT THE MATRIX SUM                     CASE  **
!               *****************************************************
!
 7030 CONTINUE
!
      ITYP9='SCAL'
      D999=0.0D0
      DO 7040 J=1,NC1
        DO 7050 I=1,NR1
          D999=D999+DBLE(YM1(I,J))
 7050   CONTINUE
 7040 CONTINUE
      SCAL9=REAL(D999)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 71--                                      **
!               **  TREAT THE MATRIX ADD ROW                 CASE  **
!               *****************************************************
!
 7100 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
      DO 7110 J=1,NC1
         DO 7120 I=1,NR1
           YM9(I,J)=YM1(I,J)
 7120    CONTINUE
         YM9(NR1+1,J)=Y2(J)
 7110 CONTINUE
!
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1+1
      IUPFLG='SUBS'
      GO TO 9000
!               *****************************************************
!               **  STEP 72--                                      **
!               **  TREAT THE MATRIX DELETE ROW              CASE  **
!               *****************************************************
!
 7200 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
      DO 7210 J=1,NC1
         ICOUNT=0
         DO 7220 I=1,NR1
           IF(IYS2.NE.I)THEN
             ICOUNT=ICOUNT+1
             YM9(ICOUNT,J)=YM1(I,J)
           ENDIF
 7220    CONTINUE
 7210 CONTINUE
!
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1-1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 73--                                      **
!               **  TREAT THE DISTANCE FROM MEAN             CASE  **
!               *****************************************************
!
 7300 CONTINUE
!
      ICASE='COLU'
      CALL VARCOV(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,DTEMP1,   &
                  ICASE,IBUGA3,IERROR)
!
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
        GO TO 9000
      ENDIF
 7371 FORMAT('*** ERROR FROM MATAR3: UNABLE TO COMPUTE THE INVERSE OF ',   &
             'THE COVARIANCE MATRIX.')
 7372 FORMAT('    PROBLEM: SOME COLUMNS ARE LINEARLY DEPDENDENT ON ',   &
             ' OTHER COLUMNS.')
 7373 FORMAT('    SUGGESTED SOLUTION: WORK WITH A SUBSET OF THE ',   &
             'ORIGINAL COLUMNS.')
!
      IJOB=1
      CALL SGEDI(YM2,MAXROM,NC1,INDEX,Y1,Y2,IJOB)
!
      IWRITE='OFF'
      DO 7320 I=1,NR1
        DO 7330 J=1,NC1
          Y3(J)=YM1(I,J)-REAL(DTEMP1(J))
 7330   CONTINUE
        CALL QUAFRM(YM2,MAXROM,MAXCOM,NC1,NC1,Y3,IWRITE,SCAL9,   &
                    IBUGA3,IERROR)
        VECT9(I)=SCAL9
 7320 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 74--                                      **
!               **  TREAT THE LINEAR COMBINATION             CASE  **
!               *****************************************************
!
 7400 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
!CCCC 2024/07: THE FOLLOWING 3 LINES ARE NOT NEEDED
!
!CCCC ICASE='COLU'
!CCCC CALL VARCOV(YM1,YM2,MAXROM,MAXCOM,NR1,NC1,DTEMP1,
!CCCC1            ICASE,IBUGA3,IERROR)
!
      DO 7430 J=1,NR1
        DSUM1=0.0D0
        DO 7440 L=1,NC1
          DSUM1=DSUM1 + DBLE(Y2(L))*DBLE(YM1(J,L))
 7440     CONTINUE
          VECT9(J)=REAL(DSUM1)
 7430   CONTINUE
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='SUBS'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 75--                                      **
!               **  TREAT THE VECTOR TIMES TRANSPOSE         CASE  **
!               *****************************************************
!
 7500 CONTINUE
!
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
!
      DO 7520 I=1,N1
        DO 7530 J=1,N1
          YM9(I,J)=Y1(I)*Y1(J)
 7530   CONTINUE
 7520 CONTINUE
!
      ITYP9='MATR'
      NR9=N1
      NC9=N1
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 76--                                        **
!               **  TREAT THE MATRIX GROUP MEANS                CASE **
!               *******************************************************
!
 7600 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 7609
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
      GO TO 9000
 7609 CONTINUE
!
      CALL GRPMEA(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,   &
      Y2,Y3,INDEX,N2,NK,Y4,IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NK
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *******************************************************
!               **  STEP 77--                                        **
!               **  TREAT THE MATRIX GROUP STANDARD DEVIATIONS  CASE **
!               *******************************************************
!
 7700 CONTINUE
!
      IF(NR1.EQ.N2)GO TO 7709
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
      GO TO 9000
 7709 CONTINUE
!
      CALL GRPSD(YM1,YM9,MAXROM,MAXCOM,NR1,NC1,   &
      Y2,Y3,INDEX,N2,NK,Y4,IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NK
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 78--                                        **
!     **  TREAT THE MULTIVARIATE NORM RANDOM NUMBERS  CASE **
!     *******************************************************
!
 7800 CONTINUE
!
      IF(N1.EQ.NR2)GO TO 7809
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
      GO TO 9000
 7809 CONTINUE
!
      NTEMP=INT(YS3)
      LDSIG=MAXROM
      LTF=.TRUE.
      IFLAG=0
!
      DO 7820 I=1,NTEMP
        CALL RDMNOR(Y1,YM2,LDSIG,NR2,LTF,Y4,IFLAG,ISEED)
        IF(IFLAG.EQ.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7821)
 7821     FORMAT('***** ERROR IN MATARI--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7822)
 7822     FORMAT('      FOR THE MULTIVARIATE NORMAL RANDOM NUMBERS ',   &
                 'CASE,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7823)
 7823     FORMAT('      UNABLE TO COMPUTE THE CHOLESKY DECOMPOSITION ',   &
                 'OF THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7824)
 7824     FORMAT('      SIGMA MATRIX.  THIS IMPLIES SIGMA IS NOT ',   &
                 'POSITIVE DEFINITE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7825)
 7825     FORMAT('      THE MULTIVARIATE RANDOM NUMBERS WERE NOT ',   &
                 'GENERATED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        DO 7830 J=1,NR2
          YM9(I,J)=Y4(J)
 7830   CONTINUE
 7820 CONTINUE
!
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR2
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 79--                                        **
!     **  TREAT THE MULTINOMIAL  RANDOM NUMBERS       CASE **
!     **  LET M = MULTINOMIAL RANDOM NUMBERS P N NEVENTS   **
!     *******************************************************
!
 7900 CONTINUE
!
      DSUM1=0.0D0
      DO 7909 I=1,N1
        DSUM1=DSUM1 + DBLE(Y1(I))
        IF(Y1(I).LE.0.0 .OR. Y1(I).GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7911)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7901)
 7901     FORMAT('      THE SPECIFIED PROBABILITIES MUST BE IN ',   &
                 'THE INTERVAL (0,1).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7903)I,Y1(I)
 7903     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
          GO TO 9000
        ENDIF
 7909 CONTINUE
!
      NTRIAL=INT(YS2+0.5)
      NEVENT=INT(YS3+0.5)
!
      IF(NTRIAL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7911)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7916)
 7916   FORMAT('      THE NUMBER OF TRIALS IS LESS THAN 1.  ',   &
               'NTRIALS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(NEVENT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7911)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7918)
 7918   FORMAT('      THE NUMBER OF EVENTS IS LESS THAN 1.  ',   &
               'NEVENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NCAT=N1
      IERROR='NO'
!
      DO 7920 I=1,NEVENT
        CALL MULRAN(NTRIAL,Y1,NCAT,ITEMP1,ISEED,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        DO 7930 J=1,NCAT
          YM9(I,J)=REAL(ITEMP1(J))
 7930   CONTINUE
 7920 CONTINUE
!
      ITYP9='MATR'
      NR9=NEVENT
      NC9=NCAT
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 79.5--                                      **
!     **  TREAT THE MULTINOMIAL PDF                   CASE **
!     **  LET M = MULTINOMIAL PDF X P                      **
!     *******************************************************
!
 7950 CONTINUE
!
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
 7957   FORMAT('            NUMBER OF ROWS FOR NUMBER OF SUCCESSES = '   &
              ,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7958)N2
 7958   FORMAT('            NUMBER OF ROWS FOR PROBABILITY OF ',   &
               'SUCCESS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 7960 I=1,N1
        IF(Y1(I).GE.0.0)THEN
          Y1(I)=REAL(INT(Y1(I)+0.1))
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7951)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7961)
 7961     FORMAT('      THE NUMBER OF SUCCESSES MUST BE A ',   &
                 'NON-NEGATIVE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7963)I,Y1(I)
 7963     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
 7960 CONTINUE
!
      DSUM1=0.0D0
      DO 7970 I=1,N1
        DSUM1=DSUM1 + DBLE(Y2(I))
        IF(Y2(I).LE.0.0 .OR. Y2(I).GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7951)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7971)
 7971     FORMAT('      THE SPECIFIED PROBABILITIES MUST BE IN ',   &
                 'THE INTERVAL (0,1).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7973)I,Y2(I)
 7973     FORMAT('      ROW ',I8,' = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
          GO TO 9000
        ENDIF
 7970 CONTINUE
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 7990 I=1,N1
        DSUM1=DSUM1+DBLE(Y1(I))
        DSUM2=DSUM2+DBLE(Y2(I))
 7990 CONTINUE
      DN=DSUM1
      DNORM=DSUM2
!
      NTRIAL=INT(DN)
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DLNPDF=DLNGAM(DN+1.0D0)
!
      DO 7992 I=1,N1
        DLNPDF=DLNPDF - DLNGAM(DBLE(Y1(I) + 1.0D0))
 7992 CONTINUE
      DO 7995 I=1,N1
        DLNPDF=DLNPDF + DLOG(DBLE(Y2(I))/DNORM)*DBLE(Y1(I))
 7995 CONTINUE
!
      IF(DLNPDF.LT.LOG(CPUMAX))THEN
        DLNPDF=DEXP(DLNPDF)
      ELSE
        WRITE(ICOUT,7998)
 7998   FORMAT('***** WARNING: LOGARITHM OF MULTINOMIAL PDF ',   &
               'RETURNED TO AVOID OVERFLOW.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      SCAL9=REAL(DLNPDF)
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 80--                                        **
!     **  TREAT THE WISHART      RANDOM NUMBERS       CASE **
!     **  LET M = WISHART RANDOM NUMBERS MU SIGMA N        **
!     *******************************************************
!
 8000 CONTINUE
!
      IF(N1.EQ.NR2)GO TO 8009
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
      GO TO 9000
 8009 CONTINUE
!
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
 8013   FORMAT('      THE NUMBER OF ROWS IN THE SIGMA MATRIX MUST ',   &
               'EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      THE NUMBER OF COLUMNS; SUCH WAS NOT THE CASE ',   &
               'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8017)NR1
 8017   FORMAT('            NUMBER OF ROWS    =',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8018)NC1
 8018   FORMAT('            NUMBER OF COLUMNS =',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL SPOCO(YM2,MAXROM,NR2,RCOND,Y4,INFO)
!
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
!
      WRITE(ICOUT,8061)RCOND
      CALL DPWRST('XXX','TEXT ')
 8061 FORMAT('THE RECIPROCAL CONDITION NUMBER FOR THE SIGMA MATRIX = ',   &
             E15.7)
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
!
      ICOUNT=0
      DO 8080 I=1,NR2
      DO 8082 J=I,NC2
        IF(J.GE.I)THEN
          ICOUNT=ICOUNT+1
          Y2(ICOUNT)=YM2(I,J)
        ENDIF
 8082 CONTINUE
 8080 CONTINUE
!
!
      NTEMP=INT(YS3)
      NP=NR2
      NNP=NP*(NP+1)/2
!
      CALL WSHRT(Y2,NTEMP,NP,NNP,Y3,Y4,ISEED)
!
      ICOUNT=0
      DO 8090 J=1,NP
        DO 8092 I=1,NP
          IF(I.LE.J)THEN
            ICOUNT=ICOUNT+1
            YM9(I,J)=Y4(ICOUNT)
            IF(I.NE.J)YM9(J,I)=YM9(I,J)
          ENDIF
 8092   CONTINUE
 8090 CONTINUE
!
      ITYP9='MATR'
      NR9=NP
      NC9=NP
      IUPFLG='FULL'
      GO TO 9000
!
!               ***********************************************
!               **  STEP 81--                                **
!               **  TREAT THE CATCHER MATRIX CASE            **
!               **  C = X(X'X)**(-1)                         **
!               ***********************************************
!
 8100 CONTINUE
!
      CALL CATCHR(YM1,YM2,YM9,Y1,Y2,INDEX,   &
      MAXROM,MAXCOM,NR1,NC1,   &
      IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ***********************************************
!               **  STEP 82--                                **
!               **  TREAT THE (X'X)**(-1) MATRIX CASE        **
!               **  C = X(X'X)**(-1)                         **
!               ***********************************************
!
 8200 CONTINUE
!
      CALL XTXINV(YM1,YM9,Y1,Y2,INDEX,   &
      MAXROM,MAXCOM,NR1,NC1,   &
      IBUGA3,IERROR)
!
      ITYP9='MATR'
      NR9=NC1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 83--                                 **
!               **  TREAT THE VARIANCE INFLATION FACTORS CASE **
!               ************************************************
!
 8300 CONTINUE
!
      CALL CATCHR(YM1,YM2,YM9,Y1,Y2,INDEX,   &
      MAXROM,MAXCOM,NR1,NC1,   &
      IBUGA3,IERROR)
!
      DO 8310 J=1,NC1
        DSUM1=0.0D0
        DSUM2=0.0D0
        DO 8320 I=1,NR1
          DSUM1=DSUM1 + DBLE(YM9(I,J))**2
          DSUM2=DSUM2 + DBLE(YM1(I,J))
 8320   CONTINUE
        DMEAN=DSUM2/DBLE(NR1)
        DSUM2=0.0D0
        DO 8330 I=1,NR1
          DSUM2=DSUM2 + (DBLE(YM1(I,J)) - DMEAN)**2
 8330   CONTINUE
        VECT9(J)=REAL(DSUM1*DSUM2)
 8310 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ***********************************************
!               **  STEP 84--                                **
!               **  TREAT THE CONDITION INDICES CASE         **
!               **  (USEFUL FOR REGRESSION DIAGNOSTICS)      **
!               ***********************************************
!
 8400 CONTINUE
!
!  SCALE DESIGN MATRIX
!
      DO 8410 J=1,NC1
        DSUM1=0.0D0
        DO 8420 I=1,NR1
          DSUM1=DSUM1 + DBLE(YM1(I,J))*DBLE(YM1(I,J))
 8420   CONTINUE
        DSUM1=DSQRT(DSUM1)
        DO 8430 I=1,NR1
          YM1(I,J)=YM1(I,J)/REAL(DSUM1)
 8430   CONTINUE
 8410 CONTINUE
!
!  COMPUTE SINGULAR VALUES OF SCALED MATRIX
!
      IERR2=0
      IJOB=0
      CALL SSVDC(YM1,MAXROM,NR1,NC1,VECT9,Y1,YM1,MAXROM,   &
      YM1,MAXROM,Y2,IJOB,IERR2)
!
      DO 8440 I=1,NC1
        VECT9(I)=VECT9(I)*VECT9(I)
 8440 CONTINUE
!
      CALL MAXIM(VECT9,NC1,IWRITE,XMAX,IBUGA3,IERROR)
      DO 8450 I=1,NC1
        IF(VECT9(I).NE.0.0)THEN
          VECT9(I)=XMAX/VECT9(I)
        ELSE
          VECT9(I)=0.0
        ENDIF
 8450 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ***********************************************
!               **  STEP 85--                                **
!               **  TREAT THE CREATE MATRIX  CASE            **
!               **  LET M = CREATE MATRIX V1 V2 ... VK       **
!               **  NOTE: MOST OF THE REAL WORK OF THIS      **
!               **  FUNCTION ACTUALLY DONE IN DPMAT2, HERE   **
!               **  SIMPLY DOING A MATRIX COPY.              **
!               ***********************************************
!
 8500 CONTINUE
!
      DO 8510 J=1,NC1
        DO 8520 I=1,NR1
          YM9(I,J)=YM1(I,J)
 8520   CONTINUE
 8510 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               ****************************************************
!               **  STEP 85B-                                     **
!               **  TREAT THE GENERATE MATRIX  CASE               **
!               **  LET M = GENERATE MATRIX <STAT> V1 V2 ... VK   **
!               **  NOTE: MOST OF THE REAL WORK OF THIS           **
!               **  FUNCTION ACTUALLY DONE IN DPMAT2, HERE        **
!               **  SIMPLY DOING A MATRIX COPY.                   **
!               ****************************************************
!
 8550 CONTINUE
!
      DO 8560 J=1,NC1
        DO 8570 I=1,NR1
          YM9(I,J)=YM1(I,J)
 8570   CONTINUE
 8560 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!     *********************************************************************
!     **  STEP 86--                                                      **
!     **  TREAT THE INDEPENDENT UNIFORM RANDOM NUMBERS  CASE             **
!     **    LET M = INDEPENDENT UNIFORM RANDOM NUMBER LOWLIM UPPLIM NP   **
!     *********************************************************************
!
 8600 CONTINUE
!
      NROW=INT(YS3 + 0.1)
      NCOL=N1
!
      DO 8620 J=1,NCOL
        ATEMP1=AMIN1(Y1(J),Y2(J))
        ATEMP2=ABS(Y2(J)-Y1(J))
        CALL UNIRAN(NROW,ISEED,Y4)
        DO 8630 I=1,NROW
          YM9(I,J)=ATEMP1 + ATEMP2*Y4(I)
 8630   CONTINUE
 8620 CONTINUE
!
      ITYP9='MATR'
      NR9=NROW
      NC9=NCOL
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 87--                                        **
!     **  TREAT THE MULTIVARIATE NORMAL CDF           CASE **
!     *******************************************************
!
 8700 CONTINUE
!
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
        GO TO 9000
      ELSE
        N=NR1
      ENDIF
!
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
 8717     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',   &
                 '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8718)N2
 8718     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
 8727     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',   &
                 '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8728)N2
 8728     FORMAT('            NUMBER OF ROWS FOR THE LOWER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8729)N3
 8729     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      DO 8741 I=1,N
        DTEMP1(I)=0.0D0
        DTEMP2(I)=0.0D0
        DTEMP3(I)=0.0D0
 8741 CONTINUE
      ICNT=0
      DO 8760 J=1,N
        DO 8765 I=1,N
          IF(J.LT.I)THEN
            ICNT=ICNT+1
            INDX=J + ((I-2)*(I-1))/2
            DTEMP1(INDX)=DBLE(YM1(I,J))
          ENDIF
 8765   CONTINUE
 8760 CONTINUE
!
      IF(N3.EQ.0)THEN
        DO 8770 I=1,N
          ITEMP1(I)=0
          DTEMP3(I)=DBLE(Y2(I))
          DTEMP2(I)=DBLE(Y2(I))
 8770   CONTINUE
      ELSE
        DO 8775 I=1,N
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
!
      MAXPTS=5000*N*N*N
!CCCC ABSEPS=0.00005D0
!CCCC RELEPS=0.0D0
      ABSEPS=DBLE(ABSE)
      RELEPS=DBLE(RELE)
      VALS=0.0D0
      ERRS=0.0D0
      IFTS=0
!
      IF(IMVNTY.EQ.'SADM')THEN
        CALL SADMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'RANM')THEN
        CALL RANMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'KROM')THEN
        CALL KROMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'SPHM')THEN
        CALL SPHMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSE
        CALL SADMVN(N,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ENDIF
!
      IF(IFTS.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8791)
 8791   FORMAT('***** WARNING IN MULTIVARIATE NORMAL CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8792)ABSEPS
 8792   FORMAT('      ERROR IS GREATER THAN REQUESTED VALUE OF ',   &
               E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      ITYP9='SCAL'
      SCAL9=REAL(VALS)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      AERROR=ERRS
      GO TO 9000
!
!     *******************************************************
!     **  STEP 88--                                        **
!     **  TREAT THE MULTIVARIATE T      CDF           CASE **
!     *******************************************************
!
 8800 CONTINUE
!
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
        GO TO 9000
      ELSE
        N=NR1
      ENDIF
!
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
 8817     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',   &
                 '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8818)N3
 8818     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
 8827     FORMAT('            NUMBER OF ROWS FOR SIGMA MATRIX    ',   &
                 '          = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8828)N3
 8828     FORMAT('            NUMBER OF ROWS FOR THE LOWER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8829)N4
 8829     FORMAT('            NUMBER OF ROWS FOR THE UPPER LIMIT ',   &
                 'VECTOR    = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      NU=INT(YS2+0.1)
!
      DO 8841 I=1,N
        DTEMP1(I)=0.0D0
        DTEMP2(I)=0.0D0
        DTEMP3(I)=0.0D0
 8841 CONTINUE
      ICNT=0
      DO 8860 J=1,N
        DO 8865 I=1,N
          IF(J.LT.I)THEN
            ICNT=ICNT+1
            INDX=J + ((I-2)*(I-1))/2
            DTEMP1(INDX)=DBLE(YM1(I,J))
          ENDIF
 8865   CONTINUE
 8860 CONTINUE
!
      IF(N4.EQ.0)THEN
        DO 8870 I=1,N
          ITEMP1(I)=0
          DTEMP3(I)=DBLE(Y3(I))
          DTEMP2(I)=DBLE(Y3(I))
 8870   CONTINUE
      ELSE
        DO 8875 I=1,N
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
!
      MAXPTS=5000*N*N*N
!CCCC ABSEPS=0.00005D0
!CCCC RELEPS=0.0D0
      ABSEPS=DBLE(ABSE)
      RELEPS=DBLE(RELE)
      VALS=0.0D0
      ERRS=0.0D0
      IFTS=0
!
      IF(IMVNTY.EQ.'SADM')THEN
        CALL SADMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'RANM')THEN
        CALL RANMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSEIF(IMVNTY.EQ.'KROM')THEN
        CALL KROMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ELSE
        CALL SADMVT(N,NU,DTEMP2,DTEMP3,ITEMP1,DTEMP1,   &
                    MAXPTS,ABSEPS,RELEPS,ERRS,VALS,IFTS)
      ENDIF
!
      IF(IFTS.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8891)
 8891   FORMAT('***** WARNING IN MULTIVARIATE T CDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8892)ABSEPS
 8892   FORMAT('      ERROR IS GREATER THAN REQUESTED VALUE OF ',   &
               E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      ITYP9='SCAL'
      SCAL9=REAL(VALS)
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!     *******************************************************
!     **  STEP 89--                                        **
!     **  TREAT THE MULTIVARIATE T    RANDOM NUMBERS  CASE **
!     *******************************************************
!
 8900 CONTINUE
!
      IF(N1.EQ.NR2)GO TO 8909
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
      GO TO 9000
 8909 CONTINUE
!
      NTEMP=INT(YS4)
      LDSIG=MAXROM
      LTF=.TRUE.
!
      DO 8920 I=1,NTEMP
        CALL RDMNOR(Y1,YM2,LDSIG,NR2,LTF,Y4,IFLAG,ISEED)
        DO 8930 J=1,NR2
          YM9(I,J)=Y4(J)
 8930   CONTINUE
 8920 CONTINUE
!
!  NOW DIVIDE BY SQRT(CHIRAN(NU)/NU)
!
      NU=INT(YS3+0.1)
      DO 8940 J=1,NR2
        CALL CHSRAN(NTEMP,REAL(NU),ISEED,Y4)
        DO 8945 I=1,NTEMP
          YM9(I,J)=YM9(I,J)/SQRT(Y4(I)/REAL(NU))
 8945   CONTINUE
 8940 CONTINUE
!
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR2
      IUPFLG='FULL'
      GO TO 9000
!
!     *****************************************************************
!     **  STEP 89.5--                                                **
!     **  TREAT THE DIRICHLET RANDOM NUMBERS  CASE                   **
!     **    LET M = DIRICHLET RANDOM NUMBER ALPHA N                  **
!     *****************************************************************
!
 8950 CONTINUE
!
      NTEMP=INT(YS2 + 0.1)
      NRAN=1
!
      DO 8959 J=1,N1
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
          GO TO 9000
        ENDIF
 8959 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
      NRAN=1
      DO 8970 I=1,NTEMP
        DSUM=0.0D0
        DO 8980 J=1,N1
          CALL GAMRAN(NRAN,Y1(J),ISEED,Y4(J))
          DSUM=DSUM+DBLE(Y4(J))
 8980   CONTINUE
        DO 8985 J=1,N1
          YM9(I,J)=REAL(DBLE(Y4(J))/DSUM)
 8985   CONTINUE
 8970 CONTINUE
!
      ITYP9='MATR'
      NR9=NTEMP
      NC9=N1
      IUPFLG='FULL'
      GO TO 9000
!
!     *****************************************************************
!     **  STEP 93-- -                                                **
!     **  TREAT THE DIRICHLET PDF             CASE                   **
!     **    LET M = DIRICHLET PDF X THETA                            **
!     **    LET M = DIRICHLET LOG PDF X THETA                        **
!     *****************************************************************
!
 9300 CONTINUE
!
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
 9357   FORMAT('            NUMBER OF ROWS FOR THE X VECTOR     = ',   &
              I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9358)N2
 9358   FORMAT('            NUMBER OF ROWS FOR THE ALPHA VECTOR = ',   &
              I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 9360 I=1,N1
        DSUM1=DSUM1+DBLE(Y2(I)-1.0)*DBLE(LOG(Y1(I)))
 9360 CONTINUE
      DLNPDF=DSUM1
!
      DO 9370 I=1,N1
        DSUM2=DSUM2 + DBLE(Y2(I))
 9370 CONTINUE
      DLNPDF=DLNPDF + DLNGAM(DSUM2)
      DO 9380 I=1,N1
        DLNPDF=DLNPDF - DLNGAM(DBLE(Y2(I)))
 9380 CONTINUE
!
      SCAL9=REAL(DLNPDF)
      IF(IMCASE.EQ.'DPDF')THEN
        SCAL9=EXP(SCAL9)
      ENDIF
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!     ***********************************************************
!     **  STEP 94--                                            **
!     **  TREAT THE UNIFORM           RANDOM NUMBERS  CASE     **
!     **  (CORRELATED CASE)                                    **
!     **  LET M = MULTIVARIATE UNIFORM RANDOM NUMBERS SIGMA N  **
!     **  ALGORITHM FROM GENTLE (2003), 'RANDOM NUMBER         **
!     **  GENERATION AND MONTE CARLO METHODS', 2ND. ED., P. 207**
!     **  GENERATE NORMAL RANDOM NUMBERS AND THEN TAKE NORCDF  **
!     **  OF THOSE NUMBERS.  NOTE THAT THE LOCATION PARAMETER  **
!     **  IS ASSUMED TO BE ZERO.                               **
!     ***********************************************************
!
 9400 CONTINUE
!
      NTEMP=INT(YS2)
      LDSIG=MAXROM
      LTF=.TRUE.
!
      DO 9410 I=1,NR1
        Y1(I)=0.0
 9410 CONTINUE
!
      DO 9420 I=1,NTEMP
        CALL RDMNOR(Y1,YM1,LDSIG,NR1,LTF,Y4,IFLAG,ISEED)
        DO 9430 J=1,NR1
          CALL NORCDF(Y4(J),YM9(I,J))
 9430   CONTINUE
 9420 CONTINUE
!
      ITYP9='MATR'
      NR9=NTEMP
      NC9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 95--                                      **
!               **  TREAT THE MATRIX PARTITION STATISTIC     CASE  **
!               *****************************************************
!
!  THIS COMMAND SPLITS THE FULL MATRIX INTO SUB-PARTITIONS
!  (DETERMINED BY NROWPA AND NCOLPA) AND CREATE A NEW MATRIX
!  CONTAINING THE COMPUTED STATISTIC FOR EACH OF THESE SUB-MATRICES.
!
!  TWO CASES ARE SUPPORTED:
!
!  1) IF THE SECOND AND THIRD ARGUMENTS ARE BOTH SCALAR, THEN
!     EXTRACT EQUI-SIZED PARTITIONS.
!
!  2) IF EITHER THE SECOND OR THIRD ARGUMENT IS A VECTOR, THEN
!     EXTRACT UNEQUAL PARTITIONS.  THE VECTOR IS TREATED AS A
!     TAG VARIABLE WHICH IDENTIFIES THE SUB-MATRICES.  WITH THIS
!     APPROACH, THE SUB-MATRICES DO NOT NEED TO BE OF EQUAL SIZE
!     AND DO NOT NEED TO DEFINE CONTIGUOUS SUBSETS.
!
 9500 CONTINUE
!
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
!
      NROWPA=INT(ABS(YS2+0.5))
      NCOLPA=INT(ABS(YS3+0.5))
      IF(N2.LE.0 .AND. N3.LE.0)THEN
!
        IF(NROWPA.EQ.0)NROWPA=2
        IF(NCOLPA.EQ.0)NCOLPA=2
!
        IROW=0
        ICOL=0
        DO 9510 I=1,NC1,NCOLPA
          ICOL=ICOL+1
          ICOL1=I
          ICOL2=I+NCOLPA-1
          IF(ICOL2.GT.NC1)ICOL2=NC1
          IROW=0
          DO 9515 J=1,NR1,NROWPA
            IROW=IROW+1
            IROW1=J
            IROW2=J+NROWPA-1
            IF(IROW2.GT.NR1)IROW2=NR1
            III=0
            DO 9520 II=ICOL1,ICOL2
              DO 9530 JJ=IROW1,IROW2
                III=III+1
                NTEMP=III
                Y1(III)=YM1(JJ,II)
 9530         CONTINUE
 9520       CONTINUE
            ASTAT=0.0
            CALL CMPSTA(   &
          Y1,Y2,Y2,Y3,Y4,Y5,Y6,Y6,MAXNXT,NTEMP,NTEMP,NTEMP,NUMV2,   &
          ICASS7,ISTARA,   &
          ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
          DTEMP1,DTEMP2,DTEMP3,   &
          ASTAT,   &
          ISUBRO,IBUGA3,IERROR)
            YM9(IROW,ICOL)=ASTAT
 9515     CONTINUE
 9510   CONTINUE
!
        ITYP9='MATR'
        NR9=IROW
        NC9=ICOL
        IUPFLG='FULL'
        GO TO 9000
!
      ELSE
!
        IF(N2.GE.1)THEN
          IF(N2.NE.NR1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9551)
 9551       FORMAT('***** ERROR IN MATRIX PARTITION <STATISTIC>--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9552)N2
 9552       FORMAT('      THE NUMBER OF ELEMENTS IN THE ROW VECTOR ',   &
                   '= ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9553)NR1
 9553       FORMAT('      WHILE THE NUMBER OF ROWS IN THE MATRIX = ',   &
                   I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          CALL DISTIN(Y2,N2,IWRITE,Y5,NROWPA,IBUGA3,IERROR)
          DO 9557 I=1,NROWPA
            DTEMP1(I)=DBLE(Y5(I))
 9557     CONTINUE
        ELSE
          NROWPA=1
          DTEMP1(1)=1.0D0
          DO 9558 I=1,NR1
            Y2(I)=1.0
 9558     CONTINUE
        ENDIF
!
        IF(N3.GE.1)THEN
          IF(N3.NE.NC1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9561)
 9561       FORMAT('***** ERROR IN MATRIX PARTITION <STATISTIC>--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9562)N2
 9562       FORMAT('      THE NUMBER OF ELEMENTS IN THE COLUMN ',   &
                   'VECTOR = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9563)NC1
 9563       FORMAT('      WHILE THE NUMBER OF COLUMNS IN THE ',   &
                   'MATRIX = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          CALL DISTIN(Y3,N3,IWRITE,Y5,NCOLPA,IBUGA3,IERROR)
          DO 9567 I=1,NCOLPA
            DTEMP2(I)=DBLE(Y5(I))
 9567     CONTINUE
        ELSE
          NCOLPA=1
          DTEMP2(1)=1.0D0
          DO 9568 I=1,NC1
            Y3(I)=1.0
 9568     CONTINUE
        ENDIF
!
        DO 9571 IROW=1,NROWPA
          AROW=REAL(DTEMP1(IROW))
          DO 9572 ICOL=1,NCOLPA
            ACOL=REAL(DTEMP2(ICOL))
!
            NTEMP=0
            DO 9580 JJ=1,NC1
              DO 9590 II=1,NR1
                IF(AROW.EQ.Y2(II) .AND. ACOL.EQ.Y3(JJ))THEN
                  NTEMP=NTEMP+1
                  Y1(NTEMP)=YM1(II,JJ)
                ENDIF
 9590         CONTINUE
 9580       CONTINUE
            IF(NTEMP.GE.1)THEN
              ASTAT=0.0
              CALL CMPSTA(   &
              Y1,Y5,Y5,YM2(1,1),YM2(1,2),YM2(1,3),Y6,Y6,MAXNXT,   &
              NTEMP,NTEMP,NTEMP,NUMV2,ICASS7,ISTARA,   &
              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
              DTEMP1,DTEMP2,DTEMP3,   &
              ASTAT,   &
              ISUBRO,IBUGA3,IERROR)
              YM9(IROW,ICOL)=ASTAT
            ELSE
              YM9(IROW,ICOL)=0.0
            ENDIF
 9572     CONTINUE
 9571   CONTINUE
!
        ITYP9='MATR'
        NR9=NROWPA
        NC9=NCOLPA
        IUPFLG='FULL'
        GO TO 9000
!
      ENDIF
!
!               *****************************************************
!               **  STEP 96--                                      **
!               **  TREAT THE MATRIX STATISTIC               CASE  **
!               *****************************************************
!
!  THIS COMMAND COMPUTES A SPECIFIED STATISTIC FOR THE ENTIRE MATRIX.
!
!  NOTE 3/2007: ADD CRAMER CONTINGENCY COEFFICIENT AND
!               PEARSON CONTINGENCY COEFFICIENT.  THESE WORK
!               DIFFERENTLY THAN THE OTHER STATISTICS IN THAT
!               THEY ARE INTERPRETED AS RXC TABLES RATHER THAN
!               ONE ARRAY CONTAINING ALL THE MATRIX OBSERVATIONS.
!
 9600 CONTINUE
!
      IWRITE='OFF'
      MAXNXT=MAXOBV
      IF(ICASS7.EQ.'INTE')NUMV2=1
!
      IF(ICASS7.EQ.'CRAM')THEN
         CALL CRAME2(YM1,MAXROM,NR1,NC1,IWRITE,Y1,ASTAT,   &
                     IBUGA3,IERROR)
         GO TO 9699
      ELSEIF(ICASS7.EQ.'PEAR')THEN
         CALL PEARC2(YM1,MAXROM,NR1,NC1,IWRITE,Y1,ASTAT,   &
                     IBUGA3,IERROR)
         GO TO 9699
      ENDIF
!
      ICNT=0
      DO 9610 I=1,NC1
        DO 9620 J=1,NR1
          ICNT=ICNT+1
          IF(ICNT.GT.MAXOBV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9611)
 9611       FORMAT('***** ERROR FROM MATRIX STATISTIC--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9613)MAXOBV
 9613       FORMAT('      THE NUMBER OF ELEMENTS IS GREATER THAN ',   &
                   I10)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          Y1(ICNT)=YM1(I,J)
 9620   CONTINUE
 9610 CONTINUE
      ASTAT=0.0
      CALL CMPSTA(   &
          Y1,Y2,Y2,Y3,Y4,Y5,Y6,Y6,MAXNXT,ICNT,ICNT,ICNT,NUMV2,   &
          ICASS7,ISTARA,   &
          ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
          DTEMP1,DTEMP2,DTEMP3,   &
          ASTAT,   &
          ISUBRO,IBUGA3,IERROR)
!
 9699 CONTINUE
      SCAL9=ASTAT
      ITYP9='SCAL'
      NR9=1
      NC9=1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 97--                                      **
!               **  TREAT THE MATRIX BIN                     CASE  **
!               *****************************************************
!
!  THIS COMMAND BINS THE DATA IN A MATRIX (I.E., USEFUL FOR
!  GENERATING A HISTOGRAM OF ALL THE POINTS IN THE MATRIX.
!
 9700 CONTINUE
!
      IWRITE='OFF'
      MAXNXT=MAXOBV
!
      ICNT=0
      DO 9710 I=1,NC1
        DO 9720 J=1,NR1
          ICNT=ICNT+1
          IF(ICNT.GT.MAXOBV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9711)
 9711       FORMAT('***** ERROR FROM MATRIX STATISTIC--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9713)MAXOBV
 9713       FORMAT('      THE NUMBER OF ELEMENTS IS GREATER THAN ',   &
                   I10)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          Y1(ICNT)=YM1(J,I)
 9720   CONTINUE
 9710 CONTINUE
!
      CALL DPBIN(Y1,ICNT,IRELAT,CLWID,XSTART,XSTOP,IRHSTG,   &
                 Y4,MAXNXT,IHSTCW,IHSTOU,   &
                 Y2,Y3,N2,IBUGA3,IERROR)
!
      ITYP9='VECT'
      NVECT9=N2
      IUPFLG='FULL'
      DO 9760 I=1,NVECT9
        VECT9(I)=Y2(I)
        Y2(I)=Y3(I)
 9760 CONTINUE
      GO TO 9000
!
!               *****************************************************
!               **  STEP 98--                                      **
!               **  TREAT THE MINIMAL SPANNING TREE          CASE  **
!               **  STEP 1:  CREATE A DISTANCE MATRIX FROM THE     **
!               **           TWO INPUT VECTORS (THE (X,Y)          **
!               **           COORDINATES)                          **
!               **  STEP 2:  CALL MINSPT TO COMPUTE THE EDGES OF   **
!               **           THE MINIMAL SPANNING TREE             **
!               **  STEP 3:  CONVERT THESE EDGES TO A LIST OF      **
!               **           VERTICES THAT CAN BE EASILY PLOTTED   **
!               *****************************************************
!
 9800 CONTINUE
!
!     STEP 1: COMPUTE A DISTANCE MATRIX
!
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
        GO TO 9000
      ENDIF
!
      DO 9810 I=1,N1
        YM1(I,I)=0.0
        IF(I.EQ.N1)GO TO 9810
        AY1=Y1(I)
        AX1=Y2(I)
        DO 9820 J=I+1,N1
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
            GO TO 9000
          ENDIF
          YM1(I,J)=ADIST
          YM1(J,I)=ADIST
 9820   CONTINUE
 9810   CONTINUE
        NR1=N1
!
!     STEP 2: COMPUTE THE EDGES OF THE MINIMAL SPANNING TREE
!
      CALL MINSPT(YM1,MAXROM,NR1,ITEMP1,ITEMP2,ITEMP3,Y3)
!
!     STEP 3: CONVERT TO A LIST OF VERTICES.  NOTE THAT THERE ARE
!             N-1 EDGES.  AN EDGE ESSENTIALLY DEFINES TWO VERTICES.
!             WE WILL ALSO DEFINED A "TAG" VARIABLE (THIS SIMPLIFIES
!             PLOTTING).
!
      ICNT1=0
      DO 9830 I=1,NR1-1
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
!
       DO 9840 I=1,NVECT9
         VECT9(I)=Y3(I)
         Y2(I)=Y4(I)
 9840  CONTINUE
!
       NTAG=NVECT9/2
       ICNT1=0
       ICNT2=0
       DO 9850 I=1,NTAG
         ICNT2=ICNT2+1
         ICNT1=ICNT1+1
         Y3(ICNT1)=REAL(ICNT2)
         ICNT1=ICNT1+1
         Y3(ICNT1)=REAL(ICNT2)
 9850  CONTINUE
!
      ITYP9='VECT'
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 99--                                      **
!               **  TREAT THE MINIMAL SPANNING TREE          CASE  **
!               **  FOR THIS VARIANT, WE START WITH A DISTANCE     **
!               **  MATRIX (RATHER THAN THE VERTICES).  THE        **
!               **  DISTANCES MAY IN FACT REFLECT "COSTS" OR       **
!               **  "WEIGHTINGS" AS OPPOSSED TO ACTUAL DISTANCES.  **
!               **  IN THIS CASE, THE RETURNED OUTPUT IS THE       **
!               **  LIST OF EDGES (I.E., WE DO NOT CONVERT BACK    **
!               **  TO ORIGINAL VERTICES).                         **
!               *****************************************************
!
 9900 CONTINUE
!
!     STEP 1: CHECK FOR A SQUARE MATRIX
!
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
        GO TO 9000
      ENDIF
!
!     STEP 2: CHECK FOR A VALID DISTANCE MATRIX
!
!             A) DIAGONAL ELEMENTS SHOULD BE ZERO
!             B) ALL NON-DIAGONAL ELEMENTS SHOULD BE NON-ZERO
!             C) DIST(I,J) = DIST(J,I)
!
!
      DO 9910 I=1,N1
!
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
          GO TO 9000
        ENDIF
!
        DO 9920 J=I+1,N1
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
            GO TO 9000
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
            GO TO 9000
          ENDIF
 9920   CONTINUE
 9910   CONTINUE
        NR1=N1
!
!     STEP 3: COMPUTE THE EDGES OF THE MINIMAL SPANNING TREE
!
      CALL MINSPT(YM1,MAXROM,NR1,ITEMP1,ITEMP2,ITEMP3,Y3)
!
!     STEP 3: CONVERT TO A LIST OF VERTICES.  NOTE THAT THERE ARE
!             N-1 EDGES.  AN EDGE ESSENTIALLY DEFINES TWO VERTICES.
!             WE WILL ALSO DEFINED A "TAG" VARIABLE (THIS SIMPLIFIES
!             PLOTTING).
!
      NVECT9=NR1-1
      DO 9950 I=1,NVECT9
        VECT9(I)=REAL(ITEMP1(I))
        Y2(I)=REAL(ITEMP2(I))
 9950 CONTINUE
!
      ITYP9='VECT'
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10000--                                   **
!               **  TREAT THE MATRIX RENUMBER                CASE  **
!               *****************************************************
!
!  THIS COMMAND REORDERS THE ROWS (BASED ON Y2) AND COLUMNS
!  (BASED ON Y3) OF A MATRIX.
!
10000 CONTINUE
!
      IWRITE='OFF'
!
!     STEP 1: CHECK Y2 AND Y3
!
      IF(N2.NE.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
10001   FORMAT('***** ERROR FROM MATRIX RENUMBER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10003)
10003   FORMAT('      THE NUMBER OF ELEMENTS IN THE ROW ',   &
               'PERMUATION VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10005)
10005   FORMAT('      IS NOT EQUAL TO THE NUMBER OF ROWS IN ',   &
               'THE MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10007)N2
10007   FORMAT('      NUMBER OF ELEMENTS IN THE ROW PERMUATION ',   &
               'VECTOR    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10009)NR1
10009   FORMAT('      NUMBER OF ROWS IN THE MATRIX             ',   &
               '          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10010 I=1,N2
        ITEMP1(I)=INT(Y2(I)+0.1)
        Y4(I)=REAL(ITEMP1(I))
10010 CONTINUE
      CALL DISTIN(Y4,N2,IWRITE,Y5,NDIST,IBUGA3,IERROR)
      CALL MINIM(Y4,N2,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y4,N2,IWRITE,XMAX,IBUGA3,IERROR)
!
      IF(N2.NE.NDIST)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,10001)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,10013)
!10013   FORMAT('      THE VALUES IN THE ROW PERMUTATION ',
!CCCC1         'VECTOR ARE NOT ALL UNIQUE.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
      ELSEIF(XMIN.LT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10023)
10023   FORMAT('      THE MINIMUM VALUE IN THE ROW PERMUTATION ',   &
               'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10025)
10025   FORMAT('      IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10027)XMIN
10027   FORMAT('      THE MINIMUM VALUE IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(XMAX.GT.REAL(N2))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10033)
10033   FORMAT('      THE MAXIMUM VALUE IN THE ROW PERMUTATION ',   &
               'VECTOR')
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
        GO TO 9000
      ENDIF
!
      IF(N3.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10043)
10043   FORMAT('      THE NUMBER OF ELEMENTS IN THE COLUMN ',   &
               'PERMUATION VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10045)
10045   FORMAT('      IS NOT EQUAL TO THE NUMBER OF COLUMNS IN ',   &
               'THE MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10047)N3
10047   FORMAT('      NUMBER OF ELEMENTS IN THE COLUMN PERMUATION ',   &
               'VECTOR    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10049)NC1
10049   FORMAT('      NUMBER OF COLUMNS IN THE MATRIX             ',   &
               '          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10050 I=1,N3
        ITEMP2(I)=INT(Y3(I)+0.1)
        Y4(I)=REAL(ITEMP2(I))
10050 CONTINUE
      CALL DISTIN(Y4,N3,IWRITE,Y5,NDIST,IBUGA3,IERROR)
      CALL MINIM(Y4,N3,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y4,N3,IWRITE,XMAX,IBUGA3,IERROR)
!
      IF(N3.NE.NDIST)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,10001)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,10053)
!10053   FORMAT('      THE VALUES IN THE COLUMN PERMUTATION ',
!CCCC1         'VECTOR ARE NOT ALL UNIQUE.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
      ELSEIF(XMIN.LT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10063)
10063   FORMAT('      THE MINIMUM VALUE IN THE COLUMN PERMUTATION ',   &
               'VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10065)
10065   FORMAT('      IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10067)XMIN
10067   FORMAT('      THE MINIMUM VALUE IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(XMAX.GT.REAL(N3))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10073)
10073   FORMAT('      THE MAXIMUM VALUE IN THE ROW PERMUTATION ',   &
               'VECTOR')
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
        GO TO 9000
      ENDIF
!
      DO 10081 I=1,NR1
        DO 10082 J=1,NC1
          IROW=ITEMP1(I)
          ICOL=ITEMP2(J)
          YM9(IROW,ICOL)=YM1(I,J)
10082   CONTINUE
10081 CONTINUE
!
      ITYP9='MATR'
      NR9=NR1
      NC9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10100--                                   **
!               **  TREAT THE ADJACENCY MATRIX               CASE  **
!               *****************************************************
!
!  THIS COMMAND CREATES AN ADJACENCY MATRIX FROM A LIST OF EDGES.
!
10100 CONTINUE
!
      IWRITE='OFF'
      NVERT=INT(YS3+0.1)
      NVERT=MAX(NVERT,N1)
      NVERT=MAX(NVERT,N2)
!
!     STEP 1: CHECK TO SEE IF THE MATRIX WILL FIT
!
      IF(NVERT.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10101)
10101   FORMAT('***** ERROR FROM ADJACENCY MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10103)
10103   FORMAT('      THE NUMBER OF VERTICES EXCEEDS THE ',   &
               'MAXIMUM')
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
        GO TO 9000
      ENDIF
!
      IF(NVERT.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10113)
10113   FORMAT('      THE NUMBER OF VERTICES EXCEEDS THE ',   &
               'MAXIMUM')
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
        GO TO 9000
      ENDIF
!
!     STEP 2: NOW CREATE THE ADJACENCY MATRIX
!
!
      DO 10120 J=1,N1
        DO 10130 I=1,N1
          YM9(I,J)=0.0
10130   CONTINUE
10120 CONTINUE
!
      DO 10140 I=1,N1
        IROW=INT(Y1(I)+0.1)
!
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
          GO TO 9000
        ENDIF
!
        ICOL=INT(Y2(I)+0.1)
!
        IF(ICOL.LT.1 .OR. ICOL.GT.NVERT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10153)I
10153     FORMAT('      FOR EDGE ',I8,' THE COLUMN INDEX IS OUT OF ',   &
                 'RANGE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10155)ICOL
10155     FORMAT('      COLUMN INDEX = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        YM9(IROW,ICOL)=1.0
        IF(IMCASE.EQ.'ADMA')YM9(ICOL,IROW)=1.0
10140 CONTINUE
!
      ITYP9='MATR'
      NR9=NVERT
      NC9=NVERT
      IUPFLG='FULL'
      GO TO 9000
!
!               ************************************************
!               **  STEP 10200--                              **
!               **  TREAT THE MATRIX ROW FIT       CASE       **
!               **  PERFORM A FIT OF EACH ROW OF THE MATRIX   **
!               **  AGAINST A COMMON X VARIABLE.  RIGHT NOW,  **
!               **  LIMIT TO LINEAR FIT (BUT MAYBE ADD        **
!               **  QUADRATIC FIT IN FUTURE).                 **
!               ************************************************
!
!CCCC IMPLEMENTED FEBRUARY 2010.
10200 CONTINUE
!
      IF(N2.NE.NC1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10201)
10201   FORMAT('****** ERROR IN MATRIX ROW FIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10202)NC1
10202   FORMAT('      THE NUMBER OF COLUMNS IN THE MATRIX (',I8,') ',   &
               'IS NOT EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10203)N1
10203   FORMAT('      THE NUMBER OF ROWS IN THE X VARIABLE (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10210 I=1,NR1
        DO 10220 J=1,NC1
          Y3(J)=YM1(I,J)
10220   CONTINUE
        ICNT=0
        DO 10230 J=1,NC1
          IF(Y3(J).EQ.PSTAMV)GO TO 10230
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
          CALL LINFIT(Y3,Y4,NPTS,   &
                      PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,   &
                      ISUBRO,IBUGA3,IERROR)
        ENDIF
        YM9(I,1)=PPA0
        YM9(I,2)=PPA1
        YM9(I,3)=SDPPA0
        YM9(I,4)=SDPPA1
10210 CONTINUE
!
      ITYP9='VECT'
      NR9=1
      NC9=1
      DO 10240 I=1,NR1
        VECT9(I)=YM9(I,1)
        Y2(I)=YM9(I,2)
        Y3(I)=YM9(I,3)
        Y4(I)=YM9(I,4)
10240 CONTINUE
      NVECT9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *************************************************
!               **  STEP 10300--                               **
!               **  TREAT THE MATRIX COLUMN FIT       CASE     **
!               **  PERFORM A FIT OF EACH COLUMN OF THE MATRIX **
!               **  AGAINST A COMMON X VARIABLE.  RIGHT NOW,   **
!               **  LIMIT TO LINEAR FIT (BUT MAYBE ADD         **
!               **  QUADRATIC FIT IN FUTURE).                  **
!               *************************************************
!
!CCCC IMPLEMENTED FEBRUARY 2010.
10300 CONTINUE
!
      IF(N2.NE.NR1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10301)
10301   FORMAT('****** ERROR IN MATRIX COLUMN FIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10302)NR1
10302   FORMAT('      THE NUMBER OF ROWS IN THE MATRIX (',I8,') ',   &
               'IS NOT EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10303)N1
10303   FORMAT('      THE NUMBER OF ROWS IN THE X VARIABLE (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10310 I=1,NC1
        DO 10320 J=1,NR1
          Y3(J)=YM1(J,I)
10320   CONTINUE
        ICNT=0
        DO 10330 J=1,NR1
          IF(Y3(J).EQ.PSTAMV)GO TO 10330
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
          CALL LINFIT(Y3,Y4,NPTS,   &
                      PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,   &
                      ISUBRO,IBUGA3,IERROR)
        ENDIF
        YM9(I,1)=PPA0
        YM9(I,2)=PPA1
        YM9(I,3)=SDPPA0
        YM9(I,4)=SDPPA1
10310 CONTINUE
!
      ITYP9='VECT'
      NR9=1
      NC9=1
      DO 10340 I=1,NC1
        VECT9(I)=YM9(I,1)
        Y2(I)=YM9(I,2)
        Y3(I)=YM9(I,3)
        Y4(I)=YM9(I,4)
10340 CONTINUE
      NVECT9=NC1
      IUPFLG='FULL'
      GO TO 9000
!
!               *************************************************
!               **  STEP 10400--                               **
!               **  TREAT THE VARIABLE TO MATRIX      CASE     **
!               *************************************************
!
!CCCC IMPLEMENTED NOVEMBER 2010.
10400 CONTINUE
!
      NROW=INT(YS2+0.1)
      NCOL=N1/NROW
      NREM=N1 - (NROW*NCOL)
      IF(NREM.GT.0)NCOL=NCOL+1
!
      IF(NROW.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10401)
10401   FORMAT('****** ERROR IN VARIABLE TO MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10402)NROW
10402   FORMAT('      THE REQUESTED NUMBER OF ROWS FOR THE MATRIX (',   &
               I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10403)MAXROM
10403   FORMAT('      THE MAXIMUM NUMBER  OF ROWS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NCOL.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10411)
10411   FORMAT('****** ERROR IN VARIABLE TO MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10412)NCOL
10412   FORMAT('      THE REQUESTED NUMBER OF COLUMNS FOR THE MATRIX (',   &
               I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10413)MAXCOM
10413   FORMAT('      THE MAXIMUM NUMBER  OF COLUMNS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(IVARMA.EQ.'COLU')THEN
        ICNT=0
        DO 10420 J=1,NCOL
          DO 10430 I=1,NROW
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
        DO 10470 I=1,NROW
          DO 10480 J=1,NCOL
            ICNT=ICNT+1
            IF(ICNT.LE.N1)THEN
              YM9(I,J)=Y1(ICNT)
            ELSE
              YM9(I,J)=PSTAMV
            ENDIF
10480     CONTINUE
10470   CONTINUE
      ENDIF
!
      ITYP9='MATR'
      NR9=NROW
      NC9=NCOL
      IUPFLG='FULL'
      GO TO 9000
!
!               *************************************************
!               **  STEP 10500--                               **
!               **  TREAT THE MATRIX TO VARIABLE CASE          **
!               *************************************************
!
!CCCC IMPLEMENTED NOVEMBER 2010.
10500 CONTINUE
!
      IF(IMATVA.EQ.'COLU')THEN
        ICNT=0
        DO 10520 J=1,NC1
          DO 10530 I=1,NR1
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
10502         FORMAT('      MAXIMUM NUMBER OF ROWS IN VARIABLE (',   &
                     I8,') EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
10530     CONTINUE
10520   CONTINUE
      ELSE
        ICNT=0
        DO 10570 I=1,NR1
          DO 10580 J=1,NC1
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
              GO TO 9000
            ENDIF
10580     CONTINUE
10570   CONTINUE
      ENDIF
!
      ITYP9='VECT'
      NVECT9=ICNT
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10600-                                    **
!               **  TREAT THE MATRIX COMBINE ROW             CASE  **
!               *****************************************************
!
10600 CONTINUE
!
      IF(NC1.NE.NC2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10611)
10611   FORMAT('***** ERROR IN MATRIX COMBINE ROW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10613)
10613   FORMAT('      THE NUMBER OF COLUMNS IN THE TWO MATRICES IS ',   &
               'NOT EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10615)NC1
10615   FORMAT('      THE NUMBER OF COLUMNS FOR THE FIRST  MATRIX: ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10617)NC2
10617   FORMAT('      THE NUMBER OF COLUMNS FOR THE SECOND MATRIX: ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10610 J=1,NC1
         DO 10620 I=1,NR1
           YM9(I,J)=YM1(I,J)
10620    CONTINUE
         DO 10630 I=1,NR2
           IINDX=I+NR1
           IF(IINDX.GT.MAXROM)THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10611)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10633)
10633        FORMAT('      THE MAXIMUM NUMBER OF ROWS FOR THE ',   &
                    'OUTPUT MATRIX HAS BEEN EXCEEDED.')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10635)MAXROM
10635        FORMAT('      THE MAXIMUM NUMBER OF ROWS  = ',I8)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10637)NR1+NR2
10637        FORMAT('      THE REQUIRED NUMBER OF ROWS = ',I8)
             CALL DPWRST('XXX','BUG ')
             IERROR='YES'
             GO TO 9000
           ENDIF
           YM9(I+NR1,J)=YM2(I,J)
10630    CONTINUE
10610 CONTINUE
!
      ITYP9='MATR'
      NC9=NC1
      NR9=NR1+NR2
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10700-                                    **
!               **  TREAT THE MATRIX COMBINE COLUMN          CASE  **
!               *****************************************************
!
10700 CONTINUE
!
      IF(NR1.NE.NR2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10711)
10711   FORMAT('***** ERROR IN MATRIX COMBINE COLUMN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10713)
10713   FORMAT('      THE NUMBER OF ROWS IN THE TWO MATRICES IS ',   &
               'NOT EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10715)NR1
10715   FORMAT('      THE NUMBER OF ROWS FOR THE FIRST  MATRIX: ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10717)NR2
10717   FORMAT('      THE NUMBER OF ROWS FOR THE SECOND MATRIX: ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 10710 I=1,NR1
         DO 10720 J=1,NC1
           YM9(I,J)=YM1(I,J)
10720    CONTINUE
         DO 10730 J=1,NC2
           IINDX=I+NC1
           IF(IINDX.GT.MAXCOM)THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10711)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10733)
10733        FORMAT('      THE MAXIMUM NUMBER OF COLUMNS FOR THE ',   &
                    'OUTPUT MATRIX HAS BEEN EXCEEDED.')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10735)MAXCOM
10735        FORMAT('      THE MAXIMUM NUMBER OF COLUMNS  = ',I8)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,10737)NC1+NC2
10737        FORMAT('      THE REQUIRED NUMBER OF COLUMNS = ',I8)
             CALL DPWRST('XXX','BUG ')
             IERROR='YES'
             GO TO 9000
           ENDIF
           YM9(I,J+NC1)=YM2(I,J)
10730    CONTINUE
10710 CONTINUE
!
      ITYP9='MATR'
      NC9=NC1+NC2
      NR9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10800-                                    **
!               **  TREAT THE DEX CORE                       CASE  **
!               *****************************************************
!
10800 CONTINUE
!
      MAXK=25
      NROW=10000
      CALL DPCORE(YM1,NC1,NR1,MAXROM,MAXK,   &
                  YM9,ITEMP1,NROW,NUMCOR,   &
                  ITEMP2,Y1,Y2,   &
                  IBUGA3,ISUBRO,IERROR)
!
      ITYP9='MATR'
      NC9=5
      NR9=NUMCOR
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 10900-                                    **
!               **  TREAT THE DEX CONFOUND                   CASE  **
!               *****************************************************
!
10900 CONTINUE
!
      MAXK=25
      MAX2T=500
      IF(NR1*MAX2T.GT.46*MAXOBV/3)THEN
        WRITE(ICOUT,10901)
10901   FORMAT('***** ERROR IN DEX CONFOUND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10903)
10903   FORMAT('      INSUFFICIENT SPACE TO GENERATE CONFOUNDING ',   &
               'STRUCTURE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      CALL DPDCF2(YM1,NC1,NR1,MAXK,MAXROM,   &
                  YM2,MAX2T,   &
                  Y1,Y3,Y4,VECT9,Y2,   &
                  INDEX,ITEMP1,   &
                  ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,ITEMP7,   &
                  STME,STMEC,ST2T,ST2TC,STC,STT,   &
                  NUMCON,   &
                  IBUGA3,ISUBRO,IERROR)
!
      ITYP9='VECT'
      NVECT9=NUMCON
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 11000-                                    **
!               **  TREAT THE DEX CHECK CLASSIC              CASE  **
!               *****************************************************
!
11000 CONTINUE
!
!     CHECK IF MATRIX IS IN "CLASSIC" FORM FOR 2-LEVEL FACTORIAL DESIGN.
!     THAT IS, IF A VALUE OTHER THAN -1, 0, OR 1 IS DETECTED, THEN THE
!     MATRIX IS NOT IN CLASSIC FORM.  SET OUTPUT TO 1 FOR THE CLASSIC
!     CASE AND 0 OTHERWISE.
!
      SCAL9=1.0
      DO 11010 J=1,NC1
        DO 11020 I=1,NR1
          IF(YM1(I,J).EQ.-1.0 .OR. YM1(I,J).EQ.0.0 .OR.   &
             YM1(I,J).EQ.1.0)GO TO 11020
          SCAL9=0.0
          GO TO 11030
11020   CONTINUE
11010 CONTINUE
!
11030 CONTINUE
      ITYP9='SCAL'
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************************************************
!               **  STEP 11000-                                    **
!               **  TREAT THE DEX CHECK CENTER POINT         CASE  **
!               *****************************************************
!
11100 CONTINUE
!
!     CHECK FOR CENTER POINTS IN A LIST OF FACTOR VARIABLES.  CREATE
!     A TAG VARIABLE THAT WILL BE 1 FOR ROWS THAT ARE NOT CENTER POINTS
!     0 FOR ROWS THAT ARE CENTER POINTS.
!
!     2018/10: UPDATE SO THAT WE DO NOT ASSUME THE FACTORS ARE IN
!              CLASSIC UNITS (I.E., CENTER POINT EQUAL 0).  INSTEAD,
!              CHECK IF EQUAL TO THE MEDIAN VALUE OF THE DISTINCT
!              VALUES (FOR EVEN NUMBER OF DISTINCT LEVELS NO CENTER
!              POINT WILL BE DETECTED, FOR ODD NUMBER OF DISTINCT
!              LEVELS, CENTER POINT EQUALS THE MEDIAN).
!
      SCAL9=1.0
      DO 11110 I=1,NR1
        IFLAG=0
        DO 11120 J=1,NC1
!
!         FOR COLUMN J, DETERMINE THE CENTER POINT (= THE MEDIAN
!         OF THE DISTINCT VALUES)
!
          DO 11130 K=1,NR1
            Y1(K)=YM1(K,J)
11130     CONTINUE
          CALL DISTIN(Y1,NR1,IWRITE,Y2,NDIST,IBUGA3,IERROR)
          CALL MEDIAN(Y2,NDIST,IWRITE,Y3,MAXOBV,YMED,IBUGA3,IERROR)
!
!         NOW CHECK WHETHER THE CURRENT ROW IS EQUAL TO THE MEDIAN
!
          IF(YM1(I,J).NE.YMED)THEN
            IFLAG=1
            GO TO 11129
          ENDIF
11120   CONTINUE
11129   CONTINUE
        VECT9(I)=REAL(IFLAG)
11110 CONTINUE
!
      ITYP9='VECT'
      NVECT9=NR1
      IUPFLG='FULL'
      GO TO 9000
!
!               ***************************************************
!               **  STEP 11200--                                 **
!               **  TREAT THE TWO WAY DUMMY DESIGN MATRIX   CASE **
!               ***************************************************
!
!CCCC IMPLEMENTED NOVEMBER 2023.
11200 CONTINUE
!
!     STEP 1: COMPUTE CODED VARIABLES AND DETERMINE NUMBER OF
!             DISTINCT LEVELS
!
      CALL CODE(Y1,N1,IWRITE,Y3,Y5,MAXOBV,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      DO 11201 II=1,N1
        Y1(II)=Y3(II)
11201 CONTINUE
      CALL DISTIN(Y1,N1,IWRITE,Y3,NA,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(Y3,NA,Y3)
!
      CALL CODE(Y2,N1,IWRITE,Y4,Y5,MAXOBV,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      DO 11203 II=1,N1
        Y2(II)=Y4(II)
11203 CONTINUE
      CALL DISTIN(Y2,N1,IWRITE,Y4,NB,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(Y4,NB,Y4)
      print *,'na,nb = ',na,nb
!
      NROW=N1
      NCOL=(NA-1) + (NB-1) + (NA-1)*(NB-1)
!
      IF(NROW.GT.MAXROM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11211)
11211   FORMAT('****** ERROR IN TWO WAY ANOVA DUMMY DESIGN MATRIX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11212)NROW
11212   FORMAT('      THE REQUESTED NUMBER OF ROWS FOR THE MATRIX (',   &
               I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11213)MAXROM
11213   FORMAT('      THE MAXIMUM NUMBER  OF ROWS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NCOL.GT.MAXCOM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11222)NCOL
11222   FORMAT('      THE REQUESTED NUMBER OF COLUMNS FOR THE MATRIX (',   &
               I8,') ','IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11223)MAXCOM
11223   FORMAT('      THE MAXIMUM NUMBER  OF COLUMNS   = (',I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     STEP 2: CREATE THE COLUMNS FOR FACTOR A
!
      DO 11230 JJ=1,NA-1
        IHOLD2=INT(Y3(JJ)+0.01)
        IHOLD3=INT(Y3(NA)+0.01)
        print *,'jj,ihold2,ihold3 = ',jj,ihold2,ihold3
        DO 11235 II=1,N1
          IHOLD1=INT(Y1(II)+0.01)
          IF(IHOLD1.EQ.IHOLD2)THEN
            YM9(II,JJ)=1.0
          ELSEIF(IHOLD1.EQ.IHOLD3)THEN
            YM9(II,JJ)=-1.0
          ELSE
            YM9(II,JJ)=0.0
          ENDIF
11235   CONTINUE
11230 CONTINUE
!
!     STEP 3: CREATE THE COLUMNS FOR FACTOR B
!
      DO 11240 JJ=1,NB-1
        IHOLD2=INT(Y4(JJ)+0.01)
        IHOLD3=INT(Y4(NB)+0.01)
        JCOL=(NA-1) + JJ
        DO 11245 II=1,N1
          IHOLD1=INT(Y2(II)+0.01)
          IF(IHOLD1.EQ.IHOLD2)THEN
            YM9(II,JCOL)=1.0
          ELSEIF(IHOLD1.EQ.IHOLD3)THEN
            YM9(II,JCOL)=-1.0
          ELSE
            YM9(II,JCOL)=0.0
          ENDIF
11245   CONTINUE
11240 CONTINUE
!
!     STEP 4: CREATE THE COLUMNS FOR THE A-B INTERACTIONS
!
      ICNT=0
      DO 11250 JJ=1,NA-1
        DO 11255 KK=1,NB-1
          ICNT=ICNT+1
          JCOL1=JJ
          JCOL2=(NA-1) + KK
          JCOL3=(NA-1) + (NB-1) + ICNT
          DO 11258 II=1,N1
            YM9(II,JCOL3)=YM9(II,JCOL1)*YM9(II,JCOL2)
11258     CONTINUE
11255   CONTINUE
11250 CONTINUE
!
      ITYP9='MATR'
      NR9=NROW
      NC9=NCOL
      IUPFLG='FULL'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'ATR3')GO TO 9090
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF MATAR3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4
 9012 FORMAT('IBUGA3,ISUBRO,IMCASE,ITYPA1,ITYPA2,ITYPA3,ITYPA4 = ',   &
      A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
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
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NR1,NC1
 9031 FORMAT('NR1,NC1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR1.LE.0)GO TO 9039
      IF(NC1.LE.0)GO TO 9039
      JMAX=NC1
      IF(JMAX.GT.10)JMAX=10
      DO 9032 I=1,NR1
      WRITE(ICOUT,9033)I,(YM1(I,J),J=1,JMAX)
 9033 FORMAT('I,YM1(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NR2,NC2
 9041 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9049
      IF(NC2.LE.0)GO TO 9049
      JMAX=NC2
      IF(JMAX.GT.10)JMAX=10
      DO 9042 I=1,NR2
      WRITE(ICOUT,9043)I,(YM2(I,J),J=1,JMAX)
 9043 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9042 CONTINUE
 9049 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)NR9,NC9
 9051 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9059
      IF(NC9.LE.0)GO TO 9059
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9055 I=1,NR9
      WRITE(ICOUT,9056)I,(YM9(I,J),J=1,JMAX)
 9056 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9055 CONTINUE
 9059 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9111)N1
 9111 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N1.LE.0)GO TO 9119
      DO 9112 I=1,N1
      WRITE(ICOUT,9113)I,Y1(I)
 9113 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9112 CONTINUE
 9119 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9121)N2
 9121 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N2.LE.0)GO TO 9129
      DO 9122 I=1,N2
      WRITE(ICOUT,9123)I,Y2(I)
 9123 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9122 CONTINUE
 9129 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9131)N3
 9131 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N3.LE.0)GO TO 9139
      DO 9132 I=1,N3
      WRITE(ICOUT,9133)I,Y3(I)
 9133 FORMAT('I,Y3(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9132 CONTINUE
 9139 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9141)N4
 9141 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(N4.LE.0)GO TO 9149
      DO 9142 I=1,N4
      WRITE(ICOUT,9143)I,Y4(I)
 9143 FORMAT('I,Y4(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9142 CONTINUE
 9149 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9151)ITYP9,SCAL9
 9151 FORMAT('ITYP9,SCAL9 = ',A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9161)NVECT9
 9161 FORMAT('NVECT9 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NVECT9.LE.0)GO TO 9169
      DO 9162 I=1,NVECT9
      WRITE(ICOUT,9163)I,VECT9(I)
 9163 FORMAT('I,VECT9(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9162 CONTINUE
 9169 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9171)NR9,NC9
 9171 FORMAT('NR9,NC9 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR9.LE.0)GO TO 9179
      IF(NC9.LE.0)GO TO 9179
      JMAX=NC9
      IF(JMAX.GT.10)JMAX=10
      DO 9172 I=1,NR9
      WRITE(ICOUT,9173)I,(YM9(I,J),J=1,JMAX)
 9173 FORMAT('I,YM9(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9172 CONTINUE
 9179 CONTINUE
!
      IF(IMCASE.NE.'MASS')GO TO 9189
      WRITE(ICOUT,9181)NR2,NC2
 9181 FORMAT('NR2,NC2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NR2.LE.0)GO TO 9189
      IF(NC2.LE.0)GO TO 9189
      JMAX=NC2+1
      IF(JMAX.GT.10)JMAX=10
      NR2P1=NR2+1
      DO 9182 I=1,NR2P1
      WRITE(ICOUT,9183)I,(YM2(I,J),J=1,JMAX)
 9183 FORMAT('I,YM2(I,.) = ',I8,10E10.3)
      CALL DPWRST('XXX','BUG ')
 9182 CONTINUE
!CCCC WRITE(ICOUT,9187)NR2,NLTZ,NGTZ,NEQZ
!9187 FORMAT('NR2,NLTZ,NGTZ,NEQZ = ',4I8)
      WRITE(ICOUT,9187)NR2
 9187 FORMAT('NR2 = ',I8)
      CALL DPWRST('XXX','BUG ')
 9189 CONTINUE
!
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE MATAR3
      SUBROUTINE MATWRI(YMAT,NROW,NCOL,ITITLE,NCTITL,   &
                        ICAPSW,ICAPTY,IFORSW,NUMDIG,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PRINT A MATRIX
!     INPUT  ARGUMENTS--YMAT   = THE SINGLE PRECISION MATRIX
!                                TO BE PRINTED.
!                       NROW   = THE INTEGER NUMBER OF ROWS IN THE
!                                MATRIX YMAT.
!                       NCOL   = THE INTEGER NUMBER OF COLUMNS IN THE
!                                MATRIX YMAT.
!                       NCOL   = THE INTEGER NUMBER OF COLUMNS IN THE
!                                MATRIX YMAT.
!                       ITILE  = TITLE FOR THE MATRIX PRINT.
!                       NCTITL = THE INTEGER NUMBER OF CHARACTERS IN
!                                ITITLE
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/06
!     ORIGINAL VERSION--JUNE      2024.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*60 ITITLE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION YMAT(NROW,NCOL)
!
      PARAMETER (MAXROW=30)
      PARAMETER (MAXCOL=5)
      PARAMETER(NUMCLI=MAXCOL)
      PARAMETER(MAXLIN=1)
      CHARACTER*60 ITITL9
      CHARACTER*15 ITEXT(MAXROW)
      CHARACTER*15 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*4  ALIGN(MAXCOL)
      CHARACTER*4  VALIGN(MAXCOL)
      REAL         AMAT(MAXROW,MAXCOL)
      INTEGER      NCTEXT(MAXCOL)
      INTEGER      IDIGIT(MAXCOL)
      INTEGER      NTOT(MAXCOL)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      LOGICAL      IFRST
      LOGICAL      ILAST
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='MATW'
      ISUBN2='RI  '
      IERROR='NO'
      IWRITE='OFF'
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TWRI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF MATWRI--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)NROW,NCOL,MAXCOL,IBUGA3,ISUBRO
   52   FORMAT('NROW,NCOL,MAXCOL,IBUGA3,ISUBRO = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,NROW
          DO 58 J=1,NCOL
            WRITE(ICOUT,57)I,YMAT(I,J)
   57       FORMAT('I,Y(I,J) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   58     CONTINUE
   56   CONTINUE
      ENDIF
!
!               ****************************
!               **  STEP 1--              **
!               **  WRITE THE MATRIX      **
!               ****************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TWRI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'ON')THEN
!
        ITITL9=' '
        NCTIT9=0
        NUMLIN=1
        ITEMP=0
!
        MAXCO2=MIN(7,MAXCOL)
        IF(NCOL.GT.MAXCO2)THEN
          ITEMP1=NCOL/MAXCO2
          ITEMP2=MOD(NCOL,MAXCO2)
          IF(ITEMP2.EQ.0)THEN
            ITEMP=ITEMP1
          ELSE
            ITEMP=ITEMP1+1
          ENDIF
        ELSE
          ITEMP=1
        ENDIF
!
        IF(NROW.GT.MAXROW)THEN
          JTEMP1=NROW/MAXROW
          JTEMP2=MOD(NROW,MAXROW)
          IF(JTEMP2.EQ.0)THEN
            JTEMP=JTEMP1
          ELSE
            JTEMP=JTEMP1+1
          ENDIF
        ELSE
          JTEMP=1
        ENDIF
!
        DO 100 JCOL=1,JTEMP
!
          ICOL1=(JCOL-1)*MAXCOL+1
          ICOL2=ICOL1+MAXCOL-1
          IF(ICOL2.GT.NCOL)ICOL2=NCOL
          NUMCOL=ICOL2-ICOL1+1
!
          DO 200 IROW=1,ITEMP
!
            IROW1=(IROW-1)*MAXROW+1
            IROW2=IROW1+MAXROW-1
            IF(IROW2.GT.NROW)IROW2=NROW
            NUMROW=IROW2-IROW1+1
!
            DO 310 J=1,NUMCLI
              DO 320 I=1,MAXLIN
                ITITL2(I,J)=' '
                NCTIT2(I,J)=0
  320         CONTINUE
              ITITL2(1,J)='Col '
              IF(J.LE.9)THEN
                WRITE(ITITL2(1,J)(5:5),'(I1)')J
                NCTIT2(1,J)=5
              ELSEIF(J.LE.99)THEN
                WRITE(ITITL2(1,J)(5:6),'(I2)')J
                NCTIT2(1,J)=6
              ELSEIF(J.LE.999)THEN
                WRITE(ITITL2(1,J)(5:7),'(I3)')J
                NCTIT2(1,J)=7
              ENDIF
  310       CONTINUE
!
            NMAX=0
            DO 330 I=1,NUMCOL
              VALIGN(I)='b'
              ALIGN(I)='r'
              NTOT(I)=15
              NMAX=NMAX+NTOT(I)
              IDIGIT(I)=NUMDIG
              IWHTML(I)=150
              IF(I.EQ.1)THEN
                IWRTF(I)=1800
              ELSE
                IWRTF(I)=IWRTF(I-1)+1800
              ENDIF
  330       CONTINUE
!
            DO 340 I=IROW1,IROW2
              NCTEXT(I)=0
              DO 350 J=ICOL1,ICOL2
                AMAT(I,J)=YMAT(I,J)
  350         CONTINUE
  340       CONTINUE
            IFRST=.TRUE.
            ILAST=.TRUE.
!
            CALL DPDTA2(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        ITEXT,NCTEXT,AMAT,MAXROW,NUMROW,   &
                        IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        ISUBRO,IBUGA3,IERROR)
!
  200     CONTINUE
  100   CONTINUE
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TWRI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MATWRI--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE MATWRI
      SUBROUTINE MATCH(X,Z,NX,VAL,NVAL,IWRITE,Y,ICASE,   &
                       IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--MATCH EACH VALUE IN THE VALUE ARRAY TO THE
!              CLOSEST VALUE IN THE X ARRAY.  THE RETURNED
!              Y ARRAY WILL CONTAIN THE CORRESPONDING INDEX
!              VALUES OF THE X ARRAY (I.E., DON'T RETURN
!              THE MATCHING VALUE, JUST THE INDEX OF THE
!              MATCHING VALUE).
!              IF ICASE IS TRAN, THEN RETURN THE VALUE OF THE
!              ARRAY Z CORRESPONDING TO INDEX.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/10
!     ORIGINAL VERSION--OCTOBER   2001.
!     UPDATED         --DECEMBER  2019. ADD ISUBRO
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
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
!
!------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION VAL(*)
!
!-----COMMON----------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT--------------------------------------------------
!
      ISUBN1='MATC'
      ISUBN2='H   '
      IERROR='NO'
!
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
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I),Z(I)
   56     FORMAT('I,X(I),Z(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************
!               **  COMPUTE INDICES OF MATCHING VALUES *
!               ****************************************
!
      DO 100 I=1,NVAL
        VALTMP=VAL(I)
        INDTMP=1
        YDIFF=CPUMAX
        DO 200 J=1,NX
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ATCH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MATCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NX
 9012   FORMAT('IERROR,NX = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NVAL
          WRITE(ICOUT,9016)I,VAL(I),Y(I)
 9016     FORMAT('I,VAL(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE MATCH
      SUBROUTINE MATCH2(X,NX,VAL,NVAL,Y,IWRITE,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--SORT THE VALUES IN X.  FIND THE INDEX, IVAL,
!              SUCH THAT
!
!                    X(I) <= VAL < X(I+1)
!
!              IF VAL < X(1), RETURN A 0 AND IF VAL > X(NX) RETURN
!              NX + 1.
!
!              DO THIS FOR EACH ROW OF THE VAL VECTOR.
!
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
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION VAL(*)
      DIMENSION Y(*)
!
!---------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT--------------------------------------------------
!
      ISUBN1='MATC'
      ISUBN2='H2  '
      IERROR='NO'
!
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
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I), VAL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************
!               **  ERROR CHECKING                    **
!               ****************************************
!
      IF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN MATCH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)NX
  103   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE (',I5,') IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NVAL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NVAL
  105   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE MATCH ',   &
               'VARIABLE (',I5,') IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  COMPUTE INDICES OF MATCHING VALUES *
!               ****************************************
!
      CALL SORT(X,NX,X)
!
      DO 100 I=1,NVAL
        VALTMP=VAL(I)
        IF(VALTMP.LT.X(1))THEN
          Y(I)=0.
        ELSEIF(VALTMP.GT.X(NX))THEN
          Y(I)=REAL(NX+1)
        ELSE
          DO 200 J=1,NX-1
            IF(VALTMP.GE.X(J) .AND. VALTMP.LT.X(J+1))THEN
              Y(I)=REAL(J)
              GO TO 100
            ENDIF
  200     CONTINUE
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ATC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF MATCH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NVAL
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE MATCH2
      SUBROUTINE MATCDF(X,K,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE CLASSICAL MATCHING
!              DISTRIBUTION ON THE INTERVAL (0,K).
!              THIS DISTRIBUTION HAS MEAN = 1
!              AND STANDARD DEVIATION = 1
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              MASS FUNCTION:
!
!                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
!                         X = 0, 1, ..., K
!
!              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
!              ARRANGED IN A RANDOM ORDER.  THE MATCHING
!              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
!              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
!              ORDER.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                       K      = THE INTEGER VALUE THAT SPECIFIES
!                                THE MAXIMUM VALUE
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             DISTRIBUTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP, POIPDF.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
!                 PAGES 409-414.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DK
      DOUBLE PRECISION DI
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DGAMMA
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0
!
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
             'MATCDF SUBROUTINE IS LESS THAN 0.')
!
      IX=INT(X+0.5)
      IF(IX.LT.0 .OR. IX.GT.K)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)IX
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',   &
             'MATCDF SUBROUTINE IS OUTSIDE THE (0,N) INTERVAL')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!-----START POINT-----------------------------------------------------
!
!     FOR K SUFFICENTLY LARGE, USE POISSON (WITH LAMBDA = 1)
!     APPROXIMATION
!
      IF(K.GE.20)THEN
        ALAMB=1.0
        CALL POICDF(X,ALAMB,CDF)
      ELSE
        DK=DBLE(K)
        DCDF=0.0D0
        DO 200 J=0,IX
          IX2=J
          DX=DBLE(J)
          DTERM1=1.0D0/DGAMMA(DX+1.0D0)
          DSUM1=0.0D0
          DO 100 I=0,K-IX2
            DI=DBLE(I)
            DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100     CONTINUE
          DPDF=DTERM1*DSUM1
          DCDF=DCDF + DPDF
  200   CONTINUE
        CDF=REAL(DCDF)
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MATCDF
      SUBROUTINE MATPDF(X,K,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE CLASSICAL MATCHING
!              DISTRIBUTION ON THE INTERVAL (0,K).
!              THIS DISTRIBUTION HAS MEAN = 1
!              AND STANDARD DEVIATION = 1
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              MASS FUNCTION:
!
!                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
!                         X = 0, 1, ..., K
!
!              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
!              ARRANGED IN A RANDOM ORDER.  THE MATCHING
!              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
!              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
!              ORDER.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                       K      = THE INTEGER VALUE THAT SPECIFIES
!                                THE MAXIMUM VALUE
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA, DLNGAM.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
!                 PAGES 409-414.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DK
      DOUBLE PRECISION DI
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DGAMMA
      DOUBLE PRECISION DLNGAM
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
!
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
             'MATPDF SUBROUTINE IS LESS THAN 0.')
!
      IX=INT(X+0.5)
      IF(IX.LT.0 .OR. IX.GT.K)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)IX
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO THE ',   &
             'MATPDF SUBROUTINE IS OUTSIDE THE (0,N) INTERVAL')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!-----START POINT-----------------------------------------------------
!
      DX=DBLE(IX)
      DK=DBLE(K)
!
!     FOR K SUFFICENTLY LARGE, USE APPROXIMATION EXP(-1)/X!
!
      IF(K.GE.20)THEN
        DPDF=DEXP(-1.0D0 - DLNGAM(DX+1.0D0))
      ELSE
        DTERM1=1.0D0/DGAMMA(DX+1.0D0)
        DSUM1=0.0D0
        DO 100 I=0,K-IX
          DI=DBLE(I)
          DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100   CONTINUE
        DPDF=DTERM1*DSUM1
      ENDIF
      PDF=REAL(DPDF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MATPDF
      SUBROUTINE MATPPF(P,K,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE CLASSICAL MATCHING
!              DISTRIBUTION ON THE INTERVAL (0,K).
!              THIS DISTRIBUTION HAS MEAN = 1
!              AND STANDARD DEVIATION = 1
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              MASS FUNCTION:
!
!                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
!                         X = 0, 1, ..., K
!
!              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
!              ARRANGED IN A RANDOM ORDER.  THE MATCHING
!              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
!              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
!              ORDER.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                       K      = THE INTEGER VALUE THAT SPECIFIES
!                                THE MAXIMUM VALUE
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             DISTRIBUTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND N, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DGAMMA.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP, POIPPF.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS" SECOND EDITION,
!                 PAGES 409-414.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
!
      IF(K.LT.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
   12 FORMAT('***** ERROR--THE SECOND INPUT ARGUMENT TO THE ',   &
             'MATCDF SUBROUTINE IS LESS THAN 0.')
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)P
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    2 FORMAT('***** ERROR--THE FIRST INPUT ARGUMENT TO ',   &
             'MATPPF IS OUTSIDE THE (0,1) INTERVAL')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
!-----START POINT-----------------------------------------------------
!
!     P = 0 AND P = 1 CASES
!
      IF(P.LE.0.0)THEN
        PPF=0.0
        GO TO 9000
      ELSEIF(P.GE.1.0)THEN
        PPF=REAL(K)
        GO TO 9000
      ENDIF
!
!     FOR K SUFFICENTLY LARGE, USE POISSON (WITH LAMBDA = 1)
!     APPROXIMATION
!
      IF(K.GE.20)THEN
        ALAMB=1.0
        CALL POIPPF(P,ALAMB,PPF)
        GO TO 9000
      ELSE
        DK=DBLE(K)
        DP=DBLE(P)
        DCDF=0.0D0
        DEPS=1.0D-7
        DO 200 J=0,K
          IX2=J
          DX=DBLE(J)
          DTERM1=1.0D0/DGAMMA(DX+1.0D0)
          DSUM1=0.0D0
          DO 100 I=0,K-IX2
            DI=DBLE(I)
            DSUM1=DSUM1 + (-1.0D0)**DI/DGAMMA(DI+1.0D0)
  100     CONTINUE
          DPDF=DTERM1*DSUM1
          DCDF=DCDF + DPDF
          IF(DCDF.GE.DP-DEPS)THEN
            PPF=REAL(J)
            GO TO 9000
          ENDIF
!
  200   CONTINUE
        PPF=1.0
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE MATPPF
      SUBROUTINE MATRAN(N,K,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE MATCHING DISTRIBUTION
!              WITH SHAPE PARAMETER K.
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              MASS FUNCTION:
!
!                P(X;K) = (1/X!)*SUM[i=1 to k-1][(-1)**i/i!]
!                         X = 0, 1, ..., K
!
!              GIVEN K ENTITIES NUMBERED 1 TO K THAT ARE
!              ARRANGED IN A RANDOM ORDER.  THE MATCHING
!              DISTRIBUTION IS THE NUMBER OF ENTITITIES FOR
!              WHICH THE NUMBERED ORDER IS THE SAME AS THE RANDM
!              ORDER.
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --NPAR   = THE INTEGER VALUE
!                                OF THE SHAPE PARAMETER.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE MATCHING DISTRIBUTION
!             WITH SHAPE PARAMETERS N AND NPAR.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --NPAR > 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, LCTPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP (1992).  "UNIVARIATE
!                 DISCRETE DISTRIBUTIONS", SECOND EDITION,
!                 WILEY, PP. 242-244.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/6
!     ORIGINAL VERSION--JUNE      2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INTEGER N
      INTEGER K
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
        GO TO 9999
      ENDIF
!
      IF(K.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)K
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',   &
      'MATCHING RANDOM NUMBERS IS NON-POSITIVE')
   12 FORMAT('***** ERROR--THE K PARAMETER FOR THE ',   &
      'MATCHING RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
! 100 CONTINUE
!
      IF(K.LT.20)THEN
        CALL UNIRAN(N,ISEED,X)
        DO 100 I=1,N
          XTEMP=X(I)
          CALL MATPPF(XTEMP,K,PPF)
          X(I)=PPF
  100   CONTINUE
      ELSE
        ALAMB=1.0
        CALL POIRAN(N,ALAMB,ISEED,X)
      ENDIF
!
 9999 CONTINUE
!
      RETURN
      END SUBROUTINE MATRAN

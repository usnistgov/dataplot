      SUBROUTINE DPI(NPLOTV,NPLOTP,NS,ICASPL,ISEED,IAND1,IAND2,
     1               ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING 4 I PLOTS--
C
C              1) MEDIAN;
C              2) MEAN;
C              3) MIDRANGE;
C              4) MIDMEAN;
C              5) TRIMMED MEAN;
C              6) BIWEIGHT;
C
C              NOTE 2013/10: THIS PLOT HAS BEEN UPDATED IN THE
C              FOLLOWING WAYS:
C
C              1) ADD THE FOLLOWING VARIANTS:
C
C                    ONE STANDARD ERROR PLOT
C                    TWO STANDARD ERROR PLOT
C                    ONE STANDARD DEVIATION PLOT
C                    TWO STANDARD DEVIATION PLOT
C                    MEAN CONFIDENCE LIMIT PLOT
C                    MEDIAN CONFIDENCE LIMIT PLOT
C                    QUANTILE CONFIDENCE LIMIT PLOT
C                    TRIMMED MEAN CONFIDENCE LIMIT PLOT
C                    BIWEIGHT CONFIDENCE LIMIT PLOT
C                    NORMAL TOLERANCE LIMIT PLOT
C                    NORMAL PREDICTION LIMIT PLOT
C                    STANDARD DEVIATION CONFIDENCE LIMIT PLOT
C                    AGRESTI COUL CONFIDENCE LIMIT PLOT
C
C                    FOLLOWING ADDED 11/2017:
C                    DIFFERENCE OF PROPORTION CONFIDENCE LIMIT PLOT
C                    COEFFICENT OF VARIATION CONFIDENCE LIMIT PLOT
C                    COEFFICENT OF DISPERSION CONFIDENCE LIMIT PLOT
C
C                    FOLLOWING ADDED 12/2017:
C                    COEFFICENT OF QUARTILE DISPERSION CONFIDENCE LIMIT PLOT
C
C                    FOLLOWING ADDED 04/2018:
C                    CORRELATION CONFIDENCE LIMIT PLOT
C
C                 RATHER THAN THE LOCATION/MIN/MAX FORM OF THE
C                 PLOT, THESE WILL GENERATE A POINT ESTIMATE,
C                 LOWER INTERVAL, AND UPPER INTERVAL.
C
C              2) ADD A 3-VARIABLE FORM OF THE PLOT:
C
C                    I-PLOT Y X TAG
C
C                 THIS HANDLES REPLICATION IN A DIFFERENT WAY
C                 THAN THE "REPLICATED" OPTION.  WITH THE REPLICATION
C                 OPTION, GIVEN X1 = 1, 2, 3 AND X2 = 1, 2, THE
C                 X-COORDINATES FOR THE PLOT WILL BE
C
C                     X1     X2  |  X-COOR
C                     ====================
C                      1      1          1
C                      2      1          2
C                      3      1          3
C                      1      2          4
C                      2      2          5
C                      3      2          6
C
C                 WITH THE 3-VARIABLE FORM OF THE PLOT, THE
C                 X-COORDINATES WILL BE AS FOLLOWS
C
C
C                     X1     X2  |  X-COOR
C                     ====================
C                      1      1        0.8
C                      2      1        1.8
C                      3      1        2.8
C                      1      2        1.2
C                      2      2        2.2
C                      3      2        3.2
C
C                 ALSO, THE 3-VARIABLE FORM ALLOWS THE X2 GROUPS
C                 TO BE DRAWN WITH DIFFERENT ATTRIBUTES (E.G.,
C                 DIFFERENT COLORS) WHILE THE REPLICATED OPTION
C                 USES THE SAME ATTRIBUTES.
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3 TO PERFORM
C                                       THE COMMAND PARSING
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR TWO GROUP-ID VARIABLES
C     UPDATED         --OCTOBER   2013. SUPPORT FOR THREE-VARIABLE FORM
C     UPDATED         --OCTOBER   2013. SUPPORT FOR ALTERNATIVES TO
C                                       MEDIAN I PLOT
C     UPDATED         --OCTOBER   2013. SUPPORT FOR "INTERVAL" TYPE
C                                       PLOTS
C     UPDATED         --JULY      2016. FOR MULTIPLE CASE, ALLOW
C                                       CREATION OF "STACKED" VARIABLES
C                                       THAT ARE 5*MAXOBV
C     UPDATED         --NOVEMBER  2017. UPDATES TO AGRESTI-COUL
C                                       CONFIDENCE LIMTIS
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF PROPORTIONS
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEANS
C     UPDATED         --NOVEMBER  2017. COEFFICIENT OF VARIATION
C     UPDATED         --NOVEMBER  2017. COEFFICIENT OF DISPERSION
C     UPDATED         --DECEMBER  2017. COEFFICIENT OF QUARTILE DISPERSION
C     UPDATED         --APRIL     2018. CORRELATION
C     UPDATED         --JUNE      2018. SUPPORT FOR UNEQUAL SAMPLE SIZES
C                                       FOR DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF PROPORTION
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IREPL
      CHARACTER*4 IREP2
      CHARACTER*4 IMULT
      CHARACTER*4 IWRITE
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IERRO2
C
      CHARACTER*4 IA1
      CHARACTER*4 IA2
      CHARACTER*4 IA3
      CHARACTER*4 IA4
      CHARACTER*4 IA5
      CHARACTER*4 IA6
      CHARACTER*4 IA7
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASEQ
      CHARACTER*4 ICASE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(5*MAXOBV)
      DIMENSION Y2(5*MAXOBV)
      DIMENSION X1(5*MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
      DIMENSION X6(MAXOBV)
C
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMPZ(MAXOBV)
      DIMENSION XTEMP0(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION XTEMP5(MAXOBV)
      DIMENSION XTEMP6(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X2(1))
      EQUIVALENCE (GARBAG(IGARB2),X3(1))
      EQUIVALENCE (GARBAG(IGARB3),X4(1))
      EQUIVALENCE (GARBAG(IGARB4),X5(1))
      EQUIVALENCE (GARBAG(IGARB5),X6(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB7),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),XTEMP3(1))
      EQUIVALENCE (GARBAG(IGAR10),XTEMP4(1))
      EQUIVALENCE (GARBAG(JGAR11),XTEMP5(1))
      EQUIVALENCE (GARBAG(JGAR12),XTEMP6(1))
      EQUIVALENCE (GARBAG(JGAR13),XTEMP0(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP1(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP2(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMPZ(1))
      EQUIVALENCE (GARBAG(JGAR17),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGAR11),Y1(1))
      EQUIVALENCE (GARBAG(IGAR16),Y2(1))
      EQUIVALENCE (GARBAG(IGAR21),X1(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
C
      COMMON/IPLOT/NREPI1,NREPI2
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
      IERROR='NO'
      IFOUND='NO'
      IFOUN1='NO'
      IFOUN2='NO'
      IWRITE='OFF'
      ISUBN1='DPI '
      ISUBN2='    '
C
      NREPI1=0
      NREPI2=0
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *******************************
C               **  TREAT THE I   PLOT CASE  **
C               *******************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  EXTRACT THE COMMAND                             **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
C               **    1) I PLOT Y X1 ... X2                         **
C               **    2) MULTIPLE I PLOT Y1 ... YK                  **
C               **    3) REPLICATED I PLOT Y X1 X2                  **
C               **  THE "REPLICATION" CASE IS ACTUALLY THE DEFAULT  **
C               **  AND THE KEYWORD "REPLICATION" IS OPTIONAL.      **
C               **  HOWEVER, SUPPORT IT FOR COMPATABILITY WITH      **
C               **  OTHER COMMANDS.                                 **
C               ******************************************************
C
C     NOTE 2013/10: FOLLOWING ADDITIONAL COMMANDS ADDED:
C
C          ONE STANDARD ERROR PLOT
C          TWO STANDARD ERROR PLOT
C          MEAN CONFIDENCE LIMIT PLOT
C          MEDIAN CONFIDENCE LIMIT PLOT
C          QUANTILE CONFIDENCE LIMIT PLOT
C          TRIMMED MEAN CONFIDENCE LIMIT PLOT
C          BIWEIGHT CONFIDENCE LIMIT PLOT
C          STANDARD DEVIATION CONFIDENCE LIMIT PLOT
C          NORMAL TOLERANCE LIMIT PLOT
C          NORMAL PREDICTION LIMIT PLOT
C          AGRESTI COUL CONFIDENCE LIMIT PLOT
C
C     NOTE 2017/11: FOLLOWING ADDITIONAL COMMANDS ADDED:
C
C          DIFFERENCE OF MEANS CONFIDENCE LIMIT PLOT
C          DIFFERENCE OF PROPORTIONS CONFIDENCE LIMIT PLOT
C          STANDARD DEVIATION CONFIDENCE LIMIT PLOT
C          COEFFICIENT OF VARIATION CONFIDENCE LIMIT PLOT
C          COEFFICIENT OF DISPERSION CONFIDENCE LIMIT PLOT
C
C     NOTE 2017/11: FOLLOWING ADDITIONAL COMMANDS ADDED:
C
C          COEFFICIENT OF QUARTILE DISPERSION CONFIDENCE LIMIT PLOT
C
C     NOTE 2018/04: FOLLOWING ADDITIONAL COMMANDS ADDED:
C
C          CORRELATION CONFIDENCE LIMIT PLOT
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'I')GOTO89
      IF(ICOM.EQ.'MULT')GOTO89
      IF(ICOM.EQ.'REPL')GOTO89
      IF(ICOM.EQ.'MEAN')GOTO89
      IF(ICOM.EQ.'MEDI')GOTO89
      IF(ICOM.EQ.'MIDR')GOTO89
      IF(ICOM.EQ.'MIDM')GOTO89
      IF(ICOM.EQ.'MEDI')GOTO89
      IF(ICOM.EQ.'QUAN')GOTO89
      IF(ICOM.EQ.'TRIM')GOTO89
      IF(ICOM.EQ.'BIWE')GOTO89
      IF(ICOM.EQ.'NORM')GOTO89
      IF(ICOM.EQ.'STAN' .AND. IHARG(1).EQ.'DEVI')GOTO89
      IF(ICOM.EQ.'SD  ')GOTO89
      IF(ICOM.EQ.'ONE' .OR. ICOM.EQ.'1')GOTO89
      IF(ICOM.EQ.'TWO' .OR. ICOM.EQ.'2')GOTO89
      IF(ICOM.EQ.'AGRE')GOTO89
      IF(ICOM.EQ.'PROP')GOTO89
      IF(ICOM.EQ.'BINO' .AND. IHARG(1).EQ.'PROP')GOTO89
      IF(ICOM.EQ.'COEF')GOTO89
      IF(ICOM.EQ.'DIFF')GOTO89
      IF(ICOM.EQ.'CORR')GOTO89
      GOTO9000
C
   89 CONTINUE
      ICASPL='MDIP'
      IMULT='OFF'
      IREPL='OFF'
      IREP2='OFF'
      ILASTC=-9999
      ISTOP=NUMARG
      NRESP=1
      DO91I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          ISTOP=I
          GOTO93
        ENDIF
   91 CONTINUE
   93 CONTINUE
C
      DO100I=0,ISTOP
C
        IF(I.EQ.0)THEN
          IA1=ICOM
        ELSE
          IA1=IHARG(I)
        ENDIF
        IA2=IHARG(I+1)
        IA3=IHARG(I+2)
        IA4=IHARG(I+3)
        IA5=IHARG(I+4)
        IA6=IHARG(I+5)
        IA7=IHARG(I+6)
C
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(IA1.EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IA1.EQ.'MULT')THEN
          IMULT='ON'
        ELSEIF(IA1.EQ.'I' .AND. IA2.EQ.'PLOT')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
          GOTO109
        ELSEIF(IA1.EQ.'MEDI' .AND. IA2.EQ.'I' .AND.
     1         IA3.EQ.'PLOT')THEN
          ICASPL='MDIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GOTO109
        ELSEIF(IA1.EQ.'TRIM' .AND. IA2.EQ.'MEAN' .AND.
     1         IA3.EQ.'I   ' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='TMIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'MEAN' .AND. IA2.EQ.'I' .AND.
     1         IA3.EQ.'PLOT')THEN
          ICASPL='MEIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GOTO109
        ELSEIF(IA1.EQ.'MIDR' .AND. IA2.EQ.'I' .AND.
     1         IA3.EQ.'PLOT')THEN
          ICASPL='MRIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GOTO109
        ELSEIF(IA1.EQ.'MIDM' .AND. IA2.EQ.'I' .AND.
     1         IA3.EQ.'PLOT')THEN
          ICASPL='MMIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GOTO109
        ELSEIF(IA1.EQ.'BIWE' .AND. IA2.EQ.'I' .AND.
     1         IA3.EQ.'PLOT')THEN
          ICASPL='BWIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GOTO109
        ELSEIF(IA1.EQ.'TRIM' .AND. IA2.EQ.'MEAN' .AND.
     1         IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.
     1         IA5.EQ.'PLOT')THEN
          ICASPL='TMCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GOTO109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'MEAN' .AND. IA4.EQ.'CONF' .AND.
     1         IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='DMEA'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          NRESP=2
          GOTO109
        ELSEIF(IA1.EQ.'MEAN' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='MECL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'SD  ' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='SDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'STAN' .AND. IA2.EQ.'DEVI' .AND.
     1         IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.
     1         IA5.EQ.'PLOT')THEN
          ICASPL='SDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GOTO109
        ELSEIF(IA1.EQ.'MEDI' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='MECL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'QUAN' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='QUCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'BIWE' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='BWCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'PROP' .AND. IA4.EQ.'CONF' .AND.
     1         IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='DPRO'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          NRESP=2
          GOTO109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'BINO' .AND. IA4.EQ.'PROP' .AND.
     1         IA5.EQ.'CONF' .AND. IA6.EQ.'LIMI' .AND.
     1         IA7.EQ.'PLOT')THEN
          ICASPL='DPRO'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+6)
          NRESP=2
          GOTO109
        ELSEIF(IA1.EQ.'AGRE' .AND. IA2.EQ.'COUL' .AND.
     1         IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.
     1         IA5.EQ.'PLOT')THEN
C
C         2017/11: ADD THE FOLLOWING SYNONYMS FOR
C                  AGRESTI COUL
C         PROPORTION CONFIDENCE LIMIT
C         BINOMIAL PROPORTION CONFIDENCE LIMIT
C
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GOTO109
        ELSEIF(IA1.EQ.'BINO' .AND. IA2.EQ.'PROP' .AND.
     1         IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.
     1         IA5.EQ.'PLOT')THEN
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GOTO109
        ELSEIF(IA1.EQ.'PROP' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'VARI' .AND. IA4.EQ.'CONF' .AND.
     1         IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='CVCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          GOTO109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'DISP' .AND. IA4.EQ.'CONF' .AND.
     1         IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='CDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          GOTO109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.
     1         IA3.EQ.'QUAR' .AND. IA4.EQ.'DISP' .AND.
     1         IA5.EQ.'CONF' .AND. IA6.EQ.'LIMI' .AND.
     1         IA7.EQ.'PLOT')THEN
          ICASPL='CQCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+6)
          GOTO109
        ELSEIF((IA1.EQ.'ONE ' .OR. IA1.EQ.'1') .AND.
     1          IA2.EQ.'STAN' .AND. IA3.EQ.'ERRO' .AND.
     1          IA4.EQ.'PLOT')THEN
          ICASPL='1SE '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF((IA1.EQ.'TWO ' .OR. IA1.EQ.'2') .AND.
     1          IA2.EQ.'STAN' .AND. IA3.EQ.'ERRO' .AND.
     1          IA4.EQ.'PLOT')THEN
          ICASPL='2SE '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
        ELSEIF((IA1.EQ.'ONE ' .OR. IA1.EQ.'1') .AND.
     1          IA2.EQ.'STAN' .AND. IA3.EQ.'DEVI' .AND.
     1          IA4.EQ.'PLOT')THEN
          ICASPL='1SD '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF((IA1.EQ.'TWO ' .OR. IA1.EQ.'2') .AND.
     1          IA2.EQ.'STAN' .AND. IA3.EQ.'DEVI' .AND.
     1          IA4.EQ.'PLOT')THEN
          ICASPL='2SD '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
        ELSEIF(IA1.EQ.'NORM' .AND. IA2.EQ.'TOLE' .AND.
     1          IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='NTOL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'NORM' .AND. IA2.EQ.'PRED' .AND.
     1          IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='NPRE'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GOTO109
        ELSEIF(IA1.EQ.'CORR' .AND. IA2.EQ.'CONF' .AND.
     1         IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='CRCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          NRESP=2
          GOTO109
        ENDIF
  100 CONTINUE
  109 CONTINUE
C
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN I PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,107)
  107     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THIS PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL
  112   FORMAT('ICASPL,IMULT,IREPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='I PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
      ENDIF
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=NRESP
      IF(IREPL.EQ.'ON')THEN
        MINNVA=MINNVA+1
      ENDIF
C
C     DIFFERENCE OF MEAN AND DIFFERENCE OF PROPORTION SUPPORT 4 VARIABLE
C     CASE (Y1 TAG1 Y2 TAG2) TO HANDLE UNEQUAL SAMPLE SIZES.  HOWEVER,
C     DO NOT SUPPORT THIS IF EITHER REPLICATION OR MULTIPLE SWITCH
C     IS ON.
C
      IF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF')THEN
        IF(ICASPL.EQ.'DMEA' .OR. ICASPL.EQ.'DPRO')THEN
          IFLAGE=19
          MINNVA=3
          MAXNVA=4
        ENDIF
      ENDIF
C
C     NOTE: NEED TO KEEP "I PLOT Y" AS VALID SYNTAX, SO
C           MINIMUM NUMBER OF VARIABLES IS 1 EVEN FOR REPLICATION
C           CASE.
C
      IF(IMULT.EQ.'ON')THEN
        MAXNVA=30
      ELSE
        MAXNVA=NRESP+6
      ENDIF
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG2,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
      IF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF' .AND.
     1   NUMVAR.EQ.NRESP+2)THEN
        IF(ICASPL.EQ.'DMEA' .OR. ICASPL.EQ.'DPRO')THEN
          IREP2='OFF'
        ELSE
          IREP2='ON'
        ENDIF
      ELSEIF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF')THEN
        IREPL='ON'
      ENDIF
C
      NREPL=0
      IF(IMULT.EQ.'ON')THEN
        IF(NRESP.EQ.1)THEN
          NRESP=NUMVAR
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,521)
  521     FORMAT('      THE MULTIPLE OPTION IS NOT SUPPORTED FOR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,523)
  523     FORMAT('      CASES WITH MULTIPLE RESPONSE VARIABLES. ')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.0 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 0 AND 6;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PBOX')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IH='ALPH'
      IH2='A   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHA=0.05
        IF(ICASPL.EQ.'NTOL')ALPHA=0.95
      ELSE
        ALPHA=VALUE(ILOCP)
        IF(ALPHA.GE.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.
        IF(ALPHA.GT.0.5 .AND. ALPHA.LT.1.0)ALPHA=1.0 - ALPHA
        IF(ALPHA.LE.0.0 .OR. ALPHA.GE.0.5)ALPHA=0.05
      ENDIF
C
      IF(ICASPL.EQ.'NTOL')THEN
        IH='GAMM'
        IH2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          GAMMA=0.95
        ELSE
          GAMMA=VALUE(ILOCP)
          IF(GAMMA.GE.1.0 .AND. GAMMA.LE.100.0)GAMMA=GAMMA/100.
          IF(GAMMA.GT.0.0 .AND. GAMMA.LT.0.5)GAMMA=1.0 - GAMMA
          IF(GAMMA.LE.0.5 .OR.  GAMMA.GE.1.0)GAMMA=0.95
        ENDIF
      ELSE
        GAMMA=CPUMIN
      ENDIF
C
      IF(ICASPL.EQ.'NPRE')THEN
        IH='NNEW'
        IH2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NNEW=INT(VALUE(ILOCP)+0.5)
        ELSE
          NNEW=1
        ENDIF
        IF(NNEW.LT.1)NNEW=1
      ELSE
        NNEW=0
      ENDIF
C
      IH='P1  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P1=0.25
      ELSE
        P1=VALUE(ILOCP)
        IF(P1.GE.1.0 .AND. P1.LE.50.0)P1=P1/100.
        IF(P1.LE.0.0 .OR. P1.GE.0.4)P1=0.25
      ENDIF
C
      IH='P2  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P2=0.75
      ELSE
        P2=VALUE(ILOCP)
        IF(P2.GE.50.0 .AND. P2.LE.100.0)P2=P2/100.
        IF(P2.LE.0.50 .OR. P2.GT.1.0)P2=0.75
      ENDIF
C
      IH='XQ  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P100=0.50
      ELSE
        P100=VALUE(ILOCP)
        IF(P100.GE.1.0 .AND. P100.LE.100.0)P100=P100/100.
        IF(P100.LE.0.0 .OR. P100.GE.1.0)P100=0.50
      ENDIF
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  FOR THE 1-VARIABLE CASE ONLY,                     *
C               **  DETERMINE IF THE ANALYST                          **
C               **  HAS SPECIFIED    THE GROUP SIZE,                  **
C               **  FOR THE I   PLOT      ANALYSIS.                   **
C               **  THE GROUP SIZE SETTING IS DEFINED BY SEARCHING THE**
C               **  INTERNAL TABLE FOR THE PARAMETER NAME      NI   ; **
C               **  IF FOUND, USE THE SPECIFIED VALUE.                **
C               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.          **
C               ********************************************************
C
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               **************************************************
C               **  STEP 7A--                                   **
C               **  CASE 1: NO "MULTIPLE" CASE--CAN HAVE EITHER **
C               **          1, 2, OR 3 VARIABLES.  THE FIRST    **
C               **          VARIABLE IS A RESPONSE VARIABLE     **
C               **          AND THE SECOND AND THIRD VARIABLES  **
C               **          ARE REPLICATION VARIABLES (IF       **
C               **          PRESENT).  NOTE THAT THIS VERSION   **
C               **          DOES NOT ACCEPT MATRIX ARGUMENTS    **
C               **          EVEN IF ONLY A SINGLE ARGUMENT IS   **
C               **          GIVEN (YOU CAN USE THE MULTIPLE     **
C               **          OPTION IN THAT CASE).               **
C               **************************************************
C
      IF(IREP2.EQ.'ON')THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        IF(NRESP.EQ.1)THEN
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,X2,X3,X4,X5,X6,NLOCAL,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
        ELSE
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,Y2,X1,X2,X3,X4,X5,NLOCAL,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GOTO9000
C
C       FOR THIS CASE, THERE ARE EXACTLY TWO REPLICATION VARIABLES.
C       LOOP OVER THE DISTINCT VALUES IN THE SECOND REPLICATION
C       VARIABLE.
C
        CALL DISTIN(X1,NLOCAL,IWRITE,XTEMP3,NUMSE1,IBUGG2,IERROR)
        CALL DISTIN(X2,NLOCAL,IWRITE,XTEMP3,NUMSE2,IBUGG2,IERROR)
        CALL SORT(XTEMP3,NUMSE2,XTEMP3)
        CALL CODE(XTEMP3,NUMSE2,IWRITE,XTEMP4,XTEMP5,MAXOBV,
     1            IBUGG2,IERROR)
        NREPI1=NUMSE1
        NREPI2=NUMSE2
C
C       RESTRICT SECOND REPLICATION VARIABLE TO A MAXIMUM OF
C       10 DISTINCT VALUE.
C
        IF(NUMSE2.GT.10)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7006)
 7006     FORMAT('      FOR THE 3-VARIABLE REPLICATION CASE, THE ',
     1           'NUMBER OF REPLICATIONS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7007)
 7007     FORMAT('      FOR THE SECOND REPLICTATION VARIABLE ',
     1           'IS GREATER THAN 10.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(NUMSE2.EQ.2)THEN
          XSTRT=-0.2
          XINC=0.4
        ELSEIF(NUMSE2.EQ.3)THEN
          XSTRT=-0.2
          XINC=0.2
        ELSEIF(NUMSE2.EQ.4)THEN
          XSTRT=-0.3
          XINC=0.2
        ELSEIF(NUMSE2.EQ.5)THEN
          XSTRT=-0.4
          XINC=0.2
        ELSE
          XSTRT=-0.4
          XINC=0.8/REAL(NUMSE2-1)
        ENDIF
        NPLOTP=0
        DO7001K=1,NUMSE2
          ATEMP=XTEMP3(K)
          ICNT=0
          XFACT=XSTRT + (K-1)*XINC
          DO7003L=1,NLOCAL
            IF(ATEMP.EQ.X2(L))THEN
              ICNT=ICNT+1
              XTEMP5(ICNT)=Y1(L)
              TEMPZ(ICNT)=Y2(L)
              XTEMP6(ICNT)=X1(L) + XFACT
            ENDIF
 7003     CONTINUE
          NUMV2=NRESP+1
          IF(K.EQ.1)JD=0
          CALL DPI2(XTEMP5,XTEMP6,TEMPZ,XTEMP6,ICNT,ICNT,NUMV2,
     1              ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,
     1              ITOLGC,ITOLM2,PTOLDF,
     1              ISIZE,ICONT,IQUAME,IQUASE,MAXOBV,ISEED,
     1              ALPHA,GAMMA,P1,P2,P100,NNEW,JD,
     1              XIDTEM,XIDTE2,TEMP,XTEMP4,TEMP1,TEMP2,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG2,ISUBRO,IERROR)
 7001   CONTINUE
C
      ELSEIF(IMULT.EQ.'OFF')THEN
        ISTEPN='7B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        IF(NRESP.EQ.1)THEN
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,X2,X3,X4,X5,X6,NLOCAL,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
        ELSE
C
C         2018/06: HANDLE 4-VARIABLE CASE FOR DIFFERENCE OF MEANS
C                  AND DIFFERENCE OF PROPORTIONS SEPARATELY.
C
          IF(NUMVAR.EQ.4 .AND.
     1      (ICASPL.EQ.'DMEA' .OR. ICASPL.EQ.'DPRO'))THEN
C
            ICOL=1
            NUMVA2=2
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                  INAME,IVARN1,IVARN2,IVARTY,
     1                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                  MAXCP4,MAXCP5,MAXCP6,
     1                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                  Y1,X1,X1,X1,X1,X1,X1,NLOCAL,
     1                  IBUGG2,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
C
            ICOL=3
            NUMVA2=2
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                  INAME,IVARN1,IVARN2,IVARTY,
     1                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                  MAXCP4,MAXCP5,MAXCP6,
     1                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                  Y2,X2,X2,X2,X2,X2,X2,NLOCA2,
     1                  IBUGG2,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
C
            JD=0
            NPLOTP=0
            CALL DPI2(Y1,X1,Y2,X2,NLOCAL,NLOCA2,NUMVAR,
     1                ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,
     1                ITOLGC,ITOLM2,PTOLDF,
     1                ISIZE,ICONT,IQUAME,IQUASE,MAXOBV,ISEED,
     1                ALPHA,GAMMA,P1,P2,P100,NNEW,JD,
     1                XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,
     1                Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
            GOTO9000
          ENDIF
C
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,Y2,X1,X2,X3,X4,X5,NLOCAL,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IF(NREP.EQ.6)THEN
            ICOL=8
            NUMVA2=1
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                  INAME,IVARN1,IVARN2,IVARTY,
     1                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                  MAXCP4,MAXCP5,MAXCP6,
     1                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                  X6,X6,X6,X6,X6,X6,X6,NLOCA2,
     1                  IBUGG2,ISUBRO,IFOUND,IERROR)
          ENDIF
        ENDIF
        IF(IERROR.EQ.'YES')GOTO9000
C
C       IF THERE ARE TWO OR MORE REPLICATION VARIABLES, COMBINE
C       THEM TO CREATE A SINGLE REPLICATION VARIABLE.
C
        IF(NUMVAR.EQ.NRESP+2)THEN
          CALL CODCT2(X1,X2,NLOCAL,ICCTOF,ICCTG1,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,
     1                IBUGG2,ISUBRO,IERROR)
          DO7011I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7011     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+3)THEN
          CALL CODCT3(X1,X2,X3,NLOCAL,ICCTOF,ICCTG1,ICCTG2,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,
     1                IBUGG2,ISUBRO,IERROR)
          DO7012I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7012     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+4)THEN
          CALL CODCT4(X1,X2,X3,X4,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                IBUGG2,ISUBRO,IERROR)
          DO7013I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7013     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+5)THEN
          CALL CODCT5(X1,X2,X3,X4,X5,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,
     1                IBUGG2,ISUBRO,IERROR)
          DO7014I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7014     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+6)THEN
          CALL CODCT6(X1,X2,X3,X4,X5,X6,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,XTEMP6,
     1                IBUGG2,ISUBRO,IERROR)
          DO7015I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7015     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.LE.1)THEN
          IH='NI  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')THEN
            ISIZE=NLOCAL
          ELSE
            ISIZE=VALUE(ILOCP)+0.5
          ENDIF
C
        ENDIF
C
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')THEN
          ISTEPN='7C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,791)NLOCAL
  791     FORMAT('DPI2 AFTER FORM REPLICATION VARIABLES: NLOCAL = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO793I=1,NLOCAL
            WRITE(ICOUT,795)I,Y1(I),X1(I)
  795       FORMAT('I,Y1(I),X1(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  793     CONTINUE
        ENDIF
C
C               *********************************************************
C               **  STEP 7B--                                         **
C               **  GENERATE THE I PLOT.                              **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
C               *********************************************************
C
        JD=0
        NPLOTP=0
        CALL DPI2(Y1,X1,Y2,X1,NLOCAL,NLOCAL,NUMVAR,
     1            ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,
     1            ITOLGC,ITOLM2,PTOLDF,
     1            ISIZE,ICONT,IQUAME,IQUASE,MAXOBV,ISEED,
     1            ALPHA,GAMMA,P1,P2,P100,NNEW,JD,
     1            XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ***********************************************
C               **  STEP 8A--                                **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.     **
C               **          THESE CAN BE EITHER VARIABLE OR  **
C               **          MATRIX ARGUMENTS.                **
C               ***********************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES.  NOTE THAT IN
C       THIS CASE, WE ARE ULTIMATELY CREATING A "Y X" SYNTAX, SO THE
C       LOOP IS MERELY ADDING A NEW GROUP.  NEED TO BE CAREFUL THAT
C       COMBINED DATA DOES NOT EXCEED MAXIMUM POINTS FOR AN ARRAY.
C
C       2016/07: ALLOW Y1 AND X1 TO HAVE DIMENSION 5*MAXOBV.  THIS
C                CAN BE HELPFUL FOR LARGER DATA SETS.
C
C       2017/11: NOTE THAT THIS CASE NOT SUPPORTED FOR THE CASES
C                WHERE THERE ARE TWO RESPONSE VARIABLES.  THIS IS
C                CHECKED FOR ABOVE.
C
        NPLOTP=0
        DO810IRESP=1,NRESP
          NCURVE=IRESP
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                XTEMP1,XTEMP2,XTEMP3,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          DO815JJ=1,NLOCAL
            NPLOTP=NPLOTP+1
CCCCC       IF(NPLOTP.GT.MAXOBV)THEN
            IF(NPLOTP.GT.5*MAXOBV)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,816)
  816         FORMAT('      FOR THE MULTIPLE CASE, THE MAXIMUM NUMBER ',
     1               'OF POINTS HAS BEEN EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
            Y1(NPLOTP)=XTEMP1(JJ)
            X1(NPLOTP)=REAL(NCURVE)
  815     CONTINUE
C
  810   CONTINUE
        NLOCAL=NPLOTP
        NUMVAR=2
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
        NPLOTP=0
        JD=0
        CALL DPI2(Y1,X1,Y2,X1,NLOCAL,NLOCAL,NUMVAR,
     1            ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,
     1            ITOLGC,ITOLM2,PTOLDF,
     1            ISIZE,ICONT,IQUAME,IQUASE,MAXOBV,ISEED,
     1            ALPHA,GAMMA,P1,P2,P100,NNEW,JD,
     1            XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IFOUN1,IFOUN2,IERROR
 9012   FORMAT('IFOUND,IFOUN1,IFOUN2,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ISIZE,NUMVAR
 9014   FORMAT('ISIZE,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9015I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPI2(Y,X,YZ,XZ,N,NZ,NUMV2,
     1                ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,
     1                ITOLGC,ITOLM2,PTOLDF,
     1                ISIZE,ICONT,IQUAME,IQUASE,MAXNXT,ISEED,
     1                ALPHA,GAMMA,P1,P2,P100,NNEW,JD,
     1                XIDTEM,XIDTE2,TEMP,TEMPZ,XTEMP1,XTEMP2,
     1                Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN I PLOT
C              OF THE FOLLOWING TYPES--
C                 1) (MEDIAN) I  PLOT;
C                 2) MEAN I  PLOT;
C                 3) MIDRANGE I  PLOT;
C                 4) MIDMEAN I  PLOT;
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  2011. ISUBRO ARGUMENT
C     UPDATED         --OCTOBER   2013. ADD SUPPORT FOR NEW PLOT OPTIONS
C     UPDATED         --NOVEMBER  2017. UPDATES TO AGRESTI-COUL
C                                       CONFIDENCE LIMTIS
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF PROPORTIONS
C     UPDATED         --NOVEMBER  2017. COEFFICIENT OF VARIATION
C     UPDATED         --NOVEMBER  2017. COEFFICIENT OF DISPERSION
C     UPDATED         --DECEMBER  2017. COEFFICIENT OF QUARTILE DISPERSION
C     UPDATED         --APRIL     2018. CORRELATION
C     UPDATED         --JUNE      2018. SUPPORT UNEQUAL SAMPLE SIZES FOR:
C                                          DIFFERENCE OF MEAN
C                                          DIFFERENCE OF PROPORTIONS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICVACI
      CHARACTER*4 ICONT
      CHARACTER*4 IQUAME
      CHARACTER*4 IQUASE
      CHARACTER*4 ITOLGC
      CHARACTER*4 ITOLM2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 ICASA4
      CHARACTER*4 ICASA5
      CHARACTER*4 IDIST
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION YZ(*)
      DIMENSION X(*)
      DIMENSION XZ(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
      DIMENSION ALPHAT(1)
      DIMENSION ALOWLV(1)
      DIMENSION AUPPLV(1)
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
      ISUBN1='DPI2'
      ISUBN2='    '
      IWRITE='OFF'
C
      I2=0
      AN=0.0
      N50=1
      NRESP=1
C
C     2018/06: DIFFERENCE OF MEAN AND DIFFERENCE OF PROPORTION SUPPORT
C              A 4 VARIABLE CASE (Y1 X1 Y2 X2) FOR UNEQUAL SAMPLE
C              SIZES.
C
      IFLAGD=0
      IF(ICASPL.EQ.'DPRO' .OR. ICASPL.EQ.'DMEA' .OR.
     1   ICASPL.EQ.'CRCL')THEN
        NRESP=2
        IF(ICASPL.EQ.'DPRO' .OR. ICASPL.EQ.'DMEA')THEN
          IF(NUMV2.EQ.4)IFLAGD=1
        ENDIF
      ENDIF
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN I PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IFLAGD.EQ.1 .AND. NZ.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,42)
   42   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',
     1         'RESPONSE VARIABLE MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NZ
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     DON'T TREAT ALL RESPONSE VALUES EQUAL AS AN ERROR
C
CCCCC HOLD=Y(1)
CCCCC DO60I=1,N
CCCCC   IF(Y(I).NE.HOLD)GOTO69
CCC60 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,31)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,62)
CCC62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,63)HOLD
CCC63 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
CCC69 CONTINUE
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,NZ,NRESP,NUMV2,ISIZE,PSTAMV,ICONT,ICASPL
   71   FORMAT('N,NZ,NRESP,NUMV2,ISIZE,PSTAMV,ICONT,ICASPL = ',
     1         5I8,G15.7,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO72I=1,MAX(N,NZ)
          WRITE(ICOUT,73)I,Y(I),X(I),YZ(I),XZ(I)
   73     FORMAT('I,Y(I),X(I),YZ(I),XZ(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR AN I PLOT .       **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMV2.EQ.NRESP)THEN
        NUMSET=0
        DO120I=ISIZE,N,ISIZE
          I2=I
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=NUMSET
  120   CONTINUE
        IF(I2.LT.N)THEN
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=NUMSET
        ENDIF
        DO145I=1,N
          IGROUP=1+((I-1)/ISIZE)
          IMID=(IGROUP-1)*ISIZE+(ISIZE/2)
          X(I)=IMID
  145   CONTINUE
C
      ELSEIF(IFLAGD.EQ.1)THEN
C
        CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        XID1=XIDTEM(1)
        XID2=XIDTEM(NUMSET)
        CALL DISTIN(XZ,NZ,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
C       CURRENTLY ONLY SUPPORT CASE WHERE THE GROUPS ARE THE SAME
C
        IF(NUMSET.EQ.NUMSE2)THEN
          DO155II=1,NUMSET
            IF(XIDTEM(II).NE.XIDTE2(II))THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,156)
  156         FORMAT('      THE GROUP-ID VARIABLES DO NOT HAVE THE ',
     1               'SAME VALUES.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
  155     CONTINUE
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,157)
  157     FORMAT('      THE GROUP-ID VARIABLES DO NOT HAVE THE ',
     1               'SAME NUMBER OF ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,158)NUMSET
  158     FORMAT('      GROUP-ID VARIABLE ONE HAS ',I8,' UNIQUE ',
     1               'ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,159)NUMSE2
  159     FORMAT('      GROUP-ID VARIABLE TWO HAS ',I8,' UNIQUE ',
     1               'ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(NUMV2.EQ.NRESP+1)THEN
        CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        XID1=XIDTEM(1)
        XID2=XIDTEM(NUMSET)
      ENDIF
C
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('      NUMSET = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
      ELSEIF(NUMSET.EQ.N)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      NUMSET = N')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES**
C               **  FOR THE DESIRED PLOT,                             **
C               **  FIRST BRANCH TO THE PROPER SUBCASE--              **
C               **         1) (MEDIAN) I  PLOT;                       **
C               **         2) MEAN I  PLOT;                           **
C               ********************************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ***************************************************
C               **  STEP 4A--                                    **
C               **  DETERMINE PLOT COORDINATES FOR 4 SUBCASES--  **
C               **      1) (MEDIAN) I  PLOT;                     **
C               **      2) MEAN I  PLOT;                         **
C               **      3) MIDRANGE I  PLOT;                     **
C               **      4) MIDMEAN I  PLOT;                      **
C               ***************************************************
C
 1100 CONTINUE
C
      ISTEPN='4A'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AN=N
      ANUMSE=NUMSET
C
C     2018/06: CHECK FOR "MISSING DATA" VALUES
C
      NUMCPL=11
      J=N2
      DO1110ISET=1,NUMSET
C
        K=0
        K2=0
        IF(IFLAGD.EQ.0)THEN
          DO1120I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
C
C             NOTE: CORRELATION DATA VALUES MUST BE PAIRED, SO ELIMINATE
C                   ANY ROWS WHERE EITHER DATA VALUE IS MISSING
C
              IF(ICASPL.EQ.'CORR')THEN
                IF(Y(I).NE.PSTAMV .AND. YZ(I).NE.PSTAMV)THEN
                  K=K+1
                  TEMP(K)=Y(I)
                  TEMPZ(K)=YZ(I)
                ENDIF
              ELSE
                IF(Y(I).NE.PSTAMV)THEN
                  K=K+1
                  TEMP(K)=Y(I)
                ENDIF
                IF(NRESP.EQ.2 .AND. YZ(I).NE.PSTAMV)THEN
                  K2=K2+1
                  TEMPZ(K2)=YZ(I)
                ENDIF
              ENDIF
            ENDIF
 1120     CONTINUE
        ELSE
          DO1130I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              IF(Y(I).NE.PSTAMV)THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
            ENDIF
 1130     CONTINUE
          DO1135I=1,NZ
            IF(XZ(I).EQ.XIDTEM(ISET))THEN
              IF(YZ(I).NE.PSTAMV)THEN
                K2=K2+1
                TEMPZ(K2)=YZ(I)
              ENDIF
            ENDIF
 1135     CONTINUE
        ENDIF
C
        NI=K
        ANI=NI
        NI2=K2
        ANI2=NI2
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
          WRITE(ICOUT,1121)ISET,XIDTEM(ISET),NI,NI2
 1121     FORMAT('ISET,XIDTEM(ISET),NI,NI2 = ',I8,G15.7,2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NI.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      NI FOR SOME CLASS = 0')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)ISET,XIDTEM(ISET),NI
 1143     FORMAT('      ISET,XIDTEM(ISET),NI = ',I8,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(NRESP.EQ.2 .AND. NI2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1147)
 1147     FORMAT('      NI2 FOR SOME CLASS = 0')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1148)ISET,XIDTEM(ISET),NI2
 1148     FORMAT('      ISET,XIDTEM(ISET),NI2 = ',I8,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        XMID=XIDTEM(ISET)
C
        IF(ICASPL.EQ.'MECL')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD,IBUGG3,IERROR)
          CDF=1.0 - (ALPHA/2.0)
          NM1=NI-1
          CALL TPPF(CDF,REAL(NM1),TCVAL)
          YMAX=Y50 + TCVAL*YSD/SQRT(REAL(NI))
          YMIN=Y50 - TCVAL*YSD/SQRT(REAL(NI))
        ELSEIF(ICASPL.EQ.'DMEA')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y501,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD1,IBUGG3,IERROR)
          AN1=REAL(NI)
          YTEMP1=YSD1**2/AN1
          CALL MEAN(TEMPZ,NI2,IWRITE,Y502,IBUGG3,IERROR)
          CALL SD(TEMPZ,NI2,IWRITE,YSD2,IBUGG3,IERROR)
          AN2=REAL(NI2)
          YTEMP2=YSD2**2/AN2
          Y50=Y501 - Y502
          YSTERR=SQRT(YTEMP1 + YTEMP2)
          TERM1=(YTEMP1 + YTEMP2)**2
          TERM2=YTEMP1*YTEMP1/(AN1-1.0) + YTEMP2*YTEMP2/(AN2-1.0)
          V=TERM1/TERM2
          IV=INT(V+0.5)
          CDF=1.0 - (ALPHA/2.0)
          CALL TPPF(CDF,REAL(IV),TCVAL)
          YMAX=Y50 + TCVAL*YSTERR
          YMIN=Y50 - TCVAL*YSTERR
        ELSEIF(ICASPL.EQ.'1SE ')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD,IBUGG3,IERROR)
          YMAX=Y50 + YSD/SQRT(REAL(NI))
          YMIN=Y50 - YSD/SQRT(REAL(NI))
        ELSEIF(ICASPL.EQ.'2SE ')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD,IBUGG3,IERROR)
          YMAX=Y50 + 2.0*YSD/SQRT(REAL(NI))
          YMIN=Y50 - 2.0*YSD/SQRT(REAL(NI))
        ELSEIF(ICASPL.EQ.'1SD ')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD,IBUGG3,IERROR)
          YMAX=Y50 + YSD
          YMIN=Y50 - YSD
        ELSEIF(ICASPL.EQ.'2SD ')THEN
          CALL MEAN(TEMP,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL SD(TEMP,NI,IWRITE,YSD,IBUGG3,IERROR)
          YMAX=Y50 + 2.0*YSD
          YMIN=Y50 - 2.0*YSD
        ELSEIF(ICASPL.EQ.'MDCL' .OR. ICASPL.EQ.'QUCL')THEN
          IF(ICASPL.EQ.'MDCL')THEN
            CALL MEDIAN(TEMP,NI,IWRITE,XTEMP1,MAXNXT,XMED,
     1                  IBUGG3,IERROR)
            Y50=XMED
          ELSE
            CALL QUANT(P100,TEMP,NI,IWRITE,XTEMP1,MAXNXT,IQUAME,XQUANT,
     1                 IBUGG3,IERROR)
            Y50=XQUANT
          ENDIF
          CALL QUANSE(P100,TEMP,NI,IWRITE,XTEMP1,MAXNXT,IQUASE,XQUASE,
     1                IBUGG3,IERROR)
          CDF=1.0 - (ALPHA/2.0)
          CALL NORCDF(CDF,TCVAL)
          YMAX=Y50 + TCVAL*XQUASE
          YMIN=Y50 - TCVAL*XQUASE
        ELSEIF(ICASPL.EQ.'TMCL' .OR. ICASPL.EQ.'TMIP')THEN
          NTRIM1=-1
          NTRIM2=-1
          CALL TRIMME(TEMP,NI,P1,P2,NTRIM1,NTRIM2,IWRITE,XTEMP1,
     1                MAXNXT,Y50,IBUGG3,ISUBRO,IERROR)
          IF(ICASPL.EQ.'TMCL')THEN
            CALL TRIMSE(TEMP,NI,P1,P2,NRIM1,NTRIM2,IWRITE,XTEMP1,XTEMP2,
     1                  MAXNXT,YSTERR,IBUGG3,ISUBRO,IERROR)
C
            AN1=NI
            LAMBDA=INT(AN1*(P1+P2)/100.)
            V=0.7*(AN1-1.0)
            IV=NI - LAMBDA - 1
            IF(IV.LT.1)IV=1
            CDF=1.0 - (ALPHA/2.0)
            CALL TPPF(CDF,REAL(IV),TCVAL)
            YMAX=Y50 + TCVAL*YSTERR
            YMIN=Y50 - TCVAL*YSTERR
          ELSE
            CALL MINIM(TEMP,NI,IWRITE,YMIN,IBUGG3,IERROR)
            CALL MAXIM(TEMP,NI,IWRITE,YMAX,IBUGG3,IERROR)
          ENDIF
        ELSEIF(ICASPL.EQ.'BWCL' .OR. ICASPL.EQ.'BWIP')THEN
          CALL BIWLOC(TEMP,NI,IWRITE,XTEMP1,XTEMP2,MAXNXT,Y50,
     1                IBUGG3,IERROR)
          IF(ICASPL.EQ.'BWCL')THEN
            CALL BIWSCA(TEMP,NI,IWRITE,XTEMP1,XTEMP2,MAXNXT,YBSC,
     1                  IBUGG3,IERROR)
            AN1=NI
            YSTERR=SQRT(YBSC/AN1)
            V=0.7*(AN1-1.0)
            IV=INT(V+0.5)
            CDF=1.0 - (ALPHA/2.0)
            CALL TPPF(CDF,REAL(IV),TCVAL)
            YMAX=Y50 + TCVAL*YSTERR
            YMIN=Y50 - TCVAL*YSTERR
          ELSE
            CALL MINIM(TEMP,NI,IWRITE,YMIN,IBUGG3,IERROR)
            CALL MAXIM(TEMP,NI,IWRITE,YMAX,IBUGG3,IERROR)
          ENDIF
        ELSEIF(ICASPL.EQ.'NTOL')THEN
          XMEAN=CPUMIN
          AN=REAL(NI)
          ICASAN='2   '
          CALL DPTOL3(TEMP,NI,XMEAN,XSD,AN,PTOLDF,
     1                ICASAN,ALPHA,GAMMA,ITOLGC,ITOLM2,
     1                AK,ALOWLM,AUPPLM,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=XMEAN
          YMAX=AUPPLM
          YMIN=ALOWLM
        ELSEIF(ICASPL.EQ.'NPRE')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA2='LIMI'
          ICASA3='LOWE'
          ICASA4='RAW '
          ICASA5='TWOS'
          CALL DPPRL3(TEMP,NI,NNEW,ICASA2,ICASA3,ICASA4,ICASA5,
     1                YMEAN,YSD,
     1                ALPHAT,NALPHA,ALOWLV,AUPPLV,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=YMEAN
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
C
        ELSEIF(ICASPL.EQ.'SDCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA2='LIMI'
          ICASA3='UPPE'
          ICASA4='RAW '
          ICASA5='TWOS'
          CALL MEAN(TEMP,NI,IWRITE,YMEAN,IBUGG3,IERROR)
          CALL DPSDC3(TEMP,NI,ICASA2,ICASA3,ICASA4,ICASA5,
     1                YSD,
     1                ALPHAT,NALPHA,ALOWLV,AUPPLV,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=YSD
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CVCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA2='LIMI'
          ICASA3='UPPE'
          ICASA4='RAW '
          ICASA5='TWOS'
          IDIST='NORM'
          YMEAN=CPUMIN
          YSD=CPUMIN
          CALL DPCVC3(TEMP,NI,YMEAN,YSD,
     1                ICASA2,ICASA3,ICASA4,ICASA5,
     1                ISEED,MAXNXT,IDIST,
     1                XTEMP1,XTEMP2,
     1                ICVACI,ALPHAT,NALPHA,ALOWLV,AUPPLV,YCV,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=YCV
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CDCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA3='UPPE'
          ICASA5='TWOS'
          CALL DPCDC3(TEMP,NI,ICASA3,ICASA5,ISEED,MAXNXT,
     1                XTEMP1,ALPHAT,NALPHA,ALOWLV,AUPPLV,
     1                YCD,YMED,YAAD,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=YCD
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CRCL')THEN
          CALL CORR(TEMP,TEMPZ,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL DPCRC3(Y50,NI,ALPHA,U,Z,YMIN,YMAX,
     1                IBUGG3,ISUBRO,IERROR)
        ELSEIF(ICASPL.EQ.'CQCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA3='UPPE'
          ICASA5='TWOS'
          CALL DPCQD3(TEMP,NI,ICASA3,ICASA5,ISEED,MAXNXT,IQUAME,
     1                XTEMP1,ALPHAT,NALPHA,ALOWLV,AUPPLV,
     1                CQV,Q1,Q3,
     1                ISUBRO,IBUGG3,IERROR)
          Y50=CQV
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
       ELSEIF(ICASPL.EQ.'AGCL')THEN
C
C         11/2017: USE DPPRC3 IN ORDER TO ACCOMODATE MULTIPLE METHODS
C                  FOR COMPUTING PROOPORTION CONFIDENCE LIMITS.
C
CCCCC     ISUCC=0
CCCCC     DO1126II=1,NI
CCCCC       IF(TEMP(II).GE.0.5 .AND. TEMP(II).LE.1.5)THEN
CCCCC         ISUCC=ISUCC+1
CCCCC       ENDIF
C1126     CONTINUE
CCCCC     Y50=REAL(ISUCC)/REAL(NI)
CCCCC     CALL DPAGCO(Y50,NI,ALPHA,IWRITE,YMIN,YMAX,IBUGG3,IERROR)
          CALL DPPRC3(TEMP,NI,ALPHA,PSTAMV,IBINME,XTEMP1,
     1                Y50,YMIN,YMAX,
     1                ISUBRO,IBUGG3,IERROR)
       ELSEIF(ICASPL.EQ.'DPRO')THEN
          CALL DPPRC4(TEMP,NI,TEMPZ,NI2,ALPHA,PSTAMV,IBI2ME,XTEMP1,
     1                Y50,YMIN,YMAX,
     1                ISUBRO,IBUGG3,IERROR)
        ELSEIF(ICASPL.EQ.'MDIP' .OR. ICASPL.EQ.'MEIP' .OR.
     1     ICASPL.EQ.'MRIP' .OR. ICASPL.EQ.'MMIP')THEN
C
          CALL SORT(TEMP,NI,TEMP)
C
C               ***************************
C               **  STEP 4.1--           **
C               **  COMPUTE THE MAXIMUM  **
C               ***************************
C
          YMAX=TEMP(NI)
C
C               *********************************
C               **  STEP 4.2--                 **
C               **  COMPUTE THE TYPICAL VALUE  **
C               **  (MEDIAN, MEAN,             **
C               **  MIDRANGE, OR TRIMMED MEAN) **
C               *********************************
C
          IF(ICASPL.EQ.'MDIP')THEN
            N50=NI/2
            N50P1=N50+1
            IEVODD=NI-2*(NI/2)
            IF(IEVODD.EQ.0)Y50=(TEMP(N50)+TEMP(N50P1))/2.0
            IF(IEVODD.EQ.1)Y50=TEMP(N50P1)
          ELSEIF(ICASPL.EQ.'MEIP')THEN
            SUM=0.0
            DO1134I=1,NI
              SUM=SUM+TEMP(I)
 1134       CONTINUE
            Y50=SUM/ANI
          ELSEIF(ICASPL.EQ.'MRIP')THEN
            Y50=(TEMP(1)+TEMP(NI))/2.0
          ELSEIF(ICASPL.EQ.'MMIP')THEN
            NP1=P1*ANI+0.0001
            NP2=P2*ANI+0.0001
            IMIN=NP1+1
            IMAX=N-NP2
            IF(IMIN.LT.1)IMIN=1
            IF(IMAX.GT.NI)IMAX=NI
            IF(IMIN.GT.IMAX)IMIN=IMAX
            Y50=TEMP(1)
            SUM=0.0
            L=0
            DO1138I=IMIN,IMAX
              L=L+1
              SUM=SUM+TEMP(I)
 1138       CONTINUE
            AL=L
            Y50=SUM/AL
          ENDIF
C
          NP1=P1*ANI+0.0001
          NP2=P2*ANI+0.0001
C
C               ***************************
C               **  STEP 4.3--           **
C               **  COMPUTE THE MINIMUM  **
C               ***************************
C
          YMIN=TEMP(1)
C
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,262)
  262     FORMAT('      UNRECOGNIZED CASE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,266)ICASPL
  266     FORMAT('      ICASPL = ',A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
           WRITE(ICOUT,1151)YMIN,Y50,YMAX,ISET,K,TEMP(K)
 1151      FORMAT('YMIN,Y50,YMAX,ISET,K,TEMP(K) = ',3G15.7,2I8,G15.7)
           CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ********************************************
C               **  STEP 4.11--                           **
C               **  DEFINE THE CHARACTER AT THE MAXIMUM;  **
C               ********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YMAX,YMAX,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C               ***************************************
C               **  STEP 4.12--                      **
C               **  DEFINE THE CHARACTER             **
C               **  FOR THE TYPICAL VALUE            **
C               **  (SUCH AS THE MEDIAN OR MEAN)     **
C               ***************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y50,Y50,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
C
C               ********************************************
C               **  STEP 4.13--                           **
C               **  DEFINE THE CHARACTER AT THE MINIMUM.  **
C               ********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YMIN,YMIN,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *************************************
C               **  STEP 4.14--                    **
C               **  DEFINE THE VERTICAL LINE FROM  **
C               **  THE MAX TO THE TYPICAL VALUE   **
C               *************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YMAX,Y50,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               **********************************
C               **  STEP 4.15--                 **
C               **  DEFINE THE VERTICAL LINE    **
C               **  FROM THE TYPICAL VALUE      **
C               **  TO THE MIN                  **
C               **********************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y50,YMIN,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
 1110 CONTINUE
C
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,NUMSET,N2,IERROR
 9012   FORMAT('ICASPL,N,NUMSET,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)AN,NI,N50,NUMV2,N2
 9014   FORMAT('AN,NI,N50,NUMV2,N2 = ',G15.7,4I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPICHA(ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--RETURN THE COLLATING SEQUENCE NUMBER (USUALLY THE
C              ASCII INDEX NUMBER) IN A PREVIOUSLY DEFINED STRING.
C              THIS IS ESSENTIALLY EQUIVALENT TO USING THE
C              ICHAR FUNCTION IN FORTRAN.  NOTE THAT ALL MAJOR
C              PLATFORMS CURRENTLY USE THE ASCII COLLATING SEQUENCE,
C              BUT A FEWER OLDER PLATFORMS DO NOT (E.G., EBCDIC
C              ON SOME IBM).
C     EXAMPLE--LET IVAL = ICHAR S
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/11
C     ORIGINAL VERSION--NOVEMBER  2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEL
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*8 ISTR
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOZI.INC'
C
      INTEGER ITEMP1(MAXOBV)
C
      EQUIVALENCE(ITEMP1(1),IGARBG(1))
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
      ISUBN1='DPIC'
      ISUBN2='HA  '
C
      IERROR='NO'
C
      ILOC3=0
C
C               *****************************************************
C               **  TREAT THE SUBCASE OF THE LET FUNCTION COMMAND  **
C               **  WHICH DEFINES A FUNCTION                       **
C               *****************************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ICHA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPICHA--')
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
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
      ICASEL='UNKN'
      NIOLD=0
      ICOLL=0
      ICOL2=0
C
C               ******************************************************
C               **  STEP 2--                                         *
C               **  EXAMINE THE LEFT-HAND SIDE--                     *
C               **  IF THIS IS A PREVIOUSLY DEFINED NAME, IT SHOULD  *
C               **  BE EITHER A PARAMETER OR A VARIABLE.             *
C               ******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
C
      DO1910I=1,4
        IF(IHLEFT(I:I).EQ.'(')THEN
          IHLEFT(I:4)=' '
          IHLEF2=' '
          ICASEL='ELEM'
          GOTO1999
        ENDIF
 1910 CONTINUE
      DO1920I=1,4
        IF(IHLEF2(I:I).EQ.'(')THEN
          IHLEF2(I:4)=' '
          ICASEL='ELEM'
          GOTO1999
        ENDIF
 1920 CONTINUE
 1999 CONTINUE
C
      DO2000I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I2).EQ.'P')THEN
            ICASEL='PARA'
            ILISTL=I2
            GOTO2900
          ELSEIF(IUSE(I2).EQ.'V')THEN
            ICASEL='VARI'
            ILISTL=I2
            ICOLL=IVALUE(ILISTL)
            NIOLD=IN(ILISTL)
            GOTO2900
          ELSE
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
 2001       FORMAT('***** ERROR IN ICHAR--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2003)IHLEFT,IHLEF2
 2003       FORMAT('      THE NAME ON THE LEFT HAND SIDE (',
     1             A4,A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2005)
 2005       FORMAT('      ALREADY EXISTS, BUT NOT AS A PARAMETER ',
     1             'OR A VARIABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
 2000 CONTINUE
C
      NEWNAM='YES'
C
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',
     1         'FUNCTION')
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
 2206   FORMAT('      THEN REDEFINE OR DELETE SOME OF THE ALREADY ',
     1         'USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2900 CONTINUE
C
C               *****************************************************
C               **  STEP 3--                                       **
C               **  EXTRACT THE NAME ON THE RIGHT HAND SIDE        **
C               *****************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRIGH=IHARG(4)
      IHRIG2=IHARG2(4)
      DO3000I=1,NUMNAM
        I4=I
        IF(IHRIGH.EQ.IHNAME(I).AND.IHRIG2.EQ.IHNAM2(I))THEN
          IF(IUSE(I4).NE.'F')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3003)IHRIGH,IHRIG2
 3003       FORMAT('      THE NAME ON THE RIGHT HAND SIDE (',
     1             A4,A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3005)
 3005       FORMAT('      ALREADY EXISTS, BUT NOT AS A STRING.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
            ISTART=IVSTAR(I4)
            ISTOP=IVSTOP(I4)
            NLEN=ISTOP-ISTART+1
            DO3010J=1,NLEN
              IINDX=ISTART+J-1
              CALL DPCOAN(IFUNC(IINDX)(1:1),IVAL)
              ITEMP1(J)=IVAL
 3010       CONTINUE
            GOTO3900
          ENDIF
        ENDIF
 3000 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3003)IHRIGH,IHRIG2
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3015)
 3015 FORMAT('      WAS NOT FOUND IN THE CURRENT NAME LIST.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 3900 CONTINUE
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  SAVE PARAMETER                                 **
C               *****************************************************
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')THEN
        ISTEPN='4'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4011)ISTART,ISTOP,IVAL
 4011   FORMAT('ISTART,ISTOP,IVAL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4013)ICASEL
 4013   FORMAT('ICASEL = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NLEN.EQ.1)THEN
        IF(ICASEL.EQ.'UNKN')ICASEL='PARA'
      ELSEIF(NLEN.GT.1)THEN
        IF(ICASEL.EQ.'UNKN')ICASEL='VARI'
      ENDIF
C
      IF(ICASEL.EQ.'PARA')THEN
C
        ISTEPN='4A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='P'
        VALUE(ILISTL)=REAL(ITEMP1(1))
        IVALUE(ILISTL)=VALUE(ILISTL)+0.5
        IN(ILISTL)=1
        IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
C
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,15111)IHLEFT,IHLEF2,ITEMP1(1)
15111     FORMAT(A4,A4,' = ',I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(ICASEL.EQ.'VARI')THEN
C
        ISTEPN='4B'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NEWNAM.EQ.'YES')THEN
          NUMNAM=NUMNAM+1
          NUMCOL=NUMCOL+1
          ICOLL=NUMCOL
        ENDIF
        DO15200I=1,NLEN
          RIGHT=REAL(ITEMP1(I))
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
C
15200   CONTINUE
C
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='V'
        IVALUE(ILISTL)=ICOLL
        VALUE(ILISTL)=ICOLL
        IN(ILISTL)=NLEN
C
C
        DO15210J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
            IUSE(J4)='V'
            IVALUE(J4)=ICOLL
            VALUE(J4)=ICOLL
            IN(J4)=NLEN
            GOTO15219
          ENDIF
15210   CONTINUE
15219   CONTINUE
C
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,15211)IHLEFT,IHLEF2,IHRIGH,IHRIG2
15211     FORMAT(A4,A4,' CONTAINS THE ASCII COLLATING SEQUENCE ',
     1           'VALUES FOR ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(ICASEL.EQ.'ELEM')THEN
C
        ISTEPN='4C'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       SEARCH IANS STRING FOR "(xx) =".  IF NO PARENTHESIS
C       FOUND BEFORE "=", THEN DO NOT KNOW WHAT ROW OF THE
C       VARIABLE TO SAVE.  TREAT THIS AS AN ERROR.
C
        NLEFT=-1
        NRIGHT=-1
        NEQUAL=-1
        DO16001I=1,IWIDTH
          IF(IANS(I)(1:1).EQ.'(' .AND. NLEFT.LT.0)THEN
            NLEFT=I
          ELSEIF(IANS(I)(1:1).EQ.')' .AND. NRIGHT.LT.0)THEN
            NRIGHT=I
          ELSEIF(IANS(I)(1:1).EQ.'=' .AND. NEQUAL.LT.0)THEN
            NEQUAL=I
          ENDIF
16001   CONTINUE
C
C       NEED  NLEFT < NRIGHT < NEQUAL
C
        NSTRT=NLEFT+1
        NSTOP=NRIGHT-1
        NLEN=NSTOP-NSTRT+1
        IF(NLEFT.GT.NRIGHT .OR. NRIGHT.GT.NEQUAL .OR.
     1     NSTRT.GT.NSTOP .OR. NLEN.GT.8) THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16011)
16011     FORMAT('      UNRECOGNIZED SYNTAX FOR VARIABLE ELEMENT ON')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16013)
16013     FORMAT('      LEFT HAND SIDE EQUAL SIGN.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSE
          ISTR=' '
          DO16020I=1,NLEN
            ISTR(I:I)=IANS(NSTRT+I-1)(1:1)
16020     CONTINUE
          READ(ISTR,'(I8)',ERR=16029)IARGL
          GOTO16049
C
16029     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16011)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16013)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
16049     CONTINUE
        ENDIF
C
        IF(IARGL.LT.1 .OR. IARGL.GT.MAXN)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16052)IARGL,ILEFT
16052     FORMAT('      THE SPECIFIED ROW (',I8,') OF VARIABLE ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16054)
16054     FORMAT('      WAS LESS THAN 1 OR GREATER THAN THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16055)MAXN
16055     FORMAT('      MAXIMUM ALLOWABLE ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(NEWNAM.EQ.'YES')THEN
          NIOLD=1
        ENDIF
        NINEW=NIOLD
        IF(IARGL.GT.NINEW)NINEW=IARGL
        NS2=1
C
        RIGHT=REAL(ITEMP1(1))
        IJ=MAXN*(ICOLL-1)+IARGL
        IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
        IF(ICOLL.EQ.MAXCP1)PRED(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP2)RES(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP3)YPLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP4)XPLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP5)X2PLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP6)TAGPLO(IARGL)=RIGHT
C
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='V'
        IVALUE(ILISTL)=ICOLL
        VALUE(ILISTL)=ICOLL
        IN(ILISTL)=NINEW
C
        IF(NEWNAM.EQ.'YES')THEN
          NUMNAM=NUMNAM+1
          NUMCOL=NUMCOL+1
        ENDIF
C
        DO16200J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
            IUSE(J4)='V'
            IVALUE(J4)=ICOLL
            VALUE(J4)=ICOLL
            IN(J4)=NINEW
            GOTO16209
          ENDIF
16200   CONTINUE
16209   CONTINUE
C
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16211)IHRIGH,IHRIG2,ITEMP1(1)
16211     FORMAT('THE ASCII COLLATING SEQUENCE VALUE OF  ',A4,A4,
     1           ' = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      GOTO9000
C
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ICHA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPICHA--')
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
      SUBROUTINE DPICLA(ICOM,ICOM2,
     1                  IMACRO,IMACNU,IMACCS,
     1                  IMACL1,IMACL2,IMACLR,IMALEV,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                  IANSLC,IWIDTH,
     1                  IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--WHEN THE CALL COMMAND IS ENTERED INSIDE OF A LOOP,
C              THE CALL COMMAND IS NOT INCLUDED IN THE SAVED LOOP
C              LINES.  RATHER THE CONTENTS OF THE CALL FILE ARE
C              SAVED.  ONE SIDE EFFECT OF THIS IS THAT ARGUMENTS
C              TO THE COMMAND LINE ARE NOT SAVED.
C
C              THIS COMMAND IMPLEMENTS THE COMMAND
C
C                   INSERT CALL ARGUMENTS <STRING>
C
C              WHERE <STRING> CONTAINS THE COMMAND LINE ARGUMENTS.
C
C              WHEN A CALL COMMAND IS EXECUTED IN LOOP STORE MODE AND
C              THE CALL COMMAND HAS ARGUMENTS, AN "INSERT CALL 
C              ARGUMENTS" COMMAND WILL BE ENTERED INTO THE SAVED LOOP
C              LINES (THIS IS DONE IN THE "DPMACR" ROUTINE).  THIS
C              COMMAND THEN IMPLEMENTS THAT COMMAND WHEN THE LOOP LINES
C              ARE EXECUTED.
C
C     INPUT  ARGUMENTS--ICOM
C                     --ICOM2
C     INPUT  ARGUMENTS--IMACNU (AN INTEGER VALUE
C                              BY WHICH THE MACRO FILE/SUBFILE MAY BE
C                              REFERENCED IN A FORTRAN I/O STATEMENT.
C                     --IMACCS (A HOLLERITH VARIABLE CONTAINING STATUS
C                              INFORMATION FOR THE MACRO FILE/SUBFILE
C                     --IANSLC (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
C                              CONTAINS THE I-TH CHARACTER OF THE
C                              ORIGINAL INPUT COMMAND LINE.
C                     --IWIDTH (AN INTEGER VARIABLE WHICH CONTAINS THE
C                              NUMBER OF CHARACTERS IN THE ORIGINAL
C                              COMMAND LINE.
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IBUG   (A HOLLERITH VARIABLE FOR DEBUGGING
C     OUTPUT ARGUMENTS--IMACRO (AN INTEGER VARIABLE WHICH IF 'ON'
C                              INDICATES THAT CURRENT COMMANDS ARE ALSO
C                              BEING DIVERTED SO AS TO CONSTRUCT A MACRO;
C                              AND IF OFF INDICATES THAT A MACRO IS NOT
C                              BEING CONSTRUCTED.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 Gaithersburg, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/07
C     ORIGINAL VERSION--JULY      2017.
C     UPDATED         --MAY       2018. UPDATED TO SUPPORT NEW FORMS OF
C                                       PASSING ARGUMENTS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 ICOM2
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANSLC
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 IANSI
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFILQZ
      CHARACTER*4 IHYPS2
      CHARACTER*1 ICJUNK
      CHARACTER*255 ICANS
      CHARACTER*255 ISTR
      CHARACTER*255 ISTR2
C
C ---------------------------------------------------------------------
C
      DIMENSION IANSLC(*)
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
C-----COMMON----------------------------------------------------------
C
      CHARACTER*80 IMANAM(10)
      COMMON/IMAC/IMACN2,IMALE2,IMANAM
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
C
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
      ISUBN1='DPIC'
      ISUBN2='LA  '
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,51)
   51    FORMAT('***** AT THE BEGINNING OF DPICLA--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,52)IMACRO,IMACNU,IMACCS,IMACL1,IMACL2
   52    FORMAT('IMACRO,IMACNU,IMACCS,IMACL1,IMACL2 = ',
     1          A4,I8,2X,A12,I8,I8)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,53)IBUGS2,IERROR,ICOM,ICOM2,MAXOBV,IWIDTH
   53    FORMAT('IBUGS2,IERROR,ICOM,ICOM2,MAXOBV,IWIDTH = ',
     1          4(A4,2X),2I8)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,56)(IANSLC(I),I=1,MIN(120,IWIDTH))
   56    FORMAT('IANSLC(.) = ',120A1)
         CALL DPWRST('XXX','BUG ')
C
         WRITE(ICOUT,57)NUMARG
   57    FORMAT('NUMARG = ',I8)
         CALL DPWRST('XXX','BUG ')
         IF(NUMARG.GE.1)THEN
            DO58I=1,NUMARG
               WRITE(ICOUT,59)I,IHARG(I),IHARG2(I)
   59          FORMAT('I,IHARG(I),IHARG2(I) = ',I8,2X,A4,2X,A4)
               CALL DPWRST('XXX','BUG ')
   58       CONTINUE
         ENDIF
C
         WRITE(ICOUT,62)NUMNAM,NUMCHA
   62    FORMAT('NUMNAM,NUMCHA = ',2I8)
         CALL DPWRST('XXX','BUG ')
         IF(NUMNAM.GE.1)THEN
            DO65I=1,NUMNAM
               WRITE(ICOUT,66)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1         IVALUE(I),VALUE(I)
   66          FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),',
     1         'IVALUE(I),VALUE(I) = ',I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
               CALL DPWRST('XXX','BUG ')
   65       CONTINUE
         ENDIF
      ENDIF
C
C               ***********************************************
C               **  STEP 1--                                 **
C               **  CHECK FOR                                **
C               **     INSERT CALL ARGUMENTS                 **
C               ***********************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'INSE ' .AND. IHARG(1).EQ.'CALL' .AND.
     1   IHARG(2).EQ.'ARGU')THEN
        IFOUND='YES'
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
C               **************************************************
C               **  STEP 63--                                   **
C               **  FIND THE FIRST AND LAST ROW OF THE SUB-CHUNK**
C               **  OF THE FILE BEING EXECUTED                  **
C               **  IMACL1 = FIRST LINE TO BE EXECUTED          **
C               **  IMACL2 = LAST  LINE TO BE EXECUTED          **
C               **  IMACLR = NUMBER OF LINES ALREADY EXECUTED   **
C               **************************************************
C
      ISTEPN='63'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICANS=' '
      DO6310I=1,IWIDTH
        ICANS(I:I)=IANSLC(I)(1:1)
 6310 CONTINUE
C
      IMACL1=1
      IMACL2=100000
      IHYPS2=IHYPSW
      ICOMCL='ON'
C
      DO6380II=1,50
        IMACAR(II)=' '
        IMACLA(II)=' '
        IMACLL(II)=0
 6380 CONTINUE
      NMACAG=0
      NMACLA=0
      NSARG=3
      IF(NUMARG.LT.NSARG)GOTO9000
C
      IFILQZ=IFILQU
C
C     DPTYPE DOES NOT SPLIT WORDS IN THE WAY NEEDED IN PARSING
C     COMMAND LINE ARGUMENTS.  CALL DPNUWO TO DETERMINE THE
C     NUMBER OF WORDS ON THE COMMAND LINE.
C
      ISTR=' '
      DO18394II=1,IWIDTH
        ISTR(II:II)=IANSLC(II)(1:1)
18394 CONTINUE
      ISTART=1
      CALL DPNUWO(ISTR,ISTART,IWIDTH,NWORD,
     1            IBUGS2,ISUBRO,IERROR)
C
      IFILQU='ON'
      DO6370J=NSARG+1,NWORD
        NMACAG=NMACAG+1
        IF(NMACAG.GT.50)GOTO6379
        ISTART=1
        ISTOP=IWIDTH
        IWORD=J
        IHYPSW='OFF'
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,IMACAR(NMACAG),NCTEMP,
     1              IBUGS2,ISUBRO,IERROR)
        IHYPSW=IHYPS2
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          ISTEPN='637'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,6394)J,IWORD,NMACAG,NCTEMP,IMACAR(NMACAG)
 6394     FORMAT('6370: J,IWORD,NMACAG,NCTEMP,IMACAR(NMACAG) = ',
     1           4I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       CHECK IF 80 CHARACTERS EXCEEDED (BE SURE TO MAKE LAST
C       CHARACTER A QUOTE IF FIRST CHARACTER IS A QUOTE).
C
        IF(NCTEMP.GT.80)THEN
          NCTEMP=80
          IF(IMACAR(NMACAG)(1:1).EQ.'"')
     1       IMACAR(NMACAG)(NCTEMP:NCTEMP)='"'
        ENDIF
C
C       REMOVE LEADING/TRAILING QUOTES IF NEEEDED
C
        IF(IMACAR(NMACAG)(1:1).EQ.'"' .AND.
     1     IMACAR(NMACAG)(NCTEMP:NCTEMP).EQ.'"')THEN
          IF(IQUOST.EQ.'ON')THEN
            IMACAR(NMACAG)(1:NCTEMP-2)=IMACAR(NMACAG)(2:NCTEMP-1)
            NCTEMP=NCTEMP-2
            IMACAR(NMACAG)(NCTEMP+1:NCTEMP+2)='  '
          ENDIF
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,17394)
17394     FORMAT('AFTER STRIP QUOTES: NMACAG,IMACAR(NMACAG) =',
     1           I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       2016/09: IF FIRST ARGUMENT IS "NULL", THEN BLANK OUT
C                ARGUMENT LIST AND SET NUMBER OF ARGUMENTS TO 0.
C
        IF(NMACAG.EQ.1)THEN
          IF(IMACAR(1).EQ.'NULL' .OR. IMACAR(1).EQ.'null')THEN
            IMACAR(1)=' '
            NMACAG=0
            GOTO6379
          ENDIF
        ENDIF
C
C       2018/05: ALLOW "EMPTY" ARGUMENTS, DO NOT SET TO ZZZZNULL
C
        IF(NCTEMP.EQ.1 .AND. IMACAR(NMACAG)(1:1).EQ.' ')THEN
          IMACAR(NMACAG)=' '
          NCTEMP=1
        ELSEIF(NCTEMP.EQ.0)THEN
          IMACAR(NMACAG)=' '
          NCTEMP=1
        ENDIF
C
C       2016/10: CHECK FOR NAMED ARGUMENTS
C
C       2018/04: CHECK FOR FOLLOWING 2 CASES
C
C                1.  "FRAME=FOR I = 1 1 50"
C                2.   FRAME="FOR I = 1 1 50"
C
C                AS FIRST STEP, CHECK FOR FIRST OCCURENCE OF QUOTE
C                (IF ANY) AND FIRST OCCURRENCE OF EQUAL SIGN.
C
C                NOTE THAT CODE WAS ADDED IN MAIN AND DPTYPE ROUTINES
C                SO THAT EQUAL SIGN WILL NOT BE A DELIMITER ON A CALL
C                COMMAND.
C
        IPOSQU=0
        IPOSEQ=0
C
C       CHECK FOR FIRST EQUAL CHARACTER.  HOWEVER, IF THE EQUAL
C       CHARACTER IS PRECEEDED BY AN ESCAPE CHARACTER ("\"), THEN
C       REMOVE THE ESCAPE CHARACTER BUT TREAT AS NO EQUAL CHARACTER
C       CASE.  START WITH CHARACTER POSITION 2 AS THERE NEEDS TO BE
C       AT LEAST ONE CHARACTER FOR THE ARGUMENT NAME.
C
        DO36311II=2,NCTEMP-1
          IF(IMACAR(NMACAG)(II:II).EQ.'=')THEN
            IF(II.GT.1 .AND. IMACAR(NMACAG)(II-1:II-1).EQ.'\')THEN
              IMACAR(NMACAG)(II-1:NCTEMP-1)=IMACAR(NMACAG)(II:NCTEMP)
              IMACAR(NMACAG)(NCTEMP:NCTEMP)=' '
              NCTEMP=NCTEMP-1
              GOTO36319
            ENDIF
            IPOSEQ=II
            GOTO36319
          ENDIF
36311   CONTINUE
36319   CONTINUE
C
C       NOW CHECK FOR OCCURENCE OF QUOTE.  NOTE THAT QUOTE IS ONLY
C       TREATED AS AN ARGUMENT DELIMITER IF IT IS THE FIRST CHARACTER
C       IN THE STRING OR THE FIRST CHARACTER AFTER THE EQUAL SIGN.  IN
C       ADDITION, IF A QUOTE DELIMITER IS FOUND, CHECK FOR A QUOTE AS
C       THE LAST CHARACTER.  IF NOT FOUND, THEN ADD IT.
C
        IF(IMACAR(NMACAG)(1:1).EQ.'"')THEN
          IPOSQU=1
        ELSEIF(IMACAR(NMACAG)(IPOSEQ+1:IPOSEQ+1).EQ.'"')THEN
          IPOSQU=IPOSEQ+1
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,36394)IPOSEQ,IPOSQU,NCTEMP
36394     FORMAT('IPOSEQ,IPOSQU,NCTEMP = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       IF ENDING QUOTE NOT PRESENT, ADD IT.
C
        IF(IPOSQU.GT.0)THEN
          IF(IMACAR(NMACAG)(NCTEMP:NCTEMP).EQ.')')THEN
            IF(IMACAR(NMACAG)(NCTEMP-1:NCTEMP-1).NE.'"')THEN
              NCTEMP=NCTEMP+1
              IMACAR(NMACAG)(NCTEMP-1:NCTEMP)='")'
            ENDIF
          ELSE
            IF(IMACAR(NMACAG)(NCTEMP:NCTEMP).NE.'"')THEN
              NCTEMP=NCTEMP+1
              IMACAR(NMACAG)(NCTEMP:NCTEMP)='"'
            ENDIF
          ENDIF
        ENDIF
C
C       PROCESS STRING BASED ON WHETHER QUOTES/EQUAL SIGNS ARE PRESENT.
C
        IF(IPOSEQ.EQ.0 .AND. IPOSQU.EQ.0)THEN
C
C         CASE WITH NO EQUAL AND NO QUOTE.  IN THIS CASE, WE HAVE
C         A POSITIONAL ARGUMENT AND DO NOT NEED TO PROCESS QUOTES.
C
C         IN THIS CASE, NO ADDITIONAL PROCESSING IS REQUIRED.
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46351)
46351       FORMAT('NO EQUAL, NO QUOTE CASE: NOTHING DONE')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSEIF(IPOSEQ.EQ.0 .AND. IPOSQU.GE.1)THEN
C
C         CASE WITH NO EQUAL BUT WITH QUOTE.  IN THIS CASE, WE HAVE
C         A POSITIONAL ARGUMENT AND WE NEED TO PROCESS QUOTES.
C
C         ONLY PROCESSING REQUIRED IS TO STRIP OFF LEADING/TRAILING
C         QUOTE IF THAT OPTION SET.
C
          IF(IQUOST.EQ.'ON')THEN
            IF(IMACAR(NMACAG)(NCTEMP:NCTEMP).EQ.')')THEN
              IMACAR(NMACAG)(1:NCTEMP-3)=IMACAR(NMACAG)(2:NCTEMP-2)
              NCTEMP=NCTEMP-2
              IMACAR(NMACAG)(NCTEMP:NCTEMP+2)=')  '
            ELSE
              IMACAR(NMACAG)(1:NCTEMP-2)=IMACAR(NMACAG)(2:NCTEMP-1)
              IMACAR(NMACAG)(NCTEMP+1:NCTEMP+2)='  '
              NCTEMP=NCTEMP-2
            ENDIF
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46361)
46361       FORMAT('NO EQUAL, QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46363)NMACAG,NCTEMP,IMACAR(NMACAG)
46363       FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I8,A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSEIF(IPOSEQ.GE.1 .AND. IPOSQU.EQ.0)THEN
C
C         CASE WITH EQUAL AND NO QUOTE.  IN THIS CASE, WE HAVE
C         A NAMED ARGUMENT AND DO NOT NEED TO PROCESS QUOTES.
C
C         IN THIS CASE, NEED TO MODIFY THE MACRO NAME TABLE AND
C         ALSO ADJUST THE ARGUMENT STRING.
C
          NMACLA=NMACLA+1
          IMACLL(NMACLA)=NMACAG
          IMACLA(NMACLA)(1:IPOSEQ-1)=IMACAR(NMACAG)(1:IPOSEQ-1)
          IMACNC(NMACLA)=IPOSEQ-1
          ICNT2=NCTEMP-IPOSEQ
          IF(ICNT2.GE.1)THEN
            IMACAR(NMACAG)(1:ICNT2)=IMACAR(NMACAG)(IPOSEQ+1:NCTEMP)
            IMACAR(NMACAG)(ICNT2+1:NCTEMP)=' '
            NCTEMP=ICNT2
          ELSE
            IMACAR(NMACAG)=' '
            NCTEMP=1
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46371)
46371       FORMAT('EQUAL, NO QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46373)NMACAG,NCTEMP,IMACAR(NMACAG)
46373       FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I8,A80)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),
     1                        IMACLA(NMACLA)
46375       FORMAT('NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),'
     1             'IMACLA(NMACLA) = ',3I8,A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSEIF(IPOSEQ.GE.1 .AND. IPOSQU.GE.1)THEN
C
C         CASE WITH EQUAL AND QUOTE.  IN THIS CASE, WE HAVE
C         A NAMED ARGUMENT AND WE NEED TO PROCESS QUOTES.
C
C         TREAT CASE WHERE QUOTE IS FOR THE FULL STRING SEPARATELY
C         FROM CASE WHERE QUOTE IS FOR THE VALUE ONLY.
C
          IF(IPOSQU.EQ.1)THEN
            NMACLA=NMACLA+1
            IMACLL(NMACLA)=NMACAG
            ICNT=IPOSEQ-2
            IMACLA(NMACLA)(1:ICNT)=IMACAR(NMACAG)(2:IPOSEQ-1)
            IMACNC(NMACLA)=ICNT
            ICNT2=NCTEMP-IPOSEQ-1
            IF(ICNT2.GE.1)THEN
              IMACAR(NMACAG)(1:ICNT2)=
     1          IMACAR(NMACAG)(IPOSEQ+1:NCTEMP-1)
              IMACAR(NMACAG)(ICNT2+1:80)=' '
              NCTEMP=ICNT2
            ELSE
              IMACAR(NMACAG)=' '
              NCTEMP=1
            ENDIF
          ELSE
            NMACLA=NMACLA+1
            IMACLL(NMACLA)=NMACAG
            ICNT=IPOSEQ-1
            IMACLA(NMACLA)(1:ICNT)=IMACAR(NMACAG)(1:IPOSEQ-1)
            IMACNC(NMACLA)=ICNT
            ICNT2=NCTEMP-IPOSEQ
            IF(ICNT2.GE.1)THEN
              IMACAR(NMACAG)(1:ICNT2)=IMACAR(NMACAG)(IPOSEQ+1:NCTEMP)
              IMACAR(NMACAG)(ICNT2+1:80)=' '
              NCTEMP=ICNT2
            ELSE
              IMACAR(NMACAG)=' '
              NCTEMP=1
            ENDIF
            IF(IQUOST.EQ.'ON' .AND. NCTEMP.GE.2)THEN
              IF(IMACAR(NMACAG)(NCTEMP:NCTEMP).EQ.')')THEN
                IMACAR(NMACAG)(1:NCTEMP-3)=IMACAR(NMACAG)(2:NCTEMP-2)
                NCTEMP=NCTEMP-2
                IMACAR(NMACAG)(NCTEMP:NCTEMP+2)=')  '
              ELSE
                IMACAR(NMACAG)(1:NCTEMP-2)=IMACAR(NMACAG)(2:NCTEMP-1)
                NCTEMP=NCTEMP-2
                IMACAR(NMACAG)(NCTEMP+1:NCTEMP+2)='  '
              ENDIF
              IF(NCTEMP.LE.0)THEN
                IMACAR(NMACAG)=' '
                NCTEMP=1
              ENDIF
            ENDIF
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46381)
46381       FORMAT('EQUAL, QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46373)NMACAG,NCTEMP,IMACAR(NMACAG)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),
     1                        IMACLA(NMACLA)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ENDIF
C
        IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,6373)NMACAG,NCTEMP,IMACAR(NMACAG)
 6373     FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I5,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 6370 CONTINUE
 6379 CONTINUE
      IFILQU=IFILQZ
C
C     2018/04: CHECK FOR OPENING AND CLOSING PARENTHEIS AROUND FULL
C              ARGUMENT LIST (E.G., CALL (Y=Y,X=X)).
C
C              NOTE THAT THE LEFT PARENTHESIS MAY BE PART OF THE
C              ARGUMENT LABEL (IF GIVEN) OR THE ARGUMENT VALUE
C              (IF ARGUMENTS ENTERED BY POSITION).  THE RIGHT
C              PARENTHESIS WILL ALWAYS BE PART OF THE ARGUMENT
C              VALUE.
C
C              AS A FURTHER COMPLICATION, NEED TO CHECK IF THE
C              LEFT PARENTHESIS IS FOLLOWED BY A SPACE AND LIKEWISE
C              IF THE RIGHT PARENTHESIS IS PRECEEDED BY A SPACE.
C
      IF(NMACAG.GE.1)THEN
C
C       STEP 1: CHECK IF EITHER FIRST LABEL OR FIRST ARGUMENT IS
C               STARTS WITH A PARENTHESIS.  CHECK LABEL FIRST.
C
        IFLAG=0
        IF(IMACLA(1)(1:1).EQ.'(')THEN
          IFLAG=1
          DO16392KK=1,7
            IMACLA(1)(KK:KK)=IMACLA(1)(KK+1:KK+1)
16392     CONTINUE
          IMACLA(1)(8:8)=' '
          IMACNC(1)=IMACNC(1)-1
C
C         NOW REMOVE ANY LEADING SPACES FROM LABEL
C
          DO16394KK=1,IMACNC(1)
            IF(IMACLA(1)(KK:KK).NE.' ')THEN
              IF(KK.GT.1)THEN
                NCTEMP=IMACNC(1) - KK + 1
                IMACLA(1)(1:NCTEMP)=IMACLA(1)(KK:IMACNC(1))
                IMACLA(1)(NCTEMP+1:8)=' '
                IMACNC(1)=NCTEMP
              ENDIF
              GOTO16396
            ENDIF
16394     CONTINUE
16396     CONTINUE
        ELSEIF(IMACAR(1)(1:1).EQ.'(')THEN
          IFLAG=2
          DO16393KK=1,79
            IMACAR(1)(KK:KK)=IMACAR(1)(KK+1:KK+1)
16393     CONTINUE
          IMACAR(1)(80:80)=' '
        ENDIF
C
        IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,46661)IFLAG
46661     FORMAT('IFLAG = ',I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       STEP 2: CHECK IF LAST ARGUMENT VALUE ENDS WITH RIGHT PARENTHESIS
C
        DO16391JJ=80,1,-1
          IF(IMACAR(NMACAG)(JJ:JJ).EQ.')')THEN
            IMACAR(NMACAG)(JJ:JJ)=' '
          ENDIF
16391   CONTINUE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,46391)
46391     FORMAT('AFTER REMOVE LEADING/TRAILING PARENTHESIS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46392)NMACLA,IMACLL(1),IMACNC(1),
     1                      IMACNC(1),IMACLA(1)
46392     FORMAT('NMACLA,IMACLL(1),IMACNC(1),IMACLA(1) = ',
     1           3I8,2X,A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),
     1                      IMACNC(NMACL),IMACLA(NMACLA)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
 6399 CONTINUE
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
        WRITE(ICOUT,9010)NMACLA,NMACAG
 9010   FORMAT('NMACLA,NMACAG = ',2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NMACLA.GE.1)THEN
          DO9011JJ=1,NMACLA
            WRITE(ICOUT,9013)JJ,IMACLL(JJ),IMACLA(JJ)
 9013       FORMAT('JJ,IMACLL(JJ),IMACLA(JJ) = ',2I5,2X,A8)
            CALL DPWRST('XXX','BUG ')
 9011     CONTINUE
        ENDIF
C
        IF(NMACAG.GE.1)THEN
          DO9020JJ=1,NMACAG
            WRITE(ICOUT,9021)JJ,IMACAR(JJ)
 9021       FORMAT('JJ,IMACAR(JJ) = ',I5,2X,A80)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPICOM(Y,X,N,MINSIZ,
     1Y2,XLOW,XUPP,N2,
     1ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--FOR DISCRETE DISTRIBUTIONS, WE TYPICALLY WANT TO
C              GENERATE A FREQUENCY DISTRIBUTION FOR THE NON-NEGATIVE
C              INTEGERS.  THIS ROUTINE WILL DO THAT.  TWO ADDITIONAL
C              FEATURES:
C
C              1) FOR LONG-TAILED DISTRIBUTIONS (E.G., THE YULE
C                 OR ZETA DISTRIBUTIONS, WE WILL HAVE AN EXTREMELY
C                 LARGE NUMBER OF EMPTY CELLS IN THE TAIL.  SO
C                 THIS ROUTINE WILL RETURN THE FREQUENCY TABLE
C                 IN THE FORM:
C
C                    FREQ  CLASS-LOWER-LIMIT  CLASS-UPPER-LIMIT
C
C                 EMPTY CLASSES WILL BE COMBINED WITH THE NEXT
C                 HIGHEST NON-EMPTY CLASS.
C
C              2) FOR THE CHI-SQUARE GOODNESS OF FIT, IT IS
C                 RECOMMENDED THAT CLASSES WITH LESS THAN 5
C                 OBSERVATIONS BE COMBINED IN ORDER FOR THE CHI-SQUARE
C                 GOODNESS OF FIT TES TO BE VALID.  AFTER COMPUTING
C                 THE FREQUENCY TABLE, CLASSES WILL BE COMBINED SO
C                 THAT ALL CLASSES HAVE A FREQUENCY OF AT LEAST
C                 "MINSIZ" WHERE "MINSIZ" IS SET BY THE USER
C                 (THE DEFAULT VALUE IS 5).
C
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
C     VERSION NUMBER--2006/5
C     ORIGINAL VERSION--MAY       2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION XLOW(*)
      DIMENSION XUPP(*)
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
      ISUBN1='DPIC'
      ISUBN2='OM  '
C
      IERROR='NO'
      IWRITE='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN INTEGER FREQUENCY TABLE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF INPUT VALUE IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF INPUT VALUES HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ICOM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPICOM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,MINSIZ
   72   FORMAT('N,MINSIZ = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I)
   74     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 2--                              **
C               **  ROUND TO NEAREST INTEGER VALUE (AND   **
C               **  CHECK FOR NEGARIVE VALUES)            **
C               ********************************************
C
      DO100I=1,N
        ITEMP=INT(Y(I)+0.5)
        IF(ITEMP.LT.0)THEN
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)I,Y(I)
  102     FORMAT('      ROW ',I8,' IS NON-POSITIVE.  VALUE = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        Y(I)=REAL(ITEMP)
  100 CONTINUE
C
C               ********************************************
C               **  STEP 3--                              **
C               **  1) SORT                               **
C               **  2) EXTRACT DISTINCT VALUES IN INPUT   **
C               **     VECTOR                             **
C               **  3) GENERATE THE FREQUENCY TABLE       **
C               ********************************************
C
      CALL SORT(Y,N,Y)
      CALL DISTIN(Y,N,IWRITE,X,NDIST,IBUGA3,IERROR)
C
C     CHECK IF ALL DATA VALUES EQUAL TO SAME VALUE.
C
      IF(NDIST.EQ.1)THEN
        Y2(1)=X(1)
        XLOW(1)=X(1)-0.5
        XUPP(1)=X(1)+0.5
        N2=1
        GOTO9000
      ENDIF
C
      DO200I=1,NDIST
        Y2(I)=0.0
        XLOW(I)=0.0
        XUPP(I)=0.0
  200 CONTINUE
C
      DO300J=1,NDIST
        AHOLD=X(J)
        IF(J.EQ.1)THEN
          XLOW(J)=AHOLD-0.5
          AHOLD2=X(J+1)
          XUPP(J)=AHOLD2-0.5
        ELSEIF(J.EQ.NDIST)THEN
          XUPP(J)=AHOLD+0.5
          XLOW(J)=XUPP(J-1)
        ELSE
          XLOW(J)=XUPP(J-1)
          XUPP(J)=AHOLD+0.5
        ENDIF
        DO310I=1,N
          IF(Y(I).EQ.AHOLD)THEN
            Y2(J)=Y2(J)+1
          ENDIF
  310   CONTINUE
  300 CONTINUE
C
C
C               **********************************************
C               **  STEP 4--                                **
C               **  COMBINE CLASSES WITH A FREQUECNY LESS   **
C               **  THAN MINSIZ.                            **
C               **********************************************
C
      N2=0
      IFLAG=0
      ISTRT=1
      ICNT2=NDIST
      AMINSZ=REAL(MINSIZ)
      EPS=1.0E-10
C
C  RIGHT TAIL TO CENTER.  TEMPORARILY STORE IN UPPER PART OF
C  XLOW, XUPP, AND Y2 ARRARYS, WILL THEN FLIP THE SORT AT THE
C  END.
C
      DO400I=NDIST,1,-1
        ALOW=XLOW(I)
        AHIGH=XUPP(I)
        ATEMP=Y2(I)
        IF(IFLAG.EQ.0)THEN
          IF(ATEMP+EPS.GE.AMINSZ)THEN
            ICNT2=ICNT2+1
            XLOW(ICNT2)=ALOW
            XUPP(ICNT2)=AHIGH
            Y2(ICNT2)=ATEMP
          ELSE
            IFLAG=1
            ASUM=ATEMP
            ISTOP=I
          ENDIF
        ELSE
          ASUM=ASUM + ATEMP
          IF(ASUM+EPS.GE.AMINSZ)THEN
            ICNT2=ICNT2 + 1
            XLOW(ICNT2)=ALOW
            XUPP(ICNT2)=XUPP(ISTOP)
            Y2(ICNT2)=ASUM
            ISTOP=-1
            IFLAG=0
          ENDIF
        ENDIF
  400 CONTINUE
C
      IF(IFLAG.EQ.1 .AND. ASUM.GT.0.0)THEN
        XLOW(ICNT2)=XLOW(1)
        XUPP(ICNT2)=XLOW(ICNT2-1)
        Y2(ICNT2)=Y2(ICNT2) + ASUM
      ENDIF
      N2RGHT=ICNT2
C
C  NOW COPY REVERSE ORDER RIGHT TAIL ENTRIES
C
      ICNT=0
      DO500I=ICNT2,NDIST+1,-1
        ICNT=ICNT+1
        Y2(ICNT)=Y2(I)
        XLOW(ICNT)=XLOW(I)
        XUPP(ICNT)=XUPP(I)
  500 CONTINUE
      N2=ICNT
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPICOM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,N2
 9012   FORMAT('IERROR,N2 = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),XLOW(I),XUPP(I)
 9016     FORMAT('I,Y2(I),XLOW(I),XUPP(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIF(ILOCS,ICASIF,IBUGQ,ISUBRO,IERROR)
C
C     PURPOSE--DEFINE A TRUE-FALSE CHARACTER VARIABLE
C              WHICH WILL BE USED IN OTHER SUBROUTINES
C              FOR THE CONDITIONAL EXECTUION OF STATEMENTS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83/1
C     ORIGINAL VERSION--JANUARY   1983.
C     UPDATED         --AUGUST    1987. (TO ALLOW <> TO WORK)
C     UPDATED         --AUGUST    1992. TO ALLOW    ... NOT EXIST
C     UPDATED         --AUGUST    1997. TO ALLOW    ... EXIST
C     UPDATED         --FEBRUARY  1999. IF ERROR, SET IF TO FALSE
C     UPDATED         --JULY      2002. REDO IF NOT EXIST AND IF EXIST
C     UPDATED         --JULY      2002. ADD: IF STRING = "..."
C     UPDATED         --SEPTEMBER 2012. ADD ISUBRO
C     UPDATED         --NOVEMBER  2014. FIX BUG WITH "<>" FOR STRINGS
C     UPDATED         --NOVEMBER  2014. FOR STRINGS, LET RHS BE A
C                                       PRE-DEFINED STRING
C     UPDATED         --FEBRUARY  2015. SUPPORT "SET FATAL ERROR"
C     UPDATED         --OCTOBER   2016. IF COMMAND LINE ARGUMENT xx
C                                       EXISTS
C     UPDATED         --MARCH     2017. SUPPORT AND, OR, AND XOR
C                                       FOR TWO IF CLAUSES
C     UPDATED         --FEBRUARY  2018. SUPPORT FOR AND, OR, AND XOR
C                                       EXTENDED TO THREE OR MORE IF
C                                       CLAUSES
C     UPDATED         --FEBRUARY  2018. FOR CASE   IF A = 1
C                                       IF A DOES NOT EXIST, SET TO
C                                       FALSE BUT DO NOT GENERATE
C                                       ERROR MESSAGE
C     UPDATED         --MAY       2018. SUPPORT NUMERIC VALUE FOR LHS:
C                                          IF 3 > 2
C     UPDATED         --MAY       2018. SUPPORT QUOTED LITERAL STRING
C                                       FOR LHS:
C                                          IF "XXXX" = SOLD
C     UPDATED         --MAY       2018. COMMAND LINE ARGUMENT ... NOT EXIST
C     UPDATED         --MAY       2018. BETTER MESSAGING FOR SPECIAL
C                                       CASES
C     UPDATED         --JUNE      2018. FIXED ISSUE WITH
C                                         IF " " = ...
C     UPDATED         --SEPTEMBER 2018. SUPPORT "<", "<=", ">", ">="
C                                       FOR STRINGS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASIF
      CHARACTER*4 ICASI1
      CHARACTER*4 ICASI2
      CHARACTER*4 ICASI3
      CHARACTER*4 INOT
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPER(10)
C
      CHARACTER*4 ISTATI
      CHARACTER*4 ICASSC
      CHARACTER*4 ICASQU
      CHARACTER*4 ICASPA
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASOP
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IFILQZ
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*1 IC1
C
      CHARACTER*8 ITYPE
      CHARACTER*80 ITEXT1
      CHARACTER*80 ITEXT2
      CHARACTER*80 ISTRIN
      CHARACTER*80 ISTRI2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOSU.INC'
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
      ISUBN1='DPIF'
      ISUBN2='    '
      IERROR='NO'
      ICASIF='TRUE'
      ICASI1='NULL'
      ICASI2='NULL'
      INOT='OFF'
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
C
C               ********************************
C               **  TREAT THE IF     CASE     **
C               ********************************
C
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ILOCS,ICASIF,IBUGQ,IERROR
   52   FORMAT('ILOCS,ICASIF,IBUGQ,IERROR = ',I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMARG,NUMNAM,MAXNAM,N,MAXN
   55   FORMAT('NUMARG,NUMNAM,MAXNAM,N,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IWIDTH,ILOCS
   56   FORMAT('IWIDTH,ILOCS = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.   **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************************
C               **  STEP 1B-                                          **
C               **  CHECK FOR LOGICAL OPERATORS:                      **
C               **            AND                                     **
C               **            OR                                      **
C               **            XOR                                     **
C               ********************************************************
C
C               NOTE: CURRENTLY ALLOW ONLY ONE "AND", "OR", "XOR"
C                     OPERATOR.
C
C               NOTE (2017/07): EXTEND TO ALLOW TWO "AND", "OR", "XOR"
C                               OPERATORS
C
C                               HAS NOW BEEN EXTENDED TO "MAXLOG"
C                               (CURRENTLY SET TO 10)
C
      ISTEPN='1B'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MAXLOG=10
      NOPER=0
      NPASS=1
      JPASS=0
      NPOSLO=101
      DO59II=1,MAXLOG
        IOPER(II)='    '
   59 CONTINUE
      ICNT3=0
      IWIDTT=IWIDTH
C
C     SEARCH FOR AND, OR, XOR IN IANS
C
1000  CONTINUE
C
      ISTEPN='1C'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMART=NUMARG
C
      IF(NUMARG.LE.1)THEN
        IF(JPASS.GE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,60)ICASIF
   60     FORMAT('       COMBINED IF STATUS   = ',A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ELSE
           ICASIF='FALS'
           IF(IFEEDB.EQ.'ON')THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,1011)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,7112)
 7112        FORMAT('      THERE IS ONLY ONE (OR ZERO) ARGUMENT TO ',
     1              'THE IF COMMAND')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,7017)ICASIF
             CALL DPWRST('XXX','BUG ')
           ENDIF
           GOTO9000
        ENDIF
        GOTO9009
      ENDIF
C
      IFLAG=0
      DO61II=1,NUMARG
        IF(IHARG(II).EQ.'AND ' .OR. IHARG(II).EQ.'OR  ' .OR.
     1     IHARG(II).EQ.'XOR ')THEN
          NOPER=NOPER+1
          NPASS=NPASS+1
          IF(NOPER.LE.MAXLOG)IOPER(NOPER)=IHARG(II)
          NPOSLO=II
          IFLAG=1
          GOTO63
        ENDIF
   61 CONTINUE
   63 CONTINUE
C
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,66)NOPER,NPASS,NPOSLO,NUMART,IFLAG
   66   FORMAT('NOPER,NPASS,NPOSLO,NUMART,IFLAG = ',5I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NOPER.GT.MAXLOG)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)MAXLOG
   68   FORMAT('      MORE THAN ',I2,' LOGICAL OPERATORS (AND, OR, ',
     1         'XOR) DETECTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     FIND LOCATION IN STRING
C
      IF(NPASS.GE.2 .AND. IFLAG.EQ.1)THEN
        NUMART=NPOSLO-1
        IF(IOPER(NOPER).EQ.'AND ')THEN
          DO71I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'A' .AND.
     1         IANS(I+2)(1:1).EQ.'N' .AND. IANS(I+3)(1:1).EQ.'D' .AND.
     1         IANS(I+4)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GOTO79
            ENDIF
   71     CONTINUE
        ELSEIF(IOPER(NOPER).EQ.'OR  ')THEN
          DO72I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'O' .AND.
     1         IANS(I+2)(1:1).EQ.'R' .AND. IANS(I+3)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GOTO79
            ENDIF
   72     CONTINUE
        ELSEIF(IOPER(NOPER).EQ.'XOR ')THEN
          DO73I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'X' .AND.
     1         IANS(I+2)(1:1).EQ.'O' .AND. IANS(I+3)(1:1).EQ.'R' .AND.
     1         IANS(I+4)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GOTO79
            ENDIF
   73     CONTINUE
        ELSE
          IWIDTT=IWIDTH
          ILOCP2=IWIDTH
        ENDIF
C
   79   CONTINUE
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,74)IWIDTT,ILOCP2,ILOCSV
   74     FORMAT('AT 74: IWIDTT,ILOCP2,ILOCSV = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C     2016/10: CHECK IF FIRST ARGUMENT IS "NOT"
C
      IF(NUMART.GE.1 .AND. IHARG(1).EQ.'NOT ')THEN
        INOT='ON'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGQ,IERROR)
        NUMART=NUMART-1
      ENDIF
C
C
CCCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1992
C               **************************************************
C               **  STEP 2.0--                                  **
C               **  TREAT THE     IF ... NOT EXIST CASE         **
C               **      IF ... NOT EXIST THEN ==> ICASIF = TRUE **
C               **      IF ... EXIST THEN ==> ICASIF = FALSE    **
C               **************************************************
C
C               2016/10: ADD
C
C                        IF COMMAND LINE ARGUMENT <name> EXISTS
C
C               2018/05: ADD
C
C                        IF COMMAND LINE ARGUMENT <name> NOT EXISTS
C
      ISTEPN='2'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMART.GE.5)THEN
        IF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.
     1     IHARG(3).EQ.'ARGU' .AND. IHARG(5).EQ.'EXIS')THEN
C
          ISTRIN=' '
          ISTRI2=' '
          DO75II=1,IWIDTT
            ISTRIN(II:II)=IANSLC(II)(1:1)
   75     CONTINUE
          ISTART=1
          ISTOP=IWIDTT
          IWORD=5
          CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                ICOL1,ICOL2,ISTRI2,NCSTRI,
     1                IBUGQ,ISUBRO,IERROR)
          DO77II=1,NMACLA
            IF(ISTRI2(1:8).EQ.IMACLA(II)(1:8))THEN
              IEXIST='YES'
              ICASIF='TRUE'
              GOTO7011
            ENDIF
   77     CONTINUE
          IEXIST='NO'
          ICASIF='FALS'
C
 7011     CONTINUE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7012)IHARG(4),IHARG2(4)
 7012       FORMAT('      IF     NAME         = ',2A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7013)IEXIST
 7013       FORMAT('      IF     NAME EXISTS  = ',A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7017)ICASIF
 7017       FORMAT('      IF     STATUS       = ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          GOTO9000
C
        ELSEIF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.
     1     IHARG(3).EQ.'ARGU' .AND. IHARG(5).EQ.'NOT ' .AND.
     1     IHARG(6).EQ.'EXIS')THEN
C
          ISTRIN=' '
          ISTRI2=' '
          DO7078II=1,IWIDTT
            ISTRIN(II:II)=IANSLC(II)(1:1)
 7078     CONTINUE
          ISTART=1
          ISTOP=IWIDTT
          IWORD=5
          CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                ICOL1,ICOL2,ISTRI2,NCSTRI,
     1                IBUGQ,ISUBRO,IERROR)
          DO7079II=1,NMACLA
            IF(ISTRI2(1:8).EQ.IMACLA(II)(1:8))THEN
              ICASIF='FALS'
              IEXIST='YES'
              GOTO7081
            ENDIF
 7079     CONTINUE
          IEXIST='NO'
          ICASIF='TRUE'
C
 7081     CONTINUE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7012)IHARG(4),IHARG2(4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7013)IEXIST
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7017)ICASIF
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          GOTO9000
C
        ENDIF
      ENDIF
C
      IF(NUMART.GE.3)THEN
         IF(IHARG(2).EQ.'NOT'.AND.IHARG(3).EQ.'EXIS')THEN
C
            IH=IHARG(1)
            IH2=IHARG2(1)
            MESSAG='NO'
            CALL CHECKF(IH,IH2,IHWUSE,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,
     1                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
C
            IF(ITYPE.EQ.'NONE')THEN
              IEXIST='NO'
              ICASIF='TRUE'
            ELSE
              IEXIST='YES'
              ICASIF='FALS'
            ENDIF
            IERROR='NO'
C
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1011)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7012)IHARG(4),IHARG2(4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7013)IEXIST
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7017)ICASIF
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            GOTO9000
C
         ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED NOVEMBER 1997
C               **************************************************
C               **  STEP 2.0A--                                 **
C               **  TREAT THE     IF ...     EXIST CASE         **
C               **      IF ...     EXIST THEN ==> ICASIF = TRUE **
C               **      IF ... NOT EXIST THEN ==> ICASIF = FALSE**
C               **************************************************
C
      ISTEPN='2A'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASIF='TRUE'
      IF(NUMART.GE.2)THEN
         IF(IHARG(2).EQ.'EXIS')THEN
C
            IH=IHARG(1)
            IH2=IHARG2(1)
            MESSAG='NO'
            ILOC=-99
            ITYPE='NONE'
            CALL CHECKF(IH,IH2,IHWUSE,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,
     1                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
C
            IF(ITYPE.EQ.'NONE')THEN
              ICASIF='FALS'
            ELSE
              ICASIF='TRUE'
            ENDIF
            IERROR='NO'
C
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1011)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7012)IHARG(4),IHARG2(4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7013)IEXIST
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7017)ICASIF
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            GOTO9000
C
         ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED JULY 2002.
C               **************************************************
C               **  STEP 2.0B-                                  **
C               **  TREAT THE                                   **
C               **      IF STRING = "....."     CASE            **
C               **      IF STRING <> "..."      CASE            **
C               **************************************************
C
C     ORIGINAL IMPLEMENTATION REQUIRED PRE-DEFINED STRING ON
C     LEFT HAND SIDE OF "=" (OR "<>"). 
C
C     2018/05: FOR STRINGS, MAKE MATCH TRUE IF ONE SIDE IS A
C              SINGLE BLANK CHARACTER AND THE OTHER IS A
C              NULL (NO CHARACTERS) STRING.
C
C     2018/06: THE FOLLOWING WAS A PROBLEM
C
C                 IF " " = ....
C
C              IN THIS CASE, THE EQUAL SIGN IS IN IHARG(3), NOT
C              IHARG(2).  PACK THIS INTO "".
C
C              RELATED TO THIS, CONSTRUCTS LIKE
C
C                 IF "SUBSET Y 4" = ....
C
C              ARE AN ISSUE.   CHECK FOR LOCATION OF THE EQUAL SIGN.
C
      ISTEPN='2B'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'IF')THEN
C
        IF(IHARG(1).EQ.'"   ' .AND. IHARG(2).EQ.'"   ')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGQ,IERROR)
          IHARG(1)(1:1)='"'
          IHARG(1)(2:2)='"'
        ENDIF
C
        IPOSEQ=-1
        DO147KK=2,NUMARG-1
          IF(IHARG(KK).EQ.'=   ' .OR. IHARG(KK).EQ.'<>  ' .OR.
     1       IHARG(KK).EQ.'<=  ' .OR. IHARG(KK).EQ.'>=  ' .OR.
     1       IHARG(KK).EQ.'<   ' .OR. IHARG(KK).EQ.'>   ')THEN
            IPOSEQ=KK
            GOTO148
          ENDIF
  147   CONTINUE
  148   CONTINUE
C
CCCCC   IF(NUMART.GE.3.AND.(IHARG(2).EQ.'='.OR.IHARG(2).EQ.'<>'))THEN
        IF(NUMART.GE.3.AND.IPOSEQ.GT.0)THEN
C
          IH=IHARG(1)
          IH2=IHARG2(1)
          MESSAG='NO'
          CALL CHECKF(IH,IH2,IHWUSE,
     1    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
C
C         IF FIRST ARGUMENT IS A PREVIOSLY DEFINED STRING, EXTRACT
C         THE VALUE.  OTHERWISE, CHECK TO SEE IF THE FIRST ARGUMENT
C         IS A QUOTED STRING.  LITERAL STRINGS MUST BE ENCLOSED IN
C         DOUBLE QUOTES (IF NOT, THE ARGUMENT IS INTERPRETED AS A
C         NAME AND A STRING COMPARISON IS NOT PERFORMED).
C
          IF(ITYPE.EQ.'STRING')THEN
            NTEXT1=0
            ITEXT1=' '
            NSTRT=IVSTAR(ILOC)
            NSTOP=IVSTOP(ILOC)
            DO140J=NSTRT,NSTOP
              NTEXT1=NTEXT1+1
              ITEXT1(NTEXT1:NTEXT1)=IFUNC(J)(1:1)
  140       CONTINUE
          ELSE
            ISTRIN=' '
            ISTRI2=' '
            DO142II=1,IWIDTT
              ISTRIN(II:II)=IANSLC(II)(1:1)
  142       CONTINUE
            ISTART=1
            ISTOP=IWIDTT
            IWORD=2
            IFILQZ=IFILQU
            IFILQU='ON'
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRI2,NCSTR,
     1                  IBUGQ,ISUBRO,IERROR)
            IFILQU=IFILQZ
            IF(ISTRI2(1:1).EQ.'"' .AND. ISTRI2(NCSTR:NCSTR).EQ.'"')THEN
              ITEXT1(1:NCSTR-2)=ISTRI2(2:NCSTR-1)
              NTEXT1=NCSTR-2
            ELSE
              GOTO199
            ENDIF
          ENDIF
C
          ISTEPN='2B1'
          IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(IHARG(IPOSEQ).EQ.'=')THEN
            IFLAG=0
          ELSEIF(IHARG(IPOSEQ).EQ.'<>')THEN
            IFLAG=1
          ELSEIF(IHARG(IPOSEQ).EQ.'<'.AND.IHARG(IPOSEQ+1).EQ.'=')THEN
            IFLAG=3
          ELSEIF(IHARG(IPOSEQ).EQ.'<')THEN
            IFLAG=2
          ELSEIF(IHARG(IPOSEQ).EQ.'>'.AND.IHARG(IPOSEQ+1).EQ.'=')THEN
            IFLAG=5
          ELSEIF(IHARG(IPOSEQ).EQ.'>')THEN
            IFLAG=4
          ELSE
            IERROR='YES'
            GOTO9000
          ENDIF
          IERROR='NO'
C
CCCCC SEARCH FOR STRING AFTER THE "=" OR "<>".
C
CCCCC 2014/11: CHECK IF PRE-DEFINED STRING GIVEN FIRST
C
          IF(IFLAG.EQ.3 .OR. IFLAG.EQ.5)THEN
            IH=IHARG(IPOSEQ+2)
            IH2=IHARG2(IPOSEQ+2)
          ELSE
            IH=IHARG(IPOSEQ+1)
            IH2=IHARG2(IPOSEQ+1)
          ENDIF
          MESSAG='NO'
          CALL CHECKF(IH,IH2,IHWUSE,
     1    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
C
          IF(ITYPE.EQ.'STRING')THEN
            NTEXT2=0
            ITEXT2=' '
            NSTRT=IVSTAR(ILOC)
            NSTOP=IVSTOP(ILOC)
            DO143J=NSTRT,NSTOP
              NTEXT2=NTEXT2+1
              ITEXT2(NTEXT2:NTEXT2)=IFUNC(J)(1:1)
  143       CONTINUE
          ELSE
            IF(IFLAG.EQ.0 .OR.IFLAG.EQ.2 .OR. IFLAG.EQ.4)THEN
C
              ISTEPN='2B2'
              IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              DO110I=1,IWIDTT
                IF(IANSLC(I).EQ.'=')THEN
                  ISTRT=I+1
                  GOTO119
                ENDIF
  110         CONTINUE
              IERROR='YES'
              GOTO9000
  119         CONTINUE
            ELSEIF(IFLAG.EQ.1 .OR. IFLAG.EQ.3 .OR. IFLAG.EQ.5)THEN
C
              ISTEPN='2B2'
              IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              DO120I=1,IWIDTT
                IF(IANSLC(I).EQ.'<' .AND. IANSLC(I+1).EQ.'>')THEN
                  ISTRT=I+2
                  GOTO129
                ELSEIF(IANSLC(I).EQ.'<' .AND. IANSLC(I+1).EQ.'=')THEN
                  ISTRT=I+2
                  GOTO129
                ELSEIF(IANSLC(I).EQ.'>' .AND. IANSLC(I+1).EQ.'=')THEN
                  ISTRT=I+2
                  GOTO129
                ENDIF
  120         CONTINUE
              IERROR='YES'
              GOTO9000
  129         CONTINUE
            ENDIF
C
            ISTEPN='2B3'
            IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
              CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
              WRITE(ICOUT,156)ISTRT
  156         FORMAT('ISTRT = ',I5)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            NTEXT2=0
            ITEXT2=' '
            DO130I=ISTRT,IWIDTT
              IF(IANSLC(I).EQ.' ')THEN
                GOTO130
              ELSEIF(IANSLC(I).EQ.'"')THEN
                NSTRT=I+1
                ICOUNT=0
                DO132J=NSTRT,IWIDTT
                  IF(IANSLC(J).EQ.'"')THEN
                    NLAST=J-1
                    GOTO134
                  ELSE
                    ICOUNT=ICOUNT+1
                    ITEXT2(ICOUNT:ICOUNT)=IANSLC(J)(1:1)
                  ENDIF
  132           CONTINUE
                NLAST=IWIDTH
  134           CONTINUE
                NTEXT2=NLAST-NSTRT+1
                GOTO139
              ELSE
                NSTRT=I
                ICOUNT=0
                DO137J=NSTRT,IWIDTT
                  IF(IANSLC(J).EQ.' ')THEN
                    NLAST=J-1
                    GOTO138
                  ELSE
                    ICOUNT=ICOUNT+1
                    ITEXT2(ICOUNT:ICOUNT)=IANSLC(J)(1:1)
                  ENDIF
  137           CONTINUE
                NLAST=IWIDTT
  138           CONTINUE
                NTEXT2=NLAST-NSTRT+1
                GOTO139
              ENDIF
  130       CONTINUE
  139       CONTINUE
          ENDIF
C
CCCCC     NOW COMPARE THE TWO STRINGS (IMTCH=0 FOR MATCH,
CCCCC     1 FOR NO MATCH).
CCCCC
CCCCC     2018/05: MAKE SINGLE BLANK CHARACTER MATCH TRUE TO
CCCCC              EMPTY STRING.
CCCCC     2018/09: "<", "<=", ">", ">=" CASES HANDLED SEPARATELY.
C
          IF(IFLAG.EQ.2)THEN
C
C           LESS THAN CASE
C
            DO1141II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              ICNT=II
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.GT.IVAL2)THEN
                ICASIF='FALS'
                GOTO1651
              ENDIF
 1141       CONTINUE
            IF(IVAL1.EQ.IVAL2)ICASIF='FALS'
            GOTO1651
          ELSEIF(IFLAG.EQ.3)THEN
C
C           LESS THAN OR EQUAL TO CASE
C
            DO1142II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.GT.IVAL2)THEN
                ICASIF='FALS'
                GOTO1651
              ENDIF
 1142       CONTINUE
            GOTO1651
          ELSEIF(IFLAG.EQ.4)THEN
C
C           GREATER THAN CASE
C
            DO1143II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.LT.IVAL2)THEN
                ICASIF='FALS'
                GOTO1651
              ENDIF
 1143       CONTINUE
            IF(IVAL1.EQ.IVAL2)ICASIF='FALS'
            GOTO1651
          ELSEIF(IFLAG.EQ.5)THEN
C
C           GREATER THAN OR EQUAL TO CASE
C
            DO1144II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.LT.IVAL2)THEN
                ICASIF='FALS'
                GOTO1651
              ENDIF
 1144       CONTINUE
            GOTO1651
          ENDIF
C
          IMTCH=0
          IF(NTEXT1.EQ.1 .AND. NTEXT2.EQ.0 .AND.
     1      ITEXT1(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.1 .AND. NTEXT2.EQ.1 .AND.
     1      ITEXT1(1:1).EQ.' ' .AND. ITEXT2(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.0 .AND. NTEXT2.EQ.0)THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.0 .AND. NTEXT2.EQ.1 .AND.
     1      ITEXT2(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.NTEXT2)THEN
            DO150J=1,NTEXT1
              IF(ITEXT1(J:J).NE.ITEXT2(J:J))THEN
                IMTCH=1
                GOTO159
              ENDIF
  150       CONTINUE
  159       CONTINUE
          ELSE
            IMTCH=1
          ENDIF
C
CCCCC SET IF STATUS
C
          IF(IFLAG.EQ.0)THEN
            IF(IMTCH.EQ.0)THEN
              ICASIF='TRUE'
            ELSE
              ICASIF='FALS'
            ENDIF
          ELSE
            IF(IMTCH.EQ.0)THEN
              ICASIF='FALS'
            ELSE
              ICASIF='TRUE'
            ENDIF
          ENDIF
C
 1651     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1601)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1652)ITEXT1(1:NTEXT1)
 1652       FORMAT('         LEFT HAND STRING  = ',A)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1654)ITEXT2(1:NTEXT2)
 1654       FORMAT('         RIGHT HAND STRING = ',A)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1655)ICASIF
 1655       FORMAT('         IF    STATUS      = ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GOTO9000
        ENDIF
      ENDIF
C
  199 CONTINUE
C
C               ********************************************************
C               **  STEP 2.1-- C                                      **
C               **  INITIALIZE ALL ELEMENTS IN ISUB(.) TO 11 ISUB(.)  **
C               **  ISUB(.) WILL TAKE ON 4 VALUES AT MOST--00, 01,    **
C               **  10, 11 .  THE FIRST  DIGIT INDICATES WHETHER OR   **
C               **  NOT THE GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE **
C               **  LOCAL  CUMULATIVE UNION SET.  THE SECOND DIGIT    **
C               **  INDICATES WHETHER OR NOT THE GIVEN ELEMENT IS OUT **
C               **  (0) OR IN (1) OF THE GLOBAL CUMULATIVE            **
C               **  INTERSECTIONS THE INITIALIZATION OF ALL ELEMENTS  **
C               **  TO 11 THUS INDICATES THAT INITIALLY ALL ELEMENTS  **
C               **  (TEMPORARILY) ARE IN THE LOCAL UNION SET, AND     **
C               **  INITIALLY ALL ELEMENTS ARE IN THE GLOBAL          **
C               **  INTERSECTION SET.                                 **
C               ********************************************************
C
      ISTEPN='2.1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NIOLD=1
      DO200I=1,NIOLD
        ISUB(I)=11
  200 CONTINUE
C
C               *************************************************
C               **  STEP 2.2--                                 **
C               **  IF EXISTENT,                               **
C               **  PACK < = INTO <=                           **
C               **  PACK = < INTO =<                           **
C               **  PACK > = INTO >=                           **
C               **  PACK = > INTO =>                           **
C               **  THIS IS BECAUSE = SIGNS ARE AUTOMATICALLY  **
C               **  GIVEN A SPACE IN DPTYPE AND TREATED AS     **
C               **  AS A SEPARATE WORD.                        **
C               **  NOTE THAT NUMARG WILL BE CHANGED.          **
C               *************************************************
C
      ISTEPN='2.2'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL ADJUS2(IHARG,IHARG2,IARG,ARG,IARGT,NUMART)
C
C               ************************************************
C               **  STEP 3.1--                                **
C               **  CHECK TO SEE IF HAVE THE  IF      CASE.   **
C               **  LOCATE THE POSITION IN THE ARGUMENT LIST  **
C               **  OF THE WORD    IF   .                     **
C               ************************************************
C
      ISTEPN='3.1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=0
      ICASSC='SEAR'
      ICASQU='UNKN'
      NUMSV=0
      DO300IPASS=1,100
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)
  301     FORMAT('***** AT THE BEGINNING OF ANOTHER PASS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,302)IPASS,JMAX
  302     FORMAT('IPASS,JMAX = ',2I8)
          CALL DPWRST('XXX','BUG ')
          IF(ILOCTG.GE.1 .AND. ILOCTG.LE.100)THEN
            WRITE(ICOUT,303)ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG)
  303       FORMAT('ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG) = ',
     1             A4,I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,304)ICASSC,ILOCTG
  304       FORMAT('ICASSC,ILOCTG = ',A4,2X,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
C
        IF(ICASSC.EQ.'STOP')GOTO1100
        JMIN=JMAX+1
        IF(JMIN.GT.NUMART)GOTO1100
        IF(JMIN.EQ.NUMART.AND.IHARG(JMIN).EQ.'AND '.AND.
     1     IHARG2(JMIN).EQ.'    ')GOTO1100
C
        IF(ICASSC.EQ.'CONT')GOTO600
        DO310I=1,NIOLD
          ITEMP=ISUB(I)
          IF(ITEMP.EQ.00)ISUB(I)=00
          IF(ITEMP.EQ.10)ISUB(I)=00
          IF(ITEMP.EQ.01)ISUB(I)=00
          IF(ITEMP.EQ.11)ISUB(I)=11
  310   CONTINUE
C
        ICASQU='UNKN'
        DO340J=JMIN,NUMART
          J2=J
          IF(IHARG(J).EQ.'IF  '.AND.IHARG2(J).EQ.'    ')THEN
            ICASQU='IF  '
            ILOCS=J2
            GOTO390
          ENDIF
  340   CONTINUE
        IF(JMIN.EQ.1.AND.
     1     ICOM.EQ.'IF  '.AND.ICOM2.EQ.'    ')THEN
          J2=0
          ICASQU='IF  '
          ILOCS=J2
          GOTO390
        ENDIF
        ILOCS=NUMART+1
        GOTO1100
C
  390   CONTINUE
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,391)IPASS,ICASQU,ILOCS
  391     FORMAT('IPASS,ICASQU,ILOCS = ',I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               *******************************************
C               **  STEP 3.2--                           **
C               **  IF HAVE THE IF     CASE,             **
C               **  INITIALIZE ISUB(.) TO 0X--00 OR 01.  **
C               *******************************************
C
        ISTEPN='3.2'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(ICASQU.EQ.'IF  ')THEN
          DO401I=1,NIOLD
            ITEMP=ISUB(I)
            IF(ITEMP.EQ.00)ISUB(I)=00
            IF(ITEMP.EQ.10)ISUB(I)=00
            IF(ITEMP.EQ.01)ISUB(I)=01
            IF(ITEMP.EQ.11)ISUB(I)=01
  401     CONTINUE
        ELSE
          IERROR='YES'
          GOTO9000
        ENDIF
C
C               ****************************************************
C               **  STEP 4--                                      **
C               **  CHECK VALIDITY OF FIRST ARGUMENT AFTER     IF **
C               **  THIS SHOULD BE THE IF PARAMETER               **
C               ****************************************************
C
C       2018/05: ALLOW SYNTAX LIKE
C
C                   IF 3 > 2
C                   IF 3 > B
C
C                THAT IS, THE ARGUMENT ON THE LEFT HAND SIDE CAN BE
C                A NUMERIC VALUE AS WELL AS A PARAMETER.
C
        ISTEPN='4'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICASPA='UNKN'
        ILOCS1=ILOCS+1
        JMAX=ILOCS1
        IF(ILOCS1.GT.NUMART)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,411)
  411     FORMAT('***** ERROR IN IF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,412)
  412     FORMAT('      THE WORD    IF    WAS THE FINAL WORD ON THE')
          CALL DPWRST('XXX','BUG ')
          GOTO8000
        ENDIF
C
        IHSET=IHARG(ILOCS1)
        IHSET2=IHARG2(ILOCS1)
C
C       2015/02: IF PARAMETER NOT FOUND, SET IF STATUS TO FALSE
C
  440   CONTINUE
        ICASPA='P   '
        IHWUSE='P'
CCCCC   2018/02: IF PARAMETER DOES NOT EXIST, SIMPLY SET CLAUSE TO
CCCCC            FALSE, BUT DO NOT GENERATE ERROR MESSAGE.
CCCCC   MESSAG='YES'
        MESSAG='NO'
        CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')THEN
          IF(IARGT(ILOCS1).EQ.'NUMB')THEN
            ASETV=ARG(ILOCS1)
          ELSE
            ICASIF='FALS'
            IERROR='FALS'
            GOTO9000
          ENDIF
        ELSE
          ASETV=VALUE(ILOC)
        ENDIF
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='4A'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,451)ILOCS1,NUMART,IPASS,IHSET,IHSET2,ICASPA,ASETV
  451     FORMAT('ILOCS1,NUMARGT,IPASS,IHSET,IHSET2,ICASPA,ASETV = ',
     1           3I8,3X,2A4,2X,A4,3X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  CHECK TO SEE IF NEXT ARGUMENT IS                  **
C               **        <                                           **
C               **        <=                                          **
C               **        =                                           **
C               **        >=                                          **
C               **        >                                           **
C               **        <>                                          **
C               **  IF NONE OF THE ABOVE, THEN THE ASSUMED OPERATION  **
C               **  IS =   .                                          **
C               ********************************************************
C
        ISTEPN='5'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICASOP='UNKN'
        ILOCS2=ILOCS+2
        JMAX=ILOCS2
        IF(ILOCS2.GT.NUMART)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,411)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,502)
  502     FORMAT('      THE   IF   PARAMETER NAME WAS THE FINAL WORD ',
     1           'ON')
          CALL DPWRST('XXX','BUG ')
          GOTO8000
        ENDIF
C
        IHSET=IHARG(ILOCS2)
        IHSET2=IHARG2(ILOCS2)
C
        IF(IHSET.EQ.'<   ')THEN
          ICASOP='<   '
          ILOCTG=ILOCS2
        ELSEIF(IHSET.EQ.'<=  ' .OR. IHSET.EQ.'=<  ')THEN
          ICASOP='<=  '
          ILOCTG=ILOCS2
        ELSEIF(IHSET.EQ.'=   ')THEN
          ICASOP='=   '
          ILOCTG=ILOCS2
        ELSEIF(IHSET.EQ.'>=  ' .OR. IHSET.EQ.'=>  ')THEN
          ICASOP='>=  '
          ILOCTG=ILOCS2
        ELSEIF(IHSET.EQ.'>   ')THEN
          ICASOP='>   '
          ILOCTG=ILOCS2
        ELSEIF(IHSET.EQ.'<>  ' .OR. IHSET.EQ.'><  ' .OR.
     1         IHSET.EQ.'!=  ')THEN
          ICASOP='<>  '
          ILOCTG=ILOCS2
        ELSE
          ICASOP='=ASS'
          ILOCTG=ILOCS2-1
        ENDIF
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='5A'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,591)IPASS,ILOCTG,IHSET,IHSET2,ICASPA,ICASOP
  591     FORMAT('IPASS,ILOCTG,IHSET,IHSET2,ICASPA,ICASOP = ',
     1           2I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               **********************************************************
C               **  STEP 6--                                            **
C               **  DETERMINE THE LOWER LIMIT OF THE INTERVAL OF INTEREST.
C               **  THIS IS DONE BY CHECKING THE FIRST (NEXT) ARGUMENT  **
C               **  IN THE LIST.                                        **
C               **  ALSO, FOR THOSE 4 CASES IN WHICH                    **
C               **  ICASOP IS   <   <=   >=   >                         **
C               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF INTEREST.
C               **********************************************************
C
  600   CONTINUE
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='6'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,601)
  601     FORMAT('     AT THE BEGINNING OF STEP 6 IN DPIF--')
          CALL DPWRST('XXX','BUG ')
          DO605I=1,NIOLD
            WRITE(ICOUT,606)I,ISUB(I)
  606       FORMAT('I,ISUB(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
  605     CONTINUE
       ENDIF
C
        ILOCTG=ILOCTG+1
        JMAX=ILOCTG
        IF(ILOCTG.GT.NUMART)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,611)
  611     FORMAT('***** ERROR IN DPIF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,612)
  612     FORMAT('      THE    IF    OPERATION   <   <=  =  >=  > ',
     1           'WAS THE FINAL WORD ON')
          CALL DPWRST('XXX','BUG ')
          GOTO8000
        ENDIF
C
        IF(IARGT(ILOCTG).EQ.'NUMB')THEN
          DMIN=ARG(ILOCTG)
          DMAX=ARG(ILOCTG)
          IF(ICASOP.EQ.'<   ')THEN
            DMIN=CPUMIN
            DMAX=ARG(ILOCTG)
          ELSEIF(ICASOP.EQ.'<=  ')THEN
            DMIN=CPUMIN
            DMAX=ARG(ILOCTG)
          ELSEIF(ICASOP.EQ.'>=  ')THEN
            DMIN=ARG(ILOCTG)
            DMAX=CPUMAX
          ELSEIF(ICASOP.EQ.'>   ')THEN
            DMIN=ARG(ILOCTG)
            DMAX=CPUMAX
          ENDIF
        ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
          IH=IHARG(ILOCTG)
          IH2=IHARG2(ILOCTG)
          IHWUSE='P'
CCCCC     MESSAG='YES'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
C
          IF(IERROR.EQ.'YES')THEN
            ICASIF='FALS'
            IERROR='FALS'
            GOTO9000
          ENDIF
C
          DMIN=VALUE(ILOC)
          DMAX=VALUE(ILOC)
          IF(ICASOP.EQ.'<   ')THEN
            DMIN=CPUMIN
            DMAX=VALUE(ILOC)
          ELSEIF(ICASOP.EQ.'<=  ')THEN
            DMIN=CPUMIN
            DMAX=VALUE(ILOC)
          ELSEIF(ICASOP.EQ.'>=  ')THEN
            DMIN=VALUE(ILOC)
            DMAX=CPUMAX
          ELSEIF(ICASOP.EQ.'>   ')THEN
            DMIN=VALUE(ILOC)
            DMAX=CPUMAX
          ENDIF
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,631)
  631     FORMAT('***** INTERNAL ERROR IN DPIF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,632)
  632     FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,633)
  633     FORMAT('      EITHER A NUMBER OR A WORD, IS NEITHER.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,634)IHARG(ILOCTG),IHARG2(ILOCTG)
  634     FORMAT('      ARGUMENT                  = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,635)ILOCTG
  635     FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,636)IARGT(ILOCTG)
  636     FORMAT('      ARGUMENT TYPE             = ',A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,637)
  637     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,638)(IANS(I),I=1,MIN(100,IWIDTH))
  638       FORMAT('      ',100A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          ICASIF='FALS'
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,691)IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX
  691     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX = ',
     1           I8,4(2X,A4,2X,A4,2X,A4,2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               **********************************************************
C               **  STEP 7--                                            **
C               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF INTEREST.
C               **  NOTE THAT FOR THOSE 4 CASES IN WHICH ICASOP IS      **
C               **  <   <=   >=   >    THE UPPER LIMIT OF THE INTERVAL  **
C               **  HAS ALREADY BEEN DETERMINED AND SO ALL OF THE CODE  **
C               **  OF THIS SECTION MAY BE  SKIPPED.  ON THE OTHER HAND **
C               **  WHEN THE OPERATION IS    =  , (EXPLICITLY OR        **
C               **  ASSUMED),  OR <>    ,  THE UPPER LIMIT MUST BE      **
C               **  DETERMINED.  THIS IS DONE BY CHECKING THE NEXT      **
C               **  ARGUMENT IN THE LIST.  IF THIS NEXT ARGUMENT IS TO, **
C               **  THIS IMPLIES THAT AN UPPER LIMIT WILL BE PROVIDED   **
C               **  (IN THE ARGUMENT AFTER THE   TO   ).  HOWEVER, IF   **
C               **  THE NEXT ARGUMENT IS NOT A    TO   , THEN THIS      **
C               **  IMPLIES THAT THE LIST CONSISTS OF INDIVIDUAL        **
C               **  ELEMENTS OF THE SUBSET AND SO THE UPPER LIMIT WILL  **
C               **  BE IDENTICAL TO THE LOWER LIMIT.                    **
C               **********************************************************
C
  700   CONTINUE
C
        ISTEPN='7'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(ICASOP.EQ.'<   ' .OR. ICASOP.EQ.'<=  ' .OR.
     1     ICASOP.EQ.'>=  ' .OR. ICASOP.EQ.'>   ')THEN
          ICASSC='SEAR'
          GOTO790
        ENDIF
C
        ILOCTG=ILOCTG+1
C
        IF(ILOCTG.GT.NUMART .OR.
     1     (ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1     IHARG2(ILOCTG).EQ.'    '))THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='STOP'
          DMAX=DMIN
          GOTO790
        ELSEIF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.
     1         IHARG2(ILOCTG).EQ.'    ')THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='SEAR'
          DMAX=DMIN
          GOTO790
        ELSEIF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'TO  '.AND.
     1         IHARG2(ILOCTG).EQ.'    ')THEN
          ILOCTG=ILOCTG+1
          JMAX=ILOCTG
          IF(ILOCTG.GT.NUMART)GOTO760
          IF(ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1       IHARG2(ILOCTG).EQ.'    ')GOTO760
          IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.
     1       IHARG2(ILOCTG).EQ.'    ')GOTO760
          IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'TO  '.AND.
     1       IHARG2(ILOCTG).EQ.'    ')GOTO760
          GOTO770
C
        ELSE
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='CONT'
          DMAX=DMIN
          GOTO790
        ENDIF
C
  760   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,762)
  762   FORMAT('      THE WORD    TO    SHOULD HAVE BEEN FOLLOWED BY A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,764)
  764   FORMAT('      NUMBER OR A PARAMETER NAME, BUT WAS NOT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,765)IHARG(ILOCTG),IHARG2(ILOCTG)
  765   FORMAT('      TO    WAS FOLLOWED BY THE WORD   ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,766)
  766   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,638)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
C
  770   CONTINUE
        IF(IARGT(ILOCTG).EQ.'NUMB')THEN
          DMAX=ARG(ILOCTG)
        ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
          IH=IHARG(ILOCTG)
          IH2=IHARG2(ILOCTG)
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')THEN
            ICASIF='FALS'
            GOTO9000
          ENDIF
          DMAX=VALUE(ILOC)
        ELSE
          IBRAN=770
          WRITE(ICOUT,631)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,771)IBRAN
  771     FORMAT('      IMPOSSIBLE BRANCH CONDITION AT BRANCH POINT = ',
     1           I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,772)ILOCTG,IARGT(ILOCTG)
  772     FORMAT('ILOCTG, IARGT(ILOCTG) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        ILOCTG=ILOCTG+1
        ICASSC='CONT'
        IF(ILOCTG.GT.NUMART)ICASSC='STOP'
        IF(ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1     IHARG2(ILOCTG).EQ.'    ')ICASSC='STOP'
        IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.
     1     IHARG2(ILOCTG).EQ.'    ')ICASSC='SEAR'
        ILOCTG=ILOCTG-1
        JMAX=ILOCTG
C
  790   CONTINUE
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,791)IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX
  791     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX = ',
     1           I8,4(2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ***************************************************
C               **  STEP 8--                                     **
C               **  TO ALLOW FOR ROUNDOFF ERRORS IN THE          **
C               **  STORAGE OF NUMBERS,                          **
C               **  JUDICIOUSLY EXPAND THE INTERVAL OF INTEREST  **
C               **  BY AN    EPSILON    AMOUNT.                  **
C               ***************************************************
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='8'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,801)
  801     FORMAT('      AT THE BEGINNING OF STEP 8--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,802)DMIN,DMAX
  802     FORMAT('DMIN,DMAX = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(DMIN.GT.DMAX)THEN
          HOLD=DMIN
          DMIN=DMAX
          DMAX=HOLD
        ENDIF
C
        IF(DMIN.EQ.CPUMIN)GOTO819
        IF(DMIN.EQ.CPUMAX)GOTO819
        IF(ABS(DMIN).EQ.0.0)EPS=0.000001
        IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
        IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'<>  ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
  819   CONTINUE
C
        IF(DMAX.EQ.CPUMAX)GOTO829
        IF(DMAX.EQ.CPUMIN)GOTO829
        IF(ABS(DMAX).EQ.0.0)EPS=0.000001
        IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
        IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'<>  ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
        IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
  829   CONTINUE
C
  890   CONTINUE
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,891)IPASS,ICASPA,ICASOP,IH,IH2
  891     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2 = ',I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,892)EPS,DMIN,DMAX,CPUMIN,CPUMAX
  892     FORMAT('EPS,DMIN,DMAX,CPUMIN,CPUMAX = ',5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ****************************************************
C               **  STEP 9--                                      **
C               **  DEFINE THE ISUB(.) VECTOR--                   **
C               **  FOR ANY K (K = 1 TO NIOLD),                   **
C               **  IF THE K-TH ELEMENT OF THE                    **
C               **  SUBSET SPECIFICATION VARIABLE                 **
C               **  (THE VARIABLE SPECIFIED AFTER    SUBSET       **
C               **  IN THE COMMAND LINE)                          **
C               **  IS WITHIN THE SPECIFIED (DMIN,DMAX) LIMITS,   **
C               **  THEN ISUB(K) SHOULD RESULT IN A VALUE OF 1;   **
C               **  BUT IF THE K-TH ELEMENT OF THE                **
C               **  SUBSET SPECIFICATION VARIABLE                 **
C               **  IS OUTSIDE THE SPECIFIED (DMIN,DMAX) LIMITS,  **
C               **  THEN ISUB(K) SHOULD RESULT IN A 0 .           **
C               ****************************************************
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='9'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,901)ILOCS1,IHSET,IHSET2,ICASPA,ASETV,MAXCOL
  901     FORMAT('ILOCS1,IHSET,IHSET2,ICASPA,ASETV,MAXCOL = ',
     1           I8,3(2X,A4),G15.7,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ICASPA.NE.'P   ')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,631)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,912)
  912     FORMAT('      IMPROPER VALUE FOR ICASPA AND/OR ASETV')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,913)ICASPA,ASETV,MAXCOL,MAXCP1,MAXCP2
  913     FORMAT('      ICASPA,ASETV,MAXCOL,MAXCP1,MAXCP2 = ',A4,
     1           G15.7,3I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          ICASIF='FALS'
          GOTO9000
        ENDIF
C
        NS=0
        ND=0
        DO941I=1,NIOLD
          VIJ=ASETV
C
          IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
            WRITE(9,947)I,NIOLD,ASETV,DMIN,DMAX,VIJ
  947       FORMAT('I,NIOLD,ASETV,DMIN,DMAX,VIJ = ',2I8,G15.7,3F12.5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          TARGET=VIJ
          ISTATI='FALS'
C
          IF(ICASQU.EQ.'IF  '.AND.ICASOP.EQ.'<>  ')THEN
            IF(TARGET.LT.DMIN .OR. DMAX.LT.TARGET)THEN
              ISTATI='TRUE'
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=10
              IF(ITEMP.EQ.10)ISUB(I)=10
              IF(ITEMP.EQ.01)ISUB(I)=11
              IF(ITEMP.EQ.11)ISUB(I)=11
              NS=NS+1
            ELSEIF(DMIN.LE.TARGET.AND.TARGET.LE.DMAX)THEN
              ND=ND+1
            ENDIF
          ELSEIF(ICASQU.EQ.'IF  ')THEN
            IF(DMIN.LE.TARGET.AND.TARGET.LE.DMAX)THEN
              ISTATI='TRUE'
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=10
              IF(ITEMP.EQ.10)ISUB(I)=10
              IF(ITEMP.EQ.01)ISUB(I)=11
              IF(ITEMP.EQ.11)ISUB(I)=11
              NS=NS+1
            ELSE
              ND=ND+1
            ENDIF
          ENDIF
C
  941   CONTINUE
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,991)IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND
  991     FORMAT('IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND = ',
     1           I8,2X,A4,3G15.7,3I8)
          CALL DPWRST('XXX','BUG ')
          DO992I=1,NIOLD
            WRITE(ICOUT,993)I,ISUB(I)
  993       FORMAT('I,ISUB(I) = ',I8,I8)
            CALL DPWRST('XXX','BUG ')
  992     CONTINUE
          WRITE(ICOUT,995)ITEMP,ISTATI
  995     FORMAT('ITEMP,ISTATI = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               *************************************************
C               **  STEP 10--                                  **
C               **  WRITE OUT A MESSAGE FOR THIS STEP          **
C               **  INDICATING                                 **
C               **  THE SUBSET PARAMETER NAME,                 **
C               **  THE SUBSET MINIMUM,                        **
C               **  THE SUBSET MAXIMUM,                        **
C               **  THE SUBSET PARAMETER VALUE,                **
C               **  THE SUBSET PARAMETER STATUS,               **
C               *************************************************
C
        ISTEPN='10'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
 1010   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1011)
 1011     FORMAT('***** NOTE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1012)IHARG(ILOCS1),IHARG2(ILOCS1)
 1012     FORMAT('      IF     PARAMETER = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1013)DMIN
 1013     FORMAT('      IF     MINIMUM   = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1014)DMAX
 1014     FORMAT('      IF     MAXIMUM   = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1015)ASETV
 1015     FORMAT('      IF     PARAMETER VALUE    = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IF(INOT.EQ.'ON')THEN
            IF(ISTATI.EQ.'TRUE')THEN
              WRITE(ICOUT,1017)
 1017         FORMAT('      IF     PARAMETER STATUS   = FALS')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,1018)
 1018         FORMAT('      IF     PARAMETER STATUS   = TRUE')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
            WRITE(ICOUT,1016)ISTATI
 1016       FORMAT('      IF     PARAMETER STATUS   = ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
C
        NUMSV=IPASS
C
  300 CONTINUE
C
 1100 CONTINUE
      DO1110I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=00
        IF(ITEMP.EQ.10)ISUB(I)=00
        IF(ITEMP.EQ.01)ISUB(I)=00
        IF(ITEMP.EQ.11)ISUB(I)=11
 1110 CONTINUE
C
C               *************************************
C               **  STEP 11--                      **
C               **  PUT ISUB(.) IN FINAL 0,1 FORM  **
C               *************************************
C
      ISTEPN='11'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1210I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=0
        IF(ITEMP.EQ.10)ISUB(I)=0
        IF(ITEMP.EQ.01)ISUB(I)=1
        IF(ITEMP.EQ.11)ISUB(I)=1
 1210 CONTINUE
C
C               *****************************************
C               **  STEP 12--                          **
C               **  IF THERE WERE 2 OR MORE SUBSET     **
C               **  VARIABLES, GATHER INFORMATION      **
C               **  FOR A FINAL SUMMARY MESSAGE BY     **
C               **  DETERMINING THE FINAL NUMBER OF    **
C               **  ELEMENTS IN THE SUBSET             **
C               **  (AFTER ALL VARIABLES HAVE          **
C               **  BEEN INDIVIDUALLY ACCOUNTED FOR).  **
C               *****************************************
C
 1500 CONTINUE
C
      ISTEPN='12'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSV.GT.1)THEN
        NS=0
        DO1510I=1,NIOLD
          IF(ISUB(I).EQ.1)NS=NS+1
 1510   CONTINUE
      ENDIF
C
C               *************************************************
C               **  STEP 13--                                  **
C               **  IF THERE WERE 2 OR MORE SUBSET VARIABLES,  **
C               **  WRITE OUT A FINAL MESSAGE                  **
C               **  SUMMARIZING FOR ALL VARIABLES              **
C               **  THE NUMBER OF SUBSET VARIABLES             **
C               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
C               **  THE NUMBER OF OBSERVATIONS IGNORED         **
C               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
C               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
C               **  ALSO, CHECK THAT NS IS POSITIVE.           **
C               *************************************************
C
      ISTEPN='13'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASIF='FALS'
      IF(ISUB(1).EQ.1)ICASIF='TRUE'
C
      IF(NUMSV.GT.1 .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1601)
 1601   FORMAT('*****    IF    SUMMARY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1602)NUMSV
 1602   FORMAT('      NUMBER OF SPECIFICATIONS       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1605)ICASIF
 1605   FORMAT('      FINAL    IF    STATUS          = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      GOTO9000
C
 8000 CONTINUE
      WRITE(ICOUT,414)
  414 FORMAT('      COMMAND LINE.  THE WORD    IF    SHOULD HAVE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,415)
  415 FORMAT('      BEEN FOLLOWED BY OTHER ARGUMENTS, AS IN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,416)
  416 FORMAT('           IF A = 4')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,417)
  417 FORMAT('           IF A > 6')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,418)
  418 FORMAT('           IF X >= B')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,419)
  419 FORMAT('           AND SO FORTH.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,421)
  421 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,422)(IANSLC(I),I=1,MIN(100,IWIDTH))
  422   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
C     CHECK FOR "NOT" STATUS
C
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,9001)JPASS,NPASS,NUMART,ICNT3,IWIDTT,ICASIF,IERROR
 9001   FORMAT('AT 9000: JPASS,NPASS,NUMART,ICNT3,IWIDTT,ICASIF,',
     1         'IERROR = ',5I5,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)IOPER(1),IOPER(2),IOPER(3),IOPER(4)
 9002   FORMAT('IOPER(1),IOPER(2),IOPER(3),IOPER(4) = ',
     1         3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(INOT.EQ.'ON')THEN
        IF(ICASIF.EQ.'TRUE')THEN
          ICASIF='FALS'
        ELSEIF(ICASIF.EQ.'FALS')THEN
          ICASIF='TRUE'
        ENDIF
      ENDIF
C
C     CHECKS FOR CASE WHEN AND/OR/XOR
C
      IF(IERROR.EQ.'YES' .OR. NPASS.LE.1)GOTO9009
      IF(JPASS.EQ.0)THEN
        ICASI1=ICASIF
        ISHIFT=NUMART+1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGQ,IERROR)
        ICNT=0
        DO9005I=ILOCP2,IWIDTH
          ICNT=ICNT+1
          IANS(ICNT)=IANS(I)
          IANSLC(ICNT)=IANSLC(I)
 9005   CONTINUE
        IWIDTT=ICNT
        JPASS=1
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,9006)(IANS(JJ)(1:1),JJ=1,MIN(80,IWIDTT))
 9006     FORMAT('IANS: ',80A1)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9007)ICASI1,IWIDTT,NUMARG
 9007     FORMAT('JPASS = 0 CASE: ICASI1,IWIDTT,NUMARG = ',
     1           A4,2X,2I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        NUMART=NUMARG
C
        GOTO1000
      ELSEIF(JPASS.EQ.1)THEN
        ICASI2=ICASIF
        ICNT3=ICNT3+1
        IF(IOPER(ICNT3).EQ.'AND')THEN
          ICASIF='FALS'
          IF(ICASI1.EQ.'TRUE' .AND. ICASI2.EQ.'TRUE')ICASIF='TRUE'
        ELSEIF(IOPER(ICNT3).EQ.'OR')THEN
          ICASIF='FALS'
          IF(ICASI1.EQ.'TRUE' .OR. ICASI2.EQ.'TRUE')ICASIF='TRUE'
        ELSEIF(IOPER(ICNT3).EQ.'XOR')THEN
          ICASIF='FALS'
          IF(ICASI1.EQ.'TRUE' .AND. ICASI2.EQ.'FALS')ICASIF='TRUE'
          IF(ICASI1.EQ.'FALS' .AND. ICASI2.EQ.'TRUE')ICASIF='TRUE'
        ENDIF
C
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,9003)ICASI1,ICASI2,ICASIF
 9003     FORMAT('JPASS = 1 CASE: ICASI1,ICASI2,ICASIF = ',
     1           2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        ICASI1=ICASIF
        ISHIFT=NUMART+1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGQ,IERROR)
        ICNT=0
        DO9008I=ILOCP2,IWIDTH
          ICNT=ICNT+1
          IANS(ICNT)=IANS(I)
          IANSLC(ICNT)=IANSLC(I)
 9008   CONTINUE
        IWIDTT=ICNT
        GOTO1000
      ENDIF
C
C  IF ERROR, THEN SET IF STATUS TO FALSE.    FEBRUARY 1999
C
C  2012/10: ADD PROMPT IF ERROR DETECTED.
C
 9009 CONTINUE
      IF(IERROR.EQ.'YES')THEN
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1              ISUBN1,ISUBN2,ICASIF,
     1              IBUGQ,ISUBRO,IERROR)
      ENDIF
C
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NIOLD,ILOCS,NS,IBUGQ,IERROR
 9012   FORMAT('NIOLD,ILOCS,NS,IBUGQ,IERROR = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMARG,NUMNAM,MAXNAM,N,MAXN
 9015   FORMAT('NUMARG,NUMNAM,MAXNAM,N,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IWIDTH,ILOCS,ILOCS2,ILOCTG
 9016   FORMAT('IWIDTH,ILOCS,ILOCS2,ILOCTG = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)NUMSV,ND,ILOCP2,ICASI1,ICASI2
 9017   FORMAT('NUMSV,ND,ILOCP2,ICASI1,ICASI2 = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)ICASQU,ICASPA,ICASOP,ICASSC
 9018   FORMAT('ICASQU,ICASPA,ICASOP,ICASSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NIOLD
          WRITE(ICOUT,9021)I,ISUB(I)
 9021     FORMAT('I,ISUB(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,9023)ISTATI,ICASIF,JMIN,JMAX,NUMARG
 9023   FORMAT('ISTATI,ICASIF,JMIN,JMAX,NUMARG = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIMAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE AN IMAGE PLOT.  YOU CAN GENERATE EITHER
C              A GREY-SCALE IMAGE OR A FULL RGB COLOR IMAGE.
C              THE INPUT CAN BE EITHER VECTORS:
C                  RED    = VECTOR CONTAINING VALUES FOR "RED" COMPONENT
C                  BLUE   = VECTOR CONTAINING VALUES FOR "BLUE" COMPONENT
C                  GREEN  = VECToR CONTAINING VALUES FOR "GREEN" COMPONENT
C                  ROWID  = VECTOR CONTAINING THE ROW-ID
C                  COLID  = VECTOR CONTAINING THE COLUMN-ID
C
C                  GREY   = VECTOR CONTAINING VALUES FOR GREY SCALE
C                  ROWID  = VECTOR CONTAINING THE ROW-ID
C                  COLID  = VECTOR CONTAINING THE COLUMN-ID
C
C              OR MATRICES:
C                  RED    = MATRIX CONTAINING VALUES FOR "RED" COMPONENT
C                  BLUE   = MATRIX CONTAINING VALUES FOR "BLUE" COMPONENT
C                  GREEN  = MATRIX CONTAINING VALUES FOR "GREEN" COMPONENT
C
C                  GREY   = MATRIX CONTAINING VALUES FOR GREY SCALE
C
C     EXAMPLES--IMAGE PLOT GREY
C               IMAGE PLOT RED BLUE GREY
C               IMAGE PLOT GREY ROWID COLID
C               IMAGE PLOT RED BLUE GREEN ROWID COLID
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/3
C     ORIGINAL VERSION--MARCH     2008.
C     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE
      CHARACTER*4 ICASCO
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IUSE1
      CHARACTER*4 IUSE2
      CHARACTER*4 IUSE3
      CHARACTER*4 IUSE4
      CHARACTER*4 IUSE5
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 ICTAR1
      CHARACTER*4 ICTAR2
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOCP.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      INTEGER COLR
C
      CHARACTER*1 BOX
C
      DIMENSION YRED(MAXOBV)
      DIMENSION YBLUE(MAXOBV)
      DIMENSION YGREEN(MAXOBV)
      DIMENSION YALPHA(MAXOBV)
      DIMENSION ROWID(MAXOBV)
      DIMENSION COLID(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),YRED(1))
      EQUIVALENCE (GARBAG(IGARB2),YBLUE(1))
      EQUIVALENCE (GARBAG(IGARB3),YGREEN(1))
      EQUIVALENCE (GARBAG(IGARB4),YALPHA(1))
      EQUIVALENCE (GARBAG(IGARB5),ROWID(1))
      EQUIVALENCE (GARBAG(IGARB6),COLID(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
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
      ISUBN1='DPIM'
      ISUBN2='AG  '
C
      ICASCO='GREY'
      ICASE='VARI'
      ICASPL='IMAG'
C
      IFOUND='NO'
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIMAG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,MAXNPP
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,MAXNPP = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFOUND,IERROR
   57   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  TREAT THE IMAGE PLOT CASE    **
C               ***********************************
C
      IFOUND='YES'
      ICASPL='IMAG'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='IMAGE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=2
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=5
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C     IF VARIABLE ARGUMENTS GIVEN, THEN 3 TO 5 ARGUMENTS EXPECTED.
C     IF MATRIX ARGUMENTS GIVEN, THEN 1 TO 3 ARGUMENTS EXPECTED.
C
      IF(IVARTY(1).EQ.'VARI')THEN
        IF(NUMVAR.LT.3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,291)
  291     FORMAT('***** ERROR IN IMAGE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,292)
  292     FORMAT('      WHEN VARIABLE ARGUMENTS ARE GIVEN, AT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,293)
  293     FORMAT('      LEAST THREE ARGUMENTS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,294)NUMVAR
  294     FORMAT('      NUMBER OF ARGUMENTS FOUND = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
        IF(NUMVAR.GT.3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,291)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,296)
  296     FORMAT('      WHEN MATRIX ARGUMENTS ARE GIVEN, AT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,298)
  298     FORMAT('      MOST THREE ARGUMENTS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,294)NUMVAR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C     IF VARIABLE ARGUMENTS, PUT LAST TWO ARGUMENTS IN ROWID AND
C     COLUMN ID.  THE REST SHOULD GO IN YRED, YBLUE AND YGREEN.
C
      ICASCO='COLO'
      IF(IVARTY(1).EQ.'VARI')THEN
        IF(NUMVAR.LE.3)ICASCO='GREY'
        NUMVA2=NUMVAR-2 
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YRED,YBLUE,YGREEN,NS,NTEMP,NTEMP,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        NUMVA2=2 
        ICOL=NUMVAR-1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              ROWID,COLID,COLID,NS,NTEMP,NTEMP,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
C
        IF(NUMVAR.LE.1)ICASCO='GREY'
        DO301I=1,MAXOBV
          ROWID(I)=0.0
          COLID(I)=0.0
          YRED(I)=0.0
          YBLUE(I)=0.0
          YGREEN(I)=0.0
  301   CONTINUE
C
        ILISR=1
        ICOL31=IVALUE(ILISR)
        ICOL32=IVALU2(ILISR)
        NROW=IN(ILISR)
        NCOL=(ICOL32 - ICOL31) + 1
        ICNT=0
        DO310JCOL=1,NCOL
          DO320IROW=1,NROW
            ICNT=ICNT+1
            ROWID(ICNT)=REAL(IROW)
            COLID(ICNT)=REAL(ICOL)
  320     CONTINUE
  310   CONTINUE
C
        ICOL=1
        NUMVA2=1 
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YRED,YRED,YRED,NS,NTEMP,NTEMP,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NUMVAR.GE.2)THEN
          ICOL=2
          NUMVA2=1 
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                YBLUE,YBLUE,YBLUE,NS,NTEMP,NTEMP,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NUMVAR.GE.3)THEN
          ICOL=3
          NUMVA2=1 
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                YGREEN,YGREEN,YGREEN,NS,NTEMP,NTEMP,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
      ENDIF
C
C               ********************************************************
C               **  STEP 61--                                          *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS VARIABLES    *
C               **  (Y(.) AND X(.), RESPECTIVELY) FOR THE PLOT.        *
C               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .        *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
C               ********************************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASCO.EQ.'GREY')ICASPL='IMA2'
      CALL DPIMA2(YRED,YBLUE,YGREEN,YALPHA,ROWID,COLID,NS,
     1            ICASCO,PCOLMX,
     1            TEMP1,TEMP2,MAXOBV,
     1            Y,X,D,DCOLOR,DFILL,DSYMB,DSIZE,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIMAG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASPL
 9012   FORMAT('IFOUND,IERROR,ICASPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,MAXN,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,MAXN,IAND1,IAND2 = ',
     1         4I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9020I=1,MIN(NPLOTP,200)
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
          DO9030I=1,MIN(NPLOTP,200)
            WRITE(ICOUT,9031)I,DCOLOR(I),DFILL(I),DSYMB(I)
 9031       FORMAT('I,DCOLOR(I),DFILL(I),DSYMB(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9030     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIMA2(YRED,YBLUE,YGREEN,YALPHA,ROWID,COLID,N,
     1ICASCO,PCOLMX,
     1TEMP1,TEMP2,MAXOBV,
     1Y2,X2,D2,DCOLOR,DFILL,DSYMB,DSIZE,NPLOTP,NPLOTV,
     1IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--FORM A IMAGE PLOT.  THE X AND Y COORDINATES WILL
C              BE ROW AND COLUMN ID'S, RESPECTIVELY.  THE
C              RGB COLORS CORRESPONDING TO EACH ROWID/COLUMM ID
C              WILL BE CONTAINED IN DCOLOR, DFILL, AND DSYMB
C              (DSIZE IS BEING RESERVED FOR AN "ALPHA" CHANNEL,
C              THE ALPHA CHANNEL IS NOT YET IMPLEMENTED, BUT
C              IS BEING RESERVED FOR FUTURE IMPLEMENTATION).
C              GREYSCALE IMAGES WILL ONLY USE DCOLOR.
C
C           
C              COLORS WILL BE SCALED TO A (0,1) SCALE (THE
C              ROUTINES THAT ACTUALLY RENDER THE IMAGE WILL
C              CONVERT TO THE APPROPRIATE RESOLUTION FOR A
C              SPECIFIC DEVICE).
C     EXAMPLE--IMAGE PLOT RED BLUE GREEN ROWID COLID
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/3
C     ORIGINAL VERSION--MARCH     2008.
C
C-----COMMON----------------------------------------------------------
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ICASCO
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
C
      DIMENSION YRED(*)
      DIMENSION YBLUE(*)
      DIMENSION YGREEN(*)
      DIMENSION YALPHA(*)
      DIMENSION ROWID(*)
      DIMENSION COLID(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION DCOLOR(*)
      DIMENSION DFILL(*)
      DIMENSION DSYMB(*)
      DIMENSION DSIZE(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
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
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIMA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO
   52   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,ICASCO,PCOLMX
   53   FORMAT('N,ICASCO,PCOLMX = ',I8,2X,A4,2X,G12.4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(N,100)
          WRITE(ICOUT,56)I,YRED(I),YBLUE(I),YGREEN(I),ROWID(I),COLID(I)
   56     FORMAT('I,YRED(I),YBLUE(I),YGREEN(I),ROWID(I),COLID(I) = ',
     1           I8,5G12.4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CREATE RED, BLUE, AND GREEN COMPONENTS           **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOTE: IMAGE WILL BE DRAWN FROM TOP TO BOTTOM,
C           LEFT TO RIGHT.  SO CODE X2 AND Y2 APPROPRIATELY.
C
      IWRITE='OFF'
      CALL CODE(ROWID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO910I=1,N
        ROWID(I)=TEMP1(I)
  910 CONTINUE
      CALL CODE(COLID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO920I=1,N
        COLID(I)=TEMP1(I)
  920 CONTINUE
C
      AMAX=CPUMIN
      DO1000I=1,N
C
        X2(I)=COLID(I)
        Y2(I)=ROWID(I)
        D2(I)=1.0
        IF(ICASCO.EQ.'GREY')THEN
          DCOLOR(I)=ABS(YRED(I))
          IF(YRED(I).GT.AMAX)AMAX=YRED(I)
        ELSE
          DCOLOR(I)=ABS(YRED(I))
          DFILL(I)=ABS(YBLUE(I))
          DSYMB(I)=ABS(YGREEN(I))
          IF(YRED(I).GT.AMAX)AMAX=YRED(I)
          IF(YBLUE(I).GT.AMAX)AMAX=YBLUE(I)
          IF(YGREEN(I).GT.AMAX)AMAX=YGREEN(I)
        ENDIF
C
 1000 CONTINUE
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  NOW SCALE TO (0,1) SCALE                         **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AMAX=MAX(AMAX,PCOLMX)
      DO2000I=1,N
C
        IF(ICASCO.EQ.'GREY')THEN
          DCOLOR(I)=DCOLOR(I)/AMAX
        ELSE
          DCOLOR(I)=DCOLOR(I)/AMAX
          DFILL(I)=DFILL(I)/AMAX
          DSYMB(I)=DSYMB(I)/AMAX
        ENDIF
C
 2000 CONTINUE
C
      NPLOTP=N
      NPLOTV=2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIMA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO
 9012   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTP,NPLOTV
 9013   FORMAT('NPLOTP,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9015I=1,MIN(200,NPLOTP)
            WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016       FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3F10.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
          DO9025I=1,MIN(200,NPLOTP)
            WRITE(ICOUT,9026)I,DCOLOR(I),DFILL(I),DSYMB(I)
 9026       FORMAT('I,DCOLOR(I),DFILL(I),DSYMB(I) = ',I8,3G12.4)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIMP1(IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--THIS IS IMPLEMENTATION MODULE NUMBER 1.
C              THIS WILL RESULT IN--
C                 1) NO TIC MARKS OR TIC MARK LABELS ON UPPER FRAME LINE
C                 2) NO TIC MARKS OR TIC MARK LABELS ON RIGHT FRAME LINE
C                 3) NO VERTICAL LABEL (Y2LABEL) ON RIGHT FRAME LINE
C     NOTE--THIS SUBROUTINE WILL BE EXECUTED WHEN THE
C           ANALYST ENTERS THE COMMAND--
C                   IMPLEMENT 1
C     NOTE--THE IMPLEMENT COMMAND IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
C           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
C           SIZE OR TIC MARK CONVENTIONS OTHER THAN DATAPLOT'S DEFAULT)
C     INPUT  ARGUMENTS--NONE
C     OUTPUT ARGUMENTS--
C                     --IX2TSW
C                     --IY2TSW
C                     --IX2ZSW
C                     --IY2ZSW
C                     --NCY2LA
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
C
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY2TSW
C
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY2ZSW
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
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
      IFOUND='YES'
C
      IF(IBUGS2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPIMP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IX2TSW,IY2TSW
   52 FORMAT('IX2TSW,IY2TSW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IX2ZSW,IY2ZSW
   53 FORMAT('IX2ZSW,IY2ZSW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NCY2LA
   54 FORMAT('NCY2LA = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGS2,IFOUND,IERROR
   59 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C               ***********************************************
C               **  STEP 1--                                 **
C               **  DEFINE PARAMETER CHANGES TO BE MADE      **
C               **  FOR THIS IMPLEMENTATION MODULE NUMBER 1  **
C               ***********************************************
C
      IX2TSW='ON'
      IY2TSW='ON'
C
      IX2ZSW='ON'
      IY2ZSW='ON'
C
CCCCC NCY2LA=0
C
C               ***************************
C               **  STEP 2--             **
C               **  WRITE OUT A MESSAGE. **
C               ***************************
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('THE IMPLEMENTATION MODULE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      HAS JUST BEEN ACTIVATED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      WHICH ALLOWS TIC MARKS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)
 1154 FORMAT('      AND TIC MARK LABELS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      ON THE TOP AND RIGHT FRAME LINES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)
 1156 FORMAT('      OF ALL SUBSEQUENT PLOTS.')
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPIMP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IX2TSW,IY2TSW
 9012 FORMAT('IX2TSW,IY2TSW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IX2ZSW,IY2ZSW
 9013 FORMAT('IX2ZSW,IY2ZSW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NCY2LA
 9014 FORMAT('NCY2LA = ',I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPIMP2(ANUMVP,ANUMHP,
     1ISQUAR,
     1PXMIN,PYMIN,PXMAX,PYMAX,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--THIS IS IMPLEMENTATION MODULE NUMBER 2.
C              THIS WILL RESULT IN--
C                    THE PLOT FRAME CHANGED FROM RECTANGULAR
C                    TO SQUARE FOR ALL FUTURE PLOTS
C                    ON TEKTRONIX GRAPHICS DEVICES.
C     NOTE--THIS SUBROUTINE WILL BE EXECUTED WHEN THE
C           ANALYST ENTERS THE COMMAND--
C                   IMPLEMENT 2
C     NOTE--THE IMPLEMENT COMMAND IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
C           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
C           SIZE OR TIC MARK CONVENTIONS OTHER THAN DATAPLOT'S DEFAULT)
C     INPUT  ARGUMENTS--NONE
C     OUTPUT ARGUMENTS--
C                     --PXMIN
C                     --PYMIN
C                     --PXMAX
C                     --PYMAX
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISQUAR
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
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
      IFOUND='YES'
C
      IF(IBUGS2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPIMP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ANUMVP,ANUMHP
   52 FORMAT('ANUMVP,ANUMHP = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISQUAR
   53 FORMAT('ISQUAR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)PXMIN,PXMAX,PYMIN,PYMAX
   54 FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGS2,IFOUND,IERROR
   59 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ***********************************************
C               **  STEP 1--                                 **
C               **  DEFINE PARAMETER CHANGES TO BE MADE      **
C               **  FOR THIS IMPLEMENTATION MODULE NUMBER 2  **
C               ***********************************************
C
      ISQUAR='ON'
C
CCCCC PXMIN=15.0
CCCCC PYMIN=20.0
CCCCC PYMAX=90.0
C
CCCCC PYDEL=PYMAX-PYMIN
CCCCC PXDEL=PYDEL*(ANUMVP/ANUMHP)
CCCCC PXMAX=PXMIN+PXDEL
C
C               ***************************
C               **  STEP 2--             **
C               **  WRITE OUT A MESSAGE. **
C               ***************************
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('THE IMPLEMENTATION MODULE ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      HAS JUST BEEN ACTIVATED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      WHICH YIELDS A SQUARE PLOT FRAME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)
 1154 FORMAT('      FOR ALL SUBSEQUENT PLOTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      ON (CONTINUOUS) GRAPHICS DEVICES.')
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPIMP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ISQUAR
 9012 FORMAT('ISQUAR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ANUMVP,ANUMHP
 9013 FORMAT('ANUMVP,ANUMHP = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)PXMIN,PXMAX,PYMIN,PYMAX
 9014 FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGS2,IFOUND,IERROR
 9019 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPIMPL(IHARG,IARGT,IARG,NUMARG,
     1IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,
     1ISQUAR,
     1PXMIN,PYMIN,PXMAX,PYMAX,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--REINITIALIZE A SET OF UNDERLYING
C              FORTRAN PARAMETERS SO AS TO ACHIEVE
C              ALTERNATE SETTINGS FOR SUCH PARAMETERS.
C     NOTE--THIS CAPABILITY IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
C           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
C           SIZE OR NO TIC MARKS ON UPPER AND RIGHT FRAME).
C     INPUT  ARGUMENTS--
C                     --IHARG
C                     --IARGT
C                     --IARG
C                     --NUMARG
C                     --IBUGS2
C     OUTPUT ARGUMENTS--
C                     --IX2TSW
C                     --IY2TSW
C                     --IX2ZSW
C                     --IY2ZSW
C                     --NCY2LA
C
C                     --PXMIN
C                     --PYMIN
C                     --PXMAX
C                     --PYMAX
C
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISQUAR
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY2TSW
C
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY2ZSW
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
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
      IHOLD=(-999)
      IMPLNU=(-999)
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1050
      IF(IHARG(NUMARG).EQ.'ON')GOTO1050
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1050
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1050
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1050
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1060
      GOTO1040
C
 1040 CONTINUE
      IF(IHARG(NUMARG).EQ.'TICS')GOTO1100
      IF(IHARG(NUMARG).EQ.'SQUA')GOTO1200
      GOTO8000
C
 1050 CONTINUE
      IHOLD=0
      GOTO1070
C
 1060 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1070
C
 1070 CONTINUE
      IFOUND='YES'
      IMPLNU=IHOLD
C
      IF(IMPLNU.EQ.1)GOTO1100
      IF(IMPLNU.EQ.2)GOTO1200
      GOTO8000
C
 1100 CONTINUE
      CALL DPIMP1(IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,
     1IBUGS2,IFOUND,IERROR)
      GOTO9000
C
 1200 CONTINUE
      CALL DPIMP2(ANUMVP,ANUMHP,
     1ISQUAR,
     1PXMIN,PYMIN,PXMAX,PYMAX,
     1IBUGS2,IFOUND,IERROR)
      GOTO9000
C
 8000 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)
 8111 FORMAT('***** ERROR IN DPIMPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)
 8112 FORMAT('      AN ATTEMPT WAS MADE TO ACTIVATE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8113)IHARG(NUMARG)
 8113 FORMAT('      IMPLEMENTATION MODULE ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8114)
 8114 FORMAT('      BUT SUCH A MODULE DOES NOT EXIST.')
      CALL DPWRST('XXX','BUG ')
 8119 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DPIMPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NUMARG
 9012 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IHARG(1),IARGT(1),IARG(1)
 9013 FORMAT('IHARG(1),IARGT(1),IARG(1) = ',
     1A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHARG(NUMARG),IARGT(NUMARG),IARG(NUMARG)
 9014 FORMAT('IHARG(NUMARG),IARGT(NUMARG),IARG(NUMARG) = ',
     1A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IHOLD
 9015 FORMAT('IHOLD = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IMPLNU
 9016 FORMAT('IMPLNU = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)IBUGS2,IFOUND,IERROR
 9029 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPINCU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  MAXNXT,
     1                  ISEED,
     1                  ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING INFLUENCE CURVES--
C              AN INFLUENCE CURVE IS A MEASURE OF ROBUSTNESS.
C              IT PLOTS THE VALUE OF A STATISTIC WHEN ONE ADDITIONAL
C              VALUE IS ADDED.  FOR EXAMPLE,
C                 MEAN INFLUENCE CURVE Y XSEQ
C              CYCLES THROUGH THE POINTS IN XSEQ.  THE VERTICAL
C              AXIS IS THE VALUE OF THE MEAN FOR THE POINTS IN Y
C              WITH THE SINGLE VALUE IN XSEQ ADDED TO Y.
C
C              FOR THIS PLOT, ONLY ONE VARIABLE STATISTICS ARE
C              SUPPORTED (I.E., NO CORRELATION, ETC.).
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
C     VERSION NUMBER--2002/7
C     ORIGINAL VERSION--JULY      2002.
C     UPDATED         --MAY       2007. TRIMMED STANDARD DEVIATION
C     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO
C                                       COMMON
C     UPDATED         --NOVEMBER  2007. DOUBLE PRECISION ARRAYS FOR
C                                       CMPSTA
C     UPDATED         --NOVEMBER  2007. LP LOCATION
C     UPDATED         --NOVEMBER  2007. VARIANCE LP LOCATION
C     UPDATED         --NOVEMBER  2007. SD LP LOCATION
C     UPDATED         --SEPTEMBER 2008. BINOMIAL PROBABILITY
C     UPDATED         --FEBRUARY  2009. GRUBB
C     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
C     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
C     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
C     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
C     UPDATED         --MARCH     2009. PARSE WITH "EXTSTA"
C     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
C     UPDATED         --MARCH     2011. SUPPORT MULTIPLE CURVES (BUT
C                                       NOT REPLICATION)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4  ISTADF
      CHARACTER*60 ISTANM
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=30)
      CHARACTER*40 INAME
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Z1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
C
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),Z1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP3(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB9),XTEMP2(1))
C
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
C
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
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
      IERROR='NO'
      IFOUND='NO'
C
      ISUBN1='INCU'
      ISUBN2='    '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               **************************************
C               **  TREAT THE INFLUENCE CURVE CASE  **
C               **************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINCU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',4(A4,2X)A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPINCU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE IF OF THIS TYPE  **
C               **  AND BRANCH ACCORDINGLY.    **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.AND.ISUBRO.NE.'INCU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
C
C     MARCH 2009: USE EXTSTA TO PARSE STATISTIC
C
      JMIN=0
      JMAX=NUMARG
C
      DO200I=1,NUMARG
        IF(IHARG(I).EQ.'INFL')THEN
          JMAX=I-1
          ILASTC=I+1
          GOTO209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GOTO9000
  209 CONTINUE
C
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1            ICASPL,ISTANM,ISTANR,ISTADF,IFOUND,ILOCV,
     1            ISUBRO,IBUGG3,IERROR)
C
      IF(ISTANR.GE.2)IFOUND='NO'
      IF(IFOUND.EQ.'NO')GOTO9000
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='INFLUENCE CURVE'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=MAXSPN
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C     EXTRACT THE "SEQUENCE" VARIABLE.
C
      ICOL=NUMVAR
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            X1,X1,X1,NX,NX,NX,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ***********************************************
C               **  STEP 8A--                                **
C               **  MULTIPLE RESPONSE VARIABLES.  THESE CAN  **
C               **  BE EITHER VARIABLE OR MATRIX ARGUMENTS.  **
C               ***********************************************
C
      ISTEPN='8A'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
      NPLOTP=0
      NUMVA2=1
      NUMVA3=2
C
      DO810IRESP=1,NUMVAR-1
        ICOL=IRESP
        NCURVE=IRESP
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,Y1,Y1,NY,NY,NY,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************************
C               **  STEP 8B--                                        **
C               **  COMPUTE THE APPROPRIATE INFLUENCE CURVE --       **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
C               *******************************************************
C
        ISTEPN='8B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPINC2(Y1,X1,Z1,NX,NY,NUMVA3,ICASPL,ISIZE,ICONT,
     1              TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              IQUAME,IQUASE,PSTAMV,
     1              Y,X,D,NPLOTP,NPLOTV,NCURVE,
     1              ISUBRO,IBUGG3,IERROR)
C
C               *************************************************
C               **  STEP 29--                                  **
C               **  SAVE DIFFERENCE BETWEEN HIGHEST VALUE AND  **
C               **  LOWEST VALUE OF STATISTIC IN INTERNAL      **
C               **  PARAMETER ALOWHIGH                         **
C               *************************************************
C
C       CURRENTLY, ONLY DO THIS FOR FIRST CURVE.
C
        IF(NCURVE.EQ.1)THEN
          AMINS=CPUMAX
          AMAXS=CPUMIN
          DO2910I=1,NPLOTP
            IF(D(I).NE.1.0)GOTO2910
            IF(Y(I).GT.AMAXS)THEN
              AMAXS=Y(I)
              IMAXIN=I
            ENDIF
            IF(Y(I).LT.AMINS)THEN
              AMINS=Y(I)
              IMININ=I
            ENDIF
 2910     CONTINUE
          ADIFF=AMAXS-AMINS
          IF(IMAXIN.GT.IMININ)ADIFF=-ADIFF
C
          ISUBN0='INCU'
C
          IH='ALOW'
          IH2='HIGH'
          VALUE0=ADIFF
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGG3,IERROR)
        ENDIF
C
  810  CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINCU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,ISIZE
 9013   FORMAT('IFOUND,IERROR,ISIZE = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(IFOUND.EQ.'YES'.AND.NPLOTP.GT.0)THEN
          DO9025I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINC2(Y,X,Z,NX,NY,NUMV2,ICASPL,ISIZE,ICONT,
     1                  TEMP,TEMPZ,XIDTEM,XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  IQUAME,IQUASE,PSTAMV,
     1                  Y2,X2,D2,N2,NPLOTV,NCURVE,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING INFLUENCE CURVES--
C              AN INFLUENCE CURVE IS A MEASURE OF ROBUSTNESS.
C              IT PLOTS THE VALUE OF A STATISTIC WHEN ONE ADDITIONAL
C              VALUE IS ADDED.  FOR EXAMPLE,
C                 MEAN INFLUENCE CURVE Y XSEQ
C              CYCLES THROUGH THE POINTS IN XSEQ.  THE VERTICAL
C              AXIS IS THE VALUE OF THE MEAN FOR THE POINTS IN Y
C              WITH THE SINGLE VALUE IN XSEQ ADDED TO Y.
C
C              FOR THIS PLOT, ONLY ONE VARIABLE STATISTICS ARE
C              SUPPORTED (I.E., NO CORRELATION, ETC.).
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--MOSTELLER AND TUKEY, "EXPLORATORY DATA ANALYSIS".
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/7
C     ORIGINAL VERSION--JULY      2002.
C     UPDATED         --AUGUST    2002. USE CMPSTA TO COMPUTE THE
C                                       STATISTIC.
C     UPDATED         --APRIL     2003. ADD SN AND QN.  REQUIRED
C                                       ADDITIONAL SCRATCH ARRAYS
C     UPDATED         --NOVEMBER  2007. DOUBLE PRECISION ARRAYS FOR
C                                       CMPSTA
C     UPDATED         --NOVEMBER  2007. LP LOCATION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IQUAME
      CHARACTER*4 IQUASE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
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
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION XIDTEM(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
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
      ISUBN1='DPIN'
      ISUBN2='C2  '
C
      IWRITE='OFF'
C
      I2=0
      ISIZE2=0
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(NY.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN INFLUENCE CURVE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NY
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPINC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO
   71   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)NY,NX,NUMV2,ISIZE,ICASPL,ICONT
   72   FORMAT('NY,NX,NUMV2,ISIZE,ICASPL,ICONT = ',4I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,MAX(NY,NX)
          WRITE(ICOUT,74)I,Y(I),X(I)
   74     FORMAT('I, Y(I),X(I) = ',I8,2F15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  SORT THE HORIZONTAL AXIS VARIABLE, EXTRACT        **
C               **  THE DISTINCT VALUES.                              **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
  150 CONTINUE
      IWRITE='OFF'
      CALL SORT(X,NX,X)
      CALL DISTIN(X,NX,IWRITE,X,NXDIST,IBUGG3,IERROR)
C
C               ******************************************
C               **  STEP 2--                            **
C               **  COMPUTE THE SPECIFIED STATISTIC     **
C               **  FOR EACH DISTINCT VALUE OF X ADDED  **
C               **  TO THE Y VARIABLE.                  **
C               ******************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
C
      DO11000ISET=1,NXDIST
C
      ILAST=NY+1
      DO11011I=1,NY
        TEMP(I)=Y(I)
11011 CONTINUE
      TEMP(ILAST)=X(ISET)
      NS2=ILAST
C
      CALL CMPSTA(
     1TEMP,TEMPZ,TEMPZ,XTEMP1,XTEMP2,XTEMP3,
     1MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,
     1ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1DTEMP1,DTEMP2,DTEMP3,
CCCCC1IQUAME,IQUASE,PSTAMV,
     1RIGHT,
     1ISUBRO,IBUGG3,IERROR)
C
C     ---------------------------
C
79000 CONTINUE
      N2=N2+1
      Y2(N2)=RIGHT
      X2(N2)=X(ISET)
      D2(N2)=REAL(NCURVE)
C
11000 CONTINUE
      NPLOTV=3
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINC2--')
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIND2(X1,Y1,X2,Y2,PX,PY,
     1                  IFIG,ILINPA,ILINCO,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,PREBTH,
     1                  IREFSW,IREFCO,
     1                  IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A INDUCTOR WITH ONE END AT (X1,Y1)
C              AND THE OTHER END AT (X2,Y2).
C     NOTE--THE HEIGHT OF EACH LOOP IS PTEXHE.
C           THE WIDTH  OF EACH LOOP IS PTEXWI.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPINDU
C                                       RATHER THAN DPIND2
C
C-----NON-COMMON VARIABLES-------------------------------------
C
      DIMENSION PX(*)
      DIMENSION PY(*)
C
      CHARACTER*4 IFIG
C
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
C
      CHARACTER*4 IREBLI
      CHARACTER*4 IREBCO
      CHARACTER*4 IREFSW
      CHARACTER*4 IREFCO
      CHARACTER*4 IREPTY
      CHARACTER*4 IREPLI
      CHARACTER*4 IREPCO
C
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
C
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION PLINTH(*)
C
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
      DIMENSION PREPTH(*)
      DIMENSION PREPSP(*)
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
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'IND2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIND2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2
   53   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61   FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',A4,2X,A4,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IFIG,AREGBA(1)
   62   FORMAT('IFIG,AREGBA(1) = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1)
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1) = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64   FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1         3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXWI
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXWI = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE THE COORDINATES  **
C               **  FOR THE FIGURE             **
C               *********************************
C
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
C
      AJXMIN=PTEXWI
      AJXDEL=PTEXWI
      AJYDEL=PTEXHE
      AJXMAX=ALEN-2*AJXDEL
C
      XMIN=AJXMIN
      XDEL=AJXDEL
      YDEL=AJYDEL
      XMAX=AJXMAX
C
      K=0
C
      X=0
      Y=0
      K=K+1
      PX(K)=X1
      PY(K)=Y1
C
      X=XMIN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      AJX=AJXMIN-AJXDEL
CCCCC DO1450JX=JXMIN,JXMAX,JXDEL
 1440 CONTINUE
      AJX=AJX+AJXDEL
      IF(AJX.GT.AJXMAX)GOTO1460
C
      X=AJX
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      AJX3=XP
      AJY3=YP
C
      X=AJX+AJXDEL
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      AJX4=XP
      AJY4=YP
C
      CALL DPIND3(AJX3,AJY3,AJX4,AJY4,PX,PY,K,PX3,PY3,NP3,
     1            IFIG,IPATT,PTHICK,ICOL)
C
 1450 CONTINUE
      GOTO1440
C
 1460 CONTINUE
C
CCCCC X=XMAX
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      NP=K
C
C               ***********************
C               **  STEP 2--         **
C               **  FILL THE FIGURE  **
C               **  (IF CALLED FOR)  **
C               ***********************
C
CCCCC IF(IREFSW(1).EQ.'OFF')GOTO2190
CCCCC IPATT=IREPTY(1)
CCCCC PTHICK=PREPTH(1)
CCCCC PXGAP=PREPSP(1)
CCCCC PYGAP=PREPSP(1)
CCCCC ICOLF=IREFCO(1)
CCCCC ICOLP=IREPCO(1)
CCCCC CALL DPFIRE(PX,PY,NP,
CCCCC1            IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP)
C2190 CONTINUE
C
C               ***************************
C               **  STEP 3--             **
C               **  DRAW OUT THE FIGURE  **
C               ***************************
C
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,ICOL,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'IND2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIND2--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPIND3(X1,Y1,X2,Y2,PX,PY,K,PX3,PY3,NP3,
     1IFIG,IPATT,PTHICK,ICOL)
C
C     PURPOSE--DRAW A SEMI-CIRCLE FOR AN INDUCTOR
C              WITH ONE END OF THE DIAGONAL AT (X1,Y1)
C              AND THE OTHER END AT (X2,Y2).
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
C
C-----NON-COMMON VARIABLES-----------------------------------------
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
C
      DIMENSION PX(*)
      DIMENSION PY(*)
C
CCCCC DIMENSION PX3(*)
CCCCC DIMENSION PY3(*)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'IND3')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPIND3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)X1,Y1
   53 FORMAT('X1,Y1 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)X2,Y2
   54 FORMAT('X2,Y2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)K
   55 FORMAT('K = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      R=ALEN/2.0
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
C
      X=0.0
      Y=0.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      DO3010I=1,181,5
      IREV=181-I+1
      PHI2=IREV-1
      PHI2=PHI2*(2.0*3.1415926)/360.0
      X=R*COS(PHI2)+R
      Y=R*SIN(PHI2)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      IF(K.LE.490)GOTO3010
      NP=K
      IFLAG='ON'
CCCCC CALL DPDRPL(PX,PY,NP,PX3,PY3,NP3,
CCCCC1IFIG,IPATT,PTHICK,ICOL)
      CALL DPDRPL(PX,PY,NP,
     1IFIG,IPATT,PTHICK,ICOL,
     1JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
      K=0
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
 3010 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'IND3')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPIND3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)K
 9014 FORMAT('K = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,K
      WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016 FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPINDM(Y1,N1,Y2,N2,ICASE,
     1                  STATVA,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE IMPLEMENTS THE FOLLOWING COMMANDS:
C
C                 LET A = INDEX FIRST MATCH Y1 Y2
C                 LET A = INDEX LAST  MATCH Y1 Y2
C                 LET A = INDEX FIRST NOT MATCH Y1 Y2
C                 LET A = INDEX LAST  NOT MATCH Y1 Y2
C
C              THAT IS, RETURN THE INDEX OF EITHER THE FIRST OR LAST
C              MATCHING (OR NON-MATCHING) ENTRIES FOR TWO ARRAYS.
C              NOTE THAT THE INPUT ARRAYS NEED NOT BE OF THE SAME
C              SIZE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/11
C     ORIGINAL VERSION--NOVEMBER  2011
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
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
      ISUBN1='DPIN'
      ISUBN2='DM  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INDM')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPINDM--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2
   52   FORMAT('IBUGA3,ISUBRO,N1,N2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO66I=1,N2
          WRITE(ICOUT,67)I,Y2(I)
   67     FORMAT('I,Y2(I) = ',I8,E15.7)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INDM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN INDEX ... MATCH')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N1
 1115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(N2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)
 1123   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE SECOND ',
     1         'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************
C               **  STEP 21--               **
C               **  DETERMINE THE INDEX     **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INDM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      STATVA=0.0
C
      IF(ICASE.EQ.'FMAT')THEN
        DO100I=1,MIN(N1,N2)
          IF(Y1(I).EQ.Y2(I))THEN
            STATVA=REAL(I)
            GOTO9000
          ENDIF
  100   CONTINUE
      ELSEIF(ICASE.EQ.'LMAT')THEN
        DO200I=MIN(N1,N2),1,-1
          IF(Y1(I).EQ.Y2(I))THEN
            STATVA=REAL(I)
            GOTO9000
          ENDIF
  200   CONTINUE
      ELSEIF(ICASE.EQ.'FNOM')THEN
        DO300I=1,MIN(N1,N2)
          IF(Y1(I).NE.Y2(I))THEN
            STATVA=REAL(I)
            GOTO9000
          ENDIF
  300   CONTINUE
        IF(N1.NE.N2)STATVA=REAL(MIN(N1,N2)+1)
      ELSEIF(ICASE.EQ.'LNOM')THEN
        DO400I=MIN(N1,N2),1,-1
          IF(Y1(I).NE.Y2(I))THEN
            STATVA=REAL(I)
            GOTO9000
          ENDIF
  400   CONTINUE
        IF(N1.NE.N2)STATVA=REAL(MAX(N1,N2))
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INDM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINDM--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR,STATVA
 9012   FORMAT('IERROR,STATVA = ',A4,2X,G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINDU(IHARG,IARGT,ARG,NUMARG,
     1                  PXSTAR,PYSTAR,PXEND,PYEND,
     1                  ILINPA,ILINCO,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,PREBTH,
     1                  IREFSW,IREFCO,
     1                  IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG,
     1                  IGRASW,IDIASW,
     1                  PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                  PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DRAW ONE OR MORE INDUCTORS (DEPENDING ON HOW MANY NUMBERS ARE
C              PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE THE 2 ENDS OF THE INDUCTOR.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
C     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
C           LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER ABSOLUTE OR
C           RELATIVE) AS DEFINED BY THE 2 NUMBERS.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
C           ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2 NUMBERS TO THE
C           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE THIRD
C           AND FOURTH NUMBERS.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
C           (X,Y) POSITION AS RESULTING FROM THE THIRD AND FOURTH NUMBERS TO
C           THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
C           FIFTH AND SIXTH NUMBERS.
C     NOTE--AND SO FORTH FOR 8, 10, 12, ... NUMBERS.
C     INPUT  ARGUMENTS--IHARG
C                     --IARGT
C                     --ARG
C                     --NUMARG
C                     --PXSTAR
C                     --PYSTAR
C     OUTPUT ARGUMENTS--PXEND
C                     --PYEND
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --NOVEMBER  1982.
C     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
C     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
C                                       NONE DEVICE
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
C     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPINDU
C                                       RATHER THAN DPIND2
C
C-----NON-COMMON VARIABLES-----------------------------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
C
      CHARACTER*4 IREBLI
      CHARACTER*4 IREBCO
      CHARACTER*4 IREFSW
      CHARACTER*4 IREFCO
      CHARACTER*4 IREPTY
      CHARACTER*4 IREPLI
      CHARACTER*4 IREPCO
C
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDFONT
      CHARACTER*4 UNITSW
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFIG
      CHARACTER*4 IBELSW
      CHARACTER*4 IERASW
      CHARACTER*4 IBACCO
      CHARACTER*4 ICOPSW
      CHARACTER*4 ITYPEO
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION PLINTH(*)
C
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
      DIMENSION PREPTH(*)
      DIMENSION PREPSP(*)
      DIMENSION PDSCAL(*)
C
      DIMENSION IDMANU(*)
      DIMENSION IDMODE(*)
      DIMENSION IDMOD2(*)
      DIMENSION IDMOD3(*)
      DIMENSION IDPOWE(*)
      DIMENSION IDCONT(*)
      DIMENSION IDCOLO(*)
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
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
      IFOUND='NO'
      IERROR='NO'
      IERRG4=IERROR
C
      ILOCFN=0
      NUMNUM=0
C
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'INDU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINDU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMARG,NUMDEV
   53   FORMAT('NUMARG,NUMDEV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMARG
          WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56     FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2(2X,A4),G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)PXSTAR,PYSTAR,PXEND,PYEND
   57   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61   FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1)
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1) = ',
     1         2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64   FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1         3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG= ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,76)IGRASW,IDIASW
   76   FORMAT('IGRASW,IDIASW = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,77)PGRAXF,PGRAYF,PDIAXC,PDIAYC
   77   FORMAT('PGRAXF,PGRAYF,PDIAXC,PDIAYC = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,78)PDIAHE,PDIAWI,PDIAVG,PDIAHG
   78   FORMAT('PDIAHE,PDIAWI,PDIAVG,PDIAHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO81I=1,NUMDEV
          WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82     FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',
     1           3(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83     FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84     FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
   81   CONTINUE
        WRITE(ICOUT,88)IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR
   88   FORMAT('IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFIG='INDU'
      NUMPT=2
      NUMPT2=2*NUMPT
C
C               ********************************
C               **  STEP 0--                  **
C               **  STEP THROUGH EACH DEVICE  **
C               ********************************
C
      IF(NUMDEV.LE.0)GOTO9000
      DO8000IDEVIC=1,NUMDEV
C
        IF(IDPOWE(IDEVIC).EQ.'OFF')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'OFF')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'NULL')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'NONE')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'DISC')GOTO8000
C
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
        IOFFSV=IDNVOF(IDEVIC)
        IOFFSH=IDNHOF(IDEVIC)
        IGUNIT=IDUNIT(IDEVIC)
        PCHSCA=PDSCAL(IDEVIC)
C
C               ************************************
C               **  STEP 1--                      **
C               **  CARRY OUT OPENING OPERATIONS  **
C               **  ON THE GRAPHICS DEVICES       **
C               ************************************
C
        CALL DPOPDE
C
        IBELSW='OFF'
        NUMRIN=0
        IERASW='OFF'
        IBACCO='JUNK'
C
        CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO)
C
C               *****************************************
C               **  STEP 2--                           **
C               **  SEARCH FOR COMMAND SPECIFICATIONS  **
C               *****************************************
C
        IF(NUMARG.GE.2.AND.
     1     IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=1
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.
     1         IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=2
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.
     1         IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='RELA'
          ILOCFN=2
        ELSE
          GOTO1130
        ENDIF
C
        IF(ILOCFN.GT.NUMARG)GOTO1130
        DO1120I=ILOCFN,NUMARG
          IF(IARGT(I).NE.'NUMB')GOTO1130
 1120   CONTINUE
        IFOUND='YES'
C
C               ****************************
C               **  STEP 3--              **
C               **  DRAW OUT THE LINE(S)  **
C               ****************************
C
        NUMNUM=NUMARG-ILOCFN+1
        IF(NUMNUM.LT.NUMPT2)THEN
          J=ILOCFN-1
          X1=PXSTAR
          Y1=PYSTAR
        ELSE
          J=ILOCFN
          IF(J.GT.NUMARG)GOTO1190
          X1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,
     1       IBUGD2,ISUBRO,IERROR)
          J=J+1
          IF(J.GT.NUMARG)GOTO1190
          Y1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,
     1       IBUGD2,ISUBRO,IERROR)
        ENDIF
C
 1160   CONTINUE
        J=J+1
        IF(J.GT.NUMARG)GOTO1190
        X2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X2=X1+X2
        J=J+1
        IF(J.GT.NUMARG)GOTO1190
        Y2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
C
 1170   CONTINUE
        CALL DPIND2(X1,Y1,X2,Y2,PX,PY,
     1              IFIG,ILINPA,ILINCO,PLINTH,
     1              AREGBA,IREBLI,IREBCO,PREBTH,
     1              IREFSW,IREFCO,
     1              IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1              PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
        X1=X2
        Y1=Y2
C
        GOTO1160
 1190   CONTINUE
C
        PXEND=X2
        PYEND=Y2
C
C               ************************************
C               **  STEP 4--                      **
C               **  CARRY OUT CLOSING OPERATIONS  **
C               **  ON THE GRAPHICS DEVICES       **
C               ************************************
C
        ICOPSW='OFF'
        NUMCOP=0
        CALL DPCLPL(ICOPSW,NUMCOP,
     1              PGRAXF,PGRAYF,
     1              IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1              PDIAHE,PDIAWI,PDIAVG,PDIAHG)
C
        CALL DPCLDE
C
 8000 CONTINUE
      GOTO9000
C
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN INDUCTOR (DPINDU)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE INDUCTOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A INDUCTOR ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      FROM THE POINT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      TO THE POINT 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      INDUCTOR 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      INDUCTOR ABSOLUTE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      INDUCTOR RELATIVE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'INDU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINDU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ILOCFN,NUMNUM
 9012   FORMAT('IFOUND,IERROR,ILOCFN,NUMNUM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)X1,Y1,X2,Y2,X3,Y3
 9013   FORMAT('X1,Y1,X2,Y2,X3,Y3 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PXSTAR,PYSTAR,PXEND,PYEND
 9015   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINFU(IFUNC3,N3,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1                  NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,
     1                  NEWNAM,MAXN3,
     1                  IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
C
C     PURPOSE--INSERT (IF NECESSARY) THE FUNCTION IN IFUNC3(.) INTO THE
C              GENERAL DATAPLOT INTERNAL FUNCTION TABLE IFUNC(.).
C              ALSO, UPDATE INTERNAL DATAPLOT LISTS (IF NECESSARY).
C
C     INPUT  FUNCTION--IN IFUNC3(.)
C     OUTPUT FUNCTION--SOMEWHERE IN IFUNC(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1978.
C     UPDATED         --JANUARY   1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1993. FIX BUG STATEMENT
C                                       MAXCHF => 120
C     UPDATED         --JANUARY   2012. IF N3 < 0, THEN DELETE THE
C                                       STRING
C     UPDATED         --MARCH     2015. UPDATE "IN" ARRAY TO 0 WHEN
C                                       DEFINING A NEW STRING
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFUNC3
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IFUNC
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
C
      DIMENSION IANS(*)
C
      DIMENSION IFUNC3(*)
      DIMENSION IFUNC(*)
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
      ISUBN1='DPIN'
      ISUBN2='FU  '
      IERROR='NO'
C
      IDEL=0
C
C               ******************************************
C               **  INSERT A FUNCTION                   **
C               **  INTO THE GENERAL DATAPLOT FUNCTION  **
C               **  TABLE IFUNC(.).                     **
C               **  MAKE ADJUSTMENTS TO THE             **
C               **  INTERNAL DATAPLOT LISTS.            **
C               ******************************************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMNAM,ILISTL,NEWNAM,IBUGA3
   53   FORMAT('NUMNAM,ILISTL,NEWNAM,IBUGA3 = ',2I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVSTAR(I),IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1           I8,2X,A4,A4,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,57)N3,NUMCHF,MAXN3,MAXCHF
   57   FORMAT('N3,NUMCHF,MAXN3,MAXCHF = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(N3.GE.1)THEN
          WRITE(ICOUT,59)(IFUNC3(I),I=1,MIN(N3,120))
   59     FORMAT('IFUNC3(.) = ',120A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
CCCCC   THE FOLLOWING LINE WAS CHANGED     DECEMBER 1993
CCCCC   WRITE(ICOUT,60)(IFUNC(I),I=1,MAXCHF)
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(MAXCHF,120))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      NUMCH0=NUMCHF
C
C               *****************************************************
C               **  STEP 2--                                       **
C               **  DETERMINE IF THE ADDITION OF THE NEW FUNCTION  **
C               **  TO THE INTERNAL DATAPLOT TABLE                 **
C               **  WILL OVERFLOW THE TABLE (TYPICALLY             **
C               **  THERE IS A MAXCHF CHARACTER LIMIT                **
C               **  FOR THE SUM TOTAL OVER ALL FUNCTIONS).         **
C               *****************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')THEN
        IF(NUMNAM.GE.MAXN3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
 1001     FORMAT('***** ERROR IN DPINFU--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1003)IHLEFT,IHLEF2
 1003     FORMAT('      MAXIMUM NUMBER OF NAMES EXCEEDED.  STRING ',
     1           2A4,' NOT UPDATED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        N0TEST=NUMCHF+N3
      ELSE
        IMIN=IVSTAR(ILISTL)
        IMAX=IVSTOP(ILISTL)
        N3OLD=IMAX-IMIN+1
        IF(N3.GE.0)THEN
          IDEL=N3-N3OLD
          N0TEST=NUMCHF+IDEL
        ELSE
          IDEL=N3OLD
          N0TEST=NUMCHF-N3OLD
        ENDIF
      ENDIF
C
      IF(N0TEST.GT.MAXCHF)THEN
        WRITE(ICOUT,2301)
 2301   FORMAT('***** ERROR IN DPINFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2302)
 2302   FORMAT('      ERROR CAUSED IN ENTERING THE FUNCTION INTO THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2304)
 2304   FORMAT('      INTERNAL DATAPLOT FUNCTION TABLE.  THE TOTAL ',
     1         'NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2306)MAXCHF
 2306   FORMAT('      CHARACTERS IN THAT TABLE (FOR ALL FUNCTIONS) ',
     1         'MAY NOT EXCEED ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2307)
 2307   FORMAT('      SUCH AN OVERFLOW CONDITION HAS JUST BEEN ',
     1         'ENCOUNTERED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2309)
 2309   FORMAT('      THE FUNCTION TABLE HAS BEEN RESET TO ITS STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2311)
 2311   FORMAT('      BEFORE ATTEMPTING TO ENTER THE LAST FUNCTION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2313)
 2313   FORMAT('      THE TOTAL NUMBER OF CHARACTERS IN THE FUNCTION ',
     1         'TABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2315)NUMCHF
 2315   FORMAT('      HAS BEEN RESET TO ITS PREVIOUS VALUE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2316)
 2316   FORMAT('      THE NUMBER OF CHARACTERS IN THE FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2317)N3
 2317   FORMAT('      THAT WAS ATTEMPTED TO BE ENTERED = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2318)
 2318   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2319)(IANS(I),I=1,MIN(IWIDTH,100))
 2319   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2321)
 2321   FORMAT('      SUGGESTED POSSIBLE SOLUTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2322)
 2322   FORMAT('      REDEFINE SOME OF THE OTHER ALREADY DEFINED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2324)
 2324   FORMAT('      FUNCTIONS THAT MAY NO LONGER BE NEEDED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2325)
 2325   FORMAT('      SO THAT THEY ARE ONLY 1 CHARACTER LONG')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2326)
 2326   FORMAT('      EXAMPLE--LET FUNCTION F3=C')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ***************************************************
C               **  STEP 3--                                     **
C               **  MOVE THE SEGMENT OF THE STRING IN IFUNC(.)   **
C               **  WHICH IS BEYOND THE FUNCTION OF INTEREST     **
C               **  OVER AN APPROPRIATE NUMBER OF SPACES.        **
C               ***************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')THEN
        ISTART=NUMCHF+1
        ISTOP=ISTART+N3-1
      ELSEIF(N3.GE.0)THEN
        ISTART=IVSTAR(ILISTL)
        ISTOP=ISTART+N3-1
      ELSE
        ISTART=IVSTAR(ILISTL)
        ISTOP=ISTART+N3OLD-1
      ENDIF
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2401)N3,N3OLD,IDEL,ISTART,ISTOP
 2401   FORMAT('N3,N3OLD,IDEL,ISTART,ISTOP = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NEWNAM.EQ.'YES')GOTO3290
      IF(N3.LT.0)THEN
        KMIN=ISTART
        KMAX=NUMCHF-IDEL
        DO3205K=KMIN,KMAX
          L=K+IDEL
          IFUNC(K)=IFUNC(L)
 3205   CONTINUE
      ELSE
        KMIN=ISTOP+1
        KMAX=NUMCHF+IDEL
C
        IF(IDEL.EQ.0)GOTO3290
        IF(IDEL.GT.0)THEN
          DO3215K=KMIN,KMAX
            KREV=KMAX-K+KMIN
            LREV=KREV-IDEL
            IFUNC(KREV)=IFUNC(LREV)
 3215     CONTINUE
        ELSEIF(IDEL.LT.0)THEN
          DO3225K=KMIN,KMAX
            L=K-IDEL
            IFUNC(K)=IFUNC(L)
 3225     CONTINUE
        ENDIF
      ENDIF
C
 3290 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3291)(IFUNC(I),I=1,MIN(MAXCHF,120))
 3291   FORMAT('AT 3290: IFUNC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               **************************************************
C               **  STEP 4--                                    **
C               **  MOVE THE NEW FUNCTION INTO THE APPROPRIATE  **
C               **  PLACE IN IFUNC(.).                          **
C               **************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N3.GE.0)THEN
        L=0
        DO4200K=ISTART,ISTOP
          L=L+1
          IFUNC(K)=IFUNC3(L)
 4200    CONTINUE
      ENDIF
C
C               ************************************
C               **  STEP 5--                      **
C               **  REDEFINE NUMCHF = THE UPDATED **
C               **  LENGTH OF IFUNC(.).           **
C               ************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCHF=N0TEST
C
C               *************************************************
C               **  STEP 6--                                   **
C               **  MAKE THE ADJUSTMENTS TO THE INTERNAL LIST  **
C               *************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')THEN
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='F'
        IN(ILISTL)=0
        IVSTAR(ILISTL)=ISTART
        IVSTOP(ILISTL)=ISTOP
        NUMNAM=NUMNAM+1
        GOTO9000
      ELSEIF(N3.GE.0)THEN
        N3OLD=IVSTOP(ILISTL)-IVSTAR(ILISTL)+1
        IDEL=N3-N3OLD
C
        DO6210I=1,NUMNAM
          IF(IUSE(I).EQ.'F')THEN
            IF(IVSTAR(I).GT.ISTART)IVSTAR(I)=IVSTAR(I)+IDEL
            IF(IVSTOP(I).GE.ISTART)IVSTOP(I)=IVSTOP(I)+IDEL
          ENDIF
 6210   CONTINUE
      ELSE
        N3OLD=IVSTOP(ILISTL)-IVSTAR(ILISTL)+1
        IDEL=N3OLD
C
        DO6220I=1,NUMNAM
          IF(IUSE(I).EQ.'F')THEN
            IF(IVSTAR(I).GT.ISTART)IVSTAR(I)=IVSTAR(I)-IDEL
            IF(IVSTOP(I).GE.ISTART)IVSTOP(I)=IVSTOP(I)-IDEL
          ENDIF
 6220   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NEWNAM,NUMNAM
 9013   FORMAT('IERROR,NEWNAM,NUMNAM = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1           I8,2X,A4,A4,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9018)NUMCH0,N3,NUMCHF,MAXN3,MAXCHF
 9018   FORMAT('NUMCH0,N3,NUMCHF,MAXN3,MAXCHF = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)(IFUNC3(I),I=1,MIN(N3,120))
 9019   FORMAT('IFUNC3(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
CCCCC   THE FOLLOWING LINE WAS CHANGED     DECEMBER 1993
CCCCC   WRITE(ICOUT,9020)(IFUNC(I),I=1,MAXCHF)
        WRITE(ICOUT,9020)(IFUNC(I),I=1,MIN(MAXCHF,120))
 9020   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINLE(ILEGN2,ISTH,N2,ILEGNA,ILEGST,ILEGSP,
     1NUMLEG,MAXLEG,ILEGTE,NCLEG,MXCLEG,IANS,IWIDTH,IBUGIL,IERROR)
C
C     PURPOSE--INSERT(IF NECESSARY) THE HOLLERITH LEGEND
C              IN ISTH(.)
C              INTO (RESPECTIVELY) THE PACKED
C              INTERNAL DATAPLOT TABLES ILEGTE(.)
C              ALSO, UPDATE INTERNAL DATAPLOT LISTS
C              ILEGNA(.), ILEGST(.), AND ILEGSP(.).
C              A CHECK FOR N2 BEING POSITIVE IS DONE HEREIN.
C
C     NOTE--IT IS ASSUMED IN ALL CASES (EVEN FOR
C           A BLANKED-OUT LEGEND) THAT THE NUMBER
C           OF CHARACTERS IN THE LEGEND IS AT LEAST 1;
C           (THAT IS, THE INPUT N2 IS 1 OR LARGER).
C
C     INPUT  LEGENDS --IN ISTH(.)
C     OUTPUT LEGENDS --SOMEWHERE IN ILEGTE(.)
C
C     ILEGN2 = NAME FOR THE INPUT LEGEND.
C     ISTH   = VECTOR CONTAINING INPUT LEGEND STRING (IN HOLLERITH)
C     N2     = LENGTH OF INPUT LEGEND STRING.
C     ILEGNA = TABLE OF EXISTING LEGEND NAMES.
C     ILEGST = TABLE OF EXISTING START POSITIONS IN ILEGTE.
C     ILEGSP = TABLE OF EXISTING STOP  POSITIONS IN ILEGTE.
C     NUMLEG = NUMBER OF EXISTING LEGENDS.
C     MAXLEG = MAXIMUM NUMBER OF ALLOWABLE LEGENDS.
C     ILEGTE  = VECTOR OF PACKED LEGENDS (HOLLERITH) WHERE FINAL STORAGE IS DONE
C     NCLEG = NUMBER OF PACKED CHARACTERS IN ILEGTE(.)
C     MXCLEG = MAX NUMBER OF ALLOWABLE CHARACTERS IN ILEGTE(.)
C
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1979.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1994.  BUG FIX
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ILEGN2
      CHARACTER*4 ISTH
      CHARACTER*4 ILEGNA
      CHARACTER*4 ILEGTE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGIL
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION ISTH(*)
      DIMENSION ILEGNA(*)
      DIMENSION ILEGST(*)
      DIMENSION ILEGSP(*)
      DIMENSION ILEGTE(*)
      DIMENSION IANS(*)
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
      ISUBN1='DPIN'
      ISUBN2='LE  '
C
      ILISTL=0
      IDEL=0
C
      NEWNAM='UNKN'
C
C               ******************************************
C               **  INSERT A LEGEND                     **
C               **  INTO THE GENERAL DATAPLOT LEGEND    **
C               **  TABLES ILEGTE(.)        **
C               **  MAKE ADJUSTMENTS TO THE             **
C               **  INTERNAL DATAPLOT LISTS.            **
C               ******************************************
C
      IF(IBUGIL.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)
   71 FORMAT('***** AT THE BEGINNING OF DPINLE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)ILEGN2,N2
   72 FORMAT('ILEGN2,N2 = ',A4,3X,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,73)(ISTH(I),I=1,N2)
   73 FORMAT('ISTH(.) = ',55A2)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,75)NCLEG,MXCLEG
   75 FORMAT('NCLEG,MXCLEG = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,76)(ILEGTE(I),I=1,NCLEG)
   76 FORMAT('ILEGTE(.) = ',55A2)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)NUMLEG,MAXLEG
   81 FORMAT('NUMLEG,MAXLEG = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO82I=1,NUMLEG
      WRITE(ICOUT,83)I,ILEGNA(I),ILEGST(I),ILEGSP(I)
   83 FORMAT('I,ILEGNA(I),ILEGST(I),ILEGSP(I) = ',I4,3X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
   82 CONTINUE
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      NUMCH0=NCLEG
C
      IF(N2.GE.1)GOTO190
C
      WRITE(ICOUT,111)
  111 FORMAT('***** INTERNAL ERROR IN DPLEG--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT LENGTH N2 OF THE STRING IS ',
     1'NON-POSITIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)N2
  113 FORMAT('      N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  190 CONTINUE
C
C               ************************************
C               **  STEP 2--                      **
C               **  DETERMINE IF THE LEGEND NAME  **
C               **  ALREADY EXISTS IN THE TABLE.  **
C               ************************************
C
      ISTEPN='2'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='YES'
      IF(NUMLEG.LE.0)GOTO250
      DO210I=1,NUMLEG
      I2=I
      IF(ILEGN2.EQ.ILEGNA(I))GOTO220
  210 CONTINUE
      GOTO250
C
  220 CONTINUE
      NEWNAM='NO'
      ILISTL=I2
      GOTO290
C
  250 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMLEG+1
      GOTO290
C
  290 CONTINUE
C
C               ***********************************************************
C               **  STEP 3--                                             **
C               **  FOR THE CASE WHEN HAVE A NEW NAME,                   **
C               **  DETERMINE IF THIS NEW NAME                           **
C               **  WILL OVERFLOW THE ALLOWABLE NUMBER OF LEGEND NAMES   **
C               **   IN TABLE ILEGNA(.).                                 **
C               ***********************************************************
C
      ISTEPN='3'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'NO')GOTO390
      IF(ILISTL.LE.MAXLEG)GOTO390
C
      WRITE(ICOUT,301)
  301 FORMAT('***** ERROR IN DPINLE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,302)
  302 FORMAT('      ERROR CAUSED IN ENTERING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,303)
  303 FORMAT('      THE LEGEND   INTO THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,304)
  304 FORMAT('      INTERNAL DATAPLOT LEGEND   TABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,305)
  305 FORMAT('      THE TOTAL NUMBER OF LEGENDS IN THAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,306)MAXLEG
  306 FORMAT('      TABLE (FOR ALL LEGENDS) MAY NOT EXCEED ',
     1I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,307)
  307 FORMAT('      SUCH AN OVERFLOW CONDITION HAS JUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,308)
  308 FORMAT('      BEEN ENCOUNTERED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,309)
  309 FORMAT('      THE LEGEND   TABLE HAS JUST BEEN RESET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,310)
  310 FORMAT('      TO  ITS STATUS BEFORE THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
  311 FORMAT('      LAST LEGEND   WAS ATTEMPTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      TO BE ENTERED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,313)
  313 FORMAT('      THE TOTAL NUMBER OF LEGENDS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,314)
  314 FORMAT('      IN THE LEGEND   TABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,315)NUMLEG
  315 FORMAT('      HAS JUST BEEN RESET TO ITS PREVIOUS VALUE =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,318)
  318 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,319)(IANS(I),I=1,IWIDTH)
  319 FORMAT('      ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,321)
  321 FORMAT('      SUGGESTED POSSIBLE SOLUTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,322)
  322 FORMAT('      REDEFINE SOME OF THE OTHER ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,323)
  323 FORMAT('      ALREADY-DEFINED LEGENDS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,324)
  324 FORMAT('      THAT MAY NO LONGER BE NEEDED.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  390 CONTINUE
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  DETERMINE IF THE ADDITION OF THE NEW LEGEND    **
C               **  STRING TO THE INTERNAL DATAPLOT TABLES         **
C               **  ILEGTE(.)                                  **
C               **  WILL OVERFLOW THE TABLE (TYPICALLY             **
C               **  THERE IS A 500 CHARACTER LIMIT                 **
C               **  FOR THE SUM TOTAL OVER ALL LEGENDS).           **
C               *****************************************************
C
      ISTEPN='4'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')GOTO2100
      GOTO2200
C
 2100 CONTINUE
      N0TEST=NCLEG+N2
      GOTO2300
C
 2200 CONTINUE
      IMIN=ILEGST(ILISTL)
      IMAX=ILEGSP(ILISTL)
      N2OLD=IMAX-IMIN+1
      IDEL=N2-N2OLD
      N0TEST=NCLEG+IDEL
      GOTO2300
C
 2300 CONTINUE
      IF(N0TEST.LE.MXCLEG)GOTO2390
      WRITE(ICOUT,2301)
 2301 FORMAT('***** ERROR IN DPINLE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2302)
 2302 FORMAT('      ERROR CAUSED IN ENTERING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2303)
 2303 FORMAT('      THE LEGEND   INTO THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2304)
 2304 FORMAT('      INTERNAL DATAPLOT LEGEND   TABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2305)
 2305 FORMAT('      THE TOTAL NUMBER OF CHARACTERS IN THAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2306)MXCLEG
 2306 FORMAT('      TABLE (FOR ALL LEGEND  S) MAY NOT EXCEED ',
     1I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2307)
 2307 FORMAT('      SUCH AN OVERFLOW CONDITION HAS JUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2308)
 2308 FORMAT('      BEEN ENCOUNTERED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2309)
 2309 FORMAT('      THE LEGEND   TABLE HAS JUST BEEN RESET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2310)
 2310 FORMAT('      TO  ITS STATUS BEFORE THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2311)
 2311 FORMAT('      LAST LEGEND   WAS ATTEMPTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2312)
 2312 FORMAT('      TO BE ENTERED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2313)
 2313 FORMAT('      THE TOTAL NUMBER OF CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2314)
 2314 FORMAT('      IN THE LEGEND   TABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2315)NCLEG
 2315 FORMAT('      HAS JUST BEEN RESET TO ITS PREVIOUS VALUE =',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2316)
 2316 FORMAT('      THE NUMBER OF CHARACTERS IN THE LEGEND  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2317)N2
 2317 FORMAT('      THAT WAS ATTEMPTED TO BE ENTERED = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2318)
 2318 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2319)(IANS(I),I=1,IWIDTH)
 2319 FORMAT('      ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2321)
 2321 FORMAT('      SUGGESTED POSSIBLE SOLUTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2322)
 2322 FORMAT('      REDEFINE (SHORTEN) SOME OF THE OTHER ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2323)
 2323 FORMAT('      ALREADY-DEFINED LEGENDS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2324)
 2324 FORMAT('      THAT MAY NO LONGER BE NEEDED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2325)
 2325 FORMAT('      SO THAT THEY ARE ONLY 1 CHARACTER LONG')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2326)
 2326 FORMAT('      EXAMPLE--LEGEND 4    ')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2390 CONTINUE
C
C               ***************************************************
C               **  STEP 5--                                     **
C               **  MOVE THE SEGMENT OF THE STRING IN ILEGTE(.)   **
C               **  WHICH IS BEYOND THE LEGEND   OF INTEREST     **
C               **  OVER AN APPROPRIATE NUMBER OF SPACES.        **
C               ***************************************************
C
      ISTEPN='5'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')GOTO3110
      GOTO3120
C
 3110 CONTINUE
      ISTART=NCLEG+1
      ISTOP=ISTART+N2-1
      GOTO3190
C
 3120 CONTINUE
      ISTART=ILEGST(ILISTL)
      ISTOP=ISTART+N2-1
      GOTO3190
C
 3190 CONTINUE
C
      IF(NEWNAM.EQ.'YES')GOTO3290
      KMIN=ISTOP+1
      KMAX=NCLEG+IDEL
CCCCC JUNE 1994.  FOLLOWING LINE CAUSED SPURIOUS CHARACTERS IF 
CCCCC HIGHER LEGENDS BLANKED OUT, EARLIER LEGEND LONGER THAN THE
CCCCC ORIGINAL.
CCCCC IF(KMIN.GT.NCLEG)GOTO3290
      IF(IDEL.LE.0)GOTO3210
      GOTO3220
C
 3210 CONTINUE
      NCLEGP=NCLEG+1
      DO3211K=KMIN,KMAX
      L=K-IDEL
      IF(L.GE.NCLEGP)GOTO3211
      ILEGTE(K)=ILEGTE(L)
 3211 CONTINUE
      GOTO3290
C
 3220 CONTINUE
      DO3221K=KMIN,KMAX
      KREV=KMAX-K+KMIN
      L=KREV-IDEL
      IF(L.LE.0)GOTO3221
      ILEGTE(KREV)=ILEGTE(L)
 3221 CONTINUE
      GOTO3290
C
 3290 CONTINUE
C
C               **************************************************
C               **  STEP 6--                                    **
C               **  MOVE THE NEW LEGEND   INTO THE APPROPRIATE  **
C               **  PLACE IN ILEGTE(.).                          **
C               **************************************************
C
      ISTEPN='6'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      L=0
      DO4200K=ISTART,ISTOP
      L=L+1
      ILEGTE(K)=ISTH(L)
 4200 CONTINUE
 4290 CONTINUE
C
C               ************************************
C               **  STEP 7--                      **
C               **  REDEFINE NCLEG = THE UPDATED **
C               **  LENGTH OF ILEGTE(.).           **
C               ************************************
C
      ISTEPN='7'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCLEG=N0TEST
C
C               *************************************************
C               **  STEP 8--                                   **
C               **  MAKE THE ADJUSTMENTS TO THE INTERNAL LIST  **
C               *************************************************
C
      ISTEPN='8'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NEWNAM.EQ.'YES')GOTO6100
      GOTO6200
C
 6100 CONTINUE
      ILEGNA(ILISTL)=ILEGN2
      ILEGST(ILISTL)=ISTART
      ILEGSP(ILISTL)=ISTOP
      NUMLEG=NUMLEG+1
      GOTO9000
C
 6200 CONTINUE
      N2OLD=ILEGSP(ILISTL)-ILEGST(ILISTL)+1
      IDEL=N2-N2OLD
C
      DO6210I=1,NUMLEG
      IF(ILEGST(I).GT.ISTART)ILEGST(I)=ILEGST(I)+IDEL
      IF(ILEGSP(I).GE.ISTART)ILEGSP(I)=ILEGSP(I)+IDEL
 6210 CONTINUE
      GOTO9000
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      ISTEPN='9'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGIL.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DPINLE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)N2,N2OLD,IDEL,KMIN,KMAX
 9012 FORMAT('N2,N2OLD,IDEL,KMIN,KMAX = ',5I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NCLEG,MXCLEG,NUMCH0
 9013 FORMAT('NCLEG,MXCLEG,NUMCH0 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)(ILEGTE(I),I=1,NCLEG)
 9014 FORMAT('ILEGTE(.) = ',55A2)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NEWNAM,ILISTL,NUMLEG
 9015 FORMAT('NEWNAM,ILISTL,NUMLEG = ',A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)NEWNAM,NUMLEG
 9021 FORMAT('NEWNAM,NUMLEG = ',A4,3X,I8)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,NUMLEG
      WRITE(ICOUT,9023)I,ILEGNA(I),ILEGST(I),ILEGSP(I)
 9023 FORMAT('I,ILEGNA(I),ILEGST(I),ILEGSP(I) = ',I4,3X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPINPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  MAXNPP,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--FORM AN INTERACTION PLOT, I.E.
C              INTERACTION PLOT Y X1 X2 
C              IS A PLOT OF Y VERSUS X1*X2
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/10
C     ORIGINAL VERSION--OCTOBER   1999.
C     UPDATED         --NOVEMBER  2009. UPDATE PARSING TO USE DPPARS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IERRO2
      CHARACTER*4 IERRO4
C
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
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
      ISUBN1='DPIN'
      ISUBN2='PL  '
      IFOUND='YES'
      IAND2='NO'
      ICASPL='INTE'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXNPP
   53   FORMAT('ICASPL,IAND1,IAND2,MAXNPP = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGG3,IBUGG2,ISUBRO,IFOUND,IERROR
   54   FORMAT('IBUGG3,IBUGG2,ISUBRO,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)GOTO9000
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      INAME='INTERACTION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=MAXSPN
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 16--                                        **
C               **  FORM THE PLOT COORIDINATES                       **
C               *******************************************************
C
      ISTEPN='15.2'
      IF(ISUBRO.EQ.'INPL'.OR.IBUGG3.EQ.'ON')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
 1519 CONTINUE
C
      L=0
      NLOCAL=NRIGHT(1)
C
      DO1520I=1,NLOCAL
        IF(ISUB(I).EQ.0)GOTO1520
        L=L+1
C
        IF(L.GT.MAXNPP)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1521)
 1521     FORMAT('***** ERROR IN INTERACTION PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1523)MAXNPP
 1523     FORMAT('      THE NUMBER OF PLOT POINTS HAS JUST EXCEEDED ',
     1           I8)
          CALL DPWRST('XXX','BUG ')
C
          IF(ISUBRO.EQ.'INPL' .OR. IBUGG2.EQ.'ON')THEN
             WRITE(ICOUT,1525)I,NLOCAL,L,MAXN,MAXNPP,NPLOTP
 1525        FORMAT('I,NLOCAL,L,MAXN,MAXNPP,NPLOTP = ',6I8)
             CALL DPWRST('XXX','BUG ')
          ENDIF
C
          GOTO9000
        ENDIF
C
        IVAV=ICOLR(1)
        IJ=MAXN*(IVAV-1)+I
        IF(IVAV.LE.MAXCOL)Y(L)=V(IJ)
        IF(IVAV.EQ.MAXCP1)Y(L)=PRED(I)
        IF(IVAV.EQ.MAXCP2)Y(L)=RES(I)
        IF(IVAV.EQ.MAXCP3)Y(L)=YPLOT(I)
        IF(IVAV.EQ.MAXCP4)Y(L)=XPLOT(I)
        IF(IVAV.EQ.MAXCP5)Y(L)=X2PLOT(I)
        IF(IVAV.EQ.MAXCP6)Y(L)=TAGPLO(I)
        X(L)=1.0
C
        IF(NUMVAR.GE.2)THEN
          DO1530K=2,NUMVAR
            IVAV=ICOLR(K)
            IJ=MAXN*(IVAV-1)+I
            IF(IVAV.LE.MAXCOL)ATEMP=V(IJ)
            IF(IVAV.EQ.MAXCP1)ATEMP=PRED(I)
            IF(IVAV.EQ.MAXCP2)ATEMP=RES(I)
            IF(IVAV.EQ.MAXCP3)ATEMP=YPLOT(I)
            IF(IVAV.EQ.MAXCP4)ATEMP=XPLOT(I)
            IF(IVAV.EQ.MAXCP5)ATEMP=X2PLOT(I)
            IF(IVAV.EQ.MAXCP6)ATEMP=TAGPLO(I)
            X(L)=X(L)*ATEMP
 1530     CONTINUE
C
          D(L)=1.0
          NPLOTP=L
        ENDIF
C
 1520 CONTINUE
      NPLOTV=2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(ISUBRO.EQ.'INPL' .OR. IBUGG3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,MAXNPP
 9012   FORMAT('IFOUND,IERROR,MAXNPP = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,F15.7,F15.7,F15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINQU(IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SUPPORTS THE FOLLOWING COMMAND:
C
C                 LET IFLAG = INQUIRE <FILE>
C
C              WHERE IFLAG IS SET TO "1" IF FILE EXISTS AND SET TO "0"
C              IF IT DOES NOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/12
C     ORIGINAL VERSION--DECEMBER  2015.
C     UPDATED         --FEBRUARY  2016. SUPPORT QUOTES
C     UPDATED         --JUNE      2016. SUPPORT FOR INTEGER VALUE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOFILE
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
      CHARACTER*4 IFILSV
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*80 IFILE
      CHARACTER*255 ICANS
C
      LOGICAL LEXIST
      LOGICAL LOPEN
C
C ---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
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
      ISUBN0='INQU'
      ISUBN1='DPIN'
      ISUBN2='QU  '
      IFOUND='YES'
      IERROR='NO'
      IEXIST='NO'
      IFILSV=IFILQU
      IFILQU='ON'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINQU--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************
C               **  STEP 1--                                      **
C               **  CHECK FOR VALID COMMAND.                      **
C               ****************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHARG(2).NE.'=' .OR. IHARG(3).NE.'INQU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN INQUIRE COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      INVALID FORM FOR THE COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *********************************************************
C               **  STEP 2--                                            *
C               **  EXAMINE THE LEFT-HAND SIDE--                        *
C               **  IS THE PARAMETER NAME TO LEFT OF = SIGN             *
C               **  ALREADY IN THE NAME LIST?                           *
C               **  NOTE THAT     IHLEFT    IS THE NAME OF THE VARIABLE *
C               **  ON THE LEFT.                                        *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE    *
C               **  OF THE NAME ON THE LEFT.                            *
C               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12) *
C               **  FOR THE NAME OF THE LEFT.                           *
C               *********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO200I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'P')THEN
            ILISTL=I2
            GOTO290
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,201)
  201       FORMAT('      THE NAME ON THE LEFT HAND SIDE WAS FOUND IN')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,202)
  202       FORMAT('      THE NAME TABLE, BUT NOT AS A PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
  200 CONTINUE
C
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,222)
  222   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER NAMES ',
     1         'HAS JUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)MAXNAM
  223   FORMAT('      EXCEEDED THE MAX ALLOWABLE ',I8,'  .  ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  290 CONTINUE
C               ********************************************************
C               **  STEP 3--                                          **
C               **  EXTRACT THE FILE NAME                             **
C               ********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2016/02: DATAPLOT COMMANDS THAT USE FILE NAMES HAVE A COUPLE OF
C              SPECIFIC CRITERION:
C
C                 1. A "." IS REQUIRED IN THE NAME TO IDENTIFY THE
C                    STRING AS A FILE.  THE INQUIRE COMMAND DOES NOT
C                    HAVE THIS RESTRICTION.
C
C                 2. FILE NAMES THAT CONTAIN SPACES OR HYPHENS SHOULD
C                    BE ENCLOSED IN QUOTES.  THIS ALSO APPLIES TO THE
C                    INQUIRE COMMAND.
C
C                 3. DATAPLOT WILL LOOK IN THE DATAPLOT AUXILLARY
C                    DIRECTORIES IF IT DOES FIND THE FILE IN CURRENT
C                    DIRECTORY.  IN ADDITION, DATAPLOT WILL CHECK FOR
C                    ALL LOWER CASE OR ALL UPPER CASE CHARACTERS IN
C                    THE NAME.  THE INQUIRE COMMAND WILL ONLY CHECK THE
C                    FILE NAME AS GIVEN.
C
C     2016/06: CHECK IF THE ARGUMENT TYPE IS NUMERIC.  CAN DO AN INQUIRE
C              OF A UNIT NUMBER.
C
      IWORD=5
      IF(IARGT(IWORD-1).EQ.'NUMB')THEN
        IUNIT=INT(ARG(IWORD-1)+0.1)
        IF(IUNIT.LT.1 .OR. IUNIT.GT.99)THEN
          IEXIST='NO'
        ELSE
          INQUIRE(UNIT=IUNIT,EXIST=LEXIST,OPENED=LOPEN,ACTION=IACC)
          IEXIST='NO'
          IOPEN='NO'
          IF(LEXIST)IEXIST='YES'
          IF(LOPEN)IOPEN='YES'
        ENDIF
        GOTO400
      ENDIF
C
CCCCC CALL DPFILE(IANSLC,IWIDTH,IWORD,
CCCCC1            IOFILE,IBUGA3,ISUBRO,IERROR)
      IOFILE='YES'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,301)IWORD,IWIDTH,IOFILE
  301   FORMAT('IWORD,IWIDTH,IOFILE = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 31--                               **
C               **  IF NO FILE NAME GIVEN,                  **
C               **  THEN GENERATE AN ERROR MESSAGE.         **
C               **********************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)
  312   FORMAT('      NO FILE NAME WAS GIVEN.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************
C               **  STEP 32--                      **
C               **  COPY THE FILE INTO THE STRING  **
C               **  "FILE"                         **
C               *************************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO320I=1,255
        ICANS(I:I)=IANSLC(I)(1:1)
  320 CONTINUE
C
      NCFILE=0
      IFILE=' '
      ISTART=1
      ISTOP=IWIDTH
      IWORD=5
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1            ICOL1,ICOL2,IFILE,NCFILE,
     1            IBUGA3,ISUBRO,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,321)NCFILE,IFILE
  321   FORMAT('NCFILE,IFILE = ',I6,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NCFILE.GE.3)THEN
        IF(IFILE(1:1).EQ.'"' .AND. IFILE(NCFILE:NCFILE).EQ.'"')THEN
          ICNT=0
          DO401J=2,NCFILE-1
            ICNT=ICNT+1
            IFILE(ICNT:ICNT)=IFILE(J:J)
 401      CONTINUE
          NCFILE=NCFILE-2
          IFILE(NCFILE+1:NCFILE+2)=' '
        ENDIF
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NCFILE.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,322)
  322   FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',
     1         'LET ... = INQUIRE ...  COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,324)
  324   FORMAT('      (FOR EXAMPLE,    LET Y = INQUIRE  SAMPLE.TXT)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,325)
  325   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,326)
  326   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,327)(IANSLC(I),I=1,MIN(IWIDTH,100))
  327     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************
C               **  STEP 33--                           **
C               **  INQUIRE ABOUT EXISTENCE OF          **
C               **  SPECIFIED FILE                      **
C               ******************************************
C
      ISTEPN='33'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPINFI(IFILE,IEXIST,IOPEN,IACC,ISUBN0,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************
C               **  STEP 4--                             **
C               **  UPDATE THE LHS PARAMETER             **
C               *******************************************
C
  400 CONTINUE
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IHNAME(ILISTL)=IHLEFT
CCCCC IHNAM2(ILISTL)=IHLEF2
CCCCC IUSE(ILISTL)='P'
CCCCC VALUE(ILISTL)=1.0
CCCCC IF(IEXIST.EQ.'NO')VALUE(ILISTL)=0.0
CCCCC IVALUE(ILISTL)=VALUE(ILISTL)+0.5
CCCCC IN(ILISTL)=1
CCCCC IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
C
      IF(IEXIST.EQ.'NO')THEN
        VALUE0=0.0
      ELSE
        VALUE0=1.0
        IF(IOPEN.EQ.'YES')VALUE0=2.0
      ENDIF
      CALL DPADDP(IHLEFT,IHLEF2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='FILE'
      IH2='ACCE'
      IF(IACC.EQ.'READWRIT')THEN
        VALUE0=1.0
      ELSEIF(IACC.EQ.'READ')THEN
        VALUE0=2.0
      ELSEIF(IACC.EQ.'WRITE')THEN
        VALUE0=3.0
      ELSE
        VALUE0=0.0
      ENDIF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGA3,IERROR)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IEXIST.EQ.'YES')THEN
          WRITE(ICOUT,601)IHLEFT,IHLEF2,IVALUE(ILISTL)
  601     FORMAT('THE SPECIFIED FILE EXISTS, THE PARAMETER ',
     1           A4,A4,' = ',I8)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,602)IHLEFT,IHLEF2,IVALUE(ILISTL)
  602     FORMAT('THE SPECIFIED FILE DOES NOT EXIST, THE PARAMETER ',
     1           A4,A4,' = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      IFILQU=IFILSV
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN 
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINQU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,IEXIST,IOPEN,IACC
 9013   FORMAT('IFOUND,IERROR,IEXIST,IOPEN,IACC = ',4(A4,2X),A8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINT2(MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,
     1                  IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IVARN,IVARN2,NUMVAR,XMIN,XMAX,XINT,
     1                  IFLGFB,IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,
     1                  PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,
     1                  IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
C
C     2015/09: ADD LINES TO ARGUMENT LIST FOR FUNCTION BLOCK
C              AUGMENTATION
C
C     PURPOSE--COMPUTE THE INTEGRAL OF A FUNCTION
C              FROM THE LIMITS XMIN TO XMAX.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --JULY      1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 2015. SUPPORT FUNCTION BLOCKS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 MODEL
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 IFORSW
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IVARN
      CHARACTER*4 IVARN2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IFOUND
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION W,Z
      DOUBLE PRECISION DMIN,DMAX,DNUMSE,DINT,DJ,DELTA2,DMIN2,DMAX2
      DOUBLE PRECISION DB0,DB1,DSUM2,DX,DY,DINT2
C
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION XPLOT(*)
      DIMENSION YPLOT(*)
      DIMENSION X2PLOT(*)
      DIMENSION TAGPLO(*)
      DIMENSION V(*)
C
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE(*)
C
      DIMENSION ILOCV(10)
C
      DIMENSION W(16)
      DIMENSION Z(16)
C
C     2015/08: FUNCTION BLOCK
C
      INCLUDE 'DPCOFB.INC'
C
      CHARACTER*8 IFBNAM
      CHARACTER*8 IFBANS
C
      COMMON/IFBL2/IFLGF2
C
      CHARACTER*4 IFEESV
      COMMON/IFEED/IFEESV
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
      DATA Z(1),Z(2),Z(3),Z(4),Z(5),Z(6),Z(7),Z(8)
     1                   /-0.98940093499165D0,-0.944575023073233D0,
     1-0.865631202387832D0,-0.755404408355003D0,-0.617876244402644D0,
     1-0.458016777657227D0,-0.281603550779259D0,-0.095012509837637D0/
      DATA Z(9),Z(10),Z(11),Z(12),Z(13),Z(14),Z(15),Z(16)
     1/0.095012509837637D0,0.281603550779259D0,0.458016777657227D0,
     10.617876244402644D0,0.755404408355003D0,0.865631202387832D0,
     10.944575023073233D0,0.989400934991650D0/
      DATA W(1),W(2),W(3),W(4),W(5),W(6),W(7),W(8)
     1                  /0.027152459411754D0,0.062253523938648D0,
     10.095158511682493D0,0.124628971255534D0,0.149595988816577D0,
     10.169156519395003D0,0.182603415044924D0,0.189450610455069D0/
      DATA W(9),W(10),W(11),W(12),W(13),W(14),W(15),W(16)
     1/0.189450610455069D0,0.182603415044924D0,0.169156519395003D0,
     10.149595988816577D0,0.124628971255534D0,0.095158511682493D0,
     10.062253523938648D0,0.027152459411754D0/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPIN'
      ISUBN2='T2  '
      IFOUND='NO  '
C
      CUTOFF=0.001
      ACCUR=0.0000001
      MAXSEG=20
      IPASS=2
      J2=0
      IFLGF2=IFLGFB
C
      ABSXIN=0.0
      XINT2=0.0
      DIFF=0.0
      RATIO=0.0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,ISUBRO,IANGLU
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO,IANGLU = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMCHA,NUMPV,NUMVAR,XMIN,XMAX
   53   FORMAT('NUMCHA,NUMPV,NUMVAR,XMIN,XMAX = ',3I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(MODEL(J),J=1,MIN(100,NUMCHA))
   54   FORMAT('MODEL(I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMPV
          WRITE(ICOUT,56)I,PARAM(I),IPARN(I),IPARN2(I)
   56     FORMAT('I,PARAM(I),IPARN(I),IPARN2(I) = ',I8,G15.7,2A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO60I=1,NUMVAR
          WRITE(ICOUT,61)I,IVARN(I),IVARN2(I)
   61     FORMAT('I, IVARN(I) = ',I8,2X,2A4)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
C               ***************************************************
C               **  STEP 1--                                     **
C               **  DETERMINE THE LOCATIONS (IN THE LIST IPARN)  **
C               **  OF THE VARIABLES OF INTEGRATION.             **
C               ***************************************************
C
      IF(IFLGFB.LE.0)THEN
        DO100I=1,NUMVAR
          IH=IVARN(I)
          IH2=IVARN2(I)
          DO200J=1,NUMPV
            J2=J
            IF(IH.EQ.IPARN(J).AND.IH2.EQ.IPARN2(J))GOTO210
  200     CONTINUE
  210     CONTINUE
          ILOCV(I)=J2
  100   CONTINUE
      ENDIF
C
      IFBNAM=' '
      IFBANS=' '
      IF(IFLGFB.EQ.1)THEN
        IFBNAM=IFBNA1
        IFBANS=IFBAN1
        IH=IFBPL1(1)(1:4)
        IH2=IFBPL1(1)(5:8)
      ELSEIF(IFLGFB.EQ.2)THEN
        IFBNAM=IFBNA2
        IFBANS=IFBAN2
        IH=IFBPL2(1)(1:4)
        IH2=IFBPL2(1)(5:8)
      ELSEIF(IFLGFB.EQ.3)THEN
        IFBNAM=IFBNA3
        IFBANS=IFBAN3
        IH=IFBPL3(1)(1:4)
        IH2=IFBPL3(1)(5:8)
      ENDIF
C
C               **************************************************
C               **  STEP 2--                                    **
C               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
C               **************************************************
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,401)
  401   FORMAT('INTEGRAL EVALUATION')
        CALL DPWRST('XXX','BUG ')
        IF(IFLGFB.LE.0)THEN
          WRITE(ICOUT,402)(MODEL(I),I=1,MIN(80,NUMCHA))
  402     FORMAT('      FUNCTION--',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,403)XMIN
  403   FORMAT('      SPECIFIED LOWER LIMIT OF INTEGRAL  = ',F20.10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,404)XMAX
  404   FORMAT('      SPECIFIED UPPER LIMIT OF INTEGRAL  = ',F20.10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,405)NUMVAR
  405   FORMAT('      NUMBER OF VARIABLES OF INTEGRATION = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,406)
  406   FORMAT('NUMBER OF    *       VALUE OF        ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,407)
  407   FORMAT('PARTITIONS   *       INTEGRAL      ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,408)
  408   FORMAT('-------------*--------------------')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  STEP THROUGH 10 DIFFERENT SEGMENTATIONS  **
C               **  OF THE DOMAIN OF THE INTEGRAL.           **
C               ***********************************************
C
 3000 CONTINUE
      DMIN=XMIN
      DMAX=XMAX
      DO3100NUMSEG=1,MAXSEG
C
C               ****************************************************
C               **  STEP 4--                                      **
C               **  WITHIN A GIVEN SEGMENTATION,                  **
C               **  APPLY THE 16-POINT GAUSSIAN QUADRATURE RULE.  **
C               ****************************************************
C
        DNUMSE=NUMSEG
        DELTA2=(DMAX-DMIN)/DNUMSE
        DINT=0.0D0
        DO3200J=1,NUMSEG
        DJ=J
        DMIN2=DMIN+(DJ-1.0D0)*DELTA2
        DMAX2=DMIN+DJ*DELTA2
        DB1=(DMAX2-DMIN2)/2.0D0
        DB0=(DMAX2+DMIN2)/2.0D0
C
        DSUM2=0.0D0
        DO3300I=1,16
          DX=DB0+DB1*Z(I)
          X=DX
C
          IF(IFLGFB.LE.0)THEN
            DO3303K=1,NUMVAR
              JLOC=ILOCV(K)
              PARAM(JLOC)=X
 3303       CONTINUE
            CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1                  IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y,
     1                  IBUGCO,IBUGEV,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
          ELSE
C
C           FUNCTION BLOCK CASE:
C
C           STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
C                   VALUE OF DESIRED PARAMETER)
C
            DO3305II=1,NUMNAM
              IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1           IUSE(II).EQ.'P')THEN
                VALUE(II)=X
                IVALUE(II)=INT(X+0.5)
                GOTO3309
              ENDIF
 3305       CONTINUE
C
C           PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
C           TO NAME LIST
C
            IF(NUMNAM.LT.MAXNAM)THEN
              NUMNAM=NUMNAM+1
              IHNAME(NUMNAM)=IH
              IHNAM2(NUMNAM)=IH2
              IUSE(NUMNAM)='P'
              VALUE(NUMNAM)=X
              IVALUE(NUMNAM)=INT(X+ 0.5)
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3306)
 3306         FORMAT('***** ERROR IN INTEGRATION--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3307)
 3307         FORMAT('      MAXIMUM NUMBER OF NAMES EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
 3309       CONTINUE
C
            IFEEDB='OFF'
            CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                  IBUGA3,IBUGA3,IBUGCO,IBUGEV,IBUGEV,
     1                  ISUBRO,IFOUND,IERROR)
            IFEEDB=IFEESV
C
C           STEP 2: RETRIEVE RESPONSE
C
            DO3320II=1,NUMNAM
              IF(IFBANS(1:4).EQ.IHNAME(II) .AND.
     1           IFBANS(5:8).EQ.IHNAM2(II))THEN
                IF(IUSE(II).EQ.'P')THEN
                  Y=VALUE(II)
                  GOTO3329
                ELSEIF(IUSE(II).EQ.'V')THEN
                  ICOLR=IVALUE(II)
                  IJ=MAXN*(ICOLR-1)+1
                  IF(ICOLR.LE.MAXCOL)Y=V(IJ)
                  IF(ICOLR.EQ.MAXCP1)Y=PRED(1)
                  IF(ICOLR.EQ.MAXCP2)Y=RES(1)
                  IF(ICOLR.EQ.MAXCP3)Y=YPLOT(1)
                  IF(ICOLR.EQ.MAXCP4)Y=XPLOT(1)
                  IF(ICOLR.EQ.MAXCP5)Y=X2PLOT(1)
                  IF(ICOLR.EQ.MAXCP6)Y=TAGPLO(1)
                  GOTO3329
                ENDIF
              ENDIF
 3320       CONTINUE
C
C           PARAMETER/VARIABLE NAME NOT FOUND
C
            WRITE(ICOUT,3306)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3321)
 3321       FORMAT('      EXPECTED PARAMETER/VARIABLE NOT FOUND IN ',
     1           'NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3323)IFBANS
 3323       FORMAT('      EXPECTED NAME = ',A8)
            CALL DPWRST('XXX','BUG ')
C
 3329       CONTINUE
C
          ENDIF
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
            WRITE(ICOUT,3352)X,Y
 3352       FORMAT('X,Y = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          DY=Y
          DSUM2=DSUM2+W(I)*DY
 3300   CONTINUE
        DINT2=DB1*DSUM2
        DINT=DINT+DINT2
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
          WRITE(ICOUT,3361)NUMSEG,J,DSUM2,DB0,DB1,DINT2
 3361     FORMAT('NUMSEG,J,DSUM2,DB0,DB1,DINT2=',2I3,4D12.5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 3200 CONTINUE
C
C               ******************************
C               **  STEP 5--                **
C               **  WRITE OUT THE INTEGRAL  **
C               ******************************
C
        XINT=DINT
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,3503)NUMSEG,XINT
 3503     FORMAT(I8,'     * ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NUMSEG.EQ.1)GOTO3550
          ABSXIN=ABS(XINT)
          DIFF=ABS(XINT-XINT2)
          IF(ABSXIN.LE.CUTOFF.AND.DIFF.LE.ACCUR)GOTO3500
          IF(ABSXIN.LE.CUTOFF.AND.DIFF.GT.ACCUR)GOTO3550
          RATIO=ABS(DIFF/XINT)
          IF(ABSXIN.GT.CUTOFF.AND.RATIO.LE.ACCUR)GOTO3500
          IF(ABSXIN.GT.CUTOFF.AND.RATIO.GT.ACCUR)GOTO3550
 3550   CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
        WRITE(ICOUT,3555)CUTOFF,ACCUR,DIFF,RATIO,ABSXIN
 3555   FORMAT('CUTOFF,ACCUR,DIFF,RATIO,ABSXIN = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      XINT2=XINT
C
 3100 CONTINUE
C
 3500 CONTINUE
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3511)XINT
 3511   FORMAT('INTEGRAL VALUE        = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)CUTOFF,ACCUR,DIFF,RATIO,ABSXIN
 9012   FORMAT('CUTOFF,ACCUR,DIFF,RATIO,ABSXIN = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR
 9014   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINTE(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,TEMP1,ITEMP1,
     1                  IANGLU,IFTEXP,IFTORD,IFORSW,
     1                  IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IERROR)
C
C     PURPOSE--TREAT THE LET CASE FOR
C              FINDING THE DEFINITE INTEGRAL OF AN FUNCTION.
C     EXAMPLE--LET A = INTEGRAL X**3+2*X**2-4*X+5 FOR X = 1 3
C            --LET X = INTEGRAL F1 FOR X = 0 B
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --JULY      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --NOVEMBER  1989. FIX AJUNK & BJUNK DIMENSIONS
C     UPDATED         --JUNE      2013. SUPPORT INDEFINITE INTEGRALS
C                                       (USE QUADPACK ROUTINE QAGI)
C     UPDATED         --SEPTEMBER 2015. SUPPORT FOR FUNCTION BLOCKS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 IHOUT
      CHARACTER*4 IHOUT2
      CHARACTER*4 IUOUT
      CHARACTER*4 IDUMV
      CHARACTER*4 IDUMV2
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
      CHARACTER*4 IHL
      CHARACTER*4 IHL2
      CHARACTER*4 IWD1
      CHARACTER*4 IWD2
      CHARACTER*4 IWD12
      CHARACTER*4 IWD22
      CHARACTER*4 ILAB
      CHARACTER*4 IKEY
      CHARACTER*4 IKEY2
      CHARACTER*4 INCLUN
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEL
      CHARACTER*4 IFOUND
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IERRO2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IOLD
      CHARACTER*4 IOLD2
      CHARACTER*4 INEW
      CHARACTER*4 INEW2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION TEMP1(*)
      DIMENSION ITEMP1(*)
C
C---------------------------------------------------------------------
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
C
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
C
      DIMENSION ILAB(10)
      DIMENSION IOLD(10)
      DIMENSION IOLD2(10)
      DIMENSION INEW(10)
      DIMENSION INEW2(10)
C
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1989
      DIMENSION BJUNK(1)
C
C-----MAKE DUMMY COMMON BLOCK-----------
C
      PARAMETER (IOPTCH=1000)
      PARAMETER (IOPTC2=100)
C
      CHARACTER*4 IBUGAZ
      CHARACTER*4 ZTYPEH
      CHARACTER*4 ZW21HO
      CHARACTER*4 ZW22HO
      CHARACTER*4 ZIPARN
      CHARACTER*4 ZPARN2
      CHARACTER*4 ZMODEL
      CHARACTER*4 ZIDUMV
      CHARACTER*4 ZDUMV2
C
      DIMENSION ZMODEL(IOPTCH)
      DIMENSION ZTYPEH(IOPTCH)
      DIMENSION ZW21HO(IOPTCH)
      DIMENSION ZW22HO(IOPTCH)
      DIMENSION Z2HOLD(IOPTCH)
C
      DIMENSION ZPARAM(IOPTC2)
      DIMENSION ZIPARN(IOPTC2)
      DIMENSION ZPARN2(IOPTC2)
      DIMENSION ZIDUMV(IOPTC2)
      DIMENSION ZDUMV2(IOPTC2)
      DIMENSION LOCDUM(IOPTC2)
C
      COMMON /OPTCMC/ IBUGAZ, ZTYPEH, ZW21HO, ZW22HO, ZIPARN, ZPARN2, 
     &                ZIDUMV, ZDUMV2, ZMODEL
      COMMON /OPTCMR/ ZPARAM, Z2HOLD, 
     &                NUMCHZ, NUMPVZ, NWHOLZ, NUMDVZ, LOCDUM
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFB.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOMC.INC'
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
C               *******************************************
C               **  TREAT THE DEFINITE INTEGRAL SUBCASE  **
C               **  OF THE LET COMMAND                   **
C               *******************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      ISUBN1='DPIN'
      ISUBN2='TE  '
      IHLEFT='UNKN'
      IHLEF2='UNKN'
      IERROR='NO'
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      ILOCMX=0
      NUMLIM=0
      ILOC3=0
      IP=0
      IV=0
      LOCDUM=0
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 2--                                         *
C               **  EXAMINE THE LEFT-HAND SIDE--                     *
C               **  IS THE NAME     NAME TO LEFT OF = SIGN           *
C               **  ALREADY IN THE NAME LIST?                        *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE *
C               **  OF THE NAME ON THE LEFT.                         *
C               ******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO2000I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GOTO2900
        ENDIF
 2000 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN INTEGRAL--')
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
 2205   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND ',
     1         'THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2206)
 2206   FORMAT('      REDEFINE (REUSE) SOME OF THE ALREADY USED NAMES')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2900 CONTINUE
C
C               ************************************************************
C               **  STEP 3.1--                                            **
C               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL EXPRESSION FROM THE **
C               **  INPUT COMMAND LINE (STARTING WITH THE FIRST NON-BLANK **
C               **  LOCATION AFTER THE EQUAL SIGN AND ENDING WITH THE END **
C               **  OF THE LINE OR WITH THE LAST NON-BLANK CHARACTER      **
C               **  BEFORE     WRT  .  PLACE THE FUNCTION IN IFUNC2(.) .  **
C               ************************************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2015/09: CHECK TO SEE IF THE FIRST ARGUMENT ON RHS IS A FUNCTION
C              BLOCK NAME.
C
      IF(IHARG(4).EQ.IFBNA1(1:4) .AND. IHARG2(4).EQ.IFBNA1(5:8))THEN
        IFLGFB=1
      ELSEIF(IHARG(4).EQ.IFBNA2(1:4) .AND. IHARG2(4).EQ.IFBNA2(5:8))THEN
        IFLGFB=2
      ELSEIF(IHARG(4).EQ.IFBNA3(1:4) .AND. IHARG2(4).EQ.IFBNA3(5:8))THEN
        IFLGFB=3
      ELSE
        IFLGFB=0
      ENDIF
C
C
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1            IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3500
C
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='FOR '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1            IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3500
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3102)
 3102 FORMAT('      INVALID COMMAND FORM FOR INTEGRATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
 3103 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
 3104 FORMAT('      LET ... = INTEGRAL ... WRT  ... ',
     1       'FOR ... = ... TO ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3105)
 3105 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3106)(IANS(I),I=1,MIN(IWIDTH,100))
 3106   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 3500 CONTINUE
C
C               *********************************************************
C               **  STEP 4--                                            *
C               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES  *
C               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES        *
C               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY    *
C               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED *
C               **  AND THE EXPRESSION IS LEFT ONLY WITH CONSTANTS,     *
C               **  PARAMETERS, AND VARIABLES--NO FUNCTIONS.  PLACE THE *
C               **  RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.)      *
C               *********************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLGFB.LT.1)THEN
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,
     1              IFUNC3,N3,MAXN3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          ILAB(1)='INPU'
          ILAB(2)='T FU'
          ILAB(3)='NCTI'
          ILAB(4)='ON  '
          ILAB(5)='    '
          ILAB(6)='  = '
          NUMWDL=6
          CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
          WRITE(ICOUT,5081)IDUMV(1),IDUMV2(1)
 5081     FORMAT('INTEGRATION VARIABLE  = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C               *************************************
C               **  STEP 5--                       **
C               **  EXTRACT QUALIFIER INFORMATION. **
C               *************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *********************************************************
C               **  STEP 5.1--                                         **
C               **  DETERMINE THE DUMMY VARIABLE FOR THE INTEGRATION.  **
C               *********************************************************
C
      IKEY='WRT '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO' .OR. IFOUN2.EQ.'NO')THEN
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        ILOCA=1
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1              IHARG,IHARG2,NUMARG,
     1              INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1              IUSE,IN,NUMNAM,
     1              IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1              IVOUT,VOUT,IUOUT,
     1              INOUT,IBUGA3,IERROR)
        IF(IFOUN1.EQ.'NO' .OR. IFOUN2.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5182)
 5182     FORMAT('      INVALID COMMAND FORM FOR INTEGRATION.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5183)
 5183     FORMAT('      NO VARIABLE OF INTEGRATION DEFINED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3103)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3104)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3105)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3106)(IANS(I),I=1,MIN(IWIDTH,100))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ELSE
          IDUMV(1)=IHOUT
          IDUMV2(1)=IHOUT2
          NUMDV=1
        ENDIF
      ELSE
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
      ENDIF
C
C               **************************************************
C               **  STEP 5.2--                                  **
C               **  DETERMINE THE LIMITS FOR   THE INTEGRATION. **
C               **************************************************
C
      ISTEPN='5.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMLIM=0
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=3
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,
     1            VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
        XMIN=VOUT
        NUMLIM=NUMLIM+1
      ENDIF
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=4
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,
     1            VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
        IF(IHOUT.EQ.'TO  ' .AND. IHOUT2.EQ.'    ')THEN
          CONTINUE
        ELSE
          XMAX=VOUT
          ILOCMX=ILOC2
          NUMLIM=NUMLIM+1
        ENDIF
      ENDIF
C
      IF(NUMLIM.LE.1)THEN
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=5
        ILOCA=1
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1              IHARG,IHARG2,NUMARG,
     1              INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1              IUSE,IN,NUMNAM,
     1              IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,
     1              ILOUT,IVOUT,VOUT,IUOUT,
     1              INOUT,IBUGA3,IERROR)
        IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
          XMAX=VOUT
          ILOCMX=ILOC2
          NUMLIM=NUMLIM+1
        ENDIF
      ENDIF
C
      IF(NUMLIM.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5182)
        CALL DPWRST('XXX','BUG ')
        IF(NUMLIM.EQ.0)THEN
          WRITE(ICOUT,5283)
 5283     FORMAT('      NO LIMITS OF INTEGRATION DEFINED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NUMLIM.EQ.1)THEN
          WRITE(ICOUT,5284)
 5284     FORMAT('      ONLY ONE LIMIT OF INTEGRATION DEFINED.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,5285)NUMLIM
 5285     FORMAT('      NUMBER OF LIMITS DEFINED = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,3103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3104)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3105)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3106)(IANS(I),I=1,MIN(IWIDTH,100))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **********************************************
C               **  STEP 6.3--                              **
C               **  SCAN THE QUALIFIERS FOR VARIABLE,       **
C               **  PARAMETER, FUNCTION, AND VALUE CHANGES  **
C               **  IN THE FUNCTION.                        **
C               **********************************************
C
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCHANG=0
      DO6300IFORI=1,10
C
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        IF(IFORI.EQ.1)ILOCA=ILOCMX
        IF(IFORI.NE.1)ILOCA=ILOC3
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1              IHARG,IHARG2,NUMARG,
     1              INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1              IUSE,IN,NUMNAM,
     1              IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,
     1              VOUT,IUOUT,
     1              INOUT,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO6380
        IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO6390
C
        ILOC3=ILOC2+2
        IF(ILOC3.GT.NUMARG)GOTO6380
        NCHANG=NCHANG+1
        IOLD(NCHANG)=IHARG(ILOC2)
        IOLD2(NCHANG)=IHARG2(ILOC2)
        INEW(NCHANG)=IHARG(ILOC3)
        INEW2(NCHANG)=IHARG2(ILOC3)
C
 6300 CONTINUE
      GOTO6390
C
 6380 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5182)
 6302 FORMAT('      INVALID COMMAND FORM FOR INTEGRATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3105)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3106)(IANS(I),I=1,MIN(IWIDTH,100))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 6390 CONTINUE
C
C               **********************************************
C               **  STEP 6.4--                              **
C               **  CARRY OUT THE VARIABLE,                 **
C               **  PARAMETER, AND FUNCTION CHANGES         **
C               **  AND THEN PRINT OUT A BRIEF MESSAGE      **
C               **  INDICATING THAT THE CHANGES             **
C               **  HAVE BEEN MADE.                         **
C               **********************************************
C
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON' .AND. NCHANG.GT.0 .AND.
     1   IFLGFB.LE.0)THEN
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        ILAB(1)='PRE '
        ILAB(2)='-CHA'
        ILAB(3)='NGE '
        ILAB(4)='FUNC'
        ILAB(5)='TION'
        ILAB(6)='  = '
        NUMWDL=6
        CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
C
        CALL COMPIC(IFUNC3,N3,IOLD,IOLD2,INEW,INEW2,NCHANG,IFUNC3,N3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        ILAB(1)='POST'
        ILAB(2)='-CHA'
        ILAB(3)='NGE '
        ILAB(4)='FUNC'
        ILAB(5)='TION'
        ILAB(6)='  = '
        NUMWDL=6
        CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
C
      ENDIF
C
C               *********************************************************
C               **  STEP 6.7--                                         **
C               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION        **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES. **
C               *********************************************************
C
      ISTEPN='6.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
      IPASS=1
      IF(IFLGFB.LE.0)THEN
        CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1              IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSE
        GOTO7650
      ENDIF
C
C               ***********************************************
C               **  STEP 7--                                 **
C               **  CHECK THAT ALL PARAMETERS                **
C               **  IN THE FUNCTION ARE ALREADY PRESENT      **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               ***********************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      IF(NUMPV.LE.0)GOTO7650
      DO7600J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))THEN
           IV=IV+1
           LOCDUM=J
           GOTO7600
        ENDIF
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHPARN,IHPAR2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'YES')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7612)
 7612     FORMAT('      A PARAMETER/FUNCTION HAS BEEN ENCOUNTERED IN ',
     1           'THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7613)
 7613     FORMAT('      FUNCTION TO BE INTEGRATED WHICH HAS NOT YET ',
     1           'BEEN DEFINED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7615)IHPARN,IHPAR2
 7615     FORMAT('      THE UNKNOWN PARAMETER/FUNCTION = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3105)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3106)(IANS(I),I=1,MIN(IWIDTH,100))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IP=IP+1
        PARAM(J)=VALUE(ILOCP)
C
 7600 CONTINUE
 7650 CONTINUE
C
C               ******************************
C               **  STEP 8--                **
C               **  DETERMINE THE INTEGRAL  **
C               ******************************
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7711)
 7711   FORMAT('***** FROM DPINTE, IMMEDIATELY BEFORE CALLING DPINT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7712)N3,NUMPV
 7712   FORMAT('N3,NUMPV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7713)NUMDV,XMIN,XMAX,XINT
 7713   FORMAT('NUMDV,XMIN,XMAX,XINT = ',I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO7714I=1,NUMDV
          WRITE(ICOUT,7715)I,IDUMV(I),IDUMV2(I)
 7715     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
 7714   CONTINUE
      ENDIF
C
C     2013/06: CALL QUADPACK ROUTINE "QAGI" IF AN INDEFINITE INTEGRAL
C              DETECTED.
C
      IF(XMIN.EQ.CPUMIN .OR. XMAX.EQ.CPUMAX)THEN
C
C       COPY OVER DUMMY COMMON BLOCKS FOR OPTFUN ROUTINE
C
        DO7805KK=1,MAXF3
          ZMODEL(KK)=IFUNC3(KK)
 7805   CONTINUE
        DO7810KK=1,IOPTCH
          ZTYPEH(KK)=ITYPEH(KK)
          ZW21HO(KK)=IW21HO(KK)
          ZW22HO(KK)=IW22HO(KK)
          Z2HOLD(KK)=W2HOLD(KK)
 7810   CONTINUE
        DO7820KK=1,IOPTC2
          ZPARAM(KK)=PARAM(KK)
          ZIPARN(KK)=IPARN(KK)
          ZPARN2(KK)=IPARN2(KK)
          ZIDUMV(KK)=IDUMV(KK)
          ZDUMV2(KK)=IDUMV2(KK)
 7820   CONTINUE
        NUMCHZ=N3
        NUMPVZ=NUMPV
        NWHOLZ=NWHOLD
        NUMDVZ=NUMDV
        IBUGAZ=IBUGA3
C
        IF(XMIN.EQ.CPUMIN .AND. XMXAX.EQ.CPUMAX)THEN
          INF=2
          BOUND=XMIN
        ELSEIF(XMIN.EQ.CPUMIN)THEN
          INF=-1
          BOUND=XMAX
        ELSE
          INF=1
          BOUND=XMIN
        ENDIF
        EPSABS=0.0
        EPSREL=1.0E-7
        AVAL=50.0*R1MACH(4)
        IF(EPSREL.LT.AVAL)THEN
          EPSREL=1.0E-04
        ENDIF
        IER=0
        XINT=0.0
        LIMIT=500
        LENW=4*LIMIT
C
        CALL QAGI(BOUND,INF,EPSABS,EPSREL,XINT,ABSERR,NEVAL,
     1            IER,LIMIT,LENW,LAST,ITEMP1,TEMP1)
C
        IF(IER.EQ.0 .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8091)XINT
 8091     FORMAT('      INDEFINITE INTERGRAL RESULT  = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8093)ABSERR
 8093     FORMAT('      ABSOLUTE ERROR              = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8095)NEVAL
 8095     FORMAT('      NUMBER OF EVALUATIONS       = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IER.GE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ENDIF
C
        IF(IER.EQ.1)THEN
          WRITE(ICOUT,8103)
 8103     FORMAT('      QAGI: MAXIMUM AKMBER OF SUBDIVISIONS EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.2)THEN
          WRITE(ICOUT,8105)
 8105     FORMAT('      QAGI: ROUNDOFF ERROR PREVENTS REQUESTED ',
     1           'TOLERANCE FROM BEING ACHIEVED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.3)THEN
          WRITE(ICOUT,8107)
 8107     FORMAT('      QAGI: BAD INTEGRAND BEHAVIOUR DETECTED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.4)THEN
          WRITE(ICOUT,8109)
 8109     FORMAT('      QAGI: INTEGRATION DID NOT CONVERGE.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.5)THEN
          WRITE(ICOUT,8111)
 8111     FORMAT('      QAIG: THE INTEGRATION IS PROBABLY DIVERGENT.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.6)THEN
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8113)
 8113     FORMAT('      QAGI: INVALID INPUT TO THE INTEGRATION ',
     1           'ROUTINE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ELSE
        CALL DPINT2(IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1              IDUMV,IDUMV2,NUMDV,XMIN,XMAX,XINT,
     1              IFLGFB,IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1              NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,
     1              PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,
     1              IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
      ENDIF
C
C               *****************************************************
C               **  STEP 9--                                       **
C               **  ENTER THE INTEGRATION VALUE INTO THE DATAPLOT  **
C               **  HOUSEKEEPING ARRAY                             **
C               *****************************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHL=IHLEFT
      IHL2=IHLEF2
      ICASEL='P'
      IXINT=XINT+0.5
CCCCC THE FOLLOWING 2 LINES WERE ADDED NOVEMBER 1989
      BJUNK(1)=AJUNK
      NJUNK=1
CCCCC THE FOLLOWING LINE WAS CHANGED NOVEMBER 1989
CCCCC CALL DPINVP(IHL,IHL2,ICASEL,AJUNK,NJUNK,XINT,IXINT,
      CALL DPINVP(IHL,IHL2,ICASEL,BJUNK,NJUNK,XINT,IXINT,
     1            ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPINTE--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1           I8,2X,A4,A4,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9017)NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV
 9017   FORMAT('NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(IFUNC(I),I=1,MIN(115,IWIDTH))
 9018   FORMAT('IFUNC(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)(IFUNC2(I),I=1,MIN(115,N2))
 9019   FORMAT('IFUNC2(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)(IFUNC3(I),I=1,MIN(115,N3))
 9021   FORMAT('IFUNC3(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IP,IV,IDUMV(1),IDUMV2(1),LOCDUM
 9023   FORMAT('IP,IV,IDUMV(1),IDUMV2(1),LOCDUM = ',2I8,2X,A4,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)IHLEFT,IHLEF2,ICASEL,IFOUND,IERROR
 9025   FORMAT('IHLEFT,IHLEF2,ICASEL,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)XMIN,XMAX,XINT
 9026   FORMAT('XMIN,XMAX,XINT = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINVP(IHLEFT,IHLEF2,ICASEL,VLEFT,NLEFT,PLEFT,ILEFT,
     1ISUBN3,ISUBN4,IBUGA3,IERROR)
C
C     PURPOSE--INSERT THE VARIABLE OR PARAMETER
C              WITH NAME   IHLEFT
C              INTO THE INTERNAL DATAPLOT TABLE.
C              ALSO, UPDATE INTERNAL DATAPLOT
C              LISTS (IF NECESSARY).
C
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ICASEL
      CHARACTER*4 ISUBN3
      CHARACTER*4 ISUBN4
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION VLEFT(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
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
      ISUBN1='DPIN'
      ISUBN2='VP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
C
      IERROR='NO'
C
      ICOLL=0
C
C               ******************************************
C               **  INSERT A VARIABLE                   **
C               **  INTO THE GENERAL DATAPLOT           **
C               **  ARRAY V(.)  ; OR                    **
C               **  INSERT A PARAMETER VALUE            **
C               **  INTO THE INTERNAL DATAPLOT TABLE.   **
C               **  MAKE ADJUSTMENTS TO THE             **
C               **  INTERNAL DATAPLOT LISTS.            **
C               ******************************************
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPINVP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT
   52 FORMAT('IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT = ',
     1A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)VLEFT(1),VLEFT(NLEFT)
   53 FORMAT('VLEFT(1),VLEFT(NLEFT) = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMNAM,MAXNAM,NUMCOL,MAXN,MAXCOL
   54 FORMAT('NUMNAM,MAXNAM,NUMCOL,MAXN,MAXCOL = ',5I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  DETERMINE WHETHER OR NOT THE NAME IN IHLEFT      **
C               **  ALREADY EXISTS IN THE INTERNAL IHNAME(.) TABLE.  **
C               **  THE 'YES' OR 'NO' RESULT IS PLACED IN    NEWNAM. **
C               **  THE LINE IN THE TABLE IS PLACED INTO ILISTL.     **
C               **  DETERMINE ALSO IF THE NUMBER OF NAMES            **
C               **  IN THE IHNAME(.) TABLE EXCEEDS THE               **
C               **  MAXIMUM ALLOWABLE NUMBER (MAXNAM).               **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2000I=1,NUMNAM
      I2=I
      IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))GOTO2030
 2000 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      GOTO2050
 2030 CONTINUE
      NEWNAM='NO'
      ILISTL=I2
 2050 CONTINUE
C
      IF(ILISTL.LE.MAXNAM)GOTO2090
      WRITE(ICOUT,2051)ISUBN1,ISUBN2,ISUBN3,ISUBN4
 2051 FORMAT('***** ERROR IN ',A4,A4,'AS CALLED FROM ',A4,A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2052)
 2052 FORMAT('      THE NUMBER OF VARIABLE/PARAMETER',
     1'/FUNCTION NAMES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2053)MAXNAM
 2053 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE (= ',
     1I8,')   .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2054)
 2054 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2055)
 2055 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2056)
 2056 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2057)
 2057 FORMAT('      AND THEN REUSE SOME NAME.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2058)
 2058 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,2059)(IANS(I),I=1,IWIDTH)
 2059 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2090 CONTINUE
C
C               ***************************************
C               **  STEP 3--                         **
C               **  IF OUTPUT IS TO BE A VARIABLE,   **
C               **  DETERMINE WHAT COLUMN IN V(.)    **
C               **  THE OUTPUT WILL GO.              **
C               **  THE RESULT WILL BE PLACED        **
C               **  INTO  ICOLL    .                 **
C               ***************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.NE.'V')GOTO3099
C
      IF(NEWNAM.EQ.'YES')NEWCOL='YES'
      IF(NEWNAM.EQ.'YES')ICOLL=NUMCOL+1
C
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).NE.'V')NEWCOL='YES'
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).NE.'V')ICOLL=NUMCOL+1
C
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).EQ.'V')NEWCOL='NO'
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).EQ.'V')ICOLL=IVALUE(ILISTL)
C
 3099 CONTINUE
C
C               *****************************************
C               **  STEP 4--                           **
C               **  DETERMINE IF THE COLUMN IN V(.)    **
C               **  WOULD EXCEED THE MAX ALLOWABLE     **
C               **  NUMBER OF COLUMNS.                 **
C               *****************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.NE.'V')GOTO4099
      IF(ICASEL.EQ.'V'.AND.ICOLL.LE.MAXCOL)GOTO4099
C
      WRITE(ICOUT,4051)ISUBN1,ISUBN2,ISUBN3,ISUBN4
 4051 FORMAT('***** ERROR IN ',A4,A4,'AS CALLED FROM ',A4,A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4052)
 4052 FORMAT('      THE NUMBER OF DATA COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4053)MAXCOL
 4053 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE ',I8,'  .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4054)
 4054 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4055)
 4055 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4056)
 4056 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4057)
 4057 FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4058)
 4058 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,4059)(IANS(I),I=1,IWIDTH)
 4059 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 4099 CONTINUE
C
C               *******************************************
C               **  STEP 5--                             **
C               **  IF OUTPUT IS TO BE A VARIABLE,       **
C               **  ENTER THE CONTENTS OF VLEFT(.)       **
C               **  (ALL NLEFT ELEMENTS OF VLEFT(.))     **
C               **  INTO COLUMN     ICOLL    OF V(.)  .  **
C               *******************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.NE.'V')GOTO5099
      IF(NLEFT.LE.0)GOTO5099
      IF(NLEFT.LE.MAXN)GOTO5039
C
      WRITE(ICOUT,5021)ISUBN1,ISUBN2,ISUBN3,ISUBN4
 5021 FORMAT('***** ERROR IN ',A4,A4,'AS CALLED FROM ',A4,A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5022)NLEFT
 5022 FORMAT('      THE NUMBER (= ',I8,') OF ELEMENTS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5023)IHLEFT,IHLEF2
 5023 FORMAT('      FOR VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5024)MAXN
 5024 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE ',I8,'  .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5025)
 5025 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5026)
 5026 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5027)
 5027 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5028)
 5028 FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5029)
 5029 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,5030)(IANS(I),I=1,IWIDTH)
 5030 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5039 CONTINUE
C
      DO5070I=1,NLEFT
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)V(IJ)=VLEFT(I)
      IF(ICOLL.EQ.MAXCP1)PRED(I)=VLEFT(I)
      IF(ICOLL.EQ.MAXCP2)RES(I)=VLEFT(I)
 5070 CONTINUE
C
 5099 CONTINUE
C
C               *******************************************
C               **  STEP 7--                             **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING    **
C               *******************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.EQ.'P')GOTO7010
      IF(ICASEL.EQ.'V')GOTO7020
      GOTO9000
C
 7010 CONTINUE
      IHNAME(ILISTL)=IHLEFT
      IHNAM2(ILISTL)=IHLEF2
      VALUE(ILISTL)=PLEFT
      IVALUE(ILISTL)=ILEFT
      IN(ILISTL)=ILEFT
      IUSE(ILISTL)='P'
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
C
      IF(IFEEDB.EQ.'OFF')GOTO7019
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7011)IHLEFT,IHLEF2,VALUE(ILISTL)
 7011 FORMAT('THE COMPUTED VALUE OF THE CONSTANT   ',A4,A4,
     1' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 7019 CONTINUE
      GOTO7190
C
 7020 CONTINUE
      IHNAME(ILISTL)=IHLEFT
      IHNAM2(ILISTL)=IHLEF2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NLEFT
C
CCCCC IUSE(ICOLL)='V'
CCCCC IVALUE(ICOLL)=ICOLL
CCCCC VALUE(ICOLL)=ICOLL
CCCCC IN(ICOLL)=NLEFT
C
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWCOL.EQ.'YES')NUMCOL=NUMCOL+1
C
      DO7100I=1,NUMNAM
      I2=I
      IF(IUSE(I).EQ.'V'.AND.IVALUE(I).EQ.ICOLL)GOTO7105
      GOTO7100
 7105 CONTINUE
      IUSE(I2)='V'
      IVALUE(I2)=ICOLL
      VALUE(I2)=ICOLL
      IN(I2)=NLEFT
 7100 CONTINUE
C
      NS=NLEFT
      IF(IFEEDB.EQ.'OFF')GOTO7119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7111)IHLEFT,IHLEF2,NS
 7111 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
 7119 CONTINUE
C
      IROW1=1
      IROWN=NLEFT
C
      IF(IFEEDB.EQ.'OFF')GOTO7149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IJ1=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)WRITE(ICOUT,7121)IHLEFT,IHLEF2,V(IJ1),
     1IROW1
 7121 FORMAT('THE FIRST          COMPUTED VALUE OF ',A4,A4,
     1' = ',E15.7,' (ROW ',I5,')')
      IF(ICOLL.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP1)WRITE(ICOUT,7121)IHLEFT,IHLEF2,PRED(IROW1),
     1IROW1
      IF(ICOLL.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP2)WRITE(ICOUT,7121)IHLEFT,IHLEF2,RES(IROW1),
     1IROW1
      IF(ICOLL.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IJN=MAXN*(ICOLL-1)+IROWN
      IF(ICOLL.LE.MAXCOL.AND.
     1NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,V(IJN),IROWN
      IF(ICOLL.LE.MAXCOL.AND.
     1NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP1.AND.
     1NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,PRED(IROWN),IROWN
      IF(ICOLL.EQ.MAXCP1.AND.
     1NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP2.AND.
     1NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,RES(IROWN),IROWN
 7131 FORMAT('THE LAST (',I5,'TH) COMPUTED VALUE OF ',A4,A4,
     1' = ',E15.7,' (ROW ',I5,')')
      IF(ICOLL.EQ.MAXCP2.AND.
     1NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(NS.NE.1)GOTO7180
C
      WRITE(ICOUT,7142)
 7142 FORMAT('NOTE--THE ABOVE VALUE WAS THE ONLY VALUE COMPUTED ',
     1'FOR THIS VARIABLE.')
      CALL DPWRST('XXX','BUG ')
 7149 CONTINUE
 7180 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO7189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7182)IHLEFT,IHLEF2,ICOLL
 7182 FORMAT('THE CURRENT COLUMN FOR ',
     1'THE VARIABLE ',A4,A4,'  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7183)IHLEFT,IHLEF2,NLEFT
 7183 FORMAT('THE CURRENT LENGTH OF  ',
     1'THE VARIABLE ',A4,A4,'  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 7189 CONTINUE
C
 7190 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DPINVP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT,ILEFT
 9012 FORMAT('IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT,ILEFT = ',
     1A4,A4,2X,A4,I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)VLEFT(1),VLEFT(NLEFT)
 9013 FORMAT('VLEFT(1),VLEFT(NLEFT) = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NEWNAM,ILISTL,ICOLL,NUMNAM
 9015 FORMAT('NEWNAM,ILISTL,ICOLL,NUMNAM = ',A4,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IHNAME(ILISTL),IHNAM2(ILISTL),IVALUE(ILISTL),
     1VALUE(ILISTL)
 9016 FORMAT('IHNAME(ILISTL),IHNAM2(ILISTL),IVALUE(ILISTL),',
     1'VALUE(ILISTL) = ',A4,A4,2X,I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IUSE(ILISTL),IN(ILISTL)
 9017 FORMAT('IUSE(ILISTL),IN(ILISTL) = ',A4,2X,I8)
      CALL DPWRST('XXX','BUG ')
      IJ1=MAXN*(ICOLL-1)+1
      IJN=MAXN*(ICOLL-1)+NLEFT
      WRITE(ICOUT,9018)IJ1,IJN,V(IJ1),V(IJN)
 9018 FORMAT('IJ1,IJN,V(IJ1),V(IJN) = ',2I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPISOP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A Z-SCORE VERSUS LAB AVERAGE PLOT AS GIVEN
C              IN THE ISO 13528 STANDARD.
C
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C
C                  ISO 13528 PLOT Y Z ROUND LABID LAB
C
C              WHERE Y IS THE ORIGINAL RESPONSE, Z IS THE Z-SCORE OF THE
C              RESPONSE, ROUND IS THE ROUND-ID, LABID IS THE LAB-ID FOR
C              ALL LABS, AND LAB IDENTIFIES THE LABS FOR WHICH THE PLOT
C              WILL BE GENERATED (TYPICALLY, THIS WILL BE A SINGLE LAB).
C
C              IN SOME CASES, ONLY THE Z-SCORES WILL BE AVAILABLE.
C              IN THIS CASE, Y WILL DENOTE THE LAB AVERAGES IN THE
C              ORIGINAL UNITS.
C
C              THE PLOT IS:
C
C                  VERTICAL AXIS: FOR A GIVEN LAB, THE Z-SCORES FOR EACH
C                                 ROUND.
C                  HORIZONRAL AXIS: FOR EACH ROUND, COMPUTE THE AVERAGE
C                                   OVER ALL LABORATORIES.
C
C              YOU CAN OPTIONALLY PROVIDE A MATERIAL-ID VARIABLE.
C              THIS IS ESSENTIALLY A HIGHLIGHTING VARIABLE (I.E.,
C              DIFFERENT MATERIALS CAN BE PLOTTED WITH DIFFERENT
C              PLOT CHARACTERS).  THIS FORM HAS THE SYNTAX
C
C                  ISO 13528 PLOT Y Z ROUND LABID MATID LAB
C
C     EXAMPLE--ISO 13528 PLOT Y Z LAB LABA
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/2
C     ORIGINAL VERSION--FEBRUARY   2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IH
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=10)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Z(MAXOBV)
      DIMENSION YRAW(MAXOBV)
      DIMENSION ROUND(MAXOBV)
      DIMENSION ALABID(MAXOBV)
      DIMENSION ALAB(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION PPA0(MAXOBV)
      DIMENSION PPA1(MAXOBV)
      DIMENSION PPA0SD(MAXOBV)
      DIMENSION PPA1SD(MAXOBV)
      DIMENSION AMATID(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Z(1))
      EQUIVALENCE (GARBAG(IGARB2),YRAW(1))
      EQUIVALENCE (GARBAG(IGARB3),ROUND(1))
      EQUIVALENCE (GARBAG(IGARB4),ALABID(1))
      EQUIVALENCE (GARBAG(IGARB5),ALAB(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB8),PPA0(1))
      EQUIVALENCE (GARBAG(IGARB9),PPA1(1))
      EQUIVALENCE (GARBAG(IGAR10),PPA0SD(1))
      EQUIVALENCE (GARBAG(JGAR11),PPA1SD(1))
      EQUIVALENCE (GARBAG(JGAR12),AMATID(1))
      EQUIVALENCE (GARBAG(JGAR13),TEMP3(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP5(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMP6(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
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
      IERROR='NO'
      IFOUND='NO'
C
      ISUBN1='DPIS'
      ISUBN2='OP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE DEX CONTOUR PLOT CASE   **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPISOP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2 .AND. ICOM.EQ.'ISO ' .AND.IHARG(1).EQ.'1352' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        ICASPL='1352'
      ELSE
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='ISO 13528 PLOT'
      MINNA=4
      MAXNA=100
      MINN2=2
      IFLAGE=98
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=5
      MAXNVA=6
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 33--                               **
C               **  FORM THE SUBSETTED VARIABLES            **
C               **       Y(.)                               **
C               **       Z(.)                               **
C               **       ROUND(.)                           **
C               **       ALABID(.)                          **
C               **       AMATID(.)                          **
C               **  CONTAINING                              **
C               **       THE RESPONSE VARIABLE (ORIGINAL    **
C               **           UNITS)                         **
C               **       THE Z-SCORE OF THE RESPONSE        **
C               **       THE ROUND-ID                       **
C               **       THE LAB-ID                         **
C               **       THE MATERIAL-ID                    **
C               **  RESPECTIVELY.                           **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NRIGHT(1).EQ.NRIGHT(2))THEN
        ICOL=1
        NUMVA2=NUMVAR-1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YRAW,Z,ROUND,ALABID,AMATID,TEMP1,TEMP1,NS,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NAVE=NS
      ELSE
        ICOL=2
        NUMVA2=NUMVAR-2
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Z,ROUND,ALABID,TEMP1,TEMP1,TEMP1,TEMP1,NS,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        ICOL=1
        NUMVA2=1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YRAW,TEMP1,TEMP1,TEMP1,TEMP1,TEMP1,TEMP1,NAVE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(NUMVAR.EQ.5)THEN
        DO3310I=1,NS
          AMATID(I)=1.0
 3310   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 34--                               **
C               **  FORM THE FULL VARIABLE                  **
C               **       ALAB(.)                            **
C               **  CONTAINING THE VALUES OF THE LABS FOR   **
C               **  WHICH THE PLOT WILL BE GENERATED.       **
C               **********************************************
C
      ISTEPN='34'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=NUMVAR
      NUMVA2=1
      NQ=NRIGHT(ICOL)
      DO3410I=1,NQ
        ISUB(I)=1.0
 3410 CONTINUE
C
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            ALAB,TEMP1,TEMP1,NLAB,NLAB,NLAB,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************************
C               **  STEP 8--                                         **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
C               *******************************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,5001)NS,NAVE,NLAB,ICASPL
 5001   FORMAT('NS,NAVE,NLAB,ICASPL=',3I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPISO2(YRAW,Z,ROUND,ALABID,AMATID,ALAB,NS,NLAB,NAVE,
     1            ICASPL,NUMVAR,MAXOBV,IISOLA,IISOME,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1            PPA0,PPA1,PPA0SD,PPA1SD,
     1            Y,X,D,
     1            NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 9--                         **
C               **  GENERATE FIT FOR EACH LAB IN     **
C               **  ALAB VARIABLE.                   **
C               ***************************************
C
 7000 CONTINUE
C
      ISTEPN='9'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPISO4(PPA0,PPA1,PPA0SD,PPA1SD,NLAB,
     1            IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPISOP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPISO2(YRAW,Z,ROUND,ALABID,AMATID,ALAB,N,NLAB,NAVE,
     1                  ICASPL,NUMVAR,MAXOBV,IISOLA,IISOME,
     1                  YMEAN,XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,
     1                  PPA0,PPA1,PPA0SD,PPA1SD,
     1                  Y,X,D,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A Z-SCORE VERSUS LAB AVERAGE PLOT AS GIVEN
C              IN THE ISO 13528 STANDARD.
C
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C
C                  ISO 13528 PLOT Y Z ROUND LABID MATID LAB
C
C              WHERE Y IS THE ORIGINAL RESPONSE, Z IS THE Z-SCORE OF THE
C              RESPONSE, ROUND IS THE ROUND-ID, LABID IS THE LAB-ID FOR
C              ALL LABS, MATID IS THE MATERIAL ID, AND LAB IDENTIFIES
C              THE LABS FOR WHICH THE PLOT WILL BE GENERATED (TYPICALLY,
C              THIS WILL BE A SINGLE LAB).
C
C              THE PLOT IS:
C
C                  VERTICAL AXIS: FOR A GIVEN LAB, THE Z-SCORES FOR EACH
C                                 ROUND.
C                  HORIZONRAL AXIS: FOR EACH ROUND, COMPUTE THE AVERAGE
C                                   OVER ALL LABORATORIES.
C
C     REFERENCE--ISO 13528 (2005), "Statistical Methods for use in
C                proficiency testing by interlaboratory comparisons,"
C                First Edition, 2005-09-01, pp. 56-57.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/2
C     ORIGINAL VERSION--FEBRUARY  2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IISOLA
      CHARACTER*4 IISOME
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICONC
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Z(*)
      DIMENSION YRAW(*)
      DIMENSION ROUND(*)
      DIMENSION ALABID(*)
      DIMENSION AMATID(*)
      DIMENSION ALAB(*)
C
      DIMENSION YMEAN(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION PPA0(*)
      DIMENSION PPA1(*)
      DIMENSION PPA0SD(*)
      DIMENSION PPA1SD(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
C
      DOUBLE PRECISION DSUM1
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
      ISUBN1='DPIS'
      ISUBN2='O2  '
      IWRITE='OFF'
C
      IERROR='NO'
      NPLOTP=0
      NPLOTV=3
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ISO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPISO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IBUGG3,ISUBRO,ICASPL,IISOLA,N,NLAB,NAVE
   72   FORMAT('IBUGG3,ISUBRO,ICASPL,IISOLA,N,NLAB = ',4(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO81I=1,N
            WRITE(ICOUT,82)I,YRAW(I),Z(I),ROUND(I),ALABID(I),AMATID(I)
   82       FORMAT('I,YRAW(I),Z(I),ROUND(I),ALABID(I),AMATID(I) = ',
     1             I8,5G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
        IF(NLAB.GT.0)THEN
          DO86I=1,NLAB
            WRITE(ICOUT,87)I,ALAB(I)
   87       FORMAT('I,ALAB(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   86     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN ISO 13528 PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 2--                              **
C               **  COMPUTE LAB AVERAGES (IN ORIGINAL     **
C               **  UNITS) FOR EACH ROUND OVER ALL LABS.  **
C               ********************************************
C
      IWRITE='OFF'
      CALL CODE(AMATID,N,IWRITE,XIDTEM,XIDTE2,MAXOBV,IBUGG3,IERROR)
      AMAX=CPUMIN
      DO110I=1,N
        AMATID(I)=XIDTEM(I)
        IF(AMATID(I).GT.AMAX)AMAX=AMATID(I)
  110 CONTINUE
C
      CALL DISTIN(ROUND,N,IWRITE,XIDTEM,NROUND,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NROUND,XIDTEM)
      CALL DISTIN(AMATID,N,IWRITE,XIDTE2,NMAT,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NMAT,XIDTE2)
C
      IF(NAVE.EQ.N)THEN
        ICNT=0
        DO1010IRND=1,NROUND
          HOLD=XIDTEM(IRND)
          DO1020IMAT=1,NMAT
            HOLD2=XIDTE2(IMAT)
            DSUM1=0.0D0
            K=0
C
            DO1030J=1,N
              IF(ROUND(J).EQ.HOLD .AND.AMATID(J).EQ.HOLD2)THEN
                K=K+1
                TEMP1(K)=YRAW(J)
              ENDIF
 1030       CONTINUE
            IF(IISOLA.EQ.'RESP')THEN
              IF(K.EQ.0)THEN
                XMEAN=CPUMIN
              ELSE
                IF(IISOME.EQ.'MEAN')THEN
                  CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
                ELSEIF(IISOME.EQ.'H15')THEN
                  C=1.5
                  NCUT=0
                  CALL H15(TEMP1,K,C,NCUT,XMEAN,XSC,TEMP2,TEMP3,
     1                     MAXOBV,ISUBRO,IBUGG3)
                ELSEIF(IISOME.EQ.'MEDI')THEN
                  CALL MEDIAN(TEMP1,K,IWRITE,TEMP2,MAXOBV,XMEAN,
     1                        IBUGG3,IERROR)
                ENDIF
              ENDIF
            ELSEIF(IISOLA.EQ.'LAVE')THEN
              IF(K.EQ.0)THEN
                XMEAN=CPUMIN
              ELSE
                XMEAN=TEMP1(1)
              ENDIF
            ENDIF
C
            ICNT=ICNT+1
            YMEAN(ICNT)=XMEAN
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ISO2')THEN
              WRITE(ICOUT,1096)IRND,IMAT,ICNT,K,YMEAN(ICNT)
 1096         FORMAT('IRND,IMAT,ICNT,K,YMEAN(ICNT) = ',4I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
 1020     CONTINUE
 1010   CONTINUE
      ELSE
        IF(NAVE.NE.NROUND)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1032)
 1032     FORMAT('      THE NUMBER OF LAB AVERAGES DOES NOT EQUAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1034)
 1034     FORMAT('      THE NUMBER OF ROUNDS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1036)NAVE
 1036     FORMAT('      THE NUMBER OF LAB AVERAGES = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1038)NROUND
 1038     FORMAT('      THE NUMBER OF ROUNDS       = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ICNT=0
        DO1040I=1,NROUND
          HOLD=YRAW(I)
          DO1050J=1,NMAT
            ICNT=ICNT+1
            YMEAN(ICNT)=HOLD
 1050     CONTINUE
 1040   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 3--                              **
C               **  GENERATE THE PLOT COORDINATES.        **
C               ********************************************
C
      DO2010J=1,NLAB
        HOLD3=ALAB(J)
        NOLD=NPLOTP
        NTEMP=0
        ICNT=(J-1)*NMAT
C
        ICNT2=0
        DO2020IRND=1,NROUND
          HOLD=XIDTEM(IRND)
          DO2030IMAT=1,NMAT
            HOLD2=XIDTE2(IMAT)
            ICNT2=ICNT2+1
            AMEAN=YMEAN(ICNT2)
            DO2030IROW=1,N
              IF(ROUND(IROW).EQ.HOLD .AND. AMATID(IROW).EQ.HOLD2 .AND.
     1           ALABID(IROW).EQ.HOLD3)THEN
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=Z(IROW)
              X(NPLOTP)=AMEAN
              IINDX=ICNT+INT(AMATID(IROW)+0.1)
              D(NPLOTP)=REAL(IINDX)
              NTEMP=NTEMP+1
            ENDIF
 2030     CONTINUE
 2020   CONTINUE
C
C       NOW COMPUTE A LINEAR FIT AND SAVE THE PARAMETER ESTIMATES
C       AND STANDARD ERRORS.  ADD OPTIONAL FITTED LINE TO GRAPH.
C
        ICNT=NLAB*NMAT + J
        IF(NTEMP.GE.2)THEN
          CALL LINFIT(Y(NOLD+1),X(NOLD+1),NTEMP,
     1                ALOC,SLOPE,XRESSD,XRESDF,PPCC,A0SD,A1SD,CCALBE,
     1                ISUBRO,IBUGG3,IERROR)
          PPA0(J)=ALOC
          PPA1(J)=SLOPE
          PPA0SD(J)=A0SD
          PPA1SD(J)=A1SD
          CALL MINIM(X(NOLD+1),NTEMP,IWRITE,XMIN,IBUGG3,IERROR)
          CALL MAXIM(X(NOLD+1),NTEMP,IWRITE,XMAX,IBUGG3,IERROR)
          NPLOTP=NPLOTP+1
          Y(NPLOTP)=ALOC + SLOPE*XMIN
          X(NPLOTP)=XMIN
          D(NPLOTP)=REAL(ICNT)
          NPLOTP=NPLOTP+1
          Y(NPLOTP)=ALOC + SLOPE*XMAX
          X(NPLOTP)=XMAX
          D(NPLOTP)=REAL(ICNT)
        ELSE
          PPA0(J)=CPUMIN
          PPA1(J)=CPUMIN
          PPA0SD(J)=CPUMIN
          PPA1SD(J)=CPUMIN
        ENDIF
C
 2010 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ISO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPISO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NPLOTP,NPLOTV
 9013   FORMAT('IERROR,NPLOTP,NPLOTV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9035I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,2E15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPISO4(PPA0,PPA1,PPA0SD,PPA1SD,NLAB,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPISOP.  FOR EACH LAB, WRITE VALUES
C              OF FITTED LINE (INTERCEPT AND SLOPE WITH THEIR STANDARD ERRORS)
C              TO EXTERNAL FILE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/2
C     ORIGINAL VERSION--FEBRUARY  2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION PPA0(*)
      DIMENSION PPA1(*)
      DIMENSION PPA0SD(*)
      DIMENSION PPA1SD(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
C
      CHARACTER*80 IFILE1
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
      CHARACTER*4 IERRF1
      CHARACTER*4 IENDF1
      CHARACTER*4 IREWI1
C
C-----COMMON----------------------------------------------------------
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
C               ***************************************
C               **  STEP 1--                         **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPISO4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IOUNI1=IST1NU
      IFILE1=IST1NA
      ISTAT1=IST1ST
      IFORM1=IST1FO
      IACCE1=IST1AC
      IPROT1=IST1PR
      ICURS1=IST1CS
      ISUBN0='ISO4'
      IERRF1='NO'
      IREWI1='ON'
C
      CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,
     1            IREWI1,ISUBN0,IERRF1,IBUGG3,ISUBRO,IERRF1)
      IST1CS=ICURS1
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        ISTEPN='2A'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)
  201   FORMAT('AFTER CALL DPOPFI, IERRF1 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)IOUNI1,IFILE1
  203   FORMAT('IOUNI1,IFILE1 = ',I5,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERRF1.EQ.'YES')GOTO9000
      WRITE(IOUNI1,299)
  299 FORMAT(13X,'A0',13X,'A1',11X,'A0SD',11X,'A1SD',
     1       5X,'A0 t-VALUE',5X,'A1 t-VALUE')
C
      DO1010I=1,NLAB
        TVAL1=CPUMIN
        TVAL2=CPUMIN
        IF(PPA0SD(I).NE.CPUMIN .AND. PPA0SD(I).NE.0.0)THEN
          TVAL1=PPA0(I)/PPA0SD(I)
        ENDIF
        IF(PPA1SD(I).NE.CPUMIN .AND. PPA1SD(I).NE.0.0)THEN
          TVAL2=PPA1(I)/PPA1SD(I)
        ENDIF
        WRITE(IOUNI1,1031)PPA0(I),PPA1(I),PPA0SD(I),PPA1SD(I),
     1                    TVAL1,TVAL2
 1031   FORMAT(6E15.7)
 1010 CONTINUE
C
      IERRF1='NO'
      IENDF1='OFF'
      IREWI1='ON'
      CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,
     1            IENDF1,IREWI1,ISUBN0,IERRF1,IBUGG3,ISUBRO,IERRF1)
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        ISTEPN='3A'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('AFTER CALL DPCLFI, IERRF1 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,303)IOUNI1,IFILE1
  303   FORMAT('IOUNI1,IFILE1 = ',I5,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERRF1.EQ.'YES')GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPISO4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPISP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                 MAXNXT,ISEED,ICONT,
     1                 ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A <STAT> INTERACTION PLOT
C              (SEE ROUTINE  EXTSTA  FOR A LIST OF SUPPORTED STATISTICS).
C              THESE DIFFER FROM THE STATISTIC PLOT CASE IN THAT THERE
C              CAN BE MORE THAN 1 X VARIABLE AND THESE ARE MULTIPLIED
C              TO GET THE INTERACTION X TERM.  THE MAIN APPLICATION
C              IS IN DESIGN OF EXPERIMENTS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/10
C     ORIGINAL VERSION--OCTOBER   1999.
C     UPDATED         --JULY      2002. BIWEIGHT LOCATION
C     UPDATED         --JULY      2002. BIWEIGHT SCALE
C     UPDATED         --JULY      2002. WINSORIZED VARIANCE
C     UPDATED         --JULY      2002. WINSORIZED SD
C     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCORRELATION PLOT
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
C                                           PLOT
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND CORRELATION
C                                           PLOT
C     UPDATED         --JULY      2002. ADD HODGES LEHMAN PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR PLOT
C     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD
C                                       ERROR PLOT
C     UPDATED         --APRIL     2003. ADD SN AND QN, REQUIRED
C                                       ADDITIONAL SCRATCH ARRAYS
C     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO COMMON
C     UPDATED         --NOVEMBER  2009. UPDATE PARSING:
C                                       1) USE "EXTSTA"
C                                       2) USE DPPARS
C     UPDATED         --NOVEMBER  2009. UPDATE CALL LIST TO DPSP2
C                                       (DPSP2 WAS MODIFIED TO ADD
C                                       SOME ENHANCEMENTS FOR THE
C                                       <STAT> PLOT COMMAND)
C     UPDATED         --JUNE      2010. UPDATE CALL LIST TO DPSP2
C     UPDATED         --APRIL     2015. UPDATE CALL LIST TO DPSP2
C     UPDATED         --FEBRUARY  2018. UPDATE CALL LIST TO DPSP2
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IERRO2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IGROUP
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
      CHARACTER*40 INAME
      CHARACTER*60 ISTANM
      CHARACTER*4  ISTADF
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
C
      PARAMETER (MAXRES=25)
C
      DIMENSION Z(MAXOBV,MAXRES)
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
C
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP6(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP7(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP8(1))
      EQUIVALENCE (GARBAG(IGARB9),Z(1,1))
C
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
C
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
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
      IERROR='NO'
      IGROUP='OFF'
      ISUBN1='PISP'
      ISUBN2='    '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *************************************************
C               **  TREAT THE INTERACTION STATISTIC PLOT CASE  **
C               *************************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPISP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************
C               **  STEP 1--                       **
C               **  EXTRACT THE DESIRED STATISTIC  **
C               *************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO9000
C
      JMIN=0
      JMAX=NUMARG
      IFLAGZ=0
      IFLAGU=0
C
      DO200I=1,NUMARG-1
        IF(I.LT.NUMARG.AND.IHARG(I).EQ.'INTE'.AND.
     1         IHARG(I+1).EQ.'PLOT')THEN
          IF(JMAX.EQ.NUMARG)JMAX=I-1
          ILASTC=I+1
          GOTO209
        ENDIF
  200 CONTINUE
      GOTO9000
  209 CONTINUE
C
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1            ICASPL,ISTANM,ISTANR,ISTADF,IFOUND,ILOCV,
     1            ISUBRO,IBUGG3,IERROR)
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,251)
  251   FORMAT('***** AFTER CALL EXTSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)ICASPL,ISTANR,ILOCV,IFOUND
  252   FORMAT('ICASPL,ISTANR,ILOCV,IFOUND = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'NO')GOTO9000
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      INAME='<stat> INTERACTION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=MAXSPN
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
      IF(IERROR.EQ.'YES')GOTO9000
C
C     NEED FOLLOWING VARIABLES:
C     1) ONE RESPONSE VARIABLE FOR STATISTICS REQUIRING ONE VARIABLE
C     2) TWO RESPONSE VARIABLES FOR STATISTICS REQUIRING TWO VARIABLES
C     3) ONE OR MORE FACTOR VARIABLES (TYPICALLY THERE ARE TWO)
C
      ISIZE=-99
      MINVAR=1+ISTANR
      IF(NUMVAR.LT.MINVAR)THEN
C
        IF(NUMVAR.EQ.MINVAR-1)THEN
          IH='NI  '
          IH2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
          IF(IERROR.EQ.'NO')THEN
            ISIZE=VALUE(ILOCP)+0.5
            GOTO219
          ENDIF
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
  211   FORMAT('***** ERROR IN INTERACTION PLOT COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,212)MINVAR
  212   FORMAT('      AT LEAST ',I5,' VARIABLES REQUIRED, BUT ONLY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)NUMVAR
  213   FORMAT('      ',I8,' VARIABLES WERE GIVEN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,215)
  215   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,216)(IANS(J),J=1,MIN(80,IWIDTH))
  216     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
  219 CONTINUE
C
C               *********************************
C               **  STEP 3--                   **
C               **  EXTRACT THE DATA           **
C               *********************************
C
 2650 CONTINUE
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      NFACT=NUMVAR-ISTANR
      EPS=1.0E-7
C
      NUMVA2=1
      DO3010K=1,ISTANR
        ICOL=K
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Z(1,K),TEMP1,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
 3010 CONTINUE
C
C     THE "INTERACTION" VARIABLE IS THE PRODUCT OF ALL THE FACTOR
C     VARIABLES.  NOTE THAT FOR THE INTERACTION PLOT, THIS PRODUCT
C     SHOULD BE "0", "+1", OR "-1".  REPORT AN ERROR IF IT IS NOT.
C
C     INTIALIZE COLUMN TO 1 AND THEN MULTIPLY BY EACH COLUMN.
C
      DO3015II=1,MAXOBV
        Z(II,ISTANR+1)=1.0
 3015 CONTINUE
C
      DO3020K=ISTANR+1,NUMVAR
        ICOL=K
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              TEMP1,TEMP2,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
C
        DO3025II=1,NLOCAL
          Z(II,ISTANR+1)=Z(II,ISTANR+1)*TEMP1(II)
          IFLAG=1
          IF(ABS(Z(II,ISTANR+1)).LE.EPS)IFLAG=0
          IF(ABS(Z(II,ISTANR+1)-1.0).LE.EPS)IFLAG=0
          IF(ABS(Z(II,ISTANR+1)+1.0).LE.EPS)IFLAG=0
C
          IF(IFLAG.EQ.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3301)
 3301       FORMAT('      A PRODUCT OF THE INDEPENDENT VARIABLES IS ',
     1             'NOT EQUAL TO -1, 0, +1')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
 3025   CONTINUE
C
 3020 CONTINUE
C
      IF(ISIZE.GT.0)THEN
        NUMVAR=NUMVAR+1
        DO3600J=1,NLOCAL
          ITEMP=MOD(J,ISIZE)
          IF(ITEMP.EQ.0)ITEMP=ISIZE
          Z(J,NUMVAR)=REAL(ITEMP)
 3600   CONTINUE
      ENDIF
C
      NUMVA2=ISTANR+1
C
C               *****************************************************
C               **  STEP 28--                                      **
C               **  COMPUTE THE APPROPRIATE INTERACTION STATISTIC  **
C               **  PLOT STATISTIC--                               **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C               *****************************************************
C
      ISTEPN='28'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,2811)NLOCAL,NUMVAR,ISTANR,IFLAGZ,IFLAGU
 2811   FORMAT('NLOCAL,NUMVAR,ISTANR,IFLAGZ,IFLAGU = ',5I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2813)ICASPL
 2813   FORMAT('ICASPL = ',A4)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1)THEN
          DO2815I=1,NLOCAL
            WRITE(ICOUT,2817)I,Z(I,1),Z(I,2),Z(I,3)
 2817       FORMAT('I,Z(I,1),Z(I,2),Z(I,3) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 2815     CONTINUE
        ENDIF
      ENDIF
C
      CALL DPSP2(Z,MAXOBV,MAXRES,NLOCAL,NUMVA2,ISTANR,IFLAGZ,IFLAGU,
     1           ICASPL,ISIZE,ICONT,
     1           TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,MAXNXT,
     1           ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1           DTEMP1,DTEMP2,DTEMP3,
     1           IQUAME,IQUASE,PSTAMV,ISTAFO,ISTASM,ISPLRL,IGROUP,
     1           Y,X,D,NPLOTP,NPLOTV,NUMSET,GRAND,
     1           ISUBRO,IBUGG3,IERROR)
C
C               *************************************************
C               **  STEP 29--                                  **
C               **  SAVE DIFFERENCE BETWEEN HIGHEST VALUE AND  **
C               **  LOWEST VALUE OF STATISTIC IN INTERNAL      **
C               **  PARAMETER ALOWHIGH                         **
C               *************************************************
C
      AMINS=CPUMAX
      AMAXS=CPUMIN
      DO2910I=1,NPLOTP
        IF(D(I).NE.1.0)GOTO2910
        IF(Y(I).GT.AMAXS)THEN
          AMAXS=Y(I)
          IMAXIN=I
        ENDIF
        IF(Y(I).LT.AMINS)THEN
          AMINS=Y(I)
          IMININ=I
        ENDIF
 2910 CONTINUE
      ADIFF=AMAXS-AMINS
      IF(IMAXIN.GT.IMININ)ADIFF=-ADIFF
C
      ISUBN0='PISP'
C
      IH='ALOW'
      IH2='HIGH'
      VALUE0=ADIFF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
      IF(GRAND.NE.CPUMIN)THEN
        IH='GRAN'
        IH2='DSTA'
        VALUE0=GRAND
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGG3,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'PISP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPISP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(IFOUND.EQ.'YES' .AND. NPLOTP.GT.0)THEN
          DO9025I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJAB2(Y,N,
     1                  TEMP1,TEMP2,MAXNXT,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  STATVA,PVAL,CDF,
     1                  ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT THE JARQUE BERA TEST
C              FOR NORMALITY.  THIS TEST IS BASED ON THE SKEWNESS
C              AND KURTOSIS PARAMETERS.
C     EXAMPLE--JARQUE BERA NORMALITY TEST Y
C     REFERENCE--BRANI VIDAKOVIC (2011), "STATISTICS FOR 
C                BIOENGINEERING SCIENCES: WITH MATLAB AND WINBUGS
C                SUPPORT", SPRINGER, PP. 521-522.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/6
C     ORIGINAL VERSION--JUNE      2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 IRANAL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      LOGICAL WGTS
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION PID(*)
C
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
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
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      LOGICAL IFLAG3
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
      DATA ALPHA /50.0, 80.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPJA'
      ISUBN2='B2  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPJAB2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: JARQUE-BERA TEST--')
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
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
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
C               ******************************
C               **  STEP 11--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR JARQUE BERA         **
C               **  TEST                    **
C               ******************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      CALL MINIM(Y,N,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,YMAX,IBUGA3,IERROR)
C
      CALL DPJAB3(Y,N,ISEED,IRANAL,MAXNXT,
     1            TEMP1,TEMP2,
     1            YSKEW,YKURT,
     1            STATVA,PVAL,CDF,
     1            CUT25,CUT50,CUT75,CUT80,CUT90,
     1            CUT95,CUT975,CUT99,CUT999,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR JARQUE BERA TEST      **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')
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
      ITITLE='Jarque-Bera Test for Normality'
      NCTITL=30
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
      ITEXT(ICNT)='H0: The Data Are Normally Distributed'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Data Are Not Normally Distributed'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Skewness:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=YSKEW
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Kurtosis:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=YKURT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO5010I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE=' '
      NCTITL=0
C
      ITITL9=' '
      NCTIT9=0
      ITITLE(1:44)='Percent Points of the Reference Distribution'
      NCTITL=44
      NUMLIN=1
      NUMROW=8
      NUMCOL=3
      ITITL2(1,1)='Percent Point'
      ITITL2(1,2)=' '
      ITITL2(1,3)='Value'
      NCTIT2(1,1)=13
      NCTIT2(1,2)=1
      NCTIT2(1,3)=5
C
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=5
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
 4221 CONTINUE
      ITYPCO(2)='ALPH'
      IDIGIT(1)=1
      IDIGIT(3)=3
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
          IF(J.EQ.2)THEN
            IVALUE(I,J)='='
            NCVALU(I,J)=1
          ELSEIF(J.EQ.3)THEN
            IF(I.EQ.1)THEN
              AMAT(I,1)=25.0
              AMAT(I,J)=RND(CUT25,IDIGIT(J))
            ELSEIF(I.EQ.2)THEN
              AMAT(I,1)=50.0
              AMAT(I,J)=RND(CUT50,IDIGIT(J))
            ELSEIF(I.EQ.3)THEN
              AMAT(I,1)=75.0
              AMAT(I,J)=RND(CUT75,IDIGIT(J))
            ELSEIF(I.EQ.4)THEN
              AMAT(I,1)=80.0
              AMAT(I,J)=RND(CUT80,IDIGIT(J))
            ELSEIF(I.EQ.5)THEN
              AMAT(I,1)=90.0
              AMAT(I,J)=RND(CUT90,IDIGIT(J))
            ELSEIF(I.EQ.6)THEN
              AMAT(I,1)=95.0
              AMAT(I,J)=RND(CUT95,IDIGIT(J))
            ELSEIF(I.EQ.7)THEN
              AMAT(I,1)=97.5
              AMAT(I,J)=RND(CUT975,IDIGIT(J))
            ELSEIF(I.EQ.8)THEN
              AMAT(I,1)=99.0
              AMAT(I,J)=RND(CUT99,IDIGIT(J))
            ENDIF
          ENDIF
 4225   CONTINUE
 4223 CONTINUE
C
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.FALSE.
C
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'GRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      NUMLIN=1
      NUMROW=4
      NUMCOL=4
      ITITL2(1,1)='Alpha'
      ITITL2(1,2)='CDF'
      ITITL2(1,3)='Critical Value'
      ITITL2(1,4)='Conclusion'
      NCTIT2(1,1)=5
      NCTIT2(1,2)=3
      NCTIT2(1,3)=14
      NCTIT2(1,4)=10
C
      NMAX=0
      DO4321I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO4323I=1,NUMROW
        DO4325J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
      IVALUE(1,1)='10%'
      IVALUE(2,1)='5%'
      IVALUE(3,1)='2.5%'
      IVALUE(4,1)='1%'
      IVALUE(1,2)='90%'
      IVALUE(2,2)='95%'
      IVALUE(3,2)='97.5%'
      IVALUE(4,2)='99%'
      NCVALU(1,1)=3
      NCVALU(2,1)=2
      NCVALU(3,1)=4
      NCVALU(4,1)=2
      NCVALU(1,2)=3
      NCVALU(2,2)=3
      NCVALU(3,2)=5
      NCVALU(4,2)=3
      IVALUE(1,4)='Accept H0'
      IVALUE(2,4)='Accept H0'
      IVALUE(3,4)='Accept H0'
      IVALUE(4,4)='Accept H0'
      NCVALU(1,4)=9
      NCVALU(2,4)=9
      NCVALU(3,4)=9
      NCVALU(4,4)=9
      IF(STATVA.GT.CDF1)IVALUE(1,4)='Reject H0'
      IF(STATVA.GT.CDF2)IVALUE(2,4)='Reject H0'
      IF(STATVA.GT.CDF3)IVALUE(3,4)='Reject H0'
      IF(STATVA.GT.CDF4)IVALUE(4,4)='Reject H0'
      AMAT(1,3)=RND(CDF1,IDIGIT(3))
      AMAT(2,3)=RND(CDF2,IDIGIT(3))
      AMAT(3,3)=RND(CDF3,IDIGIT(3))
      AMAT(4,3)=RND(CDF4,IDIGIT(3))
C
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWRTF(1)=1500
      IWRTF(2)=IWRTF(1)+1500
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(3)+2000
      IFRST=.FALSE.
      ILAST=.TRUE.
C
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJAB2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJAB3(Y,N,ISEED,IRANAL,MAXNXT,
     1                  TEMP1,TEMP2,
     1                  YSKEW,YKURT,
     1                  STATVA,PVAL,CDF,
     1                  CUT25,CUT50,CUT75,CUT80,CUT90,
     1                  CUT95,CUT975,CUT99,CUT999,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT THE JARQUE-BERA TEST
C              FOR NORMALITY.  EXTRACT FROM DPJAB3 IN ORDER TO
C              ALSO CALL BASIC COMPUTATION FROM CMPSTA.
C     REFERENCE--BRANI VIDAKOVIC (2011), "STATISTICS FOR 
C                BIOENGINEERING SCIENCES: WITH MATLAB AND WINBUGS
C                SUPPORT", SPRINGER, PP. 521-522.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/12
C     ORIGINAL VERSION--DECEMBER  2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IRANAL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IBUGAZ
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 IRANSV
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
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
      ISUBN1='DPJA'
      ISUBN2='B3  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPJAB3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: JARQUE-BARE TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 5.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
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
C               ******************************
C               **  STEP 11--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR JARQUE-BERA         **
C               **  TEST                    **
C               ******************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IBUGAZ='ON'
      IF(IBUGA3.EQ.'NO')IBUGAZ='OFF'
      IF(IBUGA3.EQ.'OFF')IBUGAZ='OFF'
      CALL STMOM3(Y,N,IWRITE,YSKEW,IBUGAZ,IERROR)
      CALL STMOM4(Y,N,IWRITE,YKURT,IBUGAZ,IERROR)
      AN=REAL(N)
      STATVA=(AN/6.0)*(YSKEW**2 + (YKURT - 3.0)**2/4.0)
C
C     FOR LARGE N, OBTAIN P-VALUE FROM CHI-SQUARE.  OTHERWISE,
C     PERFORM A SIMULATION.
C
      IF(N.LT.2000)THEN
        CALL MEAN(Y,N,IWRITE,YMEAN,IBUGAZ,IERROR)
        CALL SD(Y,N,IWRITE,YSD,IBUGAZ,IERROR)
C
C       NOW PERFORM 10,000 SIMULATIONS
C
        ISEESV=ISEED
        ISEED=2503
        IRANSV=IRANAL
        IRANAL='FINC'
        NSIM=100000
        DO1000I=1,NSIM
          CALL NORRAN(N,ISEED,TEMP1)
          DO1010J=1,N
            TEMP1(I)=YMEAN + YSD*TEMP1(I)
 1010     CONTINUE
          CALL STMOM3(TEMP1,N,IWRITE,YSKEW2,IBUGAZ,IERROR)
          CALL STMOM4(TEMP1,N,IWRITE,YKURT2,IBUGAZ,IERROR)
          AN=REAL(N)
          STATV2=(AN/6.0)*(YSKEW2**2 + (YKURT2 - 3.0)**2/4.0)
          TEMP2(I)=STATV2
 1000   CONTINUE
        IDIR='UPPE'
CCCCC   IDIR='LOWE'
        CALL DPGOF8(TEMP2,NSIM,STATVA,PVAL,IDIR,
     1              IBUGAZ,ISUBRO,IERROR)
        CDF=1.0 - PVAL
        ISEED=ISEESV
        IRANAL=IRANSV
        PTEMP=25.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT25,
     1              IBUGA3,IERROR)
        PTEMP=50.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT50,
     1              IBUGA3,IERROR)
        PTEMP=75.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT75,
     1              IBUGA3,IERROR)
        PTEMP=80.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT80,
     1              IBUGA3,IERROR)
        PTEMP=90.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT90,
     1              IBUGA3,IERROR)
        PTEMP=95.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT95,
     1              IBUGA3,IERROR)
        PTEMP=97.5
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT975,
     1              IBUGA3,IERROR)
        PTEMP=99.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT99,
     1              IBUGA3,IERROR)
        PTEMP=99.9
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT999,
     1              IBUGA3,IERROR)
      ELSE
        NU=2
        CALL CHSCDF(STATVA,NU,CDF)
        PVAL=1.0 - CDF
        CALL CHSPPF(0.25,NU,CUT25)
        CALL CHSPPF(0.50,NU,CUT50)
        CALL CHSPPF(0.75,NU,CUT75)
        CALL CHSPPF(0.80,NU,CUT80)
        CALL CHSPPF(0.90,NU,CUT90)
        CALL CHSPPF(0.95,NU,CUT95)
        CALL CHSPPF(0.975,NU,CUT975)
        CALL CHSPPF(0.99,NU,CUT99)
        CALL CHSPPF(0.999,NU,CUT999)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJAB3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)YSKEW,YKURT,STATVA,CDF
 9013   FORMAT('YSKEW,YKURT,STATVA,CDF = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBOOSS,ISEED,IBCABT,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  CLLIMI,CLWIDT,
     1                  ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,
     1                  IFOUND,IERROR)
C
C     PURPOSE--GENERATE A JACKNIFE OR BOOTSTRAP PLOT OF:
C
C              1) ANY DATAPLOT STATISTIC THAT REQUIRES
C                 EITHER ONE OR TWO RESPONSE VARIABLES
C                 (I.E., EXTSTA/CMPSTA)
C
C              2) GOODNESS OF FIT FOR DISTRIBUTIONS (FOR
C                 DISTRIBUTIONS WITH 0, 1, OR 2 SHAPE PARAMETERS).
C                 THE CURRENTLY SUPPORTED GOODNESS OF FIT
C                 STATISTCS ARE PPCC, KOLMOGOROV-SMIRNOV, AND
C                 ANDERSON-DARLING.
C
C              3) MAXIMUM LIKELIHOOD FOR DISTRIBUTIONS.
C
C              4) STATISTICS THAT ARE CURRENTLY SPECIFIC TO
C                 THE JACKNIFE/BOOTSTRAP.  THIS CURRENTY
C                 INCLUDES:
C
C                 a) LINEAR CALIBRATION
C                 b) QUADRATIC CALIBRATION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO G2RBAGE COMMON
C     UPDATED         --FEBRUARY  1994. SYNONYMS FOR TAGUCHI
C     UPDATED         --MARCH     1995. MAD AND AAD PLOTS
C     UPDATED         --MARCH     1998. SAVE CERTAIN PERCENTILE PARAMETERS
C                                       AUTOMATICALLY
C     UPDATED         --MARCH     1998. ACTIVATE RELATIVE VARIANCE AND
C                                       COEFFICENT OF VARIATION
C     UPDATED         --NOVEMBER  1998. ADD PERCENTILE PLOTS
C     UPDATED         --MARCH     1999. ADD GEOMETRIC MEAN
C     UPDATED         --MARCH     1999. ADD GEOMETRIC STAND DEVIATION
C     UPDATED         --MARCH     1999. ADD HARMONIC MEAN
C     UPDATED         --SEPTEMBER 2001. ADD IQ RANGE
C     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT LOCATION
C     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT SCALE
C     UPDATED         --JULY      2002. ADD WINSORIZED VARIANCE
C     UPDATED         --JULY      2002. ADD WINSORIZED SD
C     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE
C     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCORRELATION
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND CORRELATION
C     UPDATED         --JULY      2002. ADD HODGES LEHMAN
C     UPDATED         --JULY      2002. ADD QUANTILE
C     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR
C     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD ERROR
C     UPDATED         --JULY      2002. ADD LINEAR CALIBRATION
C     UPDATED         --JULY      2002. ADD QUADRATIC CALIBRATION
C     UPDATED         --MARCH     2003. ADD 34 "DIFFERENCE OF" STATS
C     UPDATED         --MARCH     2003. FOR "DIFFERENCE OF" STATS,
C                                       DISTINGUISH BETWEEN INDEPENDENT
C                                       AND DEPENDENT GROUPS
C     UPDATED         --APRIL     2003. ADD SN AND QN (AND DIFFERENCE
C                                       OF).  REQUIRED ADDITIONAL
C                                       SCRATCH ARRAYS.
C     UPDATED         --JULY      2003. SUPPORT FOR TWO GROUP VARIABLES
C     UPDATED         --SEPTEMBER 2003. SUPPORT FOR BCA CONFIDENCE INTERVAL
C     UPDATED         --JANUARY   2005. MAKE COMMAND SEARCH TABLE
C                                       DRIVEN
C     UPDATED         --JANUARY   2005. SUPPORT FOR BOOTSTRAPPING OF
C                                       DISTRIBUTIONAL MODELS
C     UPDATED         --MARCH     2005. ADD GENERALIZED PARETO MLE
C                                       AND MOMENTS
C     UPDATED         --MAY       2005. ADD FRECHET MLE
C     UPDATED         --AUGUST    2005. ADD INVERTED WEIBULL MLE
C     UPDATED         --SEPTEMBER 2005. ADD RATIO
C     UPDATED         --MARCH     2006. UNIFORM MLE PLOT AS SYNONYM
C                                       FOR UNIFORM MAXI LIKE
C     UPDATED         --MARCH     2006. ADD GENERALIZIED LOGISTIC
C                                       TYPE 2 - TYPE 5
C     UPDATED         --MARCH     2006. ADD BETA NORMAL
C     UPDATED         --OCTOBER   2006. MAXWELL KS PLOT
C     UPDATED         --FEBRUARY  2007. ADD SOME ADDITIONAL
C                                       DISTRIBUTUIONS
C     UPDATED         --MARCH     2007. ADD RELATIVE RISK
C     UPDATED         --MARCH     2007. ADD CRAMER CONTINCENCY COEFF
C     UPDATED         --MARCH     2007. ADD PEARSON CONTINCENCY COEFF
C     UPDATED         --MARCH     2007. FALSE POSITIVE
C     UPDATED         --MARCH     2007. FALSE NEGATIVE
C     UPDATED         --MARCH     2007. TRUE POSITIVE
C     UPDATED         --MARCH     2007. TRUE NEGATIVE
C     UPDATED         --MARCH     2007. TEST SENSITIVITY
C     UPDATED         --MARCH     2007. TEST SPECIFICITY
C     UPDATED         --APRIL     2007. POSITIVE PREDICTIVE VALUE
C     UPDATED         --APRIL     2007. NEGATIVE PREDICTIVE VALUE
C     UPDATED         --APRIL     2007. ADD LOG ODDS RATIO
C     UPDATED         --APRIL     2007. ADD LOG ODDS RATIO SE
C     UPDATED         --MAY       2007. ADD TRIMMED STAND DEVI
C     UPDATED         --MAY       2007. ADD TRIANGULAR MAXIMUM LIKELIHOOD
C     UPDATED         --JUNE      2007. ADD SLASH MAXIMUM LIKELIHOOD
C     UPDATED         --AUGUST    2007. ADD BETA NORMAL AND LOG BETA MLE
C     UPDATED         --SEPTEMBER 2007. ADD REFLECTED GENERALIZED TOPP
C                                       LEONE MLE
C     UPDATED         --OCTOBER   2007. ADD SLOPE, OGIVE
C     UPDATED         --OCTOBER   2007. ADD TWO-SIDED SLOPE
C     UPDATED         --OCTOBER   2007. ADD TWO-SIDED OGIVE
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 2
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 3
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 5
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 6
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 7
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 8
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 9
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 10
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 11
C     UPDATED         --OCTOBER   2007. ADD BURR TYPE 12
C     UPDATED         --NOVEMBER  2007. ADD DOUBLE PARETO UNIFORM
C     UPDATED         --NOVEMBER  2007. ADD KUMARASWAMY
C     UPDATED         --NOVEMBER  2007. ADD ALPHA
C     UPDATED         --NOVEMBER  2007. ADD EXPONENTIAL POWER
C     UPDATED         --NOVEMBER  2007. ADD FOLDED CAUCHY
C     UPDATED         --NOVEMBER  2007. LP LOCATION
C     UPDATED         --NOVEMBER  2007. VARIANCE OF LP LOCATION
C     UPDATED         --NOVEMBER  2007. SD OF LP LOCATION
C     UPDATED         --NOVEMBER  2007. DIFFERENCE OF LP LOCATION
C     UPDATED         --NOVEMBER  2007. DIFFERENCE OF VARI OF LP LOCATION
C     UPDATED         --NOVEMBER  2007. DIFFERENCE OF SD OF LP LOCATION
C     UPDATED         --DECEMBER  2007. POWER MLE
C     UPDATED         --DECEMBER  2007. REFLECTED POWER PPCC/KS
C     UPDATED         --JANUARY   2008. MUTH PPCC/KS
C     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL PPCC/KS
C     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL MLE
C     UPDATED         --MARCH     2008. TRUNCATED PARETO MLE
C     UPDATED         --MARCH     2008. REFLECTED POWER MLE
C     UPDATED         --JULY      2008. INVERTED GAMMA MLE
C     UPDATED         --JULY      2008. VON MISES MLE
C     UPDATED         --JULY      2008. MIELKE BETA-KAPPA PPCC/KS
C     UPDATED         --JULY      2008. KAPPA PPCC/KS/MLE
C     UPDATED         --JULY      2008. PEARSON TYPE 3 PPCC/KS/MLE
C     UPDATED         --FEBRUARY  2009. BINOMIAL PROPORTION
C     UPDATED         --FEBRUARY  2009. GRUBB
C     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
C     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
C     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
C     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
C     UPDATED         --MARCH     2010. RE-WRITE TO:
C                                       1) USE DPPARS
C                                       2) USE EXTSTA
C                                       3) USE EXTDIS
C                                       4) USE DIFFERENT SUBROUTINES
C                                          FOR DIFFERENT CASES TO KEEP
C                                          OVERALL CODE MORE DIGESTABLE
C     UPDATED         --SEPTEMBER 2010. SUPPORT A "LEVEL" VARIABLE
C                                       FOR BRITTLE FIBER WEIBULL
C                                       (MAY ADD TO A FEW OTHERS AT
C                                       A LATER TIME).  NOTE THAT THIS
C                                       IS CURRENTLY ONLY SUPPORTED
C                                       FOR THE SINGLE RESPONSE
C                                       VARIABLE RAW DATA CASE
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C     UPDATED         --JULY      2019. CALL LIST TO DPJBS7, REDUCE
C                                       NUMBER OF SCRATCH ARRAYS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IRELAT
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASP2
      CHARACTER*4 ICASJB
      CHARACTER*4 ICASEB
      CHARACTER*4 IFLAGD
      CHARACTER*4 IFLAGV
      CHARACTER*4 IFLAGI
      CHARACTER*4 IBCABT
      CHARACTER*4 ICENSO
      CHARACTER*4 IMETHD
      CHARACTER*4 ILEVEL
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHHOR
      CHARACTER*4 IHHOR2
      CHARACTER*4 IHX
      CHARACTER*4 IHX2
      CHARACTER*4 IHXG
      CHARACTER*4 IHXG2
      CHARACTER*4 IH41
      CHARACTER*4 IH42
C
      CHARACTER*4 ISTATN(17)
      CHARACTER*4 ISTAT2(17)
C
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
      CHARACTER*40 INAME
      CHARACTER*60 ISTANM
      CHARACTER*60 IDIST
      CHARACTER*4  ISTADF
C
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
C
      REAL KSLOC
      REAL KSSCAL
C
C---------------------------------------------------------------------
C
      PARAMETER (NUMCHS=2)
      CHARACTER*4 INAM2(NUMCHS,6)
      CHARACTER*4 INCASE(NUMCHS)
      CHARACTER*4 INFLAV(NUMCHS)
      CHARACTER*4 INFLAD(NUMCHS)
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
      PARAMETER (MAXBGR=2)
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Z1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XLEVEL(MAXOBV)
C
      DIMENSION TEMP0(MAXOBV)
      DIMENSION TEMPZ0(MAXOBV)
      DIMENSION TEMPL(MAXOBV)
      DIMENSION TEMPZL(MAXOBV)
      DIMENSION RES1(MAXOBV)
      DIMENSION RES2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMPTH(MAXOBV)
      DIMENSION TEMPT2(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION QP(MAXOBV)
      DIMENSION XQP(MAXOBV)
      DIMENSION XQPLCL(MAXOBV)
      DIMENSION XQPUCL(MAXOBV)
      DIMENSION WEIGHH(MAXOBV)
      DIMENSION WEIGHV(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION ZTEMP1(MAXOBV)
      DIMENSION ZTEMP2(MAXOBV)
      DIMENSION ZTEMP3(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Z1(1))
      EQUIVALENCE (GARBAG(IGARB3),X1(1))
      EQUIVALENCE (GARBAG(IGARB4),XLEVEL(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP0(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMPZ0(1))
      EQUIVALENCE (GARBAG(IGARB7),RES1(1))
      EQUIVALENCE (GARBAG(IGARB8),RES2(1))
      EQUIVALENCE (GARBAG(IGAR10),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR12),TEMP5(1))
      EQUIVALENCE (GARBAG(JGAR13),TEMPTH(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP6(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP7(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMP8(1))
      EQUIVALENCE (GARBAG(JGAR17),TEMPT2(1))
      EQUIVALENCE (GARBAG(JGAR18),QP(1))
      EQUIVALENCE (GARBAG(JGAR19),XQP(1))
      EQUIVALENCE (GARBAG(JGAR20),WEIGHH(1))
      EQUIVALENCE (GARBAG(IGAR11),WEIGHV(1))
      EQUIVALENCE (GARBAG(IGAR12),TEMP(1))
      EQUIVALENCE (GARBAG(IGAR13),TEMP2(1))
      EQUIVALENCE (GARBAG(IGAR14),TEMP3(1))
      EQUIVALENCE (GARBAG(IGAR15),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGAR16),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGAR17),ZTEMP1(1))
      EQUIVALENCE (GARBAG(IGAR18),ZTEMP2(1))
      EQUIVALENCE (GARBAG(IGAR19),ZTEMP3(1))
      EQUIVALENCE (GARBAG(IGAR20),XQPLCL(1))
      EQUIVALENCE (GARBAG(IGAR21),XQPUCL(1))
      EQUIVALENCE (GARBAG(IGAR22),TEMPL(1))
      EQUIVALENCE (GARBAG(IGAR23),TEMPZL(1))
      EQUIVALENCE (GARBAG(IGAR24),XDESGN(1,1))
C
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      DOUBLE PRECISION DTEMP4(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
      EQUIVALENCE (DGARBG(IDGAR4),DTEMP4(1))
C
      PARAMETER(NPERC2=15)
      DIMENSION APERC(NPERC2)
      DIMENSION BPERC(NPERC2)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOMC.INC'
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
      DATA (ISTATN(I),I=1,17)/
     1'BSD ',
     1'BMEA',
     1'B975',
     1'B025',
     1'B001',
     1'B005',
     1'B01 ',
     1'B05 ',
     1'B10 ',
     1'B20 ',
     1'B50 ',
     1'B80 ',
     1'B90 ',
     1'B95 ',
     1'B99 ',
     1'B995',
     1'B999'/
      DATA (ISTAT2(I),I=1,17)/
     1'    ',
     1'N   ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    ',
     1'    '/
C
      DATA APERC/ 0.1,  0.5,  1.0,  2.5,  5.0, 10.0, 20.0, 50.0,
     1           80.0, 90.0, 95.0, 97.5, 99.0, 99.5, 99.9/
C
      DATA INCASE(1)/'LICA'/
      DATA (INAM2(1,J),J=1,6)/
     1'LINE','CALI','    ','    ','    ','    '/
      DATA INFLAV(1)/'TWO '/
      DATA INFLAD(1)/'OFF '/
C
      DATA INCASE(2)/'QUCA'/
      DATA (INAM2(2,J),J=1,6)/
     1'QUAD','CALI','    ','    ','    ','    '/
      DATA INFLAV(2)/'TWO '/
      DATA INFLAD(2)/'OFF '/
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFLAGD='OFF'
      IFLAGV='ONE'
      NGRPV=0
      ICENSO='OFF'
      ILEVEL='OFF'
      IMETHD='UNIM'
      IF(IPPLCN.EQ.'KAPL')IMETHD=IPPLCN
      ICASEB='NULL'
      ISUBN1='DPJB'
      ISUBN2='SP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXV2=5
      MINN2=2
      ICOLL=0
      ICOLH=0
      ICOLX=0
C
C               **********************************************
C               **  TREAT THE BOOTSTRAP/JACKNIFE PLOT CASE  **
C               **********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPJBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',
     1         A4,2X,A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBOOSS
   54   FORMAT('IBOOSS = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE IF OF THIS TYPE  **
C               **  AND BRANCH ACCORDINGLY.    **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'JACK')THEN
        ICASJB='JACK'
      ELSEIF(ICOM.EQ.'BOOT')THEN
        ICASJB='BOOT'
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      IF(NUMARG.LE.1)GOTO9000
C
C
C               ***********************************************
C               **  STEP 1B--                                **
C               **  EXTRACT THE COMMAND                      **
C               **  1) CHECK FOR STATISTICS/CASES UNIQUE TO  **
C               **     BOOTSTRAP/JACKNIFE COMMAND            **
C               **  2) CHECK FOR SUPPORTED STATISTICS IN     **
C               **     EXTSTA                                **
C               **  3) CHECK FOR SUPPORTED DISTRIBUTIONS IN  **
C               **     EXTDIS                                **
C               ***********************************************
C
      ISTEPN='1B'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CASE 1: STATISTICS UNIQUE TO BOOTSTRAP/JACKNIFE COMMAND
C             (I.E., NOT IN EXTSTA OR EXTDIS)
C
      DO100I=1,NUMCHS
        IROW=I
        IF(INAM2(I,1).NE.ICOM)GOTO100
        DO102J=2,6
          IF(INAM2(I,J).NE.'    ')GOTO102
          ITEMP=J-1
          GOTO104
  102   CONTINUE
        ITEMP=6
  104   CONTINUE
        ILASTC=0
        IF(ITEMP.GT.1)THEN
          DO108J=2,ITEMP
            IF(INAM2(I,J).NE.IHARG(J-1))GOTO100
  108     CONTINUE
          ILASTC=ITEMP-1
        ENDIF
        I1=ILASTC+1
        I2=ILASTC+2
        I3=ILASTC+3
        IF(IHARG(I1).EQ.'PLOT')THEN
          ILASTC=I1
          ICASPL=INCASE(IROW)
          IFLAGV=INFLAV(IROW)
          IFLAGD=INFLAD(IROW)
          CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
          ICASEB='STAT'
          IFOUND='YES'
          GOTO1000
        ELSEIF(IHARG(I1).EQ.'STAT'.AND.IHARG(I2).EQ.'PLOT')THEN
          ILASTC=I2
          ICASPL=INCASE(IROW)
          IFLAGV=INFLAV(IROW)
          IFLAGD=INFLAD(IROW)
          CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
          ICASEB='STAT'
          IFOUND='YES'
          GOTO1000
        END IF
C
  100 CONTINUE
C
C     CASE 2: SUPPORTED STATISTICS
C
C             EXTRACT THE DESIRED STATISTIC
C
C             SEARCH FOR WORD "PLOT".
C
      JMIN=1
      JMAX=NUMARG
C
      DO200I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          JMAX=I-1
          ILASTC=I
          IFOUND='YES'
          GOTO209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GOTO9000
  209 CONTINUE
C
      IFOUND='NO'
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1            ICASPL,ISTANM,ISTANR,ISTADF,IFOUND,ILOCV,
     1            ISUBRO,IBUGG3,IERROR)
C
      IF(IFOUND.EQ.'YES')THEN
        ICASEB='STAT'
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,251)
  251   FORMAT('***** AFTER CALL EXTSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)ICASPL,ISTANR,ILOCV,IFOUND,ICASEB
  252   FORMAT('ICASPL,ISTANR,ILOCV,IFOUND,ICASEB = ',
     1         A4,2I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'YES')GOTO1000
C
C     CASE 3: DISTRIBUTIONAL BOOTSTRAP CASES
C
C             SEARCH FOR:
C
C             1) PPCC PLOT (DEFAULT)
C             2) KS PLOT
C             3) ANDERSON-DARLING PLOT
C             4) CENSORED (CURRENTLY SUPPORTED FOR PPCC PLOT ONLY)
C             5) MAXIMUM LIKELIHOOD
C
      ICASP2='PPCC'
      JMAX2=JMAX
C
      DO300I=1,JMAX2
        IF(I.LT.NUMARG .AND. IHARG(I).EQ.'STAT' .AND.
     1         IHARG(I+1).EQ.'PLOT')THEN
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'CENS')THEN
          ICENSO='ON'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'KS')THEN
          ICASP2='KS'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'KOLM' .AND.
     1         IHARG(I+1).EQ.'SMIR')THEN
          ICASP2='KS'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'AD')THEN
          ICASP2='AD'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'ANDE' .AND.
     1         IHARG(I+1).EQ.'DARL')THEN
          ICASP2='AD'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'PPCC')THEN
          ICASP2='PPCC'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'MAXI' .AND.
     1         IHARG(I+1).EQ.'LIKE')THEN
          ICASP2='MLE'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'MLE ')THEN
          ICASP2='MLE '
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'ML  ')THEN
          ICASP2='MLE '
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ENDIF
  300 CONTINUE
  309 CONTINUE
      IFLAGV='ONE'
      IF(ICENSO.EQ.'ON')IFLAGV='TWO'
C
      CALL EXTDIS(ICOM,ICOM2,IHARG,IHARG2,NUMARG,JMIN,JMAX,
     1            ICASPL,IDIST,NUMSHA,IFOUND,ILOCV,
     1            ISUBRO,IBUGG3,IERROR)
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,351)
  351   FORMAT('***** AFTER CALL EXTDIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,352)ICASPL,ICASP2,NUMSHA,IDIST
  352   FORMAT('ICASPL,ICASP2,NUMSHA,IDIST = ',2(A4,2X),I8,2X,A60)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'YES')THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
        GOTO9000
      ENDIF
C
C               ***************************************************
C               **  STEP 3--EXTRACT THE SHAPE PARAMETERS FOR     **
C               **          THE SPECIFIED DISTRIBUTION.          **
C               ***************************************************
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHP='PPLO'
      IHP2='C   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        PPLOC=0.0
      ELSE
        PPLOC=VALUE(ILOCV)
      ENDIF
      IHP='PPSC'
      IHP2='ALE '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        PPSCAL=1.0
      ELSE
        PPSCAL=VALUE(ILOCV)
        IF(PPSCAL.LE.0.0)PPSCAL=1.0
      ENDIF
C
      IHP='KSLO'
      IHP2='C   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        KSLOC=CPUMIN
      ELSE
        KSLOC=VALUE(ILOCV)
      ENDIF
      IHP='KSSC'
      IHP2='ALE '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        KSSCAL=CPUMIN
      ELSE
        KSSCAL=VALUE(ILOCV)
        IF(KSSCAL.LE.0.0)KSSCAL=1.0
      ENDIF
C
      IFLAGL=0
      AL=CPUMIN
      IF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'3WEI')THEN
        IF(IWEIGL.EQ.'ON')THEN
          IHP='L   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'NO')AL=VALUE(ILOCP)
          IF(AL.LE.0.0)THEN
            AL=CPUMIN
          ELSE
            IFLAGL=1
          ENDIF
        ENDIF
      ENDIF
C
      IF(ICASPL.EQ.'GMCL' .OR. ICASPL.EQ.'TRAP' .OR.
     1       ICASPL.EQ.'GTRA' .OR. ICASPL.EQ.'UTSP' .OR.
     1       ICASPL.EQ.'GLGP' .OR.
     1       ICASPL.EQ.'PARE' .OR. ICASPL.EQ.'PAR2'
     1  )THEN
        CONTINUE
      ELSE
        IHP='A   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          A=0.0
        ELSE
          A=VALUE(ILOCV)
        ENDIF
C
        IHP='B   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          B=1.0
        ELSE
          B=VALUE(ILOCV)
        ENDIF
C
      ENDIF
C
      IF(NUMSHA.GE.1)THEN
        CALL EXTPA2(ICASPL,IDIST,A,B,
     1              SHAP11,SHAP12,SHAP21,SHAP22,
     1              SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,
     1              IGETDF,ICONDF,IGOMDF,IKATDF,
     1              IGIGDF,IGEODF,
     1              ISUBRO,IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,361)
  361     FORMAT('***** AFTER CALL EXTPA2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,362)SHAP11,SHAP12
  362     FORMAT('SHAP11,SHAP12 = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      ISTANR=1
C
 1000 CONTINUE
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      INAME='BOOTSTRAP PLOT'
      IF(ICASPL.EQ.'JACK')INAME='JACKNIFE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
CCCCC IFLAGE=1
      IFLAGE=0
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1002)NQ,NUMVAR
 1002   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO1005I=1,NUMVAR
            WRITE(ICOUT,1007)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
 1007       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
 1005     CONTINUE
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 3--                   **
C               **  CREATE THE VARIABLES       **
C               *********************************
C
      IF(ICASEB.EQ.'STAT')THEN
        NRESP=ISTANR
        NGRPV=NUMVAR-NRESP
      ELSE
        NRESP=1
        NCEN=0
        NLEVEL=0
        IF(ICENSO.EQ.'ON')NCEN=1
        IF(ICASPL.EQ.'BFWE')THEN
          IF(IBFWTY.EQ.'ON' .AND. NUMVAR.GT.1 .AND.
     1       IFLAGM.EQ.0)THEN
             NLEVEL=1
             ILEVEL='ON'
          ENDIF
          NGRPV=NUMVAR-NRESP-NCEN-NLEVEL
        ELSE
          NGRPV=NUMVAR-NRESP-NCEN
        ENDIF
C
        IF(NGRPV.LT.0 .OR. NGRPV.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2510)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,521)
  521     FORMAT('      THE NUMBER OF CLASS VARIABLES IS LESS THAN ',
     1           'ZERO OR GREATER THAN TWO.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,523)NGROUP
  523     FORMAT('      THE NUMBER OF CLASS VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1101)NRESP,NGRPV,ICASPL,IBOOGR
 1101   FORMAT('NRESP,NGRPV,ICASPL,IBOOGR = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFLAGI='DEPE'
      IF(NRESP.GE.2 .AND. NGRPV.EQ.0)THEN
        IF(ISTADF.EQ.'ON'   .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMEA' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMDM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMED' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DTRM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DWNM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DGEO' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DHAR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DHDL' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBIW' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSD ' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DRMS' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DVAR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DAAD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DAAM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMAD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DIQR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBIM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBIS' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DPBN' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DGSD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DRAN' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMDR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DQSE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DQUA' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSKE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DGSK' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DPSK' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DKUR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DEKU' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DRSD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSDM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DRVA' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DVAM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMIN' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DMAX' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DEXT' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DCVA' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DCOU' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSUM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DPRO' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'10LD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'12LD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'15LD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'17LD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'20LD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'10SD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'12SD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'15SD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'17SD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'20SD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSN ' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DQN ' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DLPL' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DLPV' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DLPS' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBOR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DTSD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DPER' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D1DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D2DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D3DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D4DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D5DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D6DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D7DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D8DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'D9DE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DLHI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DUHI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DLQU' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DUQU' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSSQ' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DRSC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DQQR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'ORSE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'ODRA' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'RATI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LOSE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LODR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KS2S' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KSCV' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'CS2S' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'CC2S' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'CP2S' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FTES' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FTPV' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FTCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2TTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2TCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2T2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2TLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2TUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MWTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MWUS' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MWCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MW2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MWLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MWUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KLTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KLCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KL2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KLLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'KLUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SRTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SRCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SR2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SRLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SRUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'METE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'MECD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'ME2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2SFR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2F2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FMAT' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LMAT' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FNOM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LNOM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'WOSM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'PDIF' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2CTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2CCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2C2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2CLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'2CUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DCDI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DIDI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DQDI' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DAMD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DPRE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSNR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSHM' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DSHR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'HEDG' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'BCHG' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'COHD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'GLAS' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBLC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'DBUC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'HESE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'HELC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'HEUC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(IFLAGI.EQ.'INDE')IFLAGD='INDE'
      ENDIF
C
C     NOTE 2011/10: IDENTIFY "SUMMARY" STATISTICS THAT ARE BASED
C                   ON MEAN, SD, AND SAMPLE SIZE VALUES.
C
      IF(ICASPL.EQ.'DHHD' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'DSSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'DSMM' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'DSLA' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MPSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MPSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MPAU' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MMPS' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MMPA' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'VRSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'VARU' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GCIS' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GCIN' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'BOBS' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'BOB ' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'BCPS' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'BCP ' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MMES' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'MMEA' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'SESE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'SCEB' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GDSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GNSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GDS1' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GDS2' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'GDEA' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'FWSE' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'FAIR' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'1LNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'1UNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'1KNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'2LNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'2UNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
      IF(ICASPL.EQ.'2KNT' .AND. NUMVAR.EQ.3)IFLAGI='SUMM'
C
      IF(NRESP.GT.NUMVAR)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2510)
 2510   FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2511)NRESP
 2511   FORMAT('      THE NUMBER OF RESPONSE VARIABLES EXPECTED: ',I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2513)NUMVAR
 2513   FORMAT('      THE NUMBER OF RESPONSE VARIABLES GIVEN:    ',I5)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NGRPV.LT.0 .OR. NGRPV.GT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2510)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2521)
 2521   FORMAT('      THE NUMBER OF GROUP VARIABLES IS LESS THAN ',
     1         'ZERO OR GREATER THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2523)NGRPV
 2523   FORMAT('      THE NUMBER OF GROUP VARIABLES GIVEN: ',I5)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NMAX=NRIGHT(1)
      IF(IFLAGI.EQ.'DEPE')THEN
        DO2530I=1,NUMVAR
          IF(NRIGHT(I).NE.NMAX)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2510)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2531)IVARN1(I),IVARN2(I),NRIGHT(I)
 2531       FORMAT('      VARIABLE ',A4,A4,' HAS ',I8,' OBSERVATIONS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2533)NRIGHT(1)
 2533       FORMAT('      THE EXPECTED NUMBER OF OBSERVATIONS: ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
 2530   CONTINUE
      ELSE
        NMAX=MAX(NRIGHT(1),NRIGHT(2))
      ENDIF
C
 2650 CONTINUE
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        ISTEPN='26'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1111)NMAX,IFLAGI
 1111   FORMAT('NMAX,IFLAGI = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      J=0
      J2=0
      J3=0
      IMAX=NMAX
      IF(NQ.LT.NMAX)IMAX=NQ
      ICNT=1
      IF(NRESP.EQ.2 .OR. NCENS.EQ.1)ICNT=2
C
      DO2660I=1,IMAX
C
C       FIRST RESPONSE VARIABLE
C
        IF(ISUB(I).EQ.1 .AND. I.LE.NRIGHT(1))THEN
          J=J+1
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
        ENDIF
        ICOLC=1
C
C       SECOND RESPONSE VARIABLE
C
        IF(NRESP.GE.2 .AND. ISUB(I).EQ.1 .AND. I.LE.NRIGHT(2))THEN
          ICOLC=ICOLC+1
          J2=J2+1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Z1(J2)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Z1(J2)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Z1(J2)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Z1(J2)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Z1(J2)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Z1(J2)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Z1(J2)=TAGPLO(I)
        ENDIF
C
C       THIRD RESPONSE VARIABLE
C
        IF(NRESP.GE.3 .AND. ISUB(I).EQ.1 .AND. I.LE.NRIGHT(3))THEN
          ICOLC=ICOLC+1
          J3=J3+1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)X1(J3)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)X1(J3)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)X1(J3)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)X1(J3)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)X1(J3)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)X1(J3)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)X1(J3)=TAGPLO(I)
        ENDIF
C
C       LENGTH VARIABLE
C
        IF(ILEVEL.EQ.'ON' .AND. ISUB(I).EQ.1)THEN
          ICOLC=ICOLC+1
          IJ=MAXN*(NRIGHT(ICOLC)-1)+I
          IF(NRIGHT(ICOLC).LE.MAXCOL)X1(J)=V(IJ)
          IF(NRIGHT(ICOLC).EQ.MAXCP1)X1(J)=PRED(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP2)X1(J)=RES(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP3)X1(J)=YPLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP4)X1(J)=XPLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP5)X1(J)=X2PLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP6)X1(J)=TAGPLO(I)
        ENDIF
C
C
C       CENSORING VARIABLE
C
        IF(NCENS.EQ.1 .AND. ISUB(I).EQ.1)THEN
          ICOLC=ICOLC+1
          IJ=MAXN*(NRIGHT(ICOLC)-1)+I
          IF(NRIGHT(ICOLC).LE.MAXCOL)X1(J)=V(IJ)
          IF(NRIGHT(ICOLC).EQ.MAXCP1)X1(J)=PRED(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP2)X1(J)=RES(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP3)X1(J)=YPLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP4)X1(J)=XPLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP5)X1(J)=X2PLOT(I)
          IF(NRIGHT(ICOLC).EQ.MAXCP6)X1(J)=TAGPLO(I)
        ENDIF
C
        IF(NGRPV.GE.1 .AND. ISUB(I).EQ.1)THEN
          DO2665K=1,NGRPV
            IJ=MAXN*(ICOLR(ICOLC+K)-1)+I
            IF(ICOLR(ICOLC+K).LE.MAXCOL)XDESGN(J,K)=V(IJ)
            IF(ICOLR(ICOLC+K).EQ.MAXCP1)XDESGN(J,K)=PRED(I)
            IF(ICOLR(ICOLC+K).EQ.MAXCP2)XDESGN(J,K)=RES(I)
            IF(ICOLR(ICOLC+K).EQ.MAXCP3)XDESGN(J,K)=YPLOT(I)
            IF(ICOLR(ICOLC+K).EQ.MAXCP4)XDESGN(J,K)=XPLOT(I)
            IF(ICOLR(ICOLC+K).EQ.MAXCP5)XDESGN(J,K)=X2PLOT(I)
            IF(ICOLR(ICOLC+K).EQ.MAXCP6)XDESGN(J,K)=TAGPLO(I)
 2665     CONTINUE
        ENDIF
C
 2660 CONTINUE
C
      NLOCAL=J
      NLOCA2=J2
C
C               *******************************************************
C               **  STEP 28--                                        **
C               **  COMPUTE THE APPROPRIATE STATISTIC PLOT STATISTIC--*
C               **  (MEAN, STANDARD DEVIATION, RANGE, OR CUSUM).     **
C               **  COMPUTE CONFIDENCE LINES.                        **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
C               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S      **
C               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE,**
C               **  AND THE UPPER CONFIDENCE LINE.                   **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
C               *******************************************************
C
      ISTEPN='28'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHP='ALPH'
      IHP2='A   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHA=0.05
      ELSE
        ALPHA=VALUE(ILOCP)
      ENDIF
      IF(ALPHA.LT.0.0 .OR. ALPHA.GT.1.0)ALPHA=0.05
      IF(ALPHA.GT.0.5)ALPHA=1.0-ALPHA
C
      IHP='LOWL'
      IHP2='IMIT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALOWLM=CPUMIN
      ELSE
        ALOWLM=VALUE(ILOCP)
      ENDIF
C
      IHP='UPPL'
      IHP2='IMIT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        AUPPLM=CPUMIN
      ELSE
        AUPPLM=VALUE(ILOCP)
      ENDIF
C
      IHP='ALPH'
      IHP2='ASV '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHSV=CPUMIN
      ELSE
        ALPHSV=VALUE(ILOCP)
      ENDIF
C
      NPERC=0
      IF(IQUAVR.EQ.'NONE')THEN
        NPERC=0
      ELSEIF(IQUAVR.EQ.'DEFAULT')THEN
        QP(1)=0.5/100.0
        QP(2)=1.0/100.0
        QP(3)=2.5/100.
        QP(4)=5.0/100.0
        QP(5)=10.0/100.0
        QP(6)=20.0/100.0
        QP(7)=30.0/100.0
        QP(8)=40.0/100.0
        QP(9)=50.0/100.0
        QP(10)=60.0/100.0
        QP(11)=70.0/100.0
        QP(12)=80.0/100.0
        QP(13)=90.0/100.0
        QP(14)=95.0/100.0
        QP(15)=97.5/100.0
        QP(16)=99.0/100.0
        QP(17)=99.5/100.0
        NPERC=17
      ELSE
        IH41=IQUAVR(1:4)
        IH42=IQUAVR(5:8)
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH41,IH42,IHWUSE,
     1       IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1       ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
        IF(IERROR.EQ.'YES')THEN
          NPERC=0
        ELSE
          ICOLQP=IVALUE(ILOCV)
          NPERC=IN(ILOCV)
          ICNT=0
          DO4180I=1,NPERC
            IJ=MAXN*(ICOLQP-1)+I
            ICNT=ICNT+1
            IF(ICOLQP.LE.MAXCOL)QP(ICNT)=V(IJ)
            IF(ICOLQP.EQ.MAXCP1)QP(ICNT)=PRED(I)
            IF(ICOLQP.EQ.MAXCP2)QP(ICNT)=RES(I)
            IF(ICOLQP.EQ.MAXCP3)QP(ICNT)=YPLOT(I)
            IF(ICOLQP.EQ.MAXCP4)QP(ICNT)=XPLOT(I)
            IF(ICOLQP.EQ.MAXCP5)QP(ICNT)=X2PLOT(I)
            IF(ICOLQP.EQ.MAXCP6)QP(ICNT)=TAGPLO(I)
            IF(QP(ICNT).LE.0.0 .OR. QP(ICNT).GE.100.0)THEN
              ICNT=ICNT-1
            ENDIF
 4180     CONTINUE
          NPERC=ICNT
          IWRITE='OFF'
          CALL MAXIM(QP,NPERC,IWRITE,QPMAX,IBUGG3,IERROR)
          IF(QPMAX.GT.1.0 .AND. QPMAX.LE.100.0)THEN
            DO4183II=1,NPERC
              QP(II)=QP(II)/100.0
 4183       CONTINUE
          ENDIF
C
        ENDIF
      ENDIF
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEB.EQ.'STAT')THEN
C
        CALL DPJBS6(Y1,Z1,X1,XDESGN,NLOCAL,NLOCA2,
     1              NRESP,NGRPV,ICASPL,
     1              ISIZE,ICONT,
     1              ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,IFLAGD,
     1              TEMP,TEMP2,TEMPL,TEMP3,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,MAXBGR,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              Y,X,D,NPLOTP,NPLOTV,
     1              TEMP0,TEMPZ0,TEMPZL,RES1,RES2,TEMP4,TEMPTH,TEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,DTEMP4,
     1              APERC,BPERC,NPERC2,
     1              BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,
     1              B80,B90,B95,B975,B99,B995,B999,
     1              ICAPSW,ICAPTY,IFORSW,PVAR,IVARN1,IVARN2,ISTANM,
     1              ISUBRO,IBUGG3,IERROR)
      ELSE
        CALL DPJBS7(Y1,X1,XLEVEL,XDESGN,NLOCAL,NRESP,NGRPV,
     1              ICASPL,ICASP2,IDIST,
     1              ICENSO,ISIZE,ICONT,NPERC,KSLOC,KSSCAL,
     1              IMETHD,ILEVEL,
     1              ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,
     1              TEMP,TEMP2,TEMP0,TEMPZ0,TEMPL,TEMPZL,
     1              QP,XQP,XQPLCL,XQPUCL,
     1              TEMP3,XTEMP1,XTEMP2,XTEMP3,TEMP4,
     1              ZTEMP1,ZTEMP2,ZTEMP3,TEMP5,TEMPT2,TEMP7,
     1              TEMP8,WEIGHH,RES1,RES2,TEMP6,TEMPTH,
     1              MAXNXT,MAXBGR,
     1              ITEMP1,DTEMP1,DTEMP2,DTEMP3,
     1              YLOWLM,YUPPLM,A,B,MINMAX,
     1              SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1              SHAPE6,SHAPE7,NUMSHA,
     1              SHAP11,SHAP12,SHAP21,SHAP22,
     1              Y,X,D,NPLOTP,NPLOTV,
     1              APERC,BPERC,NPERC2,
     1              BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,
     1              B80,B90,B95,B975,B99,B995,B999,
     1              ICAPSW,ICAPTY,IFORSW,PVAR,IVARN1,IVARN2,
     1              CLLIMI,CLWIDT,IRELAT,
     1              IFLAGL,AL,
     1              ISUBRO,IBUGG3,IERROR)
      ENDIF
C
C  AUTOMATICALLY SAVE CERTAIN PERCENTILE PARAMETERS.  MARCH 1998
C  JANUARY 2005: ONLY SAVE IF 1 PARAMETER IS ESTIMARED (E.G.,
C                DISTRIBUTIONAL FITTING HAS 2 TO 4 PARAMETERS)
C
C
C               ***************************************
C               **  STEP 51--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      CUTOFF=REAL(I1MACH(9))
C
      ISTEPN='51'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPAR.GT.1)GOTO5199
      IF(ICASEB.NE.'STAT')GOTO5199
      DO5100IPASS=1,17
        IH=ISTATN(IPASS)
        IH2=ISTAT2(IPASS)
        DO5150I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')GOTO5180
 5150   CONTINUE
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,5151)
 5151     FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5152)
 5152     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5153)MAXNAM
 5153     FORMAT('      NAMES MUST BE AT MOST ',I8,'.  SUCH WAS NOT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5155)
 5155     FORMAT('      THE CASE HERE--THE MAXIMUM ALLOWABLE NUMBER OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5156)
 5156     FORMAT('      NAMES HAS JUST BEEN EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5157)
 5157     FORMAT('      SUGGESTED ACTION--ENTER     STATUS     TO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5158)
 5158     FORMAT('      DETERMINE THE IMPORTANT (VERSUS UNIMPORTANT)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5160)
 5160     FORMAT('      VARIABLES AND PARAMETERS, AND THEN REUSE SOME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5161)
 5161     FORMAT('      OF THE NAMES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5162)
 5162     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,5163)(IANS(II),II=1,MIN(80,IWIDTH))
 5163       FORMAT('      ',80A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        NUMNAM=NUMNAM+1
        ILOC=NUMNAM
        IHNAME(ILOC)=IH
        IHNAM2(ILOC)=IH2
        IUSE(ILOC)='P'
        IF(IPASS.EQ.1)VALUE(ILOC)=BSD
        IF(IPASS.EQ.2)VALUE(ILOC)=BMEAN
        IF(IPASS.EQ.3)VALUE(ILOC)=B975
        IF(IPASS.EQ.4)VALUE(ILOC)=B025
        IF(IPASS.EQ.5)VALUE(ILOC)=B001
        IF(IPASS.EQ.6)VALUE(ILOC)=B005
        IF(IPASS.EQ.7)VALUE(ILOC)=B01
        IF(IPASS.EQ.8)VALUE(ILOC)=B05
        IF(IPASS.EQ.9)VALUE(ILOC)=B10
        IF(IPASS.EQ.10)VALUE(ILOC)=B20
        IF(IPASS.EQ.11)VALUE(ILOC)=B50
        IF(IPASS.EQ.12)VALUE(ILOC)=B80
        IF(IPASS.EQ.13)VALUE(ILOC)=B90
        IF(IPASS.EQ.14)VALUE(ILOC)=B95
        IF(IPASS.EQ.15)VALUE(ILOC)=B99
        IF(IPASS.EQ.16)VALUE(ILOC)=B995
        IF(IPASS.EQ.17)VALUE(ILOC)=B999
        VAL=VALUE(ILOC)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=VAL+0.5
        IF(VAL.GT.CUTOFF)IVAL=CUTOFF
        IF(VAL.LT.(-CUTOFF))IVAL=(-CUTOFF)
        IVALUE(ILOC)=IVAL
        GOTO5100
C
 5180   CONTINUE
        IF(IPASS.EQ.1)VALUE(I2)=BSD
        IF(IPASS.EQ.2)VALUE(I2)=BMEAN
        IF(IPASS.EQ.3)VALUE(I2)=B975
        IF(IPASS.EQ.4)VALUE(I2)=B025
        IF(IPASS.EQ.5)VALUE(I2)=B001
        IF(IPASS.EQ.6)VALUE(I2)=B005
        IF(IPASS.EQ.7)VALUE(I2)=B01
        IF(IPASS.EQ.8)VALUE(I2)=B05
        IF(IPASS.EQ.9)VALUE(I2)=B10
        IF(IPASS.EQ.10)VALUE(I2)=B20
        IF(IPASS.EQ.11)VALUE(I2)=B50
        IF(IPASS.EQ.12)VALUE(I2)=B80
        IF(IPASS.EQ.13)VALUE(I2)=B90
        IF(IPASS.EQ.14)VALUE(I2)=B95
        IF(IPASS.EQ.15)VALUE(I2)=B99
        IF(IPASS.EQ.16)VALUE(I2)=B995
        IF(IPASS.EQ.17)VALUE(I2)=B999
        VAL=VALUE(I2)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=VAL+0.5
        IF(VAL.GT.CUTOFF)IVAL=CUTOFF
        IF(VAL.LT.(-CUTOFF))IVAL=(-CUTOFF)
        IVALUE(I2)=IVAL
        GOTO5100
C
 5100 CONTINUE
 5199 CONTINUE
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
 9012   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',
     1         A4,2X,A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,IBOOSS,ICASJB
 9013   FORMAT('IFOUND,IERROR,IBOOSS,ICASJB = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISIZE,NUMVAR,NRESP,NGRPV
 9015   FORMAT('ISIZE,NUMVAR,NRESP,NGRPV = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.LE.0)THEN
          DO9025I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBS3(TEMP1,N1,ICASJB,IJACIN,ISEED,TEMP2,N2,
     1                  INDX,AINDEX,
     1                  IBUGG3,IERROR)
C
C     PURPOSE--GENERATE 1 JACKNIFE  SUBSAMPLE OF SIZE N1-1
C              OR       1 BOOTSTRAP SUBSAMPLE OF SIZE N1
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C     UPDATED         --JULY      2002. ADD AN INDEX VARIABLE.  USE
C                                       FOR CASES WHERE NEED TO
C                                       KEEP TWO OR MORE RESPONSE
C                                       VARIABLES DEPENDENT (E.G.,
C                                       CORRELATION KEEPS PAIRING
C                                       INTACT).
C     UPDATED         --AUGUST    2005. DUNRAN WAS FIXED TO GO FROM
C                                       0 TO N.  THIS ROUTINE WAS
C                                       MODIFIED TO CALL A VERSION
C                                       THAT GOES FROM 1 TO N.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASJB
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
CCCCC INCLUDE 'DPCOPA.INC'
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION AINDEX(*)
      DIMENSION INDX(*)
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
      IF(IBUGG3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPJBS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N1,IJACIN,ICASJB,IBUGG3
   52   FORMAT('N1,IJACIN,ICASJB,IBUGG3 = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ISEED
   54   FORMAT('ISEED = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GT.0)THEN
          DO55I=1,N1
            WRITE(ICOUT,56)I,TEMP1(I)
   56       FORMAT('I,TEMP1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  CHECK THE INPUT NUMBER FOR ERRORS           **
C               **************************************************
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN DPJBS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE INPUT RAW DATA SAMPLE SIZE WAS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)N1
 1113   FORMAT('      THE INPUT RAW DATA SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ICASJB.EQ.'JACK')THEN
        IF(IJACIN.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1122)
 1122     FORMAT('      THE INPUT JACKNIFE INDEX WAS NON-POSITIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1123)N1
 1123     FORMAT('      THE INPUT JACKNIFE INDEX = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  GENERATE THE JACKNIFE OR BOOTSTRAP SAMPLE   **
C               **************************************************
C
      IF(ICASJB.EQ.'JACK')THEN
        J=0
        DO1211I=1,N1
          IF(I.EQ.IJACIN)GOTO1211
          J=J+1
          TEMP2(J)=TEMP1(I)
          INDX(J)=I
 1211   CONTINUE
        N2=J
      ELSE
        CALL DUNRA2(N1,N1,ISEED,AINDEX)
        DO1221I=1,N1
          J=AINDEX(I)+0.5
          TEMP2(I)=TEMP1(J)
          INDX(I)=J
 1221   CONTINUE
        N2=N1
      ENDIF
C
C               *******************
C               **   STEP 90--   **
C               **   EXIT        **
C               *******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N1,N2,IJACIN,ISEED,ICASJB
 9012   FORMAT('N1,N2,IJACIN,ISEED,ICASJB = ',4I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GT.0)THEN
          DO9021I=1,N1
            WRITE(ICOUT,9022)I,TEMP1(I),AINDEX(I),INDX(I)
 9022       FORMAT('I,TEMP1(I),AINDEX(I),INDX(I) = ',I8,2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
        IF(N2.GT.0)THEN
          DO9031I=1,N2
            WRITE(ICOUT,9032)I,TEMP2(I)
 9032       FORMAT('I,TEMP2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9031     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,XIDTEM,Y2,X2,D2)
C
C     PURPOSE--ADD A COMPUTED POINT TO THE OUTPUT PLOT VECTORS
C              FOR THE JACKNIFE AND BOOTSTRAP PLOTS.
C     CAUTION--THE INPUT ARGUMENT J CHANGES WITHIN
C              THIS ROUTINE AND IS ALSO AN OUTPUT ARGUMENT.
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C     UPDATED         --MARCH     2003. FOR REPLICATION CASE, SET
C                                       TAGPLOT (D2) TO REFLECT
C                                       REPLICATION NUMBER
C     UPDATED         --JANUARY   2005. SET D2 FOR CASE WHERE MORE
C                                       THAN ONE STATISTIC ESTIMATED
C                                       (E.G., DISTRIBUTIONAL FITS),
C                                       TAGID IDENTIFIES WHICH
C                                       STATISTIC
C
C---------------------------------------------------------------------
C
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
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
      IF(NUMSET.LE.0)GOTO1100
      GOTO1200
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  TREAT THE CASE WHEN HAVE NO (= FULL DATA) SUBSET  **
C               **************************************************
C
 1100 CONTINUE
CCCCC IF(ISET.LE.NUMSET)GOTO1110
CCCCC GOTO1120
 1110 CONTINUE
      J=J+1
      Y2(J)=RIGHT
      IF(TAGID.EQ.1.0)THEN
        J2=J2+1
      ENDIF
      X2(J)=J2
CCCCC D2(J)=1.0
      D2(J)=TAGID
      GOTO1190
 1120 CONTINUE
      GOTO9000
CCCCC J=J+1
CCCCC Y2(J)=RIGHT
CCCCC X2(J)=XIDTEM(1)
CCCCC D2(J)=2.0
CCCCC J=J+1
CCCCC Y2(J)=RIGHT
CCCCC X2(J)=XIDTEM(NUMSET)
CCCCC D2(J)=2.0
CCCCC GOTO1190
 1190 CONTINUE
      GOTO9000
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  TREAT THE CASE WHEN HAVE 2 OR MORE SUBSETS  **
C               **************************************************
C
 1200 CONTINUE
      IF(ISET.LE.NUMSET)GOTO1210
      GOTO1220
 1210 CONTINUE
      J=J+1
      Y2(J)=RIGHT
      X2(J)=XIDTEM(ISET) + (TAGID-1.0)/10.0
CCCCC D2(J)=1.0
      D2(J)=(TAGID-1.0)*REAL(NUMSET) + REAL(ISET)
      GOTO1290
 1220 CONTINUE
      GOTO9000
CCCCC J=J+1
CCCCC Y2(J)=RIGHT
CCCCC X2(J)=XIDTEM(1)
CCCCC D2(J)=2.0
CCCCC J=J+1
CCCCC Y2(J)=RIGHT
CCCCC X2(J)=XIDTEM(NUMSET)
CCCCC D2(J)=2.0
CCCCC GOTO1290
 1290 CONTINUE
      GOTO9000
C
C               *******************
C               **   STEP 90--   **
C               **   EXIT        **
C               *******************
C
 9000 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPJBS5(ISET1,ISET2,NUMSE1,NUMSE2,J,RIGHT,XIDTEM,
     1                  XIDTE2,Y2,X2,D2)
C
C     PURPOSE--ADD A COMPUTED POINT TO THE OUTPUT PLOT VECTORS
C              FOR THE JACKNIFE AND BOOTSTRAP PLOTS.
C              THIS IS A SPECIAL VERSION OF DPJBS4 FOR THE CASE
C              WHEN THERE ARE EXACTLY TWO GROUP VRIABLES.
C     CAUTION--THE INPUT ARGUMENT J CHANGES WITHIN
C              THIS ROUTINE AND IS ALSO AN OUTPUT ARGUMENT.
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
C     VERSION NUMBER--2003/7
C     ORIGINAL VERSION--JULY      2003.
C
C---------------------------------------------------------------------
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
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
      IF(ISET1.LE.0 .OR. ISET2.LE.0)GOTO9000
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  TREAT THE CASE WHEN HAVE 2 GROUPS           **
C               **************************************************
C
      AINC=0.4/REAL(NUMSE2)
      ASTRT=XIDTEM(ISET1) - 0.2
      XTEMP=ASTRT + REAL(ISET2-1)*AINC
      J=J+1
      Y2(J)=RIGHT
      X2(J)=XTEMP
      ITEMP=(ISET1-1)*NUMSE2 + ISET2
      D2(J)=REAL(ITEMP)
C
C               *******************
C               **   STEP 90--   **
C               **   EXIT        **
C               *******************
C
 9000 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPJBS6(Y,Z,Z2,XDESGN,N,N2,NUMV2,NGRPV,ICASPL,
     1                  ISIZE,ICONT,
     1                  ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,IFLAGD,
     1                  TEMP,TEMPZ,TEMPZ2,XIDTEM,XTEMP1,XTEMP2,XTEMP3,
     1                  MAXNXT,MAXBGR,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  Y2,X2,D2,NPLOTP,NPLOTV,
     1                  TEMP0,TEMPZ0,TMPZ20,RES1,RES2,
     1                  TEMP4,TEMPTH,TEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,DTEMP4,
     1                  APERC,BPERC,NPERC,
     1                  BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,
     1                  B80,B90,B95,B975,B99,B995,B999,
     1                  ICAPSW,ICAPTY,IFORSW,PID,IVARID,IVARI2,ISTANM,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A JACKNIFE OR BOOTSTRAP PLOT
C              (SEE DPJBSP FOR ALLOWABLE TYPES)
C
C              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
C                    IT PERFORMS THE BOOTSTRAP FOR "STATISTICS" AND
C                    A FEW SPECIAL FITTING/CALIBRATION CASES.  THE
C                    DISTRIBUTIONAL BOOTSTRAP IS EXTRACTED TO DPJBS7.
C
C                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
C                    SIMPLIFY THE CODE A BIT AS WELL.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/02
C     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED FROM DPJBS2
C     UPDATED         --JULY      2010. IN ADDITION TO PLOT,
C                                       GENERATE A NUMERIC TABLE
C     UPDATED         --SEPTEMBER 2010. ACCOMODATE UP TO 3 RESPONSE
C                                       VARIABLES
C     UPDATED         --OCTOBER   2011. SUPPORT FOR PERCENTILE-t
C                                       CONFIDENCE INTERVALS
C     UPDATED         --OCTOBER   2011. SUPPORT FOR SMOOTHED BOOTSTRAP
C     UPDATED         --OCTOBER   2011. SUPPORT FOR "SUMMARY" STATISTICS
C     UPDATED         --JUNE      2017. FIX BUG IN "UNPAIRED" SAMPLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBCABT
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IFLAGD
      CHARACTER*4 IFLAGI
      CHARACTER*4 ISUBRO
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*(*) ISTANM
C
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IFLAGV
      CHARACTER*4 IBCASV
      CHARACTER*4 IBOOC2
      CHARACTER*4 ICASZZ
      CHARACTER*4 IOP
C
      CHARACTER*4 ICASJB
      CHARACTER*4 ICASRA
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IVRBSV
      CHARACTER*4 IDS4SV
      CHARACTER*25 IFORMT
      CHARACTER*25 IFORMZ
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XDESGN(MAXNXT,MAXBGR)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XIDTEM(MAXNXT,MAXBGR)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMPTH(*)
      DIMENSION TEMP6(*)
      DIMENSION PID(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
      DOUBLE PRECISION DTEMP4(*)
C
      INTEGER N
      INTEGER NUMSE1(10)
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DIMENSION TEMP0(*)
      DIMENSION TEMPZ0(*)
      DIMENSION TMPZ20(*)
      DIMENSION RES1(*)
      DIMENSION RES2(*)
C
      PARAMETER (MAXPAR=1)
      DIMENSION ZMEAN(MAXPAR)
      DIMENSION ZMED(MAXPAR)
      DIMENSION ZSD(MAXPAR)
      DIMENSION ZMAD(MAXPAR)
      DIMENSION NFAIL(MAXPAR)
      REAL KSLOC
      REAL KSSCAL
C
      PARAMETER(NUMCLI=3)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*80 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*50 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      CHARACTER*50 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUZ(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
C
      DIMENSION APERC(*)
      DIMENSION BPERC(*)
C
      PARAMETER (NUMALP=6)
      DIMENSION ALPHAV(NUMALP)
      DIMENSION ALOWPA(NUMALP,1)
      DIMENSION AUPPPA(NUMALP,1)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOST.INC'
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
      DATA ALPHAV /0.50, 0.25, 0.10, 0.05, 0.01, 0.001/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='JBS6'
      ISUBN2='    '
      IVRBSV=IVRBCM
      IDS4SV=IDS4CM
      IVRBCM='OFF'
      IDS4CM='OFF'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPJBS6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,ICASJB,IBCABT,IBOOSS
   71   FORMAT('IBUGG3,ISUBRO,ICASJB,IBCABT,IBOOSS = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,N2,NUMV2,ISIZE,NGRPV
   72   FORMAT('N,N2,NUMV2,ISIZE,NGRPV = ',4I8,I4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)ICASPL,ICONT,IFLAGI,IFLAGD
   79   FORMAT('ICASPL,ICONT,IFLAGI,IFLAGD = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I),XDESGN(I,1),Z(I),Z2(I)
   74     FORMAT('I, Y(I),XDESGN(I,1),Z(I),Z2(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
        WRITE(ICOUT,78)IBOOCI,PBOOTS,IBOOSM,PBOOSM
   78   FORMAT('IBOOCI,PBOOTS,IBOOSM,PBOOSM = ',2(A4,2X,G15.7))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IWRITE='OFF'
CCCCC NOTE 2011/10: FOR STATISTIC CASE, THERE IS A SINGLE PARAMETER
CCCCC               COMPUTED.
CCCCC NUMPAR=NUMV2
      NUMPAR=1
      I2=0
      ISIZE2=0
      NUMSET=0
      DO120I=1,NGRPV
        NUMSE1(I)=0
  120 CONTINUE
C
      NACC=0
      NREJ=0
C
C     1) IF t-PERCENTILE REQUESTED, CHECK THAT A POSITIVE
C        STANDARD DEVIATION HAS BEEN ENTERED
C     2) IF BCA AND t-PERCENTILE BOTH SPECIFIED, USE t-PERCENTILE
C
      IBCASV=IBCABT
      IBOOC2=IBOOCI
C
      IF(IBOOCI.EQ.'T   ')THEN
        IF(PBOOTS.LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
  111     FORMAT('***** WARNING IN BOOTSTRAP/JACKNIFE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,112)
  112     FORMAT('      WHEN THE t-PERCENTILE CONFIDENCE INTERVALS ',
     1           'ARE RESQUESTED,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,113)
  113     FORMAT('      A POSITIVE STANDARD DEVIATION FOR THE ',
     1           'SPECIFIED STATISTIC MUST BE GIVEN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,114)
  114     FORMAT('      TO DO THIS, ENTER THE COMMAND:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,115)
  115     FORMAT('          SET BOOTSTRP T PERCENTILE STANDARD ',
     1           'DEVIATION  <value>')
          CALL DPWRST('XXX','BUG ')
          IBOOCI='PERC'
        ELSE
          IBCABT='OFF'
        ENDIF
      ENDIF
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)N
  134   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR THE GROUP VARIABLE (USUALLY VAR. 2)           **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.           **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NGRPV.GE.1)THEN
        NUMSET=1
        DO170J=1,NGRPV
          CALL DISTIN(XDESGN(1,J),N,IWRITE,XIDTEM(1,J),NUMSE1(J),
     1                IBUGG3,IERROR)
          CALL SORT(XIDTEM(1,J),NUMSE1(J),XIDTEM(1,J))
          NUMSET=NUMSET*NUMSE1(J)
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
            WRITE(ICOUT,171)NGRPV,J,NUMSE1(J),NUMSET
  171       FORMAT('NGRPV,J,NUMSE1(J),NUMSET = ',4I8)
            CALL DPWRST('XXX','BUG ')
            DO172K=1,NUMSE1(J)
              WRITE(ICOUT,173)K,XIDTEM(K,J)
  173         FORMAT('K,XIDTEM(K,J) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
  172       CONTINUE
          ENDIF
C
          IF(NUMSE1(J).LT.1 .OR. NUMSE1(J).GE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,181)
  181       FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,182)
  182       FORMAT('      THE NUMBER OF SETS FOR THE GROUP ONE ',
     1             'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,183)
  183       FORMAT('      IS ZERO OR EQUAL TO THE NUMBER OF POINTS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,184)NUMSE1(J)
  184       FORMAT('      NUMBER OF SETS = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
  170   CONTINUE
C
      ENDIF
C
      AN=N
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
        WRITE(IOUNI3,203)100.0*(1.0-ALPHA)
  203   FORMAT('      BCa BOOTSTRAP ',F7.2,'% CONFIDENCE INTERVALS:')
        WRITE(IOUNI3,205)ALPHA/2.0,(1.0-ALPHA/2)
  205   FORMAT('SIGNIFICANCE LEVELS = (',F6.3,',',F6.3,')')
        WRITE(IOUNI3,207)
  207   FORMAT(6X,'LOWER',11X,'UPPER')
        WRITE(IOUNI3,209)
  209   FORMAT(3X,'CONFIDENCE',5X,'CONFIDENCE',11X,'^',14X,'^')
        WRITE(IOUNI3,211)
  211   FORMAT(6X,'LIMIT',11X,'LIMIT',12X,'Z0',13X,'A0',6X,'ALPHA1',
     1         3X,'ALPHA2')
        WRITE(IOUNI3,213)
  213   FORMAT('---------------------------------------------------',
     1         '-------------------------')
      ENDIF
C
C               ******************************************
C               **  STEP 11--                           **
C               **  COMPUTE THE SPECIFIED STATISTIC     **
C               **  FOR EACH SUBSET OF THE DATA, AND    **
C               **  THEN FOR THE FULL DATA SET          **
C               ******************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      ICNT9=0
C
      J=0
      J2=0
      ISETMX=NUMSET+1
      NMAX=MAX(N,N2)
C
      DO11000ISET=1,ISETMX
C
        CALL DPJBS8(ISETMX,ISET,NUMSET,NUMSE1,N,N2,NGRPV,
     1              MAXNXT,MAXBGR,NUMV2,
     1              Y,Z,Z2,XDESGN,XIDTEM,TEMP0,TEMPZ0,TMPZ20,
     1              NS2,NSS2,NI,NI2,ISET1,ISET2,
     1              ISUBRO,IBUGG3,IERROR)
 
C
        NRESAM=NS2
        IF(ICASJB.EQ.'BOOT')NRESAM=IBOOSS
C
C       AUGUST 2002.  SIMPLIFY CODE BY USING "CMPSTA" TO COMPUTE
C       STATISTIC.  NOTE THAT THE FOLLOWING DISTINCT CASES ARE
C       SUPPORTED:
C
C       1) STATISTIC COMPUTED FROM A SINGLE RESPONSE VARIABLE
C          (MOST CASES IN THIS CATEGORY, E.G., THE MEAN)
C       2) STATISTIC COMPUTED FROM TWO RESPONSE VARIABLES,
C          RESPONSES ARE PAIRED (E.G., THE CORRELATION).
C       3) STATISTIC COMPUTED FROM TWO RESPONSE VARIABLES, THE
C          RESPONSES ARE NOT PAIRED (I.E., SAMPLE THE TWO VARIABLES
C          SEPARATELY).  CURRENTLY, NO CASES FOR THIS.
C       4) LINEAR AND QUADRATIC CALIBRATION HANDLED SEPARATELY.
C       5) LINEAR SLOPE, LINEAR CORRELATION, LINEAR RESSD,
C          LINEAR INTERCEPT HANDLED SEPARATELY.
C       6) SUMMARY STATISTIC CASE HANDLED SEPARATELY (USE THE
C          PARAMETERIC BOOTSTRAP)
C
C
C       HANDLE LINEAR CALIBRATION, QUADRATIC CALIBRATION SEPARATELY.
C
        IF(ICASPL.EQ.'LICA')GOTO12240
        IF(ICASPL.EQ.'QUCA')GOTO12240
C
C       FOR REMAINING CASES, DEFINE
C
C       ICASE = 1  - SINGLE RESPONSE VARIABLE
C       ICASE = 2  - PAIRED RESPONSE VARIABLES
C       ICASE = 3  - UNPAIRED RESPONSE VARIABLES
C       ICASE = 4  - LINEAR CORRELATION, LINEAR INTERCEPT,
C                    LINEAR SLOPE, LINEAR RESSD
C       ICASE = 5  - SUMMARY STATISTICS
C
        ICASE=1
        IF(NUMV2.GE.2)ICASE=2
        IF(IFLAGD.EQ.'DEPE')THEN
          ICASE=2
        ELSEIF(IFLAGD.EQ.'INDE')THEN
           IF(IBOOGR.EQ.'DEPE')THEN
             ICASE=2
           ELSE
             ICASE=3
           ENDIF
        ENDIF
        IF(IFLAGI.EQ.'SUMM')ICASE=5
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
          WRITE(ICOUT,1107)ICASE,IBOOGR,IFLAGD
 1107     FORMAT('ICASE,IBOOGR,IFLAGD ',I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C  CASES WITH TWO UNPAIRED RESPONSE VARIABLES (NOT NECESSARILY OF
C  SAME SIZE)
C
        IF(ICASPL.EQ.'ORSE' .OR. ICASPL.EQ.'ODRA' .OR.
     1     ICASPL.EQ.'LOSE' .OR. ICASPL.EQ.'LODR' .OR.
     1     ICASPL.EQ.'DBPR' .OR. ICASPL.EQ.'WOSM')THEN
           ICASE=3
           IBCABT='OFF'
        ENDIF
C
        IF(ICASPL.EQ.'LIIN' .OR. ICASPL.EQ.'LISL' .OR.
     1     ICASPL.EQ.'LIIS' .OR. ICASPL.EQ.'LISS' .OR.
     1     ICASPL.EQ.'CINT' .OR. ICASPL.EQ.'CSD ' .OR.
     1     ICASPL.EQ.'LIRE' .OR. ICASPL.EQ.'LICO')ICASE=4
C
C       FOR BCA OR t-PERCENTILE CONFIDENCE INTERVALS, COMPUTE
C       FULL-SAMPLE STATISTIC.
C
        IF(IBCABT.EQ.'ON'.AND.ICASJB.EQ.'BOOT')THEN
          CALL CMPSTA(
     1         TEMP0,TEMPZ0,TMPZ20,XTEMP1,XTEMP2,XTEMP3,
     1         MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,
     1         ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1         DTEMP1,DTEMP2,DTEMP3,
CCCCC1         IQUAME,IQUASE,PSTAMV,
     1         THETHT,
     1         ISUBRO,IBUGG3,IERROR)
          NBELOW=0
        ELSEIF(IBOOCI.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
          CALL CMPSTA(
     1         TEMP0,TEMPZ0,TMPZ20,XTEMP1,XTEMP2,XTEMP3,
     1         MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,
     1         ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1         DTEMP1,DTEMP2,DTEMP3,
     1         STATD,
     1         ISUBRO,IBUGG3,IERROR)
          STATSD=PBOOTS
        ENDIF
C
        IF(ICASE.EQ.4)THEN
          IF(ICASPL.EQ.'CINT' .OR. ICASPL.EQ.'CSD')THEN
            CALL MEAN(TEMP0,NS2,IWRITE,ALPHA,IBUGG3,IERROR)
            CALL SD(TEMP0,NS2,IWRITE,SDALPH,IBUGG3,IERROR)
            BETA0=0.0
            SDBETA=0.0
          ELSE
            CALL LINFIT(TEMP0,TEMPZ0,NS2,
     1                  ALPHA,BETA,XRESSD,XRESDF,
     1                  CCXY,SDALPH,SDBETA,CCALBE,
     1                  ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA0=BETA
          ENDIF
          DO11031I=1,NS2
            RES1(I)=TEMP0(I)-(ALPHA0+BETA0*TEMPZ0(I))
11031     CONTINUE
        ELSE
          DO11033I=1,NS2
            RES1(I)=TEMP0(I)
11033     CONTINUE
        ENDIF
C
        TAGID=1.0
        DO11361IRESAM=1,NRESAM
C
C         STEP 1: RESAMPLE ORIGINAL DATA.
C
          IF(ICASE.EQ.5)THEN
            DO11300IROW=1,NS2
              NTEMP=INT(TMPZ20(IROW)+0.5)
              AMEAN=TEMP0(IROW)
              ASD=TEMPZ0(IROW)
              CALL NORRAN(NTEMP,ISEED,XTEMP1)
              DO11301IJ=1,NTEMP
                XTEMP1(IJ)=AMEAN + ASD*XTEMP1(IJ)
11301         CONTINUE
              CALL MEAN(XTEMP1,NTEMP,IWRITE,XMEAN,IBUGG3,IERROR)
              CALL SD(XTEMP1,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
              TEMP(IROW)=XMEAN
              TEMPZ(IROW)=XSD
              TEMPZ2(IROW)=REAL(NTEMP)
11300       CONTINUE
            NS3=NS2
            NS32=NS3
          ELSE
            CALL DPJBS3(RES1,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,
     1                  TEMP4,IBUGG3,IERROR)
            IF(IBOOSM.EQ.'ON')THEN
              CALL NORRAN(NS3,ISEED,XTEMP1)
              IF(PBOOSM.EQ.CPUMIN)THEN
                AFACT=1.0/SQRT(REAL(NS3))
              ELSE
                AFACT=PBOOSM
              ENDIF
              DO11311IJ=1,NS3
                TEMP(IJ)=TEMP(IJ) + AFACT*XTEMP1(IJ)
11311         CONTINUE
            ENDIF
          ENDIF
C
C         CREATE ADDITIONAL RESPONSE VARIABLES FOR SPECIAL CASES
C         WHERE NEEDED.
C
          IF(ICASE.EQ.2)THEN
            NS32=NS3
            DO11363IJ=1,NS3
              TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
11363       CONTINUE
          ELSEIF(ICASE.EQ.3)THEN
            NS32=NSS2
            IF(ICASPL.EQ.'WOSM')THEN
              DO11323IJ=1,NS3
                TEMPZ(IJ)=TEMPZ0(IJ)
11323         CONTINUE
            ELSE
              CALL DPJBS3(TEMPZ0,NSS2,ICASJB,IRESAM,ISEED,TEMPZ,NS32,
     1                    ITEMP1,TEMP4,IBUGG3,IERROR)
              IF(IBOOSM.EQ.'ON')THEN
                CALL NORRAN(NS32,ISEED,XTEMP1)
                IF(PBOOSM.EQ.CPUMIN)THEN
                  AFACT=1.0/SQRT(REAL(NS32))
                ELSE
                  AFACT=PBOOSM
                ENDIF
                DO11321IJ=1,NS3
                  TEMPZ(IJ)=TEMPZ(IJ) + AFACT*XTEMP1(IJ)
11321           CONTINUE
              ENDIF
            ENDIF
          ELSEIF(ICASE.EQ.4)THEN
            DO11368I=1,NS3
              TEMP0(I)=(ALPHA0+BETA0*TEMPZ0(I))+TEMP(I)
11368       CONTINUE
          ENDIF
C
C         STEP 2: COMPUTE THE STATISTIC
C
          CALL CMPSTA(
     1         TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1         MAXNXT,NS3,NS32,NS32,NUMV2,ICASPL,
     1         ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1         DTEMP1,DTEMP2,DTEMP3,
CCCCC1         IQUAME,IQUASE,PSTAMV,
     1         RIGHT,
     1         ISUBRO,IBUGG3,IERROR)
C
C         STEP 3: COMPARE COMPUTED STATISTIC FROM BOOTSTRAP SAMPLE
C                 TO STATISTIC FROM ORIGINAL DATA (FOR BCA) AND ALSO
C                 COMPUTE PLOT COORDINATES.
C
          IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
            IF(RIGHT.LT.THETHT)NBELOW=NBELOW+1
            TEMP6(IRESAM)=RIGHT
          ENDIF
          IF(NGRPV.LE.1)THEN
            CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,XIDTEM(1,1),
     1                  Y2,X2,D2)
          ELSEIF(NGRPV.EQ.2)THEN
            CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,RIGHT,
     1                  XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
          ENDIF
11361   CONTINUE
C
C       FOR BCA CONFIDENCE INTERVAL, COMPUTE:
C       1) Z0HAT
C       2) JACKNIFE ESTIMATES
C
        IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
          CALL NORPPF(REAL(NBELOW)/REAL(NRESAM),Z0HAT)
          ICASZZ='JACK'
          DO11371IRESAM=1,NS2
C
            CALL DPJBS3(RES1,NS2,ICASZZ,IRESAM,ISEED,TEMP,NS3,ITEMP1,
     1                  TEMP4,IBUGG3,IERROR)
            IF(ICASE.EQ.2)THEN
              DO11373IJ=1,NS2
                TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
                IF(NUMV2.GE.3)THEN
                  TEMPZ2(IJ)=TMPZ20(ITEMP1(IJ))
                ENDIF
11373         CONTINUE
            ELSEIF(ICASE.EQ.3)THEN
              CALL DPJBS3(TEMPZ0,NS22,ICASJB,IRESAM,ISEED,TEMPZ,NS32,
     1                    ITEMP1,TEMP4,IBUGG3,IERROR)
              IF(NUMV2.GE.3)THEN
                CALL DPJBS3(TMPZ20,NS22,ICASJB,IRESAM,ISEED,TEMPZ2,NS32,
     1                      ITEMP1,TEMP4,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASE.EQ.4)THEN
              DO11378I=1,NS3
                TEMP0(I)=(ALPHA0+BETA0*TEMPZ0(I))+TEMP(I)
11378         CONTINUE
            ENDIF
C
            CALL CMPSTA(
     1           TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1           MAXNXT,NS3,NS3,NS3,NUMV2,ICASPL,
     1           ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1           DTEMP1,DTEMP2,DTEMP3,
CCCCC1           IQUAME,IQUASE,PSTAMV,
     1           RIGHT,
     1           ISUBRO,IBUGG3,IERROR)
            TEMPTH(IRESAM)=RIGHT
11371     CONTINUE
          CALL MEAN(TEMPTH,NS2,IWRITE,THETDT,IBUGG3,IERROR)
          DSUM1=0.0D0
          DSUM2=0.0D0
          DTHETM=DBLE(THETDT)
          DO11365I=1,NS2
            DTERM1=DBLE(TEMPTH(I))
            DSUM1 = DSUM1 + (DTHETM - DTERM1)**3
            DSUM2 = DSUM2 + (DTHETM - DTERM1)**2
11365     CONTINUE
          DTERM2 = DSUM1/(6.0D0*(DSUM2**1.5))
          A0HAT=REAL(DTERM2)
          CALL NORPPF(ALPHA/2.0,ALOWSL)
          CALL NORPPF(1.0 - ALPHA/2.0,AUPPSL)
          TERM1=Z0HAT + (Z0HAT + AUPPSL)/(1.0 - A0HAT*(Z0HAT+AUPPSL))
          CALL NORCDF(TERM1,ALPHA2)
          TERM1=Z0HAT + (Z0HAT + ALOWSL)/(1.0 - A0HAT*(Z0HAT+ALOWSL))
          CALL NORCDF(TERM1,ALPHA1)
          CALL PERCEN(100.0*ALPHA2,TEMP6,NS2,IWRITE,TEMP4,MAXNXT,
     1                BCAUL,IBUGG3,IERROR)
          CALL PERCEN(100.0*ALPHA1,TEMP6,NS2,IWRITE,TEMP4,MAXNXT,
     1                BCALL,IBUGG3,IERROR)
          IF(NGRPV.EQ.1)THEN
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2,
     1                         XIDTEM(ISET,1)
          ELSEIF(NGRPV.EQ.2)THEN
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2,
     1                         XIDTEM(ISET1,1),XIDTEM(ISET2,2)
          ELSE
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2
          ENDIF
11388     FORMAT(4E15.7,2F8.4,2F10.0)
C
C       2011/10: IMPLEMENT PERCENTILE T BOOTSTRAP:
C
C                   Z = (STAT(data) - STAT(boot))/SD(boot)
C
C                   LOWER CI: STAT(data) + SD(data)*Q(Z,alpha/2)
C                   UPPER CI: STAT(data) + SD(data)*Q(Z,1 - alpha/2)
C
        ELSEIF(IBOOCI.EQ.'T   ')THEN
        ENDIF
C
        GOTO79000
C
CCCCC   NOTE: FOR CALIBRATION, THERE ARE TWO METHODS FOR PERFORMING
CCCCC         THE BOOTSTRAP.
CCCCC
CCCCC         1) "RESI" USES EFROM METHOD OF RESAMPLING THE RESIDUALS.
CCCCC
CCCCC         2) "DATA" USES WU METHOD OF RESAMPLING THE ORIGINAL
CCCCC            Y AND X.
CCCCC
CCCCC         IN EITHER CASE, THE PARAMETER Y0 SHOULD BE PRE-DEFINED.
CCCCC
CCCCC         AFTER QUADRATIC FIT, QUADRATIC FORMULA IS:
CCCCC             X = (-b +/- SQRT(b**2 - 4*a*c))/(2*a)
C
12240   CONTINUE
C
        IHP='Y0  '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1       IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1       ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          GOTO9000
        ELSE
          Y0=VALUE(ILOCP)
        ENDIF
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
          DO12249I=1,NS2
            WRITE(ICOUT,12242)I,TEMP0(I),TEMPZ0(I)
12242       FORMAT('I,TEMP0(I),TEMPZ0(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
12249     CONTINUE
        ENDIF
C
        CALL MINIM(TEMPZ0,NS2,IWRITE,XLEFT,IBUGG3,IERROR)
        CALL MAXIM(TEMPZ0,NS2,IWRITE,XRIGHT,IBUGG3,IERROR)
C
        IF(IBOOME.EQ.'RESI')THEN
C
C  GENERATE FIT AND RESIDUALS FROM ORIGINAL DATA.
C
          IF(ICASPL.EQ.'LICA')THEN
            CALL LINFIT(TEMP0,TEMPZ0,NS2,
     1                  ALPHA,BETA,XRESSD,XRESDF,
     1                  CCXY,SDALPH,SDBETA,CCALBE,
     1                  ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA0=BETA
            DO12251I=1,NS2
              RES1(I)=TEMP0(I)-(ALPHA0+BETA0*TEMPZ0(I))
12251       CONTINUE
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
              WRITE(ICOUT,12533)ALPHA0,BETA0
12533         FORMAT('ALPHA0,BETA0 = ',2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSEIF(ICASPL.EQ.'QUCA')THEN
            CALL QUAFI2(TEMPZ0,TEMP0,NS2,
     1                  XTEMP1,
     1                  ALPHA,BETA1,BETA2,
     1                  ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA10=BETA1
            BETA20=BETA2
C
            C=ALPHA - Y0
            B=BETA1
            A=BETA2
            TERM1=B**2 - 4.0*A*C
            RIGH10=0.0
            RIGH20=0.0
            IF(TERM1.GE.0.0)THEN
              TERM1=SQRT(TERM1)
              RIGH10=(-B + TERM1)/(2*A)
              RIGH20=(-B - TERM1)/(2*A)
            ENDIF
            IF(RIGH10.GE.XLEFT .AND. RIGH10.LE.XRIGHT)THEN
              RIGHT0=RIGH10
            ELSEIF(RIGH20.GE.XLEFT .AND. RIGH20.LE.XRIGHT)THEN
              RIGHT0=RIGH20
            ELSE
              RIGHT0=RIGH10
            ENDIF
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB6')THEN
              WRITE(ICOUT,12262)RIGH10,RIGH20
12262         FORMAT('FULL SAMPLE ROOTS: RIGH10,RIGH20 = ',2E15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            DO12271I=1,NS2
              AJUNK1=TEMPZ0(I)
              RES1(I)=TEMP0(I)-(ALPHA0+BETA10*AJUNK1+BETA20*AJUNK1**2)
12271       CONTINUE
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
              WRITE(ICOUT,12273)ALPHA0,BETA10,BETA20
12273         FORMAT('ALPHA0,BETA10,BETA20 = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
C
C         RESAMPLE RESIDUALS.
C
          NREJ=0
          NNEG=0
          DO12281IRESAM=1,NRESAM
            CALL DPJBS3(RES1,NS2,ICASJB,IRESAM,ISEED,RES2,NS3,ITEMP1,
     1                  TEMP4,IBUGG3,IERROR)
            IF(ICASPL.EQ.'LICA')THEN
              DO12282I=1,NS3
                TEMP(I)=(ALPHA0+BETA0*TEMPZ0(I))+RES2(I)
12282         CONTINUE
              CALL LINFIT(TEMP,TEMPZ0,NS3,
     1                    ALPHA,BETA,XRESSD,XRESDF,
     1                    CCXY,SDALPH,SDBETA,CCALBE,
     1                    ISUBRO,IBUGG3,IERROR)
              A0=ALPHA
              A1=BETA
              RIGHT=(Y0-A0)/A1
C
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12283)IRESAM,ALPHA,BETA,RIGHT
12283           FORMAT('IRESAM,ALPHA0,BETA0,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
            ELSEIF(ICASPL.EQ.'QUCA')THEN
              DO12286I=1,NS3
                AJUNK1=TEMPZ0(I)
                TEMP(I)=(ALPHA0+BETA10*AJUNK1+BETA20*AJUNK1**2)+RES2(I)
12286         CONTINUE
              CALL QUAFI2(TEMPZ0,TEMP,NS3,
     1                    XTEMP1,
     1                    ALPHA,BETA1,BETA2,
     1                    ISUBRO,IBUGG3,IERROR)
              C=ALPHA - Y0
              B=BETA1
              A=BETA2
              TERM1=B**2 - 4.0*A*C
              IF(TERM1.EQ.0.0)THEN
                RIGHT=(-B + TERM1)/(2*A)
              ELSEIF(TERM1.GT.0.0)THEN
                TERM1=SQRT(TERM1)
                RIGH1=(-B + TERM1)/(2*A)
                RIGH2=(-B - TERM1)/(2*A)
                IF(RIGH1.GE.XLEFT .AND. RIGH1.LE.XRIGHT)THEN
                  IF(RIGH2.GE.XLEFT .AND. RIGH2.LE.XRIGHT)THEN
                    D1DIFF=ABS(RIGH1-RIGHT0)
                    D2DIFF=ABS(RIGH2-RIGHT0)
                    IF(D1DIFF.LE.D2DIFF)THEN
                      RIGHT=RIGH1
                    ELSE
                      RIGHT=RIGH2
                    ENDIF
                  ELSE
                    RIGHT=RIGH1
                  ENDIF
                ELSEIF(RIGH2.GE.XLEFT .AND. RIGH2.LE.XRIGHT)THEN
                  RIGHT=RIGH2
                ELSE
                  IF(RIGH1.GT.0.0 .AND. RIGH2.LE.0.0)THEN
                    RIGHT=RIGH1
                  ELSEIF(RIGH2.GT.0.0 .AND. RIGH1.LE.0.0)THEN
                    RIGHT=RIGH2
                  ELSE
                    D1DIFF=ABS(RIGH1-RIGHT0)
                    D2DIFF=ABS(RIGH2-RIGHT0)
                    IF(D1DIFF.LE.D2DIFF)THEN
                      RIGHT=RIGH1
                    ELSE
                      RIGHT=RIGH2
                    ENDIF
                  ENDIF
                ENDIF
                IF(RIGHT.LT.0)NNEG=NNEG+1
              ELSEIF(TERM1.LT.0.0)THEN
                NREJ=NREJ+1
                GOTO12281
              ENDIF
C
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12287)IRESAM,ALPHA,BETA1,BETA2,RIGHT
12287           FORMAT('IRESAM,ALPHA,BETA1,BETA2,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,12288)A,B,C,TERM1
12288           FORMAT('A, B, C, TERM1 = ',4G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
            ENDIF
C
            TAGID=1.0
            IF(NGRPV.LE.1)THEN
              CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,
     1                    XIDTEM(1,1),Y2,X2,D2)
            ELSE
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,RIGHT,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
            ENDIF
12281     CONTINUE
C
        ELSE
C
C         RESAMPLE ORIGINAL Y AND X VALUES (ROWS OF Y AND X SHOULD
C         REMAIN PAIRED).
C
          NNEG=0
          NREJ=0
          DO12291IRESAM=1,NRESAM
            CALL DPJBS3(TEMP0,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,
     1                  TEMP4,IBUGG3,IERROR)
            DO12292IJ=1,NS3
              TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
12292       CONTINUE
            IF(ICASPL.EQ.'LICA')THEN
              CALL LINFIT(TEMP,TEMPZ,NS3,
     1                    ALPHA,BETA,XRESSD,XRESDF,
     1                    CCXY,SDALPH,SDBETA,CCALBE,
     1                    ISUBRO,IBUGG3,IERROR)
              A0=ALPHA
              A1=BETA
              RIGHT=(Y0-A0)/A1
C
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB6')THEN
                WRITE(ICOUT,12293)IRESAM,ALPHA,BETA,RIGHT
12293           FORMAT('IRESAM,ALPHA0,BETA0,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
            ELSEIF(ICASPL.EQ.'QUCA')THEN
              CALL QUAFI2(TEMPZ,TEMP,NS3,
     1             XTEMP1,
     1             ALPHA,BETA1,BETA2,
     1             ISUBRO,IBUGG3,IERROR)
              C=ALPHA - Y0
              B=BETA1
              A=BETA2
              TERM1=B**2 - 4.0*A*C
              IF(TERM1.EQ.0.0)THEN
                RIGHT=(-B + TERM1)/(2*A)
              ELSEIF(TERM1.GT.0.0)THEN
                TERM1=SQRT(TERM1)
                RIGH1=(-B + TERM1)/(2*A)
                RIGH2=(-B - TERM1)/(2*A)
                IF(RIGH1.GE.XLEFT .AND. RIGH1.LE.XRIGHT)THEN
                  IF(RIGH2.GE.XLEFT .AND. RIGH2.LE.XRIGHT)THEN
                    IF(RIGH1.GT.0.0 .AND. RIGH2.LE.0.0)THEN
                      RIGHT=RIGH1
                    ELSEIF(RIGH2.GT.0.0 .AND. RIGH1.LE.0.0)THEN
                      RIGHT=RIGH2
                    ELSE
                      D1DIFF=ABS(RIGH1-RIGHT0)
                      D2DIFF=ABS(RIGH2-RIGHT0)
                      IF(D1DIFF.LE.D2DIFF)THEN
                        RIGHT=RIGH1
                      ELSE
                        RIGHT=RIGH2
                      ENDIF
                    ENDIF
                  ELSE
                    RIGHT=RIGH1
                  ENDIF
                ELSEIF(RIGH2.GE.XLEFT .AND. RIGH2.LE.XRIGHT)THEN
                  RIGHT=RIGH2
                ELSE
                  D1DIFF=ABS(RIGH1-RIGHT0)
                  D2DIFF=ABS(RIGH2-RIGHT0)
                  IF(D1DIFF.LE.D2DIFF)THEN
                    RIGHT=RIGH1
                  ELSE
                    RIGHT=RIGH2
                  ENDIF
                ENDIF
                IF(RIGHT.LT.0)NNEG=NNEG+1
              ELSEIF(TERM1.LT.0.0)THEN
                NREJ=NREJ+1
                GOTO12291
              ENDIF
C
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12287)IRESAM,ALPHA,BETA1,BETA2,RIGHT
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
            ENDIF
C
            TAGID=1.0
            IF(NGRPV.LE.1)THEN
              CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,
     1                    XIDTEM(1,1),Y2,X2,D2)
            ELSE
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,RIGHT,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
            ENDIF
C
12291     CONTINUE
C
        ENDIF
C
        IF(NREJ.GT.0)THEN
          WRITE(ICOUT,12301)
12301     FORMAT('***** WARNING FROM BOOTSTRAP PLOT--',
     1           'QUADRATIC CALIBRATION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12303)NREJ
12303     FORMAT('      FOR ',I8,' BOOTSTRAP SAMPLES, NO REAL ROOTS ',
     1           'FOR THE QUADRATIC EQUATION.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NNEG.GT.0)THEN
          WRITE(ICOUT,12301)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12305)NREJ
12305     FORMAT('      FOR ',I8,' BOOTSTRAP SAMPLES, NEGATIVE ROOT ',
     1           'SELECTED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO79000
C
79000   CONTINUE
C
C               ************************************************
C               **   STEP 19--                                **
C               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
C               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
C               **   TO DPST1F.DAT.                           **
C               ************************************************
C
CCCCC JANUARY 2005.  FOR UNGROUPED DATA, WRITE BOOTSTRAP ESTIMATES
CCCCC                TO FILE.  ALSO, ACCOMODATE CASE WHERE MORE
CCCCC                THAN ONE PARAMETER IS ESTIMATED.
C
        CALL DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,
     1              NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,
     1              APERC,BPERC,NPERC,
     1              BMEAN,BSD,BMIN,BMAX,BMAD,
     1              B001,B005,B01,B025,B05,B10,B20,B50,
     1              B80,B90,B95,B975,B99,B995,B999,
     1              ALOWPA,AUPPPA,ALPHAV,NUMALP,
     1              ZMEAN,ZMED,ZSD,ZMAD,NFAIL,
     1              ISUBRO,IBUGG3,IERROR)
C
         DO79003II=1,NUMPAR
           NFAIL(II)=NRESAM - NFAIL(II)
79003    CONTINUE
C
C       ************************************************
C       **   STEP 20--                                **
C       **   GENERATE  A NUMERIC TABLE OF THE RESULTS **
C       ************************************************
C
        IF(IPRINT.EQ.'OFF')GOTO11000
C
        ICNT9=ICNT9+1
        IF(ICNT9.EQ.1)THEN
          ITITLE(1:27)='Bootstrap Analysis for the '
           DO8211II=60,1,-1
            IF(ISTANM(II:II).NE.' ')THEN
               NCSTAT=II
               GOTO8219
            ENDIF
 8211     CONTINUE
          NCSTAT=1
 8219     CONTINUE
          IF(NCSTAT.GT.53)NCSTAT=53
          NSTRT=28
          NCTITL=NSTRT+NCSTAT-1
          ITITLE(NSTRT:NCTITL)=ISTANM(1:NCSTAT)
        ELSE
          ITITLE=' '
          NCTITL=0
        ENDIF
        ITITLZ=' '
        NCTITZ=0
        IF(IBOOSM.EQ.'ON')THEN
          IF(PBOOSM.EQ.CPUMIN)THEN
            ITITLZ='(Smoothed bootstrap with SD = 1/SQRT(N))'
            NCTITZ=40
          ELSE
            ITITLZ='(Smoothed bootstrap with SD = '
            WRITE(ITITLZ(31:45),'(G15.7)')PBOOSM
            ITITLZ(46:46)=')'
            NCTITZ=46
          ENDIF
        ELSEIF(IFLAGI.EQ.'SUMM')THEN
            ITITLZ='(Parametric Bootstrap for Summary Data)'
            NCTITZ=39
        ENDIF
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable One: '
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=31
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        IF(NUMV2.GE.2)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Response Variable Two: '
          WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(2)(1:4)
          WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(2)(1:4)
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ENDIF
        IF(NUMV2.GE.3)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Response Variable Three: '
          WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(3)(1:4)
          WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(3)(1:4)
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ENDIF
        IF(NGRPV.EQ.1)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable One (        ): '
          IF(ISET.LE.0 .OR. ISET.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+1)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+1)(1:4)
            AVALUE(ICNT)=XIDTEM(ISET,1)
            IDIGIT(ICNT)=NUMDIG
          ENDIF
          NCTEXT(ICNT)=34
        ENDIF
C
        IF(NGRPV.GE.2)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable One (        ): '
          IF(ISET1.LE.0 .OR. ISET1.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+1)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+1)(1:4)
          ENDIF
          NCTEXT(ICNT)=34
          AVALUE(ICNT)=XIDTEM(ISET1,1)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable Two (        ): '
          IF(ISET2.LE.0 .OR. ISET2.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+2)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+2)(1:4)
          ENDIF
          NCTEXT(ICNT)=34
          AVALUE(ICNT)=XIDTEM(ISET2,2)
          IDIGIT(ICNT)=NUMDIG
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Bootstrap Samples:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=REAL(NRESAM)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Observations:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=REAL(NS3)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Mean of Bootstrap Samples:'
        NCTEXT(ICNT)=26
        AVALUE(ICNT)=BMEAN
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Standard Deviation of Bootstrap Samples:'
        NCTEXT(ICNT)=40
        AVALUE(ICNT)=BSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Median of Bootstrap Samples:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=B50
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='MAD of Bootstrap Samples:'
        NCTEXT(ICNT)=25
        AVALUE(ICNT)=BMAD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Minimum of Bootstrap Samples:'
        NCTEXT(ICNT)=29
        AVALUE(ICNT)=BMIN
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Maximum of Bootstrap Samples:'
        NCTEXT(ICNT)=29
        AVALUE(ICNT)=BMAX
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        NUMROW=ICNT
        DO8321II=1,NUMROW
          NTOT(II)=15
 8321   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGG3,IERROR)
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
C
        ITITLE='Percent Points of the Bootstrap Samples'
        NCTITL=39
        NUMLIN=1
        NUMROW=15
        NUMCOL=3
        ITITL2(1,1)='Percent Point'
        ITITL2(1,2)=' '
        ITITL2(1,3)='Value'
        NCTIT2(1,1)=13
        NCTIT2(1,2)=1
        NCTIT2(1,3)=5
C
        NMAX=0
        DO2521II=1,NUMCOL
          VALIGN(II)='b'
          ALIGN(II)='r'
          NTOT(II)=15
          IF(II.EQ.2)NTOT(II)=5
          NMAX=NMAX+NTOT(II)
          IDIGIT(II)=NUMDIG
          ITYPCO(II)='NUME'
 2521   CONTINUE
        ITYPCO(2)='ALPH'
        IDIGIT(1)=1
        DO2523II=1,NUMROW
          DO2525JJ=1,NUMCOL
            NCVALU(II,JJ)=0
            IVALUZ(II,JJ)=' '
            NCVALU(II,JJ)=0
            AMAT(II,JJ)=0.0
            IF(JJ.EQ.2)THEN
              IVALUZ(II,JJ)='='
              NCVALU(II,JJ)=1
            ELSEIF(JJ.EQ.1)THEN
              AMAT(II,JJ)=APERC(II)
            ELSEIF(JJ.EQ.3)THEN
              AMAT(II,JJ)=BPERC(II)
            ENDIF
 2525     CONTINUE
 2523   CONTINUE
C
        IWHTML(1)=150
        IWHTML(2)=50
        IWHTML(3)=150
        IWRTF(1)=2000
        IWRTF(2)=IWRTF(1)+500
        IWRTF(3)=IWRTF(2)+2000
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        CALL DPDTA4(ITITL9,NCTIT9,
     1              ITITLE,NCTITL,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUZ,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              ISUBRO,IBUGG3,IERROR)
C
        CALL DPDT8B(ALOWPA(1,1),AUPPPA(1,1),ALPHAV,NUMALP,
     1              ICAPSW,ICAPTY,NUMDIG,
     1              ISUBRO,IBUGG3,IERROR)
C
11000 CONTINUE
C
      NPLOTP=J
      NPLOTV=3
C
      IOP='CLOS'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IFEEDB.EQ.'ON')THEN
C
          WRITE(ICOUT,8102)
 8102     FORMAT('THE FOLLOWING INFORMATION IS WRITTEN TO FILES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8104)
 8104     FORMAT('DPST1F.DAT: THE BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8106)
 8106     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',
     1           'TWO) COLUMNS IDENTIFY THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8112)
 8112     FORMAT('DPST2F.DAT: STATISTICS BASED ON BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8114)
 8114     FORMAT('            MEAN, SD, MEDIAN, B025, ',
     1           'B975, B05, B95, B005, B995')
          CALL DPWRST('XXX','BUG ')
          IF(NUMPAR.GT.1)THEN
            WRITE(ICOUT,8118)
 8118       FORMAT('            THE FIRST COLUMN IDENTIFIES THE ',
     1             'PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,8116)
 8116     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',
     1           'TWO) COLUMNS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8117)
 8117     FORMAT('            (AFTER THE PARAMETER ID) IDENTIFY ',
     1           'THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
C
        ENDIF
C
CCCCC ENDIF
C
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
C
        IF(IFEEDB.EQ.'ON')THEN
C
          WRITE(ICOUT,8102)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8131)
 8131     FORMAT('DPST3F.DAT: BCa CONFIDENCE INTERVALS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8132)
 8132     FORMAT('LOWER INTERVAL, UPPER INTERVAL, Z0HAT, A0HAT, ',
     1         'ALPHA1, ALPHA2, GROUP 1 ID, GROUP 2 ID')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8134)
 8134     FORMAT('WITH 4E15.7,2F8.4,2F10.0 FORMAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
C
        ENDIF
C
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IBCABT=IBCASV
      IBOOCI=IBOOC2
      IVRBCM=IVRBSV
      IDS4CM=IDS4SV
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO,ICASJB,IBOOSS
 9012   FORMAT('IBUGG3,ISUBRO,ICASJB,IBOOSS = ',A4,2X,A4,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICASPL,N,NUMSET,N2,IERROR
 9013   FORMAT('ICASPL,N,NUMSET,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP
 9014   FORMAT('NPLOTV,NPLOTP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NPLOTP
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBS7(Y,Z,XLEVEL,XDESGN,N,NUMV2,NGRPV,
     1                  ICASPL,ICASP2,IDIST,
     1                  ICENSO,ISIZE,ICONT,NPERC,KSLOC,KSSCAL,
     1                  IMETHD,ILEVEL,
     1                  ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,
     1                  TEMP,TEMPZ,TEMP0,TEMPZ0,TEMPL,TEMPZL,
     1                  QP,XQP,XQPLCL,XQPUCL,
     1                  XIDTEM,XTEMP1,XTEMP2,XTEMP3,TEMP4,
     1                  ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,
     1                  ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,ZTMP12,
     1                  MAXNXT,MAXBGR,
     1                  ITEMP1,DTEMP1,DTEMP2,DTEMP3,
     1                  YLOWLM,YUPPLM,A,B,MINMAX,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                  SHAPE6,SHAPE7,NUMSHA,
     1                  SHAP11,SHAP12,SHAP21,SHAP22,
     1                  Y2,X2,D2,NPLOTP,NPLOTV,
     1                  APERC,BPERC,NPERC2,
     1                  BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,
     1                  B80,B90,B95,B975,B99,B995,B999,
     1                  ICAPSW,ICAPTY,IFORSW,PID,IVARID,IVARI2,
     1                  CLLIMI,CLWIDT,IRELAT,
     1                  IFLAGL,AL,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A JACKNIFE OR BOOTSTRAP PLOT
C              (SEE DPJBSP FOR ALLOWABLE TYPES)
C
C              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
C                    IT PERFORMS THE BOOTSTRAP FOR "DISTRIBUTIONS".
C                    THE BOOTSTRAP FOR STATISTICS WAS EXTRACTED TO
C                    DPJBS6.
C
C                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
C                    SIMPLIFY THE CODE A BIT AS WELL.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/05
C     ORIGINAL VERSION--MAY       2010. EXTRACTED FROM DPJBS2
C     UPDATED         --AUGUST    2011. OPTION FOR ONE-SIDED PERCENTILES
C                                       (= ONE-SIDED TOLERANCE INTERVALS)
C     UPDATED         --AUGUST    2011. SOME MODIFICATIONS FOR BETTER
C                                       HANDLING CASES WHERE THERE ARE
C                                       ERRORS IN THE PARAMETER
C                                       ESTIMATES
C     UPDATED         --MARCH     2013. FOR WEIBULL, ADJUST SCALE
C                                       PARAMETER IF GAUGE LENGTH
C                                       OPTION SPECIFIED
C     UPDATED         --JULY      2019. CALL LIST TO CMPDIS
C     UPDATED         --JULY      2019. REMOVE ZTMP13, ZTMP14 FROM
C                                       CALL LIST
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*4 ICONT
      CHARACTER*4 ICENSO
      CHARACTER*4 IMETHD
      CHARACTER*4 ILEVEL
      CHARACTER*4 IFLAGI
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IRELAT
      CHARACTER*4 ISUBRO
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*60 IDIST
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IFLAGD
      CHARACTER*4 IFLAGV
      CHARACTER*4 IBCABT
      CHARACTER*4 ICASZZ
      CHARACTER*4 IOP
C
      CHARACTER*4 ICASJB
      CHARACTER*4 ICASRA
C
      CHARACTER*4 ILIMIT
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*25 IFORMT
      CHARACTER*25 IFORMZ
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION XLEVEL(*)
      DIMENSION XDESGN(MAXNXT,MAXBGR)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION QP(*)
      DIMENSION XQP(*)
      DIMENSION XQPLCL(*)
      DIMENSION XQPUCL(*)
C
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMP0(*)
      DIMENSION TEMPZ0(*)
      DIMENSION TEMPL(*)
      DIMENSION TEMPZL(*)
      DIMENSION TEMP4(*)
      DIMENSION XIDTEM(MAXNXT,MAXBGR)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION PID(*)
C
      DIMENSION ZTEMP1(*)
      DIMENSION ZTEMP2(*)
      DIMENSION ZTEMP3(*)
      DIMENSION ZTEMP4(*)
      DIMENSION ZTEMP5(*)
      DIMENSION ZTEMP6(*)
      DIMENSION ZTEMP7(*)
      DIMENSION ZTEMP8(*)
      DIMENSION ZTEMP9(*)
      DIMENSION ZTMP10(*)
      DIMENSION ZTMP11(*)
      DIMENSION ZTMP12(*)
C
      DIMENSION CLWIDT(*)
      DIMENSION CLLIMI(*)
C
      PARAMETER (MAXPAR=9)
      DIMENSION APERC(*)
      DIMENSION BPERC(*)
      PARAMETER (NUMALP=8)
      DIMENSION ALPHAV(NUMALP)
      DIMENSION ALOWPA(NUMALP,MAXPAR)
      DIMENSION AUPPPA(NUMALP,MAXPAR)
      DIMENSION ZMEAN(MAXPAR)
      DIMENSION ZMED(MAXPAR)
      DIMENSION ZSD(MAXPAR)
      DIMENSION ZMAD(MAXPAR)
      INTEGER   NFAIL(MAXPAR)
C
      CHARACTER*25 IPAR
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      INTEGER N
      INTEGER NUMSE1(10)
      INTEGER ITEMP1(*)
C
      REAL KSLOC
      REAL KSSCAL
      REAL KSLSAV
      REAL KSSSAV
C
      PARAMETER(NUMCLI=3)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*80 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*50 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      CHARACTER*50 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUZ(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOST.INC'
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
      DATA ALPHAV /0.50, 0.25, 0.10, 0.05, 0.025, 0.01, 0.005, 0.001/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='JBS7'
      ISUBN2='    '
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPJBS7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,ICASJB,IBOOSS
   71   FORMAT('IBUGG3,ISUBRO,ICASJB,IBOOSS = ',A4,2X,A4,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,ICASPL,NUMV2,ISIZE,ICONT,NGRPV,NUMSHA
   72   FORMAT('N,ICASPL,NUMV2,ISIZE,ICONT,NGRPV,NUMSHA = ',
     1         I8,2X,A4,I8,I8,2X,A4,2X,2I4)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,MIN(N,100)
          WRITE(ICOUT,74)I,Y(I),XDESGN(I,1),Z(I)
   74     FORMAT('I, Y(I),XDESGN(I,1),Z(I) = ',I8,3F15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
      IWRITE='OFF'
      NUMPAR=NUMV2
      I2=0
      ISIZE2=0
      NUMSET=0
      DO120I=1,NGRPV
        NUMSE1(I)=0
  120 CONTINUE
C
      NACC=0
      NREJ=0
C
C     SPECIFY DISTRIBUTIONS THAT ESTIMATE LOWER/UPPER LIMIT
C     PARAMETERS RATHER THAN LOCATION/SCALE
C
      ILIMIT='OFF'
      IF(ICASPL.EQ.'UNIF')ILIMIT='ON'
      IF(ICASPL.EQ.'BETA')ILIMIT='ON'
      IF(ICASPL.EQ.'TRIA')ILIMIT='ON'
      IF(ICASPL.EQ.'POWF')ILIMIT='ON'
      IF(ICASPL.EQ.'RPOW')ILIMIT='ON'
      IF(ICASPL.EQ.'JOSB')ILIMIT='ON'
      IF(ICASPL.EQ.'TSPO')ILIMIT='ON'
      IF(ICASPL.EQ.'TOPL')ILIMIT='ON'
      IF(ICASPL.EQ.'GTOL')ILIMIT='ON'
      IF(ICASPL.EQ.'RGTL')ILIMIT='ON'
      IF(ICASPL.EQ.'SLOP')ILIMIT='ON'
      IF(ICASPL.EQ.'OGIV')ILIMIT='ON'
      IF(ICASPL.EQ.'TSSL')ILIMIT='ON'
      IF(ICASPL.EQ.'TSOG')ILIMIT='ON'
      IF(ICASPL.EQ.'KUMA')ILIMIT='ON'
      IF(ICASPL.EQ.'UTSP')ILIMIT='ON'
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE MUST BE AT LEAST 5;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)N
  134   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR THE GROUP VARIABLE (USUALLY VAR. 2)           **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.           **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NGRPV.GE.1)THEN
        NUMSET=1
        DO170J=1,NGRPV
          CALL DISTIN(XDESGN(1,J),N,IWRITE,XIDTEM(1,J),NUMSE1(J),
     1                IBUGG3,IERROR)
          CALL SORT(XIDTEM(1,J),NUMSE1(J),XIDTEM(1,J))
          NUMSET=NUMSET*NUMSE1(J)
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
            WRITE(ICOUT,171)NGRPV,J,NUMSE1(J),NUMSET
  171       FORMAT('NGRPV,J,NUMSE1(J),NUMSET = ',4I8)
            CALL DPWRST('XXX','BUG ')
            DO172K=1,NUMSE1(J)
              WRITE(ICOUT,173)K,XIDTEM(K,J)
  173         FORMAT('K,XIDTEM(K,J) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
  172       CONTINUE
          ENDIF
C
          IF(NUMSE1(J).LT.1 .OR. NUMSE1(J).GE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,181)
  181       FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,182)
  182       FORMAT('      THE NUMBER OF SETS FOR THE GROUP ONE ',
     1             'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,183)
  183       FORMAT('      IS ZERO OR EQUAL TO THE NUMBER OF POINTS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,184)NUMSE1(J)
  184       FORMAT('      NUMBER OF SETS = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
  170   CONTINUE
C
      ENDIF
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
      ICNT9=0
C
      AN=N
      IF(NPERC.GT.999)NPEC=0
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      IF(NPERC.GE.1)THEN
        IFLAG4=1
      ENDIF
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C     HEADER FOR PERCENTILE FILE IF NEEDED
C
      IF(NPERC.GT.0)THEN
        IF(NGRPV.EQ.0)THEN
          IFORMT='(    E15.7)'
          IFORMZ='(    F15.3)'
          WRITE(IFORMT(3:5),'(I3)')NPERC
          WRITE(IFORMZ(3:5),'(I3)')NPERC
        ELSEIF(NGRPV.EQ.1)THEN
          IFORMT='(I8,1X,    E15.7)'
          IFORMZ='(9X,    F15.3)'
          WRITE(IFORMT(9:11),'(I3)')NPERC
          WRITE(IFORMZ(7:8),'(I3)')NPERC
        ELSEIF(NGRPV.EQ.2)THEN
          IFORMT='(I8,1X,I8,1X,    E15.7)'
          IFORMZ='(18X,    F15.3)'
          WRITE(IFORMT(15:17),'(I3)')NPERC
          WRITE(IFORMZ(8:9),'(I3)')NPERC
        ENDIF
        WRITE(IOUNI4,IFORMZ)(QP(JJ),JJ=1,NPERC)
      ENDIF
C
C               ******************************************
C               **  STEP 11--                           **
C               **  COMPUTE THE SPECIFIED STATISTIC     **
C               **  FOR EACH SUBSET OF THE DATA, AND    **
C               **  THEN FOR THE FULL DATA SET          **
C               ******************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FOR PARAMETRIC BOOTSTRAP, COMPUTE INITIAL PARAMETER
C     ESTIMATES BASED ON FULL SAMPLE.
C
      KSLSAV=KSLOC
      KSSSAV=KSSCAL
CCCCC IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF')THEN
      IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF' .AND.
     1   ICASJB.EQ.'BOOT')THEN
        CALL CMPDIS(Y,Z,XLEVEL,N,MAXNXT,ICASPL,ICASP2,
     1              XTEMP1,XTEMP2,XTEMP3,
     1              DTEMP1,DTEMP2,DTEMP3,ITEMP1,
     1              ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,
     1              ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,
     1              YLOWLM,YUPPLM,A,B,MINMAX,NUMSHA,
     1              SHAP11,SHAP12,SHAP21,SHAP22,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1              IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1              IEXPBC,IWEIBC,ICENTY,IDFTTY,
     1              MAXNXT,ICENSO,KSLOC,KSSCAL,IFORSW,ISEED,
     1              IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1              IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVEL,
     1              CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1              SH1,SH2,SH3,SH4,SH5,SH6,SH7,ALOC,ASCALE,STATVA,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,131)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,901)
  901     FORMAT('      FOR PARAMETRIC BOOTSTRAP, UNABLE TO COMPUTE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,903)
  903     FORMAT('      PARAMETER ESTIMATES FROM ORIGINAL SAMPLE.')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        IF(IFLAGL.EQ.1 .AND. AL.GT.0.0 .AND. SH1.GT.0.0)THEN
          ASCALE=AL**(1.0/SH1)*ASCALE
        ENDIF
C
        IF(ILIMIT.EQ.'ON' .AND.
     1    (ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD  ' .OR.
     1     ICASP2.EQ.'KS  '))THEN
          AVAL=KSLOC + KSSCAL
          KSSCAL=AVAL
        ENDIF
      ENDIF
C
      J=0
      J2=0
      ISETMX=NUMSET+1
      NMAX=N
C
      DO11000ISET=1,ISETMX
C
        ISTEPN='12'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPJBS8(ISETMX,ISET,NUMSET,NUMSE1,N,N,NGRPV,
     1              MAXNXT,MAXBGR,NUMV2,
     1              Y,Z,XLEVEL,XDESGN,XIDTEM,TEMP0,TEMPZ0,TEMPL,
     1              NS2,NSS2,NI,NI2,ISET1,ISET2,
     1              ISUBRO,IBUGG3,IERROR)
C
        NRESAM=NS2
        IF(ICASJB.EQ.'BOOT')NRESAM=IBOOSS
C
        IF(NPERC.GE.1)THEN
          IOP='OPEN'
          IFLAG1=0
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=1
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGG3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
C       SIMPLIFY CODE BY USING "CMPDIS" TO COMPUTE DISTRIBUTION
C       PARAMETERS.  NOTE THAT THE DISTRIBUTIONAL BOOTSTRAP
C       ASSUMES A SINGLE RESPONSE VARIABLE.  ALSO THE BCA CONFIDENCE
C       INTERVAL METHOD IS NOT CURRENTLY SUPPORTED FOR THE
C       DISTRIBUTIONAL BOOTSTRAP.
C
        TAGID=1.0
        DO11361IRESAM=1,NRESAM
C
C         STEP 1: THERE ARE TWO METHODS FOR RESAMPLING:
C
C                 1) RESAMPLE THE ORIGINAL DATA.
C                 2) GENERATE A RANDOM SAMPLE BASED ON PARAMETER
C                    ESTIMATES FROM FULL SAMPLE.
C
C                    THIS METHOD IS NOT SUPPORTED FOR CENSORED DATA.
C
          IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF' .AND.
     1       ICASJB.EQ.'BOOT')THEN
            CALL DPRAN2(ICASP2,ISEED,TEMP,NS2,ZTMP12,
     1                  A,B,MINMAX,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                  SHAPE6,SHAPE7,
     1                  IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                  ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1                  IGOMDF,IKATDF,IGIGDF,IGEODF,
     1                  IBUGG3,ISUBRO,IFOUND,IERROR)
          ELSE
            CALL DPJBS3(TEMP0,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,
     1                  TEMP4,IBUGG3,IERROR)
            IF(ICENSO.EQ.'ON')THEN
              NS32=NS3
              DO11363IJ=1,NS3
                TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
                IF(ILEVEL.EQ.'ON')THEN
                  TEMPZL(IJ)=TEMPL(ITEMP1(IJ))
                ENDIF
11363         CONTINUE
            ENDIF
          ENDIF
C
C         STEP 2: COMPUTE THE STATISTIC
C
          KSLOC=KSLSAV
          KSSCAL=KSSSAV
          IF(ICASPL.EQ.'BFWE')SH2=SHAPE2
          CALL CMPDIS(TEMP,TEMPZ,TEMPZL,NS2,MAXNXT,ICASPL,ICASP2,
     1                XTEMP1,XTEMP2,XTEMP3,
     1                DTEMP1,DTEMP2,DTEMP3,ITEMP1,
     1                ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,
     1                ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,
     1                YLOWLM,YUPPLM,A,B,MINMAX,NUMSHA,
     1                SHAP11,SHAP12,SHAP21,SHAP22,
     1                IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1                IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1                IEXPBC,IWEIBC,ICENTY,IDFTTY,
     1                MAXNXT,ICENSO,KSLOC,KSSCAL,IFORSW,ISEED,
     1                IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1                IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVEL,
     1                CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1                SH1,SH2,SH3,SH4,SH5,SH6,SH7,ALOC,ASCALE,STATVA,
     1                IBUGG3,ISUBRO,IERROR)
CCCCC     IF(IERROR.EQ.'YES')GOTO9000
C
C         NOTE 09/2010: WHEN THERE IS A "LENGTH" VARIABLE, (I.E., THE
C                       BRITTLE FIBER WEIBULL DISTRIBUTION), THE
C                       STANDARD METHOD FOR COMPUTING THE PERCENTILES
C                       (USE ESTIMATES OF DISTRIBUTIONAL PARAMETERS IN
C                       THE PERCENT POINT FUNCTION) CANNOT BE
C                       IMPLEMENTED.
C
C                       AN ALTERNATIVE IS TO COMPUTE DATA PERCENTILES.
C
C                       SET QUANTILE TO CPUMIN IF THERE WAS AN ERROR
C                       IN THE ESTIMATION STEP.
C
          IF(IFLAGL.EQ.1 .AND. AL.GT.0.0 .AND. SH1.GT.0.0)THEN
            ASCALE=AL**(1.0/SH1)*ASCALE
          ENDIF
C
          IF(NPERC.GT.0 .AND. NPERC.LE.1000)THEN
            IF(IERROR.EQ.'YES')THEN
              DO12112I=1,NPERC
                XQP(I)=CPUMIN
12112         CONTINUE
            ELSE
              IF(IBOOPE.EQ.'DATA' .OR. ILEVEL.EQ.'YES')THEN
                DO12110I=1,NPERC
                  ATEMP=QP(I)
                  CALL PERCEN(ATEMP,TEMP,NS2,IWRITE,ZTMP12,MAXNXT,
     1                        BTEMP,IBUGG3,IERROR)
                  XQP(I)=BTEMP
12110           CONTINUE
              ELSE
                CALL DPPPF1(QP,XQP,NPERC,ICASPL,
     1                      SH1,SH2,SH3,SH4,
     1                      SH5,SH6,SH7,
     1                      YLOWLM,YUPPLM,A,B,MINMAX,
     1                      ICAPSW,ICAPTY,
     1                      IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                      ILGADF,ISKNDF,IGLDDF,IBGEDF,
     1                      IGETDF,ICONDF,IGOMDF,IKATDF,
     1                      IGIGDF,IGEODF,
     1                      ALOC,ASCALE,
     1                      IBUGG3,ISUBRO,IERROR)
              ENDIF
            ENDIF
C
            IF((NGRPV.GE.1 .AND. NPERC.GT.0 .AND. ISET.LE.NUMSET) .OR.
     1         (NPERC.GT.0 .AND. NGRPV.EQ.0 .AND. ISET.EQ.1))THEN
              IF(NGRPV.EQ.0)THEN
                WRITE(IOUNI4,IFORMT)(XQP(JJ),JJ=1,NPERC)
                WRITE(IOUNI5,'(30E15.7)')(XQP(JJ),JJ=1,MIN(30,NPERC))
              ELSEIF(NGRPV.EQ.1)THEN
                WRITE(IOUNI4,IFORMT)ISET,(XQP(JJ),JJ=1,NPERC)
                WRITE(IOUNI5,'(30E15.7)')(XQP(JJ),JJ=1,MIN(30,NPERC))
              ELSEIF(NGRPV.EQ.2)THEN
                WRITE(IOUNI4,IFORMT)ISET1,ISET2,(XQP(JJ),JJ=1,NPERC)
                WRITE(IOUNI5,'(30E15.7)')(XQP(JJ),JJ=1,MIN(30,NPERC))
              ENDIF
            ENDIF
          ENDIF
C
          IF(ILIMIT.EQ.'ON' .AND.
     1      (ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD  ' .OR.
     1       ICASP2.EQ.'KS  '))THEN
            AVAL=ALOC + ASCALE
            ASCALE=AVAL
          ENDIF
          IF(NGRPV.LE.1)THEN
            TAGID=0.0
            IF(ALOC.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,ALOC,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(ASCALE.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,ASCALE,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH1,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH2,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH3,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH4,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH5,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'KS' .OR.
     1         ICASP2.EQ.'AD' .AND. STATVA.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,STATVA,TAGID,XIDTEM(1,1),
     1                    Y2,X2,D2)
            ENDIF
            NUMPAR=INT(TAGID+0.1)
          ELSEIF(NGRPV.EQ.2)THEN
            NUMPAR=0
            IF(ALOC.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,ALOC,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(ASCALE.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,ASCALE,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,SH1,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,SH2,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,SH3,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,SH4,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,SH5,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'KS' .OR.
     1         ICASP2.EQ.'AD' .AND. STATVA.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(1),NUMSE1(2),J,STATVA,
     1                    XIDTEM(1,1),XIDTEM(1,2),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
          ENDIF
11361   CONTINUE
C
C               ************************************************
C               **   STEP 19--                                **
C               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
C               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
C               **   TO DPST1F.DAT.                           **
C               ************************************************
C
CCCCC JANUARY 2005.  FOR UNGROUPED DATA, WRITE BOOTSTRAP ESTIMATES
CCCCC                TO FILE.  ALSO, ACCOMODATE CASE WHERE MORE
CCCCC                THAN ONE PARAMETER IS ESTIMATED.
C
        IF(NPERC.GE.1)THEN
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGG3,ISUBRO,IERROR)
        ENDIF
C
        DO6131II=1,NUMALP
          DO6133JJ=1,9
            ALOWPA(II,JJ)=CPUMIN
            AUPPPA(II,JJ)=CPUMIN
 6133     CONTINUE
 6131     CONTINUE
C
        ISTEPN='19'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,
     1              NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,
     1              APERC,BPERC,NPERC2,
     1              BMEAN,BSD,BMIN,BMAX,BMAD,
     1              B001,B005,B01,B025,B05,B10,B20,B50,
     1              B80,B90,B95,B975,B99,B995,B999,
     1              ALOWPA,AUPPPA,ALPHAV,NUMALP,
     1              ZMEAN,ZMED,ZSD,ZMAD,NFAIL,
     1              ISUBRO,IBUGG3,IERROR)
C
         DO6139II=1,NUMPAR
           NFAIL(II)=NRESAM - NFAIL(II)
 6139    CONTINUE
C
C       ************************************************
C       **   STEP 20--                                **
C       **   GENERATE  A NUMERIC TABLE OF THE RESULTS **
C       ************************************************
C
        IF(IPRINT.EQ.'OFF')GOTO11000
C
        ISTEPN='20'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICNT9=ICNT9+1
        IF(ICNT9.EQ.1)THEN
          IF(ICASP2.EQ.'PPCC')THEN
            ITITLE(1:32)='PPCC Bootstrap Analysis for the '
            NCTITL=32
          ELSEIF(ICASP2.EQ.'KS')THEN
            ITITLE(1:19)='Kolmogorov-Smirnov '
            ITITLE(20:46)='Bootstrap Analysis for the '
            NCTITL=46
          ELSEIF(ICASP2.EQ.'AD')THEN
            ITITLE(1:17)='Anderson-Darling '
            ITITLE(18:44)='Bootstrap Analysis for the '
            NCTITL=44
          ELSEIF(ICASP2.EQ.'MLE')THEN
            ITITLE(1:19)='Maximum Likelihood '
            ITITLE(20:46)='Bootstrap Analysis for the '
            NCTITL=46
          ENDIF
          DO8211II=60,1,-1
            IF(IDIST(II:II).NE.' ')THEN
               NCDIST=II
               GOTO8219
            ENDIF
 8211     CONTINUE
          NCDIST=1
 8219     CONTINUE
          ITITLZ(1:NCDIST)=IDIST(1:NCDIST)
          NSTRT=NCDIST+1
          NCTITZ=NSTRT+12
          ITITLZ(NSTRT:NCTITZ)=' Distribution'
        ELSE
          ITITLE=' '
          NCTITL=0
          ITITLZ=' '
          NCTITZ=0
        ENDIF
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        IF(NGRPV.EQ.1)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable One (        ): '
          IF(ISET.LE.0 .OR. ISET.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+1)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+1)(1:4)
            AVALUE(ICNT)=XIDTEM(ISET,1)
            IDIGIT(ICNT)=NUMDIG
          ENDIF
          NCTEXT(ICNT)=34
        ENDIF
C
        IF(NGRPV.GE.2)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable One (        ): '
          IF(ISET1.LE.0 .OR. ISET1.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+1)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+1)(1:4)
          ENDIF
          NCTEXT(ICNT)=34
          AVALUE(ICNT)=XIDTEM(ISET1,1)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Group ID Variable Two (        ): '
          IF(ISET2.LE.0 .OR. ISET2.EQ.ISETMX)THEN
            ITEXT(ICNT)(24:31)='All Data'
          ELSE
            WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARID(NUMV2+2)(1:4)
            WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI2(NUMV2+2)(1:4)
          ENDIF
          NCTEXT(ICNT)=34
          AVALUE(ICNT)=XIDTEM(ISET2,2)
          IDIGIT(ICNT)=NUMDIG
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Bootstrap Samples:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=REAL(NRESAM)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Observations:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=REAL(NS3)
        IDIGIT(ICNT)=0
C
        IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD' .OR.
     1     ICASP2.EQ.'KS')THEN
          IF(NUMSHA.GE.1)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Lower Limit for Shape Parameter 1:'
            NCTEXT(ICNT)=34
            AVALUE(ICNT)=SHAP11
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Upper Limit for Shape Parameter 1:'
            NCTEXT(ICNT)=34
            AVALUE(ICNT)=SHAP12
            IDIGIT(ICNT)=NUMDIG
          ENDIF
          IF(NUMSHA.GE.2)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Lower Limit for Shape Parameter 2:'
            NCTEXT(ICNT)=34
            AVALUE(ICNT)=SHAP21
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Upper Limit for Shape Parameter 2:'
            NCTEXT(ICNT)=34
            AVALUE(ICNT)=SHAP22
            IDIGIT(ICNT)=NUMDIG
          ENDIF
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        NUMROW=ICNT
        DO8311II=1,NUMROW
          NTOT(II)=15
 8311   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1              AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGG3,IERROR)
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
C
C       PRINT CONFIDENCE LIMITS FOR:
C
C       1) LOCATION PARAMETER
C       2) SCALE PARAMETER
C       3) SHAPE PARAMETER 1 - SHAPE PARAMETER 5
C       4) VALUE OF TEST STATISTIC
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
          WRITE(ICOUT,8321)
 8321     FORMAT('ALOWPA MATRIX:')
          CALL DPWRST('XXX','BUG ')
          DO8320L=1,NUMALP
            WRITE(ICOUT,8322)(ALOWPA(L,JJ),JJ=1,NUMPAR)
 8322       FORMAT(7G15.7)
            CALL DPWRST('XXX','BUG ')
 8320     CONTINUE
          WRITE(ICOUT,8331)
 8331     FORMAT('AUPPPA MATRIX:')
          CALL DPWRST('XXX','BUG ')
          DO8330L=1,NUMALP
            WRITE(ICOUT,8322)(AUPPPA(L,JJ),JJ=1,NUMPAR)
            CALL DPWRST('XXX','BUG ')
 8330     CONTINUE
        ENDIF
C
        ITAG=0
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
        ITITLZ=' '
        NCTITZ=0
C
C       LOCATION PARAMETER:
C
        IF(ALOC.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Location Parameter:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8312II=1,NUMROW
            NTOT(II)=15
 8312     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IPAR='Location'
          NCPAR=8
          IF(ILIMIT.EQ.'ON')THEN
            IPAR='Lower Limit'
            NCPAR=11
          ENDIF
          CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                ISUBRO,IBUGG3,IERROR)
        ENDIF
C
C       SCALE PARAMETER:
C
        IF(ASCALE.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Scale Parameter:'
          NCTEXT(ICNT)=16
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8313II=1,NUMROW
            NTOT(II)=15
 8313     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IPAR='Scale'
          NCPAR=5
          IF(ILIMIT.EQ.'ON')THEN
            IPAR='Upper Limit'
            NCPAR=11
          ENDIF
          CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                ISUBRO,IBUGG3,IERROR)
        ENDIF
C
C       SHAPE PARAMETER ONE:
C
        IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter 1:'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8314II=1,NUMROW
            NTOT(II)=15
 8314     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 1'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       SHAPE PARAMETER TWO:
C
        IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter 2:'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8315II=1,NUMROW
            NTOT(II)=15
 8315     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 2'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       SHAPE PARAMETER THREE:
C
        IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter 3:'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8316II=1,NUMROW
            NTOT(II)=15
 8316     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 3'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       SHAPE PARAMETER FOUR:
C
        IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter 4:'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8317II=1,NUMROW
            NTOT(II)=15
 8317     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 4'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       SHAPE PARAMETER FIVE:
C
        IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter 5:'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8318II=1,NUMROW
            NTOT(II)=15
 8318     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 5'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       VALUE OF TEST STATISTIC FOR PPCC, AD, KS
C
        IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD' .OR.
     1     ICASP2.EQ.'KS')THEN
          ITAG=ITAG+1
C
          ICNT=1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Value of Test Statistic:'
          NCTEXT(ICNT)=24
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Failed Bootstrap Samples:'
          NCTEXT(ICNT)=35
          AVALUE(ICNT)=REAL(NFAIL(ITAG))
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=ZMEAN(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=ZMED(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Standard Deviation:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=ZSD(ITAG)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median Absolute Deviation:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=ZMAD(ITAG)
          IDIGIT(ICNT)=NUMDIG
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO8319II=1,NUMROW
            NTOT(II)=15
 8319     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGG3,IERROR)
C
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Value of Test Statistic'
            NCPAR=23
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,
     1                  ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,
     1                  ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
C
C       IF PERCENTILES REQUESTED, GENERATE CONFIDENCE LIMITS FOR
C       PERCENTILES
C
        IF(NPERC.GE.1 .AND. IBOODP.NE.'OFF')THEN
          NPERCT=MIN(30,NPERC)
          IFLAG1=0
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=1
          IOP='OPEN'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGG3,ISUBRO,IERROR)
C
C         NOTE: SOMETIMES ESTIMATION METHODS CAN FAIL.  SO SKIP
C               OVER FAILED READS.
C
          DO7010L=1,NPERCT
C
            REWIND(IOUNI5)
            ICNT=0
            DO7020II=1,NRESAM
              READ(IOUNI5,'(30E15.7)',ERR=7020)(XTEMP2(JJ),JJ=1,NPERCT)
              IF(XTEMP2(L).LE.CPUMIN+100.0)GOTO7020
              ICNT=ICNT+1
              XTEMP1(ICNT)=XTEMP2(L)
 7020       CONTINUE
            CALL MEDIAN(XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,XQPMED,
     1                  IBUGG3,IERROR)
            XQP(L)=XQPMED
            IF(IBOODP.EQ.'TWOS')THEN
              P100=100.0*(ALPHA/2.0)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              XQPLCL(L)=BTEMP
              P100=100.0*(1.0 - (ALPHA/2.0))
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              XQPUCL(L)=BTEMP
            ELSEIF(IBOODP.EQ.'LOWE')THEN
              P100=100.0*(ALPHA)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              XQPLCL(L)=BTEMP
              XQPUCL(L)=CPUMIN
            ELSEIF(IBOODP.EQ.'UPPE')THEN
              P100=100.0*(1.0 - ALPHA)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              XQPUCL(L)=BTEMP
              XQPLCL(L)=CPUMIN
          ENDIF
C
 7010     CONTINUE
C
          CALL DPDT9B(QP,XQP,XQPLCL,XQPUCL,NPERC,
     1                ICAPSW,ICAPTY,NUMDIG,ALPHA,
     1                ISUBRO,IBUGG3,IERROR)
C
          IFLAG5=1
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGG3,ISUBRO,IERROR)
        ENDIF
C
11000 CONTINUE
C
      NPLOTP=J
      NPLOTV=3
C
      IOP='CLOS'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IF(NPERC.GT.0)IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IFEEDB.EQ.'ON')THEN
C
          WRITE(ICOUT,8102)
 8102     FORMAT('THE FOLLOWING INFORMATION IS WRITTEN TO FILES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8104)
 8104     FORMAT('DPST1F.DAT: THE BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8106)
 8106     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',
     1           'TWO) COLUMNS IDENTIFY THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8112)
 8112     FORMAT('DPST2F.DAT: STATISTICS BASED ON BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8114)
 8114     FORMAT('            MEAN, SD, MEDIAN, B025, ',
     1           'B975, B05, B95, B005, B995')
          CALL DPWRST('XXX','BUG ')
          IF(NUMPAR.GT.1)THEN
            WRITE(ICOUT,8118)
 8118       FORMAT('            THE FIRST COLUMN IDENTIFIES THE ',
     1             'PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,8116)
 8116     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',
     1           'TWO) COLUMNS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8117)
 8117     FORMAT('            (AFTER THE PARAMETER ID) IDENTIFY ',
     1           'THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
C
        ENDIF
C
CCCCC ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
C
C     REMOVE FAILED SAMPLES FROM PLOT
C
      ICOUNT=0
      DO9005I=1,NPLOTP
        IF(Y2(I).NE.CPUMIN)THEN
          ICOUNT=ICOUNT+1
          Y2(ICOUNT)=Y2(I)
          X2(ICOUNT)=X2(I)
          D2(ICOUNT)=D2(I)
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
            WRITE(ICOUT,9008)I,ICOUNT,Y2(ICOUNT),X2(ICOUNT),D2(ICOUNT)
 9008       FORMAT('I,ICOUNT,Y2(ICOUNT),X2(ICOUNT),D2(ICOUNT) = ',
     1             2I8,2G15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ENDIF
 9005 CONTINUE
      NPLOTP=ICOUNT
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO,ICASJB,IBOOSS
 9012   FORMAT('IBUGG3,ISUBRO,ICASJB,IBOOSS = ',2(A4,2X),A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICASPL,N,NUMSET,IERROR
 9013   FORMAT('ICASPL,N,NUMSET,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP
 9014   FORMAT('NPLOTV,NPLOTP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NPLOTP
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBS8(ISETMX,ISET,NUMSET,NUMSE2,N1,N2,NGRP,
     1                  MAXOBV,MAXGRP,NUMV2,
     1                  Y,Z,TEMPL,XDESGN,XIDTEM,TEMP0,TEMPZ0,TEMPZL,
     1                  NS2,NSS2,NI,NI2,ISET1,ISET2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--UTILITY ROUTINE FOR DPJBS6 AND DPJBS7.  FOR A
C              GIVEN REPLICATION, EXTRACT THE APPROPRIATE DATA.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/02
C     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED FROM DPJBS2
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION TEMPL(*)
      DIMENSION TEMP0(*)
      DIMENSION TEMPZ0(*)
      DIMENSION TEMPZL(*)
      DIMENSION XDESGN(MAXOBV,MAXGRP)
      DIMENSION XIDTEM(MAXOBV,MAXGRP)
C
      INTEGER NUMSE2(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      ISUBN1='JBS8'
      ISUBN2='    '
C
      IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS8')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPJBS8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ISETMX,ISET,NUMSET,NGRP,MAXGRP
   71   FORMAT('ISETMX,ISET,NUMSET,NGRP,MAXGRP = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N1,N2,NUMV2,NUMSE2(1),NUMSE2(2)
   72   FORMAT('N1,N2,NUMV2,NUMSE2(1),NUMSE2(2) = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)XIDTEM(ISET,1),XIDTEM(ISET,2)
   73   FORMAT('XIDTEM(ISET,1),XIDTEM(ISET,2) = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 11--                           **
C               **  EXTRACT THE APPROPRIATE DATA        **
C               ******************************************
C
      ISTEPN='11'
      IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS8')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISET.EQ.ISETMX)THEN
        DO1002I=1,N1
          TEMP0(I)=Y(I)
 1002   CONTINUE
        IF(NUMV2.GE.2)THEN
          DO1004I=1,N2
            TEMPZ0(I)=Z(I)
 1004     CONTINUE
        ENDIF
        IF(NUMV2.GE.3)THEN
          DO1006I=1,N2
            TEMPZL(I)=TEMPL(I)
 1006     CONTINUE
        ENDIF
        NS2=N1
        NSS2=N2
        NI=N1
        NI2=N2
        ISET1=0
        ISET2=0
C
C       NOTE: FOR GROUPED CASE, BOTH RESPONSE VARIABLES
C             MUST HAVE THE SAME LENGTH.
C
      ELSEIF(NGRP.EQ.1 .AND. ISET.LT.ISETMX)THEN
        K=0
        DO1102I=1,N1
          IF(XDESGN(I,1).NE.XIDTEM(ISET,1))GOTO1102
          K=K+1
          TEMP0(K)=Y(I)
          IF(NUMV2.GE.2)TEMPZ0(K)=Z(I)
          IF(NUMV2.GE.3)TEMPZL(K)=TEMPL(I)
C
          IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS8')THEN
            WRITE(ICOUT,1108)I,K,XDESGN(I,1),TEMP0(K),Y(I)
 1108       FORMAT('AT 1102: I,K,XDESGN(I,1),TEMP0(K),Y(I) = ',
     1             2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1102   CONTINUE
        NS2=K
        NS22=NS2
        NI=K
        NI2=K
        ISET1=0
        ISET2=0
      ELSEIF(NGRP.EQ.2 .AND. ISET.LT.NUMSET)THEN
        K=0
        ISET1=INT((ISET-1)/NUMSE2(2)) + 1
        ISET2=MOD((ISET-1),NUMSE2(2)) + 1
        DO1202I=1,N1
          IF(XDESGN(I,1).NE.XIDTEM(ISET1,1) .OR.
     1       XDESGN(I,2).NE.XIDTEM(ISET2,2))GOTO1202
          K=K+1
          TEMP0(K)=Y(I)
          IF(NUMV2.GE.2)TEMPZ0(K)=Z(I)
          IF(NUMV2.GE.3)TEMPZL(K)=TEMPL(I)
 1202   CONTINUE
        NS2=K
        NS22=NS2
        NI=K
        NI2=K
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS8')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISET,ISETMX,ISET1,ISET2
 9012   FORMAT('ISET,ISETMX,ISET1,ISET2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NS2,NSS2,NI,NI2
 9014   FORMAT('NS2,NSS2,NI,NI2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,MAX(NS2,NSS2)
          WRITE(ICOUT,9021)I,TEMP0(I),TEMPZ0(I),TEMPZL(I)
 9021     FORMAT('I,TEMP0(I),TEMPZ0(I),TEMPZL(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,
     1                  NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,
     1                  APERC,BPERC,NPERC,
     1                  BMEAN,BSD,BMIN,BMAX,BMAD,
     1                  B001,B005,B01,B025,B05,B10,B20,B50,
     1                  B80,B90,B95,B975,B99,B995,B999,
     1                  ALOWPA,AUPPPA,ALPHA,NUMALP,
     1                  ZMEAN,ZMED,ZSD,ZMAD,NFAIL,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--UTILITY ROUTINE FOR BOOTSTRAP/JACKNIFE PLOT.  THIS
C              ROUTINE WRITES INFORMATION TO DPST1F.DAT AND DPST2F.DAT
C              AND COMPUTES CERTAIN PARAMETERS (E.G., BMEAN, B10).
C
C              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
C                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
C                    SIMPLIFY THE CODE A BIT AS WELL.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/03
C     ORIGINAL VERSION--MARCH     2010. EXTRACTED FROM DPJBS2
C     UPDATED         --OCTOBER   2010. SAVE MEAN, MEDIAN, SD, AND MAD
C                                       FOR EACH PARAMETER FOR PRINT
C                                       ROUTINES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      DIMENSION Y2(*)
      DIMENSION D2(*)
      DIMENSION TEMP(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION APERC(*)
      DIMENSION BPERC(*)
      DIMENSION ALPHA(NUMALP)
      DIMENSION ALOWPA(NUMALP,*)
      DIMENSION AUPPPA(NUMALP,*)
      DIMENSION ZMEAN(*)
      DIMENSION ZMED(*)
      DIMENSION ZSD(*)
      DIMENSION ZMAD(*)
      INTEGER   NFAIL(*)
C
      INTEGER NUMSE1(*)
C
      CHARACTER*4 IWRITE
C
C-----COMMON----------------------------------------------------------
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
      IWRITE='OFF'
C
      IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS9')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)
   60   FORMAT('AT THE BEGINNING OF DPJBS9--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IBUGG3,ISUBRO,IOUNI1,IOUNI2
   61   FORMAT('IBUGG3,ISUBRO,IOUNI1,IOUNI2 = ',A4,2X,A4,2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)NGRPV,NUMPAR,NUMSET,J
   62   FORMAT('NGRPV,NUMPAR,NUMSET,J = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ISET,ISET1,ISET2,NUMSET
   63   FORMAT('ISET,ISET1,ISET2,NUMSET = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,J
          WRITE(ICOUT,74)I,Y2(I),D2(I)
   74     FORMAT('I,Y2(I),D2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               ************************************************
C               **   STEP 19--                                **
C               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
C               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
C               **   TO DPST1F.DAT.                           **
C               ************************************************
C
C       CASE 1: NO GROUPS OR DATA AGGREGATED OVER ALL GROUPS FOR
C               GROUPED CASE.
C
        IF(NGRPV.LE.0 .OR. (NGRPV.GE.1.AND.ISET.GT.NUMSET))THEN
C
CCCCC     NLAST=J*NUMPAR
          DO110I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            WRITE(IOUNI1,112)(Y2(K),K=I,NSTOP)
  112       FORMAT(8E15.7)
  110     CONTINUE
C
C         2014/04: SET NFAIL TO NUMBER OF SUCCESSFUL BOOTSTRAP.
C                  IN CALLING ROUTINE, WILL SUBTRACT THIS FROM
C                  NUMBER OF BOOTSTRAP SAMPLES TO OBTAIN THE NUMBER
C                  OF FAILURES.
C
          DO115IPAR=1,NUMPAR
            ICOUNT=0
            NFAIL(IPAR)=0
            DO116I=1,J,NUMPAR
              ATEMP=Y2(I+IPAR-1)
              IF(ATEMP.NE.CPUMIN)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
                NFAIL(IPAR)=NFAIL(IPAR)+1
              ELSE
CCCCC           NFAIL(IPAR)=NFAIL(IPAR)+1
              ENDIF
  116       CONTINUE
C
            IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS9')THEN
              WRITE(ICOUT,3118)I,ICOUNT,NFAIL(IPAR)
 3118         FORMAT('I,ICOUNT,NFAIL(IPAR)=',3I8)
              CALL DPWRST('XXX','BUG ')
              DO3119II=1,ICOUNT
                WRITE(ICOUT,3120)II,TEMP(ICOUNT)
 3120           FORMAT('II,TEMP(ICOUNT) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
 3119         CONTINUE
            ENDIF
C
C           NOTE OCTOBER 2010: FOR SOME DISTRIBUTIONS, PARAMETER
C           ESTIMATION CAN FAIL.  TO AVOID FAILURE IN GENERATING
C           PLOT/STATISTICS, KEEP TRACK OF NUMBER OF FAILURES AND
C           OMIT THESE FROM PLOT OUTPUT.
C
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEANZ,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSDZ,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50Z,
     1                  IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,
     1                  IBUGG3,IERROR)
            ZMEAN(IPAR)=BMEANZ
            ZMED(IPAR)=B50Z
            ZSD(IPAR)=BSDZ
            ZMAD(IPAR)=BMAD
C
C           COMPUTE SELECT PERCENTILES
C
CCCCC       IF(NUMPAR.EQ.1)THEN
              DO117L=1,NPERC
                ATEMP=APERC(L)
                CALL PERCEN(ATEMP,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                      BTEMP,IBUGG3,IERROR)
                BPERC(L)=BTEMP
  117         CONTINUE
CCCCC       ENDIF
C
C           COMPUTE SELECT CONFIDENCE INTERVALS
C
            DO1118L=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1118       CONTINUE
C
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,119)BMEANZ,BSDZ,B50Z,
     1                         (BPERC(LL),LL=1,15)
  119         FORMAT(18E15.7)
              BMEAN=BMEANZ
              BSD=BSDZ
              B50=B50Z
              B001=BPERC(1)
              B005=BPERC(2)
              B01=BPERC(3)
              B025=BPERC(4)
              B05=BPERC(5)
              B10=BPERC(6)
              B20=BPERC(7)
              B80=BPERC(9)
              B90=BPERC(10)
              B95=BPERC(11)
              B975=BPERC(12)
              B99=BPERC(13)
              B995=BPERC(14)
              B999=BPERC(15)
            ELSE
              WRITE(IOUNI2,118)IPAR,BMEANZ,BSDZ,B50Z,
     1                         (BPERC(LL),LL=1,15)
  118         FORMAT(I8,18E15.7)
            ENDIF
  115     CONTINUE
C
C       CASE 2: ONE GROUP CASE
C
        ELSEIF(NGRPV.EQ.1 .AND. ISET.LE.NUMSET)THEN
C
          DO120I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            IF(INT(D2(I)+0.01).EQ.ISET)THEN
              WRITE(IOUNI1,122)ISET,(Y2(K),K=I,NSTOP)
  122         FORMAT(I8,8E15.7)
            ENDIF
  120     CONTINUE
C
          DO125IPAR=1,NUMPAR
            ICOUNT=0
            DO126I=1,J,NUMPAR
              IF(INT(D2(I)+0.01).EQ.ISET)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
              ENDIF
  126       CONTINUE
C
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEAN,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSD,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50,
     1                  IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,
     1               IBUGG3,IERROR)
C
C           COMPUTE SELECT PERCENTILES
C
            DO127L=1,NPERC
              ATEMP=APERC(L)
              CALL PERCEN(ATEMP,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              BPERC(L)=BTEMP
  127       CONTINUE
C
C           COMPUTE SELECT CONFIDENCE INTERVALS
C
            DO1128L=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1128       CONTINUE
C
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,129)ISET,BMEAN,BSD,B50,
     1                         (BPERC(LL),LL=1,15)
  129         FORMAT(I8,18E15.7)
            ELSE
              WRITE(IOUNI2,128)IPAR,ISET,BMEAN,BSD,B50,
     1                         (BPERC(LL),LL=1,15)
  128         FORMAT(2I8,18E15.7)
            ENDIF
  125     CONTINUE
C
C       CASE 3: TWO GROUPS CASE
C
        ELSEIF(NGRPV.EQ.2 .AND.ISET.LE.NUMSET)THEN
C
          ITAG=(ISET1-1)*NUMSE1(2) + ISET2
          DO130I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            IF(INT(D2(I)+0.01).EQ.ITAG)THEN
              WRITE(IOUNI1,132)ISET1,ISET2,(Y2(K),K=I,NSTOP)
  132         FORMAT(2I8,8E15.7)
            ENDIF
  130     CONTINUE
C
          DO135IPAR=1,NUMPAR
            ICOUNT=0
            DO136I=1,J,NUMPAR
              IF(INT(D2(I)+0.01).EQ.ITAG)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
              ENDIF
  136       CONTINUE
C
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEAN,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSD,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50,
     1                  IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,
     1               IBUGG3,IERROR)
C
C           COMPUTE SELECT PERCENTILES
C
            DO137L=1,NPERC
              ATEMP=APERC(L)
              CALL PERCEN(APERC(L),TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP,IBUGG3,IERROR)
              BPERC(L)=BTEMP
  137       CONTINUE
C
C           COMPUTE SELECT CONFIDENCE INTERVALS
C
            DO1130II=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,
     1                    BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1130       CONTINUE
C
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,139)ISET1,ISET2,BMEAN,BSD,B50,
     1                         (BPERC(LL),LL=1,15)
  139         FORMAT(2I8,18E15.7)
            ELSE
              WRITE(IOUNI2,138)IPAR,ISET1,ISET2,BMEAN,BSD,B50,
     1                         (BPERC(LL),LL=1,15)
  138         FORMAT(3I8,18E15.7)
            ENDIF
  135     CONTINUE
        ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS9')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS9--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPJUST(ICOM,IHARG,NUMARG,
     1IDEFJU,
     1ITEXJU,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE JUSTIFICATION TYPE FOR
C              TEXT SCRIPT
C              ON A PLOT.
C              THE JUSTIFICATION FOR THE TEXT WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITEXJU.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFJU
C                     --IBUGD2
C     OUTPUT ARGUMENTS--ITEXJU
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFJU
      CHARACTER*4 ITEXJU
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPJUST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICOM,NUMARG,IDEFJU
   53 FORMAT('ICOM,NUMARG,IDEFJU = ',A4,2X,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ************************************
C               **  TREAT THE JUSTIFICATION CASE  **
C               ************************************
C
 1110 CONTINUE
      IF(ICOM.EQ.'JUST')GOTO1120
      IF(ICOM.EQ.'LEFT')GOTO1130
      IF(ICOM.EQ.'CENT')GOTO1140
      IF(ICOM.EQ.'RIGH')GOTO1150
C
 1120 CONTINUE
      IF(NUMARG.LE.0)GOTO1161
      IF(IHARG(NUMARG).EQ.'ON')GOTO1161
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1161
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1161
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LEFT')GOTO1161
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CENT')GOTO1162
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'RIGH')GOTO1163
      GOTO1170
C
 1130 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(NUMARG).EQ.'ON')GOTO1161
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1161
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1161
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GOTO1161
      GOTO9000
C
 1140 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(NUMARG).EQ.'ON')GOTO1162
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1162
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1162
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GOTO1162
      GOTO9000
C
 1150 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(NUMARG).EQ.'ON')GOTO1163
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1163
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1163
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GOTO1163
      GOTO9000
C
 1161 CONTINUE
      ITEXJU='LEFT'
      GOTO1180
C
 1162 CONTINUE
      ITEXJU='CENT'
      GOTO1180
C
 1163 CONTINUE
      ITEXJU='RIGH'
      GOTO1180
C
 1165 CONTINUE
      ITEXJU=IDEFJU
      GOTO1180
C
 1170 CONTINUE
CCCCC IERROR='YES'
CCCCC WRITE(ICOUT,1171)
C1171 FORMAT('***** ERROR IN DPJUST--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1172)
C1172 FORMAT('      ILLEGAL ENTRY FOR JUSTIFICATION ',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1'COMMAND.')
CCCCC WRITE(ICOUT,1173)
C1173 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1'PROPER FORM--')
CCCCC WRITE(ICOUT,1174)
C1174 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1175)
C1175 FORMAT('      TO HAVE ALL LEGENDS CENTERED,')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1177)
C1177 FORMAT('      THEN ALLOWABLE FORMS ARE--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1178)
C1178 FORMAT('           JUSTIFICATION CENTER ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1179)
C1179 FORMAT('           CENTER JUSTIFICATION ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC GOTO9000
      ITEXJU=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE JUSTIFICATION (FOR PLOT SCRIPT AND TEXT) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ITEXJU
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
C               ********************************************
C               **  STEP 81--                             **
C               **  TREAT THE    ?    CASE--              **
C               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
C               ********************************************
C
 8100 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)ITEXJU
 8111 FORMAT('THE CURRENT JUSTIFICATION IS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDEFJU
 8112 FORMAT('THE DEFAULT JUSTIFICATION IS ',A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPJUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ITEXJU,IDEFJU
 9013 FORMAT('ITEXJU,IDEFJU = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPKAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN KAPLAN-MEIER PLOT
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --JULY      2005. SUPPORT SWITCH FOR WHETHER
C                                       SURVIVAL CURVE (DEFAULT) OR
C                                       CDF CURVE DRAWN
C     UPDATED         --JANUARY   2012. USE DPPARS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=10)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION TAG1(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TAG1(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
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
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPKA'
      ISUBN2='PL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKAPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,MAXCOL
   52   FORMAT('ICASPL,IAND1,IAND2,MAXCOL = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               **********************************
C               **  TREAT THE KAPLAN-MEIER PLOT **
C               **********************************
C
C               *******************************************
C               **  STEP 1--                             **
C               **  SEARCH FOR KAPLAN MEIER, KAPLAN-MEIER**
C               **  MODIFIED KAPLAN MEIER, OR MODIFIED   **
C               **  KAPLAN-MEIER                         **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'KAPL'.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        ICASPL='KAPL'
      ELSEIF(NUMARG.GE.2.AND.ICOM.EQ.'KAPL'.AND.
     1       IHARG(1).EQ.'MEIE'.AND.IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='KAPL'
      ELSEIF(NUMARG.GE.2.AND.ICOM.EQ.'MODI'.AND.
     1       IHARG(1).EQ.'KAPL'.AND.IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='MKAP'
      ELSEIF(NUMARG.GE.3.AND.ICOM.EQ.'MODI'.AND.
     1       IHARG(1).EQ.'KAPL'.AND.IHARG(2).EQ.'MEIE'.AND.
     1       IHARG(3).EQ.'PLOT')THEN
        ILASTC=3
        ICASPL='MKAP'
      ELSE
        ICASPL='    '
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='KAPLAN-MEIER PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=2
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C
C               *********************************************************
C               **  STEP 41--                                          **
C               **  FORM THE VERTICAL AND HORIZONTALAXIS               **
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY)FOR THE     **
C               **  PLOT.  FORM THE CURVE DESIGNATION VARIABLED(.)  .  **
C               **  THIS WILL BE ALL ONES.                             **
C               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).       **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).       **
C               *********************************************************
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,TAG1,Y1,NS,NS,NS,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CALL DPKAP2(Y1,TAG1,NS,NUMVAR,ICASPL,MAXN,
     1            IKAPSW,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKAPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.LE.0)THEN
          DO9015I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKAP2(Y1,TAG1,N,NUMV,ICASPL,MAXN,
     1                  IKAPSW,
     1                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN KAPLAN-MEIER PLOT
C     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED) OBSERVATIONS
C                               FOR THE FIRST  VARIABLE.
C                      TAG1   = 1 = FAILURE TIME, 0 = CENSORED
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     CAUTION--THE INPUT VARIABLE Y1(.) WILL BE CHANGED HEREIN
C              (IT WILL BE SORTED)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --JULY      2005. SWITCH TO SPECIFY WHETHER
C                                       SURVIVAL CURVE (DEFAULT) OR
C                                       CDF CURVE DRAWN
C     UPDATED         --JUNE      2008. ACCOMODATE NEGATIVE DATA
C                                       (E.G. FOR REVERSE WEIBULL
C                                       OR REVERSE FRECHET)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IKAPSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DCURR
      DOUBLE PRECISION DN
      DOUBLE PRECISION DCORR
C
      DIMENSION Y1(*)
      DIMENSION TAG1(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
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
      ISUBN1='DPKA'
      ISUBN2='P2  '
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKAP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,N,MAXN,NUMV
   53   FORMAT('ICASPL,N,MAXN,NUMV = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y1(I),TAG1(I)
   56     FORMAT('I, Y1(I), TAG1(I), = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN KAPLAN-MEIER PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)N
  114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO120I=1,N
        IF(Y1(I).NE.HOLD)GOTO129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)HOLD
  122 FORMAT('      ALL ELEMENTS IN THE RESPONSE VARIABLE ARE ',
     1       'IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  129 CONTINUE
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  COMPUTE COORDINATES FOR KAPLAN MEIER PLOT**
C               **  (INCORPORATE STAIR-STEP APPEARANCE)      **
C               ***********************************************
C
      CALL SORTC(Y1,TAG1,N,Y1,TAG1)
C
      XMIN=Y1(1)
      XMAX=Y1(N)
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
        DO135I=1,N
        WRITE(ICOUT,136)I,Y1(I),TAG1(I)
  136   FORMAT('I, Y1(I), TAG1(I), = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
  135   CONTINUE
      ENDIF
C
      DN=DBLE(N)
      IF(ICASPL.EQ.'KAPL')THEN
        IR=0
        J=1
        IF(XMIN.LT.0.0)THEN
          X(J)=XMIN
        ELSE
          X(J)=0.0
        ENDIF
        Y(J)=1.0
        D(J)=1.0
C
        DPROD=1.0D0
        DO200I=1,N
          IF(NUMV.GE.2 .AND. ABS(TAG1(I)).LT.0.5)GOTO200
          J=J+1
          X(J)=Y1(I)
          Y(J)=Y(J-1)
          D(J)=1.0
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
            WRITE(ICOUT,203)I,J,X(J),Y(J)
  203       FORMAT('I,J,X(J),Y(J)=',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          DCURR=(DN - DBLE(I))/(DN - DBLE(I) + 1.0D0)
          DPROD=DPROD*DCURR
          J=J+1
          X(J)=Y1(I)
          Y(J)=REAL(DPROD)
          D(J)=1.0
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
            WRITE(ICOUT,204)I,J,X(J),Y(J)
  204       FORMAT('I,J,X(J),Y(J)=',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
  200   CONTINUE
      ELSEIF(ICASPL.EQ.'MKAP')THEN
        IR=0
        J=1
        IF(XMIN.LT.0.0)THEN
          X(J)=XMIN
        ELSE
          X(J)=0.0
        ENDIF
        Y(J)=1.0
        D(J)=1.0
C
        DPROD=1.0D0
        DCORR=(DN + 0.7D0)/(DN + 0.4D0)
        DO400I=1,N
          IF(NUMV.GE.2 .AND. ABS(TAG1(I)).LT.0.5)GOTO400
          J=J+1
          X(J)=Y1(I)
          Y(J)=Y(J-1)
          D(J)=1.0
          DCURR=(DN - DBLE(I) + 0.7D0)/(DN - DBLE(I) + 1.7D0)
          DPROD=DPROD*DCURR
          J=J+1
          X(J)=Y1(I)
          Y(J)=REAL(DCORR*DPROD)
          D(J)=1.0
  400   CONTINUE
      ENDIF
C
      NPLOTP=J
      NPLOTV=2
C
CCCCC JULY 2005: CONVERT TO CDF FORMAT
C
      IF(IKAPSW.EQ.'CDF')THEN
        DO510I=1,NPLOTP
          Y(I)=1.0 - Y(I)
  510   CONTINUE
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKAP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,MAXN,NPLOTP,NPLOTV,ICASPL,IERROR
 9013   FORMAT('N,MAXN,NPLOTP,NPLOTV,ICASPL,IERROR = ',4I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO9022I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKAR3(Y,N,TEMP1,MAXNXT,
     1                  STATVA,CUTOFF,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES KAPPENMAN'S STATISTIC FOR
C              DISTINGUISHING BETWEEN A WEIBULL AND A LOGNORMAL
C              DISTRIBUTION.  THE TEST STATISTIC IS:
C
C                 R = (A3 - A2)/(A2 - A1)
C
C              WHERE
C
C                 A1 = AVERAGE OF THE LOWER 5% OF THE ORDERED LOGARITHMS
C                      OF THE DATA
C                 A3 = AVERAGE OF THE UPPER 5% OF THE ORDERED LOGARITHMS
C                      OF THE DATA
C                 A2 = AVERAGE OF THE ORDERED LOGARITHMS OF THE DATA
C                      AFTER TRIMMING THE LOWER 20% AND THE UPPER 20%.
C              THEN IF R > 0.7477 SELECT LOGNORMAL AND IF R < 0.7477
C              CHOOSE WEIBULL.
C
C              RESTRICT THIS TEST TO A MINIMUM SAMPLE SIZE OF 20.
C
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION RESPONSE VARIABLE.
C                     --N      = AN INTEGER PARAMETER THAT SPECIFIES THE
C                                NUMBER OF VALUES IN THE RESPONSE
C                                VARIABLE.
C     OUTPUT ARGUMENTS--R      = THE SINGLE PRECISION VALUE OF THE TEST
C                                STATISTIC.
C     OUTPUT--THE SINGLE PRECISION VALUE OF THE TEST STATISTIC.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--SAMPLE SIZE SHOULD BE AT LEAST 20.
C     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, TRIMME.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE --KAPPENMAN (1988), "A SIMPLE METHOD FOR CHOOSING
C                 BETWEEN THE LOGNORMAL AND WEIBULL MODELS", STATISTICS
C                 & PROBABILITY LETTERS", VOL. 7, NO. 2, PP. 123-126.
C               --JOHN MCCOOL (2012), "USING THE WEIBULL DISTRIBUTION:
C                 RELIABILITY, MODELING, AND INFERENCE", WILEY,
C                 PP. 207-210.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2014/05
C     ORIGINAL VERSION--MAY       2014.
C
C---------------------------------------------------------------------
C
      REAL Y(*)
      REAL TEMP1(*)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
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
      IF(ISUBRO.EQ.'KAR3' .OR. IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPKAR3')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
      STATVA=CPUMIN
      CUTOFF=0.7477
C
      IF(N.LT.20)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,91)
   91   FORMAT('***** ERROR IN KAPPENMAN R STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)
   93   FORMAT('      A MINIMUM SAMPLE SIZE OF 20 IS REQUIRED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,95)N
   95   FORMAT('      THE NUMBER OF RESPONSE VALUES IS   ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        DO96I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,91)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,98)I,Y(I)
   98       FORMAT('      ROW ',I8,' IS NON-POSITIVE.  IT HAS THE ',
     1             'VALUE ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
            Y(I)=LOG(Y(I))
          ENDIF
   96   CONTINUE
      ENDIF
C
      CALL SORT(Y,N,Y)
C
C     TRIMMED MEAN WITH 20% TRIMMING ON EACH TAIL
C
      PROP1=20.0
      PROP2=20.0
      NTRIM1=0
      NTRIM2=0
      IWRITE='OFF'
      CALL TRIMME(Y,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,TEMP1,
     1            MAXNXT,A2,
     1            IBUGA3,ISUBRO,IERROR)
C
C     AVERAGE OF LOWER 5% OF VALUES
C
      AFRAC=0.05*REAL(N)
      IFRAC=INT(AFRAC)
      REM=AFRAC-REAL(IFRAC)
      IF(IFRAC.GE.1)THEN
        CALL MEAN(Y,IFRAC,IWRITE,XMEAN,IBUGA3,IERROR)
        A1=(XMEAN + REM*Y(IFRAC+1))/(REAL(IFRAC) + REM)
      ELSE
        A1=Y(1)
      ENDIF
C
C     AVERAGE OF UPPER 5% OF VALUES
C
      IF(IFRAC.GE.1)THEN
        NSTRT=N-IFRAC+1
        SUM=0.0
        DO110I=NSTRT,N
          SUM=SUM + Y(I)
  110   CONTINUE
        XMEAN=SUM/REAL(IFRAC)
        A3=(XMEAN + REM*Y(NSTRT-1))/(REAL(IFRAC) + REM)
      ELSE
        A3=Y(N)
      ENDIF
C
      STATVA=(A3-A2)/(A2-A1)
C
 9000 CONTINUE
      IF(ISUBRO.EQ.'KAR3' .OR. IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)
 9001   FORMAT('AT THE END OF DPKAR3')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)IERROR,STATVA
 9002   FORMAT('IERROR,STATVA = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9003)IFRAC,REM,A1,A2,A3
 9003   FORMAT('IFRAC,REM,A1,A2,A3 = ',I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKDEN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IKDENP,PKDEWI,ISEED,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A KERNEL DENSITY PLOT USING A 
C              GAUSSIAN WINDOW.  USES APPLIED STATISTICS
C              ALGORITHM 176 (BY B. W. SILVERMAN).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001/8
C     ORIGINAL VERSION--AUGUST    2001.
C     UPDATED         --FEBRUARY  2010. USE DPPARS
C     UPDATED         --FEBRUARY  2010. SUPPORT FOR "MULTIPLE" AND
C                                       "REPLICATION"
C     UPDATED         --MARCH     2010. USE DPPAR3 FOR SINGLE RESPONSE
C                                       VARIABLE OR MULTIPLE CASES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASE
      CHARACTER*4 ICASEQ
      CHARACTER*4 IERRO4
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DOUBLE PRECISION Y1(MAXOBV)
      DOUBLE PRECISION SMOOTH(MAXOBV)
      DOUBLE PRECISION FT(MAXOBV)
      DOUBLE PRECISION ZY(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION WORK1(MAXOBV)
      DIMENSION WORK2(MAXOBV)
      DIMENSION WORK3(MAXOBV)
      DIMENSION WORK4(MAXOBV)
      DIMENSION WORK5(MAXOBV)
      DIMENSION WORK6(MAXOBV)
      DIMENSION WORK7(MAXOBV)
      DIMENSION WORK8(MAXOBV)
      DIMENSION WORK9(MAXOBV)
      DIMENSION WORK10(MAXOBV)
      DIMENSION WORK11(4,MAXOBV)
      DIMENSION WORK12(MAXOBV,3)
C
      EQUIVALENCE (DGARBG(IDGAR1),Y1(1))
      EQUIVALENCE (DGARBG(IDGAR2),SMOOTH(1))
      EQUIVALENCE (DGARBG(IDGAR3),FT(1))
      EQUIVALENCE (DGARBG(IDGAR4),ZY(1))
C
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP3(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR11),XDESGN(1,1))
      EQUIVALENCE (GARBAG(JGAR13),WORK1(1))
      EQUIVALENCE (GARBAG(JGAR14),WORK2(1))
      EQUIVALENCE (GARBAG(JGAR15),WORK3(1))
      EQUIVALENCE (GARBAG(JGAR16),WORK4(1))
      EQUIVALENCE (GARBAG(JGAR17),WORK5(1))
      EQUIVALENCE (GARBAG(JGAR18),WORK6(1))
      EQUIVALENCE (GARBAG(JGAR19),WORK7(1))
      EQUIVALENCE (GARBAG(IGAR11),WORK8(1))
      EQUIVALENCE (GARBAG(IGAR12),WORK9(1))
      EQUIVALENCE (GARBAG(IGAR13),WORK10(1))
      EQUIVALENCE (GARBAG(IGAR14),WORK11(1,1))
      EQUIVALENCE (GARBAG(IGAR18),WORK12(1,1))
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
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
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPKD'
      ISUBN2='EN  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MAXV2=1
      MINN2=20
C
C               ***************************************************
C               **  TREAT THE KERNEL DENSITY PLOT                **
C               ***************************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKDEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  EXTRACT THE COMMAND                             **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
C               **    1) KERNEL DENSITY PLOT Y                      **
C               **    2) MULTIPLE KERNEL DENSITY PLOT Y1 ... YK     **
C               **    3) REPLICATED KERNEL DENSITY PLOT Y X1  X2    **
C               ******************************************************
C
C     NOTE: KERNEL DENSITY, KERNEL PLOT, DENSITY TRACE ARE SYNONYMS
C           FOR KERNEL DENSITY PLOT.
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'KERN')GOTO89
      IF(ICOM.EQ.'MULT')GOTO89
      IF(ICOM.EQ.'REPL')GOTO89
      GOTO9000
C
   89 CONTINUE
      ICASPL='KDEN'
      IMULT='OFF'
      IREPL='OFF'
      ILASTC=-9999
C
      IF(ICOM.EQ.'KERN')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ENDIF
C
      ISTOP=NUMARG-1
      DO90I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'TRAC')THEN
          ISTOP=I
          GOTO99
        ENDIF
   90 CONTINUE
   99 CONTINUE
C
      IFOUND='NO'
      DO100I=1,ISTOP
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(IHARG(I).EQ.'KERN')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'TRAC')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'DENS')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IHARG(I).EQ.'MULT')THEN
          IMULT='ON'
        ENDIF
  100 CONTINUE
C
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN KERNEL DENSITY PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE KERNEL DENSITY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'KDEN')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL
  112   FORMAT('ICASPL,IMULT,IREPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='KERNEL DENSITY PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IF(IMULT.EQ.'ON')IFLAGE=0
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=3
      IF(IREPL.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=3
      ELSEIF(IMULT.EQ.'ON')THEN
        MINNVA=1
        MAXNVA=100
      ENDIF
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
      NRESP=0
      NREPL=0
      IF(IREPL.EQ.'OFF' .AND. NUMVAR.GT.1)IMULT='ON'
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1 .OR. NREPL.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 1 AND 2;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
C
C               ********************************************
C               **  STEP 6--                              **
C               **  GENERATE THE KERNEL DENISTY PLOTS FOR **
C               **  THE VARIOUS CASES.                    **
C               ********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 7A--                                  **
C               **  CASE 1: SINGLE RESPONSE VARIABLE WITH NO   **
C               **          REPLICATION (RESPONSE VARIABLE CAN **
C               **          BE A MATRIX).                      **
C               *************************************************
C
      IF(NRESP.EQ.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              XTEMP1,XTEMP2,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        DO701I=1,NLOCAL
          Y1(I)=DBLE(XTEMP1(I))
  701   CONTINUE
        IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 7B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               **  RESET THE VECTOR D(.) TO ALL ONES.             **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C               *****************************************************
C
        NCURVE=1
        NPLOTP=0
        CALL DPKDE2(Y1,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,
     1              NLOCAL,ICASPL,IKDENP,PKDEWI,IKDERN,IKDEPF,
     1              ISEED,MINN2,
     1              WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1              WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
C               ******************************************
C
      ELSEIF(NRESP.GT.1)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NPLOTP=0
        DO810IRESP=1,NRESP
          NCURVE=IRESP
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                XTEMP1,XTEMP2,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
        DO801I=1,NLOCAL
          Y1(I)=DBLE(XTEMP1(I))
  801   CONTINUE
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
          CALL DPKDE2(Y1,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,
     1                NLOCAL,ICASPL,IKDENP,PKDEWI,IKDERN,IKDEPF,
     1                ISEED,MINN2,
     1                WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1                WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,
     1                Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
  810   CONTINUE
C
C               *****************************************************
C               **  STEP 9A--                                      **
C               **  CASE 3: ONE OR TWO  REPLICATION VARIABLES.     **
C               **          FOR THIS CASE, THE NUMBER OF RESPONSE  **
C               **          VARIABLES MUST BE EXACTLY 1.           **
C               **          CURRENTLY, GROUPED DATA NOT SUPPORTED  **
C               **          WITH REPLICATION.                      **
C               *****************************************************
C
      ELSEIF(NRESP.EQ.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO910I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO910
          J=J+1
C
C         RESPONSE VARIABLE IN Y1
C
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
C
          ICOLC=1
          DO920IR=1,MIN(NREPL,2)
            ICOLC=ICOLC+1
            ICOLT=ICOLR(ICOLC)
            IJ=MAXN*(ICOLT-1)+I
            IF(ICOLT.LE.MAXCOL)XDESGN(J,IR)=V(IJ)
            IF(ICOLT.EQ.MAXCP1)XDESGN(J,IR)=PRED(I)
            IF(ICOLT.EQ.MAXCP2)XDESGN(J,IR)=RES(I)
            IF(ICOLT.EQ.MAXCP3)XDESGN(J,IR)=YPLOT(I)
            IF(ICOLT.EQ.MAXCP4)XDESGN(J,IR)=XPLOT(I)
            IF(ICOLT.EQ.MAXCP5)XDESGN(J,IR)=X2PLOT(I)
            IF(ICOLT.EQ.MAXCP6)XDESGN(J,IR)=TAGPLO(I)
  920     CONTINUE
C
  910   CONTINUE
        NLOCAL=J
C
C       *****************************************************
C       **  STEP 9B--                                      **
C       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C       **                                                 **
C       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
C       **  VARIOUS REPLICATIONS.                          **
C       *****************************************************
C
        ISTEPN='9B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF FREQ--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,932)ICASPL,NUMVAR,NLOCAL
  932     FORMAT('ICASPL,NUMVAR,NQ = ',A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO935I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  935       CONTINUE
          ENDIF
        ENDIF
C
C       *****************************************************
C       **  STEP 9C--                                      **
C       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
C       **  REPLICATION VARIABLES.                         **
C       *****************************************************
C
        CALL DPFRE5(XDESGN(1,1),XDESGN(1,2),
     1             NREPL,NLOCAL,MAXOBV,
     1             XIDTEM,XIDTE2,
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,
     1             IBUGG3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 9D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO1110ISET1=1,NUMSE1
            K=0
            DO1130I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPKDE2(ZY,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,
     1                    NTEMP,ICASPL,IKDENP,PKDEWI,
     1                    IKDERN,IKDEPF,ISEED,MINN2,
     1                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1                    WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,
     1                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO1210ISET1=1,NUMSE1
          DO1220ISET2=1,NUMSE2
            K=0
            DO1290I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2)
     1          )THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPKDE2(ZY,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,
     1                    NTEMP,ICASPL,IKDENP,PKDEWI,
     1                    IKDERN,IKDEPF,ISEED,MINN2,
     1                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1                    WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,
     1                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKDEN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IKDENP,PKDEWI
 9014   FORMAT('IKDENP,PKDEWI = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.LE.0)GOTO9090
        DO9015I=1,NPLOTP
          WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016     FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
         CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
 9090   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKDE2(Y,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,
     1                  N,ICASPL,IKDENP,PKDEWI,
     1                  IKDERN,IKDEPF,ISEED,MINN2,
     1                  WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1                  WORK8,WORK9,WORK10,WORK11,WORK12,MAXNXT,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A KERNEL DENSITY PLOT.  USES THE
C              APPLIED STATISTICS ALGORITHM 176 OF B. W. SILVERMAN
C              (COMPUTES KERNEL ESTIMATE USING THE FFT).
C              CURRENTLY, ONLY A GAUSSIAN KERNEL FUNCTION IS
C              SUPPORTED.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2001/8
C     ORIGINAL VERSION--AUGUST    2001.
C     UPDATED         --FEBRUARY  2010. SUPPORT FOR "MULTIPLE" AND
C                                       "REPLICATION" CASES
C     UPDATED         --JULY      2018. OPTIONALLY GENERATE THE CDF
C                                       OR PPF INSTEAD OF PDF
C     UPDATED         --JULY      2018. OPTIONALLY GENERATE RANDOM
C                                       NUMBERS BASED ON KERNEL DENSITY
C     UPDATED         --JULY      2019. SCRATCH STORAGE FOR INTERP
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IKDEPF
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
      CHARACTER*4 IOP
C
      DOUBLE PRECISION DH
      DOUBLE PRECISION DHI
      DOUBLE PRECISION DLO
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION Y(*)
      DOUBLE PRECISION FT(*)
      DOUBLE PRECISION SMOOTH(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
C
      DIMENSION WORK1(*)
      DIMENSION WORK2(*)
      DIMENSION WORK3(*)
      DIMENSION WORK4(*)
      DIMENSION WORK5(*)
      DIMENSION WORK6(*)
      DIMENSION WORK7(*)
      DIMENSION WORK8(*)
      DIMENSION WORK9(*)
      DIMENSION WORK10(*)
      DIMENSION WORK11(4,MAXNXT)
      DIMENSION WORK12(MAXNXT,3)
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
      ISUBN1='DPKD'
      ISUBN2='E2  '
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.MINN2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN KERNEL DENSITY PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO60I=1,N
      IF(Y(I).NE.HOLD)GOTO69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)
   62 FORMAT('      ALL INPUT HORIZONTAL AXIS ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)HOLD
   63 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   69 CONTINUE
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'KDE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPKDE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,IKDENP,PKDEWI
   72   FORMAT('N,IKDENP,PKDEWI = ',I6,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
        WRITE(ICOUT,74)I,REAL(Y(I))
   74   FORMAT('I, Y(I) = ',I8,G15.7)
        CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 2--                                **
C               **  CALL DENEST ROUTINE TO COMPUTE THE      **
C               **  KERNEL DENSITY ESTIMATE.                **
C               **********************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      ICAL=0
      KFLAG=1
      CALL DSORT(Y,Y,N,KFLAG,IERROR)
      DH=DBLE(PKDEWI)
      IF(PKDEWI.LE.0)THEN
        DN=N
        DSUM=0.0D0
        DO200I=1,N
          DX=Y(I)
          DSUM=DSUM+DX
  200   CONTINUE
        DMEAN=DSUM/DN
        DSUM=0.0D0
        DO300I=1,N
          DX=Y(I)
          DSUM=DSUM+(DX-DMEAN)**2
  300   CONTINUE
        DVAR=DSUM/(DN-1.0D0)
        DSD=0.0D0
        IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
C
        P=0.25
        AN=REAL(N)
        ANI=P*(AN+1.0)
        NI=ANI
        A2NI=NI
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.N)NI=N
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.N)NIP1=N
        XPERC1=(1.0-REM)*Y(NI)+REM*Y(NIP1)
C
        P=0.75
        ANI=P*(AN+1.0)
        NI=ANI
        A2NI=NI
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.N)NI=N
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.N)NIP1=N
        XPERC2=(1.0-REM)*Y(NI)+REM*Y(NIP1)
        AIQ=(XPERC2-XPERC1)/1.34
C
CCCCC   DH=DBLE(1.06)*DSD*DN**(-1.0D0/5.0D0)
        DH=0.9D0*MIN(DSD,DBLE(AIQ))*DN**(-1.0D0/5.0D0)
      ENDIF
      DLO=Y(1) - 3.0D0*DH
      DHI=Y(N) + 3.0D0*DH
C
      CALL DENEST(Y,N,DLO,DHI,DH,FT,SMOOTH,IKDENP,ICAL,IERROR)
C
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************
C               **  STEP 3--                                **
C               **  GENERATE CDF OR PPF IF REQUESTED        **
C               **********************************************
C
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFRST=N2+1
      ILAST=N2+IKDENP
      IF(IKDEPF.EQ.'PDF')THEN
        DO410I=1,IKDENP
          N2=N2+1
          Y2(N2)=REAL(SMOOTH(I))
          X2(N2)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          D2(N2)=REAL(NCURVE)
  410   CONTINUE
      ELSEIF(IKDEPF.EQ.'CDF' .OR. IKDEPF.EQ.'PPF')THEN
        DO420I=1,IKDENP
          N2=N2+1
          X2(N2)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          TEMP1(N2)=REAL(SMOOTH(I))
          D2(N2)=REAL(NCURVE)
  420   CONTINUE
        NTEMP=2
        IWRITE='OFF'
        CALL CUMINT(TEMP1,X2,IKDENP,NTEMP,IWRITE,Y2,IBUGG3,IERROR)
        IF(IKDEPF.EQ.'PPF')THEN
          DO430I=IFRST,ILAST
            AVAL=Y2(I)
            Y2(I)=X2(I)
            X2(I)=AVAL
  430     CONTINUE
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 4--                                **
C               **  GENERATE RANDOM NUMBERS BASED ON THE    **
C               **  KERNEL DENSITY APPROXIMATION            **
C               **********************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IKDERN.GE.1)THEN
        IF(IKDEPF.EQ.'PDF')THEN
          NTEMP=2
          IWRITE='OFF'
          CALL CUMINT(Y2(IFRST+1),X2(IFRST+1),IKDENP,NTEMP,IWRITE,TEMP3,
     1                IBUGG3,IERROR)
          ICNT=0
          DO440I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=X2(I)
            TEMP2(ICNT)=TEMP3(ICNT)
  440     CONTINUE
        ELSEIF(IKDEPF.EQ.'CDF')THEN
          ICNT=0
          DO460I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=X2(I)
            TEMP2(ICNT)=Y2(I)
  460     CONTINUE
        ELSEIF(IKDEPF.EQ.'PPF')THEN
          ICNT=0
          DO470I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=Y2(I)
            TEMP2(ICNT)=X2(I)
  470     CONTINUE
        ENDIF
C
        ISTEPN='4B'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL UNIRAN(IKDERN,ISEED,TEMP3)
        CALL SORT(TEMP3,IKDERN,TEMP3)
        CALL INTERP(TEMP1,TEMP2,IKDENP,TEMP3,IKDERN,IWRITE,TEMP4,
     1              WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,
     1              WORK8,WORK9,WORK10,WORK11,WORK12,MAXNXT,
     1              IBUGG3,ISUBRO,IERROR)
C
C       WRITE RESULTS TO "dpst1f.dat"
C
        ISTEPN='4C'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=0
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        DO490I=1,IKDERN
          WRITE(IOUNI1,'(E15.7)')TEMP4(I)
  490   CONTINUE
C
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
      ENDIF
C
      NPLOTV=2
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'KDE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKDE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IERROR,N2
 9012   FORMAT('ICASPL,IERROR,N2 = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)REAL(DLO),REAL(DHI),REAL(DH),REAL(DSD)
 9013   FORMAT('DLO,DHI,DH,DSD = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
        WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016   FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
        CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
 9090   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKDNP(IHARG,IARGT,ARG,NUMARG,
     1IKDENP,IDEFKN,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE NUMBER OF POINTS USED FOR THE KERNEL DENSITY
C              CURVE IN THE KERNEL DENSITY PLOT COMMAND.
C              THE SPECIFIED KERNEL DENSITY POINTS VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE IKDENP.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFKN (A FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--IKDENP  (A  FLOATING POINT VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--2001/8
C     ORIGINAL VERSION--AUGUST    2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
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
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POIN')GOTO1110
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO9000
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'POIN')GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPKDNP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR KERNEL DENSITY POINTS COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      HOLD=IDEFKN
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IKDENP=INT(HOLD+0.5)
      IKLOW=5
      IKHIGH=11
      IF(IKDENP.LE.2**IKLOW)THEN
        IKDENP=2**IKLOW
      ELSEIF(IKDENP.GT.2**IKHIGH)THEN
        IKDENP=2**IKHIGH
      ELSE
        DO1185K=IKLOW,IKHIGH
          IF(IKDENP.GT.2**(K-1).AND.IKDENP.LE.2**K)THEN
            IKDENP=2**K
            GOTO1189
          ENDIF
 1185   CONTINUE
      ENDIF
 1189 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)IKDENP
 1281 FORMAT('THE KERNEL DENSITY POINTS HAS JUST BEEN SET ',
     1       'TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO9000
C
C               ********************************************
C               **  STEP 81--                             **
C               **  TREAT THE    ?    CASE--              **
C               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
C               ********************************************
C
 8100 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)IKDENP
 8111 FORMAT('THE CURRENT KERNEL DENSITY POINTS    IS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)IDEFKN
 8121 FORMAT('THE DEFAULT KERNEL DENSITY POINTS    IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPKDWI(IHARG,IARGT,ARG,NUMARG,
     1PKDEWI,DEFKWI,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE SMOOTHING WIDTH FOR THE
C              TO BE USED FOR THE KERNEL DENSITY ESTIMATOR.
C              THE SPECIFIED KERNEL DENSITY WIDTH VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE PKDEWI.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFKWI (A FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--PKDEWI  (A  FLOATING POINT VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--2001/8
C     ORIGINAL VERSION--AUGUST    2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
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
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')GOTO1110
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO9000
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'WIDT')GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPKDWI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR KERNEL DENSITY WIDTH COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      HOLD=DEFKWI
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PKDEWI=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      IF(PKDEWI.NE.DEFKWI)THEN
        WRITE(ICOUT,1281)PKDEWI
 1281   FORMAT('THE KERNEL DENSITY WIDTH HAS JUST BEEN SET ',
     1         'TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ELSE
        WRITE(ICOUT,1291)
 1291   FORMAT('THE KERNEL DENSITY WIDTH HAS JUST BEEN SET ',
     1         'TO THE DEFAULT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1293)
 1293   FORMAT('DATAPLOT WILL SELECT THE WIDTH BASED ON THE DATA.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
 1289 CONTINUE
      GOTO9000
C
C               ********************************************
C               **  STEP 81--                             **
C               **  TREAT THE    ?    CASE--              **
C               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
C               ********************************************
C
 8100 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)PKDEWI
 8111 FORMAT('THE CURRENT KERNEL DENSITY WIDTH    IS ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)DEFKWI
 8121 FORMAT('THE DEFAULT KERNEL DENSITY WIDTH    IS ',G15.7)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPKEEP(X,N,XREF,NREF,IOP,TAG,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN A GROUP-ID VARIABLE (X), IT MAY BE CONVENIENT
C              AT TIMES TO CREATE A TAG VARIABLE BASED ON A LIST
C              OF LABS TO EITHER KEEP OR OMIT FROM AN ANALYSIS.
C              THE VARIABLE TAG WILL BE SET TO 1 IF THE LAB IS
C              TO BE KEPT OR TO 0 IF THE LAB IS TO BE OMITTED.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS CONTAINING THE GROUP-ID's.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X AND TAG.
C                     --XREF   = THE SINGLE PRECISION VECTOR OF
C                                GROUP-ID's TO BE EITHER KEPT OR
C                                OMITTED.
C                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR XREF.
C                     --IOP    = A CHARACTER SCALAR THAT SPECIFIES
C                                WHETHER TO KEEP OR OMIT LABS BASED
C                                ON XREF.
C     OUTPUT ARGUMENTS--TAG    = THE SINGLE PRECISION VECTOR WHICH WILL
C                                BE CODED AS EITHER 0 OR 1 DEPENDING ON
C                                WHETHER THE LAB WILL BE OMITTED OR
C                                RETAINED.
C     OUTPUT--THE SINGLE PRECISION VECTOR TAG
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     ORIGINAL VERSION--APRIL     2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XREF(*)
      DIMENSION TAG(*)
C
      CHARACTER*4 IOP
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
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
C     INITIALIZE TAG VARIABLE
C
      IF(IOP.EQ.'KEEP')THEN
        ATEMP=0.0
        IF(NREF.LE.0)ATEMP=1.0
        DO21I=1,N
          TAG(I)=ATEMP
   21   CONTINUE
      ELSE
        ATEMP=1.0
        IF(NREF.LE.0)ATEMP=0.0
        DO26I=1,N
          TAG(I)=ATEMP
   26   CONTINUE
      ENDIF
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR IN DPKEEP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,17)
   17   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NREF.LE.0)THEN
C
C       IF NO LIST OF OMITTED/RETAINED ID'S GIVEN, SIMPLY RETURN.
C
        GOTO9000
      ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KEEP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,110)
  110   FORMAT('***** AT THE BEGINNING OF DPKEEP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)N,NREF
  111   FORMAT('N,NREF = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        DO112I=1,N
          WRITE(ICOUT,113)I,X(I)
  113     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
  112   CONTINUE
        DO117I=1,NREF
          WRITE(ICOUT,119)I,XREF(I)
  119     FORMAT('I,XREF(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
  117   CONTINUE
      ENDIF
C
      ATEMP=1.0
      IF(IOP.EQ.'OMIT')ATEMP=0.0
      DO1200I=1,NREF
        XREFI=XREF(I)
        DO1300J=1,N
          IF(X(J).EQ.XREFI)TAG(J)=ATEMP
 1300   CONTINUE
 1200 CONTINUE
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KEEP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKEEP--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,X(I),TAG(I)
 9016     FORMAT('I,X(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKMEA(ICAPSW,IFORSW,ISEED,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--PERFORM EITHER
C                 1. A K-MEANS CLUSTER ANALYSIS (HARTIGAN) OR
C                 2. A MIXTURE OF NORMALS CLUSTER ANALYSIS (HARTIGAN) OR
C                 3. A SINGLE LINKAGE (NEAREST NEIGHBORS) CLUSTER
C                    ANALYSIS (HARTIGAN)
C                 4. A K-MEDOIDS CLUSTER ANALYSIS (ROUSSEEUW AND
C                    KAUFFMAN, CLARA AND PAM)
C                 5. AGNES CLUSTER ANALYSIS (ROUSSEEUW AND
C                    KAUFFMAN, AGNES AND DIANA)
C     REFERENCES--JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
C                 STATISTICS, VOL. 28, NO. 1.
C               --JOHN HARTIGAN (1975), "CLUSTERING ALGORITHMS",
C                 WILEY.
C               --KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN
C                 DATA", WILEY.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/03
C     ORIGINAL VERSION--MARCH     2017.
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=100)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASAN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
C
      DIMENSION YA(20*MAXOBV)
      DIMENSION YC(MAXOBV)
      DIMENSION YD(MAXOBV)
      DIMENSION AN1(MAXOBV)
      DIMENSION AN2(MAXOBV)
      DIMENSION WSS(MAXOBV)
      DIMENSION TEMP1(3*MAXOBV)
C
      DIMENSION IC1(MAXOBV)
      DIMENSION IC2(MAXOBV)
      DIMENSION NC(MAXOBV)
      DIMENSION NCP(MAXOBV)
      DIMENSION ITRAN(MAXOBV)
      DIMENSION ILIVE(MAXOBV)
C
      CHARACTER*8 RLAB(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),YC(1))
      EQUIVALENCE (GARBAG(IGARB2),YD(1))
      EQUIVALENCE (GARBAG(IGARB3),AN1(1))
      EQUIVALENCE (GARBAG(IGARB4),AN2(1))
      EQUIVALENCE (GARBAG(IGARB5),WSS(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGAR10),YA(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),IC1(1))
      EQUIVALENCE (IGARBG(IIGAR2),IC2(1))
      EQUIVALENCE (IGARBG(IIGAR3),NC(1))
      EQUIVALENCE (IGARBG(IIGAR4),NCP(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITRAN(1))
      EQUIVALENCE (IGARBG(IIGAR6),ILIVE(1))
C
      EQUIVALENCE (CGARBG(1),RLAB(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
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
      IERROR='NO'
      ISUBN1='DPKM'
      ISUBN2='EA  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *********************************************
C               **  TREAT THE K-MEANS                CASE  **
C               *********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KMEA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKMEA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICOM,IHARG(1),IHARG(2)
   55   FORMAT('ICOM,IHARG(1),IHARG(2) = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEAN' .AND.
     1   IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMEA'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEAN')THEN
        ILASTC=1
        ICASAN='KMEA'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDO' .AND.
     1   IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDO')THEN
        ILASTC=1
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDI' .AND.
     1   IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDI')THEN
        ILASTC=1
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'MIXT' .AND.
     1   IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='NMIX'
      ELSEIF(ICOM.EQ.'SING' .AND. IHARG(1).EQ.'LINK' .AND.
     1   IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='SLIN'
      ELSEIF(ICOM.EQ.'AGNE' .AND. IHARG(1).EQ.'CLUS')THEN
        ILASTC=1
        ICASAN='AGNE'
      ELSEIF(ICOM.EQ.'AGNE')THEN
        ILASTC=0
        ICASAN='AGNE'
      ELSEIF(ICOM.EQ.'DIAN' .AND. IHARG(1).EQ.'CLUS')THEN
        ILASTC=1
        ICASAN='DIAN'
      ELSEIF(ICOM.EQ.'DIAN')THEN
        ILASTC=0
        ICASAN='DIAN'
      ELSEIF(ICOM.EQ.'FANN' .AND. IHARG(1).EQ.'CLUS')THEN
        ILASTC=1
        ICASAN='FANN'
      ELSEIF(ICOM.EQ.'FANN')THEN
        ILASTC=0
        ICASAN='FANN'
      ELSEIF(ICOM.EQ.'FUZZ' .AND. IHARG(1).EQ.'CLUS')THEN
        ILASTC=1
        ICASAN='FANN'
      ELSEIF(ICOM.EQ.'FUZZ')THEN
        ILASTC=0
        ICASAN='FANN'
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASAN.EQ.'NMIX')THEN
        INAME='NORMAL MIXTURE CLUSTERING'
      ELSEIF(ICASAN.EQ.'SLIN')THEN
        INAME='SINGLE LINKAGE CLUSTERING'
      ELSEIF(ICASAN.EQ.'KMED')THEN
        INAME='K-MEDIODS CLUSTERING'
      ELSE
        INAME='K-MEANS CLUSTERING'
      ENDIF
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=9
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=MAXSPN
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               ****************************************
C               **  STEP 3--                          **
C               **  EXTRACT THE DATA                  **
C               ****************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASAN.NE.'AGNE' .AND. ICASAN.NE.'DIAN')THEN
        IHP='NCLU'
        IHP2='STER'
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        AVAL=VALUE(ILOCV)
        NCLUST=INT(AVAL+0.1)
        IF(NCLUST.LT.2)THEN
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NCLUST=0
      ENDIF
C
      MAXNXT=20*MAXOBV
      ICOL=1
      CALL DPPARY(ICOL,IVALUE,IVALU2,IN,MAXN,MAXNXT,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            YA,NLOCAL,NROW,NCOL,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')THEN
        WRITE(ICOUT,352)NLOCAL,NROW,NCOL,NCLUST
  352   FORMAT('NLOCAL,NROW,NCOL,NCLUST = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO360II=1,MIN(NLOCAL,1000)
          WRITE(ICOUT,362)II,YA(II)
  362     FORMAT('II,YA(II) = ',I8,2X,G15.7)
          CALL DPWRST('XXX','BUG ')
  360   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  PERFORM THE CLUSTER ANALYSIS                   **
C               *****************************************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASAN.EQ.'KMEA')THEN
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO401II=1,NROW
            IF(ISUB(I).EQ.0)GOTO401
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  401     CONTINUE
        ELSE
          J=0
          DO403II=1,NQ
            IF(ISUB(I).EQ.0)GOTO403
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  403     CONTINUE
        ENDIF
        CALL DPKME2(YA,NROW,NCOL,NCLUST,
     1              YC,YD,AN1,AN2,WSS,TEMP1,
     1              IC1,IC2,NC,NCP,ITRAN,ILIVE,
     1              ICAPSW,ICAPTY,IFORSW,MAXOBV,ISEED,
     1              ISUBRO,IBUGA3,IERROR)
      ELSEIF(ICASAN.EQ.'NMIX')THEN
        IF(INMCRL.EQ.'ON')THEN
          J=0
          DO411II=1,NROW
            IF(ISUB(I).EQ.0)GOTO411
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  411     CONTINUE
        ELSE
          J=0
          DO413II=1,NROW
            IF(ISUB(I).EQ.0)GOTO413
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  413     CONTINUE
        ENDIF
        CALL DPNMI2(YA,NROW,NCOL,NCLUST,
     1              GARBAG(1),YC,AN1,AN2,IC1,
     1              IVARN1,IVARN2,RLAB,
     1              ICAPTY,ICAPSW,IFORSW,
     1              ISUBRO,IBUGA3,IERROR)
      ELSEIF(ICASAN.EQ.'KMED')THEN
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO421II=1,NROW
            IF(ISUB(I).EQ.0)GOTO421
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  421     CONTINUE
        ELSE
          J=0
          DO423II=1,NQ
            IF(ISUB(I).EQ.0)GOTO423
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  423     CONTINUE
        ENDIF
        IF(NROW.GT.IKMDPN)THEN
          CALL DPCLA2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,
     1                TEMP1(1),AN1,AN2,YC,
     1                WSS(1),WSS(10000),WSS(20000),
     1                WSS(30000),WSS(40000),
     1                WSS(50000),WSS(60000),WSS(70000),
     1                IC1,IC2,NC,NCP,
     1                ITRAN(1),ITRAN(101),ITRAN(10001),
     1                ITRAN(30001),ITRAN(40001),ITRAN(50001),
     1                ITRAN(60001),
     1                TEMP1(MAXOBV+1),ILIVE,
     1                ICAPSW,ICAPTY,IFORSW,MAXOBV,ISEED,
     1                ISUBRO,IBUGA3,IERROR)
        ELSE
          CALL DPPAM2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,
     1                YC,YD,AN1,AN2,WSS,
     1                TEMP1(1),TEMP1(MAXOBV+1),TEMP1(MAXOBV+2),
     1                IC1,IC2,NC,NCP,ITRAN,
     1                ICAPSW,ICAPTY,IFORSW,MAXOBV,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
      ELSEIF(ICASAN.EQ.'AGNE' .OR. ICASAN.EQ.'DIAN')THEN
        IF(NROW.GT.IAGNMS)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,436)
  436     FORMAT('***** ERROR IN AGNES CLUSTERING--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,437)NROW
  437     FORMAT('      NUMBER OF OBJECTS TO BE CLUSTERED (',I8,') ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,438)IAGNMS
  438     FORMAT('      IS GREATER THAN ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO431II=1,NROW
            IF(ISUB(I).EQ.0)GOTO431
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  431     CONTINUE
        ELSE
          J=0
          DO433II=1,NQ
            IF(ISUB(I).EQ.0)GOTO433
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  433     CONTINUE
        ENDIF
C
        CALL DPAGN2(YA,NROW,NCOL,IVARN1,IVARN2,
     1                YC,YD,AN1,AN2,WSS,TEMP1,TEMP1(MAXOBV+1),
     1                IC1,IC2,NC,NCP,ITRAN,
     1                ICASAN,ICAPSW,ICAPTY,IFORSW,MAXOBV,
     1                ISUBRO,IBUGA3,IERROR)
      ELSEIF(ICASAN.EQ.'FANN')THEN
        IF(NROW.GT.IFANMS)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,446)
  446     FORMAT('***** ERROR IN FANNY CLUSTERING--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,437)NROW
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,438)IFANMS
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO441II=1,NROW
            IF(ISUB(I).EQ.0)GOTO441
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  441     CONTINUE
        ELSE
          J=0
          DO443II=1,NQ
            IF(ISUB(I).EQ.0)GOTO443
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  443     CONTINUE
        ENDIF
        CALL DPFAN2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,
     1              AN1,AN2,TEMP1(1),TEMP1(MAXOBV+1),
     1              TEMP1(2*MAXOBV+1),YC,YD,WSS,
     1              IC1,IC2,NC,
     1              ICAPSW,ICAPTY,IFORSW,MAXOBV,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KMEA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKMEA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKME2(YA,NROW,NCOL,NCLUST,
     1                  YC,YD,AN1,AN2,WSS,TEMP1,
     1                  IC1,IC2,NC,NCP,ITRAN,ILIVE,
     1                  ICAPSW,ICAPTY,IFORSW,MAXNXT,ISEED,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--PERFORM A K-MEANS CLUSTER ANALYSIS USING HARTIGAN'S
C              APPLIED STATISTICS 136 ALGORITHM.
C     REFERENCES--JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
C                 STATISTICS, VOL. 28, NO. 1.
C               --ROUSSEEUW (1987), "SILHOUETTES: A GRAPHICAL AID TO THE
C                 INTERPRETATION AND VALIDATION OF CLUSTER ANALYSIS",
C                 JOURNAL OF COMPUTATIONAL AND APPLIED MATHEMATICS,
C                 VOL. 20, PP. 53-65, NORTH HOLLAND.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/03
C     ORIGINAL VERSION--MARCH       2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION YA(NROW,NCOL)
      DIMENSION YC(NCLUST,NCOL)
      DIMENSION YD(*)
      DIMENSION AN1(*)
      DIMENSION AN2(*)
      DIMENSION WSS(*)
      DIMENSION TEMP1(*)
C
      INTEGER IC1(*)
      INTEGER IC2(*)
      INTEGER NC(*)
      INTEGER NCP(*)
      INTEGER ITRAN(*)
      INTEGER ILIVE(*)
C
      DIMENSION ALOCSV(200)
      DIMENSION SCALSV(200)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICASE
      CHARACTER*4 ICASPL
      CHARACTER*4 ITYP3
      CHARACTER*4 IOP
      CHARACTER*10 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER(NUMCLI=3)
      PARAMETER(MAXLIN=3)
      PARAMETER(MAXROW=35)
      CHARACTER*65 ITITLE
      CHARACTER*60 ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*20 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      NCOLSP(MAXLIN,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
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
      ISUBN1='DPKM'
      ISUBN2='E2  '
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KME2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPKME2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)NROW,NCOL,NCLUST,IKMEIN
   72   FORMAT('NROW,NCOL,NCLUST,IKMEIN = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO75I=1,NROW
          WRITE(ICOUT,77)I,(YA(I,J),J=1,MIN(NCOL,3))
   77     FORMAT('I,YA(I,1),YA(I,2),YA(I,3) = ',I8,2X,3G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C               ******************************
C               **   STEP 1A--              **
C               **   SCALE IF REQUESTED     **
C               ******************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO90I=1,200
        ALOCSV(I)=0.0
        SCALSV(I)=1.0
   90 CONTINUE
C
      IF(IKMESC.EQ.'OFF')GOTO199
C
      DO101JJ=1,NCOL
        DO103II=1,NROW
          TEMP1(II)=YA(II,JJ)
  103   CONTINUE
        IF(ISTALO.EQ.'MEAN')THEN
          CALL MEAN(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MEDI')THEN
          CALL MEDIAN(TEMP1,NROW,IWRITE,AN1,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MIDM')THEN
          CALL MIDMEA(TEMP1,NROW,IWRITE,AN1,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'HARM')THEN
          CALL HARMEA(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MINI')THEN
          CALL MINIM(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'GEOM')THEN
          CALL GEOMEA(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'BILO')THEN
          CALL BIWLOC(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H15 ')THEN
          NCUT=0
          C=1.5
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H10 ')THEN
          NCUT=0
          C=1.0
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H12 ')THEN
          NCUT=0
          C=1.2
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H17 ')THEN
          NCUT=0
          C=1.7
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H20 ')THEN
          NCUT=0
          C=2.0
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSE
          CALL MEAN(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
C
        IF(ISTASC.EQ.'SD  ')THEN
          CALL SD(TEMP1,NROW,IWRITE,XSD,IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H15S')THEN
          NCUT=0
          C=1.5
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H10S')THEN
          NCUT=0
          C=1.0
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H12S')THEN
          NCUT=0
          C=1.2
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H17S')THEN
          NCUT=0
          C=1.7
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H20S')THEN
          NCUT=0
          C=2.0
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'BISC')THEN
          CALL BIWSCA(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MAD ')THEN
          CALL MAD(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,
     1             IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MADN')THEN
          CALL MAD(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,
     1             IBUGA3,IERROR)
          XSD=XSD/0.67449
        ELSEIF(ISTASC.EQ.'AAD ')THEN
          CALL AAD(TEMP1,NROW,IWRITE,AN1,MAXNXT,XSD,'MEAN',
     1             IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'IQRA')THEN
          CALL LOWQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH1,
     1                IBUGA3,IERROR)
          CALL UPPQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH2,
     1                IBUGA3,IERROR)
          XSD=RIGH2-RIGH1
        ELSEIF(ISTASC.EQ.'NIQR')THEN
          CALL LOWQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH1,
     1                IBUGA3,IERROR)
          CALL UPPQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH2,
     1                IBUGA3,IERROR)
          XSD=0.7413*(RIGH2-RIGH1)
        ELSEIF(ISTASC.EQ.'SNSC')THEN
          XSD=SN(TEMP1,NROW,AN1,AN2,WSS)
        ELSEIF(ISTASC.EQ.'RANG')THEN
          CALL MINIM(TEMP1,NROW,IWRITE,XMIN,IBUGA3,IERROR)
          CALL MAXIM(TEMP1,NROW,IWRITE,XMAX,IBUGA3,IERROR)
          XSD=XMAX - XMIN
        ELSE
          CALL SD(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
C
        IF(XSD.LE.0.0)THEN
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,106)JJ
  106     FORMAT('       VARIABLE ',I4,' HAS ZERO STANDARD DEVIATION ',
     1           'WHEN SCALING REQUESTED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ALOCSV(JJ)=XMEAN
        SCALSV(JJ)=XSD
        DO105II=1,NROW
          AVAL=(YA(II,JJ)-XMEAN)/XSD
          YA(II,JJ)=AVAL
  105   CONTINUE
  101 CONTINUE
C
  199 CONTINUE
C
C               ******************************
C               **   STEP 1B--              **
C               **   CREATE INITIAL CLUSTER **
C               ******************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
C     RANDOMLY SELECT ROWS TO USE AS INITIAL CLUSTER
C     CENTERS.
C
      IF(IKMEIN.EQ.'RAND')THEN
        CALL RANPER(NROW,ISEED,AN1)
        DO110II=1,NCLUST
          IROWT=INT(AN1(II)+0.1)
          DO120JJ=1,NCOL
            YC(II,JJ)=YA(IROWT,JJ)
  120     CONTINUE
  110   CONTINUE
      ELSE
C
C       USE ALGORITHM SUGGESTED BY HARTIGAN TO DEFINE
C       INITIAL CLUSTER CENTERS.
C
C       FIND COLUMN MEANS
C
        DO150II=1,NCOL
          CALL MEAN(YA(1,II),NROW,IWRITE,XMEAN,IBUGA3,IERROR)
          AN1(II)=XMEAN
  150   CONTINUE
C
C       FIND DISTANCE FROM EACH ROW TO MEAN MATRIX
C
        DO160II=1,NROW
          DO165JJ=1,NCOL
            AN2(JJ)=YA(II,JJ)
  165     CONTINUE
          ICASPL='VEDI'
          CALL VECARI(AN1,AN2,NCOL,ICASPL,IWRITE,
     1                TEMP1,N3,ADIST,ITYP3,
     1                IBUGA3,ISUBRO,IERROR)
          YD(II)=ADIST
  160   CONTINUE
        CALL SORTI(YD,NROW,YD,WSS)
C
C       SELECT ROW 1 + (L-1)*(NROW/NCLUST) FOR CLUSTER L
C
        DO170II=1,NCLUST
          AVAL=1.0 + REAL(II-1)*(REAL(NROW)/REAL(NCLUST))
          IVAL=INT(AVAL+0.1)
          IF(IVAL.LT.1)IVAL=1
          IF(IVAL.GT.NROW)IVAL=NROW
          DO180JJ=1,NROW
            JROWT=INT(WSS(JJ)+0.1)
            IF(IVAL.EQ.JROWT)THEN
              IROWT=JJ
              GOTO189
            ENDIF
  180     CONTINUE
          IROWT=NROW
  189     CONTINUE
          DO190JJ=1,NCOL
            YC(II,JJ)=YA(IROWT,JJ)
  190     CONTINUE
  170   CONTINUE
      ENDIF
C
C               ************************************
C               **   STEP 2--                     **
C               **   PERFORM THE CLUSTER ANALYSIS **
C               ************************************
C
      ITER=50
      CALL KMNS(YA,NROW,NCOL,YC,NCLUST,
     1          IC1,IC2,NC,AN1,AN2,NCP,YD,ITRAN,ILIVE,
     1          ITER,WSS,IFAULT)
C
      IF(IFAULT.EQ.1)THEN
        WRITE(ICOUT,211)
  211   FORMAT('****** ERROR IN K-MEANS CLUSTERING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
  213   FORMAT('       AT LEAST ONE CLUSTER IS EMPTY AFTER THE ',
     1         'INITIAL ASSIGNMENT.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(IFAULT.EQ.2)THEN
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)ITER
  221   FORMAT('       THE MAXIMUM NUMBER IF ITERATIONS (',I3,') WAS ',
     1         'EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(IFAULT.EQ.3)THEN
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,231)NCLUST
  231   FORMAT('       THE NUMBER OF CLUSTERS (',I5,') IS LESS THAN ',
     1         'TWO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,233)NROW
  233   FORMAT('       OR GREATER THAN THE NUMBER OF OBSERVATIONS (',
     1         I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************
C               **   STEP 3--               **
C               **   WRITE OUT EVERYTHING   **
C               ******************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CWS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO8000
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
      ITITLE='Summary of K-Means Cluster Analysis'
      NCTITL=35
      ITITL9=' '
      NCTIT9=0
C
      NUMCOL=3
      NUMLIN=3
C
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      NCOLSP(1,1)=1
      ITITL2(1,2)='Number'
      NCTIT2(1,2)=6
      NCOLSP(1,2)=1
      ITITL2(1,3)='Within'
      NCTIT2(1,3)=6
      NCOLSP(1,3)=1
C
      ITITL2(2,1)=' '
      NCTIT2(2,1)=0
      NCOLSP(2,1)=1
      ITITL2(2,2)='of Points'
      NCTIT2(2,2)=9
      NCOLSP(2,2)=1
      ITITL2(2,3)='Cluster'
      NCTIT2(2,3)=7
      NCOLSP(2,3)=1
C
      ITITL2(3,1)='Cluster'
      NCTIT2(3,1)=7
      NCOLSP(3,1)=1
      ITITL2(3,2)='in Cluster'
      NCTIT2(3,2)=10
      NCOLSP(3,2)=1
      ITITL2(3,3)='Sum of Squares'
      NCTIT2(3,3)=14
      NCOLSP(3,3)=1
C
      IWHTML(1)=150
      IWHTML(2)=200
      IWHTML(3)=200
      IINC1=1200
      IINC2=1800
      IWRTF(1)=IINC1
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC2
C
      NMAX=0
      ICNT=0
      ICNT2=0
      DO3010I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        IF(I.EQ.3)NTOT(I)=18
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
 3010 CONTINUE
C
      DO3020J=1,NCLUST
C
        ICNT=ICNT+1
        IF(ICNT.GT.MAXROW)THEN
          ICNT=ICNT-1
          IF(ICAPTY.EQ.'LATE')THEN
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
          ELSE
            IFRST=.TRUE.
            IFLAGS=.TRUE.
            IF(ICNT2.GT.0)THEN
              IFRST=.FALSE.
              IFLAGS=.FALSE.
            ENDIF
            IFLAGE=.FALSE.
            ILAST=.FALSE.
            IF(J.EQ.ICNT3)THEN
              ILAST=.TRUE.
              IFLAGE=.TRUE.
            ENDIF
          ENDIF
          CALL DPDT5B(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                NCOLSP,ROWSEP,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
          ICNT=1
          ICNT2=ICNT2+1
        ENDIF
C
        DO3030I=1,NUMCOL
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.LE.2)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
 3030   CONTINUE
        AMAT(ICNT,1)=REAL(J)
        AMAT(ICNT,2)=REAL(NC(J))
        AMAT(ICNT,3)=WSS(J)
        ROWSEP(ICNT)=0
 3020 CONTINUE
C
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
        IFLAGE=.TRUE.
        CALL DPDT5B(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              NCOLSP,ROWSEP,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
C               ************************************
C               **   STEP 4A--                    **
C               **   WRITE INFORMATION TO FILES   **
C               ************************************
C
 8000 CONTINUE
C
      ISTEPN='4A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLAG4=0
      IF(IKMESI.EQ.'ON')IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      DO8010I=1,NROW
        WRITE(IOUNI1,'(E15.7)')REAL(IC1(I))
 8010 CONTINUE
C
      DO8020I=1,NCLUST
        WRITE(IOUNI2,'(2E15.7)')WSS(I),REAL(NC(I))
 8020 CONTINUE
C
      IFORMT='(   E15.7)'
      WRITE(IFORMT(2:4),'(I3)')NCOL
      DO8030I=1,NCLUST
        DO8035J=1,MIN(NCOL,200)
          YC(I,J)=ALOCSV(J) + SCALSV(J)*YC(I,J)
 8035   CONTINUE
        WRITE(IOUNI3,IFORMT)(YC(I,J),J=1,NCOL)
 8030 CONTINUE
C
C               *****************************************
C               **   STEP 4B--                         **
C               **   CREATE VALUES FOR SILHOUETTE PLOT **
C               *****************************************
C
      ISTEPN='4B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CWS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE THE s(i) VALUE AS
C
C        s(i) = (b(i) - a(i))/max{a(i),b(i)}
C
C     WHERE
C
C        a(i)   = AVERAGE DISSIMILARITY OF THE i-TH POINT WITHH
C                 ALL OTHER POINTS IN THE CLUSTER TO WHICH IT
C                 BELONGS
C
C        b(i)   = LOWEST AVERAGE DISSIMILARITY OF THE i-TH POINT
C                 WITH ALL OTHER CLUSTERS.
C
C     USE ONE-PASS MEAN ALGORITHMS TO KEEP TRACK OF AVERAGE
C     DISSIMILARITY OF ALL CLUSTERS.  THE ONE-PASS FORMUALA IS
C
C         M(K)=X1                            K = 1
C             =M(K-1) + (X(K) - M(K-1))/K    K = 2, ...., N
C
      IF(IKMESI.EQ.'ON')THEN
C
        DO8110II=1,NROW
          ICLUS1=IC1(II)
          DO8112JJ=1,NCOL
            AN1(JJ)=YA(II,JJ)
 8112     CONTINUE
          ICASPL='VEDI'
          DO8114KK=1,NCLUST
            YD(KK)=CPUMIN
            IC2(KK)=0
 8114     CONTINUE
C
          DO8120JJ=1,NROW
            IF(II.EQ.JJ)GOTO8120
            ICLUS2=IC1(JJ)
            DO8122KK=1,NCOL
              AN2(KK)=YA(JJ,KK)
 8122       CONTINUE
            CALL VECARI(AN1,AN2,NCOL,ICASPL,IWRITE,
     1                  TEMP1,N3,ADIST,ITYP3,
     1                  IBUGA3,ISUBRO,IERROR)
            IF(ICLUS1.EQ.ICLUS2)THEN
              IC2(ICLUS1)=IC2(ICLUS1)+1
              IF(IC2(ICLUS1).EQ.1)THEN
                YD(ICLUS1)=ADIST
              ELSE
                TERM1=(ADIST - YD(ICLUS1))/REAL(IC2(ICLUS1))
                YD(ICLUS1)=YD(ICLUS1) + TERM1
              ENDIF
            ELSE
              IC2(ICLUS2)=IC2(ICLUS2)+1
              IF(IC2(ICLUS2).EQ.1)THEN
                YD(ICLUS2)=ADIST
              ELSE
                TERM1=(ADIST - YD(ICLUS2))/REAL(IC2(ICLUS2))
                YD(ICLUS2)=YD(ICLUS2) + TERM1
              ENDIF
            ENDIF
 8120     CONTINUE
C
          AI=YD(ICLUS1)
          BI=CPUMAX
          DO8130JJ=1,NCLUST
            IF(JJ.EQ.ICLUS1)GOTO8130
            IF(YD(JJ).LT.BI)BI=YD(JJ)
 8130     CONTINUE
          WSS(II)=(BI - AI)/MAX(AI,BI)
C
 8110   CONTINUE
C
        DO8140I=1,NROW
          WRITE(IOUNI4,'(2E15.7)')REAL(IC1(I)),WSS(I)
 8140   CONTINUE
      ENDIF
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8091)
 8091   FORMAT('THE CLUSTER ID VALUES ARE WRITTEN TO dpst1f.dat')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8093)
 8093   FORMAT('THE WITHIN-CLUSTER SUM OF SQUARES AND ',
     1         'THE NUMBER OF POINTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8095)
 8095   FORMAT('FOR EACH CLUSTER ARE WRITTEN TO dpst2f.dat')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8097)
 8097   FORMAT('THE CLUSTER CENTERS ARE WRITTEN TO dpst3f.dat')
        CALL DPWRST('XXX','BUG ')
        IF(IKMESI.EQ.'ON')THEN
          WRITE(ICOUT,8099)
 8099     FORMAT('THE SILHOUETTE VALUES ARE WRITTEN TO dpst4f.dat')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KME2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKME2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKNOT(IHARG,IHARG2,NUMARG,IDEFK1,IDEFK2,
     1IKNOT1,IKNOT2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE USER VARIABLE NAME IN WHICH
C              THE KNOTS FOR SPLINE FITTING RESIDE.
C              CHARACTERS 1 TO 4 OF THE SPECIFIED KNOT NAME
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IKNOT1;
C              CHARACTERS 5 TO 8 OF THE SPECIFIED KNOT NAME
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IKNOT2.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IHARG2 (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFK1 (A  HOLLERITH VARIABLE)
C                     --IDEFK2 (A  HOLLERITH VARIABLE)
C     OUTPUT ARGUMENTS--IKNOT1 (A  HOLLERITH VARIABLE)
C                     --IKNOT2 (A  HOLLERITH VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER 1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IDEFK1
      CHARACTER*4 IDEFK2
      CHARACTER*4 IKNOT1
      CHARACTER*4 IKNOT2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      IFOUND='NO'
      IERROR='NO'
C
      GOTO1110
C
 1110 CONTINUE
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD1=IDEFK1
      IHOLD2=IDEFK2
      GOTO1180
C
 1160 CONTINUE
      IHOLD1=IHARG(NUMARG)
      IHOLD2=IHARG2(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IKNOT1=IHOLD1
      IKNOT2=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IKNOT1,IKNOT2
 1181 FORMAT('THE KNOTS VARIABLE HAS JUST BEEN DESIGNATED AS ',
     1A4,A4)
      CALL DPWRST('XXX','BUG ')
      IF(IKNOT1.EQ.'    '.AND.IKNOT2.EQ.'    ')WRITE(ICOUT,1182)
 1182 FORMAT('(THAT IS, THE NO-KNOTS CASE IS BEING ASSUMED)')
      IF(IKNOT1.EQ.'    '.AND.IKNOT2.EQ.'    ')CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPKLOT(XTEMP1,XTEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A 2-SAMPLE KLOTZ TEST FOR EQUAL VARIANCES
C     EXAMPLE--KLOTZ TEST Y1 Y2
C              KLOTZ TEST Y1 Y2 Y3 Y4
C              KLOTZ TEST Y1 TO Y10
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 ICASE
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
C---------------------------------------------------------------------
C
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      DIMENSION YRANK(2*MAXOBV)
      DIMENSION YTEMP(2*MAXOBV)
      DIMENSION XTEMP3(2*MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),YRANK(1))
      EQUIVALENCE(GARBAG(IGARB3),YTEMP(1))
      EQUIVALENCE(GARBAG(IGARB5),XTEMP3(1))
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
      ISUBN1='DPKL'
      ISUBN2='OT  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IFOUND='NO'
      IERROR='NO'
C
C               ************************************************
C               **  TREAT THE KLOTZ TEST CASE                 **
C               ************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KLOT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************************
C               **  STEP 1--                                           **
C               **  EXTRACT THE COMMAND                                **
C               *********************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTZ=9999
      ICASAN='KLOT'
      ICASA2='TWOT'
C
C     LOOK FOR:
C
C          KLOTZ TEST
C          LOWER TAILED
C          UPPER TAILED
C
      DO100I=0,NUMARG-1
C
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'KLOT' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='KLOT'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='LOWE'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='UPPE'
          ILASTZ=MAX(ILASTZ,I+1)
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DPKLOT: ICASAN,ICASA2,ISHIFT = ',
     1         2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='KLOTZ TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO285I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 3A--                                       **
C               **  CASE 1: TWO RESPONSE VARIABLES, NO REPLICATION  **
C               **          HANDLE MULTIPLE RESPONSE VARIABLES      **
C               **          DIFFERENTLY FOR ONE SAMPLE AND TWO      **
C               **          SAMPLE TESTS.                           **
C               ******************************************************
C
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVA2=1
      DO5210I=1,NUMVAR
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,Y,Y,NS1,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        ISTRT2=I+1
        ISTOP2=NUMVAR
C
        DO5220J=ISTRT2,ISTOP2
C
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                X,X,X,NS2,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************
C               **  STEP 52--                            **
C               **  PERFORM A KLOTZ RANK SUM TEST        **
C               *******************************************
C
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KLOT')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPKLOT, BEFORE CALL DPKLO2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO5215II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y(II),X(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
C
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DPKLO2(Y,NS1,X,NS2,ICASA2,
     1               YRANK,YTEMP,XTEMP3,MAXNXT,
     1               ICAPSW,ICAPTY,IFORSW,
     1               IVARID,IVARI2,IVARI3,IVARI4,
     1               STATVA,STATCD,PVAL2T,PVALLT,PVALUT,
     1               CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,
     1               CTU999,CTU995,CTU990,CT975,CTU950,CTU900,
     1               IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          CALL DPMNN5(ICASA2,
     1                STATVA,STATCD,
     1                PVAL2T,PVALLT,PVALUT,
     1                CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,
     1                CTU999,CTU995,CTU990,CT975,CTU950,CTU900,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
 5220   CONTINUE
 5210 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KLOT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKLO2(Y1,N1,Y2,N2,ICASAN,
     1                  TEMP1,TEMP2,TEMP3,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  IVARID,IVARI2,IVARI3,IVARI4,
     1                  STATVA,STATCD,PVAL2T,PVALLT,PVALUT,
     1                  CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,
     1                  CTU999,CTU995,CTU990,CTU975,CTU950,CTU900,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A 2-SAMPLE KLOTZ TEST FOR
C              EQUAL VARIANCES
C     EXAMPLE--KLOTZ TEST Y1 Y2
C     SAMPLE 1 IS IN INPUT VECTOR Y1
C              (WITH N1 OBSERVATIONS).
C     SAMPLE 2 IS IN INPUT VECTOR Y2
C              (WITH N1 OBSERVATIONS).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASAN
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
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
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
C
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=4)
      REAL ALPHA2(NUMAL2)
C
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=25)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
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
      DATA ALPHA/0.90, 0.95, 0.975, 0.99, 0.995, 0.999/
      DATA ALPHA2/0.80, 0.90, 0.95, 0.99/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPKL'
      ISUBN2='O2  '
C
      IERROR='NO'
      IWRITE='OFF'
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
      CTL001=CPUMIN
      CTL005=CPUMIN
      CTL010=CPUMIN
      CTL025=CPUMIN
      CTL050=CPUMIN
      CTL100=CPUMIN
      CTU900=CPUMIN
      CTU950=CPUMIN
      CTU975=CPUMIN
      CTU990=CPUMIN
      CTU995=CPUMIN
      CTU999=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KLO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPKLO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN
   52   FORMAT('IBUGA3,ISUBRO,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID,IVARI2,IVARI3,IVARI4
   53   FORMAT('IVARID,IVARI2,IVARI3,IVARI4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,N2,NUMDIG
   55   FORMAT('N1,N2,NUMDIG = ',3I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ************************************
C               **   STEP 1--                     **
C               **   CALL DPKLO3 TO COMPUTE THE   **
C               **   BASIC TEST STATISTIC.        **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPKLO3(Y1,N1,Y2,N2,
     1            TEMP1,TEMP2,TEMP3,MAXNXT,
     1            STATVA,STATCD,PVAL2T,PVALLT,PVALUT,
     1            IBUGA3,ISUBRO,IERROR)
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL VAR(Y1,N1,IWRITE,YVAR1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      CALL VAR(Y2,N2,IWRITE,YVAR2,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 21--                        **
C               **  COMPUTE THE CRITICAL VALUES FOR  **
C               **  VARIOUS VALUES OF ALPHA          **
C               ***************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     LARGE SAMPLE NORMAL APPROXIMATION VALUES FIRST
C
      CALL NORPPF(.005,CTL005)
      CALL NORPPF(.010,CTL010)
      CALL NORPPF(.025,CTL025)
      CALL NORPPF(.050,CTL050)
      CALL NORPPF(.100,CTL100)
      CALL NORPPF(.200,CTL200)
      CALL NORPPF(.500,CTL500)
      CALL NORPPF(.500,CTU500)
      CALL NORPPF(.800,CTU800)
      CALL NORPPF(.900,CTU900)
      CALL NORPPF(.950,CTU950)
      CALL NORPPF(.975,CTU975)
      CALL NORPPF(.990,CTU990)
      CALL NORPPF(.995,CTU995)
C
C               *************************************************
C               **   STEP 22--                                 **
C               **   WRITE OUT EVERYTHING                      **
C               **   FOR A KLOTZ  TEST                         **
C               *************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      IF(ICASAN.EQ.'LOWE')THEN
        ITITLE='Two Sample Lower-Tailed Klotz Test'
        NCTITL=34
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        ITITLE='Two Sample Upper-Tailed Klotz Test'
        NCTITL=34
      ELSE
        ITITLE='Two Sample Two-Sided Klotz Test'
        NCTITL=31
      ENDIF
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
      ITEXT(ICNT)='First Response Variable: '
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Var(Y1) = Var(Y2)'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Var(Y1) <> Var(Y2)'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations for Sample 1:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean for Sample 1:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=YMEAN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Variance for Sample 1:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=YVAR1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations for Sample 2:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean for Sample 2:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=YMEAN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Variance for Sample 2:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=YVAR2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test (Normal Approximation):'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (2-tailed test):'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=PVAL2T
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (lower-tailed test):'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=PVALLT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (upper-tailed test):'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=PVALUT
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO2110I=1,NUMROW
        NTOT(I)=15
 2110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='21A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Two-Tailed Test: Normal Approximation'
      NCTITL=37
      ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) <> Var(Y2)'
      NCTIT9=45
C
      DO2130J=1,NUMCLI
        DO2140I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
C
      NUMCOL=4
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
C
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (+/-)'
      NCTIT2(3,3)=11
C
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
C
      NMAX=0
      DO2150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 2150 CONTINUE
C
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
C
      ICNT=NUMAL2
      DO2160J=1,NUMAL2
C
        AMAT(J,2)=STATVA
        ALPHAT=ALPHA2(J)
        ATEMP=(1.0 - ALPHAT)/2.0
        ATEMP=1.0 - ATEMP
        CALL NORPPF(ATEMP,CUTTMP)
        AMAT(J,3)=CUTTMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(ABS(STATVA).LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
C
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 2160 CONTINUE
C
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
C
      IF(ICASAN.EQ.'TWOT')THEN
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASAN.EQ.'LOWE')THEN
C
        ITITLE='Lower-Tailed Test: Normal Approximation'
        NCTITL=39
        ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) < Var(Y2)'
        NCTIT9=44
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
        NUMCOL=4
C
        NMAX=0
        DO2250I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2250   CONTINUE
C
        ICNT=NUMALP
        DO2260J=1,NUMALP
C
          AMAT(J,2)=STATVA
          ALPHAT=ALPHA(J)
          ATEMP=(1.0 - ALPHAT)
          CALL NORPPF(ATEMP,CUTTMP)
          AMAT(J,3)=CUTTMP
          IVALUE(J,4)(1:6)='ACCEPT'
          IF(ABS(STATVA).LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='REJECT'
          ENDIF
          NCVALU(J,4)=6
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 2260   CONTINUE
C
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASAN.EQ.'UPPE')THEN
C
        ITITLE='Upper-Tailed Test: Normal Approximation'
        NCTITL=39
        ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) > Var(Y2)'
        NCTIT9=44
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
        NUMCOL=4
C
        NMAX=0
        DO2350I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2350   CONTINUE
C
          ICNT=NUMALP
        DO2360J=1,NUMALP
C
          AMAT(J,2)=STATVA
          ALPHAT=ALPHA(J)
          ATEMP=ALPHAT
          CALL NORPPF(ATEMP,CUTTMP)
          AMAT(J,3)=CUTTMP
          IVALUE(J,4)(1:6)='ACCEPT'
          IF(ABS(STATVA).GT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='REJECT'
          ENDIF
          NCVALU(J,4)=6
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 2360   CONTINUE
C
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KLO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKLO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT
 9013   FORMAT('STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT = ',6G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKLO3(Y1,N1,Y2,N2,
     1                  TEMP1,TEMP2,YRANK,MAXNXT,
     1                  STATVA,STATCD,PVAL2T,PVALLT,PVALUT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE KLOTZ 2-SAMPLE TEST STATISTIC
C              FOR EQUAL VARIANCES AND ASSOCIATED CDF AND P-VALUES.
C
C              THIS PART IS EXTRACTED FROM DPKLO2 IN ORDER TO
C              ALLOW IT TO BE COMPUTED FROM THE "STATISTICS" ROUTINES
C              (E.G., STATISTIC PLOT, BOOTSTRAP).
C
C     EXAMPLE--KLOTZ TEST Y1 Y2
C              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS)
C              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 401 - 402.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION RSUM1
      DOUBLE PRECISION RSUM2
      DOUBLE PRECISION RSUM3
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DENOM
      DOUBLE PRECISION DRANK
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION YRANK(*)
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
      ISUBN1='DPKL'
      ISUBN2='O3  '
C
      IERROR='NO'
      IWRITE='OFF'
C
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPKLO3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2
   52   FORMAT('IBUGA3,ISUBRO,N1,N2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 01--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='01'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN KLOTZ TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'FIRST RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 2 OR LARGER.  SUCH WAS NOT THE ',
     1         'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N1
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N2.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,122)
  122   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'SECOND RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO135I=2,N1
        IF(Y1(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
  139 CONTINUE
C
      HOLD=Y2(1)
      DO145I=2,N1
        IF(Y2(I).NE.HOLD)GOTO149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,141)HOLD
  141 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
  149 CONTINUE
C
C               ************************************
C               **   STEP 11--                    **
C               **   COMPUTE KLOTZ    TEST        **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE RANKS, BUT SUBTRACT MEANS FROM DATA FIRST
C
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      DO1100I=1,N1
        TEMP1(I)=Y1(I) - YMEAN1
 1100 CONTINUE
      NTOT=N1
      DO1110I=1,N2
        NTOT=NTOT+1
        TEMP1(NTOT)=Y2(I) - YMEAN2
 1110 CONTINUE
      CALL RANK(TEMP1,NTOT,IWRITE,YRANK,TEMP2,MAXNXT,IBUGA3,IERROR)
C
C     NOW COMPUTE NORMAL SCORES
C
      DO1120I=1,NTOT
        ATEMP=YRANK(I)/REAL(NTOT+1)
        CALL NORPPF(ATEMP,APPF)
        YRANK(I)=APPF
 1120 CONTINUE
C
C     COMPUTE KLOTZ TEST STATISTIC:
C
C         T = SUM[i=1 to N1][A(i)**2 - (N1/N)*SUM[i=1 to N][A(i)**2]/
C             SQRT{(N1*N2/(N*(N-1))*[SUM[i=1 to N][A(i)**4] -
C             (1/N)*(SUM[i=1 to N][A(i)**2)**2]}
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      RSUM1=0.0D0
      RSUM2=0.0D0
      RSUM3=0.0D0
C
      DO1210I=1,N1
        DRANK=DBLE(YRANK(I))
        RSUM1=RSUM1 + DRANK**2
 1210 CONTINUE
C
      DO1220I=1,NTOT
        DRANK=DBLE(YRANK(I))
        RSUM2=RSUM2 + DRANK**2
        RSUM3=RSUM3 + DRANK**4
 1220 CONTINUE
C
      AN1=REAL(N1)
      AN2=REAL(N2)
      AN=REAL(N1 + N2)
      DNUM=RSUM1 - DBLE(AN1/AN)*RSUM2
      C1=DBLE(AN1*AN2/(AN*(AN-1.0)))
      C2=DBLE(1.0/AN)
      DENOM=C1*(RSUM3 - C2*(RSUM2**2))
      IF(DENOM.GE.0.0D0)THEN
        STATVA=DNUM/DSQRT(DENOM)
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1231)
 1231   FORMAT('      UNABLE TO COMPUTE THE KLOTZ STATISTIC.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     CDF AND P-VALUES COMPUTED FROM STANDARD NORMAL APPROXIMATION
C
      CALL NORCDF(STATVA,VAL1)
      VAL2=1.0 - VAL1
      VAL=MIN(VAL1,VAL2)
      PVAL2T=2.0*VAL
      PVALLT=VAL1
      PVALUT=VAL2
      STATCD=VAL1
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKLO3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATCD
 9013   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)PVALLT,PVALUT,PVAL2T
 9014   FORMAT('PVALLT,PVALUT,PVAL2T = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKRUS(TEMP4,TEMP5,MAXNXT,
     1                  ICAPSW,IFORSW,IMULT,
     1                  ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT KRUSKAL-WALLIS TEST
C              NON-PARAMETRIC ONE-WAY ANOVA
C     EXAMPLE--KRUSKAL-WALLIS TEST Y X
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 288-297.
C              --WALPOLE AND MEYERS (1978), "PROBABILITY AND
C                STATISTICS", SECOND EDITION, MACMILLIAN.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/6
C     ORIGINAL VERSION--JUNE      1999.
C     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML AND LATEX
C                                       OUTPUT
C     UPDATED         --FEBRUARY  2011. USE DPPARS
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      REAL PID(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION DTAG(MAXOBV)
      DIMENSION ARANK(MAXOBV)
      DIMENSION NRANK(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION RTEMP(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE(GARBAG(IGARB1),DTAG(1))
      EQUIVALENCE(GARBAG(IGARB2),ARANK(1))
      EQUIVALENCE(GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE(GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE(GARBAG(IGARB6),RTEMP(1))
C
      INCLUDE 'DPCOZI.INC'
      EQUIVALENCE(IGARBG(IIGAR1),NRANK(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
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
      ISUBN1='DPKR'
      ISUBN2='US  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IFOUND='YES'
      IERROR='NO'
C
C               ******************************************
C               **  TREAT THE KRUSKAL-WALLIS TEST CASE  **
C               ******************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KRUS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKRUS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IMULT,IKRUGS,MAXNXT
   55   FORMAT('IMULT,IKRUGS,MAXNXT = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='KRUSKAL WALLIS TEST'
      MAXNA=100
      MINNVA=1
      MAXNVA=100
      MINNA=1
      IFLAGE=1
      IFLAGM=0
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
        IFLAGM=1
      ENDIF
      MINN2=2
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,182)NQ,NUMVAR,IMULT
  182   FORMAT('NQ,NUMVAR,IMULT = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO185I=1,NUMVAR
            WRITE(ICOUT,187)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  187       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  185     CONTINUE
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  GENERATE THE KRUSKAL WALLIS TEST FOR THE VARIOUS **
C               **  CASES                                            **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: TWO RESPONSE VARIABLES     **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,X,X,NLOCAL,NLOCA2,NLOCA2,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C
C               ******************************************************
C               **  STEP 3B--
C               **  PREPARE FOR ENTRANCE INTO DPKRU2--
C               ******************************************************
C
        ISTEPN='3B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPKRUS, AS WE ARE ABOUT TO CALL DPKRU2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,332)NLOCAL
  332     FORMAT('NLOCAL = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO335I=1,NLOCAL
            WRITE(ICOUT,336)I,Y(I),X(I)
  336       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
C
        CALL DPKRU2(Y,X,NLOCAL,IVARN1,IVARN2,
     1              DTAG,ARANK,NRANK,MAXNXT,
     1              RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,
     1              ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************************
C               **  STEP 4A--                                        **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
C               **          FOR KRUSKAL-WALLIS TEST, THE MULTIPLE    **
C               **          LABS ARE CONVERTED INTO A "Y X" STACKED  **
C               **          PAIR WHERE "X" IS THE LAB-ID VARIABLE.   **
C               *******************************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=NUMVAR
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              TEMP1,Y,X,NLOCAL,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        NUMVAR=2
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KRUS')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,442)
  442     FORMAT('***** FROM THE MIDDLE  OF DPKRUS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,443)ICASAN,NUMVAR,NLOCAL
  443     FORMAT('ICASAN,NUMVAR,NLOCAL = ',A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO445I=1,NLOCAL
              WRITE(ICOUT,446)I,Y(I),X(I)
  446         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  445       CONTINUE
          ENDIF
        ENDIF
C
        CALL DPKRU2(Y,X,NLOCAL,IVARN1,IVARN2,
     1              DTAG,ARANK,NRANK,MAXNXT,
     1              RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,
     1              ISUBRO,IBUGA3,IERROR)
C
C         ***************************************
C         **  STEP 8C--                        **
C         **  UPDATE INTERNAL DATAPLOT TABLES  **
C         ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'KRUS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKRUS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NLOCAL,STATVA,STATCD
 9014   FORMAT('NLOCAL,STATVA,STATCD = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKRU2(Y,TAG,N,IVARID,IVARI2,
     1                  DTAG,ARANK,NRANK,MAXNXT,
     1                  RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,
     1                  CUT99,CUT999,
     1                  ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT KRUSKALL-WALLIS'S TEST
C              NON-PARAMETRIC ONE-WAY ANOVA
C     EXAMPLE--KRUSKALL-WALLIS TEST Y TAG
C     REFERENCE--W. J. CONOVER, "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, 1999, WILEY,
C                PP. 288-297.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/6
C     ORIGINAL VERSION--JUNE      1999.
C     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML AND LATEX
C                                       OUTPUT
C     UPDATED         --OCTOBER   2004. ADD MULTIPLE COMPARISONS
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --JANUARY   2007. CALL LIST TO RANK
C     UPDATED         --FEBRUARY  2009. SORT BY GROUP-ID VARIABLE
C                                       FIRST (THIS INSURES MULTIPLE
C                                       COMPARISONS ARE PRINTED IN
C                                       CORRECT ORDER).
C     UPDATED         --FEBRUARY  2009. ADD SOME DEBUGGING CODE
C     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA4 TO PRINT
C                                       OUTPUT TABLES.  THIS ADDS RTF
C                                       SUPPORT AND SPECIFICATION OF
C                                       THE NUMBER OF DIGITS.
C     UPDATED         --FEBRUARY  2011. OPTION TO PRINT GROUP
C                                       STATISTICS
C     UPDATED         --JULY      2011. SPLIT OFF DPKRU3, MAKE MORE
C                                       EFFICIENT USE OF STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IKRUGS
      CHARACTER*4 IKRUMC
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IATEMP
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION DTAG(*)
      DIMENSION ARANK(*)
      DIMENSION NRANK(*)
      DIMENSION RTEMP(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
C
C---------------------------------------------------------------------
C
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
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
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      LOGICAL IFLAGS
      LOGICAL IFLAGE
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
      DATA ALPHA/
     1 0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
      ISUBN1='DPKR'
      ISUBN2='U2  '
      ISUBN0='    '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KRU2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPKRU2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
      CALL DPKRU3(Y,TAG,N,
     1            DTAG,ARANK,NRANK,MAXNXT,
     1            RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            STATVA,STATCD,PVAL,NUMDF,NUMDIS,S2,
     1            IKRUGS,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CUT0=0.0
      CALL CHSPPF(.50,NUMDF,CUT50)
      CALL CHSPPF(.75,NUMDF,CUT75)
      CALL CHSPPF(.90,NUMDF,CUT90)
      CALL CHSPPF(.95,NUMDF,CUT95)
      CALL CHSPPF(.975,NUMDF,CUT975)
      CALL CHSPPF(.99,NUMDF,CUT99)
      CALL CHSPPF(.999,NUMDF,CUT999)
C
      IOP='OPEN'
      IFLG1=1
      IFLG2=0
      IFLG3=0
      IFLG4=0
      IFLG5=0
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      WRITE(IOUNI1,2305)
 2305 FORMAT('     I       J    ',
     1       '|Ri/Ni-Rj/nj|      ',
     1       '90% CV        ',
     1       '95% CV        ',
     1       '99% CV        ')
C
      IDF=N-NUMDIS
      ALPHAT=0.05
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT95)
      ALPHAT=0.10
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT90)
      ALPHAT=0.01
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT99)
      AN=REAL(N)
      AFACT2=SQRT(S2*(REAL(N)-1.0-STATVA)/REAL(N-NUMDIS))
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')THEN
        WRITE(ICOUT,2321)AFACT2
 2321   FORMAT('BEFORE MULTIPLE COMPARISONS: AFACT2 = ',G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      DO2330I=1,NUMDIS
        DO2339J=1,NUMDIS
          IF(I.LT.J)THEN
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS((ARANK(I)/ANI) - (ARANK(J)/ANJ))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
            IATEMP='    '
            IF(ADIFF.GE.ACV90)IATEMP(2:2)='*'
            IF(ADIFF.GE.ACV95)IATEMP(3:3)='*'
            IF(ADIFF.GE.ACV99)IATEMP(4:4)='*'
            WRITE(IOUNI1,2337)I,J,ADIFF,ACV90,ACV95,ACV99,IATEMP
 2337       FORMAT(I6,2X,I6,2X,4E15.7,A4)
C
            IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')THEN
              WRITE(ICOUT,2341)I,J,ANI,ANJ,ARANK(I),ARANK(J)
 2341         FORMAT('I,J,ANI,ANJ,ARANK(I),ARANK(J) = ',2I8,4G15.7)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,2343)AFACT3,ADIFF
 2343         FORMAT('AFACT3,ADIFF = ',2G15.7)
              CALL DPWRST('XXX','WRIT')
            ENDIF
C
          ENDIF
 2339   CONTINUE
 2330 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************
C               **   STEP 42--                **
C               **   WRITE OUT EVERYTHING     **
C               **   FOR KRUSKALL-WALLIS TEST **
C               ********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
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
      ITITLE='Kruskal-Wallis One Factor Test'
      NCTITL=32
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(IMULT.EQ.'OFF')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
C
C     IF REQUESTED, PRINT OUT GROUP INFORMATION.  SINCE NUMBER
C     OF GROUPS IS UNKNOWN (AND POTENTIALLY LARGE, PRINT EACH
C     GROUP AS A SEPARATE TABLE.
C
      IF(IKRUGS.EQ.'ON')THEN
C
        DO2160I=1,NUMDIS
C
          NUMROW=ICNT
          DO2165II=1,NUMROW
            NTOT(II)=15
 2165     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGA3,IERROR)
          ICNT=0
          ITITLE=' '
          NCTITL=0
          ITITLZ=' '
          NCTITZ=0
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=1
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          IF(IMULT.EQ.'ON')THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Group Variable: '
            WRITE(ITEXT(ICNT)(17:20),'(A4)')IVARID(I)(1:4)
            WRITE(ITEXT(ICNT)(21:24),'(A4)')IVARI2(I)(1:4)
            NCTEXT(ICNT)=24
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Group    '
            WRITE(ITEXT(ICNT)(7:9),'(I3)')I
            NCTEXT(ICNT)=9
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ENDIF
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Observations:'
          NCTEXT(ICNT)=23
          AVALUE(ICNT)=TEMP1(I)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=TEMP2(I)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=TEMP3(I)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='SD:'
          NCTEXT(ICNT)=3
          AVALUE(ICNT)=TEMP4(I)
          IDIGIT(ICNT)=NUMDIG
 2160   CONTINUE
C
        IF(ICNT.GT.0)THEN
          NUMROW=ICNT
          DO2168II=1,NUMROW
            NTOT(II)=15
 2168     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGA3,IERROR)
          ICNT=0
        ENDIF
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Samples Come From Identical Populations'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Samples Do Not Come From Identical Populations'
      NCTEXT(ICNT)=50
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Groups:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=REAL(NUMDIS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Kruskal-Wallis Test Statistic Value:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF of Test Statistic:'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      PVAL=1.0 - STATCD
      AVALUE(ICNT)=1.0 - STATCD
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO4210I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE=' '
      NCTITL=0
C
      ITITL9=' '
      NCTIT9=0
      ITITLE(1:55)=
     1'Percent Points of the Chi-Square Reference Distribution'
      NCTITL=55
      NUMLIN=1
      NUMROW=8
      NUMCOL=3
      ITITL2(1,1)='Percent Point'
      ITITL2(1,2)=' '
      ITITL2(1,3)='Value'
      NCTIT2(1,1)=13
      NCTIT2(1,2)=1
      NCTIT2(1,3)=5
C
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=5
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
 4221 CONTINUE
      ITYPCO(2)='ALPH'
      IDIGIT(1)=1
      IDIGIT(3)=3
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
          IF(J.EQ.1)THEN
            AMAT(I,J)=ALPHA(I)
          ELSEIF(J.EQ.2)THEN
            IVALUE(I,J)='='
            NCVALU(I,J)=1
          ELSEIF(J.EQ.3)THEN
            IF(I.EQ.1)THEN
              AMAT(I,J)=RND(CUT0,IDIGIT(J))
            ELSEIF(I.EQ.2)THEN
              AMAT(I,J)=RND(CUT50,IDIGIT(J))
            ELSEIF(I.EQ.3)THEN
              AMAT(I,J)=RND(CUT75,IDIGIT(J))
            ELSEIF(I.EQ.4)THEN
              AMAT(I,J)=RND(CUT90,IDIGIT(J))
            ELSEIF(I.EQ.5)THEN
              AMAT(I,J)=RND(CUT95,IDIGIT(J))
            ELSEIF(I.EQ.6)THEN
              AMAT(I,J)=RND(CUT975,IDIGIT(J))
            ELSEIF(I.EQ.7)THEN
              AMAT(I,J)=RND(CUT99,IDIGIT(J))
            ELSEIF(I.EQ.8)THEN
              AMAT(I,J)=RND(CUT999,IDIGIT(J))
            ENDIF
          ENDIF
 4225   CONTINUE
 4223 CONTINUE
C
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.FALSE.
C
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      NUMLIN=1
      NUMROW=4
      NUMCOL=4
      ITITL2(1,1)='Alpha'
      ITITL2(1,2)='CDF'
      ITITL2(1,3)='Critical Value'
      ITITL2(1,4)='Conclusion'
      NCTIT2(1,1)=5
      NCTIT2(1,2)=3
      NCTIT2(1,3)=14
      NCTIT2(1,4)=10
C
      NMAX=0
      DO4321I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO4323I=1,NUMROW
        DO4325J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
      IVALUE(1,1)='10%'
      IVALUE(2,1)='5%'
      IVALUE(3,1)='2.5%'
      IVALUE(4,1)='1%'
      IVALUE(1,2)='90%'
      IVALUE(2,2)='95%'
      IVALUE(3,2)='97.5%'
      IVALUE(4,2)='99%'
      NCVALU(1,1)=3
      NCVALU(2,1)=2
      NCVALU(3,1)=4
      NCVALU(4,1)=2
      NCVALU(1,2)=3
      NCVALU(2,2)=3
      NCVALU(3,2)=5
      NCVALU(4,2)=3
      IVALUE(1,4)='Accept H0'
      IVALUE(2,4)='Accept H0'
      IVALUE(3,4)='Accept H0'
      IVALUE(4,4)='Accept H0'
      NCVALU(1,4)=9
      NCVALU(2,4)=9
      NCVALU(3,4)=9
      NCVALU(4,4)=9
      IF(STATVA.GT.CUT90)IVALUE(1,4)='Reject H0'
      IF(STATVA.GT.CUT95)IVALUE(2,4)='Reject H0'
      IF(STATVA.GT.CUT975)IVALUE(3,4)='Reject H0'
      IF(STATVA.GT.CUT99)IVALUE(4,4)='Reject H0'
      AMAT(1,3)=RND(CUT90,IDIGIT(3))
      AMAT(2,3)=RND(CUT95,IDIGIT(3))
      AMAT(3,3)=RND(CUT975,IDIGIT(3))
      AMAT(4,3)=RND(CUT99,IDIGIT(3))
C
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWRTF(1)=1500
      IWRTF(2)=IWRTF(1)+1500
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(3)+2000
      IFRST=.FALSE.
      ILAST=.TRUE.
C
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      IF(IKRUMC.EQ.'OFF')GOTO9000
C
      ITITLE(1:26)='Multiple Comparisons Table'
      NCTITL=26
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(1,1)='I'
      NCTIT2(1,1)=1
      ITITL2(1,2)='J'
      NCTIT2(1,2)=1
      ITITL2(1,3)='|Ri/Ni - Rj/Nj|'
      NCTIT2(1,3)=15
      ITITL2(1,4)='90% CV'
      NCTIT2(1,4)=6
      ITITL2(1,5)='95% CV'
      NCTIT2(1,5)=6
      ITITL2(1,6)='99% CV'
      NCTIT2(1,6)=6
C
      NMAX=0
      NUMCOL=6
      DO4010I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)THEN
          NTOT(I)=5
          IDIGIT(I)=0
        ELSEIF(I.EQ.3)THEN
          NTOT(I)=17
        ENDIF
        NMAX=NMAX+NTOT(I)
 4010 CONTINUE
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
C
      ICNT=0
      DO4081I=1,NUMDIS
        DO4083J=1,NUMDIS
          IF(I.LT.J)THEN
C
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS((ARANK(I)/ANI) - (ARANK(J)/ANJ))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
C
            IF(ICNT.GE.MAXROW)THEN
              NUMLIN=1
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
              CALL DPDTA5(ITITLE,NCTITL,
     1                    ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRST,ILAST,
     1                    IFLAGS,IFLAGE,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNT=0
            ENDIF
C
            ICNT=ICNT+1
            IVALUE(ICNT,1)=' '
            NCVALU(ICNT,1)=0
            AMAT(ICNT,1)=REAL(I)
            IVALUE(ICNT,2)=' '
            NCVALU(ICNT,2)=0
            AMAT(ICNT,2)=REAL(J)
            IVALUE(ICNT,3)=' '
            NCVALU(ICNT,3)=0
            AMAT(ICNT,3)=ADIFF
            IVALUE(ICNT,4)=' '
            NCVALU(ICNT,4)=0
            AMAT(ICNT,4)=ACV90
            IVALUE(ICNT,5)=' '
            NCVALU(ICNT,5)=0
            AMAT(ICNT,5)=ACV95
            IVALUE(ICNT,6)=' '
            NCVALU(ICNT,6)=0
            AMAT(ICNT,6)=ACV99
          ENDIF
 4083   CONTINUE
 4081 CONTINUE
C
      IF(ICNT.GE.1)THEN
        NUMLIN=1
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
       ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KRU2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKRU2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9025)STATVA,STATCD
 9025   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPKRU3(Y,TAG,N,
     1                  DTAG,ARANK,NRANK,MAXNXT,
     1                  RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  STATVA,STATCD,PVAL,NUMDF,NUMDIS,S2,
     1                  IKRUGS,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT KRUSKALL-WALLIS'S TEST
C              NON-PARAMETRIC ONE-WAY ANOVA
C     EXAMPLE--KRUSKALL-WALLIS TEST Y TAG
C     REFERENCE--W. J. CONOVER, "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, 1999, WILEY,
C                PP. 288-297.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/7
C     ORIGINAL VERSION--JULY      2011. EXTRACTED FROM DPKRU3 TO ALLOW
C                                       IT TO BE CALLED FROM CMPSTA
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IKRUGS
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION DTAG(*)
      DIMENSION ARANK(*)
      DIMENSION NRANK(*)
      DIMENSION RTEMP(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
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
      ISUBN1='DPKR'
      ISUBN2='U3  '
      ISUBN0='    '
C
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KRU3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPKRU3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN KRUSKAL-WALLIS TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 2.')
        WRITE(ICOUT,1115)N
 1115   FORMAT('      THE SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1135I=2,N
        IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
 1139 CONTINUE
C
      HOLD=TAG(1)
      DO1235I=2,N
        IF(TAG(I).NE.HOLD)GOTO1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE GROUP-ID VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
 1239 CONTINUE
C
C               ********************************
C               **  STEP 41--                 **
C               **  CARRY OUT CALCULATIONS    **
C               **  FOR KRUSKALL-WALLIS TEST  **
C               ********************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL SORTC(TAG,Y,N,TAG,TEMP1)
      DO2101I=1,N
        Y(I)=TEMP1(I)
 2101 CONTINUE
      CALL DISTIN(TAG,N,IWRITE,DTAG,NUMDIS,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      CALL RANK(Y,N,IWRITE,RTEMP,TEMP1,MAXNXT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')THEN
        DO4110I=1,N
          WRITE(ICOUT,4133)I,TAG(I),Y(I),RTEMP(I)
 4133     FORMAT('I,TAG(I),Y(I),RTEMP(I) = ',I8,2X,3G15.7)
          CALL DPWRST('XXX','WRIT')
 4110   CONTINUE
      ENDIF
C
CCCCC OCTOBER 2004: THE KRUSKAL-WALLIS STATISTIC FOR THE CASE
CCCCC WITH NO TIES IS:
CCCCC
CCCCC    H = [12/(N*(N+1)]*SUM[i=1 to k][R(i)**2/N(i)] - 3*(N+1)
CCCCC
CCCCC THE FORMULA WITH TIES IS:
CCCCC
CCCCC    H = (1/S**2)*{SUM[i=1 to k][R(i)**2/N(i) - N*(N+1)**2/4}
CCCCC
CCCCC GO AHEAD AND USE THE TIES FORMULA SINCE IT IS JUST AS EASY
CCCCC AND IT ALSO FACILATES THE COMPUTATION OF MULTIPLE COMPARISONS.
C
CCCCC AFACT=12.0/(REAL(N)*REAL(N+1))
      AN=REAL(N)
      AFACT=AN*(AN+1.0)**2/4.0
C
      DSUM1=0.0D0
      DO2190I=1,N
        DSUM1=DSUM1 + DBLE(RTEMP(I))**2
 2190 CONTINUE
      S2=REAL((DSUM1 - DBLE(AFACT))/DBLE(N-1))
C
      DSUM1=0.0D0
      DO2200IDIS=1,NUMDIS
         J=0
         DSUM2=0.0D0
         DO2210I=1,N
            IF(TAG(I).EQ.DTAG(IDIS))THEN
               J=J+1
               DSUM2=DSUM2 + DBLE(RTEMP(I))
               IF(IKRUGS.EQ.'ON')TEMP1(J)=Y(I)
            ENDIF
 2210    CONTINUE
         IF(IKRUGS.EQ.'ON')THEN
           CALL MEDIAN(TEMP1,J,IWRITE,TEMP5,MAXNXT,YMED,
     1                 IBUGA3,IERROR)
           CALL MEAN(TEMP1,J,IWRITE,YMEANT,IBUGA3,IERROR)
           CALL SD(TEMP1,J,IWRITE,YSD,IBUGA3,IERROR)
           TEMP2(IDIS)=YMEANT
           TEMP3(IDIS)=YMED
           TEMP4(IDIS)=YSD
         ENDIF
         NRANK(IDIS)=J
         ARANK(IDIS)=REAL(DSUM2)
         DSUM1=DSUM1 + DSUM2**2/DBLE(NRANK(IDIS))
 2200 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')THEN
        WRITE(ICOUT,2221)NUMDIS,AFACT,S2
 2221   FORMAT('NUMDIS,AFACT,S2 = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO2220I=1,NUMDIS
          WRITE(ICOUT,2223)I,NRANK(I),ARANK(I)
 2223     FORMAT('I,NRANK(I),ARANK(I) = ',2I8,2X,G15.7)
          CALL DPWRST('XXX','WRIT')
 2220   CONTINUE
      ENDIF
C
      DTERM1=DSUM1 - DBLE(AFACT)
      STATVA=DTERM1/DBLE(S2)
      NUMDF=NUMDIS-1
      CALL CHSCDF(STATVA,NUMDF,STATCD)
      PVAL=1.0 - STATCD
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KRU3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKRU3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9025)STATVA,STATCD,PVAL
 9025   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
